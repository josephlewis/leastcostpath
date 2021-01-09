#' Create Barrier Cost Surface
#'
#' Creates a cost surface that incorporates barriers that inhibit movement in the landscape.
#'
#' @details
#'
#' The resultant Barrier Cost Surface is produced by assessing which areas of the raster coincide with the SpatialPolygons as specified in the barrier argument. The areas of the raster that coincide with the SpatialPolygons are given a conductance value of 0 (default value), with all other areas given a Conductance value of 1 (default value). The conductance value of 0 ensures that movement is inhibited within these areas. Examples of use include rivers, altitudes, and taboo areas.
#'
#' @param raster \code{RasterLayer} (raster package). The Resolution, Extent, and Spatial Reference System of the provided RasterLayer is used when creating the resultant Barrier Cost Surface
#'
#' @param barrier \code{SpatialPolygons} (sp package). Area within the landscape that movement is inhibited. See details for more
#'
#' @param neighbours \code{numeric} value. Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected numeric values are 4, 8, 16, 32, 48 or a matrix object. Default is numeric value 16
#'
#' @param field \code{numeric} value. Value assigned to cells that coincide with the barrier SpatialPolygons. Default is \code{numeric value 0}
#'
#' @param background \code{numeric} value. Value assigned to cells that do not coincide with the barrier SpatialPolygons Default is \code{numeric value 1}
#'
#' @return \code{TransitionLayer} (gdistance package) numerically expressing the barriers to movement in the landscape. The resultant \code{TransitionLayer} can be incorporated with other \code{TransitionLayer} through Raster calculations
#'
#' @author Joseph Lewis
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import Matrix
#' @import gdistance
#'
#' @export
#'
#' @examples
#' r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
#' loc1 = cbind(2667670, 6479000)
#' loc1 = sp::SpatialPoints(loc1)
#'
#' loc1 <- rgeos::gBuffer(loc1, width = 50)
#'
#' barrier <- create_barrier_cs(raster = r, barrier = loc1)

create_barrier_cs <- function(raster, barrier, neighbours = 16, field = 0, background = 1) {
    
    if (!inherits(raster, "RasterLayer")) {
        stop("raster argument is invalid. Expecting a RasterLayer object")
    }
    
    if (!inherits(barrier, "SpatialPolygons")) {
        stop("barrier argument is invalid. Expecting a Spatial* object")
    }
    
    if (any(!neighbours %in% c(4, 8, 16, 32, 48)) & (!inherits(neighbours, "matrix"))) {
        stop("neighbours argument is invalid. Expecting 4, 8, 16, 32, 48, or matrix object")
    }
    
    if (inherits(neighbours, "numeric")) {
        if (neighbours == 32) {
            neighbours <- neighbours_32
            
        } else if (neighbours == 48) {
            neighbours <- neighbours_48
        }
        
    }
    
    # create TransitionLayer of zeroes based on raster dimensions
    barrier_cs <- new("TransitionLayer", nrows = as.integer(nrow(raster)), ncols = as.integer(ncol(raster)), extent = extent(raster), crs = projection(raster, 
        asText = FALSE), transitionMatrix = Matrix(0, ncell(raster), ncell(raster)), transitionCells = 1:ncell(raster))
    
    # get cells that coincide with barrier
    barrier_cells <- raster::cellFromPolygon(object = raster, p = barrier)[[1]]
    
    # get adjacent cells to limit value change
    adj <- raster::adjacent(raster, cells = 1:raster::ncell(raster), pairs = TRUE, directions = neighbours)
    
    # change values that coincide with barrier
    barrier_cs[adj[adj[, 2] %in% barrier_cells, ]] <- field
    
    # change values that don't coincide with barrier
    barrier_cs[adj[!adj[, 2] %in% barrier_cells, ]] <- background
    
    # drop zeroes from matrix
    barrier_cs@transitionMatrix <- Matrix::drop0(barrier_cs@transitionMatrix)
    
    return(barrier_cs)
    
}
