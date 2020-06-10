#' Create Barrier Cost Surface
#'
#' Creates a cost surface that incorporates barriers that inhibit movement in the landscape.
#'
#' @details
#'
#' The resultant Barrier Cost Surface is produced by assessing which areas of the raster coincide with the Spatial object as specified in the barrier argument. The areas of raster that coincide with the Spatial object are given a conductance value of 0, with all other areas given a Conductance value of 1. The conductance value of 0 ensures that movement is inhibited within these areas. Examples include rivers, lakes, and taboo areas.
#'
#' @param raster \code{RasterLayer} (raster package). The Resolution, Extent, and Spatial Reference System of the provided RasterLayer is used when creating the resultant Barrier Cost Surface
#'
#' @param barrier \code{Spatial*} (sp package). Areas within the landscape that movement is inhibited. See details for more
#'
#' @param neighbours \code{numeric} value. Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected numeric values are 4, 8, 16, 32, 48 or a matrix object. Default is numeric value 16
#'
#' @return \code{TransitionLayer} (gdistance package) numerically expressing the barriers to movement in the landscape. The resultant \code{TransitionLayer} can be incorporated with other \code{TransitionLayer} through Raster calculations
#'
#' @author Joseph Lewis
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import gdistance
#'
#' @export
#'
#' @examples
#' r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
#' loc1 = cbind(2667670, 6479000)
#' loc1 = sp::SpatialPoints(loc1)
#'
#' barrier <- create_barrier_cs(raster = r, barrier = loc1)

create_barrier_cs <- function(raster, barrier, neighbours = 16) {
    
    if (!inherits(raster, "RasterLayer")) {
        stop("raster argument is invalid. Expecting a RasterLayer object")
    }
    
    if (!inherits(barrier, "Spatial")) {
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
    
    # crop Spatial* object to raster extent
    barrier_cropped <- raster::crop(barrier, raster)
    
    # create raster based on supplied Spatial* object
    barrier_raster <- raster::rasterize(x = barrier_cropped, y = raster, field = 0, background = 1)
    
    barrier_cs <- gdistance::transition(barrier_raster, transitionFunction = min, neighbours)
    
    return(barrier_cs)
    
}
