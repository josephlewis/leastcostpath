#' Crop Cost Surface
#'
#' Crops Cost Surfaces to the supplied SpatialPolygon* boundary
#'
#' @details
#'
#' The resultant Cost Surface is cropped to the Spatial* or RasterLayer object. All areas of the Cost Surface that are outside the supplied boundary are given a conductance value of 0. The conductance value of 0 ensures that movement is inhibited within these areas. If a RasterLayer object is supplied in the boundary argument then all cells with a value of \code{NA} will be given a Conductance value of 0.
#'
#' @param cost_surface \code{TransitionLayer} (gdistance package). Cost surface to crop
#'
#' @param boundary \code{Spatial*} (sp package) or \code{RasterLayer} (raster package). Boundary used when cropping Cost Surface. See details for more
#'
#' @return \code{TransitionLayer} (gdistance package). Cropped Cost Surface
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
#'
#' pt = cbind(2667670, 6479000)
#' pt = sp::SpatialPoints(pt)
#' polygon <- rgeos::gBuffer(spgeom = pt, width = 200)
#' raster::crs(pt) <- raster::crs(r)
#' raster::crs(polygon) <- raster::crs(r)
#'
#' slope_cs <- create_slope_cs(r, cost_function = 'tobler', neighbours = 16, max_slope = NULL)
#'
#' slope_cs_pt <- crop_cs(cost_surface = slope_cs, boundary = pt)
#' slope_cs_polygon <- crop_cs(cost_surface = slope_cs, boundary = polygon)
#'
#' r2 <- r
#' ext <- raster::extent(2667500, 2667900, 6478800, 6479500)
#' cells <- unlist(raster::cellFromPolygon(object = r, p = as(ext, 'SpatialPolygons')))
#' r2[-cells] <- NA
#'
#' slope_cs_raster <- crop_cs(cost_surface = slope_cs, boundary = r2)

crop_cs <- function(cost_surface, boundary) {
    
    if (!inherits(cost_surface, "TransitionLayer")) {
        stop("cost_surface argument is invalid. Expecting a TransitionLayer object")
    }
    
    if ((!inherits(boundary, "Spatial")) & (!inherits(boundary, "RasterLayer"))) {
        stop("boundary argument is invalid. Expecting a Spatial* or RasterLayer object")
    }
    
    ras <- raster::raster(ncol = cost_surface@ncols, nrow = cost_surface@nrows, ext = cost_surface@extent, crs = cost_surface@crs, vals = NA)
    
    if (inherits(boundary, "SpatialPoints")) {
        
        boundary_cells <- raster::cellFromXY(object = ras, xy = boundary)
        
    } else if (inherits(boundary, "SpatialLines")) {
        
        boundary_cells <- unlist(raster::cellFromLine(object = ras, lns = boundary))
        
    } else if (inherits(boundary, "SpatialPolygons")) {
        
        boundary_cells <- unlist(raster::cellFromPolygon(object = ras, p = boundary))
        
    } else if (inherits(boundary, "RasterLayer")) {
        
        boundary_cells <- which(!is.na(raster::getValues(boundary)))
        
    }
    
    adj <- gdistance::adjacencyFromTransition(cost_surface)
    
    cost_surface[adj[!adj[, 2] %in% boundary_cells, ]] <- 0
    
    cost_surface@transitionMatrix <- Matrix::drop0(cost_surface@transitionMatrix)
    
    return(cost_surface)
    
}
