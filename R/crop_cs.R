#' Crop Cost Surface
#'
#' Crops Cost Surfaces to the supplied SpatialPolygon* boundary
#'
#' @details
#'
#' The resultant Cost Surface is cropped to the SpatialPolygons* boundary. All areas of the Cost Surface that are outside the supplied boundary are given a conductance value of 0. The conductance value of 0 ensures that movement is inhibited within these areas.
#'
#' @param cost_surface \code{TransitionLayer} (gdistance package). Cost surface to crop
#'
#' @param boundary \code{SpatialPolygons*} (sp package). Boundary used when cropping Cost Surface
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
#' loc1 = cbind(2667670, 6479000)
#' loc1 = sp::SpatialPoints(loc1)
#' loc1 <- rgeos::gBuffer(spgeom = loc1, width = 200)
#' raster::crs(loc1) <- raster::crs(r)
#'
#' slope_cs <- create_slope_cs(r, cost_function = 'tobler', neighbours = 16, max_slope = NULL)
#'
#' slope_cs_cropped <- crop_cs(cost_surface = slope_cs, boundary = loc1)

crop_cs <- function(cost_surface, boundary) {
    
    if (!inherits(cost_surface, "TransitionLayer")) {
        stop("cost_surface argument is invalid. Expecting a TransitionLayer object")
    }
    
    if (!inherits(boundary, "SpatialPolygons")) {
        stop("boundary argument is invalid. Expecting a SpatialPolygons* object")
    }
    
    ras <- raster::raster(ncol = cost_surface@ncols, nrow = cost_surface@nrows, ext = cost_surface@extent, crs = cost_surface@crs)
    
    ras_pts <- raster::rasterToPoints(x = ras, spatial = TRUE)
    
    pts_not_over_boundary <- base::which(is.na(sp::over(ras_pts, boundary)))
    
    cost_surface@transitionMatrix[, pts_not_over_boundary] <- 0
    
    return(cost_surface)
    
}
