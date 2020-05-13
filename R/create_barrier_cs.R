#' create_barrier_cs
#'
#' Creates a Barrier Cost Surface
#'
#' Creates a cost surface representing barriers to movement in the landscape.
#'
#' @param raster \code{RasterLayer} (raster package). This is used to derive the resolution, extent, and spatial reference system to be used when calculating the barrier cost surface
#'
#' @param barrier \code{Spatial*}. Areas representing barriers to movement in the landscape
#'
#' @param neighbours Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected values are 4, 8, or 16. Default is 16.
#'
#' @return \code{TransitionLayer} (gdistance package) numerically expressing the barriers to movement in the landscape. The resultant \code{TransitionLayer} can be incorporated with other \code{TransitionLayer} through Raster calculations.
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
    
    if (!inherits(barrier, c("SpatialPoints", "SpatialPointsDataFrame"))) {
        stop("barrier argument is invalid. Expecting a Spatial* object")
    }
    
    if (!neighbours %in% c(4, 8, 16)) {
        stop("neighbours argument is invalid. Expecting 4, 8, or 16.")
    }
    
    # crop Spatial* object to raster extent
    barrier_cropped <- raster::crop(barrier, raster)
    
    # create raster based on supplied Spatial* object
    barrier_raster <- raster::rasterize(x = barrier_cropped, y = raster, field = 0, background = 1)
    
    barrier_cs <- gdistance::transition(barrier_raster, transitionFunction = min, 16)
    
    return(barrier_cs)
    
}
