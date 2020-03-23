#' create_cost_corridor
#'
#' Creates a Cost Corridor raster object
#'
#' Combines the accumulated cost surfaces from origin-to-destination and destination-to-origin to identify areas of preferential movement that takes into account both directions of movement.
#'
#' @param cost_surface \code{TransitionLayer} object (gdistance package). Cost surface to be used in Cost Corridor calculation
#'
#' @param origin \code{SpatialPoints}. orgin location from which the Accumulated Cost is calculated. Only the first cell is taken into account.
#'
#' @param destination \code{SpatialPoints}. destination location from which the Accumulated Cost is calculated. Only the first cell is taken into account
#'
#' @param rescale if TRUE raster values scaled to between 0 and 1. Default is FALSE
#'
#' @return RasterLayer object
#'
#' @return \code{RasterLayer} (raster package). The resultant object is the accumulated cost surface from origin-to-destination and destination-to-origin and can be used to identify areas of preferential movement in the landscape.
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
#' slope_cs <- create_slope_cs(r, cost_function = 'tobler', neighbours = 16)
#'
#' loc1 = cbind(2667670, 6479000)
#' loc1 = sp::SpatialPoints(loc1)
#'
#' loc2 = cbind(2667800, 6479400)
#' loc2 = sp::SpatialPoints(loc2)
#'
#' cost_corridor <- create_cost_corridor(slope_cs, loc1, loc2, rescale = FALSE)

create_cost_corridor <- function(cost_surface, origin, destination, rescale = FALSE) {
    
    if (!inherits(cost_surface, "TransitionLayer")) {
        stop("cost_surface argument is invalid. Expecting a TransitionLayer object")
    }
    
    if (!inherits(origin, c("SpatialPoints", "SpatialPointsDataFrame"))) {
        stop("Origin argument is invalid. Expecting SpatialPoints or SpatialPointsDataFrame object")
    }
    
    if (!inherits(destination, c("SpatialPoints", "SpatialPointsDataFrame"))) {
        stop("Destination argument is invalid. Expecting SpatialPoints or SpatialPointsDataFrame object")
    }
    
    accCost_origin <- accCost(cost_surface, origin)
    accCost_destination <- accCost(cost_surface, destination)
    
    costCorridor <- accCost_origin + accCost_destination
    
    costCorridor[is.infinite(costCorridor)] <- NA
    
    if (rescale) {
        rasterRescale <- function(r) {
            ((r - cellStats(r, "min"))/(cellStats(r, "max") - cellStats(r, "min")))
        }
        
        costCorridor <- rasterRescale(costCorridor)
        
    }
    
    return(costCorridor)
}
