#' create_cost_corridor
#'
#' Creates a Cost Corridor using a previously created Cost Surface
#'
#'The create_cost_corridor function combines the accumulated cost surfaces from origin-to-destination and destination-to-origin to identify areas of preferential movement that takes into account both diretions of movement.
#'
#' @param cost_surface Cost Surface. Expects Object of class TransitionLayer.
#'
#' @param origin origin Location from which the Accumulated Cost is calculated. Expects Object of class SpatialPoints
#'
#' @param destination origin Location from which the Accumulated Cost is calculated. Expects Object of class SpatialPoints
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
#' create_cost_corridor(slope_cs, loc1, loc2)

create_cost_corridor <- function(cost_surface, origin, destination) {
    
    if (!inherits(cost_surface, "TransitionLayer")) 
        stop("cost_surface expects TransitionLayer object")
    
    if (!inherits(origin, "SpatialPoints") & !inherits(destination, "SpatialPoints")) 
        stop("Origin and Destination expects SpatialPoints objects")
    
    accCost_origin <- accCost(cost_surface, origin)
    accCost_destination <- accCost(cost_surface, destination)
    
    costCorridor <- accCost_origin + accCost_destination
    
    costCorridor[is.infinite(costCorridor)] <- NA
    
    # function to rescale cell values between 0 and 1 from Tim Assal http://www.timassal.com/?p=859
    rasterRescale <- function(r) {
        ((r - cellStats(r, "min"))/(cellStats(r, "max") - cellStats(r, "min")))
    }
    
    costCorridor <- rasterRescale(costCorridor)
    
    return(costCorridor)
}
