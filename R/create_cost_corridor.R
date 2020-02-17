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
#' @param destination Destination Location from which the Accumulated Cost is calculated. Expects Object of class SpatialPoints
#' 
#' @param rescale Rescales cost corridor raster to values between 0 and 1. Default is TRUE
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
#' cost_corridor <- create_cost_corridor(slope_cs, loc1, loc2, rescale = TRUE)

create_cost_corridor <- function(cost_surface, origin, destination, rescale = TRUE) {
    
    if (!inherits(cost_surface, "TransitionLayer")) 
        stop("cost_surface expects TransitionLayer object")
    
    if (!inherits(origin, c("SpatialPoints", "SpatialPointsDataFrame"))) 
        stop("Origin expects SpatialPoints or SpatialPointsDataFrame object")
    
    if (!inherits(destination, c("SpatialPoints", "SpatialPointsDataFrame"))) 
        stop("Destination expects SpatialPoints or SpatialPointsDataFrame object")
    
    accCost_origin <- accCost(cost_surface, origin)
    accCost_destination <- accCost(cost_surface, destination)
    
    costCorridor <- accCost_origin + accCost_destination
    
    costCorridor[is.infinite(costCorridor)] <- NA
    
    if (rescale) {
        
        # function to rescale cell values between 0 and 1 from Tim Assal http://www.timassal.com/?p=859
        rasterRescale <- function(r) {
            ((r - cellStats(r, "min"))/(cellStats(r, "max") - cellStats(r, "min")))
        }
        
        costCorridor <- rasterRescale(costCorridor)
        
    }
    
    return(costCorridor)
}
