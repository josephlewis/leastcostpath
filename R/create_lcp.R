#' create_lcp
#'
#' Calculates Least Cost Paths which are often, but not exclusively, used in archaeological research
#'
#' The function computes the Least Cost Path from an origin location to a destination location. It uses the cost surface generated using the create_slope_cs and/or create_traversal_cs functions. The function takes a Cost Surface ('TransitionLayer' class) and point features ('SpatialPoints' class) for the origin location and destination location.
#'
#' @param cost_surface Cost Surface. Expects Object of class TransitionLayer.
#'
#' @param origin Location from which the Least Cost Path is calculated. Expects Object of class SpatialPoints
#'
#' @param destination Location to which the Least Cost Path is calculated. Expects Object of class SpatialPoints
#'
#' @param directional If FALSE (default) then Least Cost Paths computed from origin to destination and destination to origin.
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
#'@examples
#' r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
#'
#' slope_cs <- create_slope_cs(r, cost_function = 'tobler')
#'
#' traverse_cs <- create_traversal_cs(r, neighbours = 16)
#'
#' final_cost_cs <- slope_cs * traverse_cs
#'
#' loc1 = cbind(2667670, 6479000)
#' loc1 = sp::SpatialPoints(loc1)
#'
#' loc2 = cbind(2667800, 6479400)
#' loc2 = sp::SpatialPoints(loc2)
#'
#' create_lcp(cost_surface = final_cost_cs, origin = loc1,
#' destination = loc2, directional = FALSE)

create_lcp <- function(cost_surface, origin, destination, directional = FALSE) {
    
    if (!inherits(cost_surface, "TransitionLayer")) 
        stop("cost_surface expects TransitionLayer object")
    
    if (!inherits(origin, "SpatialPoints") & !inherits(destination, "SpatialPoints")) 
        stop("Origin and Destination expects SpatialPoints objects")
    
    sPath <- list()
    
    if (directional == "TRUE") {
        
        sPath[[1]] <- gdistance::shortestPath(cost_surface, origin, destination, output = "SpatialLines")
        
    } else {
        
        sPath[[1]] <- gdistance::shortestPath(cost_surface, origin, destination, output = "SpatialLines")
        sPath[[2]] <- gdistance::shortestPath(cost_surface, destination, origin, output = "SpatialLines")
    }
    
    return(sPath)
}
