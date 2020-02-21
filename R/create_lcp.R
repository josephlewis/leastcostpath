#' create_lcp
#'
#' Calculates Least Cost Path
#'
#' Calculates the Least Cost Path from an origin location to a destination location. See details for more.
#'
#' @param cost_surface \code{TransitionLayer} object (gdistance package). Cost surface to be used in Least Cost Path calculation
#'
#' @param origin \code{SpatialPoints} location from which the Least Cost Path is calculated. Only the first cell is taken into account.
#'
#' @param destination \code{SpatialPoints} location to which the Least Cost Path is calculated
#'
#' @param directional if TRUE Least Cost Path calculated from origin to destination only. If FALSE Least Cost Path calculated from origin to destination and destination to origin. See details for more
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
#' @return SpatialLines object or list of SpatialLines object (dependent on directional argument)
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
#' lcps <- create_lcp(cost_surface = final_cost_cs, origin = loc1,
#' destination = loc2, directional = FALSE)

create_lcp <- function(cost_surface, origin, destination, directional = FALSE) {
    
    if (!inherits(cost_surface, "TransitionLayer")) {
        stop("cost_surface argument is invalid. Expecting a TransitionLayer object")
    }
    
    
    if (!inherits(origin, "SpatialPoints")) {
        stop("origin argument is invalid. Expecting a SpatialPoints object")
    }
    
    if (!inherits(destination, "SpatialPoints")) {
        stop("destination argument is invalid. Expecting a SpatialPoints object")
    }
    
    
    sPath <- list()
    
    if (directional == "TRUE") {
        
        sPath[[1]] <- gdistance::shortestPath(cost_surface, origin, destination, output = "SpatialLines")
        
    } else {
        
        sPath[[1]] <- gdistance::shortestPath(cost_surface, origin, destination, output = "SpatialLines")
        sPath[[2]] <- gdistance::shortestPath(cost_surface, destination, origin, output = "SpatialLines")
    }
    
    return(sPath)
}
