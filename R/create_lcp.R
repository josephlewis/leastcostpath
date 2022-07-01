#' Calculate Least Cost Path from Origin to Destination
#'
#' Calculates a Least Cost Path from an origin location to a destination location. Applies Dijkstra's algorithm.
#'
#' @param cost_surface \code{TransitionLayer} (gdistance package). Cost surface to be used in Least Cost Path calculation
#'
#' @param origin \code{SpatialPoints*} (sp package) location from which the Least Cost Path is calculated. Only the first row is taken into account
#'
#' @param destination \code{SpatialPoints*} (sp package) location to which the Least Cost Path is calculated. Only the first row is taken into account
#'
#' @param directional \code{logical}. if TRUE Least Cost Path calculated from origin to destination only. If FALSE Least Cost Path calculated from origin to destination and destination to origin. Default is FALSE
#'
#' @param cost_distance \code{logical}. if TRUE computes total accumulated cost for each Least Cost Path. Default is FALSE
#'
#' @references Dijkstra, E. W. (1959). A note on two problems in connexion with graphs. Numerische Mathematik. 1: 269-271.
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
#' @return \code{SpatialLinesDataFrame} (sp package) of length 1 if directional argument is TRUE or 2 if directional argument is FALSE. The resultant object is the shortest route (i.e. least cost) between origin and destination using the supplied \code{TransitionLayer}.
#'
#'@examples
#' r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
#'
#' slope_cs <- create_slope_cs(r, cost_function = 'tobler')
#'
#' loc1 = cbind(2667670, 6479000)
#' loc1 = sp::SpatialPoints(loc1)
#'
#' loc2 = cbind(2667800, 6479400)
#' loc2 = sp::SpatialPoints(loc2)
#'
#' lcps <- create_lcp(cost_surface = slope_cs, origin = loc1,
#' destination = loc2, directional = FALSE, cost_distance = FALSE)

create_lcp <- function(cost_surface, origin, destination, directional = FALSE, cost_distance = FALSE) {
    
    if (!inherits(cost_surface, "TransitionLayer")) {
        stop("cost_surface argument is invalid. Expecting a TransitionLayer object")
    }
    
    if (!inherits(origin, c("SpatialPoints", "SpatialPointsDataFrame"))) {
        stop("origin argument is invalid. Expecting a SpatialPoints* object")
    }
    
    if (!inherits(destination, c("SpatialPoints", "SpatialPointsDataFrame"))) {
        stop("destination argument is invalid. Expecting a SpatialPoints* object")
    }
    
    if (directional == "TRUE") {
        
        sPath <- try(suppressWarnings(gdistance::shortestPath(cost_surface, origin, destination, output = "SpatialLines")), silent = TRUE)
        
        if(inherits(sPath, "try-error")) {
            warning("least cost path from origin to destination could not be calculated. Returning empty SpatialLinesDataFrame")  
            sPath <- SpatialLinesDataFrame(sl = SpatialLines(list(Lines(Line(matrix(0, ncol = 2)), ID = 1))), data = data.frame("direction" = "A to B"), match.ID = FALSE)
            raster::crs(sPath) <- raster::crs(cost_surface)
        }
        
        if (cost_distance) {
            if(inherits(sPath, "try-error")) {
                sPath$cost <- NA 
            } else { 
                sPath$cost <- as.vector(gdistance::costDistance(cost_surface, origin, destination))
            }
        }
        
        sPath$direction <- "A to B"
        
    } else {
        
        sPaths <- list()
        
        sPath1 <- try(suppressWarnings(gdistance::shortestPath(cost_surface, origin, destination, output = "SpatialLines")), silent = TRUE)
        if(inherits(sPath1, "try-error")) {
            warning("least cost path from origin to destination could not be calculated. Returning empty SpatialLinesDataFrame")
            sPath1 <- SpatialLinesDataFrame(sl = SpatialLines(list(Lines(Line(matrix(0, ncol = 2)), ID = 1))), data = data.frame("direction" = "A to B"), match.ID = FALSE)
            raster::crs(sPath1) <- raster::crs(cost_surface)
        }
        
        sPath2 <- try(suppressWarnings(gdistance::shortestPath(cost_surface, destination, origin, output = "SpatialLines")), silent = TRUE)
        if(inherits(sPath2, "try-error")) {
            warning("least cost path from destination to origin could not be calculated. Returning empty SpatialLinesDataFrame")
            sPath2 <- SpatialLinesDataFrame(sl = SpatialLines(list(Lines(Line(matrix(0, ncol = 2)), ID = 1))), data = data.frame("direction" = "B to A"), match.ID = FALSE)
            raster::crs(sPath2) <- raster::crs(cost_surface)
        }
        
        sPaths[[1]] <- sPath1
        sPaths[[2]] <- sPath2
        
        if (cost_distance) {
            
            if(inherits(sPath1, "try-error")) {
                sPaths[[1]]$cost <- NA
            } else {
                sPaths[[1]]$cost <- as.vector(gdistance::costDistance(cost_surface, origin, destination))  
            }
            
            if(inherits(sPath2, "try-error")) {
                sPaths[[2]]$cost <- NA
            } else {
                sPaths[[2]]$cost <- as.vector(gdistance::costDistance(cost_surface, destination, origin))  
            }
            
        }
        
        sPaths[[1]]$direction <- "A to B"
        sPaths[[2]]$direction <- "B to A"
        
        sPath <- do.call(rbind, sPaths)
    }
    
    return(sPath)
}
