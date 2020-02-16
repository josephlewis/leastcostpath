#' create_lcp_network
#'
#' Calculates a Least Cost Path network which are often, but not exclusively, used in archaeological research.
#'
#' The function computes the Least Cost Path network. It uses a cost surface generated using the functions within the leastcostpath library. The create_lcp_network function expects a Cost Surface ('TransitionLayer' class) and SpatialPoints ('SpatialPoints' class) for the locations.
#'
#' @param cost_surface Cost Surface. Expects Object of class TransitionLayer.
#'
#' @param  locations from which the Least Cost Paths are calculated. Expects Object of class SpatialPoints
#'
#' @param cost_distance computes total cost from each location to destination within the network. Results added to each Least Cost Path within the network. Default is FALSE.
#'
#'@param parallel if TRUE, the LCPs will be calculated in parallel. Number of Parallel socket clusters is total number of cores minus 1. Default is FALSE
#'
#' @author Joseph Lewis
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import gdistance
#' @import parallel
#' @import pbapply
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
#'locs <- sp::spsample(as(r, 'SpatialPolygons'),n=10,'regular')
#'
#' lcp_network <- create_lcp_network(cost_surface = final_cost_cs, locations = locs,
#' cost_distance = FALSE, parallel = FALSE)

create_lcp_network <- function(cost_surface, locations, cost_distance = FALSE, parallel = FALSE) {
    
    if (!inherits(cost_surface, "TransitionLayer")) {
        stop("cost_surface expects a TransitionLayer object")
    }
    
    if (!inherits(locations, "SpatialPoints")) {
        stop("locations expects a SpatialPoints object")
    }
    
    if (length(locations) < 1) 
        stop("Locations must contain more than one Point")
    
    network <- (expand.grid(seq_along(locations), seq_along(locations)))
    
    network <- network[network[, 1] != network[, 2], ]
    
    if (parallel) {
        
        no_cores <- parallel::detectCores() - 1
        
        cl <- parallel::makeCluster(no_cores)
        
        parallel::clusterExport(cl, varlist = c("cost_surface", "locations"), envir = environment())
        
        lcp_network <- pbapply::pbapply(network, MARGIN = 1, function(x) {
            gdistance::shortestPath(cost_surface, locations[x[1], ], locations[x[2], ], output = "SpatialLines")
        }, cl = cl)
        
        parallel::stopCluster(cl)
        
    } else {
        
        lcp_network <- pbapply::pbapply(network, MARGIN = 1, function(x) {
            gdistance::shortestPath(cost_surface, locations[x[1], ], locations[x[2], ], output = "SpatialLines")
        })
        
    }
    
    lcp_network <- do.call(rbind, lcp_network)
    
    lcp_network <- SpatialLinesDataFrame(lcp_network, data.frame(1:length(lcp_network)), match.ID = FALSE)
    
    if (cost_distance) {
        
        cost_dist <- apply(network, MARGIN = 1, function(x) {
            gdistance::costDistance(cost_surface, locations[x[1], ], locations[x[2], ])
        })
        
        lcp_network <- SpatialLinesDataFrame(lcp_network, data.frame(cost_dist), match.ID = FALSE)
        
    }
    
    return(lcp_network)
    
}
