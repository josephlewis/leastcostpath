#' create_lcp_network
#'
#' Calculates a Least Cost Path network which are often, but not exclusively, used in archaeological research.
#'
#' The function computes the Least Cost Path network. It uses the cost surface(s) generated using the create_slope_cs, create_traversal_cs and or create_openness_cs functions. The function takes a Cost Surface ('TransitionLayer' class) and vertices features ('SpatialPoints' class) for the locations.
#'
#' @param cost_surface Cost Surface. Expects Object of class TransitionLayer.
#'
#' @param vertices Location from which the Least Cost Paths are calculated. Expects Object of class SpatialPoints
#'
#' @param graph graph type used to select which vertices to use. Current implementation accepts 'delauney' (default) or 'none'.
#'
#' @param cost_distance computes total cost from each location to destination within the network. Results added to each Least Cost Path within the network. Default is FALSE.
#'
#' @author Joseph Lewis
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import spdep
#' @import gdistance
#'
#' @export
#'

create_lcp_network <- function(cost_surface, vertices, graph = "none", cost_distance = FALSE) {
    
    if (!inherits(cost_surface, "TransitionLayer") & !inherits(vertices, "SpatialPointsDataFrame") | !inherits(vertices, "SpatialPoints")) 
        stop("Invalid objects supplied. cost_surface expects TransitionLayer object and vertices expects SpatialPoints or SpatialPointsDataFrame object")
    
    if (graph == "none") {
        
        network <- expand.grid(seq_along(vertices), seq_along(vertices))
        
        network <- network[network[, 1] != network[, 2], ]
        
    }
    
    lcp_network <- apply(network, MARGIN = 1, function(x) {
        gdistance::shortestPath(cost_surface, vertices[x[1], ], vertices[x[2], ], output = "SpatialLines")
    })
    
    lcp_network <- do.call(rbind, lcp_network)
    
    lcp_network <- SpatialLinesDataFrame(lcp_network, data.frame(1:length(lcp_network)), match.ID = FALSE)
    
    if (cost_distance) {
        
        cost_dist <- apply(network, MARGIN = 1, function(x) {
            gdistance::costDistance(cost_surface, vertices[x[1], ], vertices[x[2], ])
        })
        
        lcp_network <- SpatialLinesDataFrame(lcp_network, data.frame(cost_dist), match.ID = FALSE)
        
    }
    
    return(lcp_network)
    
}
