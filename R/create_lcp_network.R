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
#' @import gdistance
#'
#' @export
#'
#'@examples
#' r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
#'
#' slope_cs <- create_slope_cs(r, cost_function = 'tobler')
#'
#' traverse_cs <- create_traversal_cs(r, traversal = 'asymmetrical')
#'
#' final_cost_cs <- slope_cs * traverse_cs
#'
#' loc1 = cbind(2667670, 6479000)
#' loc1 = sp::SpatialPoints(loc1)
#'
#' loc2 = cbind(2667800, 6479400)
#' loc2 = sp::SpatialPoints(loc2)
#'
#' lcps <- create_lcp_network(cost_surface, vertices, graph = 'delauney', cost_distance = FALSE)
#'
#' plot(raster(final_cost_cs))
#' plot(lcps, add = T)
#' plot(lcps[1,], add = T, col = 'red')

create_lcp_network <- function(cost_surface, vertices, graph = "delauney", cost_distance = FALSE) {
    
    if (!inherits(cost_surface, "TransitionLayer") & !inherits(vertices, "SpatialPointsDataFrame") | !inherits(vertices, "SpatialPoints")) 
        stop("Invalid objects supplied. cost_surface expects TransitionLayer object and vertices expects SpatialPoints or SpatialPointsDataFrame object")
    
    if (graph != "delauney" & graph != "none") 
        stop("Invalid input supplied. graph expecting 'delauney' or 'none'.")
    
    if (graph == "delauney") {
        
        print("delauney")
        
        vertices <- sp::remove.duplicates(vertices)
        
        co <- coordinates(vertices)
        coords <- as.matrix(coordinates(vertices))
        ids <- row.names(as.data.frame(vertices))
        neighbours <- spdep::tri2nb(co, row.names = ids)
        network <- spdep::nb2lines(neighbours, coords = coords, proj4string = crs(vertices))
        network <- as(data.frame(network[1:2]), "matrix")
        
    } else if (graph == "none") {
        
        network <- as(expand.grid(seq_along(vertices), seq_along(vertices)), "matrix")
        
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
