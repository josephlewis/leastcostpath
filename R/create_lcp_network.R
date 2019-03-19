create_lcp_network <- function(cost_surface, nodes, graph = "delauney", cost_distance = FALSE) {
    
    
    if (!inherits(cost_surface, "TransitionLayer") & !inherits(nodes, "SpatialPointsDataFrame") | !inherits(nodes, "SpatialPoints")) 
        stop("Invalid objects supplied. cost_surface expects TransitionLayer object and nodes expects SpatialPoints or SpatialPointsDataFrame object")
    
    if (graph != "delauney" & graph != "none") 
        stop("Invalid input supplied. graph expecting 'delauney' or 'none'.")
    
    if (graph == "delauney") {
        
        print("delauney")
        
        nodes <- sp::remove.duplicates(nodes)
        
        co <- coordinates(nodes)
        coords <- as.matrix(coordinates(nodes))
        ids <- row.names(as.data.frame(nodes))
        neighbours <- spdep::tri2nb(co, row.names = ids)
        network <- spdep::nb2lines(neighbours, coords = coords, proj4string = crs(nodes))
        network <- as(data.frame(network[1:2]), "matrix")
        
    } else if (graph == "none") {
        
        network <- as(expand.grid(seq_along(nodes), seq_along(nodes)), "matrix")
        
        network <- network[network[, 1] != network[, 2], ]
        
    }
    
    lcp_network <- apply(network, MARGIN = 1, function(x) {
        gdistance::shortestPath(cost_surface, nodes[x[1], ], nodes[x[2], ], output = "SpatialLines")
    })
    
    lcp_network <- do.call(rbind, lcp_network)
    
    lcp_network <- SpatialLinesDataFrame(lcp_network, data.frame(1:length(lcp_network)), match.ID = FALSE)
    
    if (cost_distance) {
        
        cost_dist <- apply(network, MARGIN = 1, function(x) {
            gdistance::costDistance(cost_surface, nodes[x[1], ], nodes[x[2], ])
        })
        
        lcp_network <- SpatialLinesDataFrame(lcp_network, data.frame(cost_dist), match.ID = FALSE)
        
    }
    
    return(lcp_network)
    
}
