#' Calculate Least Cost Path from Origin to Destination
#' 
#' @author Joseph Lewis
#'
#' @export
#'

create_lcp <- function(x, origin, destination, directional = FALSE, cost_distance = FALSE) { 
    
    cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, extent = x$extent, crs = x$crs)
    
    from_coords <- sf::st_coordinates(origin)[, 1:2, drop = FALSE]
    to_coords <- sf::st_coordinates(destination)[, 1:2, drop = FALSE]
    
    from_cell <- terra::cellFromXY(cs_rast, from_coords)
    to_cell <- terra::cellFromXY(cs_rast, to_coords)
    
    cm_graph <- igraph::graph_from_adjacency_matrix(x$conductanceMatrix, mode = "directed", weighted = TRUE)
    
    igraph::E(cm_graph)$weight <- (1/igraph::E(cm_graph)$weight)
    
    lcp_graph <- igraph::shortest_paths(cm_graph, from = from_cell, to = to_cell, mode = "out")
    lcp_cells <- unlist(lcp_graph$vpath)
    lcp_xy <- terra::xyFromCell(cs_rast, lcp_cells)
    lcp <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(lcp_xy)))
    lcp$costFunction <- x$costFunction
    lcp$fromCell <- from_cell
    lcp$toCell <- to_cell
    
    if (cost_distance) {
        cost <- igraph::distances(graph = cm_graph, v = from_cell, to = to_cell, mode = "out")
        lcp <- transform(lcp, cost_distance = cost)
    }
    
    return(lcp)
}