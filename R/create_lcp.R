#' Calculate Least-cost Path from Origin to Destination
#' 
#' Calculates the Least-cost path from an origin location to a destination location. Applies Dijkstra's algorithm as implemented in igraph
#' 
#' @param x \code{conductanceMatrix} 
#' 
#' @param origin \code{sf} of geometry type 'POINT' or 'MULTIPOINT'
#' 
#' @param destination \code{sf} of geometry type 'POINT' or 'MULTIPOINT'
#' 
#' @param cost_distance \code{logical} if TRUE computes total accumulated cost from origin to destination. FALSE (default)
#' 
#' @author Joseph Lewis
#' 
#' @return \code{sf}  Least-cost path from origin and destination based on the supplied \code{conductanceMatrix} 
#' 
#' @export

create_lcp <- function(x, origin, destination, cost_distance = FALSE) {
    
    cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, extent = x$extent, crs = x$crs)
    
    from_coords <- sf::st_coordinates(origin)[1, 1:2, drop = FALSE]
    to_coords <- sf::st_coordinates(destination)[1, 1:2, drop = FALSE]
    
    from_cell <- terra::cellFromXY(cs_rast, from_coords)
    to_cell <- terra::cellFromXY(cs_rast, to_coords)
    
    cm_graph <- igraph::graph_from_adjacency_matrix(x$conductanceMatrix, mode = "directed", weighted = TRUE)
    
    igraph::E(cm_graph)$weight <- (1/igraph::E(cm_graph)$weight)
    
    lcp_graph <- igraph::shortest_paths(cm_graph, from = from_cell, to = to_cell, mode = "out")
    lcp_cells <- unlist(lcp_graph$vpath)
    lcp_xy <- terra::xyFromCell(cs_rast, lcp_cells)
    lcp <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(lcp_xy)), crs = x$crs)
    lcp$costFunction <- x$costFunction
    lcp$fromCell <- from_cell
    lcp$toCell <- to_cell
    
    if (cost_distance) {
        cost <- igraph::distances(graph = cm_graph, v = from_cell, to = to_cell, mode = "out")
        lcp <- transform(lcp, cost_distance = cost)
    }
    
    return(lcp)
}