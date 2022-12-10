#' Calculate Least-cost Path from Origin to Destinations
#' 
#' Calculates the Least-cost path from an origin location to one or more destination locations. Applies Dijkstra's algorithm as implemented in the igraph R package
#' 
#' @param x \code{conductanceMatrix} 
#' 
#' @param origin \code{sf} 'POINT' or 'MULTIPOINT', \code{SpatVector}, \code{data.frame} or \code{matrix} containing the origin coordinates. Only the first row of the supplied object is used as the origin.
#' 
#' @param destination \code{sf} 'POINT' or 'MULTIPOINT', \code{SpatVector}, \code{data.frame} or \code{matrix} containing the destination coordinates. If the object contains multiple coordinates then least-cost paths will be calculated from the origin to all destinations
#' 
#' @param cost_distance \code{logical} if TRUE computes total accumulated cost from origin to the destinations. FALSE (default)
#' 
#' @author Joseph Lewis
#' 
#' @return \code{sf}  Least-cost path from origin and destinations based on the supplied \code{conductanceMatrix} 
#' 
#' @export
#' 
#' @examples 
#' 
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' 
#' slope_cs <- create_slope_cs(x = r, cost_function = "tobler", neighbours = 4)
#' 
#' locs <- sf::st_sf(geometry = sf::st_sfc(
#' sf::st_point(c(839769, 4199443)),
#' sf::st_point(c(1038608, 4100024)),
#' sf::st_point(c(1017819, 4206255)),
#' crs = terra::crs(r)))
#' 
#' lcp <- create_lcp(x = slope_cs, origin = locs[1,], destination = locs[2:3,], 
#' cost_distance = TRUE)

create_lcp <- function(x, origin, destination, cost_distance = FALSE) {
  
  cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, xmin = x$extent[1], xmax = x$extent[2], ymin = x$extent[3], ymax = x$extent[4],crs = x$crs)
  
  from_coords <- get_coordinates(origin)
  to_coords <- get_coordinates(destination)
  
  from_cell <- terra::cellFromXY(cs_rast, from_coords[1,, drop = FALSE])
  to_cell <- terra::cellFromXY(cs_rast, to_coords)
  
  cm_graph <- igraph::graph_from_adjacency_matrix(x$conductanceMatrix, mode = "directed", weighted = TRUE)
  
  igraph::E(cm_graph)$weight <- (1/igraph::E(cm_graph)$weight)
  
  lcp_graph <- igraph::shortest_paths(cm_graph, from = from_cell, to = to_cell, mode = "out")
  
  lcps <- lapply(lcp_graph$vpath, FUN = function(i) { 
    
    lcp_xy <- terra::xyFromCell(cs_rast, as.integer(i))
    lcp <- sf::st_sf(geometry = sf::st_sfc(sf::st_linestring(lcp_xy)), crs = x$crs)
    return(lcp)
  }
  )
  
  lcps <- do.call(rbind, lcps)
  
  if(!is.function(x$costFunction)) { 
    lcps$costFunction <- x$costFunction
  } else if (is.function(x$costFunction)) { 
    lcps$costFunction <- deparse(body(x$costFunction)[[2]])
  }
  
  lcps$fromCell <- from_cell
  lcps$toCell <- to_cell
  
  if (cost_distance) {
    cost <- igraph::distances(graph = cm_graph, v = from_cell, to = to_cell, mode = "out")
    lcps <- transform(lcps, cost_distance = as.numeric(cost))
  }
  
  return(lcps)
}
