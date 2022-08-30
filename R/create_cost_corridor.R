#' creates a cost corridor
#' 
#' Combines the accumulated cost surfaces from origin-to-destination and destination-to-origin to identify areas of preferential movement that takes into account both directions of movement
#'  
#' @param x \code{SpatRaster}
#' 
#' @param origin \code{sf} of geometry type 'POINT' or 'MULTIPOINT'
#' 
#' @param destination \code{sf} of geometry type 'POINT' or 'MULTIPOINT'
#' 
#' @param rescale \code{logical}. if TRUE, values scaled to between 0 and 1. FALSE (default)
#' 
#' @return \code{SpatRaster}
#' 
#' @author Joseph Lewis
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
#' crs = terra::crs(r)))
#' 
#' cc <- create_cost_corridor(x = slope_cs, origin = locs[1,], destination = locs[2,], rescale = TRUE)

create_cost_corridor <- function(x, origin, destination, rescale = FALSE) { 
  
  cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, extent = x$extent, crs = x$crs)
  
  from_coords <- sf::st_coordinates(origin)[1, 1:2, drop = FALSE]
  to_coords <- sf::st_coordinates(destination)[1, 1:2, drop = FALSE]
  
  from_cell <- terra::cellFromXY(cs_rast, from_coords)
  to_cell <- terra::cellFromXY(cs_rast, to_coords)
  
  cm_graph <- igraph::graph_from_adjacency_matrix(x$conductanceMatrix, mode = "directed", weighted = TRUE)
  
  igraph::E(cm_graph)$weight <- (1/igraph::E(cm_graph)$weight)
  
  from_distances <- igraph::distances(cm_graph, v = from_cell,  mode="out")
  to_distances <- igraph::distances (cm_graph, v= to_cell, mode="out")
  
  from_rast <- terra::setValues(cs_rast, as.numeric(from_distances))
  to_rast <- terra::setValues(cs_rast, as.numeric(to_distances))
  
  costCorridor <- from_rast + to_rast
  
  costCorridor[is.infinite(costCorridor)] <- NA
  
  if(rescale) { 
    rast_min <- terra::minmax(costCorridor)[1]
    rast_max <- terra::minmax(costCorridor)[2]
    
    costCorridor <- ((costCorridor - rast_min)/(rast_max - rast_min)) 
  }
  
  return(costCorridor)

}
