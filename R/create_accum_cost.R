#' creates an accumulated cost surface
#' 
#' Creates an accumulated cost surfaces from an origin
#'  
#' @param x \code{SpatRaster}
#' 
#' @param origin \code{sf} 'POINT' or 'MULTIPOINT', \code{SpatVector}, \code{data.frame} or \code{matrix} containing the origin coordinates. Only the first row of the supplied object is used as the origin.
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
#' loc <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(839769, 4199443)),crs = terra::crs(r)))
#' 
#' cc <- create_accum_cost(x = slope_cs, origin = loc, rescale = TRUE)

create_accum_cost <- function(x, origin, rescale = FALSE) { 
  
  cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, xmin = x$extent[1], xmax = x$extent[2], ymin = x$extent[3], ymax = x$extent[4],crs = x$crs)
  
  from_coords <- get_coordinates(origin)
  from_cell <- terra::cellFromXY(cs_rast, from_coords[1,, drop = FALSE])
  
  cm_graph <- igraph::graph_from_adjacency_matrix(x$conductanceMatrix, mode = "directed", weighted = TRUE)
  
  igraph::E(cm_graph)$weight <- (1/igraph::E(cm_graph)$weight)
  
  from_distances <- igraph::distances(cm_graph, v = from_cell,  mode="out")
  
  accum_rast <- terra::setValues(cs_rast, as.numeric(from_distances))
  
  accum_rast[is.infinite(accum_rast)] <- NA
  
  if(rescale) { 
    rast_min <- terra::minmax(accum_rast)[1]
    rast_max <- terra::minmax(accum_rast)[2]
    
    accum_rast <- ((accum_rast - rast_min)/(rast_max - rast_min)) 
  }
  
  return(accum_rast)
  
}
