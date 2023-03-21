#' creates an accumulated cost surface
#' 
#' Creates an accumulated cost surfaces from one or more origins
#'  
#' @param x \code{SpatRaster}
#' 
#' @param origins \code{sf} 'POINT' or 'MULTIPOINT', \code{SpatVector}, \code{data.frame} or \code{matrix} containing the origins coordinates. If multiple origins are supplied then the multiple accumulated cost surfaces will be summarised using the FUN argument
#' 
#' @param FUN \code{function} Apply a function to the cells of a SpatRaster. Default applied function is 'mean'. See terra::app() for more information
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
#' sf::st_point(c(907695, 4145478)),
#' crs = terra::crs(r)))
#' 
#' cc <- create_accum_cost(x = slope_cs, origins = locs, FUN = mean, rescale = FALSE)

create_accum_cost <- function(x, origins, FUN = mean, rescale = FALSE) {
  
  cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, xmin = x$extent[1], xmax = x$extent[2], ymin = x$extent[3], ymax = x$extent[4],crs = x$crs)
  
  from_coords <- get_coordinates(origins)
  from_cell <- terra::cellFromXY(cs_rast, from_coords)
  
  cm_graph <- igraph::graph_from_adjacency_matrix(x$conductanceMatrix, mode = "directed", weighted = TRUE)
  
  igraph::E(cm_graph)$weight <- (1/igraph::E(cm_graph)$weight)
  
  from_distances <- igraph::distances(cm_graph, v = from_cell,  mode="out", algorithm = "dijkstra")
  
  accum_rasts <- c(rep(cs_rast, nrow(from_distances)))
  
  for(i in 1:terra::nlyr(accum_rasts))  {
    
    accum_rasts[[i]] <- terra::setValues(accum_rasts[[i]], from_distances[i,])
    
  }
  
  accum_rast <- terra::app(accum_rasts, fun = FUN)
  
  accum_rast[is.infinite(accum_rast)] <- NA
  
  if(rescale) { 
    rast_min <- terra::minmax(accum_rast)[1]
    rast_max <- terra::minmax(accum_rast)[2]
    
    accum_rast <- ((accum_rast - rast_min)/(rast_max - rast_min)) 
  }
  
  return(accum_rast)
  
}
