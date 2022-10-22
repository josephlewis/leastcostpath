#' Calculates mean standard deviation of slope values
#' 
#' Calculates mean standard deviation of slope values from cells that coincide with the straight line from the origin to the destination.
#' 
#' @param x \code{SpatRaster} Digital Elevation Model (DEM)
#'
#' @param neighbours \code{numeric} value. Number of directions used in the conductance matrix calculation. Expected numeric values are 4, 8, 16, 32, 48, or matrix object. 16 (default)
#' 
#' @param origin \code{sf} of geometry type 'POINT' or 'MULTIPOINT'
#' 
#' @param destination \code{sf} of geometry type 'POINT' or 'MULTIPOINT'
#' 
#' @author Joseph Lewis
#' 
#' @return \code{numeric} value of the mean standard deviation of slope values that coincide with the straight line from the origin to the destination 
#' 
#' @export
#' 
#' @examples
#' 
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' 
#' locs <- sf::st_sf(geometry = sf::st_sfc(
#' sf::st_point(c(839769, 4199443)),
#' sf::st_point(c(1038608, 4100024)),
#' crs = terra::crs(r)))
#' 
#' calculate_slope_variance(x = r, neighbours = 4, origin = locs[1,], destination = locs[2,])

calculate_slope_variance <- function(x, neighbours, origin, destination) { 
  
  neighbours <- neighbourhood(neighbours = neighbours)
  
  coords = rbind(sf::st_coordinates(origin), sf::st_coordinates(destination))
  line_from_coords <- sf::st_sfc(sf::st_linestring(coords), crs = terra::crs(x))
  
  terra_vect <- terra::vect(line_from_coords)
  cells <- terra::cells(x, terra_vect)[, 2]
  na_cells <- which(is.na(terra::values(x)))
  
  adj <- terra::adjacent(x = x, cells = cells, directions = neighbours, pairs = TRUE)
  adj <- adj[!adj[, 2] %in% na_cells, ]
  elev_values <- terra::values(x)[, 1]
  message("calculating slope...")
  rise <- (elev_values[adj[, 2]] - elev_values[adj[, 1]])
  run <- calculate_distance(x = x, adj = adj)
  mathematical_slope <- rise/run
  
  adj_vals <- cbind(adj, mathematical_slope)
  
  adj_vals_var <- stats::aggregate(adj_vals[,3], by = list(adj_vals[,2]), FUN = function(x) { stats::sd(x)})
  
  # If variance calculated using a single value then NA returned. NA values replaced with zero.
  adj_vals_var$x[is.na(adj_vals_var$x)] <- 0
  
  mean_var <- mean(adj_vals_var$x, na.rm = TRUE)
  
  return(mean_var)
  
  }