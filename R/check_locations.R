check_locations <- function(x, locations) { 
  
  cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, extent = x$extent, crs = x$crs)
  
  coords <- sf::st_coordinates(locations)[, 1:2, drop = FALSE]
  cells <- terra::cellFromXY(cs_rast, coords)
  cells <- cbind(cells[!is.nan(cells)])
  
  connectivity_list <- apply(X = cells, MARGIN = 1, FUN = function(j) { all(x$conductanceMatrix[,j] == 0)})
  connected <- sum(connectivity_list)
  
  message(nrow(locations), " locations were supplied")
  message(nrow(cells) - connected, " location(s) are traversable from at least one adjacent cell")
  message(nrow(locations) - nrow(cells), " location(s) are outside the extent of the conductanceMatrix")
  
}