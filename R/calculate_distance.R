#' calculates distance between adjacent cells
#' 
#' @param x \code{spatRaster} 
#' 
#' @param adj \code{matrix} of adjacent cells
#' 
#' @author Joseph Lewis
#' 
#' @return \code{matrix} euclidean distances between adjacent cells
#' 
#' @export
 
calculate_distance <- function(x, adj) { 

  xy1 <- data.frame(terra::xyFromCell(x, adj[, 1]))
  xy2 <- data.frame(terra::xyFromCell(x, adj[, 2]))
  
  xy1 <- sf::st_as_sf(xy1, coords = c("x", "y"), crs = terra::crs(x))
  xy2 <- sf::st_as_sf(xy2, coords = c("x", "y"), crs = terra::crs(x))
  
  dist <- as.vector(sf::st_distance(x = xy1, xy2, by_element = TRUE))

  return(dist)
  
}
