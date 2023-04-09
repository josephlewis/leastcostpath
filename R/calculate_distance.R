#' calculate distance between adjacent cells
#' 
#' @details 
#' 
#' calculate_distance function allows for both projected and geographic coordinate systems. If the coordinate system is geographic (e.g. wgs84) the distance is calculated using the sf::st_distance function else distance calculated using Pythagorean theorem  
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
  
  if(sf::st_is_longlat(x)) { 

  xy1 <- data.frame(terra::xyFromCell(x, adj[, 1]))
  xy2 <- data.frame(terra::xyFromCell(x, adj[, 2]))
  
  xy1 <- sf::st_as_sf(xy1, coords = c("x", "y"), crs = terra::crs(x))
  xy2 <- sf::st_as_sf(xy2, coords = c("x", "y"), crs = terra::crs(x))
  
  dist <- as.vector(sf::st_distance(x = xy1, xy2, by_element = TRUE))
  
  } else {
    
    xy1 <- terra::xyFromCell(x, adj[, 1])
    xy2 <- terra::xyFromCell(x,adj[, 2])
    
    xy3 <- (xy1[,1] - xy2[,1])^2
    xy4 <- (xy1[,2] - xy2[,2])^2
    
    dist <- sqrt(xy3 + xy4)  
    
  }

  return(dist)
  
}
