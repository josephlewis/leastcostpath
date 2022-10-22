calculate_distance <- function(x, adj) { 
  
  xy1 <- terra::xyFromCell(x, adj[, 1])
  xy2 <- terra::xyFromCell(x,adj[, 2])
  
  xy3 <- (xy1[,1] - xy2[,1])^2
  xy4 <- (xy1[,2] - xy2[,2])^2
  
  dist <- sqrt(xy3 + xy4)
  
  return(dist)
  
}