#' Coerce an anisotropic cost surface to an isotropic cost surface
#' 

force_isotropy <- function(x) { 
  
  adj <- which(x$conductanceMatrix!=0,arr.ind=TRUE)
  x$conductanceMatrix[adj] <- rowMeans(cbind(x$conductanceMatrix[adj[,1:2]], x$conductanceMatrix[adj[,2:1]]))
  
  return(x)

}