#' Coerce an anisotropic cost surface to an isotropic cost surface
#' 
#' Averages conductance values from-to adjacent cells
#' 
#' @param x \code{conductanceMatrix} 
#' 
#' @export
#' 
#' @return \code{conductanceMatrix} 
#' 
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' 
#' slope_cs_aniso <- create_slope_cs(x = r, cost_function = "tobler", neighbours = 4)
#' 
#' slope_cs_iso <- force_isotropy(x = slope_cs_aniso)

force_isotropy <- function(x) { 
  
  adj <- as.matrix(Matrix::summary(x$conductanceMatrix))
  x$conductanceMatrix[adj[,1:2]] <- Matrix::rowMeans(cbind(x$conductanceMatrix[adj[,1:2]], x$conductanceMatrix[adj[,2:1]]))
  
  return(x)

}