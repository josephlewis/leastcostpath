#' converts conductanceMatrix to SpatRaster
#' 
#' @param x \code{conductanceMatrix}
#' 
#' @author Joseph Lewis
#' 
#' @return \code{spatRaster} 
#' 
#' @export
#' 
#' @examples 
#' 
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' 
#' slope_cs <- create_slope_cs(x = r, cost_function = "tobler", neighbours = 4)
#' 
#' cs_rast <- rasterise(slope_cs)

rasterise <- function(x) { 
  
  cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, xmin = x$extent[1], xmax = x$extent[2], ymin = x$extent[3], ymax = x$extent[4],crs = x$crs)
  
  col_sum <- Matrix::colSums(x$conductanceMatrix)
  row_sum <- Matrix::rowSums(x$conductanceMatrix)
  
  logical_sm <- methods::as(x$conductance, "lMatrix")
  
  ncols <- Matrix::colSums(logical_sm)
  nrows <- Matrix::rowSums(logical_sm)
  
  vals <- ((col_sum / ncols) + (row_sum / nrows)) / 2
  
  cs_rast <- terra::setValues(cs_rast, vals)
  
  return(cs_rast)

}
