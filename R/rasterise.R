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
#' r <- terra::rast(system.file("ex/test.grd", package="terra"))
#' 
#' slope_cs <- create_slope_cs(x = r, cost_function = "tobler")
#' 
#' rast <- rasterise(slope_cs)

rasterise <- function(x) { 
  
  cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, extent = x$extent, crs = x$crs)
  
  col_sum <- Matrix::colSums(x$conductanceMatrix)
  row_sum <- Matrix::rowSums(x$conductanceMatrix)
  
  logical_sm <- methods::as(x$conductance, "lMatrix")
  
  ncols <- Matrix::colSums(logical_sm)
  nrows <- Matrix::rowSums(logical_sm)
  
  vals <- ((col_sum / ncols) + (row_sum / nrows)) / 2
  
  cs_rast <- terra::setValues(cs_rast, vals)
  
  return(cs_rast)

}
