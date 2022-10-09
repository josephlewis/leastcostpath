#' Calculates the RMSE of a Digial Elevation Model after aggregation
#'
#' @param x \code{SpatRaster} original Digital Elevation Model
#' 
#' @param y \code{SpatRaster} aggregated Digital Elevation Model
#' 
#' @param x_rmse \code{numeric} RMSE of original Digital Elevation Model before aggregation 
#'  
#' @author Joseph Lewis
#'
#' @export
#' 
#' @examples 
#' 
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' 
#' r2 <- terra::aggregate(r, 10)
#' 
#' calculate_rmse(x = r, y = r2, x_rmse = 0)

calculate_rmse <- function(x, y, x_rmse = 0) { 
  
  xy <- terra::xyFromCell(object = x, cell = 1:terra::ncell(x))
  actual_vals <- x[xy][,1]
  aggregated_vals <- y[xy][,1]
  
  vals <- cbind(actual_vals, aggregated_vals)
  vals <- stats::na.omit(vals)
  
  rmse <- sqrt(mean((vals[,1] - vals[,2])^2))
  
  if(x_rmse != 0) { 
    rmse <- rmse + x_rmse
  }
  
  return(rmse)
  
  
}
