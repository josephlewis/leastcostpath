#' Incorporate vertical error into a Digital Elevation Model
#' 
#' @param x \code{spatRaster}
#'
#' @param rmse \code{numeric}. Vertical Root Mean Square Error of the Digital Elevation Model
#' 
#' @details
#' Digital Elevation Models (DEMs) are representations of the earth's surface and are subject to error (Wechsler, 1999).
#'
#' The add_dem_error function incorporates vertical error into the supplied DEM by assuming that the error for each cell follows a gaussian (normal) distribution around the measured elevation value and the global Root Mean Square Error (RMSE) estimating the local error variance around this values (Fisher and Tate, 2006). Addition of spatial autocorrelation applied by using a mean-window filter based on a window size (Wechsler and Kroll, 2006).
#'
#' Examples of RMSE for various datasets:
#'
#' Shuttle Radar Topography Mission (SRTM) has a RMSE of 9.73m
#'
#' Advanced Spaceborne Thermal Emission and Reflection Radiometer (ASTER) Global Digital Elevation Model (GDEM) has a RMSE of 10.20m
#'
#' Ordnance Survey OS Terrain 5 has a maximum RMSE of 2.5m
#'
#' Ordnance Survey OS Terrain 50 has a maximum RMSE of 4m
#' 
#' TINITALY DEM has a RMSE of 4.3m
#' 
#' @author Joseph Lewis
#' 
#' @export
#' 
#' @examples 
#' 
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' 
#' r2 <- add_dem_error(x = r, rmse = 4.3)

add_dem_error <- function(x, rmse) { 

  dem_error <- x
  dem_error[] <- stats::rnorm(n = terra::ncell(x), mean = 0, sd = abs(rmse))
  
  dem_df <- sf::st_as_sf(as.data.frame(x,xy=TRUE, na.rm = TRUE), coords=1:2)
  
  vario <- gstat::variogram(dem_df[[1]] ~ 1, data = dem_df)
  fit = gstat::fit.variogram(vario, gstat::vgm(c("Exp", "Mat", "Sph", "Gau")))
  
  window <- round(fit$range[2]/max(terra::res(x)))
  window <- ifelse(test = (window%%2) != 0, yes = window, no = window + 1)

  message("Variogram model = ", fit$model[2], ", ", "size of window = ", window)
  
  dem_error <- terra::focal(x = dem_error, w = matrix(1/window, nrow = window, ncol = window), na.rm = TRUE, pad = TRUE)
  dem_error[] <- base::scale(terra::values(dem_error)) * rmse
  
  x <- x + dem_error
  
  return(x)
  
}
