#' Incorporate vertical error into a Digital Elevation Model
#' 
#' @param x \code{spatRaster}
#'
#' @param rmse \code{numeric}. Vertical Root Mean Square Error of the Digital Elevation Model
#' 
#' @param type \code{character} type 'u' (unfiltered), 'n' (neighbourhood autocorrelation), and 'd' (mean spatial dependence) implemented. See details for more information
#' 
#' @param samples \code{numeric} number of random spatial data locations sampled when using type 'd'. This can be used to overcome issues with computing time and memory limits
#' 
#' @details
#' Digital Elevation Models (DEMs) are representations of the earth's surface and are subject to error (Wechsler and Kroll, 2006)
#'
#' The add_dem_error function incorporates vertical error into the supplied DEM. Three methods are implemented:
#' 
#' Unfiltered: Random error based on DEM RMSE range. Autocorrelation between random error is not accounted for. This can be interpreted as the worst case scenario
#' 
#' Neighbourhood autocorrelation: Random error is spatially autocorrelated by passing a mean low pass filter in a 3x3 neighbourhood over the surface
#' 
#' Mean Spatial Dependence: Random error is spatially autocorrelated by passing a DxD kernel over each cell. The centre cell of each kernel is replaced by the mean of the surrounding DxD cells. Distance of spatial dependence (D) is estimated by calcualting the semi-variogram nugget using the gstat package  
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
#' @references 
#' 
#' Wechsler, S. P., & Kroll, C. N. (2006). Quantifying DEM Uncertainty and its Effect on Topographic Parameters. Photogrammetric Engineering & Remote Sensing, 72(9), 1081-1090. https://doi.org/10.14358/PERS.72.9.1081
#' 
#' Fisher, P., & Tate, N. J. (2006). Causes and consequences of error in digital elevation models. Progress in Physical Geography: Earth and Environment, 30(4), 467-489. https://doi.org/10.1191/0309133306pp492ra
#' 
#' @author Joseph Lewis
#' 
#' @export
#' 
#' @examples 
#' 
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' 
#' r2 <- add_dem_error(x = r, rmse = 4.3, type = "u")

add_dem_error <- function(x, rmse, type = "u", samples = NULL) { 
  
  dem_error <- x
  dem_error[] <- stats::rnorm(n = terra::ncell(x), mean = 0, sd = abs(rmse))
  
  if(type == "u") {
    dem_error <- dem_error
  } else if(type == "n")  {
    dem_error <- terra::focal(x = dem_error, w = 3, fun = mean, na.rm = TRUE, pad = TRUE)
  } else if (type == "d") {
    
    dem_df <- sf::st_as_sf(as.data.frame(x,xy=TRUE, na.rm = TRUE), coords=1:2)
    
    if(!is.null(samples)) { 
      print(paste0(samples, " random spatial data locations sampled"))
      dem_df <- dem_df[sample(nrow(dem_df), samples),]
    }
    
    vario <- gstat::variogram(dem_df[[1]] ~ 1, data = dem_df)
    fit = gstat::fit.variogram(vario, gstat::vgm(c("Exp", "Mat", "Sph", "Gau")))
    
    window <- round(fit$range[2]/max(terra::res(x)))
    window <- ifelse(test = (window%%2) != 0, yes = window, no = window + 1)
    
    message("Variogram model = ", fit$model[2], ", ", "size of window = ", window)
    dem_error <- terra::focal(x = dem_error, w = window, fun = mean, na.rm = TRUE, pad = TRUE)
    
  }
  
  dem_error[] <- base::scale(terra::values(dem_error)) * rmse
  
  x <- x + dem_error
  
  return(x)
  
}
