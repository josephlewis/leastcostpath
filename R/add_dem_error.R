#' Incorporate vertical error into Digital Elevation Model
#'
#' Incorporates vertical error into the supplied Digital Elevation Model.
#'
#' @param dem \code{RasterLayer} (raster package). Digital Elevation Model
#'
#' @param rmse \code{numeric}. Vertical Root Mean Square Error of the Digital Elevation Model
#'
#' @param size \code{character} or \code{numeric}. Size of window when applying mean filter to random error fields. Increasing the size of the window increases the spatial autocorreltion in the random error field. Size of window is automatically calculated via a variogram when argument is "auto" (default). If size of window is user-supplied, then numeric value must be odd.
#'
#' @param vgm_model \code{character}. Variogram model type when determining window size. Accepted values are "Sph" (default), "Exp", "Gau", "Mat". See details for more information
#'
#' @references
#'Fisher, P. F., Tate, N. J. (2006). Causes and consequences of error in digital elevation models. Progress in Physical Geography, 30(4), 467-489. \doi{10.1191/0309133306pp492ra}
#'
#'Hunter, G. J., Goodchild, M. F. (1997). Modeling the uncertainty of slope and aspect estimates derived from spatial databases. Geographical Analysis, 29: 35-49.
#'
#' Wechsler, S. P. (1999) Digital Elevation Model (DEM) uncertainty: evaluation and effect on topographic parameters In Proceedings of the 1999 ESRI User Conference (available at: \url{https://proceedings.esri.com/library/userconf/proc99/proceed/papers/pap262/p262.htm})
#'
#' Wechsler, S. P. (2003). Perceptions of Digital Elevation Model Uncertainty by DEM Users, URISA Journal, 15, 57-64.
#'
#' Wechsler, S. P., Kroll, C. N. (2006). Quantifying DEM Uncertainty and its Effect on Topographic Parameters. Photogrammetric Engineering & Remote Sensing, 72(9), 1081-1090. \doi{10.14358/pers.72.9.1081}
#'
#' Wechsler, S. P. (2007). Uncertainties associated with digital elevation models for hydrologic applications: a review. Hydrology and Earth System Sciences, 11, 4, 1481-1500. \doi{10.5194/hess-11-1481-2007}
#'
#' @return \code{raster} (raster package). Digital Elevation Model with a single realisation of vertical error incorporated
#'
#' @details
#' Digital Elevation Models (DEMs) are representations of the earth's surface and are subject to error (Wechsler, 1999). However the impact of the error on the results of analyses is often not evaluated (Hunter and Goodchild, 1997; Wechsler, 1999).
#'
#' The add_dem_error function incorporates vertical error into the supplied Digital Elevation Model by assuming that the error for each cell follows a gaussian (normal) distribution around the measured elevation value and the global Root Mean Square Error (RMSE) estimating the local error variance around this values (Fisher and Tate, 2006). Addition of spatial autocorrelation applied by using a mean-window filter based on a window size (Wechsler and Kroll, 2006). If size argument is 'auto' then window size calculated via a variogram (Wechsler and Kroll, 2006).
#'
#' vgm_model is the model fitted to the observed DEM variogram. This is used to calculate the distance at which spatial autocorrelation is no longer present (i.e. the range). If the vgm model type is not able to converge, try another model type (e.g. "Gau").
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
#' @author Joseph Lewis
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import gstat
#'
#' @export
#'
#' @examples
#'
#'r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
#'
#'r_error <- add_dem_error(r, rmse = 9.73, size = "auto", vgm_model = "Gau")

add_dem_error <- function(dem, rmse, size = "auto", vgm_model = "Sph") {

    if (!inherits(dem, "RasterLayer")) {
        stop("dem argument is invalid. Expecting a RasterLayer object")
    }

    if (all((!is.numeric(size)) & (size != "auto")))  {
        stop("size argument is invalid. Expecting 'auto' or a numeric value")
    }

    error_mean <- 0
    error_sd <- abs(rmse)

    dem_error <- dem
    dem_error[] <- stats::rnorm(n = raster::ncell(dem), mean = error_mean, sd = error_sd)

    if (size == "auto") {

        dem_spdf <- as(dem, "SpatialPixelsDataFrame")
        names(dem_spdf) <- "dem"
        vario = gstat::variogram(dem_spdf$dem ~1, dem_spdf)
        fit = gstat::fit.variogram(vario, gstat::vgm(vgm_model))
        window <- round(fit[fit$model == vgm_model,]$range / max(raster::res(dem)))

        # Ensures window is an odd-number to be used in raster::focal
        window <- ifelse(test = (window %% 2) != 0, yes = window, no = window + 1)

        message("Size of window = ", window)

        size <- window

        }

    dem_error <- raster::focal(dem_error, w = matrix(1/9, nrow = size, ncol = size), na.rm = TRUE, pad = TRUE)

    # rescale to mean of 0 and standard deviation of RMSE of DEM
    dem_error[] <- base::scale(raster::values(dem_error)) * rmse

    dem <- dem + dem_error

    return(dem)

}
