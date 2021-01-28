#' Incorporate vertical error into Digital Elevation Model
#'
#' Incorporates vertical error into the supplied Digital Elevation Model.
#'
#' @param dem \code{RasterLayer} (raster package). Digital Elevation Model
#'
#' @param rmse \code{numeric}. Vertical Root Mean Square Error of the Digital Elevation Model
#'
#' @param type \code{character}. Methods for creating random fields. Argument currently accepts 'unfiltered' or 'autocorrelated'. Default is 'autocorrelated'. See details for more information
#'
#' @param size \code{numeric}. Size of window when applying mean filter to random error fields. Increasing the size of the window increases the spatial autocorreltion in the random error field. Default size of window is 3x3.
#'
#' @param confidence_level \code{numeric}. Assuming a normal distribution of vertical error, the supplied rmse can be multipled by the confidence level z score in order to calculate confidence intervals that are used when generating the random error field. The confidence level denotes the probability that the true elevation value for each cell falls within a range of values (i.e. the confidence interval).
#'
#'@references
#'
#'Fisher, P. F., Tate, N. J. (2006). Causes and consequences of error in digital elevation models. Progress in Physical Geography, 30(4), 467-489. \url{https://doi.org/10.1191/0309133306pp492ra}
#'
#'Hunter, G. J., Goodchild, M. F. (1997). Modeling the uncertainty of slope and aspect estimates derived from spatial databases. Geographical Analysis, 29: 35-49.
#'
#' Wechsler, S. P. (1999) Digital Elevation Model (DEM) uncertainty: evaluation and effect on topographic parameters In Proceedings of the 1999 ESRI User Conference (available at: \url{https://ibis.geog.ubc.ca/courses/geob370/notes/uncertainty/DEM_uncertainty_wechsler_dissertation.html})
#'
#' Wechsler, S. P. (2003). Perceptions of Digital Elevation Model Uncertainty by DEM Users, URISA Journal, 15, 57-64.
#'
#' Wechsler, S. P., Kroll, C. N. (2006). Quantifying DEM Uncertainty and its Effect on Topographic Parameters. Photogrammetric Engineering & Remote Sensing, 72(9), 1081-1090. \url{https://doi.org/10.14358/pers.72.9.1081}
#'
#' Wechsler, S. P. (2007). Uncertainties associated with digital elevation models for hydrologic applications: a review. Hydrology and Earth System Sciences, 11, 4, 1481-1500. \url{https://doi.org/10.5194/hess-11-1481-2007}
#'
#' @return \code{raster} (raster package). Digital Elevation Model with a single realisation of vertical error incorporated
#'
#' @details
#'
#' Digital Elevation Models are representations of the earth's surface (DEM) and are subject to error (Wechsler, 1999). However the impact of the error on the results of analyses is often not evaluated (Hunter and Goodchild, 1997; Wechsler, 1999).
#'
#' The add_dem_error function with the type argument as 'unfiltered' incorporates vertical error into the supplied Digital Elevation Model by assuming that the error for each cell follows a gaussian (normal) distribution around the measured elevation value and the global Root Mean Square Error (RMSE) estimating the local error variance around this values (Fisher and Tate, 2006). However, this assumes that the vertical error is random and does not show spatial autocorrelation.
#'
#' The type argument 'autocorrelated' (default) increases the spatial autocorrelation by applying a users-specific filter over the surface (Wechsler and Kroll, 2006).
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
#'
#' @export
#'
#'@examples
#'
#'r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
#'
#'r_error <- add_dem_error(r, rmse = 9.73)

add_dem_error <- function(dem, rmse, type = "unfiltered", size = 3, confidence_level) {

    if (!inherits(dem, "RasterLayer")) {
        stop("dem argument is invalid. Expecting a RasterLayer object")
    }

    allowed_types <- c("unfiltered", "autocorrelated")

    if (!type %in% allowed_types) {
        stop("type argument is invalid. See details for more information")
    }

    error_mean <- 0
    error_sd <- abs(rmse)

    dem_error <- dem
    dem_error[] <- stats::rnorm(n = raster::ncell(dem), mean = error_mean, sd = error_sd)

    if (type == "autocorrelated") {

        dem_error <- raster::focal(dem_error, w = matrix(1/9, nrow = size, ncol = size), na.rm = TRUE, pad = TRUE)

        # rescale to mean of 0 and standard deviation of RMSE of DEM
        dem_error[] <- base::scale(raster::values(dem_error)) * rmse

    }

    if (!missing(confidence_level)) {

        cl <- c(70, 75, 80, 90, 95, 99)
        z_score <- c(1.04, 1.15, 1.28, 1.645, 1.96, 2.58)

        if (!confidence_level %in% cl) {
            stop("confidence_level argument is invalid. Expects value of 70, 75, 80, 90, 95, or 99.")
        }

        cl_match <- base::match(confidence_level, cl)

        rmse_upper <- error_mean + (rmse * z_score[cl_match])
        rmse_lower <- error_mean - (rmse * z_score[cl_match])

        while (sum(raster::values(dem_error) > rmse_upper | raster::values(dem_error) < rmse_lower, na.rm = TRUE) != 0) {
            dem_error[raster::values(dem_error) > rmse_upper | raster::values(dem_error) < rmse_lower] <- stats::rnorm(sum(raster::values(dem_error) > rmse_upper |
                raster::values(dem_error) < rmse_lower, na.rm = TRUE), mean = error_mean, sd = error_sd)

        }

    }

    dem <- dem + dem_error

    return(dem)

}
