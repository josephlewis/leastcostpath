#' Incorporate vertical error into Digital Elevation Model
#'
#' Incorporates vertical error into the supplied Digital Elevation Model.
#'
#' @param dem \code{RasterLayer} (raster package). Digital Elevation Model
#'
#' @param rmse \code{numeric}. Vertical Root Mean Square Error of the Digital Elevation Model
#'
#' @param type \code{character}. Method for creating random field based on vertical error information. Current implementations are 'simple' only. Default is 'simple'. See Details for more information.

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
#' The add_dem_error function with the type argument as 'simple' incorporates vertical error into the supplied Digital Elevation Model by assuming that the error for each cell follows a gaussian (normal) distribution around the measured elevation value and the global Root Mean Square Error (RMSE) estimating the local error variance around this values (Fisher and Tate, 2006).
#'
#' Although this method does not account for spatial autocorrelation, of which is generally recognised to be present in vertical errors (Hunter and Goodchild, 1997; Wechsler, 1999), the lack of further information beyond RMSE often necessitates its use (Wechsler, 2003; Wechsler, 2007). However, it should be noted that by assuming the vertical error for each cell is random, the resultant DEM incorporating vertical error can be considered of as a 'worst-case-scenario' as it provides the largest potential errors (Wechsler, 1999; Wechsler and Kroll, 2006).
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

add_dem_error <- function(dem, rmse, type = "simple") {
    
    if (!inherits(dem, "RasterLayer")) {
        stop("dem argument is invalid. Expecting a RasterLayer object")
    }
    
    if (!type %in% "simple") {
        stop("type argument is invalid. See details for more information")
    }
    
    if (type == "simple") {
        
        error <- stats::rnorm(n = raster::ncell(dem), mean = 0, sd = abs(rmse))
        
        dem <- dem + error
        
    }
    
    return(dem)
    
}
