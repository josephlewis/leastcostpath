#' calculate slope (rise over run) from supplied digital elevation model (DEM)
#'
#' @param dem \code{RasterLayer} (raster package). Digital Elevation Model
#'
#' @param neighbours \code{numeric} value. Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected values are 4, 8, 16, 32, or 48. Default is 16
#'
#' @noRd
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import gdistance
#' @importFrom stats runif
#'
#' @author Joseph Lewis

calculate_slope <- function(dem, neighbours) {
    
    altDiff_slope <- function(x) {
        x[2] - x[1]
    }
    
    hd <- suppressWarnings(gdistance::transition(x = dem, transitionFunction = altDiff_slope, directions = neighbours, symm = FALSE))
    
    slope <- gdistance::geoCorrection(hd, scl = FALSE)
    
    return(slope)
    
}


