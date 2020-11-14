#' calculate slope (rise over run) from supplied digital elevation model (DEM)
#'
#' @param dem \code{RasterLayer} (raster package). Digital Elevation Model
#'
#' @param neighbours \code{numeric} value. Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected values are 4, 8, 16, 32, or 48. Default is 16
#'
#' @param exaggeration \code{logical}. if TRUE, positive slope values (ie. up-hill movement) multiplied by 1.99 and negative slope values (ie. down-hill movement) multiplied by 2.31.
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

calculate_slope <- function(dem, neighbours, exaggeration = exaggeration) {
    
    altDiff_slope <- function(x) {
        x[2] - x[1]
    }
    
    hd <- suppressWarnings(gdistance::transition(x = dem, transitionFunction = altDiff_slope, directions = neighbours, symm = FALSE))
    
    slope <- gdistance::geoCorrection(hd, scl = FALSE)
    
    if (exaggeration) {
        
        slope@transitionMatrix@x <- ifelse(slope@transitionMatrix@x > 0, slope@transitionMatrix@x * 1.99, slope@transitionMatrix@x * 2.31)
        
    }
    
    return(slope)
    
}


