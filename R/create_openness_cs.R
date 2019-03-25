#' create_openness_cs
#'
#' Creates openness Cost Surface to be used in Least Cost Path calculation.
#'
#'The create_openness_cs function computes a cost surface based on degree in which a location is sheltered or open to vision
#'
#' @param dem Digital Elevation Model. Expects Object of class RasterLayer
#'
#' @param kernel Kernel size to calculate openness. Larger kernel size increases the distance that the openness is calculated to. Default kernel size is is 3.
#'
#' @author Joseph Lewis
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import gdistance
#'
#' @export
#'
#' @examples
#' r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
#' create_openness_cs(r, kernel = 3)

create_openness_cs <- function(dem, kernel = 3) {
    
    kernel <- round(kernel)
    
    if (kernel%%2 == 0) 
        stop("Kernel size must be odd number")
    
    if (kernel < 3) 
        stop("Kernel size must be 3 or greater")
    
    open_Diff <- function(x) {
        x[2] - x[1]
    }
    
    kernel_m <- matrix(c(rep(1, kernel^2)), ncol = kernel, nrow = kernel, byrow = TRUE)
    
    center <- ((kernel + 1)/2)
    
    kernel_m[center, center] <- 0
    
    open_diff_ts <- gdistance::transition(dem, open_Diff, kernel_m, symm = FALSE)
    
    open_diff_ts <- gdistance::geoCorrection(open_diff_ts)
    
    open_adj <- gdistance::adjacencyFromTransition(open_diff_ts)
    
    open_diff_ts[open_adj] <- 90 - atan(open_diff_ts[open_adj])
    
    return(open_diff_ts)
}
