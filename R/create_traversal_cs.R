#' create_traversal_cs
#'
#' Creates Traversal across slope Cost Surface to be used in Least Cost Path calculation.
#'
#'The create_traversal_cs function computes a cost surface based on the difficulty of traversing across the slope. Difficulty of traversal is based on the figure given in Bell and Lock (2000). Traversal across slope accounts for downhill being easier than uphill. The function requires a Digital Elevation Model (class 'RasterLayer').
#'
#'
#' @param dem Digital Elevation Model. Expects Object of class RasterLayer
#'
#' @param neighbours Number of directions used in the Least Cost Path calculation. \href{https://www.ncbi.nlm.nih.gov/pubmed/17892887}{Huber and Church (1985)} for methodological considerations when considering number of neighbours. Expected input values are 4, 8, 16. Default is 16.
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
#' traversal_cs <- create_traversal_cs(r, neighbours = 16)

create_traversal_cs <- function(dem, neighbours = 16) {
    
    if (!inherits(dem, "RasterLayer")) {
        stop("dem argument expects a RasterLayer object")
    }
    
    if (!neighbours %in% c(4, 8, 16)) {
        stop("Incorrect number of neighbours - expecting 4, 8, or 16.")
    }
    
    aspect_dem <- raster::terrain(dem, opt = "aspect", unit = "degrees", neighbors = 8)
    
    aspect_dem[aspect_dem >= 0 & aspect_dem <= 90] <- aspect_dem[aspect_dem >= 0 & aspect_dem <= 90] + 90
    
    aspect_dem[aspect_dem > 90 & aspect_dem <= 180] <- aspect_dem[aspect_dem > 90 & aspect_dem <= 180] - 90
    
    aspect_dem[aspect_dem > 180 & aspect_dem <= 270] <- aspect_dem[aspect_dem > 180 & aspect_dem <= 270] - 90
    
    aspect_dem[aspect_dem > 270 & aspect_dem <= 360] <- (aspect_dem[aspect_dem > 270 & aspect_dem <= 360] + 90) - 360
    
    altDiff_traversal <- function(x) {
        if (abs(x[2] - x[1]) == 0) {
            1
        } else if (x[2] > x[1]) {
            if (abs(x[2] - x[1]) > 0 & abs(x[2] - x[1]) <= 45) {
                hrma <- abs(x[2] - x[1])
                1 + (0.5/45) * hrma
            } else if (abs(x[2] - x[1]) > 45 & abs(x[2] - x[1]) <= 90) {
                hrma <- abs(x[2] - x[1])
                2 - (0.5/45) * hrma
            } else {
                1
            }
        } else if (x[2] < x[1]) {
            if (abs(x[2] - x[1]) > 0 & abs(x[2] - x[1]) <= 45) {
                hrma <- abs(x[2] - x[1])
                1 - (0.5/45) * hrma
            } else if (abs(x[2] - x[1]) > 45 & abs(x[2] - x[1]) <= 90) {
                hrma <- abs(x[2] - x[1])
                (0.5/45) * hrma
            } else {
                1
            }
        }
    }
    
    trans <- gdistance::transition(aspect_dem, altDiff_traversal, neighbours, symm = FALSE)
    
    if (neighbours == 8 | neighbours == 16) {
        trans <- gdistance::geoCorrection(trans)
    }
    
    return(trans)
}
