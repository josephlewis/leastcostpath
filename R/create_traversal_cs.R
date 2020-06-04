#' Create a Traversal across Slope Cost Surface
#'
#' Creates a cost surface based on the difficulty of traversing across slope. Difficulty of traversal is based on the figure given in Bell and Lock (2000). Traversal across slope accounts for movement directly perpendicular across slope being easier than movement diagonally up/down slope.
#'
#' @param dem \code{RasterLayer} (raster package). Digital Elevation Model
#'
#' @param neighbours \code{numeric} value. Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected values are 4, 8, or 16. Default is 16
#'
#' @return \code{TransitionLayer} (gdistance package) numerically expressing the difficulty of moving across slope based on figure given in Bell and Lock (2000). The traversal_cs \code{TransitionLayer} should be multiplied by the create_slope_cs \code{TransitionLayer}, resulting in a \code{TransitionLayer} that takes into account movement across slope in all directions
#'
#' @author Joseph Lewis
#'
#' @import rgdal
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
        stop("dem argument is invalid. Expecting a RasterLayer object")
    }
    
    if (!neighbours %in% c(4, 8, 16, 32, 48)) {
        stop("neighbours argument is invalid. Expecting 4, 8, 16, 32, 48.")
    }
    
    if (neighbours == 32) {
        
        neighbours <- neighbours_32
        
        print("ok")
        
    } else if (neighbours == 48) {
        
        print("48")
        
        neighbours <- neighbours_48
    }
    
    trans <- gdistance::transition(dem, transitionFunction = function(x) {
        0
    }, neighbours, symm = FALSE)
    
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
    
    trans_aspect <- gdistance::transition(aspect_dem, altDiff_traversal, neighbours, symm = FALSE)
    
    adj <- raster::adjacent(dem, cells = 1:raster::ncell(dem), pairs = TRUE, directions = neighbours)
    
    trans[adj] <- trans_aspect[adj]
    
    return(trans)
}
