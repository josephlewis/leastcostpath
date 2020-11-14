#' Create a Traversal across Slope Cost Surface
#'
#' Creates a cost surface based on the difficulty of traversing across slope. Difficulty of traversal is based on the figure given in Bell and Lock (2000). Traversal across slope accounts for movement directly perpendicular across slope being easier than movement diagonally up/down slope.
#'
#' @param dem \code{RasterLayer} (raster package). Digital Elevation Model
#'
#' @param neighbours \code{numeric} value. Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected numeric values are 4, 8, 16, 32, 48 or a matrix object. Default is numeric value 16
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
    
    if (any(!neighbours %in% c(4, 8, 16, 32, 48)) & (!inherits(neighbours, "matrix"))) {
        stop("neighbours argument is invalid. Expecting 4, 8, 16, 32, 48, or matrix object")
    }
    
    if (inherits(neighbours, "numeric")) {
        if (neighbours == 32) {
            neighbours <- neighbours_32
            
        } else if (neighbours == 48) {
            neighbours <- neighbours_48
        }
        
    }
    
    aspect <- raster::terrain(dem, opt = "aspect", unit = "degrees", neighbors = 8)
    
    aspect[aspect > 180] <- aspect[aspect > 180] - 180
    
    traverse <- aspect
    
    traverse[aspect >= 0 & aspect < 90] <- aspect[aspect >= 0 & aspect < 90] - 90
    traverse[aspect >= 90] <- aspect[aspect >= 90] - 90
    
    altDiff_traversal <- function(x) {
        altDiff <- abs(x[2]) - abs(x[1])
        
        if (altDiff == 0) {
            traversal <- 1
        } else if (altDiff > 0) {
            if (altDiff > 0 & altDiff <= 45) {
                hrma <- abs(altDiff)
                traversal <- 1 + (0.5/45) * hrma
                
            } else if (altDiff > 45 & altDiff <= 90) {
                hrma <- abs(altDiff)
                traversal <- 2 - (0.5/45) * hrma
                
            }
            
        } else if (altDiff < 0) {
            if (altDiff < 0 & altDiff >= -45) {
                hrma <- abs(altDiff)
                traversal <- 1 - (0.5/45) * hrma
                
            } else if (altDiff < -45 & altDiff >= -90) {
                hrma <- abs(altDiff)
                
                traversal <- (0.5/45) * hrma
                
            }
            
        }
        
        return(traversal)
    }
    
    trans_aspect <- gdistance::transition(traverse, altDiff_traversal, neighbours, symm = FALSE)
    
    return(trans_aspect)
}
