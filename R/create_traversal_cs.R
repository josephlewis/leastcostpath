#' create_traversal_cs
#'
#' Creates Traversal Cost Surface to be used in Least Cost Path calculation.
#'
#'The create_traversal_cs function computes a cost surface based on the difficulty of traversing across the slope. Difficulty of traversal is based on the figure given in Bell and Lock (2000). Based on symmetrical or asymmetrical difficulty. Asymmetrical accounts for downhill being easier than uphill. The function requires a Digital Elevation Model (class 'RasterLayer') and the whether the traversal function is symmetrical.
#'
#'traversal input include 'symmetrical', 'asymmetrical'. Default is 'asymmetrical'.
#'
#' @param dem Digital Elevation Model. Expects Object of class RasterLayer
#'
#' @param traversal Difficulty of traversal is based on the figure given in Bell and Lock (2000). Based on symmetrical or asymmetrical difficulty. Asymmetrical accounts for downhill being easier than uphill.
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
#' create_traversal_cs(r, traversal = 'asymmetrical')

create_traversal_cs <- function(dem, traversal = "asymmetrical", neighbours = 16) {
    
    if (traversal != "symmetrical" & traversal != "asymmetrical") {
        stop("traversal expecting 'symmetrical' or 'asymmetrical' as input.")
    }
    
    aspect_dem <- raster::terrain(dem, opt = "aspect", unit = "degrees", neighbors = 8)
    
    aspect_dem <- raster::calc(aspect_dem, function(x) {
        ifelse(x >= 180, x - 180, x)
    })
    
    aspect_dem <- raster::calc(aspect_dem, function(x) {
        ifelse(x >= 0 & x <= 90, x + 90, x - 90)
    })
    
    
    if (traversal == "asymmetrical") {
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
        
    } else if (traversal == "symmetrical") {
        altDiff_traversal <- function(x) {
            
            if (abs(x[2] - x[1]) == 0) {
                1
            } else if (x[2] > x[1]) {
                if (abs(x[2] - x[1]) > 0 & abs(x[2] - x[1]) <= 45) {
                  hrma <- abs(x[2] - x[1])
                  1 - (0.5/45) * hrma
                } else if (abs(x[2] - x[1]) > 45 & abs(x[2] - x[1]) <= 90) {
                  hrma <- abs(x[2] - x[1])
                  (0.5/45) * hrma
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
        
    }
    
    trans <- gdistance::transition(aspect_dem, altDiff_traversal, neighbours, symm = FALSE)
    
    trans <- gdistance::geoCorrection(trans)
    
    return(trans)
}
