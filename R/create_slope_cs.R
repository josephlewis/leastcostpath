#' create_slope_cs
#'
#' Creates Slope Cost Surface to be used in Least Cost Path calculation.
#'
#'The create_slope_cs function computes a cost surface based on the difficulty of traversing the slope. It implements multiple isotropic and anisotropic cost functions that estimate human movement across a landscape.The function requires a Digital Elevation Model (class 'RasterLayer'), the cost function to be used, and the critical slope - critical slope only used in 'wheeled transport' cost function.
#'
#'isotropic cost functions include 'tobler', 'modified tobler', 'wheeled transport', 'herzog', and 'all'. Default is 'all'.

#' The most widely used cost function that approximates the difficulty of moving across a landscape is Waldo Tobler's 'Hiking Function' (Gorenflo and Gale, 1990; Wheatley and Gillings, 2001). This function allows for the assessment of time necessary to traverse a surface and takes into account up-slope and down-slope momentum (Kantner, 2004; Tobler, 1993).
#'
#' The Modified Hiking cost function combines MIDE (París Roche, 2002), a method to calculate walking hours for an average hiker with a light load (Márquez-Pérez et al., 2017), and Tobler's 'Hiking Function' (Tobler, 1993). The Modified Hiking Function benefits from the precision of the MIDE rule and the continuity of Tobler's Hiking Function (Márquez-Pérez et al., 2017).
#'
#' Herzog (2013), based on the cost function provided by Llobera and Sluckin (2007), has developed a cost function to approximate the cost for wheeled transport. The cost function is symmetric and is most applicable for use when the same route was taken in both directions.
#'
#' Herzog's (2014) Sixth-degree polynomial cost function approximates the energy expenditure values found in Minetti et al. (2002) but eliminates the problem of unrealistic negative energy expenditure values for steep downhill slopes.
#'
#' @param dem Digital Elevation Model. Expects Object of class RasterLayer
#'
#' @param cost_function Cost Function to be used in the Least Cost Path calculation. Current implementation computes LCPs using Tobler's Hiking function, Marquez-Perez et al. Modified Hiking function, Herzog's wheeled transport function, and Herzog's sixth degree polynomial function. Default parameter value is is 'all'.
#'
#' @param neighbours Number of directions used in the Least Cost Path calculation. \href{https://www.ncbi.nlm.nih.gov/pubmed/17892887}{Huber and Church (1985)} for methodological considerations when considering number of neighbours. Expected input values are 4, 8, 16. Default is 16.
#'
#' @param crit_slope Critical Slope (in percent) is 'the transition where switchbacks become more effective than direct uphill or downhill paths'. Cost of climbing the critical slope is twice as high as those for moving on flat terrain and is used for estimating the cost of using wheeled vehicles. Critical slope defaulted is 15 degrees, which is the postulated maximum gradient traversable by ancient transport (Verhagen and Jeneson, 2012).
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
#' create_slope_cs(r, cost_function = 'tobler')

create_slope_cs <- function(dem, cost_function = "all", neighbours = 16, crit_slope = 12) {
  
  if (cost_function != "tobler" & cost_function != "modified tobler" & cost_function != "wheeled transport" & cost_function != "herzog" & cost_function != "all") {
    stop("Incorrect cost_function. Expecting 'tobler', 'modified tobler', 'wheeled transport' 'herzog', or 'all'.")
  }
    
    altDiff_slope <- function(x) {
        x[2] - x[1]
    }
    
    hd <- gdistance::transition(dem, altDiff_slope, 8, symm = FALSE)
    
    slope <- gdistance::geoCorrection(hd)
    
    adj <- raster::adjacent(dem, cells = 1:raster::ncell(dem), pairs = TRUE, directions = neighbours)
    
    if (cost_function == "tobler") {
        slope[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
        Conductance <- gdistance::geoCorrection(slope)
    }
    
    if (cost_function == "modified tobler") {
        slope[adj] <- 4.8 * exp(-5.3 * abs((slope[adj] * 0.7) + 0.03))
        Conductance <- gdistance::geoCorrection(slope)
    }
    
    if (cost_function == "wheeled transport") {
        slope[adj] <- 1/(1 + abs(slope[adj] * 100/crit_slope)^2)
        Conductance <- gdistance::geoCorrection(slope)
    }
    
    if (cost_function == "herzog") {
        slope[adj] <- 1/((1337.8 * slope[adj]^6) + (278.19 * slope[adj]^5) - (517.39 * slope[adj]^4) - (78.199 * slope[adj]^3) + (93.419 * slope[adj]^2) + (19.825 * 
            slope[adj]) + 1.64)
        Conductance <- gdistance::geoCorrection(slope)
    }
    
    if (cost_function == "all") {
        
        slope_stack <- raster::stack(slope, slope, slope, slope)
        
        slope_stack[[1]][adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
        slope_stack[[2]][adj] <- 4.8 * exp(-5.3 * abs((slope[adj] * 0.7) + 0.03))
        slope_stack[[3]][adj] <- 1/(1 + abs(slope[adj] * 100/crit_slope)^2)
        slope_stack[[4]][adj] <- 1/((((1337.8 * slope[adj]^6) + (278.19 * slope[adj]^5) - (517.39 * slope[adj]^4) - (78.199 * slope[adj]^3) + (93.419 * slope[adj]^2) + 
            (19.825 * slope[adj]) + 1.64)))
        
        Conductance <- raster::stack(gdistance::geoCorrection(slope_stack[[1]]), gdistance::geoCorrection(slope_stack[[2]]), gdistance::geoCorrection(slope_stack[[3]]), 
            gdistance::geoCorrection(slope_stack[[4]]))
    }
    
    return(Conductance)
    
}
