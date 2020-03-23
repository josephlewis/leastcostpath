#' create_slope_cs
#'
#' Creates a Slope Cost Surface
#'
#' Creates a cost surface based on the difficulty of moving up/down slope. This function allows for multiple isotropic and anisotropic cost functions that estimate human movement across a landscape. The maximum percentage slope traversal can also be supplied.
#'
#' @details
#'
#' The most widely used cost function that approximates the difficulty of moving across a landscape is Waldo Tobler's 'Hiking Function' (Gorenflo and Gale, 1990; Wheatley and Gillings, 2001). This function allows for the assessment of time necessary to traverse a surface and takes into account up-slope and down-slope momentum (Kantner, 2004; Tobler, 1993).
#'
#' The Modified Hiking cost function combines MIDE (París Roche, 2002), a method to calculate walking hours for an average hiker with a light load (Márquez-Pérez et al., 2017), and Tobler's 'Hiking Function' (Tobler, 1993). The Modified Hiking Function benefits from the precision of the MIDE rule and the continuity of Tobler's Hiking Function (Márquez-Pérez et al., 2017).
#'
#' Herzog (2013), based on the cost function provided by Llobera and Sluckin (2007), has provided a cost function to approximate the cost for wheeled transport. The cost function is symmetric and is most applicable for use when the same route was taken in both directions.
#'
#' Herzog's (2010) Sixth-degree polynomial cost function approximates the energy expenditure values found in Minetti et al. (2002) but eliminates the problem of unrealistic negative energy expenditure values for steep downhill slopes.
#'
#' @param dem \code{RasterLayer} (raster package). Digital Elevation Model.
#'
#' @param cost_function Cost Function used in the Least Cost Path calculation. Implemented cost functions include 'tobler', 'modified tobler', 'wheeled transport' 'herzog', or 'all'. Default is 'tobler'. See Details for more information.
#'
#' @param neighbours \code{numeric} value. Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected values are 4, 8, or 16. Default is 16.
#'
#' @param crit_slope Critical Slope (in percentage) is 'the transition where switchbacks become more effective than direct uphill or downhill paths'. Cost of climbing the critical slope is twice as high as those for moving on flat terrain and is used for estimating the cost of using wheeled vehicles. Default value is 12, which is the postulated maximum gradient traversable by ancient transport (Verhagen and Jeneson, 2012). Critical slope only used in 'wheeled transport' cost function.
#'
#' @param max_slope \code{numeric} value. Maximum percentage slope that is traversable. Default is NULL.
#'
#' @return \code{TransitionLayer} (gdistance package) numerically expressing the difficulty of moving up/down slope based on the cost function provided in the cost_function argument.
#'
#' @author Joseph Lewis
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import gdistance
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
#' slope_cs <- create_slope_cs(r, cost_function = 'tobler', neighbours = 16, max_slope = NULL)

create_slope_cs <- function(dem, cost_function = "tobler", neighbours = 16, crit_slope = 12, max_slope = NULL) {
    
    if (!inherits(dem, "RasterLayer")) {
        stop("dem argument is invalid. Expecting a RasterLayer object")
    }
    
    if (cost_function != "tobler" & cost_function != "modified tobler" & cost_function != "wheeled transport" & cost_function != "herzog" & 
        cost_function != "all") {
        stop("cost_function argument is invalid. Expecting 'tobler', 'modified tobler', 'wheeled transport', 'herzog', or 'all'")
    }
    
    if (!neighbours %in% c(4, 8, 16)) {
        stop("neighbours argument is invalid. Expecting 4, 8, or 16.")
    }
    
    altDiff_slope <- function(x) {
        x[2] - x[1]
    }
    
    hd <- gdistance::transition(dem, altDiff_slope, 8, symm = FALSE)
    
    slope <- gdistance::geoCorrection(hd)
    
    adj <- raster::adjacent(dem, cells = 1:raster::ncell(dem), pairs = TRUE, directions = neighbours)
    
    rand <- stats::runif(1, min = 0.01, max = 1)
    
    if (inherits(max_slope, "numeric")) {
        
        max_slope <- max_slope/100
        
        slope[adj] <- ifelse(slope[adj] > max_slope, rand, slope[adj])
        
        slope[adj] <- ifelse(slope[adj] < -max_slope, rand, slope[adj])
        
    }
    
    if (cost_function == "tobler") {
        slope[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
        
        if (inherits(max_slope, "numeric")) {
            slope@transitionMatrix@x[slope@transitionMatrix@x == 6 * exp(-3.5 * abs(rand + 0.05))] <- 0
        }
        
        Conductance <- gdistance::geoCorrection(slope)
    }
    
    if (cost_function == "modified tobler") {
        slope[adj] <- 4.8 * exp(-5.3 * abs((slope[adj] * 0.7) + 0.03))
        
        if (inherits(max_slope, "numeric")) {
            slope@transitionMatrix@x[slope@transitionMatrix@x == 4.8 * exp(-5.3 * abs((rand * 0.7) + 0.03))] <- 0
        }
        Conductance <- gdistance::geoCorrection(slope)
    }
    
    if (cost_function == "wheeled transport") {
        slope[adj] <- 1/(1 + abs(slope[adj] * 100/crit_slope)^2)
        if (inherits(max_slope, "numeric")) {
            slope@transitionMatrix@x[slope@transitionMatrix@x == 1/(1 + abs(rand * 100/crit_slope)^2)] <- 0
        }
        Conductance <- gdistance::geoCorrection(slope)
    }
    
    if (cost_function == "herzog") {
        slope[adj] <- 1/((1337.8 * slope[adj]^6) + (278.19 * slope[adj]^5) - (517.39 * slope[adj]^4) - (78.199 * slope[adj]^3) + (93.419 * 
            slope[adj]^2) + (19.825 * slope[adj]) + 1.64)
        if (inherits(max_slope, "numeric")) {
            slope@transitionMatrix@x[slope@transitionMatrix@x == 1/((((1337.8 * rand^6) + (278.19 * rand^5) - (517.39 * rand^4) - (78.199 * 
                rand^3) + (93.419 * rand^2) + (19.825 * rand) + 1.64)))] <- 0
        }
        Conductance <- gdistance::geoCorrection(slope)
    }
    
    if (cost_function == "all") {
        
        slope_stack <- raster::stack(slope, slope, slope, slope)
        
        slope_stack[[1]][adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
        slope_stack[[1]]@transitionMatrix@x[slope@transitionMatrix@x == 6 * exp(-3.5 * abs(rand + 0.05))] <- 0
        
        slope_stack[[2]][adj] <- 4.8 * exp(-5.3 * abs((slope[adj] * 0.7) + 0.03))
        slope_stack[[2]]@transitionMatrix@x[slope@transitionMatrix@x == 4.8 * exp(-5.3 * abs((rand * 0.7) + 0.03))] <- 0
        
        slope_stack[[3]][adj] <- 1/(1 + abs(slope[adj] * 100/crit_slope)^2)
        slope_stack[[3]]@transitionMatrix@x[slope@transitionMatrix@x == 1/(1 + abs(rand * 100/crit_slope)^2)] <- 0
        
        slope_stack[[4]][adj] <- 1/((((1337.8 * slope[adj]^6) + (278.19 * slope[adj]^5) - (517.39 * slope[adj]^4) - (78.199 * slope[adj]^3) + 
            (93.419 * slope[adj]^2) + (19.825 * slope[adj]) + 1.64)))
        slope_stack[[4]]@transitionMatrix@x[slope@transitionMatrix@x == 1/((((1337.8 * rand^6) + (278.19 * rand^5) - (517.39 * rand^4) - 
            (78.199 * rand^3) + (93.419 * rand^2) + (19.825 * rand) + 1.64)))] <- 0
        
        Conductance <- raster::stack(gdistance::geoCorrection(slope_stack[[1]]), gdistance::geoCorrection(slope_stack[[2]]), gdistance::geoCorrection(slope_stack[[3]]), 
            gdistance::geoCorrection(slope_stack[[4]]))
    }
    
    return(Conductance)
    
}
