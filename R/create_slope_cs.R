#' Create a slope based cost surface
#'
#' Creates a cost surface based on the difficulty of moving up/down slope. This function provides the choice of multiple isotropic and anisotropic cost functions that estimate human movement across a landscape. Maximum percentage slope possible for traversal can also be suppplied.
#'
#' @details
#'
#' Tobler's 'Hiking Function' is the most widely used cost function when approximating the difficulty of moving across a landscape (Gorenflo and Gale, 1990; Wheatley and Gillings, 2001). The function assess the time necessary to traverse a surface and takes into account up-slope and down-slope (Kantner, 2004; Tobler, 1993).
#'
#' Tobler's offpath Hiking Function reduces the speed of the Tobler's Hiking Function by 0.6 to take into account walking off-path (Tobler, 1993)
#'
#'The Irmischer and Clark functions were modelled from speed estimates of United States Military Academy (USMA) cadets while they navigated on foot over hilly, wooded terrain as part of their summer training in map and compass navigation.
#'
#' The Modified Hiking cost function combines MIDE (París Roche, 2002), a method to calculate walking hours for an average hiker with a light load (Márquez-Pérez et al. 2017), and Tobler's 'Hiking Function' (Tobler, 1993). The Modified Hiking Function benefits from the precision of the MIDE rule and the continuity of Tobler's Hiking Function (Márquez-Pérez et al. 2017).
#'
#' Herzog (2013), based on the cost function provided by Llobera and Sluckin (2007), has provided a cost function to approximate the cost for wheeled transport. The cost function is symmetric and is most applicable for use when the same route was taken in both directions.
#'
#' Herzog's (2010) Sixth-degree polynomial cost function approximates the energy expenditure values found in Minetti et al. (2002) but eliminates the problem of unrealistic negative energy expenditure values for steep downhill slopes.
#'
#' Llobera and Sluckin (2007) cost function approximates the metabolic energy expenditure in KJ/(m*kg) when moving across a landscape.
#'
#' @param dem \code{RasterLayer} (raster package). Digital Elevation Model
#'
#' @param cost_function \code{character}. Cost Function used in the Least Cost Path calculation. Implemented cost functions include 'tobler', 'tobler offpath', 'irmischer-clarke male', 'irmischer-clarke offpath male', 'irmischer-clarke female', 'irmischer-clarke offpath female', 'modified tobler', 'wheeled transport', 'herzog', 'llobera-sluckin'. Default is 'tobler'. See Details for more information
#'
#' @param neighbours \code{numeric} value. Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected values are 4, 8, 16, 32, or 48. Default is 16
#'
#' @param crit_slope \code{numeric} value. Critical Slope (in percentage) is 'the transition where switchbacks become more effective than direct uphill or downhill paths'. Cost of climbing the critical slope is twice as high as those for moving on flat terrain and is used for estimating the cost of using wheeled vehicles. Default value is 12, which is the postulated maximum gradient traversable by ancient transport (Verhagen and Jeneson, 2012). Critical slope only used in 'wheeled transport' cost function
#'
#' @param max_slope \code{numeric} value. Maximum percentage slope that is traversable. Slope values that are greater than the specified max_slope are given a conductivity value of 0. Default is NULL
#'
#' @return \code{TransitionLayer} (gdistance package) numerically expressing the difficulty of moving up/down slope based on the cost function provided in the cost_function argument. list of \code{TransitionLayer} if cost_function = 'all'
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
    
    if (!neighbours %in% c(4, 8, 16, 32, 48)) {
        stop("neighbours argument is invalid. Expecting 4, 8, 16, 32, or 48")
    }
    
    if (neighbours == 32) {
        
        neighbours <- neighbours_32
        
    } else if (neighbours == 48) {
        
        neighbours <- neighbours_48
    }
    
    slope <- calculate_slope(dem = dem, neighbours = neighbours)
    
    adj <- raster::adjacent(dem, cells = 1:raster::ncell(dem), pairs = TRUE, directions = neighbours)
    
    if (inherits(max_slope, "numeric")) {
        
        if (max_slope < 0) {
            stop("max_slope argument is invalid. Expecting numeric value above 0")
        }
        
        rand <- stats::runif(1, min = 0.01, max = 1)
        
        max_slope <- max_slope/100
        
        slope[adj] <- ifelse(slope[adj] >= max_slope, rand, slope[adj])
        
        slope[adj] <- ifelse(slope[adj] <= -max_slope, rand, slope[adj])
        
        index <- which(slope[adj] == rand)
        
    }
    
    cf <- cost(cost_function = cost_function, adj = adj, crit_slope = crit_slope)
    
    slope[adj] <- cf(slope)
    
    Conductance <- gdistance::geoCorrection(slope, scl = FALSE)
    
    if (inherits(max_slope, "numeric")) {
        
        Conductance[adj][index] <- 0
        
    }
    
    return(Conductance)
    
}
