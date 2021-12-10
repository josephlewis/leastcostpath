#' Apply Cost Function to Slope (rise over run) values
#'
#' Creates a Conductivity surface based on the difficulty of moving up/down slope. This function applies the cost function to the slope (rise over run) values as calcualted using calculate_slope()
#'
#' @details
#'
#' Tobler's 'Hiking Function' is the most widely used cost function when approximating the difficulty of moving across a landscape (Gorenflo and Gale, 1990; Wheatley and Gillings, 2001). The function assesses the time necessary to traverse a surface and takes into account up-slope and down-slope (Kantner, 2004; Tobler, 1993). Time unit measured in seconds.
#'
#' Tobler's offpath Hiking Function reduces the speed of the Tobler's Hiking Function by 0.6 to take into account walking off-path (Tobler, 1993). Time unit measured in seconds.
#'
#'The Irmischer and Clark cost functions (2018) were modelled from speed estimates of United States Military Academy (USMA) cadets while they navigated on foot over hilly, wooded terrain as part of their summer training in map and compass navigation. Time unit measured in seconds.
#'
#' The Modified Hiking cost function combines MIDE (París Roche, 2002), a method to calculate walking hours for an average hiker with a light load (Márquez-Pérez et al. 2017), and Tobler's 'Hiking Function' (Tobler, 1993). Time unit measured in seconds.
#'
#' Herzog (2013), based on the cost function provided by Llobera and Sluckin (2007), has provided a cost function to approximate the cost for wheeled transport. The cost function is symmetric and is most applicable for use when the same route was taken in both directions.
#'
#' Herzog's (2010) Sixth-degree polynomial cost function approximates the energy expenditure values (J/(kg*m)) found in Minetti et al. (2002) but eliminates the problem of unrealistic negative energy expenditure values for steep downhill slopes.
#'
#' Llobera and Sluckin (2007) cost function approximates the metabolic energy expenditure (KJ/m) when moving across a landscape.
#'
#' Campbell (2019) cost function (Lorentz distribution) approximates the time taken to traverse a surface based on crowdsourced GPS data (1.05 million travel rate records). Data divided into travel rate percentiles (1st, 5th to 95th, by 5, and 99th). max_slope argument is fixed at 30 degrees slope to reflect the maximum slope that the cost function is parametised to. Time unit measured in seconds.
#'
#' @param slope \code{TransitionLayer} (gdistance package). Slope (rise over run) as calculated using calculate_slope()
#'
#' @param cost_function \code{character}. Cost Function used in the Least Cost Path calculation. Implemented cost functions include 'tobler', 'tobler offpath', 'irmischer-clarke male', 'irmischer-clarke offpath male', 'irmischer-clarke female', 'irmischer-clarke offpath female', 'modified tobler', 'wheeled transport', 'herzog', 'llobera-sluckin' and 'campbell 2019'. Default is 'tobler'. See Details for more information
#'
#' @param neighbours \code{numeric} value. Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected numeric values are 4, 8, 16, 32, 48 or a matrix object. Default is numeric value 16
#'
#' @param crit_slope \code{numeric} value. Critical Slope (in percentage) is 'the transition where switchbacks become more effective than direct uphill or downhill paths'. Cost of climbing the critical slope is twice as high as those for moving on flat terrain and is used for estimating the cost of using wheeled vehicles. Default value is 12, which is the postulated maximum gradient traversable by ancient transport (Verhagen and Jeneson, 2012). Critical slope only used in 'wheeled transport' cost function
#'
#' @param max_slope \code{numeric} value. Maximum percentage slope that is traversable. Slope values that are greater than the specified max_slope are given a conductivity value of 0. If cost_function argument is 'campbell 2019' then max_slope is fixed at 30 degrees slope to reflect the maximum slope that the cost function is parametised to. Default is NULL
#'
#' @param percentile \code{numeric} value. Travel rate percentile only used in 'campbell 2019' cost_function. Expected numeric values are 0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.99. Default is numeric value 0.50
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
#'
#' @export
#'

apply_cost <- function(slope = slope, cost_function = "tobler", neighbours = 16, crit_slope = 12, max_slope = NULL, percentile = 0.5) {
    
    if (!inherits(slope, "TransitionLayer")) {
        stop("slope argument is invalid. Expecting a TransitionLayer object")
    }
    
    slope_raster <- raster(slope)
    
    adj <- raster::adjacent(slope_raster, cells = 1:raster::ncell(slope_raster), pairs = TRUE, directions = neighbours)
    
    cf <- cost(cost_function = cost_function, adj = adj, crit_slope = crit_slope, percentile = percentile)
    
    cost <- slope
    
    cost[adj] <- cf(slope)
    
    speed_cfs <- c("tobler", "tobler offpath", "irmischer-clarke male", "irmischer-clarke offpath male", "irmischer-clarke female", "irmischer-clarke offpath female", "modified tobler", "campbell 2019")
    
    if (cost_function %in% speed_cfs) {
        
        cost[adj] <- cost[adj] * 0.278
        
        Conductance <- gdistance::geoCorrection(cost, scl = FALSE)
        
    } else {
        
        Conductance <- gdistance::geoCorrection(cost, scl = FALSE)
        
    }
    
    if (cost_function == "campbell 2019") {
        
        if (is.null(max_slope)) {
            message("max_slope argument set to 30 degrees slope to reflect the maximum slope that the cost function is parametised to")
            max_slope <- 30
        } else if (max_slope > 30) {
            message("max_slope argument cannot be above 30 degree slope. max_slope argument set to 30 degrees slope to reflect the maximum slope that the cost function is parametised to")
            max_slope <- 30
        } else {
            max_slope <- max_slope
        }
    }
    
    if (inherits(max_slope, "numeric")) {
        
        if (max_slope < 0) {
            stop("max_slope argument is invalid. Expecting numeric value above 0")
        }
        
        max_slope <- max_slope/100
        
        index <- abs(slope[adj]) >= max_slope
        
        Conductance[adj][index] <- 0
        
    }
    
    return(Conductance)
    
}
