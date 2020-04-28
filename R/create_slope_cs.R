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

create_slope_cs1 <- function(dem, cost_function = "tobler", neighbours = 16, crit_slope = 12, max_slope = NULL) {

    if (!inherits(dem, "RasterLayer")) {
        stop("dem argument is invalid. Expecting a RasterLayer object")
    }

    cfs <- c("tobler", "tobler offpath", "modified tobler", "wheeled transport", "herzog", "all")

    if (!cost_function %in% cfs) {
        stop("cost_function argument is invalid. See details for accepted cost functions")
    }

    if (!neighbours %in% c(4, 8, 16)) {
        stop("neighbours argument is invalid. Expecting 4, 8, or 16.")
    }

    altDiff_slope <- function(x) {
        x[2] - x[1]
    }

    hd <- suppressWarnings(gdistance::transition(dem, altDiff_slope, 8, symm = FALSE))

    slope <- gdistance::geoCorrection(hd)

    adj <- raster::adjacent(dem, cells = 1:raster::ncell(dem), pairs = TRUE, directions = neighbours)

    rand <- stats::runif(1, min = 0.01, max = 1)

    if (inherits(max_slope, "numeric")) {

        max_slope <- max_slope/100

        slope[adj] <- ifelse(slope[adj] > max_slope, rand, slope[adj])

        slope[adj] <- ifelse(slope[adj] < -max_slope, rand, slope[adj])

    }

    if (cost_function == "tobler") {

        cf <- function(x){(6 * exp(-3.5 * abs(x[adj] + 0.05)))}

    }

    if (cost_function == "tobler offpath") {

        cf <- function(x){(6 * exp(-3.5 * abs(x[adj] + 0.05))) * 0.6}

    }

    if (cost_function == "modified tobler") {

        cf <- function(x){(4.8 * exp(-5.3 * abs((x[adj] * 0.7) + 0.03)))}

    }

    if (cost_function == "wheeled transport") {

        cf <- function(x){(1/(1 + abs(x[adj] * 100/crit_slope)^2))}

    }

    if (cost_function == "herzog") {

        cf <- function(x){(1/((1337.8 * x[adj]^6) + (278.19 * x[adj]^5) - (517.39 * x[adj]^4) - (78.199 * x[adj]^3) + (93.419 * x[adj]^2) + (19.825 * x[adj]) + 1.64))}

    }

    if (cost_function %in% cfs[!cfs == "all"]) {


        slope[adj] <- cf(slope)

        if (inherits(max_slope, "numeric")) {
            slope@transitionMatrix@x[slope@transitionMatrix@x == cf(rand)] <- 0
        }

        Conductance <- gdistance::geoCorrection(slope)
    }

    if (cost_function %in% cfs[cfs == "all"]) {

        slope_stack <- list(slope, slope, slope, slope, slope)

        cf1 <- function(x){(6 * exp(-3.5 * abs(x[adj] + 0.05)))}
        cf2 <- function(x){(6 * exp(-3.5 * abs(x[adj] + 0.05))) * 0.6}
        cf3 <- function(x){(4.8 * exp(-5.3 * abs((x[adj] * 0.7) + 0.03)))}
        cf4 <- function(x){(1/(1 + abs(x[adj] * 100/crit_slope)^2))}
        cf5 <- function(x){(1/((1337.8 * x[adj]^6) + (278.19 * x[adj]^5) - (517.39 * x[adj]^4) - (78.199 * x[adj]^3) + (93.419 * x[adj]^2) + (19.825 * x[adj]) + 1.64))}

        slope_stack[[1]][adj] <- cf1(slope)
        slope_stack[[2]][adj] <- cf2(slope)
        slope_stack[[3]][adj] <- cf3(slope)
        slope_stack[[4]][adj] <- cf4(slope)
        slope_stack[[5]][adj] <- cf5(slope)

        if (inherits(max_slope, "numeric")) {
            slope_stack[[1]]@transitionMatrix@x[slope_stack[[1]]@transitionMatrix@x == cf1(rand)] <- 0
            slope_stack[[2]]@transitionMatrix@x[slope_stack[[2]]@transitionMatrix@x == cf2(rand)] <- 0
            slope_stack[[3]]@transitionMatrix@x[slope_stack[[3]]@transitionMatrix@x == cf3(rand)] <- 0
            slope_stack[[4]]@transitionMatrix@x[slope_stack[[4]]@transitionMatrix@x == cf4(rand)] <- 0
            slope_stack[[5]]@transitionMatrix@x[slope_stack[[5]]@transitionMatrix@x == cf5(rand)] <- 0
        }


        Conductance <- lapply(slope_stack, function(x) { gdistance::geoCorrection(x)})

        names(Conductance) <- cfs[!cfs == "all"]

    }

    return(Conductance)

}

