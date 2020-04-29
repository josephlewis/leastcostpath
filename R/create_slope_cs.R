#' create_slope_cs
#'
#' Creates a Slope Cost Surface
#'
#' Creates a cost surface based on the difficulty of moving up/down slope. This function allows for multiple isotropic and anisotropic cost functions that estimate human movement across a landscape. The maximum percentage slope traversal can also be supplied.
#'
#' @details
#'
#' Tobler's 'Hiking Function' is the most widely used cost function when approximating the difficulty of moving across a landscape (Gorenflo and Gale, 1990; Wheatley and Gillings, 2001). The function assess the time necessary to traverse a surface and takes into account up-slope and down-slope (Kantner, 2004; Tobler, 1993).
#'
#' Tobler's offpath Hiking Function reduces the speed of the Tobler's Hiking Function by 0.6 to take into account walking off-path (Tobler, 1993)
#'
#'The Irmischer and Clark functions were modelled from speed estimates of United States Military Academy (USMA) cadets while they navigated on foot over hilly, wooded terrain as part of their summer training in map and compass navigation.
#'
#' The Modified Hiking cost function combines MIDE (París Roche, 2002), a method to calculate walking hours for an average hiker with a light load (Márquez-Pérez et al., 2017), and Tobler's 'Hiking Function' (Tobler, 1993). The Modified Hiking Function benefits from the precision of the MIDE rule and the continuity of Tobler's Hiking Function (Márquez-Pérez et al., 2017).
#'
#' Herzog (2013), based on the cost function provided by Llobera and Sluckin (2007), has provided a cost function to approximate the cost for wheeled transport. The cost function is symmetric and is most applicable for use when the same route was taken in both directions.
#'
#' Herzog's (2010) Sixth-degree polynomial cost function approximates the energy expenditure values found in Minetti et al. (2002) but eliminates the problem of unrealistic negative energy expenditure values for steep downhill slopes.
#'
#' Llobera and Sluckin (2007) cost function approximates the metabolic energy expenditure in KJ/(m*kg) when moving across a landscape.
#'
#' @param dem \code{RasterLayer} (raster package). Digital Elevation Model.
#'
#' @param cost_function Cost Function used in the Least Cost Path calculation. Implemented cost functions include 'tobler', 'tobler offpath', 'irmischer-clarke male', 'irmischer-clarke offpath male', 'irmischer-clarke female', 'irmischer-clarke offpath female', 'modified tobler', 'wheeled transport', 'herzog', 'llobera-sluckin', 'all'. Default is 'tobler'. See Details for more information.
#'
#' @param neighbours \code{numeric} value. Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected values are 4, 8, or 16. Default is 16.
#'
#' @param crit_slope Critical Slope (in percentage) is 'the transition where switchbacks become more effective than direct uphill or downhill paths'. Cost of climbing the critical slope is twice as high as those for moving on flat terrain and is used for estimating the cost of using wheeled vehicles. Default value is 12, which is the postulated maximum gradient traversable by ancient transport (Verhagen and Jeneson, 2012). Critical slope only used in 'wheeled transport' cost function.
#'
#' @param max_slope \code{numeric} value. Maximum percentage slope that is traversable. Default is NULL.
#'
#' @return \code{TransitionLayer} (gdistance package) numerically expressing the difficulty of moving up/down slope based on the cost function provided in the cost_function argument. list of \code{TransitionLayer} if cost_function = 'all'.
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

    cfs <- c("tobler", "tobler offpath", "irmischer-clarke male", "irmischer-clarke offpath male", "irmischer-clarke female", "irmischer-clarke offpath female",
        "modified tobler", "wheeled transport", "herzog", "llobera-sluckin", "all")

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

        cf <- function(x) {
            (6 * exp(-3.5 * abs(x[adj] + 0.05)))
        }

    }

    if (cost_function == "tobler offpath") {

        cf <- function(x) {
            (6 * exp(-3.5 * abs(x[adj] + 0.05))) * 0.6
        }

    }

    if (cost_function == "irmischer-clarke male") {

        cf <- function(x) {
            (0.11 + exp(-(abs(x[adj]) * 100 + 5)^2/(2 * 30^2)))
        }

    }

    if (cost_function == "irmischer-clarke offpath male") {

        cf <- function(x) {
            (0.11 + 0.67 * exp(-(abs(x[adj]) * 100 + 2)^2/(2 * 30^2)))
        }

    }

    if (cost_function == "irmischer-clarke female") {

        cf <- function(x) {
            0.95 * (0.11 + exp(-(abs(x[adj]) * 100 + 5)^2/(2 * 30^2)))
        }

    }

    if (cost_function == "irmischer-clarke offpath female") {

        cf <- function(x) {
            0.95 * (0.11 + 0.67 * exp(-(abs(x[adj]) * 100 + 2)^2/(2 * 30^2)))
        }

    }

    if (cost_function == "modified tobler") {

        cf <- function(x) {
            (4.8 * exp(-5.3 * abs((x[adj] * 0.7) + 0.03)))
        }

    }

    if (cost_function == "wheeled transport") {

        cf <- function(x) {
            (1/(1 + abs(x[adj] * 100/crit_slope)^2))
        }

    }

    if (cost_function == "herzog") {

        cf <- function(x) {
            (1/((1337.8 * x[adj]^6) + (278.19 * x[adj]^5) - (517.39 * x[adj]^4) - (78.199 * x[adj]^3) + (93.419 * x[adj]^2) + (19.825 * x[adj]) +
                1.64))
        }

    }

    if (cost_function == "llobera-sluckin") {

        cf <- function(x) {
            (1/(2.635 + (17.37 * abs(x[adj])) + (42.37 * abs(x[adj])^2) - (21.43 * abs(x[adj])^3) + (14.93 * abs(x[adj])^4)))
        }

    }

    if (cost_function %in% cfs[!cfs == "all"]) {


        slope[adj] <- cf(slope)

        if (inherits(max_slope, "numeric")) {
            slope[adj] <- ifelse(slope[adj] == unique(cf(rand))[!is.na(unique(cf(rand)))], 0, slope[adj])
            slope[adj] <- ifelse(slope[adj] == unique(cf(0))[!is.na(unique(cf(0)))], 0, slope[adj])
        }

        Conductance <- gdistance::geoCorrection(slope)
    }

    if (cost_function %in% cfs[cfs == "all"]) {

        slope_stack <- list(slope, slope, slope, slope, slope, slope, slope, slope, slope, slope)

        cf1 <- function(x) {
            (6 * exp(-3.5 * abs(x[adj] + 0.05)))
        }
        cf2 <- function(x) {
            (6 * exp(-3.5 * abs(x[adj] + 0.05))) * 0.6
        }

        cf3 <- function(x) {
            (0.11 + exp(-(abs(x[adj]) * 100 + 5)^2/(2 * 30^2)))
        }

        cf4 <- function(x) {
            (0.11 + 0.67 * exp(-(abs(x[adj]) * 100 + 2)^2/(2 * 30^2)))
        }

        cf5 <- function(x) {
            0.95 * (0.11 + exp(-(abs(x[adj]) * 100 + 5)^2/(2 * 30^2)))
        }

        cf6 <- function(x) {
            0.95 * (0.11 + 0.67 * exp(-(abs(x[adj]) * 100 + 2)^2/(2 * 30^2)))
        }

        cf7 <- function(x) {
            (4.8 * exp(-5.3 * abs((x[adj] * 0.7) + 0.03)))
        }
        cf8 <- function(x) {
            (1/(1 + abs(x[adj] * 100/crit_slope)^2))
        }
        cf9 <- function(x) {
            (1/((1337.8 * x[adj]^6) + (278.19 * x[adj]^5) - (517.39 * x[adj]^4) - (78.199 * x[adj]^3) + (93.419 * x[adj]^2) + (19.825 * x[adj]) +
                1.64))
        }
        cf10 <- function(x) {
            (1/(2.635 + (17.37 * abs(x[adj])) + (42.37 * abs(x[adj])^2) - (21.43 * abs(x[adj])^3) + (14.93 * abs(x[adj])^4)))
        }

        slope_stack[[1]][adj] <- cf1(slope)
        slope_stack[[2]][adj] <- cf2(slope)
        slope_stack[[3]][adj] <- cf3(slope)
        slope_stack[[4]][adj] <- cf4(slope)
        slope_stack[[5]][adj] <- cf5(slope)
        slope_stack[[6]][adj] <- cf6(slope)
        slope_stack[[7]][adj] <- cf7(slope)
        slope_stack[[8]][adj] <- cf8(slope)
        slope_stack[[9]][adj] <- cf9(slope)
        slope_stack[[10]][adj] <- cf10(slope)

        if (inherits(max_slope, "numeric")) {

            slope_stack[[1]][adj] <- ifelse(slope_stack[[1]][adj] == unique(cf1(rand))[!is.na(unique(cf1(rand)))], 0, slope_stack[[1]][adj])
            slope_stack[[1]][adj] <- ifelse(slope_stack[[1]][adj] == unique(cf1(0))[!is.na(unique(cf1(0)))], 0, slope_stack[[1]][adj])

            slope_stack[[2]][adj] <- ifelse(slope_stack[[2]][adj] == unique(cf2(rand))[!is.na(unique(cf2(rand)))], 0, slope_stack[[2]][adj])
            slope_stack[[2]][adj] <- ifelse(slope_stack[[2]][adj] == unique(cf2(0))[!is.na(unique(cf2(0)))], 0, slope_stack[[2]][adj])

            slope_stack[[3]][adj] <- ifelse(slope_stack[[3]][adj] == unique(cf3(rand))[!is.na(unique(cf3(rand)))], 0, slope_stack[[3]][adj])
            slope_stack[[3]][adj] <- ifelse(slope_stack[[3]][adj] == unique(cf3(0))[!is.na(unique(cf3(0)))], 0, slope_stack[[3]][adj])

            slope_stack[[4]][adj] <- ifelse(slope_stack[[4]][adj] == unique(cf4(rand))[!is.na(unique(cf4(rand)))], 0, slope_stack[[4]][adj])
            slope_stack[[4]][adj] <- ifelse(slope_stack[[4]][adj] == unique(cf4(0))[!is.na(unique(cf4(0)))], 0, slope_stack[[4]][adj])

            slope_stack[[5]][adj] <- ifelse(slope_stack[[5]][adj] == unique(cf5(rand))[!is.na(unique(cf5(rand)))], 0, slope_stack[[5]][adj])
            slope_stack[[5]][adj] <- ifelse(slope_stack[[5]][adj] == unique(cf5(0))[!is.na(unique(cf5(0)))], 0, slope_stack[[5]][adj])

            slope_stack[[6]][adj] <- ifelse(slope_stack[[6]][adj] == unique(cf6(rand))[!is.na(unique(cf6(rand)))], 0, slope_stack[[6]][adj])
            slope_stack[[6]][adj] <- ifelse(slope_stack[[6]][adj] == unique(cf6(0))[!is.na(unique(cf6(0)))], 0, slope_stack[[6]][adj])

            slope_stack[[7]][adj] <- ifelse(slope_stack[[7]][adj] == unique(cf7(rand))[!is.na(unique(cf7(rand)))], 0, slope_stack[[7]][adj])
            slope_stack[[7]][adj] <- ifelse(slope_stack[[7]][adj] == unique(cf7(0))[!is.na(unique(cf7(0)))], 0, slope_stack[[7]][adj])

            slope_stack[[8]][adj] <- ifelse(slope_stack[[8]][adj] == unique(cf8(rand))[!is.na(unique(cf8(rand)))], 0, slope_stack[[8]][adj])
            slope_stack[[8]][adj] <- ifelse(slope_stack[[8]][adj] == unique(cf8(0))[!is.na(unique(cf8(0)))], 0, slope_stack[[8]][adj])

            slope_stack[[9]][adj] <- ifelse(slope_stack[[9]][adj] == unique(cf9(rand))[!is.na(unique(cf9(rand)))], 0, slope_stack[[9]][adj])
            slope_stack[[9]][adj] <- ifelse(slope_stack[[9]][adj] == unique(cf9(0))[!is.na(unique(cf9(0)))], 0, slope_stack[[9]][adj])

            slope_stack[[10]][adj] <- ifelse(slope_stack[[10]][adj] == unique(cf10(rand))[!is.na(unique(cf10(rand)))], 0, slope_stack[[10]][adj])
            slope_stack[[10]][adj] <- ifelse(slope_stack[[10]][adj] == unique(cf10(0))[!is.na(unique(cf10(0)))], 0, slope_stack[[10]][adj])



        }


        Conductance <- lapply(slope_stack, function(x) {
            gdistance::geoCorrection(x)
        })

        names(Conductance) <- cfs[!cfs == "all"]

    }

    return(Conductance)

}
