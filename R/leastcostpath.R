#' leastcostpath
#'
#' Calculates Least Cost Paths for archaeological application
#'
#' The function computes the Least Cost Path from an origin location to a destination location. It implements multiple isotropic and anisotropic cost functions that estimate human movement across a landscape. It also implements symmetrical and assymetrical cost functions for moving across slope as well as allowing for the incorporation of other costs such as landscape feature attractions. The function takes a Digital Elevation Model ('RasterLayer' class) and point features ('SpatialPoints' class) for the origin location and destination location.
#'
#' @param dem Digital Elevation Model. Expects Object of class RasterLayer
#'
#' @param origin Location from which the Least Cost Path is calculated. Expects Object of class SpatialPoints or data.frame
#'
#' @param destination Location to which the Least Cost Path is calculated. Expects Object of class SpatialPoints or data.frame
#'
#' @param cost_function Cost Function to be used in the Least Cost Path calculation. Current implementation computes LCPs using Tobler's Hiking function, Marquez-Perez et al. Modified Hiking function, Herzog's wheeled transport function, and Herzog's sixth degree polynomial function. Default parameter value is is 'all'. See Details for more.
#'
#'@param directional If FALSE (default) then Least Cost Paths computed from origin to destination and destination to origin. This is to reflect the ansitropy of Tobler's and Marquez-Perez's cost functions. If TRUE then Least Cost Paths computed from origin to destination only. see Details for more.
#'
#' @param neighbours Number of directions used in the Least Cost Path calculation. \href{https://www.ncbi.nlm.nih.gov/pubmed/17892887}{Huber and Church (1985)} for methodological considerations when considering number of neighbours. Expected input values are 4, 8, 16. Default is 16.
#'
#' @param crit_slope Critical Slope (in percent) is 'the transition where switchbacks become more effective than direct uphill or downhill paths'. Cost of climbing the critical slope is twice as high as those for moving on flat terrain and is used for estimating the cost of using wheeled vehicles. Critical slope defaulted is 15 degrees, which is the postulated maximum gradient traversable by ancient transport (Verhagen and Jeneson, 2012).
#'
#' @param traverse Cost function to be used to quantify cost when moving across a slope. Input values are 'asymmetrical', 'symmetrical', 'none'. Default value is 'assymetrical'. See details for more.
#'
#' @param other_costs Ability to add other costs such as landscape feature attractions, rivers, political boundaries. Expects Object of class RasterLayer or RasterStack. See Details for more.
#'
#' @param suffix Text to add to end of file name. Useful when calculating least cost paths with different parameters.
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
#' loc1 = cbind(2667670, 6479000)
#' loc1 = sp::SpatialPoints(loc1)
#'
#' loc2 = cbind(2667800, 6479400)
#' loc2 = sp::SpatialPoints(loc2)
#'
#' leastcostpath(dem = r, origin = loc1, destination = loc2, traverse = 'asymmetrical')

leastcostpath <- function(dem, origin, destination, cost_function = "all", directional = FALSE, neighbours = 16, crit_slope = 12, traverse = "asymmetrical", other_costs = c(),
    suffix = "") {

    altDiff_slope <- function(x) {
        x[2] - x[1]
    }

    hd <- gdistance::transition(dem, altDiff_slope, 8, symm = FALSE)

    slope <- gdistance::geoCorrection(hd)

    if (cost_function == "all") {
        adj <- raster::adjacent(dem, cells = 1:raster::ncell(dem), pairs = TRUE, directions = neighbours)
        slope_stack <- raster::stack(slope, slope, slope, slope, slope)

        slope_stack[[1]][adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
        slope_stack[[2]][adj] <- 4.8 * exp(-5.3 * abs((slope[adj] * 0.7) + 0.03))
        slope_stack[[3]][adj] <- 1/(1 + abs(slope[adj] * 100/crit_slope)^2)
        slope_stack[[4]][adj] <- 1/((((1337.8 * slope[adj]^6) + (278.19 * slope[adj]^5) - (517.39 * slope[adj]^4) - (78.199 * slope[adj]^3) + (93.419 * slope[adj]^2) +
            (19.825 * slope[adj]) + 1.64)))

        Conductance <- raster::stack(gdistance::geoCorrection(slope_stack[[1]]), gdistance::geoCorrection(slope_stack[[2]]), gdistance::geoCorrection(slope_stack[[3]]),
            gdistance::geoCorrection(slope_stack[[4]]))
    }

    if (traverse == "asymmetrical" | traverse == "symmetrical" | traverse == "none") {

        aspect_dem <- raster::terrain(dem, opt = "aspect", unit = "degrees", neighbors = 8)

        aspect_dem <- calc(aspect_dem, function(x) {
            ifelse(x >= 180, x - 180, x)
        })

        aspect_dem <- calc(aspect_dem, function(x) {
            ifelse(x >= 0 & x <= 90, x + 90, x - 90)
        })

        if (traverse == "asymmetrical") {
            altDiff_traverse <- function(x) {

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

            trans <- gdistance::transition(aspect_dem, altDiff_traverse, neighbours, symm = FALSE)
            trans <- gdistance::geoCorrection(trans)

            Conductance[[1]] <- Conductance[[1]] * trans
            Conductance[[2]] <- Conductance[[2]] * trans
            Conductance[[3]] <- Conductance[[3]] * trans
            Conductance[[4]] <- Conductance[[4]] * trans

        } else if (traverse == "symmetrical") {

            altDiff_traverse <- function(x) {

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

            trans <- gdistance::transition(aspect_dem, altDiff_traverse, neighbours, symm = FALSE)
            trans <- gdistance::geoCorrection(trans)

            Conductance[[1]] <- Conductance[[1]] * trans
            Conductance[[2]] <- Conductance[[2]] * trans
            Conductance[[3]] <- Conductance[[3]] * trans
            Conductance[[4]] <- Conductance[[4]] * trans

        } else if (traverse == "none") { 
          Conductance[[1]] <- Conductance[[1]] 
          Conductance[[2]] <- Conductance[[2]] 
          Conductance[[3]] <- Conductance[[3]] 
          Conductance[[4]] <- Conductance[[4]] 
          }
    } else { 
      stop("traverse expecting 'asymmertrical', 'symmetrical' or 'none'. See ?leastcostpath for more details.")
      }

    if (inherits(other_costs, "RasterStack")) {

        other_costs_prod <- prod(other_costs)

        cost_trans <- gdistance::transition(other_costs_prod, mean, 16)
        cost_trans <- gdistance::geoCorrection(cost_trans)

        Conductance[[1]] <- Conductance[[1]] * cost_trans
        Conductance[[2]] <- Conductance[[2]] * cost_trans
        Conductance[[3]] <- Conductance[[3]] * cost_trans
        Conductance[[4]] <- Conductance[[4]] * cost_trans

    } else if (inherits(other_costs, "RasterLayer")) {
        other_costs_prod <- other_costs

        cost_trans <- gdistance::transition(other_costs_prod, mean, 16)
        cost_trans <- gdistance::geoCorrection(cost_trans)

        Conductance[[1]] <- Conductance[[1]] * cost_trans
        Conductance[[2]] <- Conductance[[2]] * cost_trans
        Conductance[[3]] <- Conductance[[3]] * cost_trans
        Conductance[[4]] <- Conductance[[4]] * cost_trans
    }

    if (inherits(origin, "SpatialPoints") & inherits(destination, "SpatialPoints")) {

        sPath <- list()
        origin <- sp::coordinates(origin)
        destination <- sp::coordinates(destination)
    } else {
        return("Origin or Destination is not a SpatialPoints")
    }

    if (directional == "TRUE") {

        sPath[[1]] <- gdistance::shortestPath(Conductance[[1]], origin, destination, output = "SpatialLines")
        sPath[[2]] <- gdistance::shortestPath(Conductance[[2]], origin, destination, output = "SpatialLines")
        sPath[[3]] <- gdistance::shortestPath(Conductance[[3]], origin, destination, output = "SpatialLines")
        sPath[[4]] <- gdistance::shortestPath(Conductance[[4]], origin, destination, output = "SpatialLines")

        names(sPath) <- c("Toblers A to B", "Marquez-Perez A to B", "Lloberas A to B", "Herzog A to B")

    } else {

        sPath[[1]] <- gdistance::shortestPath(Conductance[[1]], origin, destination, output = "SpatialLines")
        sPath[[2]] <- gdistance::shortestPath(Conductance[[1]], destination, origin, output = "SpatialLines")
        sPath[[3]] <- gdistance::shortestPath(Conductance[[2]], origin, destination, output = "SpatialLines")
        sPath[[4]] <- gdistance::shortestPath(Conductance[[2]], destination, origin, output = "SpatialLines")
        sPath[[5]] <- gdistance::shortestPath(Conductance[[3]], origin, destination, output = "SpatialLines")
        sPath[[6]] <- gdistance::shortestPath(Conductance[[4]], origin, destination, output = "SpatialLines")
        names(sPath) <- c("Toblers A to B", "Toblers B to A", "Marquez-Perez A to B", "Marquez-Perez B to A", "Lloberas A to B", "Herzog A to B")
    }

    lcp_lengths <- unlist(lapply(sPath, function(x) {
        FUN = rgeos::gLength(x, byid = TRUE)
    }))

    for (i in 1:length(sPath)) {
        sPath[[i]]$length <- lcp_lengths[i]
    }

    for (i in 1:length(sPath)) {
        rgdal::writeOGR(sPath[[i]], ".", paste0(names(sPath)[i], suffix), driver = "ESRI Shapefile", overwrite_layer = TRUE)

    }
}
