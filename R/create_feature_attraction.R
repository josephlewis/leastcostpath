#' create_feature_attraction
#'
#' Creates landscape feature attraction Cost Surface to be used in Least Cost Path calculation.
#'
#'The create_feature_attraction function computes a cost surface representating the landscape feature attraction.It implements ths method proposed by Llobera (200) and uses a linear decay rate to decrease the attraction of the feature as distance increases from the feature.The function requires a Digital Elevation Model (class 'RasterLayer'),the location of the landscape(s) (class 'SpatialPoints'),the maximum attraction value of the landscape feature(s), and the maximum distance that the landscape feature has influence.
#'
#' @param dem Digital Elevation Model. Expects Object of class RasterLayer
#'
#' @param locs Locations of landscape features. Expects Object of class SpatialPoints
#'
#' @param max_attraction Value denoting the maximum attraction value of the landscape feature(s). Default is 5. Value must be above 0.
#' @param distance Maximum distance that the landscape feature has influence. Default is 500. Value must be above 0.
#'
#' @param eq Attraction decay rate: 'linear' or 'exponential'
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
#' locs <- rbind(loc1, loc2)
#' feature_attraction <- create_feature_attraction(dem = r, locs = locs,
#' max_attraction = 5, distance = 20)

create_feature_attraction <- function(dem, locs, max_attraction = 5, distance = 500, eq = "linear") {

    message("note: create_feature_attraction expects planar coordinates")

    if (!inherits(dem, "RasterLayer")) {
        stop("dem expects a RasterLayer object")
    }

    if (!inherits(locs, "SpatialPoints")) {
        stop("locs expects a SpatialPoints object")
    }

    if (max_attraction <= 0 | distance <= 0) {
        stop("max_attraction and distance must be above 0")
    }

    if (any(eq != "linear" & eq != "exponential")) {
        stop("eq expects 'linear' or 'exp' as an argument")
    }

    locs_raster <- raster::rasterize(coordinates(locs), dem)

    locs_distance <- raster::distance(locs_raster)

    if (eq == "linear") {

        attraction_decay <- raster::calc(locs_distance, fun = function(x) {
            (-abs(max_attraction)/abs(distance)) * (x) + abs(max_attraction)
        })

    }

    if (eq == "exponential") {

        attraction_decay <- raster::calc(locs_distance, fun = function(x) {

            (abs(max_attraction) * exp((log(0.001/max_attraction)/distance) * (x)))
        })

    }

    attraction_decay[is.na(attraction_decay) | attraction_decay < 1] <- 1

    attraction_decay <- gdistance::transition(attraction_decay, mean, 16, symm = TRUE)

    return(attraction_decay)

}
