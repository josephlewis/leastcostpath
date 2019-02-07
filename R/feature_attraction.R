#' feature_attraction
#'
#' Calculates landscape feature attraction cost RasterLayer
#'
#' The function computes a landscape feature attraction cost RasterLayer. It implements the method proposed by Llobera (2000) and uses the linear decay rate to decrease the attraction as distance increases from the feature. The function takes a Digital Elevation Model ('RasterLayer' class) and the linear decay rate.
#'
#' @param dem Digital Elevation Model. Expects Object of class RasterLayer
#'
#' @param locations Location from which the Least Cost Path is calculated. Expects Object of class SpatialPoints or data.frame
#'
#' @param viewshed Cost Function to be used in the Least Cost Path calculation. Current implementation computes LCPs using Tobler's Hiking function, Marquez-Perez et al. Modified Hiking function, Herzog's wheeled transport function, and Herzog's sixth degree polynomial function. Default parameter value is is 'all'. See Details for more.
#'
#'@param decay If FALSE (default) then Least Cost Paths computed from origin to destination and destination to origin. This is to reflect the ansitropy of Tobler's and Marquez-Perez's cost functions. If TRUE then Least Cost Paths computed from origin to destination only. see Details for more.
#'
#' @param decay_rate Number of directions used in the Least Cost Path calculation. \href{https://www.ncbi.nlm.nih.gov/pubmed/17892887}{Huber and Church (1985)} for methodological considerations when considering number of neighbours. Expected input values are 4, 8, 16. Default is 16.
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

feature_attraction <- function(dem, locations, viewshed = NULL, decay = "linear", decay_rate = c(5, 500), suffix = "") {

    if (inherits(locations, "SpatialPoints")) {
        locations <- data.frame(locations)
    }

    decay_invert_stack <- stack()

    for (i in 1:nrow(locations)) {

        dem_locations <- rasterize(coordinates(locations[i, ]), dem)

        location_distance <- distance(dem_locations)

        decay_invert <- calc(location_distance, fun = function(x) {
            (-abs(decay_rate[1])/abs(decay_rate[2])) * (x[1]) + abs(decay_rate[1])
        })

        plot(decay_invert)

        decay_invert_stack <- raster::stack(decay_invert_stack, decay_invert)

        plot(decay_invert_stack[[i]])

    }

    if (nlayers(decay_invert_stack) > 1) {
        decay_invert_sum <- sum(decay_invert_stack)
    } else {
        decay_invert_sum <- decay_invert_stack
    }


    if (inherits(viewshed, "RasterLayer")) {

        view <- viewshed

        view[view == 0] <- NA

        attraction_raster <- decay_invert_sum * view

        attraction_raster[is.na(attraction_raster) | attraction_raster < 1] <- 1

        plot(attraction_raster, main = "test")

    } else if (inherits(viewshed, "NULL")) {

        decay_invert_sum[is.na(decay_invert_sum) | decay_invert_sum < 1] <- 1

        attraction_raster <- decay_invert_sum

        plot(attraction_raster, main = "test")

    } else {
        print("Input RasterLayer")
    }

    writeRaster(attraction_raster, paste0("feature_attraction", "_", suffix, ".tif"), overwrite = TRUE)

}
