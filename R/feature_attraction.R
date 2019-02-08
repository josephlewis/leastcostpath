#' feature_attraction
#'
#' Calculates landscape feature attraction cost RasterLayer
#'
#' The function computes a landscape feature attraction cost RasterLayer. It implements the method proposed by Llobera (2000) and uses the linear decay rate to decrease the attraction as distance increases from the feature. The function takes a Digital Elevation Model ('RasterLayer' class), the location of the features and the linear decay rate.
#'
#' @param dem Digital Elevation Model. Expects Object of class RasterLayer
#'
#' @param locations Locations of landscape features. Expects Object of class SpatialPoints or data.frame
#'
#' @param viewshed if viewshed is supplied with a RasterLayer then landscape feature attraction values are limited to only visible areas.
#'
#'@param decay Default is "linear". Exponential to be implemented soon.
#'
#' @param decay_rate Expects vector of two values denoting the Rank of the landscape feature and the maximum attraction value. Default is c(5, 500). scale in metres (m).
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

        decay_invert_stack <- raster::stack(decay_invert_stack, decay_invert)

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

    } else if (inherits(viewshed, "NULL")) {

        decay_invert_sum[is.na(decay_invert_sum) | decay_invert_sum < 1] <- 1

        attraction_raster <- decay_invert_sum

    } else {
        print("Input RasterLayer")
    }

    writeRaster(attraction_raster, paste0("feature_attraction", suffix, ".tif"), overwrite = TRUE)

}
