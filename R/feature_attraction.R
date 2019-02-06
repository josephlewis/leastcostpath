feature_attraction <- function(dem, locations, viewshed = NULL, decay = "linear", decay_rate = c(5, 500), export_name = "feature attraction") {
  
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

    writeRaster(attraction_raster, paste0(export_name, ".tif"), overwrite = TRUE)

}