validation_buffer <- function(lcp, data, buffers = c(50, 100, 250, 500, 1000), export = FALSE) {
    if (inherits(lcp, "SpatialLines") & inherits(data, "SpatialLines")) {
        buffer_list <- list()
        n <- 0
        if (length(buffers) > 1) {
            for (i in buffers) {
                n <- n + 1
                buffer_list[[n]] <- rgeos::gBuffer(data, byid = FALSE, width = i)
            }

            buffer_output <- do.call(raster::bind, buffer_list)
        } else {
            buffer_output <- rgeos::gBuffer(data, byid = FALSE, width = buffers)
        }
    } else {
        return("Input lcp or data is not a SpatialLines")
    }

    if (export == TRUE) {

        buffer_output <- sp::SpatialPolygonsDataFrame(buffer_output, data.frame(seq(1, length(buffers)), match.id = FALSE))

        rgdal::writeOGR(buffer_output, ".", paste0("Buffers", "_", format(Sys.time(), "%d-%b-%Y %H.%M")), driver = "ESRI Shapefile", overwrite_layer = TRUE)

    }

    lcp_clipped <- rgeos::gIntersection(buffer_output, lcp, byid = TRUE)

    accuracy <- rgeos::gLength(lcp_clipped, byid = TRUE)/gLength(data) * 100

    lcp_df <- data.frame(seq(1, length(buffers)), buffers, format(accuracy, digits = 3))
    names(lcp_df) <- c("ID", "Buffer Applied from data (m)", "Percent of LCP within Buffer Distance (%)")
    utils::write.csv(lcp_df, file = paste0("Least Cost Path Accuracy - Buffer Method", "_", format(Sys.time(), "%d-%b-%Y %H.%M"), ".csv"), row.names = FALSE)
    return(lcp_df)
}

