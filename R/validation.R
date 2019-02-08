#' validation_buffer
#'
#' Calculates the accuracy of the Least Cost Path by reporting the percentage of Least Cost Path within certain distance(s) from another line.
#'
#' The function computes the accuracy of the Least Cost Path using the buffer method proposed by \href{https://www.tandfonline.com/doi/abs/10.1080/136588197242419}{Goodchild and Hunter (1997)}. Expects two SpatialLines objects and a vector of buffer distances. Returns buffer SpatialPolygons and data.frame with accuracy results.
#'
#' @param lcp Least Cost Path to assess the accuracy of. Expects object of class SpatialLines
#'
#' @param comparison SpatialLines to validate the Least Cost Path against. Expects object of class SpatialLines
#'
#' @param buffers Expects vector of buffer distances to assess. Default values are c(50, 100, 250, 500, 1000).
#'
#' @param export_buffers If TRUE then SpatialPolygons of the created buffers are exported. Default is FALSE.
#'
#'@param suffix Text to add to end of file name. Useful when assessing least cost paths with different buffer distances.
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
#' x1 <- c(1,5,4,8)
#' y1 <- c(1,3,4,7)
#' line1 <- sp::SpatialLines(list(sp::Lines(sp::Line(cbind(x1,y1)), ID='a')))
#' x2 <- c(1,5,5,8)
#' y2 <- c(1,4,6,7)
#' line2 <- sp::SpatialLines(list(sp::Lines(sp::Line(cbind(x2,y2)), ID='b')))
#'
#' validation_buffer(lcp = line1, comparison = line2, buffers = c(0.1, 0.2, 0.5, 1))

validation_buffer <- function(lcp, comparison, buffers = c(50, 100, 250, 500, 1000), export_buffers = FALSE, suffix = "") {
    if (inherits(lcp, "SpatialLines") & inherits(comparison, "SpatialLines")) {
        buffer_list <- list()
        n <- 0
        if (length(buffers) > 1) {
            for (i in buffers) {
                n <- n + 1
                buffer_list[[n]] <- rgeos::gBuffer(comparison, byid = FALSE, width = i)
            }

            buffer_output <- do.call(raster::bind, buffer_list)
        } else {
            buffer_output <- rgeos::gBuffer(comparison, byid = FALSE, width = buffers)
        }
    } else {
        return("Input lcp or comparison is not a SpatialLines")
    }

    if (export_buffers == TRUE) {

        buffer_output <- sp::SpatialPolygonsDataFrame(buffer_output, data.frame(seq(1, length(buffers)), match.id = FALSE))

        rgdal::writeOGR(buffer_output, ".", paste0("Buffers", "_", suffix), driver = "ESRI Shapefile", overwrite_layer = TRUE)

    }

    lcp_clipped <- rgeos::gIntersection(buffer_output, lcp, byid = TRUE)

    accuracy <- rgeos::gLength(lcp_clipped, byid = TRUE)/rgeos::gLength(lcp) * 100

    lcp_df <- data.frame(seq(1, length(buffers)), buffers, format(accuracy, digits = 3))
    names(lcp_df) <- c("ID", "Buffer Applied from data (m)", "Percent of LCP within Buffer Distance (%)")
    utils::write.csv(lcp_df, file = paste0("Least Cost Path Accuracy - Buffer Method", suffix, ".csv"), row.names = FALSE)
    return(lcp_df)
}

