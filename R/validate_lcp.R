#' validate_lcp
#'
#' Calculates the accuracy of the Least Cost Path by calculating the percentage of Least Cost Path within certain distance(s) from another SpatialLine.
#'
#' The function computes the accuracy of the Least Cost Path using the buffer method proposed by \href{https://www.tandfonline.com/doi/abs/10.1080/136588197242419}{Goodchild and Hunter (1997)}. Expects two SpatialLines objects and a vector of buffer distances. Returns buffer SpatialPolygons and data.frame with accuracy results.
#'
#' @param lcp Least Cost Path to assess the accuracy of. Expects object of class SpatialLines
#'
#' @param comparison SpatialLines to validate the Least Cost Path against. Expects object of class SpatialLines
#'
#' @param buffers Expects vector of buffer distances to assess. Default values are c(50, 100, 250, 500, 1000).
#'
#' @param suffix Text to add to end of file name. Useful when assessing least cost paths with different buffer distances.
#'
#'@param export Exports csv with calculated percentage within chosen buffers to current directory.
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
#' validate_lcp(lcp = line1, comparison = line2, buffers = c(0.1, 0.2, 0.5, 1))

validate_lcp <- function(lcp, comparison, buffers = c(50, 100, 250, 500, 1000), suffix = "", export = FALSE) {
    if (inherits(lcp, "SpatialLines") & inherits(comparison, "SpatialLines") | inherits(comparison, "SpatialPolygonsDataFrame")) {
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
    
    buffer_output <- sp::SpatialPolygonsDataFrame(buffer_output, data.frame(seq(1, length(buffers)), match.id = FALSE))
    
    lcp_clipped <- rgeos::gIntersection(buffer_output, lcp, byid = TRUE)
    
    accuracy <- rgeos::gLength(lcp_clipped, byid = TRUE)/rgeos::gLength(lcp) * 100
    
    lcp_df <- data.frame(seq(1, length(buffers)), buffers, format(accuracy, digits = 3))
    names(lcp_df) <- c("ID", "Buffer Applied from data (m)", "Percent of LCP within Buffer Distance (%)")
    
    if (export_table == TRUE) {
        utils::write.csv(lcp_df, file = paste0("Least Cost Path Accuracy - Buffer Method", suffix, ".csv"), row.names = FALSE)
    }
    return(lcp_df)
}
