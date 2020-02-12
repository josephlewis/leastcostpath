#' validate_lcp
#'
#' Calculates the accuracy of the Least Cost Path by calculating the percentage of Least Cost Path within certain distance(s) from another SpatialLine.
#'
#' The function computes the accuracy of the Least Cost Path using the buffer method proposed by \href{https://www.tandfonline.com/doi/abs/10.1080/136588197242419}{Goodchild and Hunter (1997)}. Expects two SpatialLines/SpatialLinesDataFrame objects and a numeric vector of buffer distances. Returns a data.frame with accuracy results.
#'
#' @param lcp Least Cost Path to assess the accuracy of. Expects object of class SpatialLines/SpatialLinesDataFrame
#'
#' @param comparison SpatialLines to validate the Least Cost Path against. Expects object of class SpatialLines/SpatialLinesDataFrame
#'
#' @param buffers Vector of buffer distances to assess. Default values are c(50, 100, 250, 500, 1000).
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

validate_lcp <- function(lcp, comparison, buffers = c(50, 100, 250, 500, 1000)) {
    
    if (!inherits(lcp, "SpatialLines")) {
        stop("lcp expects a SpatialLines or SpatialLinesDataFrame object")
    }
    
    if (!inherits(comparison, "SpatialLines")) {
        stop("Comparison expects a SpatialLines object, SpatialLinesDataFrame object, or SpatialPolygonsDataFrame object")
    }
    
    if (!inherits(buffers, "numeric")) {
        stop("buffers must include numeric values only")
    }
    
    if (length(buffers) > 1) {
        buffer_list <- list()
        
        for (i in 1:length(buffers)) {
            buffer_list[[i]] <- rgeos::gBuffer(comparison, byid = FALSE, width = buffers[i])
        }
        
        buffer_output <- do.call(raster::bind, buffer_list)
        
    } else {
        buffer_output <- rgeos::gBuffer(comparison, byid = FALSE, width = buffers[1])
    }
    
    buffer_output <- sp::SpatialPolygonsDataFrame(buffer_output, data = data.frame(ID = 1:length(buffers), row.names = 1:length(buffers)), match.ID = FALSE)
    
    lcp_clipped <- rgeos::gIntersection(buffer_output, lcp, byid = TRUE)
    
    accuracy <- rgeos::gLength(lcp_clipped, byid = TRUE)/rgeos::gLength(lcp) * 100
    
    lcp_df <- data.frame(ID = 1:length(buffers), Buffer_Applied_from_data = buffers, Percent_LCP_within_Distance = accuracy, stringsAsFactors = FALSE)
    
    return(lcp_df)
}
