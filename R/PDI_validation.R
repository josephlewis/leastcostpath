#' Calculate Path Deviation Index
#'
#' Calculates the Path Deviation Index of a Least Cost Path and a comparison SPatialLines using the method proposed by Jan et al. (1999).
#'
#' @param lcp \code{SpatialLines*} (sp package). Least Cost Path to assess the accuracy of. Expects object of class SpatialLines.
#'
#' @param comparison \code{SpatialLines*} to validate the Least Cost Path against.
#'
#' @param normalise \code{logical}. If TRUE, PDI is normalised to allow for comparisons when PDIs are calculated for paths with different origin and destination points. Default is FALSE.
#'
#' @references
#'
#' Jan, O., Horowitz, A.J., Peng, Z,R. 1999. Using GPS data to understand variations in path choice. Paper presented at the 78th meeting of the Transportation Research Board, Washington.
#'
#' @return \code{numeric value} (base package). The average distance between two paths and is calculated as the area between paths divided by the shortest path (straight line distance) between the start and end point.
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
#' x1 <- c(1,5,4,50)
#' y1 <- c(1,3,4,50)
#' line1 <- sp::SpatialLines(list(sp::Lines(sp::Line(cbind(x1,y1)), ID='a')))
#' x2 <- c(1,5,5,50)
#' y2 <- c(1,4,6,50)
#' line2 <- sp::SpatialLines(list(sp::Lines(sp::Line(cbind(x2,y2)), ID='b')))
#'
#'val_lcp <- PDI_validation(lcp = line1, line2)

PDI_validation <- function(lcp, comparison, normalise = FALSE) {
    
    if (!inherits(lcp, "SpatialLines")) {
        stop("lcp argument is invalid. Expecting SpatialLines* object")
    }
    
    if (!inherits(comparison, "SpatialLines")) {
        stop("Comparison argument is invalid. Expecting a SpatialLines* object")
    }
    
    lcp_pts <- methods::as(lcp, "SpatialPoints")
    comparison_pts <- methods::as(comparison, "SpatialPoints")
    
    lcp_coords <- sp::coordinates(lcp_pts)
    comparison_coords <- sp::coordinates(comparison_pts)
    
    start <- lcp_coords[1, ]
    end <- lcp_coords[base::nrow(lcp_coords), ]
    
    coords <- base::rbind(lcp_coords, comparison_coords)
    
    p = sp::Polygon(coords)
    ps = sp::Polygons(list(p), 1)
    sps = sp::SpatialPolygons(list(ps), proj4string = crs(lcp))
    
    plot(sps, col = "black")
    
    PDI_area <- rgeos::gArea(sps, byid = FALSE)
    
    max_dist <- raster::pointDistance(p1 = start, p2 = end, type = "Euclidean", lonlat = FALSE)
    
    PDI <- PDI_area/max_dist
    
    if (normalise) {
        
        PDI <- (PDI_area/max_dist)/max_dist * 100
        
    }
    
    return(PDI)
    
}
