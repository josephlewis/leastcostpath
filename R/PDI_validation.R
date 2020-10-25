#' Calculate Path Deviation Index
#'
#' Calculates the Path Deviation Index of a Least Cost Path and a comparison SpatialLines using the method proposed by Jan et al. (1999).
#'
#' @param lcp \code{SpatialLines*} (sp package). Least Cost Path to assess the accuracy of. Expects object of class SpatialLines.
#'
#' @param comparison \code{SpatialLines*} to validate the Least Cost Path against. Expects object of class SpatialLines.
#'
#' @param snap \code{logical}. If TRUE (default) origin and destination points of the Least Cost path are snapped to the Origin and Destination points of the comparison SpatialLine. This ensures that the SpatialPolygon that is returned is valid as the Origin and Destination points of the Least Cost Path is the centre of the Raster cell whilst the Origin and Destination of the comparison SpatialLine is not restricted by the Raster Grid.
#'
#' @details
#'
#' The Path Deviation Index measures the deviation (i.e. the spatial separation) between a pair of paths and aims to overcome the shortcomings of measuring the percentage of coverage of a least cost path from a comparison path (for example, the validation_lcp function).
#'
#' The index is defined as the area between paths divided by the distance of the shortest path (i.e. Euclidean) between an origin and destination. The index can be interpreted as the average distance between the paths.
#'
#' \code{Path Deviation Index  = Area between paths / length of shortest path}
#'
#' The value of the Path Deviation Index depends on the length of the path and makes comparison of PDIs difficult for paths with different origins and destinations. This can be overcome by normalising the Path Deviation Index by the distance of the shortest path (i.e. Euclidean) between an origin and destination.
#'
#' \code{Normalised PDI = PDI / length of shortest path x 100}
#'
#' The normalised Path Deviation Index is the percent of deviation between the two paths over the shortest path. For example, if a normalised PDI is 30 percent, it means that the average distance between two paths is 30 percent of the length of the shortest path. With normalised PDI, all path deviation can be compared regardless of the length of the shortest path.
#'
#' @references
#'
#' Jan, O., Horowitz, A.J., Peng, Z,R. 1999. Using GPS data to understand variations in path choice. Paper presented at the 78th meeting of the Transportation Research Board, Washington. Available at: \url{https://pdfs.semanticscholar.org/22bb/3ae1c37632eeee7b6e3b8d973fdaf534f9ab.pdf?_ga=2.242461442.1085768207.1593946556-1126142591.1590329375}
#'
#' @return \code{SpatialPolygonsDataFrame} (sp package). Area between the lcp and comparison SpatialLines* with a data.frame containing the Area, PDI, normalised PDI and the distance of the shortest path (i.e. Euclidean) between the origin and destination
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

PDI_validation <- function(lcp, comparison, snap = TRUE) {
    
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
    
    lcp_coords <- base::unname(lcp_coords)
    comparison_coords <- base::unname(comparison_coords)
    
    if (snap) {
        
        lcp_coords[1, ] <- comparison_coords[1, ]
        lcp_coords[nrow(lcp_coords), ] <- comparison_coords[nrow(comparison_coords), ]
        
    }
    
    if (base::identical(lcp_coords[1, ], comparison_coords[1, ])) {
        
        # if the origin location of the lcp is the same as the origin of the comparison, reverse order of comparison coordinates. This is ensure that the order
        # of coordinates is correct when creating the polygon between the two SpatialLines
        comparison_coords <- comparison_coords[nrow(comparison_coords):1, ]
        
    }
    
    
    start <- lcp_coords[1, ]
    end <- lcp_coords[base::nrow(lcp_coords), ]
    
    coords <- base::rbind(lcp_coords, comparison_coords)
    
    p = sp::Polygon(coords)
    ps = sp::Polygons(list(p), 1)
    sps = sp::SpatialPolygons(list(ps), proj4string = crs(lcp))
    
    if ((!suppressWarnings(rgeos::gIsValid(sps))) & ((gArea(spgeom = sps, byid = FALSE) > 0))) {
        
        # if the SpatialPolygon is invalid AND the Area is greater than zero (i.e. the two supplied SpatialLines are different, and so the resultant sps is a
        # SpatialPolygon not a pseudo-SpatialPolygon with an Area of zero), then the SpatialPolygons is corrected.
        
        sps <- rgeos::gPolygonize(rgeos::gNode(rgeos::gBoundary(sps)))
        
        sps <- rgeos::gUnaryUnion(spgeom = sps)
        
    }
    
    PDI_area <- rgeos::gArea(sps, byid = FALSE)
    
    max_distance <- raster::pointDistance(p1 = start, p2 = end, type = "Euclidean", lonlat = FALSE)
    
    PDI <- PDI_area/max_distance
    
    sps$area <- PDI_area
    sps$PDI <- PDI
    sps$max_distance <- max_distance
    
    norm_PDI <- (PDI_area/max_distance)/max_distance * 100
    
    sps$normalised_PDI <- norm_PDI
    
    return(sps)
    
}
