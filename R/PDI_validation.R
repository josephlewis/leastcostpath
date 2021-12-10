#' Calculate Path Deviation Index
#'
#' Calculates the Path Deviation Index of a Least Cost Path and a comparison SpatialLines using the method proposed by Jan et al. (1999).
#'
#' @param lcp \code{SpatialLines*} (sp package). Least Cost Path to assess the accuracy of. Expects object of class SpatialLines. Only first feature used. 
#'
#' @param comparison \code{SpatialLines*} to validate the Least Cost Path against. Expects object of class SpatialLines. Only first feature used.
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
#' Note: Direction of lcp and comparison SpatialLines must be in the same order. Check First point (Origin) and Last point (Destination) for confirmation. 
#' 
#' @references
#'
#' Jan, O., Horowitz, A.J., Peng, Z.R. (2000). Using Global Positioning System Data to Understand Variations in Path Choice. Transportation Research Record, 1725, 37-44 \doi{10.3141/1725-06}
#'
#' @return \code{SpatialPolygonsDataFrame} or \code{SpatialLinesDataFrame} (sp package). SpatialPolygonsDataFrame of Area between the LCP and comparison SpatialLines if LCP and comparison SpatialLines are not identical, else returns SpatialLinesDataFrame. Data frame containing Area, PDI, distance of the Euclidean shortest path between the origin and destination and normalised PDI. 
#'
#' @author Joseph Lewis
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import gdistance
#' @import methods
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

PDI_validation <- function(lcp, comparison) {
    
    if (!inherits(lcp, "SpatialLines")) {
        stop("lcp argument is invalid. Expecting SpatialLines* object")
    }
    
    if (!inherits(comparison, "SpatialLines")) {
        stop("Comparison argument is invalid. Expecting a SpatialLines* object")
    }
    
    lcp_subset <- lcp[1, ]
    comparison_subset <- comparison[1, ]
    
    lcp_pts <- methods::as(lcp_subset, "SpatialPoints")
    comparison_pts <- methods::as(comparison_subset, "SpatialPoints")
    
    lcp_coords <- sp::coordinates(lcp_pts)
    comparison_coords <- sp::coordinates(comparison_pts)
    
    lcp_coords[1, ] <- comparison_coords[1, ]
    lcp_coords[nrow(lcp_coords), ] <- comparison_coords[nrow(comparison_coords), ]
    
    start <- lcp_coords[1, ]
    end <- lcp_coords[base::nrow(lcp_coords), ]
    
    coords <- base::rbind(lcp_coords, comparison_coords[nrow(comparison_coords):1, ])
    
    p = sp::Polygon(coords)
    ps = sp::Polygons(list(p), 1)
    sps = sp::SpatialPolygons(list(ps), proj4string = raster::crs(comparison))
    
    # check if SpatialPolygons (sps) is invalid
    if (!suppressWarnings(rgeos::gIsValid(sps))) {
        # if invalid and lcp_coords and comparison_coords of provided lcp and comparison SpatialLines equal then coerce to SpatialLines
        
        if (identical(lcp_coords, comparison_coords)) {
            
            sps <- as(sps, "SpatialLines")
            
            
            # if lcp and comparison SpatialLines not equal then correct SpatialPolygon
        } else {
            
            sps <- rgeos::gPolygonize(rgeos::gNode(rgeos::gBoundary(sps)))
            sps <- rgeos::gUnaryUnion(spgeom = sps)
            
        }
        
    }
    
    PDI_area <- rgeos::gArea(sps, byid = FALSE)
    
    max_distance <- raster::pointDistance(p1 = start, p2 = end, type = "Euclidean", lonlat = FALSE)
    
    PDI <- PDI_area/max_distance
    
    sps$area <- PDI_area
    sps$PDI <- PDI
    sps$max_distance <- max_distance
    
    norm_PDI <- (PDI/max_distance) * 100
    
    sps$normalised_PDI <- norm_PDI
    
    return(sps)
    
}
