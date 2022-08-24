#' Calculate the similarity of a least-cost path to a known route
#' 
#' Calculates the similarity of a least-cost path to a known route using the path deviation index method proposed by Jan et al. (1999)
#'  
#' @param lcp \code{sf} 
#' 
#' @param comparison \code{sf}
#' 
#' @details
#'  
#' The Path Deviation Index (pdi) measures the spatial separation between a pair of paths and aims to overcome the shortcomings of measuring the percentage of coverage of a least cost path from a comparison path (e.g. as implemented in the buffer_validation function).
#'
#' The pdi index is defined as the area between paths divided by the Euclidean distance of the shortest path between the origin and destination of the paths. The index can be interpreted as the average distance between the paths.
#'
#' \code{pdi  = area / length}
#'
#' The value of the pdi depends on the length of the path and makes comparison of pdis difficult for paths with different origins and destinations. This is overcome by normalising the pdi by the Euclidean distance of the shortest path between the origin and destination of the paths
#' 
#' \code{Normalised pdi = pdi / length * 100}
#'
#' The normalised pdi is the percent of spatial separation between the two paths over the shortest path. For example, if a normalised pdi is 30 percent, it means that the average distance between two paths is 30 percent of the length of the shortest path. With normalised pdi, the spatial separations of all paths  can be compared regardless of the length of the shortest path.
#' 
#' Note: If the lcp path has a different origin and destination than the comparison path, the origin and destination of the lcp path are replaced with the origin and destination of the comparison path. This to ensure that a polygon can be created between the two paths which is required for calculating the area of spatial separation.
#' 
#' @return \code{sf} POLYGON of the area between the lcp and comparison with data.frame of area, pdi, max distance, and normalised pdi
#' 
#' @author Joseph Lewis
#' 
#' @export
#' 

PDI_validation <- function(lcp, comparison) {
 
  lcps <- list(lcp, sf::st_reverse(lcp))
  
  diff_polygons <- lapply(X = lcps, FUN = function(x) {
    
    x$geometry[[1]][1,] <- comparison$geometry[[1]][1,]
    x$geometry[[1]][nrow(x$geometry[[1]]),] <- comparison$geometry[[1]][nrow(comparison$geometry[[1]]),]
    
    diff_polygon <- sf::st_polygon(list(rbind(sf::st_coordinates(sf::st_reverse(x)), sf::st_coordinates(comparison))))
    diff_polygon <- sf::st_sfc(diff_polygon, crs = sf::st_crs(comparison))
    
    diff_polygon <- sf::st_zm(diff_polygon)

    pdi_area <- as.vector(sf::st_area(diff_polygon))
    max_distance <- sf::st_distance(sf::st_point(comparison$geometry[[1]][1,]), sf::st_point(comparison$geometry[[1]][nrow(comparison$geometry[[1]]),]), which = "Euclidean")
    pdi <- pdi_area/max_distance
    normalised_pdi <- (pdi/max_distance) * 100
    
    diff_polygon <- sf::st_sf(area = pdi_area, pdi = pdi, max_distance = max_distance, normalised_pdi = normalised_pdi, geometry = diff_polygon)
    
    return(diff_polygon)
    
})
  
  min_diff_polygon <- diff_polygons[[which.min(sapply(diff_polygons, function(x) min(x$area, na.rm=TRUE)))]]
  
  return(min_diff_polygon)
  
}