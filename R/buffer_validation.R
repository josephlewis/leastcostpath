#' Calculate the similarity of a least-cost path to a known route
#' 
#' Calculates the similarity of a least-cost path to a known route using the buffer method proposed by Goodchild and Hunter (1997)
#'  
#' @param lcp \code{sf} or \code{spatVector}
#' 
#' @param comparison \code{sf} or \code{spatVector}
#' 
#' @param dist \code{numeric} buffer distances to assess similarity
#' 
#' @return \code{data.frame}
#' 
#' @importFrom foreach %do%
#' 
#' @author Joseph Lewis
#' 
#' @export
#' 
#' @examples 
#' 
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' 
#' slope_cs <- create_slope_cs(x = r, cost_function = "tobler", neighbours = 4)
#' 
#' locs <- sf::st_sf(geometry = sf::st_sfc(
#' sf::st_point(c(839769, 4199443)),
#' sf::st_point(c(1038608, 4100024)),
#' crs = terra::crs(r)))
#' 
#' lcp1 <- create_lcp(x = slope_cs, origin = locs[1,], destination = locs[2,])
#' 
#' lcp2 <- create_lcp(x = slope_cs, origin = locs[2,], destination = locs[1,])
#' 
#' buffer_validation(lcp = lcp1, comparison = lcp2, dist = c(1000, 2500, 5000, 10000))

buffer_validation <- function(lcp, comparison, dist) { 
  
  if(inherits(lcp, "spatVector")) {
    lcp <- sf::st_as_sf(lcp)
  }
  
  if(inherits(comparison, "spatVector")) {
    comparison <- sf::st_as_sf(comparison)
  }
  
  val_table <- foreach::foreach(dist_no = 1:length(dist), .combine = "rbind") %do% { 
    
    compar_buffer <- sf::st_buffer(x = comparison, dist = dist[dist_no])
    lcp_intersect <-  suppressWarnings(sf::st_intersection(x = compar_buffer, y = lcp))
    similarity <- (sf::st_length(lcp_intersect) / sf::st_length(lcp)) * 100
    
    similarity_df <- data.frame(ID = dist_no, dist = dist[dist_no], similarity = as.numeric(similarity))
    
    return(similarity_df)
  }
  
  return(val_table)
}
