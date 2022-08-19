#' Calculate Least-cost Paths from each location to all other locations 
#' 
#' Calculates Least-cost paths from-everywhere-to-everywhere. This is based on the approach proposed by White and Barber (2012).
#' 
#' 
#' @param x \code{conductanceMatrix} 
#' 
#' @param locations \code{sf} of geometry type 'POINT' or 'MULTIPOINT'
#' 
#' @param cost_distance \code{logical} if TRUE computes total accumulated cost from origin to destination. FALSE (default)
#' 
#' @author Joseph Lewis
#' 
#' @return \code{sf}  Least-cost paths from-everywhere-to-everywhere based on the supplied \code{conductanceMatrix} 
#' 
#' @importFrom foreach %do%
#' 
#' @export

create_FETE_lcps <- function(x, locations, cost_distance = FALSE) {
  
  network <- expand.grid(1:nrow(locations), 1:nrow(locations))
  network <- network[network[,1] != network[,2],]
  
  lcp_network <- foreach::foreach(i = 1:nrow(network), .packages = c("leastcostpath"), .errorhandling = "remove", .combine = "rbind") %do% {
    lcp <- suppressWarnings(create_lcp(x = x,
                                       origin = locations[network[i,1],],
                                       destination = locations[network[i,2],],
                                       cost_distance = cost_distance))
    lcp$origin_ID <- network[i,1]
    lcp$destination_ID <- network[i,2]
    
    return(lcp)
  }

  empty_lcps <- sf::st_is_empty(lcp_network)

  lcp_network <- lcp_network[!empty_lcps,]
  lcp_network <- lcp_network[order(lcp_network$origin_ID),]

  if(sum(empty_lcps) > 0) {
    message(nrow(network) - nrow(lcp_network), " lcps could not able to be calculated. Ensure that all locations are reachable by using check_locations()")
  }

  return(lcp_network)
  
}