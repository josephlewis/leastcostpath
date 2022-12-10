#' Calculate Least-cost Paths from each location to all other locations 
#' 
#' Calculates Least-cost paths from-everywhere-to-everywhere. This is based on the approach proposed by White and Barber (2012).
#' 
#' 
#' @param x \code{conductanceMatrix} 
#' 
#' @param locations \code{sf} 'POINT' or 'MULTIPOINT', \code{SpatVector}, \code{data.frame} or \code{matrix} containing the locations coordinates
#' 
#' @param cost_distance \code{logical} if TRUE computes total accumulated cost from origin to destination. FALSE (default)
#' 
#' @param ncores \code{numeric} Number of cores used when calculating least-cost paths from-everywhere-to-everywhere. 1 (default)
#' 
#' @author Joseph Lewis
#' 
#' @return \code{sf}  Least-cost paths from-everywhere-to-everywhere based on the supplied \code{conductanceMatrix} 
#' 
#' @importFrom foreach %dopar%
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
#' sf::st_point(c(907695, 4145478)),
#' sf::st_point(c(1054446, 4232288)),
#' sf::st_point(c(957989, 4208863)),
#' crs = terra::crs(r)))
#' 
#' lcps <- create_FETE_lcps(x = slope_cs, locations = locs, cost_distance = TRUE)

create_FETE_lcps <- function(x, locations, cost_distance = FALSE, ncores = 1) {
  
  myCluster <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(myCluster)
  
  network <- expand.grid(1:nrow(locations), 1:nrow(locations))
  network <- network[network[,1] != network[,2],]
  
  lcp_network <- foreach::foreach(i = 1:nrow(network), .errorhandling = "remove", .combine = "rbind", .packages = c("sf", "terra")) %dopar% {
    lcp <- create_lcp(x = x,
                      origin = locations[network[i,1],],
                      destination = locations[network[i,2],],
                      cost_distance = cost_distance)

    lcp$origin_ID <- network[i,1]
    lcp$destination_ID <- network[i,2]
    
    return(lcp)
  }
  
  parallel::stopCluster(myCluster)

  empty_lcps <- sf::st_is_empty(lcp_network)

  lcp_network <- lcp_network[!empty_lcps,]
  lcp_network <- lcp_network[order(lcp_network$origin_ID),]

  if(sum(empty_lcps) > 0) {
    message(nrow(network) - nrow(lcp_network), " lcps could not able to be calculated. Ensure that all locations are reachable by using check_locations()")
  }

  return(lcp_network)
  
}
