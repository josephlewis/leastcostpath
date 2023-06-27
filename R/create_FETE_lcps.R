#' Calculate Least-cost Paths from each location to all other locations 
#' 
#' Calculates Least-cost paths from-everywhere-to-everywhere. This is based on the approach proposed by White and Barber (2012).
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
#' @return \code{sf} or \code{spatVector} Least-cost paths from-everywhere-to-everywhere based on the supplied \code{conductanceMatrix}. If supplied \code{locations} is a \code{spatVector} object then \code{spatVector} object returned else \code{sf} object 
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
#' sf::st_point(c(907695, 4145478)),
#' crs = terra::crs(r)))
#' 
#' lcps <- create_FETE_lcps(x = slope_cs, locations = locs)

create_FETE_lcps <- function(x, locations, cost_distance = FALSE, ncores = 1, truncate = FALSE, distance = 0) {
  
  check_locations(x, locations)
  
  loc_vect <- inherits(locations, "SpatVector")
  
  myCluster <- parallel::makeCluster(ncores)
  doParallel::registerDoParallel(myCluster)
  
  nlocs <- nrow(locations)
  
  if(loc_vect) { 
    locations <- sf::st_as_sf(locations)
  }
  
  if(truncate==T){
    distance = distance
    distmat=drop_units(st_distance(st_as_sf(locations)))
    distmat[lower.tri(distmat)] <- 0 
    comps=which(distmat<distance&distmat!=0,arr.ind=T)
    
    lcp_network <- foreach::foreach(i = 1:nrow(comps), .errorhandling = "remove", .combine = "rbind", .packages = c("sf", "terra")) %dopar% {
      
      lcp <- create_lcp(x = x,
                                       origin = locations[comps[i,1],, drop = FALSE],
                                       destination = locations[comps[i,2],, drop = FALSE],
                                       cost_distance = cost_distance)
      
      lcp$origin_ID <- comps[i,1]
      lcp$destination_ID <- comps[i,2]
      
      return(lcp)
    }
    parallel::stopCluster(myCluster)
    
    lcp_network <- lcp_network[!is.na(sf::st_is_valid(lcp_network)),]
    
    empty_lcps <- sf::st_is_empty(lcp_network)
    
    lcp_network <- lcp_network[!empty_lcps,]
    lcp_network <- lcp_network[order(lcp_network$origin_ID),]
    rownames(lcp_network) <- 1:nrow(lcp_network)
    
    if((nlocs*nlocs-nlocs) - nrow(lcp_network) != 0) { 
      message((nlocs*nlocs-nlocs) - nrow(lcp_network), " least-cost paths could not be calculated due to duplicate locations.")
    }
    
    if(sum(empty_lcps) != 0) { 
      message(sum(empty_lcps), " least-cost paths could not calculated due to being unreachable. If so, check via check_locations()")
    }
    
    if(loc_vect) { 
      lcp_network <- terra::vect(lcp_network)
    }
    
    return(lcp_network)
    
  }
  
  if(truncate==F){
    lcp_network <- foreach::foreach(i = 1:nlocs, .errorhandling = "remove", .combine = "rbind", .packages = c("sf", "terra")) %dopar% {
      
      lcp <- create_lcp(x = x,
                                       origin = locations[i,, drop = FALSE],
                                       destination = locations[-i,, drop = FALSE],
                                       cost_distance = cost_distance)
      
      lcp$origin_ID <- i
      lcp$destination_ID <- (1:nlocs)[-i]
      
      return(lcp)
    }
    
    
    parallel::stopCluster(myCluster)
    
    lcp_network <- lcp_network[!is.na(sf::st_is_valid(lcp_network)),]
    
    empty_lcps <- sf::st_is_empty(lcp_network)
    
    lcp_network <- lcp_network[!empty_lcps,]
    lcp_network <- lcp_network[order(lcp_network$origin_ID),]
    rownames(lcp_network) <- 1:nrow(lcp_network)
    
    if((nlocs*nlocs-nlocs) - nrow(lcp_network) != 0) { 
      message((nlocs*nlocs-nlocs) - nrow(lcp_network), " least-cost paths could not be calculated due to duplicate locations.")
    }
    
    if(sum(empty_lcps) != 0) { 
      message(sum(empty_lcps), " least-cost paths could not calculated due to being unreachable. If so, check via check_locations()")
    }
    
    if(loc_vect) { 
      lcp_network <- terra::vect(lcp_network)
    }
    
    return(lcp_network)
    
  }
}
