create_FETE_lcps <- function(x, locations, cost_distance = FALSE, ncores = 1) { 
  
  # network <- expand.grid(1:nrow(locations), 1:nrow(locations))
  # network <- network[network[,1] != network[,2],]
  # 
  # cl <- snow::makeCluster(ncores, type = "SOCK")
  # doSNOW::registerDoSNOW(cl)
  #   
  # #cl <- parallel::makeCluster(ncores)
  # # parallel::clusterExport(cl, varlist = c("x", "locations", "cost_distance"), envir = environment())
  # 
  # # lcp_network <- pbapply::pbapply(network, MARGIN = 1, function(x) {
  # #   
  # #   lcp <- leastcostpath::create_lcp(x = x, origin = locations[x[1],], destination = locations[x[2],], cost_distance = cost_distance)
  # #   lcp$origin_ID <- x[1]
  # #   lcp$destination_ID <- x[2]
  # #   
  # #   return(lcp)
  # #   
  # #   }, cl = cl)
  # # 
  # # parallel::stopCluster(cl)
  # 
  # lcp_network <- foreach::foreach(i = 1:nrow(network), .packages = "leastcostpath", .combine=rbind, .errorhandling = "pass") %dopar% { 
  #   lcp <- suppressWarnings(create_lcp(x = x, origin = locations[network[i,1],], 
  #                                      destination = locations[network[i,2],], 
  #                                      cost_distance = cost_distance))
  #   lcp$origin_ID <- network[i,1]
  #   lcp$destination_ID <- network[i,2]
  #   
  #   return(lcp)
  # }
  # 
  # snow::stopCluster(cl)
  # 
  # empty_lcps <- sf::st_is_empty(lcp_network)
  # 
  # lcp_network <- lcp_network[!empty_lcps,]
  # lcp_network <- lcp_network[order(lcp_network$origin_ID),]
  # 
  # if(sum(empty_lcps) > 0) { 
  #   message(nrow(network) - nrow(lcp_network), " lcps could not able to be calculated. Ensure that all locations are reachable by using check_locations()")
  # }
  # 
  # return(lcp_network)
  
}