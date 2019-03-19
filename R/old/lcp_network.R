lcp_network <- function(dem, nodes, graph = "delauney", cost_distance = FALSE, cost_function = "tobler",  neighbours = 16, traverse = "asymmetrical", crit_slope = 12, suffix = "" , export = FALSE) { 
  
  if(!inherits(dem, "RasterLayer") | !inherits(nodes, "SpatialPointsDataFrame") | !inherits(nodes, "SpatialPoints")) 
    stop("Invalid objects supplied. dem expects RasterLayer object and nodes expects SpatialPoints or SpatialPointsDataFrame object")
  
  altDiff_slope <- function(x) {
    x[2] - x[1]
  }
  
  hd <- gdistance::transition(dem, altDiff_slope, 8, symm = FALSE)
  
  slope <- gdistance::geoCorrection(hd)
  
  adj <- raster::adjacent(dem, cells = 1:raster::ncell(dem), pairs = TRUE, directions = neighbours)
  
  if (cost_function == "tobler") {
    slope[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
  } else if (cost_function == "modified_tobler") { 
    slope[adj] <- 4.8 * exp(-5.3 * abs((slope[adj] * 0.7) + 0.03))
  } else if (cost_function == "wheeled_transport") { 
    slope[adj] <- 1/(1 + abs(slope[adj] * 100/crit_slope)^2)  
    } else if (cost_function == "herzog") {
      slope[adj] <- 1/((((1337.8 * slope[adj]^6) + (278.19 * slope[adj]^5) - (517.39 * slope[adj]^4) - (78.199 * slope[adj]^3) + (93.419 * slope[adj]^2) +(19.825 * slope[adj]) + 1.64))) 
    } else {
      stop("Cost Function expecting 'tobler', 'modified_tobler', 'wheeled_transport', 'herzog'")
    }
  
  Conductance <- gdistance::geoCorrection(slope)

  if (traverse == "asymmetrical" | traverse == "symmetrical" | traverse == "none") {
    
    aspect_dem <- raster::terrain(dem, opt = "aspect", unit = "degrees", neighbors = 8)
    
    aspect_dem <- calc(aspect_dem, function(x) {
      ifelse(x >= 180, x - 180, x)
    })
    
    aspect_dem <- calc(aspect_dem, function(x) {
      ifelse(x >= 0 & x <= 90, x + 90, x - 90)
    })
    
    if (traverse == "asymmetrical") {
      altDiff_traverse <- function(x) {
        
        if (abs(x[2] - x[1]) == 0) {
          1
        } else if (x[2] > x[1]) {
          if (abs(x[2] - x[1]) > 0 & abs(x[2] - x[1]) <= 45) {
            hrma <- abs(x[2] - x[1])
            1 + (0.5/45) * hrma
          } else if (abs(x[2] - x[1]) > 45 & abs(x[2] - x[1]) <= 90) {
            hrma <- abs(x[2] - x[1])
            2 - (0.5/45) * hrma
          } else {
            1
          }
        } else if (x[2] < x[1]) {
          if (abs(x[2] - x[1]) > 0 & abs(x[2] - x[1]) <= 45) {
            hrma <- abs(x[2] - x[1])
            1 - (0.5/45) * hrma
          } else if (abs(x[2] - x[1]) > 45 & abs(x[2] - x[1]) <= 90) {
            hrma <- abs(x[2] - x[1])
            (0.5/45) * hrma
            
          } else {
            1
          }
        }
      }
      
      trans <- gdistance::transition(aspect_dem, altDiff_traverse, neighbours, symm = FALSE)
      
      trans <- gdistance::geoCorrection(trans)
      
      Conductance <- Conductance * trans

    } else if (traverse == "symmetrical") {
      
      altDiff_traverse <- function(x) {
        
        if (abs(x[2] - x[1]) == 0) {
          1
        } else if (x[2] > x[1]) {
          if (abs(x[2] - x[1]) > 0 & abs(x[2] - x[1]) <= 45) {
            hrma <- abs(x[2] - x[1])
            1 - (0.5/45) * hrma
          } else if (abs(x[2] - x[1]) > 45 & abs(x[2] - x[1]) <= 90) {
            hrma <- abs(x[2] - x[1])
            (0.5/45) * hrma
          } else {
            1
          }
        } else if (x[2] < x[1]) {
          if (abs(x[2] - x[1]) > 0 & abs(x[2] - x[1]) <= 45) {
            hrma <- abs(x[2] - x[1])
            1 - (0.5/45) * hrma
          } else if (abs(x[2] - x[1]) > 45 & abs(x[2] - x[1]) <= 90) {
            hrma <- abs(x[2] - x[1])
            (0.5/45) * hrma
            
          } else {
            1
          }
        }
      }
      
      trans <- gdistance::transition(aspect_dem, altDiff_traverse, neighbours, symm = FALSE)
      trans <- gdistance::geoCorrection(trans)
      
      Conductance <- Conductance * trans
}
  } else {
    stop("traverse expecting 'asymmertrical', 'symmetrical' or 'none'. See ?leastcostpath for more details.")
  }
  
  if (graph == "delauney") { 
    
    nodes <- sp::remove.duplicates(nodes)
    
    co <- coordinates(nodes)
    coords <- as.matrix(coordinates(nodes))
    ids <- row.names(as.data.frame(nodes))
    neighbours <- spdep::tri2nb(co, row.names=ids)
    network <- spdep::nb2lines(neighbours, coords=coords, proj4string = crs(nodes))
    network <- as(data.frame(network[1:2]), "matrix")

    lcp_netwk <- apply(network, MARGIN = 1, function (x) { gdistance::shortestPath(Conductance,nodes[x[1],], nodes[x[2],],output = "SpatialLines")})
    
    lcp_netwk <- do.call(rbind, lcp_netwk)

  } else if (graph == "none") { 
      
    network <- as(expand.grid(seq_along(nodes), seq_along(nodes)), "matrix")
    
    network <- network[network[,1] != network[,2],]
    
    lcp_netwk <- apply(network, MARGIN = 1, function (x) { gdistance::shortestPath(Conductance,nodes[x[1],], nodes[x[2],],output = "SpatialLines")})
    
  } else { 
    stop("graph expecting 'delauney' or 'none'.")
  }
  
  lcp_netwk <- SpatialLinesDataFrame(lcp_netwk, data.frame(1:length(lcp_netwk)), match.ID = FALSE)
  
  if (cost_distance) { 
    
    cost_dist <- apply(network, MARGIN = 1, function (x) { gdistance::costDistance(Conductance,nodes[x[1],], nodes[x[2],])})
    
    lcp_netwk <- SpatialLinesDataFrame(lcp_netwk, data.frame(cost_dist), match.ID = FALSE)
    
  }
  
  return(lcp_netwk)
  
}
