leastcostpath <- function(dem, origin, destination, cost_function = "all", direction = FALSE, neighbours = 16, crit_slope = 15, aspect = "asymmetrical") {
    
    altDiff_slope <- function(x) {
        x[2] - x[1]
    }
    
    hd <- gdistance::transition(dem, altDiff_slope, 8, symm = FALSE)
    
    slope <- gdistance::geoCorrection(hd)
    
    if (cost_function == "all") {
        adj <- raster::adjacent(dem, cells = 1:raster::ncell(dem), pairs = TRUE, directions = neighbours)
        slope_stack <- raster::stack(slope, slope, slope, slope)
        
        slope_stack[[1]][adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
        slope_stack[[2]][adj] <- 4.8 * exp(-5.3 * abs(slope[adj] * 0.7) + 0.03)
        slope_stack[[3]][adj] <- 1/(1 + abs(slope[adj]/crit_slope)^2)
        slope_stack[[4]][adj] <- 1/((((1337.8 * slope[adj]^6) + (278.19 * slope[adj]^5) - (517.39 * slope[adj]^4) - (78.199 * slope[adj]^3) + (93.419 * 
            slope[adj]^2) + (19.825 * slope[adj]) + 1.64)))
        
        Conductance <- raster::stack(gdistance::geoCorrection(slope_stack[[1]]), gdistance::geoCorrection(slope_stack[[2]]), gdistance::geoCorrection(slope_stack[[3]]), 
            gdistance::geoCorrection(slope_stack[[4]]))
    }
    
    if (aspect == "asymmetrical" | aspect == "symmetrical" | aspect == "none") {
        
        aspect_dem <- raster::terrain(dem, opt = "aspect", unit = "degrees", neighbors = 8)
        
        aspect_dem <- calc(aspect_dem, function(x) {
            ifelse(x >= 180, x - 180, x)
        })
        
        aspect_dem <- calc(aspect_dem, function(x) {
            ifelse(x >= 0 & x <= 90, x + 90, x - 90)
        })
        
        if (aspect == "asymmetrical") {
            altDiff_aspect <- function(x) {
                
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
            
            trans <- gdistance::transition(aspect_dem, altDiff_aspect, neighbours, symm = FALSE)
            
            Conductance[[1]] <- Conductance[[1]] * trans
            Conductance[[2]] <- Conductance[[2]] * trans
            Conductance[[3]] <- Conductance[[3]] * trans
            Conductance[[4]] <- Conductance[[4]] * trans
            
        } else if (aspect == "symmetrical") {
            altDiff_aspect <- function(x) {
                
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
            
            trans <- gdistance::transition(aspect_dem, altDiff_aspect, neighbours, symm = FALSE)
            
            Conductance[[1]] <- Conductance[[1]] * trans
            Conductance[[2]] <- Conductance[[2]] * trans
            Conductance[[3]] <- Conductance[[3]] * trans
            Conductance[[4]] <- Conductance[[4]] * trans
            
        } else if (aspect == "none") {
            NULL
        }
    } else {
        print("Aspect only accepts asymmetrical (default), symmetrical or none")
    }
    
    if (inherits(origin, "SpatialPoints") & inherits(destination, "SpatialPoints")) {
        sPath <- list()
        origin <- sp::coordinates(origin)
        destination <- sp::coordinates(destination)
    } else {
        return("Origin or Destination is not a SpatialPoints")
    }
    
    if (direction == "TRUE") {
        
        sPath[[1]] <- gdistance::shortestPath(Conductance[[1]], origin, destination, output = "SpatialLines")
        
        sPath[[2]] <- gdistance::shortestPath(Conductance[[2]], origin, destination, output = "SpatialLines")
        
        sPath[[3]] <- gdistance::shortestPath(Conductance[[3]], origin, destination, output = "SpatialLines")
        
        sPath[[4]] <- gdistance::shortestPath(Conductance[[4]], origin, destination, output = "SpatialLines")
        
        names(sPath) <- c("Toblers A to B", "Marquez-Perez A to B", "Lloberas A to B", "Herzog A to B")
        
    } else {
        
        sPath[[1]] <- gdistance::shortestPath(Conductance[[1]], origin, destination, output = "SpatialLines")
        
        sPath[[2]] <- gdistance::shortestPath(Conductance[[1]], destination, origin, output = "SpatialLines")
        
        sPath[[3]] <- gdistance::shortestPath(Conductance[[2]], origin, destination, output = "SpatialLines")
        
        sPath[[4]] <- gdistance::shortestPath(Conductance[[2]], destination, origin, output = "SpatialLines")
        
        sPath[[5]] <- gdistance::shortestPath(Conductance[[3]], origin, destination, output = "SpatialLines")
        
        sPath[[6]] <- gdistance::shortestPath(Conductance[[4]], origin, destination, output = "SpatialLines")
        
        names(sPath) <- c("Toblers A to B", "Toblers B to A", "Marquez-Perez A to B", "Marquez-Perez B to A", "Lloberas A to B", "Herzog A to B")
        
    }
    
    lcp_lengths <- unlist(lapply(sPath, function(x) {
        FUN = rgeos::gLength(x, byid = TRUE)
    }))
    
    for (i in 1:length(sPath)) {
        sPath[[i]]$length <- lcp_lengths[i]
    }
    
    for (i in 1:length(sPath)) {
        rgdal::writeOGR(sPath[[i]], ".", paste0(names(sPath)[i]), driver = "ESRI Shapefile", overwrite_layer = TRUE)
        
    }
}
