leastcostpath <- function(dem, origin, destination, cost_function = "all", direction = FALSE, neighbours = 16, crit_slope = 15, hf = "linear", linear_values = c(-2/180, 2), binary_values = c(45, 999, 1), forward_values = c(999, 1, 0.5)) {
    
    altDiff_slope <- function(x) {
        x[2] - x[1]
    }
    
    hd <- gdistance::transition(dem, altDiff_slope, 8)
    
    slope <- gdistance::geoCorrection(hd)
    
    if (cost_function == "all") {
        adj <- raster::adjacent(dem, cells = 1:ncell(dem), pairs = TRUE, directions = neighbours)
        slope_stack <- raster::stack(slope, slope, slope)
        
        slope_stack[[1]][adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
        slope_stack[[2]][adj] <- 4.8 * exp(-5.3 * abs(slope[adj] * 0.7) + 0.03)
        slope_stack[[3]][adj] <- 1/(1 + abs(slope[adj]/crit_slope)^2)
        
        Conductance <- stack(gdistance::geoCorrection(slope_stack[[1]]), gdistance::geoCorrection(slope_stack[[2]]), gdistance::geoCorrection(slope_stack[[3]]))
    }
    
    aspect <- raster::terrain(dem, opt = "aspect", unit = "degrees", neighbors = 8)
    
    altDiff_aspect <- function(x) {
        if (hf == "linear") {
            if (abs(x[2] - x[1]) > 180) {
                hrma <- abs(x[2] - x[1]) - 180
                (linear_values[1]) * hrma + linear_values[2]
            } else {
                hrma <- abs(x[2] - x[1])
                (linear_values[1]) * hrma + linear_values[2]
            }
        } else if (hf == "binary") {
            if (abs(x[2] - x[1]) > 180) {
                hrma <- abs(x[2] - x[1]) - 180
                ifelse(hrma > binary_values[1], binary_values[3], binary_values[2])
            } else {
                hrma <- abs(x[2] - x[1])
                ifelse(hrma > binary_values[1], binary_values[3], binary_values[2])
            }
        } else if (hf == "forward") {
            if (abs(x[2] - x[1]) > 180) {
                hrma <- abs(x[2] - x[1]) - 180
                if (hrma < 45) {
                  forward_values[1]
                } else if (hrma >= 45 & hrma < 90) {
                  forward_values[2]
                } else {
                  forward_values[3]
                }
            } else {
                hrma <- abs(x[2] - x[1])
                if (hrma < 45) {
                  forward_values[1]
                } else if (hrma >= 45 & hrma < 90) {
                  forward_values[2]
                } else {
                  forward_values[3]
                }
            }
        }
    }
    
    
    trans <- gdistance::transition(aspect, altDiff_aspect, 16)
    
    Conductance[[1]] <- Conductance[[1]] * trans
    Conductance[[2]] <- Conductance[[2]] * trans
    Conductance[[3]] <- Conductance[[3]] * trans
    
    plot(raster(Conductance[[1]]))
    plot(raster(Conductance[[2]]))
    plot(raster(Conductance[[3]]))
    
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
        
        names(sPath) <- c("Toblers A to B", "Marquez-Perez A to B", "Lloberas A to B")
        
    } else {
        
        sPath[[1]] <- gdistance::shortestPath(Conductance[[1]], origin, destination, output = "SpatialLines")
        
        sPath[[2]] <- gdistance::shortestPath(Conductance[[1]], destination, origin, output = "SpatialLines")
        
        sPath[[3]] <- gdistance::shortestPath(Conductance[[2]], origin, destination, output = "SpatialLines")
        
        sPath[[4]] <- gdistance::shortestPath(Conductance[[2]], destination, origin, output = "SpatialLines")
        
        sPath[[5]] <- gdistance::shortestPath(Conductance[[3]], origin, destination, output = "SpatialLines")
        
        names(sPath) <- c("Toblers A to B", "Toblers B to A", "Marquez-Perez A to B", "Marquez-Perez B to A", "Lloberas A to B")
        
    }
    
    
    lcp_lengths <- unlist(lapply(sPath, function(x) {
        FUN = rgeos::gLength(x, byid = TRUE)
    }))
    
    for (i in 1:length(sPath)) {
        sPath[[i]]$length <- lcp_lengths[i]
    }
    
    for (i in 1:length(sPath)) {
        rgdal::writeOGR(sPath[[i]], ".", paste0(names(sPath)[i], "_", hf), driver = "ESRI Shapefile", overwrite_layer = TRUE)
        
    }
    
}