leastcostpath <- function(dem, origin, destination, cost_function = "all", neighbours = 16, crit_slope = 15) {

    altDiff <- function(x) {
        x[2] - x[1]
    }

    hd <- gdistance::transition(dem, altDiff, 8, symm = FALSE)

    slope <- gdistance::geoCorrection(hd)

    if (cost_function == "all") {
        adj <- raster::adjacent(dem, cells = 1:ncell(dem), pairs = TRUE, directions = neighbours)
        slope_stack <- raster::stack(slope, slope, slope)

        slope_stack[[1]][adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
        slope_stack[[2]][adj] <- 4.8 * exp(-5.3 * abs(slope[adj] * 0.7) + 0.03)
        slope_stack[[3]][adj] <- 1/(1 + abs(slope[adj]/crit_slope)^2)

        Conductance <- stack(gdistance::geoCorrection(slope_stack[[1]]), gdistance::geoCorrection(slope_stack[[2]]), gdistance::geoCorrection(slope_stack[[3]]))
    }

    sPath <- list()

    if (inherits(origin, "SpatialPoints") & inherits(destination, "SpatialPoints")) {
        origin <- sp::coordinates(origin)
        destination <- sp::coordinates(destination)
    } else {
        return("Origin or Destination is not a SpatialPoints")
    }

    sPath[[1]] <- gdistance::shortestPath(Conductance[[1]], origin, destination, output = "SpatialLines")

    sPath[[2]] <- gdistance::shortestPath(Conductance[[2]], origin, destination, output = "SpatialLines")

    sPath[[3]] <- gdistance::shortestPath(Conductance[[3]], origin, destination, output = "SpatialLines")


    names(sPath) <- c("Toblers Hiking Function", "Marquezs Hiking Function", "Lloberas Hiking Function")

    lcp_lengths <- unlist(lapply(sPath, function(x) {
        FUN = rgeos::gLength(x, byid = TRUE)
    }))

    for (i in 1:length(sPath)) {
        sPath[[i]]$length <- lcp_lengths[i]
    }

    for (i in 1:length(sPath)) {
        rgdal::writeOGR(sPath[[i]], ".", paste0(names(sPath)[i], "_", format(Sys.time(), "%d-%b-%Y %H.%M")), driver = "ESRI Shapefile", overwrite_layer = TRUE)
    }

}
