#' Calculate wide least cost path
#'
#' Calculates a wide least cost path from an origin location to a destination location. Applies Dijkstra's algorithm. See details for more information
#'
#' @param dem \code{RasterLayer} (raster package). Digital Elevation Model
#'
#' @param origin \code{SpatialPoints*} (sp package) location from which the Least Cost Path is calculated. Only the first row is taken into account
#'
#' @param destination \code{SpatialPoints*} (sp package) location to which the Least Cost Path is calculated. Only the first row is taken into account
#'
#' @param cost_function \code{character}. Cost Function used in the Least Cost Path calculation. Implemented cost functions include 'tobler', 'tobler offpath', 'irmischer-clarke male', 'irmischer-clarke offpath male', 'irmischer-clarke female', 'irmischer-clarke offpath female', 'modified tobler', 'wheeled transport', 'herzog', 'llobera-sluckin'. Default is 'tobler'. See Details for more information
#'
#' @param neighbours \code{numeric} value. Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected values are 4, 8, 16, 32, or 48. Default is 16
#'
#' @param crit_slope \code{numeric} value. Critical Slope (in percentage) is 'the transition where switchbacks become more effective than direct uphill or downhill paths'. Cost of climbing the critical slope is twice as high as those for moving on flat terrain and is used for estimating the cost of using wheeled vehicles. Default value is 12, which is the postulated maximum gradient traversable by ancient transport (Verhagen and Jeneson, 2012). Critical slope only used in 'wheeled transport' cost function
#'
#' @param max_slope \code{numeric} value. Maximum percentage slope that is traversable. Slope values that are greater than the specified max_slope are given a conductivity value of 0. Default is NULL
#'
#' @param ncells \code{numeric} value. Dimension of wide path matrix. Note that the value refers to the number of cells and not distance. See \code{\link{wide_path_matrix}} for example
#'
#' @param cost_distance \code{logical}. if TRUE computes total accumulated cost for each Least Cost Path. Default is FALSE
#'
#' @details
#'
#' The calculation of a wide least cost path is inspired by Shirabe (2015).Instead of calculating a least cost path where the path width is assumed to be zero or negligible compared to the cell size, create_wide_lcp creates a wide least cost path where the path has a specified cell width.
#'
#' The algorithm proceeds as follows:
#'
#' An odd-dimension matrix approximating the shape of an ocotogan is created based on the numeric value provided in the ncells argument. The resultant matrix represents the wide path and identifies which neighbours are adjacent from each cell in the provided raster. A transitionMatrix is created from the raster and calculates the permeability of passage from each cell to all of its adjacent neighbours. This information is stored within each column of the TransitionMatrix, with each column representing each cell of the raster.
#'
#' Each column of the transitionMatrix is summed, resulting in a numeric vector equal to the original number of cells in the provided raster. Each cell in the raster now represents the total permeability of passage from each adjacent neighbour based on the odd-dimension matrix. A transitionMatrix is created from this total permeability of passage raster, with the permeability calculated taking the mean between each cell to its adjacent cells.
#'
#' Using this total permeability of passage transitionMatrix, the least cost path can be calculated. This represents the least cost path between two locations based on the total permeability of passage transitionMatrix that incorporates the summed permeability of passage based on the odd-dimension matrix. To visualise this, the least cost path line is instead represented as a polygon with the width based on the odd-dimension matrix. As the summed permeability of passage transitionMatrix is calculated by taking the mean between each cell to its adjacent cells
#'
#' @references Dijkstra, E. W. (1959). A note on two problems in connexion with graphs. Numerische Mathematik. 1: 269-271.
#'
#' Shirabe, T. (2015). A method for finding a least-cost wide path in raster space. International Journal of Geographical Information Science 30, 1469-1485. \url{https://doi.org/10.1080/13658816.2015.1124435}
#'
#' @author Joseph Lewis
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import gdistance
#'
#' @export
#'
#' @return \code{SpatialPolygons} (sp package). The resultant object is the shortest wide path route (i.e. least cost) between origin and destination
#'
#'@examples
#' r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
#'
#' n <- 3
#'
#' loc1 = cbind(2667670, 6479000)
#' loc1 = sp::SpatialPoints(loc1)
#'
#' loc2 = cbind(2667800, 6479400)
#' loc2 = sp::SpatialPoints(loc2)
#'
#' lcp <- create_wide_lcp(dem = r, origin = loc1, destination = loc2, ncells = n)

create_wide_lcp <- function(dem, origin, destination, cost_function = "tobler", neighbours = 16, crit_slope = 12, max_slope = NULL, ncells, cost_distance = FALSE) {
    
    if (!inherits(dem, "RasterLayer")) {
        stop("dem argument is invalid. Expecting a RasterLayer object")
    }
    
    if (!inherits(origin, c("SpatialPoints", "SpatialPointsDataFrame"))) {
        stop("origin argument is invalid. Expecting a SpatialPoints* object")
    }
    
    if (!inherits(destination, c("SpatialPoints", "SpatialPointsDataFrame"))) {
        stop("destination argument is invalid. Expecting a SpatialPoints* object")
    }
    
    if (!neighbours %in% c(4, 8, 16, 32, 48)) {
        stop("neighbours argument is invalid. Expecting 4, 8, 16, 32, or 48.")
    }
    
    if (neighbours == 32) {
        
        neighbours <- neighbours_32
        
    } else if (neighbours == 48) {
        
        neighbours <- neighbours_48
    }
    
    window <- wide_path_matrix(ncells = ncells)
    
    slope <- calculate_slope(dem = dem, neighbours = window)
    
    adj <- raster::adjacent(dem, cells = 1:raster::ncell(dem), pairs = TRUE, directions = window)
    
    if (inherits(max_slope, "numeric")) {
        
        if (max_slope < 0) {
            stop("max_slope argument is invalid. Expecting numeric value above 0")
        }
        
        rand <- stats::runif(1, min = 0.01, max = 1)
        
        max_slope <- max_slope/100
        
        slope[adj] <- ifelse(slope[adj] >= max_slope, rand, slope[adj])
        
        slope[adj] <- ifelse(slope[adj] <= -max_slope, rand, slope[adj])
        
        index <- (which(slope[adj] == rand))
        
    }
    
    cf <- cost(cost_function = cost_function, adj = adj, crit_slope = crit_slope)
    
    slope[adj] <- cf(slope)
    
    Conductance <- gdistance::geoCorrection(slope, scl = FALSE)
    
    if (inherits(max_slope, "numeric")) {
        
        Conductance[adj][index] <- 0
        
    }
    
    accumulated_wide_path <- raster(Conductance, "colSums")
    
    wide_Conductance <- gdistance::transition(accumulated_wide_path, mean, neighbours)
    
    wide_Conductance <- gdistance::geoCorrection(wide_Conductance, scl = FALSE)
    
    sPath <- gdistance::shortestPath(wide_Conductance, origin, destination, output = "TransitionLayer")
    
    sPath_raster <- raster(sPath)
    
    index <- raster::Which(x = sPath_raster, cells = TRUE)
    
    sPath_adj <- adjacent(sPath, cells = index, directions = window, sorted = TRUE)
    
    sPath_raster[sPath_adj[, 2]] <- 1
    
    sPath_polygon <- raster::rasterToPolygons(x = sPath_raster, dissolve = TRUE, n = 16, fun = function(x) {
        x == 1
    })
    
    if (cost_distance) {
        
        sPath_polygon$cost <- as.vector(gdistance::costDistance(wide_Conductance, origin, destination))
        
    }
    
    return(sPath_polygon)
    
}
