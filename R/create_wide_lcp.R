#' Calculate wide least cost path
#'
#' Calculates a wide least cost path from an origin location to a destination location. Applies Dijkstra's algorithm. See details for more information
#'
#' @param cost_surface \code{TransitionLayer} (gdistance package). Cost surface to be used in Least Cost Path calculation
#'
#' @param origin \code{SpatialPoints*} (sp package) location from which the Least Cost Path is calculated. Only the first row is taken into account
#'
#' @param destination \code{SpatialPoints*} (sp package) location to which the Least Cost Path is calculated. Only the first row is taken into account
#'
#' @param neighbours \code{numeric} value. Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected numeric values are 4, 8, 16, 32, 48 or a matrix object. Default is numeric value 16
#'
#' @param path_ncells \code{numeric} value. Dimension of wide path matrix. Note that the value refers to the number of cells and not distance. See \code{\link{wide_path_matrix}} for example
#'
#' @details
#'
#' The calculation of a wide least cost path is inspired by Shirabe (2015).Instead of calculating a least cost path where the path width is assumed to be zero or negligible compared to the cell size, create_wide_lcp creates a wide least cost path where the path is calculated based on a cost surface that incorporates the total permeability of passage from adjacent cells
#'
#' The algorithm proceeds as follows:
#'
#' Each column of the supplied cost surface is summed, resulting in a raster with each cell representing the total permeability of passage from each adjacent neighbour (adjacent cells specificed when creating cost surface through the use of wide_path_matirx(). A transitionMatrix is created from this total permeability of passage raster, with the permeability of movement between cells based on the total permeability raster. That is, moving into each cell regardless of direction will incur the same cost.
#'
#' Using this total permeability of passage cost surface, the least cost path can be calculated. This represents the least cost path between two locations based on the total permeability of passage cost surface that incorporates the summed permeability of passage. To visualise the wide least cost path, the least cost path is represented as a polygon with the width as supplied in the path_ncells argument.
#'
#' @references Dijkstra, E. W. (1959). A note on two problems in connexion with graphs. Numerische Mathematik. 1: 269-271.
#'
#' Shirabe, T. (2015). A method for finding a least-cost wide path in raster space. International Journal of Geographical Information Science 30, 1469-1485. \doi{10.1080/13658816.2015.1124435}
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
#' slope_cs <- create_slope_cs(r, cost_function = 'tobler', neighbours = wide_path_matrix(n))
#'
#' loc1 = cbind(2667670, 6479000)
#' loc1 = sp::SpatialPoints(loc1)
#'
#' loc2 = cbind(2667800, 6479400)
#' loc2 = sp::SpatialPoints(loc2)
#'
#' lcps <- create_wide_lcp(cost_surface = slope_cs, origin = loc1,
#' destination = loc2, path_ncells = n)

create_wide_lcp <- function(cost_surface, origin, destination, neighbours = 16, path_ncells) {
    
    if (inherits(neighbours, "numeric")) {
        if (neighbours == 32) {
            neighbours <- neighbours_32
            
        } else if (neighbours == 48) {
            neighbours <- neighbours_48
        }
        
    }
    
    window <- wide_path_matrix(ncells = path_ncells)
    
    accumulated_wide_path <- raster(cost_surface, "colSums")
    
    tf <- function(x) {
        x[1]
    }
    
    wide_Conductance <- gdistance::transition(x = accumulated_wide_path, transitionFunction = tf, neighbours)
    
    wide_Conductance <- gdistance::geoCorrection(wide_Conductance, scl = FALSE)
    
    sPath <- gdistance::shortestPath(wide_Conductance, origin, destination, output = "TransitionLayer")
    
    sPath_raster <- raster(sPath)
    
    index <- raster::Which(x = sPath_raster, cells = TRUE)
    
    sPath_adj <- adjacent(sPath, cells = index, directions = window, sorted = TRUE)
    
    sPath_raster[sPath_adj[, 2]] <- 1
    
    sPath_polygon <- raster::rasterToPolygons(x = sPath_raster, dissolve = TRUE, n = 16, fun = function(x) {
        x == 1
    })
    
    return(sPath_polygon)
    
}
