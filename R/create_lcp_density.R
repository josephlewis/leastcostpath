#' create_lcp_density
#'
#' Creates a cumulative least cost path raster
#'
#' Cumulatively combines Least Cost Paths in order to identify routes of preferential movement within the landscape.
#'
#' @param lcps \code{SpatialLines} or \code{SpatialLinesDataFrame}. Least Cost Paths
#'
#' @param raster \code{RasterLayer} (raster package). This is used to derive the resolution, extent, and spatial reference system to be used when calculating the cumulative least cost path raster
#'
#' @param rescale if TRUE raster values scaled to between 0 and 1. Default is TRUE
#'
#' @return RasterLayer object
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
#' @examples
#' r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
#'
#' slope_cs <- create_slope_cs(r, cost_function = 'tobler')
#'
#' traverse_cs <- create_traversal_cs(r, neighbours = 16)
#'
#' final_cost_cs <- slope_cs * traverse_cs
#'
#' locs <- sp::spsample(as(r, 'SpatialPolygons'),n=10,'regular')
#'
#' lcp_network <- create_FETE_lcps(cost_surface = final_cost_cs, locations = locs,
#' cost_distance = FALSE, parallel = FALSE)
#'
#' cumulative_lcps <- create_lcp_density(lcps = lcp_network, raster = r, rescale = FALSE)

create_lcp_density <- function(lcps, raster, rescale = TRUE) {
    
    if (!inherits(lcps, c("SpatialLines", "SpatialLinesDataFrame"))) {
        stop("lcps expects a SpatialLines or SpatialLinesDataFrame object")
    }
    
    if (!inherits(raster, "RasterLayer")) {
        stop("raster expects a RasterLayer object")
    }
    
    lcp_pts <- methods::as(lcps, "SpatialPoints")
    
    cumulative_pts <- raster::rasterize(x = lcp_pts, y = raster, fun = "count")
    
    if (rescale) {
        
        rasterRescale <- function(r) {
            ((r - raster::cellStats(r, "min"))/(raster::cellStats(r, "max") - raster::cellStats(r, "min")))
        }
        
        cumulative_pts <- rasterRescale(cumulative_pts)
    }
    
    return(cumulative_pts)
    
}
