#' create_cumulative_lcps
#'
#' Creates a cumulative cost path raster using previously created LCP network
#'
#'The create_cumulative_lcps function cumulatively combines LCPs to show LCP density and thus routes of preferential movement within the landscape.
#'
#' @param lcps Least Cost Paths. Expects Object of class SpatialLines or SpatialLinesDataFrame
#'
#' @param raster Expects Object of class RasterLayer. This is used to derive extent and spatial reference system of the cumulative cost path raster.
#'
#' @param rescale Rescales cost corridor raster to values between 0 and 1. Default is TRUE
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
#' lcp_network <- create_lcp_network(cost_surface = final_cost_cs, locations = locs,
#' cost_distance = FALSE, parallel = FALSE)
#'
#' cumulative_lcps <- create_cumulative_lcps(lcps = lcp_network, raster = r, rescale = FALSE)

create_cumulative_lcps <- function(lcps, raster, rescale = TRUE) {
    
    if (!inherits(lcps, c("SpatialLines", "SpatialLinesDataFrame"))) {
        stop("lcps expects a SpatialLines or SpatialLinesDataFrame object")
    }
    
    if (!inherits(raster, "RasterLayer")) {
        stop("raster expects a RasterLayer object")
    }
    
    lcp_pts <- as(lcps, "SpatialPoints")
    
    cumulative_pts <- raster::rasterize(x = lcp_pts, y = raster, fun = "count")
    
    if (rescale) {
        
        rasterRescale <- function(r) {
            ((r - cellStats(r, "min"))/(cellStats(r, "max") - cellStats(r, "min")))
        }
        
        cumulative_pts <- rasterRescale(cumulative_pts)
    }
    
    return(cumulative_pts)
    
}
