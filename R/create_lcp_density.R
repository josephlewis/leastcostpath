#' Creates a cumulative Least Cost Path Raster
#'
#' Cumulatively combines Least Cost Paths in order to identify routes of preferential movement within the landscape.
#'
#' @param lcps \code{SpatialLines*} (sp package). Least Cost Paths
#'
#' @param raster \code{RasterLayer} (raster package). This is used to derive the resolution, extent, and spatial reference system to be used when calculating the cumulative least cost path raster
#'
#' @param rescale \code{logical}. if TRUE, raster values scaled to between 0 and 1. Default is FALSE
#'
#' @param rasterize_as_points \code{logical}. if TRUE (default) then the coordinates of the Least Cost Path vertices are rasterised. If FALSE Least Cost Paths are represented as lines and rasterised. As the Least Cost Path SpatialLines are converted from vector to raster, the Least Cost Paths represented as lines may result in the width of the rasterized line being greater than one cell, particularly at places of diagonal movement. Conversely, the Least Cost Paths represented as points (default) will result in some raster cells not being counted in the resultant RasterLayer. A greater number of cells not counted is expected when the number of neighbours used when creating the cost surface increases. NOTE: rasterisation of Lines takes much longer than rasterizing points.
#'
#' @return \code{RasterLayer} (raster package). The resultant object is the cumulatively combined Least Cost Paths. This identifies routes of preferential movement within the landscape.
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
#'
#'r <- raster::raster(nrow=50, ncol=50,  xmn=0, xmx=50, ymn=0, ymx=50, crs='+proj=utm')
#'
#'r[] <- stats::runif(1:length(r))
#'
#'slope_cs <- create_slope_cs(r, cost_function = 'tobler')
#'
#'x1 <- c(seq(1,10), seq(11,25), seq(26,30))
#'y1 <- c(seq(1,10), seq(11,25), seq(26,30))
#'line1 <- sp::SpatialLines(list(sp::Lines(sp::Line(cbind(x1,y1)), ID='a')))
#'
#'x2 <- c(seq(1,10), seq(11,25), seq(26, 30))
#'y2 <- c(seq(1,10), seq(11,25), rep(25, 5))
#'line2 <- sp::SpatialLines(list(sp::Lines(sp::Line(cbind(x2,y2)), ID='b')))
#'
#'lcp_network <- rbind(line1, line2)
#'
#'cumulative_lcps <- create_lcp_density(lcps = lcp_network, raster = r, rescale = FALSE)

create_lcp_density <- function(lcps, raster, rescale = FALSE, rasterize_as_points = TRUE) {
    
    if (!inherits(lcps, c("SpatialLines", "SpatialLinesDataFrame"))) {
        stop("lcps expects a SpatialLines* object")
    }
    
    if (!inherits(raster, "RasterLayer")) {
        stop("raster expects a RasterLayer object")
    }
    
    if (rasterize_as_points) {
        
        lcps <- methods::as(lcps, "SpatialPoints")
        
    }
    
    cumulative_pts <- raster::rasterize(x = lcps, y = raster, fun = "count")
    
    cumulative_pts[is.na(cumulative_pts)] <- 0
    
    if (rescale) {
        
        rasterRescale <- function(r) {
            ((r - raster::cellStats(r, "min"))/(raster::cellStats(r, "max") - raster::cellStats(r, "min")))
        }
        
        cumulative_pts <- rasterRescale(cumulative_pts)
    }
    
    return(cumulative_pts)
    
}
