#' create_feature
#'
#' Creates Landscape Feature Cost Surface to be used in Least Cost Path calculation.
#'
#'The create_feature function computes a cost surface representating the attraction/repulsion of a feature in the landscape. The function requires a Raster (class 'RasterLayer'), the locaion of the feature(s) (class 'SpatialPoints'), and a vector (Class 'numeric') of attraction/repulsion values from the feature(s).
#'
#' @param raster Raster file. Expects Object of class RasterLayer. This is used to derive extent and spatial reference system of the landscape feature cost surface.
#'
#' @param locations Locations of landscape features. Expects Object of class SpatialPoints
#'
#' @param x vector of values denoting the attraction/repulsion from the landscape feature(s).
#'
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
#' loc1 = cbind(2667670, 6479000)
#' loc1 = sp::SpatialPoints(loc1)
#'
#' num <- seq(200, 1, length.out = 20)
#'
#' feature_attraction <- create_feature(raster = r, locations = loc1, x = num)

create_feature <- function(raster, locations, x) {
    message("note: create_feature expects planar coordinates")
    
    if (!inherits(raster, "RasterLayer")) {
        stop("raster expects a RasterLayer object")
    }
    
    if (!inherits(locations, "SpatialPoints")) {
        stop("locations expects a SpatialPoints object")
    }
    
    if (!inherits(x, "numeric")) {
        stop("x expects numeric vector")
    }
    
    r <- raster::rasterize(locations, raster)
    
    r <- raster::distance(r)
    
    mround <- function(x, base) {
        base * round(x/base)
    }
    
    r <- mround(r, res(r)[1]/100)
    
    r_mod <- matrix(cbind(unique(r)[1:length(x)], unique(r)[1:length(x)] + res(r)[1]/100, x), ncol = 3)
    
    r_mod <- r_mod[stats::complete.cases(r_mod), ]
    
    r[r > unique(r)[length(x)]] <- NA
    
    rc <- raster::reclassify(r, r_mod, include.lowest = TRUE, right = FALSE)
    
    rc[is.na(rc)] <- 1
    
    return(rc)
    
}
