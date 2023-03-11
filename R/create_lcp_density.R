#' creates a cumulative least-cost path raster
#' 
#'  Cumulatively combines least-cost paths to idenify routes of preferential movement
#'  
#' @param x \code{SpatRaster}
#' 
#' @param lcps \code{sf} or \code{spatVector}
#' 
#' @param rescale \code{logical}. if TRUE, values scaled to between 0 and 1. FALSE (default)
#' 
#' @return \code{SpatRaster}
#' 
#' @author Joseph Lewis
#' 
#' @export
#' 
#' @examples 
#' 
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' 
#' slope_cs <- create_slope_cs(x = r, cost_function = "tobler", neighbours = 4)
#' 
#' locs <- sf::st_sf(geometry = sf::st_sfc(
#' sf::st_point(c(839769, 4199443)),
#' sf::st_point(c(1038608, 4100024)),
#' sf::st_point(c(907695, 4145478)),
#' crs = terra::crs(r)))
#' 
#' lcps <- create_FETE_lcps(x = slope_cs, locations = locs)
#' 
#' lcps_dens <- create_lcp_density(x = r, lcps = lcps)

create_lcp_density <- function(x, lcps, rescale = FALSE) {
  
  ras <- x
  ras <- terra::setValues(ras, 0)
  
  if(inherits(lcps, "sf")) {
    lcps_vect <- terra::vect(lcps)
  } else { 
    lcps_vect <- lcps
    }
  
  cumul_ras <- terra::rasterizeGeom(x = lcps_vect, y = ras, fun = "crosses")

  if(rescale) { 
    rast_min <- terra::minmax(cumul_ras)[1]
    rast_max <- terra::minmax(cumul_ras)[2]
    
    cumul_ras <- ((cumul_ras - rast_min)/(rast_max - rast_min)) 
  }
  
  cumul_ras[is.na(x)] <- NA
  
  return(cumul_ras)
  
}