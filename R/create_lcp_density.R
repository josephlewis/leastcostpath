#' creates a cumulative least-cost path layer
#' 
#'  Cumulatively combines least-cost paths to idenify routes of preferential movement
#'  
#' @param x \code{SpatRaster}
#' 
#' @param lcps \code{sf}
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
#' sf::st_point(c(1054446, 4232288)),
#' sf::st_point(c(957989, 4208863)),
#' crs = terra::crs(r)))
#' 
#' lcps <- create_FETE_lcps(x = slope_cs, locations = locs, cost_distance = TRUE)
#' 
#' lcps_dens <- create_lcp_density(x = r, lcps = lcps, rescale = TRUE)

create_lcp_density <- function(x, lcps, rescale = FALSE) {
  
  ras <- x
  ras <- terra::setValues(ras, 0)
  
  lcps_vect <- terra::vect(lcps)
  
  cumul_ras <- terra::rasterizeGeom(x = lcps_vect, y = ras, fun = "crosses")

  if(rescale) { 
    rast_min <- terra::minmax(cumul_ras)[1]
    rast_max <- terra::minmax(cumul_ras)[2]
    
    cumul_ras <- ((cumul_ras - rast_min)/(rast_max - rast_min)) 
  }
  
  cumul_ras[is.na(x)] <- NA
  
  return(cumul_ras)
  
}