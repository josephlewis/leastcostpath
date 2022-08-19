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

create_lcp_density <- function(x, lcps, rescale = FALSE) {
  
  cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, extent = x$extent, crs = x$crs)
  cs_rast <- terra::setValues(cs_rast, 0)
  
  lcps_vect <- terra::vect(lcps)
  
  cum_rast <- rasterizeGeom(x = lcps_vect, y = cs_rast, fun = "crosses")

  if(rescale) { 
    rast_min <- terra::minmax(cum_rast)[1]
    rast_max <- terra::minmax(cum_rast)[2]
    
    cum_rast <- ((cum_rast - rast_min)/(rast_max - rast_min)) 
  }
  
  return(cum_rast)
  
}
