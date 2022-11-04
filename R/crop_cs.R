#' Crop conductanceMatrix to extent
#' 
#' @param x \code{spatRaster}
#'
#' @param extent \code{sf object or terra SpatRaster}. Extent obtained from object using terra::ext
#' 
#' @details
#' 
#' conductanceMatrix cropped to extent of supplied \code{Sf object} or \code{terra SpatRaster}. conductanceMatrix spatRaster dimensions and Matrix dimensions update to reflect cropped extent
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
#' ext <- sf::st_as_sfc(sf::st_bbox(rasterise(slope_cs)))
#' ext <- sf::st_buffer(ext, dist = -75000)
#' ext <- sf::st_as_sf(ext)
#' 
#' slope_cs_croped <- crop_cs(slope_cs, extent = ext)

crop_cs <- function(x, extent) { 
  
  cm_rast <- rasterise(x)
  
  cells <- terra::cells(x = cm_rast, y = terra::ext(extent))
  cm_cropped <- terra::crop(cm_rast, terra::ext(extent))
  
  x$conductanceMatrix <-  x$conductanceMatrix[cells, cells]
  
  x$nrow <- terra::nrow(cm_cropped)
  x$ncol <- terra::ncol(cm_cropped)
  x$extent <- cm_cropped@ptr$extent$vector
  
  return(x)
  
}
