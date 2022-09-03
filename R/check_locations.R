#' check supplied locations
#' 
#' checks that locations can be reached when calculating least-cost paths
#' 
#' @param x \code{conductanceMatrix}
#' 
#' @param locations \code{sf} of geometry type 'POINT' or 'MULTIPOINT'
#' 
#' @details 
#' 
#' Using the supplied conductanceMatrix, the function checks whether:
#' (1) the supplied locations are traversable from at least one adjacent cell
#' (2) the supplied locations are within the extent of the supplied conductanceMatrix
#' 
#' @author Joseph Lewis
#' 
#' @return \code{message} 
#' 
#' @export
#' 
#' @examples 
#' 
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' 
#' slope_cs <- create_slope_cs(x = r, cost_function = "tobler")
#' 
#' locs <- sf::st_sf(geometry = sf::st_sfc(
#' sf::st_point(c(861534, 4173726)),
#' sf::st_point(c(897360, 4155813)),
#' sf::st_point(c(928364, 4138588)),
#' sf::st_point(c(862223, 4128943)),
#' sf::st_point(c(1119209, 4143411)),
#' crs = terra::crs(r)))
#' 
#' check_locations(x = slope_cs, locations = locs)

check_locations <- function(x, locations) { 
  
  if(!all(sf::st_geometry_type(locations) %in% c("POINT", "MULTITYPE"))) { 
    stop("Invalid locations argument. locations must be a sf object of geometry type 'POINT' or 'MULTIPOINT'")
    }
  
  cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, xmin = x$extent[1], xmax = x$extent[2], ymin = x$extent[3], ymax = x$extent[4],crs = x$crs)
  
  coords <- sf::st_coordinates(locations)[, 1:2, drop = FALSE]
  cells <- terra::cellFromXY(cs_rast, coords)
  cells <- cbind(cells[!is.nan(cells)])
  
  connectivity_list <- apply(X = cells, MARGIN = 1, FUN = function(j) { all(x$conductanceMatrix[,j] == 0)})
  connected <- sum(connectivity_list)
  
  message(nrow(locations), " locations were supplied")
  message(nrow(cells) - connected, " location(s) are traversable from at least one adjacent cell")
  message(nrow(locations) - ((nrow(cells) - connected) + (nrow(locations) - nrow(cells))), " location(s) are not traversable from at least one adjacent cell")
  message(nrow(locations) - nrow(cells), " location(s) are outside the extent of the conductanceMatrix")
  
}
