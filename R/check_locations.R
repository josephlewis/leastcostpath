#' check supplied locations
#' 
#' checks that locations can be reached when calculating least-cost paths
#' 
#' @param x \code{conductanceMatrix}
#' 
#' @param locations \code{sf} 'POINT' or 'MULTIPOINT', \code{SpatVector}, \code{data.frame} or \code{matrix} containing the locations coordinates
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
  
  cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, xmin = x$extent[1], xmax = x$extent[2], ymin = x$extent[3], ymax = x$extent[4],crs = x$crs)
  
  coords <- get_coordinates(locations)
  cells <- terra::cellFromXY(cs_rast, coords)
  cells <- cbind(cells[!is.nan(cells)])
  cells_na <- which(is.na(cells))
  
  connectivity_list <- apply(X = cells[-cells_na,, drop = FALSE], MARGIN = 1, FUN = function(j) { !all(x$conductanceMatrix[,j] == 0)})
  connected <- sum(connectivity_list)
  
  #### NA IN CELLS IS WHERE IT'S OUTSIDE THE AREA.. I CAN USE THIS TO DO THE OUTSIDE EXTENT. JUST NEED TO ONLY DO NON-NA ABOVE CHECKS IN CELLS AND CONNECTIVITY_LIST
  
  message(nrow(locations), " locations were supplied")
  message(connected, " location(s) are traversable from at least one adjacent cell")
  message(sum(!connectivity_list), " location(s) are not traversable from at least one adjacent cell")
  message(sum(is.na(cells)), " location(s) are outside the extent of the conductanceMatrix")

}
