#' update values in conductanceMatrix
#' 
#' Update values in conductanceMatrix based on the supplied sf object
#' 
#' @param x \code{conductanceMatrix}
#' 
#' @param sf \code{sf}
#' 
#' @param FUN \code{function}
#' 
#' @details 
#' 
#' the updated conductanceMatrix is produced by assessing which areas of the conductanceMatrix coincide with the supplied sf object. The values within the areas that coincide with the sf object are manipulated based on the supplied function 
#' 
#' @return \code{conductanceMatrix} 
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
#' sf::st_point(c(960745, 4166836)),
#' crs = terra::crs(r)))
#' 
#' locs <- sf::st_buffer(x = locs, dist = 25000)
#' 
#' slope_cs2 <- update_values(x = slope_cs, sf = locs, 
#' FUN = function(j) { j * 0.6})
#' 
#' slope_cs3 <- update_values(x = slope_cs, sf = locs, 
#' FUN = function(j) { j + 10})
#' 
#' slope_cs4 <- update_values(x = slope_cs, sf = locs, 
#' FUN = function(j) { replace(x = j, values = 0)})

update_values <- function(x, sf, FUN) {
  
  if(!inherits(x, "conductanceMatrix")) { 
    stop("Invalid x argument. x must be a conductanceMatrix object")
  }
  
  if(!inherits(sf, c("sf", "sfc"))) {
    stop("Invalid sf argument. sf must be a sf or sfc object")
    }
  
  if(!is.function(FUN)) { 
    stop("Invalid FUN argument. FUN must be a function")
    }
  
  cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, xmin = x$extent[1], xmax = x$extent[2], ymin = x$extent[3], ymax = x$extent[4],crs = x$crs)
  
  terra_vect <- terra::vect(sf)
  cells_indx <- terra::cells(cs_rast, terra_vect)[, 2]
  
  adj_indx <- Matrix::which(x$conductanceMatrix!= 0, arr.ind = TRUE)
  adj_indx <- adj_indx[adj_indx[,2] %in% cells_indx,, drop = FALSE]
  
  x$conductanceMatrix[adj_indx] <- FUN(x$conductanceMatrix[adj_indx])
  
  return(x)
  
}
