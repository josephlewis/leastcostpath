#' replace values with values from another object
#' 
#' Replace values of \code{conductanceMatrix} x with the values of \code{conductanceMatrix} y that coincide with the supplied sf object
#' 
#' @param x \code{conductanceMatrix}
#' 
#' @param y \code{conductanceMatrix}
#' 
#' @param sf \code{sf}
#' 
#' @details 
#' 
#' The values of \code{conductanceMatrix} x are replaced with the values from \code{conductanceMatrix} y that coincide with the supplied sf object
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
#' x <- create_slope_cs(x = r, cost_function = "tobler", neighbours = 4)
#' 
#' locs <- sf::st_sf(geometry = sf::st_sfc(
#' sf::st_point(c(960745, 4166836)),
#' crs = terra::crs(r)))
#' locs <- sf::st_buffer(x = locs, dist = 25000)
#' 
#' y <- update_values(x = x, sf = locs, FUN = function(j) { j + 10})
#' 
#' output <- replace_values(x = x, y = y, sf = locs)
#' 
#' identical(y$conductanceMatrix, output$conductanceMatrix)

replace_values <- function(x, y, sf) { 
  
  cs_rast <- terra::rast(nrow = y$nrow, ncol = y$ncol, xmin = y$extent[1], xmax = y$extent[2], ymin = y$extent[3], ymax = y$extent[4],crs = y$crs)
  
  terra_vect <- terra::vect(sf)
  cells_indx <- terra::cells(cs_rast, terra_vect)[, 2]
  
  adj_indx <- Matrix::as.matrix(Matrix::summary(y$conductanceMatrix))[,1:2] 
  adj_indx <- adj_indx[adj_indx[,2] %in% cells_indx,, drop = FALSE]
  
  x$conductanceMatrix[adj_indx] <- y$conductanceMatrix[adj_indx]
  
  x$conductanceMatrix <- Matrix::drop0(x$conductanceMatrix)
  
  return(x)
}
