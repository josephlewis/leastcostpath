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
  
  cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, extent = x$extent, crs = x$crs)
  
  terra_vect <- terra::vect(sf)
  cells_indx <- terra::cells(cs_rast, terra_vect)[, 2]
  
  adj_indx <- which(x$conductanceMatrix!= 0, arr.ind = TRUE)
  adj_indx <- adj_indx[adj_indx[,2] %in% cells_indx,, drop = FALSE]
  
  x$conductanceMatrix[adj_indx] <- FUN(x$conductanceMatrix[adj_indx])
  
  return(x)
  
}