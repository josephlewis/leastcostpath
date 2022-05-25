#' Check locations
#'
#' Checks that locations can be reached when calculating least cost paths
#' 
#' @param cost_surface \code{TransitionLayer} (gdistance package). Cost surface to be used when checking whether supplied locations are traversable from at least one adjcacent cell
#'
#' @param locations \code{SpatialPoints*} (sp package) locations to check 
#' 
#' @details
#' Using the supplied cost surface, the function checks whether the cells of supplied locations are traversable from at least one adjacent cell. If the cells of the supplied location are not traversable from at least one adjacent cell then a calculated least cost path cannot traverse to that location.
#'
#' @return \code{numeric vector} of location indexes that are not traversable from at least one adjacent cell
#'
#' @author Joseph Lewis
#'
#' @import raster
#' @import gdistance
#'
#' @export
#'
check_locations <- function (cost_surface, locations) 
{
  if (!inherits(cost_surface, "TransitionLayer")) {
    stop("cost_surface argument is invalid. Expecting a TransitionLayer object")
  }
  if (!inherits(locations, c("SpatialPoints", "SpatialPointsDataFrame"))) {
    stop("locations argument is invalid. Expecting a SpatialPoints* object")
  }
  cs <- cost_surface
  cells <- raster::cellFromXY(raster::raster(cs), locations)
  connectivity_list <- apply(X = data.frame(cells), MARGIN = 1, 
                             FUN = function(x) {
                               all(cs@transitionMatrix[, x] == 0)
                             })
  indexes <- which(connectivity_list)
  
  if (length(indexes) > 0) {
    
    message(length(locations), " locations were supplied. ", 
            length(indexes), " locations are not traversable from at least one adjacent cell")
    return(indexes)
    
  } else { 
    message(length(locations), " locations were supplied. ", 
            " 0 locations are not traversable from at least one adjacent cell")
  }
}