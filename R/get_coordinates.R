#' get coordinates from a variety of different object classes
#' 
#' @param x \code{x} object which has coordinates. Argument expects \code{sf} of geometry type 'POINT' or 'MULTIPOINT', \code{SpatVector}, \code{data.frame}, or \code{matrix}
#' 
#' @author Joseph Lewis
#' 
#' @return \code{matrix} matrix of coordinates 
#' 
#' @noRd

get_coordinates <- function(x) { 
  
  if(inherits(x, "sf")) { 
    coords <- sf::st_coordinates(x)[, 1:2, drop = FALSE]
  }
  else if (inherits(x, "SpatVector")) { 
    coords <- terra::crds(x)
  }
  else if (inherits(x, "data.frame")) { 
    coords <- as.matrix(x)
  }
  else if (inherits(x, "matrix")) { 
    coords <- x
    }
  
  return(coords)
  
}
