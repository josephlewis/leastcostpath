#' neighbourhood matrix to represent adjacent cells
#' 
#' @param neighbours \code{numeric} or \code{matrix} Expected numeric values are 4, 8, 16, 32, or 48. A user-supplied matrix of 0 and 1s can be supplied. Here, 0 denotes non-adjaency and 1 denotes adjacency
#' 
#' @author Joseph Lewis
#' 
#' @return \code{matrix} neighbourhood matrix
#' 
#' @export

neighbourhood <- function(neighbours) { 
  
  neighbours_32 <- matrix(c(0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 
                            1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0), nrow = 7, ncol = 7, byrow = TRUE)
  
  neighbours_48 <- matrix(c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 1, 1, 1, 
                            1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 1, 0, 1, 1, 0, 
                            1, 0, 1, 0, 1, 0, 1, 0, 1, 0), nrow = 9, ncol = 9, byrow = TRUE)
  
  if (inherits(neighbours, "matrix")) { 
    neighbours <- neighbours 
  } else if (neighbours == 4) {  
    neighbours <- 4
  } else if (neighbours == 8) {
    neighbours <- 8      
  } else if (neighbours == 16) { 
      neighbours <- 16
  } else if (neighbours == 32) { 
    neighbours <- neighbours_32
  } else if (neighbours == 48) { 
  neighbours <- neighbours_48
  } else (
    stop(paste0("neighbours argument invalid. Expecting 4, 8, 16, 32, 48, or a matrix object"))
  )
  
  return(neighbours)
}
