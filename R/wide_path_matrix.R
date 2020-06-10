#' Create a wide path matrix
#'
#' Creates a wide path matrix to be used when calculating wide path least cost paths. This function will return an odd-dimension matrix approximating the shape of an octogan. The centre cell of the matrix has a value of 0 and represents the focal cell. See \code{\link[raster]{focal}}, \code{\link[raster]{focalWeight}} and \code{\link[raster]{adjacent}} for more information.
#'
#' @param ncells \code{numeric} value. Dimension of wide path matrix. Note that the value refers to the number of cells and not distance
#'
#' @return \code{matrix} wide path matrix used when calculating wide path least cost paths via \code{\link{create_wide_lcp}}
#'
#' @author Joseph Lewis
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import gdistance
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' w <- wide_path_matrix(9)

wide_path_matrix <- function(ncells) {
    
    if (!inherits(ncells, "numeric")) {
        stop("ncells argument is invalid. Expecting numeric value")
    }
    
    if (ncells%%2 == 0) {
        stop("ncells argument is invalid. Expecting odd numeric value")
    }
    
    if (ncells < 3) {
        stop("ncells argument is invalid. Expecting numeric value 3 or greater")
    }
    
    wpMatrix = matrix(1, nrow = ceiling(ncells), ncol = ceiling(ncells))
    
    indents <- ceiling((nrow(wpMatrix)/3) - 1)
    
    if (indents > 0) {
        
        for (i in 1:indents) {
            row_idx = c(i, ncol(wpMatrix) - i + 1)
            col_idx = c(1:(indents - i + 1))
            wpMatrix[row_idx, c(col_idx, nrow(wpMatrix) - col_idx + 1)] <- NA
        }
        
    }
    
    start_row = (nrow(wpMatrix))/2 + 1
    start_col = (ncol(wpMatrix))/2 + 1
    
    wpMatrix[start_row, start_col] <- 0
    
    return(wpMatrix)
    
}
