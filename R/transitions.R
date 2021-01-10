#' calculate slope between two adjacent cells. Vectorised form of gdistance::transition
#'
#' @param x RasterLayer (raster package)
#'
#' @param neighbours \code{numeric} value. Number of directions used in the Least Cost Path calculation. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected values are 4, 8, 16, 32, or 48. Default is 16
#'
#' @noRd
#'
#' @import Matrix
#' @import gdistance
#'
#' @author Joseph Lewis

transition_slope <- function(x, neighbours) {

    tr <- new("TransitionLayer", nrows = as.integer(nrow(x)), ncols = as.integer(ncol(x)), extent = extent(x), crs = projection(x, asText = FALSE), transitionMatrix = Matrix(0,
        ncell(x), ncell(x)), transitionCells = 1:ncell(x))
    transitionMatr <- transitionMatrix(tr)
    Cells <- which(!is.na(getValues(x)))

    if (inherits(neighbours, "numeric")) {
        if (neighbours == 32) {
            neighbours_matrix <- neighbours_32

        } else if (neighbours == 48) {
            neighbours_matrix <- neighbours_48
        }

    }

    adj <- raster::adjacent(x, cells = Cells, pairs = TRUE, target = Cells, directions = neighbours_matrix)

    transition.values <- getValues(x)[adj[, 2]] - getValues(x)[adj[, 1]]

    transitionMatr[adj] <- as.vector(transition.values)
    transitionMatrix(tr) <- transitionMatr
    matrixValues(tr) <- "conductance"
    return(tr)
}
