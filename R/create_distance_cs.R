#' Create a Distance based cost surface
#'
#' Creates a cost surface based on the distance between neighbouring cells. Distance corrected for if neighbours value greater than 4 (diagonal distance greater than straight line distance)
#'
#' @param raster \code{RasterLayer} (raster package).
#'
#' @param neighbours \code{numeric} value. Number of neighbouring cells. See Huber and Church (1985) for methodological considerations when choosing number of neighbours. Expected numeric values are 4, 8, 16, 32, 48 or a matrix object. Default is numeric value 16
#'
#' @return \code{TransitionLayer} (gdistance package) numerically expressing the distance between neighbouring cells
#'
#' @author Joseph Lewis
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import gdistance
#'
#' @export
#'
#' @examples
#' r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
#' distance_cs <- create_distance_cs(r, neighbours = 16)

create_distance_cs <- function(raster, neighbours = 16) {

    if (!inherits(raster, "RasterLayer")) {
        stop("raster argument is invalid. Expecting a RasterLayer object")
    }

    if (any(!neighbours %in% c(4, 8, 16, 32, 48)) & (!inherits(neighbours, "matrix"))) {
        stop("neighbours argument is invalid. Expecting 4, 8, 16, 32, 48, or matrix object")
    }

    tr <- new("TransitionLayer", nrows = as.integer(nrow(raster)), ncols = as.integer(ncol(raster)), extent = extent(raster), crs = projection(raster, asText = FALSE),
        transitionMatrix = Matrix(0, ncell(raster), ncell(raster)), transitionCells = 1:ncell(raster))
    transitionMatr <- transitionMatrix(tr)
    Cells <- which(!is.na(getValues(raster)))

    if (inherits(neighbours, "numeric")) {
        if (neighbours == 32) {
            neighbours_matrix <- neighbours_32

        } else if (neighbours == 48) {
          neighbours_matrix <- neighbours_48
        }

    }

    adj <- adjacent(raster, cells = Cells, pairs = TRUE, target = Cells, directions = neighbours_matrix)
    adj <- adj[adj[, 1] < adj[, 2], ]
    transition.values <- 1
    transitionMatr[adj] <- as.vector(transition.values)
    transitionMatr <- forceSymmetric(transitionMatr)
    transitionMatrix(tr) <- transitionMatr
    matrixValues(tr) <- "conductance"

    if (neighbours > 4) {
        tr <- gdistance::geoCorrection(tr)
    }

    return(tr)

}
