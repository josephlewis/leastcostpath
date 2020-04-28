#' create_lcp_network
#'
#' Calculates least cost paths from origins and destinations
#'
#' Computes least cost paths from each origins and destinations as specified in the neighbour matrix.
#'
#' @param cost_surface \code{TransitionLayer} object (gdistance package). Cost surface to be used in Least Cost Path calculation.
#'
#' @param locations \code{SpatialPoints}. Potential locations to calculate Least Cost Paths from and to.
#'
#' @param nb_matrix \code{matrix}. 2 column matrix representing the index of origins and destinations to calculate least cost paths between.
#'
#' @param cost_distance if TRUE computes total accumulated cost for each Least Cost Path. Default is FALSE.
#'
#'@param parallel if TRUE, the Least Cost Paths will be calculated in parallel. Number of Parallel socket clusters is total number of cores available minus 1. Default is FALSE.
#'
#' @return \code{SpatialLinesDataFrame} (sp package). The resultant object contains least cost paths calculated from each origins and destinations as specified in the neighbour matrix.
#'
#' @author Joseph Lewis
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import gdistance
#' @import parallel
#' @import pbapply
#'
#' @export
#'
#'@examples
#'
#'r <- raster::raster(nrow=50, ncol=50,  xmn=0, xmx=50, ymn=0, ymx=50,
#'crs='+proj=utm')
#'
#'r[] <- stats::runif(1:length(r))
#'
#'slope_cs <- create_slope_cs(r, cost_function = 'tobler')
#'
#'locs <- sp::spsample(as(r, 'SpatialPolygons'),n=5,'regular')
#'
#'lcp_network <- create_lcp_network(slope_cs, locations = locs,
#'nb_matrix = cbind(c(1, 4, 2, 1), c(2, 2, 4, 3)), cost_distance = FALSE, parallel = FALSE)

create_lcp_network <- function(cost_surface, locations, nb_matrix = NULL, cost_distance = FALSE, parallel = FALSE) {

    if (!inherits(cost_surface, "TransitionLayer")) {
        stop("cost_surface argument is invalid. Expecting a TransitionLayer object")
    }

    if (!inherits(locations, c("SpatialPoints", "SpatialPointsDataFrame"))) {
        stop("Locations argument is invalid. Expecting SpatialPoints or SpatialPointsDataFrame object")
    }

    if (length(locations) < 2) {
        stop("Number of locations invalid. Expecting more than one location")
    }

    if (!inherits(nb_matrix, "matrix")) {
        stop("nb_matrix argument is invalid. Expecting a two column matrix object")
    }

    if (max(nb_matrix) > length(locations)) {
        stop("Value within nb_matrix exceeds number of locations")
    }

    network <- nb_matrix

    if (parallel) {

        no_cores <- parallel::detectCores() - 1

        cl <- parallel::makeCluster(no_cores)

        parallel::clusterExport(cl, varlist = c("cost_surface", "locations"), envir = environment())

        lcp_network <- pbapply::pbapply(network, MARGIN = 1, function(x) {
            gdistance::shortestPath(cost_surface, locations[x[1], ], locations[x[2], ], output = "SpatialLines")
        }, cl = cl)

        parallel::stopCluster(cl)

    } else {

        lcp_network <- pbapply::pbapply(network, MARGIN = 1, function(x) {
            gdistance::shortestPath(cost_surface, locations[x[1], ], locations[x[2], ], output = "SpatialLines")
        })

    }

    lcp_network <- do.call(rbind, lcp_network)

    lcp_network <- SpatialLinesDataFrame(lcp_network, data.frame(from = network[, 1], to = network[, 2]), match.ID = FALSE)

    if (cost_distance) {

        cost_dist <- apply(network, MARGIN = 1, function(x) {
            gdistance::costDistance(cost_surface, locations[x[1], ], locations[x[2], ])
        })

        lcp_network$cost <- cost_dist

    }

    return(lcp_network)

}
