#' Calculate least cost paths from each location to all other locations.
#'
#' Calculates least cost paths from each location to all other locations (i.e. From Everywhere To Everywhere (FETE)). This is based on the method proposed by White and Barber (2012).
#'
#' @param cost_surface \code{TransitionLayer} (gdistance package). Cost surface to be used in Least Cost Path calculation
#'
#' @param locations \code{SpatialPoints*} (sp package). Locations to calculate Least Cost Paths from and to
#'
#' @param cost_distance \code{logical}. if TRUE computes total accumulated cost for each Least Cost Path. Default is FALSE
#'
#'@param parallel \code{logical}. if TRUE the Least Cost Paths will be calculated in parallel. Number of Parallel socket clusters is total number of cores available minus 1. Default is FALSE
#'
#'@references White, DA. Barber, SB. (2012). Geospatial modeling of pedestrian transportation networks: a case study from precolumbian Oaxaca, Mexico. J Archaeol Sci 39:2684-2696. \url{https://doi.org/10.1016/j.jas.2012.04.017}
#'
#' @return \code{SpatialLinesDataFrame} (sp package). The resultant object contains least cost paths calculated from each location to all other locations
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
#'locs <- sp::spsample(as(raster::extent(r), 'SpatialPolygons'),n=5,'regular')
#'
#'lcp_network <- create_FETE_lcps(cost_surface = slope_cs, locations = locs,
#'cost_distance = FALSE, parallel = FALSE)

create_FETE_lcps <- function(cost_surface, locations, cost_distance = FALSE, parallel = FALSE) {

    if (!inherits(cost_surface, "TransitionLayer")) {
        stop("cost_surface argument is invalid. Expecting a TransitionLayer object")
    }

    if (!inherits(locations, c("SpatialPoints", "SpatialPointsDataFrame"))) {
        stop("Locations argument is invalid. Expecting SpatialPoints* object")
    }

    if (length(locations) < 2)
        stop("Number of locations invalid. Expecting more than one location")

    network <- (expand.grid(seq_along(locations), seq_along(locations)))

    network <- network[network[, 1] != network[, 2], ]

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

    lcps_issue <- which(sapply(1:nrow(network), FUN = function(x) { nrow(lcp_network[[x]]@lines[[1]]@Lines[[1]]@coords)}) == 1)

    check_pts <- unique(network[lcps_issue,1][duplicated(network[lcps_issue,1])])

    if (length(lcps_issue) != 0) {
        warning(length(lcps_issue), " lcps could not be calculated. Please check the supplied location points to ensure they are not in areas in which movement is prohibited.")
    }

    lcp_network <- do.call(rbind, lcp_network)

    if (length(lcps_issue) != 0) {

        lcp_network <- lcp_network[-lcps_issue,]

        network <- network[-lcps_issue,]

    }

    lcp_network <- SpatialLinesDataFrame(lcp_network, data.frame(from = network[, 1], to = network[, 2]), match.ID = FALSE)

    if (cost_distance) {

        cost_dist <- apply(network, MARGIN = 1, function(x) {
            gdistance::costDistance(cost_surface, locations[x[1], ], locations[x[2], ])
        })

        lcp_network$cost <- cost_dist

        lcp_network <- lcp_network[order(lcp_network$cost), ]

    }

    return(lcp_network)

}
