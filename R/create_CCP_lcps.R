#' Calculate Cumulative Cost Paths from Radial Locations
#'
#' Calculates Least Cost Paths from radial locations of a specified distance to the centre location. This is based on the method proposed by Verhagen (2013).
#'
#' @param cost_surface \code{TransitionLayer} (gdistance package). Cost surface to be used in Least Cost Path calculation
#'
#' @param location \code{SpatialPoints*} (sp package). Location from which the Least Cost Paths are calculated. If there are multiple SpatialPoints in the supplied data, only the first SpatialPoint is taken into account
#'
#' @param distance \code{numeric} value. Distance from centre location to the radial locations
#'
#' @param radial_points \code{numeric} value. Number of radial locations around centre location
#'
#' @param cost_distance \code{logical}. if TRUE computes total accumulated cost for each Least Cost Path. Default is FALSE
#'
#' @param parallel \code{logical}. if TRUE, the Least Cost Paths will be calculated in parallel. Default is FALSE
#'
#' @param ncores \code{numeric}. Number of cores used if parallel is TRUE. Default value is 1.
#'
#' @references Verhagen, P. (2013). On the road to nowhere? Least cost paths, accessibility and the predictive modelling perspective. In Contreras F, Farjas M, Melero FJ (eds). Fusion of cultures. Proceedings of the 38th annual conference on computer applications and quantitative methods in archaeology, Granada, Spain, April 2010. (pp 383-389). Oxford: Archaeopress
#'
#' @return \code{SpatialLinesDataFrame} (sp package). The resultant object contains least cost paths (number of LCPs is dependent on radial_points argument) calculated from radial locations to a centre location within a specified distance.
#'
#' @author Joseph Lewis
#'
#' @import rgdal
#' @import rgeos
#' @import sp
#' @import raster
#' @import gdistance
#' @import pbapply
#'
#' @export
#'
#'@examples
#'
#' r <- raster::raster(nrow=50, ncol=50,  xmn=0, xmx=50, ymn=0, ymx=50,
#' crs='+proj=utm')
#'
#' r[] <- stats::runif(1:length(r))
#'
#' slope_cs <- create_slope_cs(r, cost_function = 'tobler')
#'
#' locs <- sp::spsample(as(raster::extent(r), 'SpatialPolygons'),n=1,'regular')
#'
#' lcp_network <- create_CCP_lcps(cost_surface = slope_cs, location = locs,
#' distance = 20, radial_points = 10, cost_distance = FALSE, parallel = FALSE)

create_CCP_lcps <- function(cost_surface, location, distance, radial_points, cost_distance = FALSE, parallel = FALSE, ncores = 1) {
    
    if (!inherits(cost_surface, "TransitionLayer")) {
        stop("cost_surface argument is invalid. Expecting a TransitionLayer object")
    }
    
    if (!inherits(location, c("SpatialPoints", "SpatialPointsDataFrame"))) {
        stop("Location argument is invalid. Expecting SpatialPoints* object")
    }
    
    if (!inherits(distance, "numeric")) {
        stop("distance argument is invalid. Expecting a numeric vector object")
    }
    
    if (distance <= 0) {
        stop("distance argument is invalid. Expecting a numeric value greater than 0")
    }
    
    if (!inherits(radial_points, "numeric")) {
        stop("radial argument is invalid. Expecting a numeric vector object")
    }
    
    if (radial_points <= 0) {
        stop("Number of radial points invalid. Expecting numeric value greater than 0")
    }
    
    if (!inherits(ncores, "numeric")) {
        stop("ncores argument is invalid. Expecting a numeric vector object")
    }
    
    location <- location[1, ]
    
    location <- methods::as(location, "SpatialPoints")
    
    circle <- rgeos::gBuffer(location, byid = FALSE, width = distance)
    
    circle <- methods::as(circle, "SpatialLines")
    
    circle_pts <- sp::spsample(circle, n = round(radial_points), "random")
    
    all_pts <- rbind(location, circle_pts)
    
    raster::crs(all_pts) <- raster::crs(cost_surface)
    
    ext <- methods::as(raster::extent(cost_surface), "SpatialPolygons")
    
    raster::crs(ext) <- raster::crs(cost_surface)
    
    all_pts <- raster::crop(all_pts, ext)
    
    if (length(all_pts) <= 1) {
        stop("All Radial Points outside of Raster extent. Decrease distance argument value to ensure Radial Points are inside Raster extent.")
    }
    
    network <- cbind(seq_along(all_pts), rep(x = 1, times = length(all_pts)))
    
    network <- network[network[, 1] != network[, 2], ]
    
    network <- matrix(network, ncol = 2)
    
    if (parallel) {
        
        no_cores <- ncores
        
        cl <- parallel::makeCluster(no_cores)
        
        parallel::clusterExport(cl, varlist = c("cost_surface", "all_pts"), envir = environment())
        
        lcp_network <- pbapply::pbapply(network, MARGIN = 1, function(x) {
            gdistance::shortestPath(cost_surface, all_pts[x[1], ], all_pts[x[2], ], output = "SpatialLines")
        }, cl = cl)
        
        parallel::stopCluster(cl)
        
    } else {
        
        lcp_network <- pbapply::pbapply(network, MARGIN = 1, function(x) {
            gdistance::shortestPath(cost_surface, all_pts[x[1], ], all_pts[x[2], ], output = "SpatialLines")
        })
        
    }
    
    lcps_issue <- which(sapply(1:nrow(network), FUN = function(x) {
        nrow(lcp_network[[x]]@lines[[1]]@Lines[[1]]@coords)
    }) == 1)
    
    check_pts <- unique(network[lcps_issue, 1][duplicated(network[lcps_issue, 1])])
    
    if (length(lcps_issue) != 0) {
        warning(length(lcps_issue), " lcps could not be calculated. Please check the supplied location points to ensure they are not in areas in which movement is prohibited.")
    }
    
    lcp_network <- do.call(rbind, lcp_network)
    
    if (length(lcps_issue) != 0) {
        
        lcp_network <- lcp_network[-lcps_issue, ]
        
        network <- network[-lcps_issue, ]
        
    }
    
    lcp_network <- SpatialLinesDataFrame(lcp_network, data.frame(from = network[, 1], to = network[, 2]), match.ID = FALSE)
    
    if (cost_distance) {
        
        cost_dist <- apply(network, MARGIN = 1, function(x) {
            gdistance::costDistance(cost_surface, all_pts[x[1], ], all_pts[x[2], ])
        })
        
        lcp_network$cost <- cost_dist
        
        lcp_network <- lcp_network[order(lcp_network$cost), ]
        
    }
    
    return(lcp_network)
}
