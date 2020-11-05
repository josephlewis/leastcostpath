#' Calculate Stochastic Least Cost Path from Origin to Destination
#'
#' Calculates a Stochastic Least Cost Path from an origin location to a destination location by randomly determining the neighbourhood adjacency. Method based on Pinto and Keitt (2009). Applies Dijkstra's algorithm. See details for more information.
#'
#' @param cost_surface \code{TransitionLayer} (gdistance package). Cost surface to be used in Least Cost Path calculation. Threshold value applied to cost surface before calculating least cost path
#'
#' @param origin \code{SpatialPoints*} (sp package) location from which the Least Cost Path is calculated. Only the first row is taken into account
#'
#' @param destination \code{SpatialPoints*} (sp package) location to which the Least Cost Path is calculated. Only the first row is taken into account
#'
#' @param directional \code{logical}. if TRUE Least Cost Path calculated from origin to destination only. If FALSE Least Cost Path calculated from origin to destination and destination to origin. Default is FALSE
#'
#' @param percent_quantile \code{numeric}. Optional numeric value between 0 and 1. If argument is supplied then threshold is a random value between the minimum value in the supplied cost surface and the corresponding percent quantile value in the supplied cost surface. If no argument is supplied, then the threshold is a random value between the minimum value and maximum valie in the supplied cost surface. See details for more information
#'
#' @details
#'
#' The calculation of a stochastic least cost path is based on the method proposed by Pinto and Keitt (2009). Instead of using a static neighbourhood (for example as supplied in the neighbours function in the create_slope_cs), the neighbourhood is redefined such that the adjacency is non-deterministic and is instead determined randomly based on the threshold value.
#'
#' The algorithm proceeds as follows:
#'
#' 1. If threshold_quantile is not supplied, draw a random value from a uniform distribution between the minimum value and maximum value in the supplied cost surface. If threshold_quantile is supplied, draw a random value between the minimum value in the supplied cost surface and the percent quantile as calculated using the supplied percent_quantile
#'
#' 2. Replace values in cost surface below the random value with 0. This ensures that the conductance between the neighbours are 0, and thus deemed non-adjacent.
#'
#' Supplying a percent_quantile of 0 is equivalent to calculating the non-stochastic least cost path. That is, if the supplied percent_quantile is 0, then no values are below this value and thus no values will be replaced with 0 (see step 2). This therefore does not change the neigbourhood adjacency.
#'
#' Supplying a percent_quantile of 1 is equivalent to not supplying a percent_quantile value at all. That is, if the supplied percent_quantile is 1, then the possible random threshold value is between the minimum and maximum values in the cost surface.
#'
#' The closer the percent_quantile is to 0, the less the stochastic least cost paths are expected to deviate from the least cost path. For example, a percent_quantile value of 0.2 will result in the threshold being a random value between the minimum value in the cost surface and the 0.2 percent quantile of the values in the cost surface. All values in the cost surface below the threshold will be replaced with 0 (i.e. the neighbours are no longer adjacent). In contrast, a percent_quantile value of 0.8 will result in the threshold being a random value between the minimum value in the cost surface and the 0.8 percent quantile of the values in the cost surface. In this case, there is greater probability that the random value will result in an increased number of values in the cost surface being replaced with 0.
#'
#' @references
#'
#' Dijkstra, E. W. (1959). A note on two problems in connexion with graphs. Numerische Mathematik. 1: 269-271.
#'
#' Pinto, N., Keitt, T.H. (2009) Beyond the least-cost path: evaluating corridor redundancy using a graph-theoretic approach. Landscape Ecol 24, 253-266 \url{https://doi.org/10.1007/s10980-008-9303-y}
#'
#' @return \code{SpatialLinesDataFrame} (sp package) of length 1 if directional argument is TRUE or 2 if directional argument is FALSE. The resultant object is the shortest route (i.e. least cost) between origin and destination after a random threshold has been applied to the supplied \code{TransitionLayer}.
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
#'@examples
#'
#'r <- raster::raster(nrow=50, ncol=50,  xmn=0, xmx=50, ymn=0, ymx=50,
#'crs='+proj=utm')
#'
#'r[] <- stats::runif(1:length(r))
#'
#'slope_cs <- create_slope_cs(r, cost_function = 'tobler')
#'
#'locs <- sp::spsample(as(raster::extent(r), 'SpatialPolygons'),n=2,'random')
#'
#'stochastic_lcp <- create_stochastic_lcp(cost_surface = slope_cs,
#'origin = locs[1,], destination = locs[2,], directional = FALSE)

create_stochastic_lcp <- function(cost_surface, origin, destination, directional = FALSE, percent_quantile) {
    
    if (!inherits(cost_surface, "TransitionLayer")) {
        stop("cost_surface argument is invalid. Expecting a TransitionLayer object")
    }
    
    done = FALSE
    
    while (!done) {
        
        adj <- gdistance::adjacencyFromTransition(cost_surface)
        
        min_val <- base::min(cost_surface[adj])
        max_val <- base::max(cost_surface[adj])
        
        cost <- cost_surface
        
        if (missing(percent_quantile)) {
            
            # random value between minimum and maximum value in cost surface
            threshold_val <- stats::runif(1, min_val, max_val)
        } else {
            
            quantile_val <- stats::quantile(cost[adj], percent_quantile)
            
            # random value between minimum and threshold quantile value in cost surface
            threshold_val <- stats::runif(1, min_val, quantile_val)
        }
        
        # replace values lower than threshold_val with 0. Neighbours with higher costs are more likely to be maintained. This is the inverse of stochastic
        # rule noted in Pinto and Keitt (2009) and reflects the cost surface representing conductivity rather than resistance.
        
        cost[adj] <- base::ifelse(cost[adj] < threshold_val, 0, cost[adj])
        
        stochastic_lcp <- suppressWarnings(create_lcp(cost_surface = cost, origin = origin, destination = destination, directional = directional, cost_distance = TRUE))
        
        # check to see if nrow of coordinates greater than 1. This check ensures that the LCP between origin and destination was feasible
        if (directional) {
            if (base::nrow(stochastic_lcp@lines[[1]]@Lines[[1]]@coords) > 1) {
                
            }
        } else if (directional == FALSE) {
            if (all((base::nrow(stochastic_lcp@lines[[1]]@Lines[[1]]@coords) > 1) & (base::nrow(stochastic_lcp@lines[[2]]@Lines[[1]]@coords) > 1))) {
            }
        }
        
        # check to see if costs are not infinite
        if (base::all(!is.infinite(stochastic_lcp$cost))) {
            
            done = TRUE
            
            return(stochastic_lcp)
            
        }
        
    }
    
}
