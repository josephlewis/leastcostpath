#' Adds stochasticity to a cost surface
#' 
#' Adds stochasticity to a cost surface to ensure that the neighbourhood adjacency is random. Method based on Pinto and Keitt (2009)
#' 
#' @param x \code{conductanceMatrix} 
#' 
#' @param percent_quantile \code{numeric} value between 0 and 1. Optional numeric value between 0 and 1. See details for more information
#' 
#' @details 
#' 
#' The add_stochasticity to a cost surface is based on the method proposed by Pinto and Keitt (2009). Rather than using a static neighbourhood (for example as supplied in the neighbours function in the create_slope_cs), the neighbourhood is redefined such that the adjacency is non-deterministic and is instead determined randomly based on the threshold value.
#' 
#' The algorithm proceeds as follows:
#'
#' 1. With a percent_quantile supplied, draw a random value between the minimum value in the cost surface and the supplied percent quantile
#'
#' 2. Replace values in cost surface below this random value with 0. This ensures that the conductance between the neighbours are 0, and thus deemed non-adjacent.
#'
#' Supplying a percent_quantile of 0 is equivalent to incorporating no stochasticity into the cost surface. That is, if the supplied percent_quantile is 0, then no values are below this value and thus no values will be replaced with 0 (see step 2). This therefore does not change the neigbourhood adjacency.
#'
#' Supplying a percent_quantile of 1 is equivalent to not supplying a percent_quantile value at all. That is, if the supplied percent_quantile is 1, then the possible random threshold value is between the minimum and maximum values in the cost surface.
#'
#' The closer the percent_quantile is to 0, the less stochasticity is incorporated. For example, a percent_quantile value of 0.2 will result in the threshold being a random value between the minimum value in the cost surface and the 0.2 percent quantile of the values in the cost surface. All values in the cost surface below the random value will be replaced with 0 (i.e. the neighbours are no longer adjacent). In contrast, a percent_quantile value of 0.8 will result in the threshold being a random value between the minimum value in the cost surface and the 0.8 percent quantile of the values in the cost surface. In this case, there is greater probability that the random value will result in an increased number of values in the cost surface being replaced with 0.
#' 
#' @author Joseph Lewis
#'
#' @export
#' 
#' @examples 
#' 
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' 
#' slope_cs <- create_slope_cs(x = r, cost_function = "tobler", neighbours = 4)
#' 
#' stoch_slope_cs <- add_stochasticity(slope_cs, percent_quantile = 0.2)

add_stochasticity <- function(x, percent_quantile = 1) {
  
  if(percent_quantile > 1 | percent_quantile < 0) { 
    stop("percent_quantile argument is invalid. Expecting numeric value between 0 and 1")
  }
  
  quantile_val <- stats::quantile(x$conductanceMatrix@x, percent_quantile)
  threshold_val <- stats::runif(1, 0, quantile_val)
  
  x$conductanceMatrix@x[x$conductanceMatrix@x < threshold_val] <- 0
  
  x$conductanceMatrix <- Matrix::drop0(x$conductanceMatrix)
  
  return(x)
  
}