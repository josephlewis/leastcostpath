#' Adds local stochasticity to a conductance surface
#' 
#' Adds local stochasticity to a conductance surface based on sampling adjacent neighbours. Probability of remaining adjacent and thus traversable is equal to the probability weight of the adjacent values. Sampling done with replacement so possible for multiple adjacent neighbours to remain traversable  
#' 
#' @param x \code{conductanceMatrix}
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
#' slope_cs2 <- add_local_stochasticity(slope_cs)

add_local_stochasticity <- function(x) { 
  
  adj <- Matrix::summary(x$conductanceMatrix)
  adj <- adj[order(adj$j),]
  
  stochastic_adj <- stats::aggregate(x ~ j, adj, function(x) {
    
    vals <- numeric(length = length(x))
    vals[unique(sample(x = length(x), size = length(x), prob = x / sum(x), replace = TRUE))] <- 1

    return(vals)    
  })
  
  x$conductanceMatrix@x <- x$conductanceMatrix@x * unlist(stochastic_adj$x)
  
  return(x)
  
}