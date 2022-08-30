create_stochastic_cs <- function(x, percent_quantile = 1) {
  
  if(percent_quantile > 1 | percent_quantile < 0) { 
    stop("percent_quantile argument is invalid. Expecting numeric value between 0 and 1")
  }
  
  quantile_val <- stats::quantile(x$conductanceMatrix@x, percent_quantile)
  threshold_val <- stats::runif(1, 0, quantile_val)
  
  x$conductanceMatrix@x[x$conductanceMatrix@x < threshold_val] <- 0
  
  x$conductanceMatrix <- Matrix::drop0(x$conductanceMatrix)
  
  return(x)
  
}