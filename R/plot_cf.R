#' plot a conductance function
#' 
#' create a plot showing the conductance (inverse of cost) when traversing slope
#' 
#' @details
#' 
#' The following cost functions have been implemented however users may also supply their own cost functions (see examples):
#' 
#' "tobler", "tobler offpath", "davey", 'rees', "irmischer-clarke male", "irmischer-clarke offpath male", "irmischer-clarke female", "irmischer-clarke offpath female", "modified tobler", 'garmy', 'kondo-saino', "wheeled transport", "herzog", "llobera-sluckin", 'naismith', 'minetti', 'campbell', "campbell 2019", "sullivan"
#' 
#' @param cost_function \code{character} or \code{function}. Cost function applied to slope values. See details for implemented cost functions. tobler (default)
#' 
#' @param crit_slope \code{numeric} value. Critical Slope (in percentage) is 'the transition where switchbacks become more effective than direct uphill or downhill paths'. Cost of climbing the critical slope is twice as high as those for moving on flat terrain and is used for estimating the cost of using wheeled vehicles. Default value is 12, which is the postulated maximum gradient traversable by ancient transport (Verhagen and Jeneson, 2012). Critical slope only used in 'wheeled transport' cost function
#'
#' @param percentile \code{numeric} value. Travel rate percentile only used in 'campbell 2019' cost_function. Expected numeric values are 0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.99. 0.5 (default)
#'
#' @param rescale \code{logical} if TRUE conductance values scaled to between 0 and 1. This can be useful when comparing multiple cost functions
#' 
#' @param title \code{character} title to be included in plot. NULL (default)
#' 
#' @return \code{plot} showing the conductance (inverse of cost) when traversing slope
#'
#' @author Joseph Lewis
#'
#' @export
#' 
#' @examples 
#' 
#' plot_cf(cost_function = "tobler")
#' plot_cf(cost_function = function(x) { ((6 * exp(-3.5 * abs(x + 0.05))) * 0.6) / 3.6})
#' 
#' plot_cf(cost_function = "wheeled transport", crit_slope = 8)
#' plot_cf(cost_function = "campbell 2019", percentile = 0.5)

plot_cf <- function(cost_function, crit_slope = 12, percentile = 0.5, rescale = TRUE, title = NULL) {
  
  cfs <- c("tobler", "tobler offpath", "davey", 'rees', "irmischer-clarke male", "irmischer-clarke offpath male", "irmischer-clarke female", "irmischer-clarke offpath female", "modified tobler", 'garmy', 'kondo-saino', "wheeled transport", "herzog", "llobera-sluckin", 'naismith', 'minetti', 'campbell', "campbell 2019", "sullivan")
  
  slope <- seq(-1, 1, 0.001)
  
  if (inherits(cost_function, "character")) {
    if (any(!cost_function %in% cfs)) {
      stop("cost_function argument is invalid. See details for accepted cost functions")
    }
  }
  
  cf <- cost(cost_function = cost_function, crit_slope = crit_slope, percentile = percentile)
  vals <- cf(slope)
  
  if(rescale) {
    vals <- (vals - min(vals)) / (max(vals) - min(vals))
  }
  
  plot(1, 
       type="n", 
       xlab="Mathematical Slope", 
       ylab= ifelse(rescale, "Relative Conductance", "Conductance"), 
       xlim=c(-1, 1), 
       ylim= c(0, ceiling(max(vals)) + ceiling(max(vals)) * 0.1),
       xaxt='n')
  
  graphics::lines(slope, vals, lwd = 3, lty = 1)
  graphics::axis(side = 1, at= seq(-1, 1, 0.2))
  graphics::title(main = title, adj = 0)
  graphics::legend("topright",
         legend = ifelse(test = is.function(cost_function), yes = gsub(" ", "", deparse(body(cost_function))[[2]]), no = cost_function),
         lty = 1,
         lwd = 3)
}