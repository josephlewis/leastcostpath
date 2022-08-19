#' Creates a slope-based cost surface
#' 
#'  Creates a cost surface based on the difficulty of moving up and down slope. This function provides multiple isotropic and anisotropic cost functions that estimate the 'cost' of human movement across a landscape.
#'  
#' The supplied 'spatRaster' object must have a projected CRS
#' 
#' @details
#' 
#' The following cost functions have been implemented however users may also supply their own cost functions:
#' 
#' 'tobler', 'tobler offpath', 'davey', 'rees', 'irmischer-clarke male', 'irmischer-clarke offpath male', 'irmischer-clarke female', 'irmischer-clarke offpath female', 'modified tobler', 'wheeled transport', 'herzog', 'llobera-sluckin', 'naismith', 'minetti', 'campbell', 'campbell 2019'
#'
#' @param x \code{SpatRaster}
#'
#' @param cost_function \code{character} or \code{function}. Cost function applied to slope values. See details for implemented cost functions. tobler (default)
#'
#' @param neighbours \code{numeric} value. Number of directions used in the conductance matrix calculation. Expected numeric values are 4, 8, 16, 32, 48. 16 (default)
#'
#' @param crit_slope \code{numeric} value. Critical Slope (in percentage) is 'the transition where switchbacks become more effective than direct uphill or downhill paths'. Cost of climbing the critical slope is twice as high as those for moving on flat terrain and is used for estimating the cost of using wheeled vehicles. Default value is 12, which is the postulated maximum gradient traversable by ancient transport (Verhagen and Jeneson, 2012). Critical slope only used in 'wheeled transport' cost function
#'
#' @param max_slope \code{numeric} value. Maximum percentage slope that is traversable. Slope values that are greater than the specified max_slope are given a conductivity value of 0. If cost_function argument is 'campbell 2019' then max_slope is fixed at 30 degrees slope to reflect the maximum slope that the cost function is parametised to. NULL (default)
#'
#' @param percentile \code{numeric} value. Travel rate percentile only used in 'campbell 2019' cost_function. Expected numeric values are 0.01, 0.05, 0.10, 0.15, 0.20, 0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95, 0.99. 0.5 (default)
#'
#' @param exaggeration \code{logical}. if TRUE, positive slope values (up-hill movement) multiplied by 1.99 and negative slope values (down-hill movement) multiplied by 2.31
#'
#' @return \code{conductanceMatrix} that numerically expresses the difficulty of moving across slope based on the provided cost function
#'
#' @author Joseph Lewis
#'
#' @export
#' 
#' @examples 
#' 
#' r <- terra::rast(system.file("ex/test.grd", package="terra"))
#' 
#' slope_cs <- create_slope_cs(x = r, cost_function = "tobler")
#' slope_cs2 <- create_slope_cs(x = r, 
#' cost_function = function(x) {(6 * exp(-3.5 * abs(x + 0.05))) / 3.6})
#' 

create_slope_cs <- function(x, cost_function = "tobler", neighbours = 16, crit_slope = 12, max_slope = NULL, percentile = 0.5, exaggeration = FALSE) {
    
    neighbours <- neighbourhood(neighbours = neighbours)
    
    cells <- which(!is.na(terra::values(x)))
    na_cells <- which(is.na(terra::values(x)))
    
    adj <- terra::adjacent(x = x, cells = cells, directions = neighbours, pairs = TRUE)
    adj <- adj[!adj[,2] %in% na_cells,]
    
    elev_values <- terra::values(x)[,1]
    
    rise <- (elev_values[adj[,2]] - elev_values[adj[,1]])
    run <- terra::distance(terra::xyFromCell(x, adj[,1]), terra::xyFromCell(x, adj[,2]), lonlat = FALSE, pairwise = TRUE)
    
    mathematical_slope <- rise/run
    
    if(exaggeration) { 
        mathematical_slope <- ifelse(mathematical_slope > 0, mathematical_slope * 1.99, mathematical_slope * 2.31)
    }
    
    ncells <- length(cells) + length(na_cells)
    
    cf <- cost(cost_function = cost_function, crit_slope = crit_slope, percentile = percentile)
    
    speed <- cf(mathematical_slope)
    
    conductance <- speed/run
    
    if(!is.null(max_slope)) {
        max_slope <- max_slope/100
        index <- abs(mathematical_slope) >= max_slope
        conductance[index] <- 0
    }
    
    cs_matrix <- Matrix::Matrix(data = 0, nrow = ncells, ncol = ncells)
    cs_matrix[adj] <- conductance
    
    cs <- list("conductanceMatrix" = cs_matrix, 
               "costFunction" = cost_function,
               "maxSlope" = ifelse(!is.null(max_slope), paste0(max_slope*100, "%"), NA), 
               "exaggeration" = exaggeration,
               "criticalSlope" = ifelse(test = !is.function(cost_function), yes = ifelse(test = cost_function == "wheeled transport", yes = paste0(max_slope, "%"), no = NA), no = NA),
               "percentile" = ifelse(test = !is.function(cost_function), yes = ifelse(test = cost_function == "campbell 2019", yes = percentile, no = NA), no = NA),
               "neighbours" = sum(neighbours, na.rm = TRUE), 
               "nrow" = terra::nrow(x), 
               "ncol" = terra::ncol(x), 
               "extent" = terra::ext(x), 
               "crs" = terra::crs(x, proj = TRUE))
    
    class(cs) <- "conductanceMatrix"
    
    return(cs)
    
}

#' @name create_slope_cs
#' @export

print.conductanceMatrix <- function(x) {
    cat("Class: ", class(x))
    if(!is.function(x$costFunction)) { cat("\ncost function: ", x$costFunction)}
    if(is.function(x$costFunction)) { cat("\ncost function: ", deparse(body(x$costFunction)[[2]]))}
    cat("\nneighbours:", x$neighbours)
    cat("\nmax slope:", x$maxSlope)
    cat("\nexaggeration:", x$exaggeration)
    cat("\ncritical slope:", x$criticalSlope)
    cat("\npercentile:", x$percentile)
    cat("\nSpatRaster dimenions: ", x$nrow, x$ncol, prod(x$nrow, x$ncol),  "(nrow, ncol, ncell)")
    cat("\nMatrix dimensions: ", x$conductanceMatrix@Dim,  "(nrow, ncol)")
    cat("\ncrs:", x$crs)
    cat("\nextent:", x$extent@ptr$vector, "(xmin, xmax, ymin, ymax)")
}