#' Creates a distance-based cost surface
#' 
#' Creates a cost surface based on the distance between neighbouring cells. Distance corrected for if neighbours value is greater than 4.
#' 
#' @param x \code{SpatRaster}
#'
#' @param neighbours \code{numeric} value. Number of directions used in the conductance matrix calculation. Expected numeric values are 4, 8, 16, 32, 48. 16 (default)
#'
#' @param max_slope \code{numeric} value. Maximum percentage slope that is traversable. Slope values that are greater than the specified max_slope are given a conductivity value of 0. If cost_function argument is 'campbell 2019' then max_slope is fixed at 30 degrees slope to reflect the maximum slope that the cost function is parametised to. NULL (default)
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
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' 
#' distance_cs <- create_distance_cs(x = r, neighbours = 4)

create_distance_cs <- function(x, neighbours = 16, max_slope = NULL, exaggeration = FALSE) { 
  
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
  
  conductance <- rep(1, length(mathematical_slope))
  
  if(sum(neighbours) > 4) { 
    conductance <- conductance/run
  }
  
  if(!is.null(max_slope)) {
    max_slope <- max_slope/100
    index <- abs(mathematical_slope) >= max_slope
    conductance[index] <- 0
  }
  
  cs_matrix <- Matrix::Matrix(data = 0, nrow = ncells, ncol = ncells)
  cs_matrix[adj] <- conductance
  
  cs <- list("conductanceMatrix" = cs_matrix, 
             "costFunction" = "distance",
             "maxSlope" = ifelse(!is.null(max_slope), paste0(max_slope*100, "%"), NA), 
             "exaggeration" = exaggeration,
             "criticalSlope" = NA,
             "percentile" = NA,
             "neighbours" = sum(neighbours, na.rm = TRUE), 
             "nrow" = terra::nrow(x), 
             "ncol" = terra::ncol(x), 
             "extent" = terra::ext(x), 
             "crs" = terra::crs(x, proj = TRUE))
  
  class(cs) <- "conductanceMatrix"
  
  return(cs)
  
}

