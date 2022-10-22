#' Creates a cost surface from a SpatRaster
#' 
#'  Creates a cost surface using the values in the supplied SpatRaster. This function also provides for the inclusion of maximum slope traversable
#'  
#' The supplied 'spatRaster' object must have a projected CRS
#'
#' @param x \code{SpatRaster}
#'
#' @param neighbours \code{numeric} value. Number of directions used in the conductance matrix calculation. Expected numeric values are 4, 8, 16, 32, 48, or matrix object. 16 (default)
#' 
#' @param dem \code{SpatRaster} Digital Elevation Model (DEM)
#'
#' @param max_slope \code{numeric} value. Maximum percentage slope that is traversable. Slope values that are greater than the specified max_slope are given a conductivity value of 0. If cost_function argument is 'campbell 2019' then max_slope is fixed at 30 degrees slope to reflect the maximum slope that the cost function is parametised to. NULL (default)
#'
#' @param exaggeration \code{logical}. if TRUE, positive slope values (up-hill movement) multiplied by 1.99 and negative slope values (down-hill movement) multiplied by 2.31
#'
#' @return \code{conductanceMatrix} that numerically expresses the difficulty of moving across a surface based on the provided SpatRaster
#'
#' @author Joseph Lewis
#'
#' @export
#' 
#' @examples 
#' 
#' r <- terra::rast(system.file("extdata/SICILY_1000m.tif", package="leastcostpath"))
#' 
#' cs1 <- create_cs(x = r, neighbours = 16, dem = NULL, max_slope = NULL)
#' cs2 <- create_cs(x = r, neighbours = 16, dem = r, max_slope = 10)

create_cs <- function(x, neighbours = 16, dem = NULL, max_slope = NULL, exaggeration = FALSE) {

  neighbours <- neighbourhood(neighbours = neighbours)
  cells <- which(!is.na(terra::values(x)))
  na_cells <- which(is.na(terra::values(x)))
  adj <- terra::adjacent(x = x, cells = cells, directions = neighbours, 
                         pairs = TRUE)
  adj <- adj[!adj[, 2] %in% na_cells, ]
  
  spatvals <- terra::values(x)[, 1]
  spatvals <- spatvals[adj[, 2]]
  
  if(any(c(!is.null(dem), !is.null(max_slope)))) {
    
    if(any(c(is.null(dem), is.null(max_slope)))) { 
      stop("Both dem and max_slope argument are required when setting a maximum slope traversable")
    }
    
    if(sum(is.na(terra::values(x))) != sum(is.na(terra::values(dem)))) { 
      stop("Number of NA cells in x argument is not equal to number of NA cells in dem argument")
    }
    
    message("calculating slope in order to identify slopes greater/less than max_slope argument...")
    
    elev_values <- terra::values(dem)[,1]

    rise <- (elev_values[adj[,2]] - elev_values[adj[,1]])
    run <- calculate_distance(x = x, adj = adj)
    
    mathematical_slope <- rise/run
   
    if(exaggeration) { 
      mathematical_slope <- ifelse(mathematical_slope > 0, mathematical_slope * 1.99, mathematical_slope * 2.31)
    }
    
    max_slope <- max_slope/100
    index <- abs(mathematical_slope) >= max_slope
    spatvals[index] <- 0
    }

  ncells <- length(cells) + length(na_cells)
  cs_matrix <- Matrix::Matrix(data = 0, nrow = ncells, ncol = ncells)
  cs_matrix[adj] <- spatvals
  
  cs <- list(conductanceMatrix = cs_matrix, 
             costFunction = NA, 
             "maxSlope" = ifelse(!is.null(max_slope), paste0(max_slope*100, "%"), NA), 
             exaggeration = exaggeration, 
             criticalSlope = NA,
             percentile = NA, 
             neighbours = sum(neighbours, na.rm = TRUE), nrow = terra::nrow(x), ncol = terra::ncol(x), 
             "resolution" = terra::res(x), 
             extent = x@ptr$extent$vector, 
             crs = terra::crs(x, proj = TRUE))
  
  class(cs) <- "conductanceMatrix"
  
  return(cs)
}
