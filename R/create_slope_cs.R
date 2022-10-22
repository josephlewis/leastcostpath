#' Creates a slope-based cost surface
#' 
#'  Creates a cost surface based on the difficulty of moving up and down slope. This function provides multiple isotropic and anisotropic cost functions that estimate the 'cost' of human movement when traversing a landscape
#'  
#' The supplied 'spatRaster' object must have a projected CRS
#' 
#' @details
#' 
#' The following cost functions have been implemented however users may also supply their own cost functions (see examples):
#' 
#' "tobler", "tobler offpath", "davey", 'rees', "irmischer-clarke male", "irmischer-clarke offpath male", "irmischer-clarke female", "irmischer-clarke offpath female", "modified tobler", 'garmy', 'kondo-saino', "wheeled transport", "herzog", "llobera-sluckin", 'naismith', 'minetti', 'campbell', "campbell 2019", "sullivan"
#'
#' @param x \code{SpatRaster} Digital Elevation Model (DEM)
#'
#' @param cost_function \code{character} or \code{function}. Cost function applied to slope values. See details for implemented cost functions. tobler (default)
#'
#' @param neighbours \code{numeric} value. Number of directions used in the conductance matrix calculation. Expected numeric values are 4, 8, 16, 32, 48, or matrix object. 16 (default)
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
#' @references 
#' 
#' Tobler, W. 1993. Three Presentations on Geographical Analysis and Modeling. Technical Report 93-1 (Santa Barbara, CA)
#' 
#' Davey, R.C., M. Hayes and J.M. Norman 1994. “Running Uphill: An Experimental Result and Its Applications,” The Journal of the Operational Research Society 45, 25
#' 
#' Rees, W.G. 2004. “Least-cost paths in mountainous terrain,” Computers & Geosciences 30, 203–09
#'
#' Irmischer, I.J. and K.C. Clarke 2018. “Measuring and modeling the speed of human navigation,” Cartography and Geographic Information Science 45, 177–86
#'
#' Márquez-Pérez, J., I. Vallejo-Villalta and J.I. Álvarez-Francoso 2017. “Estimated travel time for walking trails in natural areas,” Geografisk Tidsskrift-Danish Journal of Geography 117, 53–62
#'
#' Garmy, P. et al. 2005. “Logiques spatiales et ‘systèmes de villes’ en Lodévois de l’Antiquité à la période moderne,” Temps et espaces de l’homme en société, analyses et modèles spatiaux en archéologie 335–46
#'
#' Kondo, Y. and Y. Seino 2010. “GPS-aided walking experiments and data-driven travel cost modeling on the historical road of Nakasendō-Kisoji (Central Highland Japan),” Making History Interactive (Proceedings of the 37th International Conference, Williamsburg, Virginia, United States of America) 158–65
#' 
#' Herzog, I. 2013. “The potential and limits of Optimal Path Analysis,” in Bevan, A. and M. Lake (edd.), Computational approaches to archaeological spaces (Publications of the Institute of Archaeology, University College London) 179–211
#'
#' Llobera, M. and T.J. Sluckin 2007. “Zigzagging: Theoretical insights on climbing strategies,” Journal of Theoretical Biology 249, 206–17
#' 
#' Naismith, W. 1892. “Excursions: Cruach Ardran, Stobinian, and Ben More,” Scottish Mountaineering club journal 2, 136
#' 
#' Minetti, A.E. et al. 2002. “Energy cost of walking and running at extreme uphill and downhill slopes,” Journal of Applied Physiology 93, 1039–46
#' 
#' Campbell, M.J., P.E. Dennison and B.W. Butler 2017. “A LiDAR-based analysis of the effects of slope, vegetation density, and ground surface roughness on travel rates for wildland firefighter escape route mapping,” Int. J. Wildland Fire 26, 884
#' 
#' Campbell, M.J. et al. 2019. “Using crowdsourced fitness tracker data to model the relationship between slope and travel rates,” Applied Geography 106, 93–107
#' 
#' Sullivan, P.R. et al. 2020. “Modeling Wildland Firefighter Travel Rates by Terrain Slope: Results from GPS-Tracking of Type 1 Crew Movement,” Fire 3, 52
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
#' slope_cs2 <- create_slope_cs(x = r, 
#' cost_function = function(x) {(6 * exp(-3.5 * abs(x + 0.05))) / 3.6}, neighbours = 4)
#' 

create_slope_cs <- function(x, cost_function = "tobler", neighbours = 16, crit_slope = 12, max_slope = NULL, percentile = 0.5, exaggeration = FALSE) {
  
  if(terra::is.lonlat(x)) { 
    stop("supplied digital elevation model (DEM) is invalid. x argument expects DEM with a projected coordinate system")
  }
    
  neighbours <- neighbourhood(neighbours = neighbours)
    
  cells <- which(!is.na(terra::values(x)))
  na_cells <- which(is.na(terra::values(x)))
    
  adj <- terra::adjacent(x = x, cells = cells, directions = neighbours, pairs = TRUE)
  adj <- adj[!adj[,2] %in% na_cells,]
    
  elev_values <- terra::values(x)[,1]
    
  message("calculating slope...")
    
  rise <- (elev_values[adj[,2]] - elev_values[adj[,1]])
  run <- calculate_distance(x = x, adj = adj)
    
  mathematical_slope <- rise/run
    
  if(exaggeration) { 
      mathematical_slope <- ifelse(mathematical_slope > 0, mathematical_slope * 1.99, mathematical_slope * 2.31)
  }
    
  ncells <- length(cells) + length(na_cells)
    
  cf <- cost(cost_function = cost_function, crit_slope = crit_slope, percentile = percentile)
    
  if(is.function(cost_function)) { 
    message(c("Applying ", deparse(body(cost_function)[[2]]), " cost function"))
  } else{ 
    message(c("Applying ", cost_function, " cost function"))
  }
    
  speed <- cf(mathematical_slope)
    
  conductance <- speed/run
    
  if(!is.null(max_slope)) {
      max_slope <- max_slope/100
      index <- abs(mathematical_slope) >= max_slope
      conductance[index] <- 0
  }
    
  cs_matrix <- Matrix::Matrix(data = 0, nrow = ncells, ncol = ncells, sparse = TRUE)
  cs_matrix[adj] <- conductance
    
  cs <- list("conductanceMatrix" = cs_matrix, 
             "costFunction" = cost_function,
             "maxSlope" = ifelse(!is.null(max_slope), paste0(max_slope*100, "%"), NA), 
             "exaggeration" = exaggeration,
             "criticalSlope" = ifelse(test = !is.function(cost_function), yes = ifelse(test = cost_function == "wheeled transport", yes = paste0(max_slope, "%"), no = NA), no = NA),
             "percentile" = ifelse(test = !is.function(cost_function), yes = ifelse(test = cost_function == "campbell 2019", yes = percentile, no = NA), no = NA),
             "neighbours" = sum(neighbours, na.rm = TRUE),
             "resolution" = terra::res(x), 
             "nrow" = terra::nrow(x), 
             "ncol" = terra::ncol(x), 
             "extent" = x@ptr$extent$vector, 
             "crs" = terra::crs(x, proj = TRUE))
    
  class(cs) <- "conductanceMatrix"
    
  return(cs)
    
}

#' @export

print.conductanceMatrix <- function(x) {
    cat("Class: ", class(x))
    if(!is.function(x$costFunction)) { cat("\ncost function: ", x$costFunction)}
    if(is.function(x$costFunction)) { cat("\ncost function: ", deparse(body(x$costFunction)[[2]]))}
    cat("\nneighbours:", x$neighbours)
    cat("\nresolution:", x$resolution, "(x, y)")
    cat("\nmax slope:", x$maxSlope)
    cat("\nexaggeration:", x$exaggeration)
    cat("\ncritical slope:", x$criticalSlope)
    cat("\npercentile:", x$percentile)
    cat("\nSpatRaster dimenions: ", x$nrow, x$ncol, prod(x$nrow, x$ncol),  "(nrow, ncol, ncell)")
    cat("\nMatrix dimensions: ", x$conductanceMatrix@Dim,  "(nrow, ncol)")
    cat("\ncrs:", x$crs)
    cat("\nextent:", x$extent, "(xmin, xmax, ymin, ymax)")
}

#' @export

plot.conductanceMatrix <- function(x) {
    
  cs_rast <- terra::rast(nrow = x$nrow, ncol = x$ncol, xmin = x$extent[1], xmax = x$extent[2], ymin = x$extent[3], ymax = x$extent[4],crs = x$crs)
    
    col_sum <- Matrix::colSums(x$conductanceMatrix)
    row_sum <- Matrix::rowSums(x$conductanceMatrix)
    
    logical_sm <- methods::as(x$conductance, "lMatrix")
    
    ncols <- Matrix::colSums(logical_sm)
    nrows <- Matrix::rowSums(logical_sm)
    
    vals <- ((col_sum / ncols) + (row_sum / nrows)) / 2
    
    cs_rast <- terra::setValues(cs_rast, vals)
    
    terra::plot(cs_rast)
    
}
