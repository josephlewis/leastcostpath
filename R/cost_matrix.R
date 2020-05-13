#' cost_matrix
#'
#' Creates a cost based nearest neighbour matrix
#'
#' Creates a cost based nearest neighbour matrix of k length for each provided location.
#'
#' @param cost_surface \code{TransitionLayer} object (gdistance package). Cost surface to be used in calculating the k nearest neighbour
#'
#' @param locations \code{SpatialPoints}. Locations to calculate k nearest neighbours from
#'
#' @param k \code{numeric} number of nearest neighbours to be returned
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
#' @examples
#'
#'r <- raster::raster(nrow=50, ncol=50,  xmn=0, xmx=50, ymn=0, ymx=50,
#'crs='+proj=utm')
#'
#'r[] <- stats::runif(1:length(r))
#'
#'slope_cs <- create_slope_cs(r, cost_function = 'tobler')
#'
#'locs <- sp::spsample(as(raster::extent(r), 'SpatialPolygons'),n=5,'regular')
#'
#'matrix <- cost_matrix(slope_cs, locs, 2)
#'
#'lcp_network <- create_lcp_network(slope_cs, locations = locs,
#'nb_matrix = matrix, cost_distance = FALSE, parallel = FALSE)

cost_matrix <- function(cost_surface, locations, k) {
    
    if (!inherits(cost_surface, "TransitionLayer")) {
        stop("cost_surface argument is invalid. Expecting a TransitionLayer object")
    }
    
    if (!inherits(locations, c("SpatialPoints", "SpatialPointsDataFrame"))) {
        stop("Locations argument is invalid. Expecting SpatialPoints or SpatialPointsDataFrame object")
    }
    
    if (!inherits(k, "numeric")) {
        stop("k argument is invalid. Expecting numeric object")
    }
    
    if (length(locations) < 2) {
        stop("Number of locations invalid. Expecting more than one location")
    }
    
    if (k > length(locations)) {
        stop("k Value exceeds number of locations")
    }
    
    origin <- rep(1:length(locations), each = k)
    destination <- rep(0, length(locations), each = k)
    
    for (i in 1:length(locations)) {
        
        x <- costDistance(x = cost_surface, fromCoords = locations[i, ], toCoords = locations[-i, ])
        
        x <- data.frame(ID = 2:length(locations), costDistance = as.vector(x))
        
        xx <- x[order(x$costDistance), ]
        
        print(xx[1:3, ])
        
        destination[which(origin == i)] <- xx$ID[1:3]
        
    }
    
    matrix <- matrix(data = c(origin, destination), ncol = 2)
    
    return(matrix)
    
}
