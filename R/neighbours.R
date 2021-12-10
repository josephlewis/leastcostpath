#' 32 Neighbourhood matrices based on Kovanen and Sarjakoski (2015)
#'
#' see leastcostpath::neighbours_32 for layout
#'
#' @export
#'
#' @author Joseph Lewis
#'
#' @references
#'
#' Kovanen, J., Sarjakoski, T. (2015). Tilewise Accumulated Cost Surface Computation with Graphics Processing Units. ACM Transactions on Spatial Algorithms and Systems 1, 1-27. \doi{10.1145/2803172}
#'

neighbours_32 <- matrix(c(NA, 1, 1, NA, 1, 1, NA, 1, NA, 1, NA, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, NA, NA, 1, 0, 1, NA, NA, 1, 1, 1, 1, 1, 
    1, 1, 1, NA, 1, NA, 1, NA, 1, NA, 1, 1, NA, 1, 1, NA), nrow = 7, ncol = 7, byrow = TRUE)

#' 48 Neighbourhood matrices based on Kovanen and Sarjakoski (2015)
#'
#' see leastcostpath::neighbours_48 for layout
#'
#' @export
#'
#' @author Joseph Lewis
#'
#' @references
#'
#' Kovanen, J., Sarjakoski, T. (2015). Tilewise Accumulated Cost Surface Computation with Graphics Processing Units. ACM Transactions on Spatial Algorithms and Systems  1, 1-27. \doi{10.1145/2803172}

neighbours_48 <- matrix(c(NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, 1, NA, 1, 1, NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, 1, 1, 1, 1, 
    1, 1, 1, 1, NA, NA, NA, 1, 0, 1, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, 1, NA, 1, 1, NA, 
    1, NA, 1, NA, 1, NA, 1, NA, 1, NA), nrow = 9, ncol = 9, byrow = TRUE)
