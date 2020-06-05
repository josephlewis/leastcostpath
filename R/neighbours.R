#' 32 Neighbourhood matrix
#'
#' 32 neighbourhood matrix based on [Kovanen and Sarjakoski (2015)](https://doi.org/10.1145/2803172)
#'
#' @export
#'
#' @author Joseph Lewis

neighbours_32 <- matrix(c(NA, 1, 1, NA, 1, 1, NA, 1, NA, 1, NA, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, NA, NA, 1, 0, 1, NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, NA, 1, NA, 1, NA, 
    1, NA, 1, 1, NA, 1, 1, NA), nrow = 7, ncol = 7, byrow = TRUE)

#' 48 Neighbourhood matrix
#'
#' 48 neighbourhood matrix based on [Kovanen and Sarjakoski (2015)](https://doi.org/10.1145/2803172)
#'
#' @export
#'
#' @author Joseph Lewis

neighbours_48 <- matrix(c(NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, 1, NA, 1, 1, NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, NA, NA, 1, 
    0, 1, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, 1, NA, 1, 1, NA, 1, NA, 1, NA, 1, NA, 1, NA, 1, NA), nrow = 9, ncol = 9, 
    byrow = TRUE)
