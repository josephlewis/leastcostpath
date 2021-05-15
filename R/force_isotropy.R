#' Convert anisotropic cost surfaces to isotropic
#'
#' Averages transition values from-to adjacent cells
#'
#' @details
#' force_isotropy averages (mean) the transition values from-to adjacent cells in a cost_surface. Through this, anisotropic cost functions (i.e. where movement down-slope is easier than movement up-slope) are converted to an isotropic cost function. When calculating an least cost path using the resultant surface, the least cost path from A-B and B-A will be the same. This is in contrast to anisotropic cost surfaces where the least cost path from A-B and B-A can differ.
#'
#' @references
#'
#'Herzog, I (2020). Spatial Analysis Based On Cost Functions, in Gillings, M., Hacıgüzeller, P., Lock, G. Archaeological Spatial Analysis. Routledge. pp. 333-358. \doi{10.4324/9781351243858-18}
#'
#' @param cost_surface \code{TransitionLayer} (gdistance package). Conductance surface
#'
#' @return \code{TransitionLayer} (gdistance package) Conductance surface where transition values from-to adjacent cells have been averaged
#'
#' @author Joseph Lewis
#'
#' @import gdistance
#'
#' @export
#'
#'@examples
#'
#'r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
#'
#' slope_cs <- create_slope_cs(r, cost_function = 'tobler', neighbours = 16, max_slope = NULL)
#'
#' slope_cs_iso <- force_isotropy(slope_cs)

force_isotropy <- function(cost_surface) {

    if (!inherits(cost_surface, "TransitionLayer")) {
        stop("cost_surface argument is invalid. Expecting a TransitionLayer object")
    }

    adj <- gdistance::adjacencyFromTransition(x = cost_surface)

    cost_surface[adj] <- rowMeans(cbind(cost_surface[adj[,1:2]], cost_surface[adj[,2:1]]))

    return(cost_surface)

}




