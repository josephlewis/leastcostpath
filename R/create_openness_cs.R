create_openness_cs <- function(dem, neighbours = 16) {
    
    open_Diff <- function(x) {
        x[2] - x[1]
    }
    
    open_diff_ts <- gdistance::transition(dem, open_Diff, neighbours, symm = FALSE)
    
    open_diff_ts <- gdistance::geoCorrection(open_diff_ts)
    
    open_adj <- gdistance::adjacencyFromTransition(open_diff_ts)
    
    open_diff_ts[open_adj] <- 90 - atan(open_diff_ts[open_adj])
    
    return(open_diff_ts)
    
}
