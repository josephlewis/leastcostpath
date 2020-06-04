#' Cost function to apply to rise-over-run slope
#' @param cost_function \code{character}. Cost Function used in the Least Cost Path calculation. Implemented cost functions include 'tobler', 'tobler offpath', 'irmischer-clarke male', 'irmischer-clarke offpath male', 'irmischer-clarke female', 'irmischer-clarke offpath female', 'modified tobler', 'wheeled transport', 'herzog', 'llobera-sluckin', 'all'. Default is 'tobler'.
#'
#' @noRd
#'
#' @author Joseph Lewis

cost <- function(cost_function, adj, crit_slope) {
    
    cfs <- c("tobler", "tobler offpath", "irmischer-clarke male", "irmischer-clarke offpath male", "irmischer-clarke female", "irmischer-clarke offpath female", 
        "modified tobler", "wheeled transport", "herzog", "llobera-sluckin")
    
    if (!cost_function %in% cfs) {
        stop("cost_function argument is invalid. See details for accepted cost functions")
    }
    
    if (cost_function == "tobler") {
        
        cf <- function(x) {
            (6 * exp(-3.5 * abs(x[adj] + 0.05)))
        }
        
    }
    
    if (cost_function == "tobler offpath") {
        
        cf <- function(x) {
            (6 * exp(-3.5 * abs(x[adj] + 0.05))) * 0.6
        }
        
    }
    
    if (cost_function == "irmischer-clarke male") {
        
        cf <- function(x) {
            (0.11 + exp(-(abs(x[adj]) * 100 + 5)^2/(2 * 30^2)))
        }
        
    }
    
    if (cost_function == "irmischer-clarke offpath male") {
        
        cf <- function(x) {
            (0.11 + 0.67 * exp(-(abs(x[adj]) * 100 + 2)^2/(2 * 30^2)))
        }
        
    }
    
    if (cost_function == "irmischer-clarke female") {
        
        cf <- function(x) {
            0.95 * (0.11 + exp(-(abs(x[adj]) * 100 + 5)^2/(2 * 30^2)))
        }
        
    }
    
    if (cost_function == "irmischer-clarke offpath female") {
        
        cf <- function(x) {
            0.95 * (0.11 + 0.67 * exp(-(abs(x[adj]) * 100 + 2)^2/(2 * 30^2)))
        }
        
    }
    
    if (cost_function == "modified tobler") {
        
        cf <- function(x) {
            (4.8 * exp(-5.3 * abs((x[adj] * 0.7) + 0.03)))
        }
        
    }
    
    if (cost_function == "wheeled transport") {
        
        cf <- function(x) {
            (1/(1 + abs(x[adj] * 100/crit_slope)^2))
        }
        
    }
    
    if (cost_function == "herzog") {
        
        cf <- function(x) {
            (1/((1337.8 * x[adj]^6) + (278.19 * x[adj]^5) - (517.39 * x[adj]^4) - (78.199 * x[adj]^3) + (93.419 * x[adj]^2) + (19.825 * x[adj]) + 1.64))
        }
        
    }
    
    if (cost_function == "llobera-sluckin") {
        
        cf <- function(x) {
            (1/(2.635 + (17.37 * abs(x[adj])) + (42.37 * abs(x[adj])^2) - (21.43 * abs(x[adj])^3) + (14.93 * abs(x[adj])^4)))
        }
        
    }
    
    
    return(cf)
    
}
