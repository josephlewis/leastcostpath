#' Cost function to apply to rise-over-run slope
#' @param cost_function \code{character}. Cost Function used in the Least Cost Path calculation. Implemented cost functions include 'tobler', 'tobler offpath', 'irmischer-clarke male', 'irmischer-clarke offpath male', 'irmischer-clarke female', 'irmischer-clarke offpath female', 'modified tobler', 'wheeled transport', 'herzog', 'llobera-sluckin', 'campbell 2019'. Default is 'tobler'.
#'
#' @noRd
#'
#' @author Joseph Lewis

cost <- function(cost_function, adj, crit_slope, percentile) {
    
    cfs <- c("tobler", "tobler offpath", "irmischer-clarke male", "irmischer-clarke offpath male", "irmischer-clarke female", "irmischer-clarke offpath female", 
        "modified tobler", "wheeled transport", "herzog", "llobera-sluckin", "campbell 2019")
    
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
            (0.11 + exp(-(abs(x[adj]) * 100 + 5)^2/(2 * 30^2))) * 3.6
        }
        
    }
    
    if (cost_function == "irmischer-clarke offpath male") {
        
        cf <- function(x) {
            (0.11 + 0.67 * exp(-(abs(x[adj]) * 100 + 2)^2/(2 * 30^2))) * 3.6
        }
        
    }
    
    if (cost_function == "irmischer-clarke female") {
        
        cf <- function(x) {
            0.95 * (0.11 + exp(-(abs(x[adj]) * 100 + 5)^2/(2 * 30^2))) * 3.6
        }
        
    }
    
    if (cost_function == "irmischer-clarke offpath female") {
        
        cf <- function(x) {
            0.95 * (0.11 + 0.67 * exp(-(abs(x[adj]) * 100 + 2)^2/(2 * 30^2))) * 3.6
        }
        
    }
    
    if (cost_function == "modified tobler") {
        
        cf <- function(x) {
            (4.8 * exp(-5.3 * abs((x[adj] * 0.7) + 0.03)))
        }
        
    }
    
    if (cost_function == "wheeled transport") {
        
        cf <- function(x) {
            (1/(1 + ((abs(x[adj]) * 100)/crit_slope)^2))
        }
        
    }
    
    if (cost_function == "herzog") {
        
        cf <- function(x) {
            
            (1/((1337.8 * abs(x[adj])^6) + (278.19 * abs(x[adj])^5) - (517.39 * abs(x[adj])^4) - (78.199 * abs(x[adj])^3) + (93.419 * abs(x[adj])^2) + 
                (19.825 * abs(x[adj])) + 1.64))
            
        }
        
    }
    
    if (cost_function == "llobera-sluckin") {
        
        cf <- function(x) {
            (1/(2.635 + (17.37 * abs(x[adj])) + (42.37 * abs(x[adj])^2) - (21.43 * abs(x[adj])^3) + (14.93 * abs(x[adj])^4)))
        }
        
    }
    
    if (cost_function == "campbell 2019") {
        
        percentile_choice <- c(0.01, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.99)
        
        if (!percentile %in% percentile_choice) {
            stop("percentile argument is invalid. Expecting percentile value of 0.01, 0.05, 0.10, 0.15, 0.20,
        0.25, 0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.65, 0.70, 0.75, 0.80, 0.85, 0.90, 0.95 or 0.99")
        }
        
        lorentz_function_terms <- data.frame(percentile = percentile_choice, term_a = c(-2.1, -1.527, -1.568, -1.626, -1.71, -1.822, -1.858, -1.891, -1.958, 
            -2.05, -2.171, -2.317, -2.459, -2.647, -2.823, -3.067, -3.371, -3.661, -3.06, -3.485, -4), term_b = c(12.273, 14.041, 13.328, 11.847, 10.154, 
            8.827, 8.412, 8.584, 8.96, 9.402, 10.064, 10.712, 11.311, 12.089, 12.784, 13.888, 15.395, 17.137, 16.653, 17.033, 13.903), term_c = c(21.816, 
            36.813, 38.892, 38.231, 36.905, 37.111, 39.995, 44.852, 50.34, 56.172, 63.66, 71.572, 79.287, 89.143, 98.697, 113.655, 134.409, 159.027, 138.875, 
            138.04, 123.515), term_d = c(0.263, 0.32, 0.404, 0.481, 0.557, 0.616, 0.645, 0.649, 0.649, 0.646, 0.628, 0.608, 0.599, 0.576, 0.566, 0.518, 
            0.443, 0.385, 0.823, 1.179, 1.961), term_e = c(-0.00193, -0.00273, -0.00323, -0.00356, -0.00389, -0.00402, -0.0043, -0.00443, -0.00457, -0.0046, 
            -0.00463, -0.00451, -0.00461, -0.00465, -0.00493, -0.00488, -0.00472, -0.00534, -0.01386, -0.01252, -0.01081))
        
        terms <- lorentz_function_terms[lorentz_function_terms$percentile == percentile, ]
        
        cf <- function(x) {
            ((terms$term_c/((pi * terms$term_b) * (1 + (atan(x[adj]) * 180/pi - terms$term_a/terms$term_b)^2))) + terms$term_d + terms$term_e * atan(x[adj]) * 
                180/pi) * 3.6
            
        }
    }
    
    # if (cost_function == 'hernandez') { cf <- function(x) { ((0.031 * atan(x[adj])^2) + (-0.025 * atan(x[adj]) + 1)) } }
    
    return(cf)
    
}
