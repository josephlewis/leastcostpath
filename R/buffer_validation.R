#' Calculate the similarity of a least-cost path to a known route
#' 
#' Calculates the similarity of a least-cost path to a known route using the buffer method proposed by Goodchild and Hunter (1997)
#'  
#' @param lcp \code{sf} 
#' 
#' @param comparison \code{sf}
#' 
#' @param dist \code{numeric} buffer distances to assess similarity
#' 
#' @return \code{data.frame}
#' 
#' @author Joseph Lewis
#' 
#' @export

buffer_validation <- function(lcp, comparison, dist) { 
  
  val_table <- foreach::foreach(dist_no = 1:length(dist), .combine = "rbind") %do% { 
    
    compar_buffer <- sf::st_buffer(x = comparison, dist = dist[dist_no])
    lcp_intersect <-  suppressWarnings(sf::st_intersection(x = compar_buffer, y = lcp))
    similarity <- (sf::st_length(lcp_intersect) / sf::st_length(lcp)) * 100
    
    similarity_df <- data.frame(ID = dist_no, dist = dist[dist_no], similarity = as.numeric(similarity))
    
    return(similarity_df)
  }
  
  return(val_table)
}
