leastcostpath - version 1.2.4 [![Build Status](https://travis-ci.org/josephlewis/leastcostpath.svg?branch=master)](https://travis-ci.org/josephlewis/leastcostpath)
[![CRAN status](https://www.r-pkg.org/badges/version/leastcostpath)](https://cran.r-project.org/package=leastcostpath)
[![CRAN Downloads Month](https://cranlogs.r-pkg.org/badges/leastcostpath)](https://cranlogs.r-pkg.org/badges/leastcostpath)
[![CRAN Downloads TOtal](https://cranlogs.r-pkg.org/badges/grand-total/leastcostpath)](https://cranlogs.r-pkg.org/badges/grand-total/leastcostpath)
=============================

The R library <b>leastcostpath</b> provides the functionality to calculate Least Cost Paths, which are often, but not exclusively, used in archaeological research. This library can be used to apply multiple cost functions when approximating the difficulty of moving across a landscape, as well as incorporating traversal <i>across</i> slope. Furthermore, attraction/repulsion of landscape features can be incorporated within the Least Cost Path calculation.

This library also provides the functionality to calculate movement potential within a landscape through the implementation of From-Everywhere-to-Everywhere (FETE) (White and Barber, 2012), Cumulative Cost Paths (Verhagen, 2013), and Least Cost Path calculation within specified distance bands (Llobera, 2015). 

Lastly, the library provides functionality to validate the accuracy of computed Least Cost Paths relative to another path. 

This package is built on classes and functions provided in the R package gdistance (Van Etten, 2017). 

Getting Started
---------------

Installation
--------

    #install.packages("devtools")
    library(devtools)
    install_github("josephlewis/leastcostpath")
    library(leastcostpath)

Usage
--------

#### Creation of Cost Surfaces

    library(leastcostpath)
    r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
        
    slope_cs <- create_slope_cs(r, cost_function = 'tobler')
    traverse_cs <- create_traversal_cs(r)
    final_cost_cs <- slope_cs * traverse_cs

#### Least Cost Path computation

    loc1 = cbind(2667670, 6479000)
    loc1 = sp::SpatialPoints(loc1)
 
    loc2 = cbind(2667800, 6479400)
    loc2 = sp::SpatialPoints(loc2)

    lcps <- create_lcp(cost_surface = final_cost_cs, origin = loc1, destination = loc2, directional = FALSE)
  
    plot(raster(final_cost_cs))
    plot(lcps[[1]], add = T, col = "red") # location 1 to location 2
    plot(lcps[[2]], add = T, col = "blue") # location 2 to location 1
    
#### Cost Corridors

    cc <- create_cost_corridor(final_cost_cs, loc1, loc2)
    
    plot(cc)
    plot(loc1, add = T)
    plot(loc2, add = T)
    
#### From-Everywhere-to-Everywhere Least Cost Paths

    locs <- spsample(as(r, 'SpatialPolygons'),n=10,'regular')
    
    lcp_network <- create_FETE_lcps(cost_surface = final_cost_cs, locations = locs,
    cost_distance = FALSE, parallel = FALSE)
    
    plot(raster(final_cost_cs))
    plot(locs, add = T)
    plot(lcp_network, add = T)
    
#### Cumulative Cost Paths

    locs <- sp::spsample(as(r, 'SpatialPolygons'),n=1,'random')

    lcp_network <- create_CCP_lcps(cost_surface = final_cost_cs, location = locs, distance = 50,
    radial_points = 10, cost_distance = FALSE, parallel = FALSE)
    
    plot(raster(final_cost_cs))
    plot(locs, add = T)
    plot(lcp_network, add = T)
    
#### Banded Least Cost Paths

    locs <- sp::spsample(as(r, 'SpatialPolygons'),n=1,'random')

    lcp_network <- create_banded_lcps(cost_surface = final_cost_cs, location = locs, min_distance = 20,
    max_distance = 50, radial_points = 10, cost_distance = FALSE, parallel = FALSE)
    
    plot(raster(final_cost_cs))
    plot(locs, add = T)
    plot(lcp_network, add = T)

#### Least Cost Path Density

    cumulative_lcps <- create_lcp_density(lcps = lcp_network, raster = r, rescale = FALSE)

    plot(cumulative_lcps)
    
#### Pipes!

    cost_surface <- create_slope_cs(r, cost_function = 'tobler') %>%
    "*" (create_traversal_cs(r)) %>%
    "*" (create_feature(raster = r, locations = loc1, x = seq(200, 1, length.out = 20))
    
    lcp <- cost_surface %>% 
    create_lcp(cost_surface = . loc1, loc2)
    
    cost_corridor <- cost_surface %>% 
    create_cost_corridor(., loc1, loc2)
    
    locs <- sp::spsample(as(r, 'SpatialPolygons'),n=10,'regular')
    
    lcp_network <- cost_surface %>% 
    create_FETE_lcps(cost_surface = final_cost_cs, locations = locs,cost_distance = FALSE, parallel = FALSE)
    
    cumulative_cost_paths <- cost_surface %>% 
    create_FETE_lcps(cost_surface = final_cost_cs, locations = locs,cost_distance = FALSE, parallel = FALSE) %>%
    create_cumulative_lcps(lcps = ., raster = r, rescale = FALSE)

Feedback
--------

Please email josephlewis1992\[at\]gmail.com to provide your feedback or suggest functionality that you would like implemented.

Versioning
----------

-   version 0.1.0
      * First release to Github
-   version 0.1.1 
      * Implemented choice of directionality
-   version 0.1.2 
      * Implemented cost when traversing across slope
-   version 0.1.3 
      * Implemented landscape feature attractions - linear decay rate
-   version 0.1.4 
      * Re-implemented functions so LCP process is broken down and more in line with traditional logic of LCP generation.
      * Removal of landscape feature attraction function - this will be re-added at a later date
-   version 0.1.5 
      * Addition of create_cost_corridor function. 
      * Removal of validate_lcp, create_openness, and create_lcp_network - these will be re-added at a later date
-   version 0.1.6
      * Addition of create_feature_attraction
      * Improved readability of create_traversal_slope function 
-   version 1.0.0
      * Removed create_feature_attraction and replaced with create_feature_cs. 
      * Re-implemented create_lcp_network. Provides parallel and non-parallel functionality
-   version 1.1.0
      * Added create_cumulative_lcps function for the creation of cumulative least cost path rasters 
-   version 1.2.0
      * Renamed create_lcp_network to create_FETE_lcps for consistency with academic literature
      * Implemented create_CCP_lcps
      * Implemented create_banded_lcps
-   version 1.2.1
      * Maximum slope traversable argument added to create_slope_cs function
-   version 1.2.2
      * Implemented create_lcp_network
-   version 1.2.3
      * Added create_lcp_network vignette
      * Fixed create_lcp_density issue where raster values of 1 would become 0 when rescale is TRUE      
      * Addded Tobler offpath cost function
      * Added irmischer-clarke male/female cost function
      * Added irmischer-clarke offpath male/female cost function
      * Added llobera-sluckin cost function
      * Refactored create_slope_cs for easier scalability
-   version 1.2.4
      * Fixed max_slope in create_slope_cs function.
      * Fixed create_traversal_cs to work when max_slope in create_slope_cs used. 

Authors
-------

-   Joseph Lewis - *author / creator* - [Website](https://josephlewis.github.io)

Citation
--------

Please cite as:

    Lewis, J. (2020) leastcostpath: Modelling Pathways and Movement Potential Within a Landscape (version 1.2.4)
