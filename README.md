leastcostpath - version 1.7.4 [![Build Status](https://travis-ci.org/josephlewis/leastcostpath.svg?branch=master)](https://travis-ci.org/josephlewis/leastcostpath)
[![CRAN status](https://www.r-pkg.org/badges/version/leastcostpath)](https://cran.r-project.org/package=leastcostpath)
[![CRAN Downloads Month](https://cranlogs.r-pkg.org/badges/leastcostpath)](https://cranlogs.r-pkg.org/badges/leastcostpath)
[![CRAN Downloads TOtal](https://cranlogs.r-pkg.org/badges/grand-total/leastcostpath)](https://cranlogs.r-pkg.org/badges/grand-total/leastcostpath)
=============================

The R library <b>leastcostpath</b> provides the functionality to calculate Cost Surfaces based on multiple cost functions that approximate the difficulty of moving across a landscape. Furthermore, the attraction/repulsion of landscape features can be incorporated into the Cost Surfaces, as well as barriers that inhibit movement. 

Cost Surfaces can be used to calculate Least Cost Paths, which are often, but not exclusively, used in archaeological research. <b>leastcostpath</b> also provides the functionality to calculate movement potential within a landscape through the implementation of From-Everywhere-to-Everywhere (FETE) (White and Barber, 2012), Cumulative Cost Paths (Verhagen, 2013), and Least Cost Path calculation within specified distance bands (Llobera, 2015). Furthermore, the library allows for the calculation of stochastic least cost paths and wide least cost paths.

Lastly, the library provides functionality to validate the accuracy of computed Least Cost Paths relative to another path. 

This package is built on classes and functions provided in the R package gdistance (Van Etten, 2017). 

*Functions currently in development:*

* force_isotropy()

*Functions recently added:*

* crop_cs()
* PDI_validation()
* add_dem_error()

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
    
    slope_cs_10 <- create_slope_cs(r, cost_function = 'tobler', max_slope = 10)
    
#### Least Cost Path computation

    loc1 = cbind(2667670, 6479000)
    loc1 = sp::SpatialPoints(loc1)
 
    loc2 = cbind(2667800, 6479400)
    loc2 = sp::SpatialPoints(loc2)

    lcps <- create_lcp(cost_surface = slope_cs, origin = loc1, destination = loc2, directional = FALSE)
  
    plot(raster(slope_cs))
    plot(lcps[1,], add = T, col = "red") # location 1 to location 2
    plot(lcps[2,], add = T, col = "blue") # location 2 to location 1
    
#### Cost Corridors

    cc <- create_cost_corridor(slope_cs, loc1, loc2)
    
    plot(cc)
    plot(loc1, add = T)
    plot(loc2, add = T)
    
#### From-Everywhere-to-Everywhere Least Cost Paths

    locs <- sp::spsample(as(raster::extent(r), 'SpatialPolygons'),n=10,'regular')
    
    lcp_network <- create_FETE_lcps(cost_surface = slope_cs, locations = locs,
    cost_distance = FALSE, parallel = FALSE)
    
    plot(raster(slope_cs))
    plot(locs, add = T)
    plot(lcp_network, add = T)
    
#### Cumulative Cost Paths

    locs <- sp::spsample(as(raster::extent(r), 'SpatialPolygons'),n=1,'random')

    lcp_network <- create_CCP_lcps(cost_surface = slope_cs, location = locs, distance = 50,
    radial_points = 10, cost_distance = FALSE, parallel = FALSE)
    
    plot(raster(slope_cs))
    plot(locs, add = T)
    plot(lcp_network, add = T)
    
#### Banded Least Cost Paths

    locs <- sp::spsample(as(raster::extent(r), 'SpatialPolygons'),n=1,'random')

    lcp_network <- create_banded_lcps(cost_surface = slope_cs, location = locs, min_distance = 20,
    max_distance = 50, radial_points = 10, cost_distance = FALSE, parallel = FALSE)
    
    plot(raster(slope_cs))
    plot(locs, add = T)
    plot(lcp_network, add = T)

#### Least Cost Path Density

    cumulative_lcps <- create_lcp_density(lcps = lcp_network, raster = r, rescale = FALSE)

    plot(cumulative_lcps)
    
#### Least Cost Path Network

    locs <- sp::spsample(as(raster::extent(r), 'SpatialPolygons'),n=5,'regular')
    
    mat <- cbind(c(1, 4, 2, 1), c(2, 2, 4, 3))
    
    lcp_network <- create_lcp_network(slope_cs, locations = locs, 
    nb_matrix = mat, cost_distance = FALSE, parallel = FALSE)
    
#### Stochastic Least Cost Path

    locs <- sp::spsample(as(raster::extent(r), 'SpatialPolygons'),n=2,'random')

    stochastic_lcp <- replicate(n = 10, create_stochastic_lcp(cost_surface = slope_cs,
    origin = locs[1,], destination = locs[2,], directional = FALSE))
    
    stochastic_lcp <- do.call(rbind, stochastic_lcp)

#### Wide Least Cost Path

    n <- 3

    slope_cs <- create_slope_cs(r, cost_function = 'tobler', neighbours = wide_path_matrix(n))

    loc1 = cbind(2667670, 6479000)
    loc1 = sp::SpatialPoints(loc1)

    loc2 = cbind(2667800, 6479400)
    loc2 = sp::SpatialPoints(loc2)

    lcps <- create_wide_lcp(cost_surface = slope_cs, origin = loc1,
    destination = loc2, path_ncells = n)

#### Pipes!

    cost_surface <- create_slope_cs(r, cost_function = 'tobler') %>%
    "*" (create_traversal_cs(r)) %>%
    "*" (create_feature(raster = r, locations = loc1, x = seq(200, 1, length.out = 20))
    
    lcp <- cost_surface %>% 
    create_lcp(cost_surface = . loc1, loc2)
    
    cost_corridor <- cost_surface %>% 
    create_cost_corridor(., loc1, loc2)
    
    locs <- sp::spsample(as(extent(r), 'SpatialPolygons'),n=10,'regular')
    
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
      * Fixed create_traversal_cs to work when max_slope in create_slope_cs used
-   version 1.3.4
      * create_lcp returns SpatialLines rather than list
      * Added cost_distance function argument to create_lcp
-   version 1.3.5
      * Implemented create_barrier_cs
-   version 1.3.6
      * Implemented cost_matrix to be used with the create_lcp_network nb_matrix argument
      * Fixed error in create_slope_cs when using 16 neighbours
-   version 1.3.7
      * Fixed neighbour issue with cost_matrix 
-   version 1.3.8
      * Least Cost Paths created using the create_FETE_lcps function are automically ordered low-to-high cost if the cost_distance argument is TRUE 
      * Least Cost Paths created using the create_CCP_lcps are automically ordered low-to-high cost if the cost_distance argument is TRUE 
      * Least Cost Paths created using the create_banded_lcps are automically ordered low-to-high cost if the cost_distance argument is TRUE 
-   version 1.3.9
      * Implemented create_stochastic_lcp. Based on the method proposed by Pinto and Keitt (2009). See function documentation for reference.
-   version 1.4.0
      * Refactored code to enable greater flexibility
      * Added 32 and 48 neighbour matrices based on [Kovanen and Sarjakoski (2015)](https://doi.org/10.1145/2803172) (see leastcostpath::neighbours_32 and leastcostpath::neighbours_48 for their layout)
      * Implemented create_wide_lcp. Inspired by [Shirabe (2016)](https://doi.org/10.1080/13658816.2015.1124435)
      * Removed 'all' cost_function argument from create_slope_cs. 
-   version 1.4.1
      * Fixed create_stochastic_lcp when directional = FALSE. Now checks to ensure both Least Cost Paths are valid.
-   version 1.5.1
      * Implemented crop_cs. See function documentation for more information and usage
-   version 1.6.1
      * Implemented PDI_validation. See function documentation for more information and usage
-   version 1.6.2
      * Modified how lcp_density calculates which cells are covered by least cost paths. Instead of rasterising points (which can result in             cells being missed), the least cost paths themselves are rasterised. 
      * Amended PDI_validation function to return area between lines
-   version 1.7.2
      * Implemented add_dem_error. See function documentation for more information and usage
-   version 1.7.3
      * Implemented type 'autocorrelated' and the calculation of probability intervals in add_dem_error
-   version 1.7.4
      * Allow for the rasterisation of SpatialLines in create_lcp_density through the rasterize_as_points argument. If FALSE, SpatialLines are rasterised. If TRUE, SpatialLines converted to SpatialPoints and rasterised. See function documentation for more information.
      
Authors
-------

-   Joseph Lewis - *author / creator* - [Website](https://josephlewis.github.io)

Citation
--------

Please cite as:

    Lewis, J. (2020) leastcostpath: Modelling Pathways and Movement Potential Within a Landscape (version 1.7.4). 
    Available at: https://cran.r-project.org/web/packages/leastcostpath/index.html
