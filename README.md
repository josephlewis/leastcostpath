leastcostpath - version 1.8.2 [![Build Status](https://travis-ci.org/josephlewis/leastcostpath.svg?branch=master)](https://travis-ci.org/josephlewis/leastcostpath)
[![CRAN status](https://www.r-pkg.org/badges/version/leastcostpath)](https://cran.r-project.org/package=leastcostpath)
[![CRAN Downloads Month](https://cranlogs.r-pkg.org/badges/leastcostpath)](https://cranlogs.r-pkg.org/badges/leastcostpath)
[![CRAN Downloads Total](https://cranlogs.r-pkg.org/badges/grand-total/leastcostpath)](https://cranlogs.r-pkg.org/badges/grand-total/leastcostpath)
=============================

The R library <b>leastcostpath</b> provides the functionality to calculate Cost Surfaces based on multiple cost functions that approximate the difficulty of moving across a landscape. Furthermore, the attraction/repulsion of landscape features can be incorporated into the Cost Surfaces, as well as barriers that inhibit movement.

**NOTE:** The R library <b>leastcostpath</b> requires the use of projected coordinate systems. The package does not account for geographic coordinate systems.

Cost Surfaces can be used to calculate Least Cost Paths, which are often, but not exclusively, used in archaeological research. <b>leastcostpath</b> also provides the functionality to calculate movement potential within a landscape through the implementation of From-Everywhere-to-Everywhere (FETE) (White and Barber, 2012), Cumulative Cost Paths (Verhagen, 2013), and Least Cost Path calculation within specified distance bands (Llobera, 2015). Furthermore, the library allows for the calculation of stochastic least cost paths and wide least cost paths.

Lastly, the library provides functionality to validate the accuracy of computed Least Cost Paths relative to another path. 

This package is built on classes and functions provided in the R package gdistance (Van Etten, 2017). 

*Functions currently in development:*

*Functions recently added:*
* force_isotropy()
* create_distance_cs()

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
    slope_cs_exagg <- create_slope_cs(r, cost_function = 'tobler', exaggeration = TRUE)
    
    distance_cs <- create_distance_cs(r, neighbours = 16)
    
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
    
#### Probabilistic Least Cost Path

    locs <- sp::spsample(as(raster::extent(r), 'SpatialPolygons'),n=2,'random')

    RMSE <- 5
    n <- 10
    lcps <- list()
    
    for (i in 1:n) {
    
    lcps[[i]] <- leastcostpath::create_lcp(cost_surface = leastcostpath::create_slope_cs(dem = leastcostpath::add_dem_error(dem = r, rmse = RMSE, size = "auto", vgm_model = "Sph"), cost_function = "tobler", neighbours = 16), origin = locs[1,], destination = locs[2,], directional = FALSE, cost_distance = TRUE)
    
    }
    
    lcps <- do.call(rbind, lcps)
    
#### Wide Least Cost Path

    n <- 3

    slope_cs <- create_slope_cs(r, cost_function = 'tobler', neighbours = wide_path_matrix(n))

    loc1 = cbind(2667670, 6479000)
    loc1 = sp::SpatialPoints(loc1)

    loc2 = cbind(2667800, 6479400)
    loc2 = sp::SpatialPoints(loc2)

    lcps <- create_wide_lcp(cost_surface = slope_cs, origin = loc1,
    destination = loc2, path_ncells = n)
    
Common Errors
---------------

    Error in if (is.numeric(v) && any(v < 0)) { : 
    missing value where TRUE/FALSE needed
    
Error caused when trying to calculate a Least Cost Path using SpatialPoints outside of the Cost Surface Extent
  * Check SpatialPoints used in the LCP calculation coincide with Raster / Cost Surface
  * Check coordinate systems of the Raster/Cost Surface is the same as the SpatialPoints
  

    Error in get.shortest.paths(adjacencyGraph, indexOrigin, indexGoal):
    At structural_properties.c:4521 :
    Weight vector must be non-negative, Invalid value
    
Error caused when calculating a Least Cost Path using a  Cost Surface that contains negative values. Error due to Djikstra's algorithm requiring non-negative values
  * Check if there are negative values via: 
  
    
    quantile(*your_cost_surface*@transitionMatrix@x)
      
Contributing
--------

If you would like to contribute to the R Package <b>leastcostpath</b>, please follow the "fork-and-pull" Git workflow:

1. <b>Fork</b> the rep on Github
2. <b>Clone</b> the project to your own machine
3. <b>Commit</b> the changes to your own branch
4. <b>Push</b> your work back to your fork
5. Submit a <b>pull request</b> so that the changes can be reviewed

Issues
--------

Please submit issues and enhancement requests via github Issues
  * If submitting an issue, please clearly describe the issue, including steps to reproduce when it is a bug, or a justification for the proposed enhancement request


Case Studies Using _leastcostpath_
--------

Lewis, J., 2021. Probabilistic Modelling for Incorporating Uncertainty in Least Cost Path Results: a Postdictive Roman Road Case Study. Journal of Archaeological Method and Theory. https://doi.org/10.1007/s10816-021-09522-w

Lewis, J. Probabilistic Modelling using Monte Carlo Simulation for Incorporating Uncertainty in Least Cost Path Results: a Roman Road Case Study, Peer Community in Archaeology, 100005. [10.24072/pci.archaeo.100005](https://doi.org/10.24072/pci.archaeo.100005)

Ludwig, B. Reconstructing the Ancient Route Network in Pergamon's Surroundings. Land 2020, 9, 241. [https://doi.org/10.3390/land9080241](https://doi.org/10.3390/land9080241)
    
Versioning
----------

See NEWS.md for a summary of Version updates

Authors
-------

-   Joseph Lewis - *author / creator* - [Website](https://josephlewis.github.io)

Citation
--------

Please cite as:

    Lewis, J. (2021) leastcostpath: Modelling Pathways and Movement Potential Within a Landscape (version 1.8.2). 
    Available at: https://cran.r-project.org/web/packages/leastcostpath/index.html
