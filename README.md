leastcostpath - version 0.1.6
=============================

![](https://raw.githubusercontent.com/josephlewis/leastcostpath/master/images/leastcostpath_logo.png)

The R package <b>leastcostpath</b> provides the functionality to calculate Least Cost Paths (LCPs) which are often, but not exclusively, used in archaeological research. This package can be used to apply multiple cost functions when approximating the dififculty of moving across a landscape, as well as taking into account traversing across slope and other costs such as openness. This package also provides functionality to validate the accuracy of the computed LCP relative to another path. This package is built on classes and functions provided in the R package gdistance (Van Etten, 2017). 

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

#### Least Cost Path computation using created Cost Surfaces

    loc1 = cbind(2667670, 6479000)
    loc1 = sp::SpatialPoints(loc1)
 
    loc2 = cbind(2667800, 6479400)
    loc2 = sp::SpatialPoints(loc2)

    lcps <- create_lcp(cost_surface = final_cost_cs, origin = loc1, destination = loc2, directional = TRUE)
  
    plot(raster(final_cost_cs))
    plot(lcps[[1]], add = T)
    
#### Cost Corridors

  cc <- create_cost_corridor(final_cost_cs, loc1, loc2)
  
  plot(cc)
  plot(loc1, add = T)
  plot(loc2, add = T)


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
      * Implemented cost when traversing across slope. 
-   version 0.1.3 
      * Implemented landscape feature attractions - linear decay rate
-   version 0.1.4 
      * Re-implemented functions so LCP process is broken down and more in line with traditional logic of LCP generation.
      * Removal of landscape feature attraction function - this will be re-added at a later date. 
-   version 0.1.5 
      * Addition of create_cost_corridor function. 
      * Removal of validate_lcp, create_openness, and create_lcp_network - these will be re-added at a later date.

Authors
-------

-   Joseph Lewis - *author / creator* - [Website](https://josephlewis.github.io)

Citation
--------

Please cite as:

    Lewis, J. (2019) leastcostpath: R Implementation of Least Cost Path Analysis (version 0.1.5)
