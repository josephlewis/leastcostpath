leastcostpath - version 0.1.3
=============================

![](https://raw.githubusercontent.com/josephlewis/leastcostpath/master/images/leastcostpath_logo.png)

The R package <b>leastcostpath</b> provides functions to calculate Least Cost Paths (LCPs) for archaeological application. This package applies multiple cost functions when approximating the dififculty of moving across a landscape, as well as taking into account traversing across slope and other costs such as landscape feature attraction. This package also provides a function to validate the accuracy of the computed LCP relative to another path. This package is built on classes and functions provided in the R package gdistance (Van Etten, 2017). 

Getting Started
---------------

Installing
--------

    # install.packages("devtools")
    library(devtools)
    install_github("josephlewis/leastcostpath")
    library(leastcostpath)

Usage
--------

#### Computation of Least Cost Path

    library(leastcostpath)
    
    r <- raster::raster(system.file('external/maungawhau.grd', package = 'gdistance'))
    
    loc1 = sp::SpatialPoints(cbind(2667670, 6479000))
    
    loc2 =  sp::SpatialPoints(cbind(2667800, 6479400))
    
    leastcostpath(dem = r, origin = loc1, destination = loc2, traverse = "asymmetrical")
    
#### Creation of Landacape Feature Attraction

    landscape_features <- sp::SpatialPoints(cbind(c(2667775, 2667652), c(6479191, 6479237)))
    
    lfa <- feature_attraction(r, landscape_features, viewshed = NULL, 
              decay = "linear", decay_rate = c(5, 500), suffix = "")
    
#### Incorporating Landscape Feature Attractions into Least Cost Path calculation

    leastcostpath(dem = r, origin = loc1, destination = loc2, traverse = "asymmetrical", other_costs = lfa)
    
#### Least Cost Path validation against another SpatialLines object
    
    x1 <- c(1,5,4,8)
    y1 <- c(1,3,4,7)
    line1 <- sp::SpatialLines(list(sp::Lines(sp::Line(cbind(x1,y1)), ID='a')))

    x2 <- c(1,5,5,8)
    y2 <- c(1,4,6,7)
    line2 <- sp::SpatialLines(list(sp::Lines(sp::Line(cbind(x2,y2)), ID='b')))

    validation_buffer(lcp = line1, comparison = line2, buffers = c(0.1, 0.2, 0.5, 1))
  
Feedback
--------

Please email josephlewis1992[at]gmail.com to provide your feedback or suggest functionality that you would like implemented.

Versioning
----------

-   version 0.1.0 - First release to Github
-   version 0.1.1 - Implemented choice of directionality
-   version 0.1.2 - Implemented cost when traversing across slope. 
-   version 0.1.3 - Implemented landscape feature attractions - linear decay rate

Authors
-------

-   Joseph Lewis - *author / creator* - [Website](https://josephlewis.github.io)

Citation
--------

Please cite as:

    Lewis, J. (2018) leastcostpath: R Implementation of Least Cost Path Analysis (version 0.1.3)