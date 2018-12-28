leastcostpath - version 0.1.2
=============================

R Implementation of Least Cost Path (LCP) Analysis. Provides functionality to create multiple LCPs using different cost functions based on slope and aspect. See details for more.

Getting Started
---------------

### Installing

    #install.packages("devtools")
    library(devtools)
    install_github("josephlewis/leastcostpath")
    library(leastcostpath)

### Key Features

-   Computes multiple LCPs using diferent cost functions allowing the user to assess which cost function produces the most accurate LCP (see `validation_buffer` for validation method).

-   Implements cost of movement uphill and downhill, as well as across. This functionality provides a true anisotropic cost surface and more realistically represents the difficulty of moving through a landscape.
-   LCP validation method implemented following Herzog (2013. 205), who stated that Without validation, LCP results are "mere guesswork".

### Implemented functions

`leastcostpath` provides the functionality to compute Least Cost Paths using multiple cost functions that approximate human movement across a landscape and user-defined horizontal factors (change in aspect degree).

Function requires a Digital Elevation Model (`RasterLayer` class) and and point features (`SpatialPoints` class) signifying the origin and destination of the LCP.

The following cost functions are implemented:

-   [Tobler's Hiking Function (1993)](http://escholarship.org/uc/item/05r820mz)

`6 * exp(-3.5 * abs(slope + 0.05))`

-   [Marquez-Perez et al. (2017) 'Modified Hiking function')](https://www.tandfonline.com/doi/abs/10.1080/00167223.2017.1316212)

`4.8 * exp(-5.3 * abs(slope * 0.7) + 0.03)`

-   [Llobera and Sluckin (2007) 'Fourth-degree polynomial function')](https://www.ncbi.nlm.nih.gov/pubmed/17892887)

`1/(1 + abs(slope/crit_slope)^2)`

Horizontal factor incorporated either through: \* Inverse Linear function&lt;<br> \* Binary<br> \* Forward<br>

see `leastcostpath` for more details on the calculation.

`validation_buffer` computes the accuracy of the LCP based on the method proposed by Goodchild and Hunter (1997).

-   Evaluates the similarity between two linear features by determining the percentage of a linear feature that lies within a buffer distance from the 'true' linear feature (ie. the percentage of the LCP within x buffer distance from a known road).

Feedback
--------

Please email josephlewis1992\[at\]gmail.com for feedback or functionality that you would like implemented.

Versioning
----------

-   version 0.1.0 - First release to Github
-   version 0.1.1 - Implemented choice of directionality
-   version 0.1.2 - Implemented horizontal factors that account for change in aspect in the landscape.

Authors
-------

-   Joseph Lewis - *author / creator* - [Website](https://josephlewis.github.io)

Citation
--------

Please cite as:

    Lewis, J. (2016) leastcostpath: R Implementation of Least Cost Path Analysis (version 0.1.2)
