leastcostpath - version 0.1.3
=============================

![](https://raw.githubusercontent.com/josephlewis/leastcostpath/master/images/leastcostpath_logo.png)

The R package <b>leastcostpath</b> provides the functionality to calculate Least Cost Paths (LCPs) which are often, but not exclusively, used in archaeological research. This package can be used to apply multiple cost functions when approximating the dififculty of moving across a landscape, as well as taking into account traversing across slope and other costs such as openness. This package also provides functionality to validate the accuracy of the computed LCP relative to another path. This package is built on classes and functions provided in the R package gdistance (Van Etten, 2017). 

Getting Started
---------------

### Installing

    #install.packages("devtools")
    library(devtools)
    install_github("josephlewis/leastcostpath")
    library(leastcostpath)


Feedback
--------

Please email josephlewis1992\[at\]gmail.com to provide your feedback or suggest functionality that you would like implemented.

Versioning
----------

-   version 0.1.0 - First release to Github
-   version 0.1.1 - Implemented choice of directionality
-   version 0.1.2 - Implemented cost when traversing across slope. 
-   version 0.1.3 - Implemented landscape feature attractions - linear decay rate
-   version 0.1.4 - Re-implemented functions so LCP process is broken down and more in line with traditional uses of LCP generation.

Authors
-------

-   Joseph Lewis - *author / creator* - [Website](https://josephlewis.github.io)

Citation
--------

Please cite as:

    Lewis, J. (2019) leastcostpath: R Implementation of Least Cost Path Analysis (version 0.1.4)
