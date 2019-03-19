# Version 0.1.4 (19 March 2019)
* Split leastcostpath function into separate functions - allows for the building up of cost surfaces before computating least cost path.
  * create_slope_cs
  * create_traversal_cs
  * create_openness_cs
* Addition of openness cost surface creation.


-   version 0.1.0 - First release to Github
-   version 0.1.1 - Implemented choice of directionality
-   version 0.1.2 - Implemented cost when traversing across slope. 
-   version 0.1.3 - Implemented landscape feature attractions - linear decay rate
-   version 0.1.4 - Re-implemented functions so LCP process is broken down and more in line with traditional uses of LCP generation.
