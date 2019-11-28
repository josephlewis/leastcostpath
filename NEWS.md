# 0.1.6 (28 November 2019)
* Improved readability of create_traversal_slope function
* Addition of create_feature_attraction

# 0.1.5 (21 September 2019)
* Addition of create_cost_corridor function
* Removal of validate_lcp, create_openness, and create_lcp_network - these will be re-added at a later date.

# Version 0.1.4 (19 March 2019)
* Split `leastcostpath` function into separate functions - allows for the building up of cost surfaces before computating least cost path.
  * `create_slope_cs`
  * `create_traversal_cs`
  * `create_openness_cs`
* Addition of openness cost surface creation.

# version 0.1.2 - Implemented cost when traversing across slope. 
# version 0.1.1 - Implemented choice of directionality
# version 0.1.0 - First release to Github
