# 1.4.1 (5th June 2020)
* Fixed create_stochastic_lcp when directional = FALSE. Now checks to ensure both Least Cost Paths are valid. 

# 1.4.0 (4th June 2020)
* Refactored code to enable greater flexibility
* Added 32 and 48 neighbour matrices based on [Kovanen and Sarjakoski (2015)](https://doi.org/10.1145/2803172) (see leastcostpath::neighbours_32 and leastcostpath::neighbours_48 for their layout)
* Implemented create_wide_lcp. Inspired by [Shirabe (2016)](https://doi.org/10.1080/13658816.2015.1124435)
* Removed 'all' cost_function argument from create_slope_cs. 

# 1.3.9 (1st June 2020)
* Implemented create_stochastic_lcp. Based on the method proposed by Pinto and Keitt (2009). See function documentation for reference

# 1.3.8 (1st June 2020)
* Least Cost Paths created using the create_FETE_lcps are automically ordered low-to-high cost if the cost_distance argument is TRUE 
* Least Cost Paths created using the create_CCP_lcps are automically ordered low-to-high cost if the cost_distance argument is TRUE 
* Least Cost Paths created using the create_banded_lcps are automically ordered low-to-high cost if the cost_distance argument is TRUE 

# 1.3.7 (15th May 2020)
* Fixed neighbour issue with cost_matrix 

# 1.3.6 (13th May 2020)
* Implemented cost_matrix to be used with the create_lcp_network nb_matrix argument 
* Fixed error in create_slope_cs when using 16 neighbours

# 1.3.5 (11th May 2020)
* Implemented create_barrier_cs

# 1.3.4 (4th May 2020)
* create_lcp returns SpatialLines rather than list
* Added cost_distance function argument to create_lcp

# 1.2.4 (29th April 2020)
* Fixed max_slope in create_slope_cs function
* Fixed create_traversal_cs to work when max_slope in create_slope_cs used 

# 1.2.3 (28 April 2020)
* Added create_lcp_network vignette
* Fixed create_lcp_density issue where raster values of 1 would become 0 when rescale is TRUE
* Addded Tobler's offpath cost function
* Added irmischer-clarke male/female cost function
* Added irmischer-clarke offpath male/female cost function
* Added llobera-sluckin cost function
* Refactored create_slope_cs for easier scalability

# 1.2.2 (01 April 2020)
* Implemented create_lcp_network

# 1.2.1 (18 March 2020)
* Maximum slope traversable argument added to create_slope_cs function

# 1.2.0 (21 February 2020)
* Renamed create_lcp_network to create_FETE_lcps for consistency with academic literature
* Implemented create_CCP_lcps
* Implemented create_banded_lcps

# 1.1.0 (17 February 2020)
* Added create_cumulative_lcps function for the creation of cumulative least cost path rasters

# 1.0.0 (14 February 2020)
* Removed create_feature_attraction and replaced with create_feature_cs.
* Re-implemented create_lcp_network. Provides parallel and non-parallel functionality

# 0.1.6 (28 November 2019)
* Improved readability of create_traversal_slope function
* Addition of create_feature_attraction

# 0.1.5 (21 September 2019)
* Addition of create_cost_corridor function
* Removal of validate_lcp, create_openness, and create_lcp_network - these will be re-added at a later date.

# Version 0.1.4 (19 March 2019)
* Split `leastcostpath` function into separate functions - allows for the building up of cost surfaces before computating least cost path
  * `create_slope_cs`
  * `create_traversal_cs`
  * `create_openness_cs`
  * Addition of openness cost surface creation

# version 0.1.2 - Implemented cost when traversing across slope. 
# version 0.1.1 - Implemented choice of directionality
# version 0.1.0 - First release to Github
