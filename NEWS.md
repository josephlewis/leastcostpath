# 1.7.9 (9th January 2021)
* Optimised create_slope_cs() - speed increased by 700%:
  * Vectorised the calculation of slope (rise / run). Function no longer relies on apply() to calculate the difference in elevation.
  * Replacement of values in TransitionMatrix above/below max_slope now calculated using logical booleans
* Optimised create_barrier_cs() - speed increased by 200%
  * Used raster::cellFromPolygon and assigned values by Indexing rather than creating TransitionMatrix from rasterised Polygon (via raster::rasterize)
* Optimise crop_cs() - speed increased by 111,439%
  * Used raster::cellFromPolygon and assigned values by Indexing based on adjacency rather than the total TransitionMatrix columns

# 1.7.8 (13th November 2020)
* Fixed issue in PDI_validation where Area is zero. Now returns a SpatialLinesDataFrame. 
* Removed snap argument in PDI_validation. This is to reflect that the origin and destination coordinates of the two SpatialLines need to be identical for the method to work. 
* Added reverse argument to PDI_validation. If reverse equals TRUE, then reverse order of comparison coordinates. This is ensure that the order of coordinates is correct when creating the polygon between the two SpatialLines. 
* Added transitionFunction argument to create_barrier_cs. This is to allow users to specify how  transition values from centre cells to adjacent cells are calculated.
* Corrected create_traversal_cs to account for when traversal across slope angle is above 90 degrees. Rather than setting value to 1, the correct value is calculated.

# 1.7.7 (4th November 2020)
* Implemented Campbell (2019) cost functions based on  crowdsourced GPS travel rate records. [Campbell (2019)](https://doi.org/10.1016/j.apgeog.2019.03.008)
* Amended 'herzog' (2013) cost function to use absolute slope value. 
* Amended cost surfaces for "tobler", "tobler offpath", "irmischer-clarke male", "irmischer-clarke offpath male", "irmischer-clarke female", "irmischer-clarke offpath female", "modified tobler", and "campbell 2019" to return speed values in seconds.
* lcp cost distance now reported in seconds when "tobler", "tobler offpath", "irmischer-clarke male", "irmischer-clarke offpath male", "irmischer-clarke female", "irmischer-clarke offpath female", "modified tobler", and "campbell 2019" cost functions are used. 

# 1.7.6 (27th October 2020)
* Fixed issue in PDI_Validation where SpatialPolygon was not created properly due to SpatialPoints not being seen as identical. Corrected by removing header name via base::unname.
* Fixed issue in PDI_Validation where SpatialPolygon has an Area of Zero and so is not a 'true' Polygon. In the case, PDI_validation returns a SpatialPolygon with data.frame containing an Area and PDI value of Zero.
* Implemented snap argument in PDI_validation. If TRUE, this snaps the Origin and Destination points of the Least Cost Path to the Origin and Destination points of the comparison SpatialLine. This ensures that the SpatialPolygon that is returned is valid as the Origin and Destination points of the Least Cost Path is the centre of the Raster cell whilst the Origin and Destination of the comparison SpatialLine is not restricted by the Raster Grid.
* Added field argument to create_barrier_cs(). This now allows for the user to specify the conductivity of areas that coincide with the barrier SpatialObject.

# 1.7.5 (4th September 2020)
* Fixed issue with create_banded_lcps and create_CCP_lcps to filter to first SpatialPoint in the supplied SpatialPoints* 
* Implemented geographical slant in create_slope_cs. See function documentation for more information.

# 1.7.4 (17th July 2020)
* Allow for the rasterisation of SpatialLines in create_lcp_density through the rasterize_as_points argument. If FALSE, SpatialLines are rasterised. If TRUE, SpatialLines converted to SpatialPoints and rasterised. See function documentation for more information.

# 1.7.3 (17th July 2020)
* Implemented type 'autocorrelated' and the calculation of probability intervals in add_dem_error

# 1.7.2 (7th July 2020)
* Implemented add_dem_error. See function documentation for more information and usage

# 1.6.2 (6th July 2020)
* Modified how lcp_density calculates which cells are covered by least cost paths. Instead of rasterising points (which can result in cells being missed), the least cost paths themselves are rasterised. 
* Amended PDI_validation function to return area between lines

# 1.6.1 (5th July 2020)
* Implemented PDI_validation. See function documentation for more information and usage

# 1.5.1 (4th July 2020)
* Implemented crop_cs. See function documentation for more information and usage

# 1.4.1 (5th June 2020)
* Fixed create_stochastic_lcp when directional = FALSE. Now checks to ensure both Least Cost Paths are valid

# 1.4.0 (4th June 2020)
* Refactored code to enable greater flexibility
* Added 32 and 48 neighbour matrices based on [Kovanen and Sarjakoski (2015)](https://doi.org/10.1145/2803172) (see leastcostpath::neighbours_32 and leastcostpath::neighbours_48 for their layout)
* Implemented create_wide_lcp. Inspired by [Shirabe (2016)](https://doi.org/10.1080/13658816.2015.1124435)
* Removed 'all' cost_function argument from create_slope_cs. 

# 1.3.9 (1st June 2020)
* Implemented create_stochastic_lcp. Based on the method proposed by Pinto and Keitt (2009). See function documentation for reference

# 1.3.8 (1st June 2020)
* Least Cost Paths created using the create_FETE_lcps are automaticaly ordered low-to-high cost if the cost_distance argument is TRUE 
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
* Added Tobler's offpath cost function
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
* Split `leastcostpath` function into separate functions - allows for the building up of cost surfaces before computing least cost path
  * `create_slope_cs`
  * `create_traversal_cs`
  * `create_openness_cs`
  * Addition of openness cost surface creation

# version 0.1.2 - Implemented cost when traversing across slope. 
# version 0.1.1 - Implemented choice of directionality
# version 0.1.0 - First release to Github
