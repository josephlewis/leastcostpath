# 2.0.2 (22/10/2022)
* modified calculation of run in cost surface calculations to use base R rather than terra::distance. This now allows for the use of DEMs with more cells without causing memory issues

# 2.0.2 (17/10/2022)
* Ensured that Matrix::summary is made explicit rather than rely on using summary from Matrix package.

# 2.0.1 (09/10/2022)
* renamed add_stochasticity to add_global_stochasticity
* added add_local_stochasticity
* added add_global_stochasticity
* added calculate_slope_variance
* added calculate_rmse

# 2.0.0 (05/10/2022)
* From version 2.0.0 onwards the R package leastcostpath is no longer reliant on the R package gdistance. leastcostpath has been updated to work with sf and terra objects
* create_slope_cs now returns a <i>class</i> conductanceMatrix object. This object contains a record of:
- the ConductanceMatrix
- the cost function argument
- the max slope argument
- whether the slope values were exaggerated
- the critical slope argument
- the percentile argument
- the number of adjacent neighbours used in the calculation
* the cost function argument in create_slope_cs() now allows for cost functions to be stated by name (e.g. 'tobler') or as a function
* update_values() now added
* replace_values() now added
* create_cs() now added
* plot_cf() now added
* rasterise() now added