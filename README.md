# leastcostpath - version 2.0.3 <img src="https://josephlewis.github.io/leastcostpath.png" align="right"  width="20%" height="20%"/>

<b>leastcostpath</b> from version 2.0.0 onwards is built without dependency on the R package gdistance and instead relies on <b>sf</b> and <b>terra</b>.

**NOTE:** The R library <b>leastcostpath</b> requires the use of projected coordinate systems. The package does not account for geographic coordinate systems.

*Functions currently in development:*

*Functions recently added:*
* update_values()
* replace_values()
* create_cs()
* plot_cf()
* rasterise()
* add_local_stochasticity()
* add_global_stochasticity()
* calculate_slope_variance()
* calculate_rmse()

Getting Started
---------------

Installation
--------

    #install.packages("devtools")
    library(devtools)
    install_github("josephlewis/leastcostpath")
    library(leastcostpath)
    
Issues
--------

Please submit issues and enhancement requests via github Issues
  * If submitting an issue, please clearly describe the issue, including steps to reproduce when it is a bug, or a justification for the proposed enhancement request


Case Studies Using _leastcostpath_
--------

Theron, K.J., J.S. Pryke and M.J. Samways 2022. Maintaining functional connectivity in grassland corridors between plantation forests promotes high-quality habitat and conserves range restricted grasshoppers. Landsc Ecol 37, 2081–97

Barnett, T., J. Valdez-Tullett and L.M. Bjerketvedt 2022. Close encounters: visibility and accessibility of Atlantic rock art in Scotland. Abstractions Based on Circles: Papers on prehistoric rock art presented to Stan Beckensall on his 90th birthday, 63

Fjellström, M., Seitsonen, O., Wallén, H., 2022. Mobility in Early Reindeer Herding, in: Salmi, A.-K. (Ed.), Domestication in Action, Arctic Encounters. Springer International Publishing, Cham, pp. 187–212. https://doi.org/10.1007/978-3-030-98643-8_7

Field, S., Glowacki, D.M., Gettler, L.T., 2022. The Importance of Energetics in Archaeological Least Cost Analysis. J Archaeol Method Theory. https://doi.org/10.1007/s10816-022-09564-8

Lewis, J., 2021. Probabilistic Modelling for Incorporating Uncertainty in Least Cost Path Results: a Postdictive Roman Road Case Study. Journal of Archaeological Method and Theory. https://doi.org/10.1007/s10816-021-09522-w

Ludwig, B., 2020. Reconstructing the Ancient Route Network in Pergamon’s Surroundings. Land 9, 241. https://doi.org/10.3390/land9080241

Versioning
----------

See NEWS.md for a summary of Version updates

Authors
-------

-   Joseph Lewis - *author / creator* - [Website](https://josephlewis.github.io)

Citation
--------

Please cite as:

    Lewis, J. (2022) leastcostpath: Modelling Pathways and Movement Potential Within a Landscape (version 2.0.3). 
    Available at: https://github.com/josephlewis/leastcostpath
