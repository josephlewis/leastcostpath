\name{leastcostpath}
\alias{leastcostpath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Computes Least Cost Paths using multiple cost functions
%%  ~~function to do ... ~~
}
\description{

The function computes the Least Cost Path from an origin location to a destination location. It implements multiple cost functions that estimate human movement across a landscape. The function takes a Digital Elevation Model ('RasterLayer' class) and point features ('SpatialPoints' class) for the origin location and destination location.
}

\usage{
leastcostpath(dem, origin, destination, cost_function = "all", direction = FALSE, neighbours = 16, crit_slope = 15)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dem}{
Digital Elevation Model. Object of class RasterLayer
}
  \item{origin}{
Location from which the Least Cost Path is calculated. Object of class SpatialPoints
}
  \item{destination}{
Location to which the Least Cost Path is calculated. Object of class SpatialPoints
}
  \item{cost_function}{
Cost Function to be used in the Least Cost Path calculation. Current implementation computes LCPs using Tobler's Hiking Function, Marquez-Perez et al. Modified Hiking function and Llobera and Sluckin's Fourth-degree polynomial function. Default is "all".
}
  \item{direction}{
If FALSE (default) then Least Cost Paths computed from origin to destination and destination to origin. This is to reflect the anistropy of Tobler's and Marquez-Perez's cost functions. If TRUE then Least Cost Paths computed from origin to destination. see Details for more.
}
  \item{neighbours}{
Number of directions used in the Least Cost Path calculation. \href{https://www.ncbi.nlm.nih.gov/pubmed/17892887}{Huber and Church (1985)} for methodological considerations when choosing number of neighbours. Default is 16.
}
  \item{crit_slope}{
Critical Slope (in percent) is "the transition where switchbacks become more effective than direct uphill or downhill paths". Cost of climbing the critical slope is twice as high as those for moving on flat terrain and is used for estimating the cost of using wheeled vehicles. Critical slope defaulted is 15 degrees, which is the postulated maximum gradient traversable by ancient transport (Verhagen and Jeneson, 2012).
}
}
\details{

The function calculates the Least Cost Path from the origin location ('SpatialPoints' class) to the destination location ('SpatialPoints' class) using functions provided in \link[gdistance]{gdistance}.

Due to the accuracy of Least Cost Paths being sensitive to the algorithm used (Mlekuz, 2014), the function exports Least Cost paths using three cost functions.

The following Cost Functions are implemented within the function:

\href{http://escholarship.org/uc/item/05r820mz}{Tobler's (1993) 'Hiking function'}

6 * exp(-3.5 * abs(slope + 0.05))

\href{https://www.tandfonline.com/doi/abs/10.1080/00167223.2017.1316212}{Marquez-Perez et al. (2017) 'Modified Hiking function'}

4.8 * exp(-5.3 * abs(slope * 0.7) + 0.03)

\href{https://www.ncbi.nlm.nih.gov/pubmed/17892887}{Llobera and Sluckin (2007) 'Fourth-degree polynomial function'}

1/(1 + abs(slope/crit_slope)^2)

Furthermore, the default direction of movement if FALSE, meaning that the Least Cost Path will be calculated from Origin to Destination and Destination and Origin. This is to reflect the view that it is incorrect to assume that round-trips along the same cost paths were made (Kantner, 2012). If direction is TRUE then only Least Cost Path from Origin to Destination is calculated.

}

\value{

The function exports three computed Least Cost Paths ('SpatialLinesDataFrame' class) using the three aforementioned cost functions with data on the cost function name and length of each Least Cost Path (unit dependent on input DEM units)
}
\references{

Huber, D.L. Church, R.L. (1985) "Transmission corridor location modelling", Journal of Transportation Engineering, 111, pp. 114-130.

Kantner, J. (2012) 'Realism, Reality, and Routes: Evaluating Cost-Surface and CostPath Algorithms', in White, D.A. Surface-Evans, S. (eds.) Least Cost Analysis of Social Landscapes: Archaeological Case Studies. Salt Lake City: University of Utah Press, pp.225-238.

Llobera, M. Sluckin, T.J. (2007) 'Zigzagging: Theoretical insights on clumbing strategies'. Journal of Theoretical Biology, 249, 206-217.

Marquez-Perez, J. Vallejo-Villalta, I. Alvarez-Francoso, J.I. (2017) 'Estimated travel time for walking trails in natural areas'. Geografisk Tidsskrift-Danish Journal of Geography, 117, pp. 53-62.

Mlekuz, D. (2014) "Exploring the topography of movement"", in Polla, S. Verhagen, P. (eds.). Computational Approaches to the Study of Movement in Archaeology. Theory, Practice and Interpretation of Factors and Effects of Long Term Landscape Formation and Transformation. Boston: De Gruyter, pp.5-22.

Tobler, W. (1993) "Three Presentations on Geographical Analysis and Modeling: Non- Isotropic Geographic Modeling; Speculations on the Geometry of Geography; and Global Spatial Analysis, Technical Report (93-1)‟. National Center for Geographic Information and Analysis. Available at: http://escholarship.org/uc/item/05r820mz [Accessed 22 December 2018].

Verhagen, P. Jeneson, K. (2012) 'A Roman Puzzle: Trying to find the Via Belgica with GIS', in Chrysanthi, A. Flores, P.M. Papadopoulos, C. (eds.). Thinking beyond the Tool: Archaeological Computing and the Interpretative Process. Oxford: British Archaeological Reports International Series 2344, pp.123-130.

}
\author{
\href{http://josephlewis.github.io}{Joseph Lewis}
}

\examples{

r <- raster(system.file("external/maungawhau.grd", package = "gdistance"))

loc1 = cbind(2667670, 6479000)
loc1 = SpatialPoints(loc1)

loc2 = cbind(2667800, 6479400)
loc2 = SpatialPoints(loc2)

leastcostpath(r, loc1, loc2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
