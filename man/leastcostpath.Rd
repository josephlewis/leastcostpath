\name{leastcostpath}
\alias{leastcostpath}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Computes Least Cost Paths using isotropic and anisotropic cost functions
%%  ~~function to do ... ~~
}
\description{

The function computes the Least Cost Path from an origin location to a destination location. It implements multiple isotropic and anisotropic cost functions that estimate human movement across a landscape. The function takes a Digital Elevation Model ('RasterLayer' class) and point features ('SpatialPoints' class) for the origin location and destination location.
}

\usage{
leastcostpath(dem, origin, destination, cost_function = "all",
direction = FALSE, neighbours = 16,crit_slope = 15, hf = "linear", linear_values = c(-2/180, 2),
binary_values = c(45, 999, 1), forward_values = c(999, 1, 0.5))
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
If FALSE (default) then Least Cost Paths computed from origin to destination and destination to origin. This is to reflect the anistropy of Tobler's and Marquez-Perez's cost functions. If TRUE then Least Cost Paths computed from origin to destination only. see Details for more.
}
  \item{neighbours}{
Number of directions used in the Least Cost Path calculation. \href{https://www.ncbi.nlm.nih.gov/pubmed/17892887}{Huber and Church (1985)} for methodological considerations when choosing number of neighbours. Default is 16.
}
  \item{crit_slope}{
Critical Slope (in percent) is "the transition where switchbacks become more effective than direct uphill or downhill paths". Cost of climbing the critical slope is twice as high as those for moving on flat terrain and is used for estimating the cost of using wheeled vehicles. Critical slope defaulted is 15 degrees, which is the postulated maximum gradient traversable by ancient transport (Verhagen and Jeneson, 2012).
}
  \item{hf}{
  The horizontal factors (hf) influence the difficulty of moving from one cell to another by accounting for any horizontal frictional elements encountered. In this case, the horizontal frictional element is the change in aspect degree from one cell to another. Once the change in aspect degree has been determined, a graph is used to identify the actual horizontal factor. Default horizontal factor values are calculated using a inverse linear equation. Binary and Forward horizontal factor graph also available. See details for more.
}
  \item{linear_values}{
  If hf = "linear" then horizontal factors calculated using an inverse linear equation. Requires a vector of slope and intercept. Default values are c(-2/180, 2). See details for more.
}
  \item{binary_values}{
  if hf = "binary" then a vector consisting of the cutting angle and two values need to be provided. If the change in aspect degree from one cell to another is greater than the provided cutting angle, the second value in the vector is used as the horizontal factor. If the change in aspect degree from one cell to another is less than the provided cutting angle, the third value in the vector is used as the horizontal factor. Default values are c(45, 999, 1). See details for more.
}
  \item{forward_values}{

If hf = "forward" then a vector of three values need to be provided. If the change in aspect degree from one cell to another is less than 45 degrees, the first value is used as the horizontal factor. If the change in aspect degree from one cell to another is equal to 45 degrees and less than 90 degrees, the second value is used as the horizontal factor. If the change in aspect degree from one cell to another is equal to or greater than 90 degrees, the third value is used as the horizontal factor. Default values are c(999, 1, 0.5). See details for more.
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

The default direction of movement is FALSE, meaning that the Least Cost Path will be calculated from Origin to Destination and Destination and Origin. This is to reflect the view that it is incorrect to assume that round-trips along the same cost paths were made (Kantner, 2012). If direction is TRUE then only Least Cost Path from Origin to Destination is calculated.

The default number of directions used in the Least Cost Path calculation is 16. Although 4 and 8 number of directions can be specified, it is recommended that the default value is used (ie. 16). This is due to a lower number of directions resulting in incorrect accumulated costs, caused by what Harris (2000) called 'elongation error' (ie. the difference in length between the LCP and the optimal straight line route). This error is caused by the conversion from raster cells to a vector graph, in which Dijkstra's algorithm is designed for (Herzog, 2014).
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
