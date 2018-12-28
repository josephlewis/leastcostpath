\name{validation_buffer}
\alias{validation_buffer}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Least Cost Path accuracy - buffer validation
%%  ~~function to do ... ~~
}
\description{
Computes the accuracy of a least cost path based on the method proposed by \href{https://www.tandfonline.com/doi/abs/10.1080/136588197242419}{Goodchild and Hunter (1997)}. Requires two SpatialLines objects and a vector of buffer distances. Returns buffer SpatialPolygons and data frame with accuracy results.

}
\usage{
validation_buffer(lcp, data, buffers = c(50, 100, 250, 500, 1000), export = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lcp}{
Object of class SpatialLines
}
  \item{data}{
Object of class SpatialLines
}
  \item{buffers}{
vector. The buffer distances to be used in the validation method.
}
  \item{export}{
SpatialPolygonsDataFrame. Exports buffers.
}
}
\details{

}
\value{
Returns data.frame with specified buffer distances and computed accuracy of the LCP
}
\references{
Goodchild, M.F. Hunter, G.J. (1997). A Simple Positional Accuracy Measure for Linear Features, Technical Communication. International Journal of Geographical Information Science, 11, 299-306.

}
\author{
\href{http://josephlewis.github.io}{Joseph Lewis}
}

\examples{

x1 <- c(1,5,4,8)
y1 <- c(1,3,4,7)
line1 <- SpatialLines(list(Lines(Line(cbind(x1,y1)), ID="a")))

x2 <- c(1,5,5,8)
y2 <- c(1,4,6,7)
line2 <- SpatialLines(list(Lines(Line(cbind(x2,y2)), ID="b")))

validation_buffer(lcp = line1, data = line2, buffers = c(0.1, 0.2, 0.5, 1))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
