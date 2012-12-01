\name{count-methods}
\docType{methods}
\alias{count}
\alias{count,GridTopology-method}
\title{Count the number of occurrences of a vector object over a GridTopology}
\description{Counts the number of occurrences of a vector object over a \code{"GridTopology"} for a list of vector objects (usually multiple realizations of the same process).}
\usage{
\S4method{count}{GridTopology}(x, vectL, \dots)
}
\arguments{
  \item{x}{object of type \code{"GridTopology"}}
  \item{vectL}{list of vectors of class \code{"SpatialPoint*"}, \code{"SpatialLines*"} or \code{"SpatialPolygons*"} (equiprobable realizations of the same process)}
  \item{\dots}{(optional) arguments passed to the lower level functions}
}
\author{ Tomislav Hengl}
\seealso{ \code{SpatialVectorsSimulations-class}, \code{\link{vect2rast}}
}
\keyword{spatial}
\keyword{methods}
