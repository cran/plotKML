\name{vect2rast}
\docType{methods}
\alias{vect2rast}
\alias{vect2rast,SpatialPoints-method}
\alias{vect2rast.SpatialLines}
\alias{vect2rast,SpatialLines-method}
\alias{vect2rast.SpatialPolygons}
\alias{vect2rast,SpatialPolygons-method}
\alias{rasterize}
\title{Convert points, lines and/or polygons to rasters}
\description{Converts any \code{"SpatialPoints*"}, \code{"SpatialLines*"}, or \code{"SpatialPolygons*"} object to a raster map, and (optional) writes it to an external file (GDAL-supported formats; it used the SAGA GIS format by default).}
\usage{
vect2rast(obj, \dots )
}
\arguments{
  \item{obj}{Spatial-PointsDataFrame,-LinesDataFrame or -PolygonsDataFrame object}
  \item{\dots}{additional arguments that can be passed to the \code{raster::rasterize} command}
}
\value{Returns an object of type \code{"SpatialGridDataFrame"}.}
\details{
This function basically extends the \code{\link{rasterize}} function available in the raster package. The advantage of \code{vect2rast}, however, is that it requires no input from the user's side i.e. it automatically determines the grid cell size based on the properties of the input data set. \cr
The optimal grid cell size is estimated based on the density/size of features in the map (\code{nndist} function in \href{http://www.spatstat.org}{spatstat} package): (a) in the case of \code{"SpatialPoints"} cell size is determined as half the mean distance between the nearest points; (b) in the case of \code{"SpatialLines"} half cell size is determined as half the mean distance between the lines; (c) in the case of polygon data cell size is determined as half the median size (area) of polygons of interest. For more details see \href{http://dx.doi.org/10.1016/j.cageo.2005.11.008}{Hengl (2006)}. To process larger vector maps consider using \code{method="SAGA"}.
}
\references{
\itemize{
\item Hengl T., (2006) \href{http://dx.doi.org/10.1016/j.cageo.2005.11.008}{Finding the right pixel size}. Computers and Geosciences, 32(9): 1283-1298.
\item Raster package (\url{http://CRAN.R-project.org/package=raster})
\item SpatStat package (\url{http://www.spatstat.org})
}
}
\author{Tomislav Hengl }
\seealso{ \code{\link{vect2rast.SpatialPoints}}, \code{raster::rasterize}, \code{spatstat::nndist}}
\examples{
data(eberg)
coordinates(eberg) <- ~X+Y
data(eberg_zones)
\dontrun{# point map:
x <- vect2rast(eberg, fname = "SNDMHT_A")
image(x)
# polygon map:
x <- vect2rast(eberg_zones)
image(x)
# for large data sets use SAGA GIS:
x <- vect2rast(eberg_zones, method = "SAGA")
}
}
\keyword{spatial}
