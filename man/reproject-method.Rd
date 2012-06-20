\name{reproject-methods}
\docType{methods}
\alias{reproject-methods}
\alias{reproject}
\alias{reproject.RasterBrick}
\alias{reproject.RasterLayer}
\alias{reproject.RasterStack}
\alias{reproject.SpatialGrid}
\alias{reproject.SpatialPoints}
\alias{reproject,SpatialLines-method}
\alias{reproject,SpatialPoints-method}
\alias{reproject,SpatialPixelsDataFrame-method}
\alias{reproject,SpatialPixelsDataFrame-method}
\alias{reproject,SpatialPolygons-method}
\alias{reproject,SpatialGridDataFrame-method}
\alias{reproject,RasterStack-method}
\alias{reproject,RasterLayer-method}
\alias{reproject,RasterBrick-method}
\title{Methods to reproject maps to a referent coordinate system (WGS84)}
\description{This wrapper function reprojects any vector or raster spatial data to some referent coordinate system (by default: geographic coordinates on the World Geodetic System of 1984 / WGS84 datum).}
\usage{reproject(obj, \dots)}
\arguments{
  \item{obj}{Spatial* or Raster* object}
  \item{\dots}{arguments evaluated in the context of function \code{projectRaster} from the raster package}
}
\details{
In the case of raster and/or gridded maps, by selecting \code{program = "FWTools"} \code{gdalwarp} functionality will be initiated (otherwise it tries to reproject via the package raster). This requires that \href{http://fwtools.maptools.org}{FWTools} are installed and located from R via \code{paths()}. FWTools includes OpenEV, GDAL, MapServer, PROJ.4 and OGDI, and can be installed on Windows and Unix-type operating systems.
}
\references{
\itemize{
\item Raster package (\url{http://CRAN.R-project.org/package=raster})
\item FWTools (\url{http://fwtools.maptools.org})
}
}
\author{Pierre Roudier, Tomislav Hengl and Dylan Beaudette}
\section{Warning}{\code{obj} needs to have a proper proj4 string (CRS), otherwise \code{reproject} will not run.}
\seealso{\code{\link{paths}}, \code{raster::projectRaster}, \code{rgdal::spTransform}, \code{rgdal::CRS-class}}
\examples{
# example with vector data
library(rgdal)
data(eberg)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
eberg.geo <- reproject(eberg)
# example with raster data
data(eberg_grid25)
gridded(eberg_grid25) <- ~x+y
proj4string(eberg_grid25) <- CRS("+init=epsg:31467")
\dontrun{# reproject to geographical coords (can take few minutes!):
eberg_grid_ll <- reproject(eberg_grid25)
# much faster when using FWTools:
eberg_grid_ll2 <- reproject(eberg_grid25, program = "FWTools")
# optional: compare processing times:
system.time(eberg_grid_ll <- reproject(eberg_grid25[1]))
system.time(eberg_grid_ll2 <- reproject(eberg_grid25[1], program="FWTools"))
}
}
\keyword{spatial}

