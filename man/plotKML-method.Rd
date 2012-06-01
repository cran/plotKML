\name{plotKML-methods}
\docType{methods}
\alias{plotKML-methods}
\alias{plotKML}
\alias{plotKML.SpatialPredictions}
\alias{plotKML,SpatialPredictions-method}
\alias{plotKML.RasterBrickSimulations}
\alias{plotKML,RasterBrickSimulations-method}
\alias{plotKML.SpatialVectorsSimulations}
\alias{plotKML,SpatialVectorsSimulations-method}
\alias{plotKML.SpatialMaxEntOutput}
\alias{plotKML,SpatialMaxEntOutput-method}
\alias{plotKML.RasterBrickTimeSeries}
\alias{plotKML,RasterBrickTimeSeries-method}
\alias{plotKML.SpatialSamplingPattern}
\alias{plotKML,SpatialSamplingPattern-method}
\title{Methods for plotting results of spatial analysis in Google Earth}
\description{The method writes inputs and outputs of spatial analysis (a list of point, gridded and polygon data usually) to KML and opens the KML file in Google Earth (or any other default package used to view KML/KMZ files).}
\usage{plotKML(obj, \dots )
}
\arguments{
  \item{obj}{input object of specific class; usually a composite object containing both inputs and outputs of analysis}
  \item{\dots}{(optional) arguments passed to the lower level functions}
}
\details{This is a generic function to plot various R objects that contain both inputs and outputs of spatial analysis. The resulting plots (referred to as \emph{`views'}) are usually cartographically complete as they contain legends, and data and model descriptions. In principle, \code{plotKML} works only with complex objects such as \code{"SpatialPredictions"}, \code{"SpatialVectorsSimulations"}, \code{"RasterBrickSimulations"}, \code{"RasterBrickTimeSeries"}, \code{"SpatialMaxEntOutput"} and similar. To further customize visualizations consider combining the lower level functions \code{\link{kml_open}}, \code{\link{kml_close}}, \code{\link{kml_compress}}, \code{\link{kml_screen}} into your own wrapper function.
}
\note{Writting large spatial objects via plotKML can be time consuming. Please refer to the package documentation for more information.
}
\seealso{ \code{\link{SpatialPredictions-class}}, \code{\link{SpatialVectorsSimulations-class}}, \code{\link{RasterBrickSimulations-class}}, \code{\link{RasterBrickTimeSeries-class}}, \code{\link{SpatialMaxEntOutput-class}}, \code{\link{SpatialSamplingPattern-class}}   
}
\keyword{spatial}
\keyword{methods}
