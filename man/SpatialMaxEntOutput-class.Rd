\name{SpatialMaxEntOutput-class}
\docType{class}
\alias{SpatialMaxEntOutput-class}
\title{A class for outputs of analysis produced using the dismo package (MaxEnt)}
\description{A class containing input and output data produced by running the \code{maxent} (Maximum Entropy) species distribution modeling algorithm. Object of this class can be directly visualized in Google Earth by using the \code{\link{plotKML-method}}.}
\section{Slots}{
  \describe{
    \item{\code{sciname}:}{object of class \code{"character"}; vector of species name compatible with the \code{rgbif} package; usually latin "genus" and "species" name}
    \item{\code{occurrences}:}{object of class \code{"SpatialPoints"}; occurrence-only records}
    \item{\code{TimeSpan.begin}:}{object of class \code{"POSIXct"}; begin of the sampling period}
    \item{\code{TimeSpan.end}:}{object of class \code{"POSIXct"}; end of the sampling period}
    \item{\code{maxent}:}{object of class \code{"MaxEnt"} (species distribution model); produced as an output of the \code{dismo::maxent} function}
    \item{\code{sp.domain}:}{object of class \code{"Spatial"} (ideally \code{"SpatialPolygonsDataFrame"} or \code{"SpatialPixelsDataFrame"}); assumed spatial domain that can be set by the user or it will be estimated by MaxEnt (see examples below)}
    \item{\code{predicted}:}{object of class \code{"RasterLayer"}; contains results of prediction produced using the MaxEnt software}
  }
}
\section{Methods}{
  \describe{
	\item{plotKML}{\code{signature(obj = "SpatialMaxEntOutput")}: plots all MaxEnt output objects in Google Earth}
  }
}
\note{MaxEnt requires the \code{maxent.jar} file to be in the 'java' folder of the dismo package (see: \code{system.file("java", package="dismo")}). For more info refer to the dismo package documentation.
}
\references{
\itemize{
\item Hijmans, R.J, Elith, J., (2012) \href{http://cran.r-project.org/web/packages/dismo/vignettes/sdm.pdf}{Species distribution modeling with R}. CRAN, Vignette for the dismo package, 72 p.  
\item dismo package (\url{http://CRAN.R-project.org/package=dismo})
\item rgbif package (\url{http://CRAN.R-project.org/package=rgbif})
}
}
\author{ Tomislav Hengl }
\seealso{ \code{\link{plotKML-method}}, \code{dismo::maxent}, \code{rgbif::taxonsearch}, \code{GSIF::MaxEnt} 
}
\keyword{classes}
