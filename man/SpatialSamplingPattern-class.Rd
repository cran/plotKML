\name{SpatialSamplingPattern-class}
\docType{class}
\alias{SpatialSamplingPattern-class}
\alias{plotKML,SpatialSamplingPattern}
\title{A class for spatial samples produced using various \code{spsample} methods}
\description{A class containing input and output maps generated as equiprobable simulations of the same discrete object (for example multiple realizations of stream networks). Objects of this type can be directly visualized in Google Earth by using the \code{\link{plotKML-methods}}.}
\section{Slots}{
  \describe{    
    \item{\code{method}:}{object of class \code{"character"}; sampling optimisation method}
    \item{\code{pattern}:}{object of class \code{"SpatialPoints"}; sampling points}
    \item{\code{sp.domain}:}{object of class \code{"SpatialPolygonsDataFrame"}; spatial domain / strata}
  }
}
\section{Methods}{
  \describe{
	\item{plotKML}{\code{signature(obj = "SpatialSamplingPattern")}: plots generated sampling plan in Google Earth}
  }
}
\author{ Tomislav Hengl }
\seealso{ \code{spcosa::spsample}, \code{\link{plotKML-methods}}
}
\examples{ 
library(spcosa)
# read a polygon map:
shpFarmsum <- readOGR(dsn = system.file("maps", package = "spcosa"), layer = "farmsum")
# stratify `Farmsum' into 50 strata
myStratification <- stratify(shpFarmsum, nStrata = 50)
# sample two sampling units per stratum
mySamplingPattern <- spsample(myStratification, n = 2)
# plot the resulting sampling pattern
plot(myStratification, mySamplingPattern)
\dontrun{# attach the correct proj4 string:
library(RCurl)
nl.rd <- getURL("http://spatialreference.org/ref/sr-org/6781/proj4/")
proj4string(mySamplingPattern@sample) <- CRS(nl.rd) 
# prepare spatial domain (polygons):
sp.domain <- as.SpatialPolygons.SpatialPixels(myStratification@cells)
sp.domain <- SpatialPolygonsDataFrame(sp.domain, data.frame(ID=as.factor(myStratification@stratumId)), match.ID = FALSE)
proj4string(sp.domain) <- CRS(nl.rd) 
# create new object:
mySamplingPattern.ssp <- new("SpatialSamplingPattern", method = class(mySamplingPattern), 
    pattern = mySamplingPattern@sample, sp.domain = sp.domain)
# the same plot now in Google Earth:
shape = "http://maps.google.com/mapfiles/kml/pal2/icon18.png"
plotKML(mySamplingPattern.ssp, shape = shape)
}
}
\keyword{classes}
