\name{RasterBrickTimeSeries-class}
\docType{class}
\alias{RasterBrickTimeSeries-class}
\alias{plotKML,RasterBrickTimeSeries}
\title{A class for a time series of regular grids}
\description{A class containing list of rasters, begin, end times and sample points to allow exploration of the values. Objects of this class can be directly visualized in Google Earth by using the \code{\link{plotKML-method}}.}
\section{Slots}{
  \describe{    
    \item{\code{variable}:}{object of class \code{"character"}; variable name} 
    \item{\code{sampled}:}{object of class \code{"SpatialPoints"}; one or more points that can be used to visualize temporal changes in the target variable}
    \item{\code{rasters}:}{object of class \code{"RasterBrick"}; a time-series of raster objects}
    \item{\code{TimeSpan.begin}:}{object of class \code{"POSIXct"}; begin of sampling for each raster map}
    \item{\code{TimeSpan.end}:}{object of class \code{"POSIXct"}; end of sampling for each raster map}
  }
}
\section{Methods}{
  \describe{
	\item{plotKML}{\code{signature(obj = "RasterBrickTimeSeries")}: plots time-series of rasters in Google Earth}
  }
}
\author{ Tomislav Hengl }
\seealso{ \code{\link{RasterBrickSimulations-class}}, \code{\link{plotKML-method}}
}
\examples{
data(LST)
gridded(LST) <- ~x+y
proj4string(LST) <- CRS("+proj=utm +zone=33 +datum=WGS84 +units=m")
dates <- sapply(strsplit(names(LST), "LST"), function(x){x[[2]]})
datesf <- format(as.Date(dates, "\%Y_\%m_\%d"), "\%Y-\%m-\%dT\%H:\%M:\%SZ")
# begin / end dates +/- 4 days:
TimeSpan.begin = as.POSIXct(unclass(as.POSIXct(datesf))-4*24*60*60, origin="1970-01-01") 
TimeSpan.end = as.POSIXct(unclass(as.POSIXct(datesf))+4*24*60*60, origin="1970-01-01")
\dontrun{# reproject (takes time):
LST_ll <- reproject(LST)
# pick few climatic stations:
data(HRtemp08)
pnts <- HRtemp08[which(HRtemp08$NAME=="Pazin")[1],]
pnts <- rbind(pnts, HRtemp08[which(HRtemp08$NAME=="Crni Lug - NP Risnjak")[1],])
pnts <- rbind(pnts, HRtemp08[which(HRtemp08$NAME=="Cres")[1],])
coordinates(pnts) <- ~Lon + Lat
proj4string(pnts) <- CRS("+proj=longlat +datum=WGS84")
# get the dates from the file names:
LST_ll <- brick(LST_ll)
LST_ll@title = "Time series of MODIS Land Surface Temperature (8-day mosaics) images"
LST.ts <- new("RasterBrickTimeSeries", variable = "LST", 
  sampled = pnts, rasters = LST_ll, TimeSpan.begin = TimeSpan.begin, TimeSpan.end = TimeSpan.end)
data(SAGA_pal)
# plot MODIS images in Google Earth:
plotKML(LST.ts, colour_scale=SAGA_pal[[1]])
}
}
\keyword{classes}
