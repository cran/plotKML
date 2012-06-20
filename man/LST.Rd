\name{LST}
\alias{LST}
\docType{data}
\encoding{latin1}
\title{Time series of MODIS LST images}
\description{\code{LST} contains a spatial sub-sample (Istra region in Croatia) of 46 time series of MODIS LST images (estimated Land Surface Temperature in degrees C) at 1 km resolution. The temporal support size of these images is 8-days.
}
\usage{data(LST)}
\format{
The \code{LST} data frame contains the following layers:
  \describe{
	\item{\code{LST2008_01_01}}{8-day MODIS LST mosaick for period 2007-12-29 to 2008-01-04}
	\item{\code{LST2008_01_09}}{8-day MODIS LST mosaick for period 2008-01-05 to 2008-01-13}
	\item{\code{\dots}}{subsequent bands}
  \item{\code{x}}{a numeric vector; x-coordinate (m) in the UTM system }
  \item{\code{y}}{a numeric vector; y-coordinate (m) in the UTM system }
  }
} 
\author{ Tomislav Hengl and Melita Percec Tadic }
\references{
\itemize{
\item Hengl, T., Heuvelink, G.B.M., Percec Tadic, M., Pebesma, E., (2011) \href{http://dx.doi.org/10.1007/s00704-011-0464-2}{Spatio-temporal prediction of daily temperatures using time-series of MODIS LST images}. Theoretical and Applied Climatology, 107(1-2): 265-277.
\item MODIS products (\url{https://lpdaac.usgs.gov/lpdaac/products/modis_products_table/})
}
}
\note{Time series of 46 day-time and night-time 8-day composite LST images (\href{https://lpdaac.usgs.gov/lpdaac/products/modis_products_table/land_surface_temperature_emissivity/8_day_l3_global_1km/mod11a2}{MOD11A2} product bands \code{1} and \code{5}) was obtained from the NASA's FTP server (\url{ftp://e4ftl01u.ecs.nasa.gov}). The original 8-day composite images were created by patching together images from a period of \enc{±}{+-}4 days, so that the proportion of clouds can be reduced to a minimum. The \code{"zvalue"} slot in the \code{"RasterBrick"} object can be used as the dateTime column expressed as:\cr 

\code{yyyy-mm-ddThh:mm:sszzzzzz}\cr

where T is the separator between the date and the time, and the time zone is either Z (for UTC) or zzzzzz, which represents \enc{±}{+-}hh:mm in relation to UTC.}
\examples{
data(LST)
gridded(LST) <- ~x+y
proj4string(LST) <- CRS("+proj=utm +zone=33 +datum=WGS84 +units=m")
\dontrun{# get the dates from the file names:
dates <- sapply(strsplit(names(LST), "LST"), function(x){x[[2]]})
LST <- brick(LST)
LST@title = "Time series of MODIS Land Surface Temperature (8-day mosaics) images"
LST@zvalue <- format(as.Date(dates, "\%Y_\%m_\%d"), "\%Y-\%m-\%dT\%H:\%M:\%SZ")
data(SAGA_pal)
# plot MODIS images in Google Earth:
LST_ll <- reproject(LST)  # reprojecting rasters takes few minutes;
# copy the dates:
LST_ll@zvalue <- LST@zvalue
# KML plot with block temporal support of 8 days:
kml(LST_ll, colour_scale=SAGA_pal[[1]], dtime=8*24*60^2)
}
}
\keyword{datasets}
