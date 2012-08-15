\name{kml_layer.STTDF}
\encoding{latin1}
\alias{kml_layer.STTDF}
\alias{STTDF}
\title{Write a space-time trajectory to KML}
\description{Writes an object of class \code{"STTDF"} to a KML file with a possibility to parse attribute variables using several aesthetics arguments.
}
\usage{
kml_layer.STTDF(obj, id.name = names(obj@data)[which(names(obj@data)== "burst")],
       dtime = "", extrude = FALSE, 
       start.icon = paste(get("home_url", envir = plotKML.opts), "3Dballyellow.png", sep = ""), 
       end.icon = paste(get("home_url", envir = plotKML.opts), "golfhole.png", sep = ""), 
       LabelScale = 0.8 * get("LabelScale", envir = plotKML.opts), z.scale = 1,
       metadata = NULL, html.table = NULL, \dots )
}
\arguments{
  \item{obj}{space-time object of class \code{"STTDF"} (spatio-temporal irregular data.frames trajectory)}
  \item{id.name}{trajectory ID column name}
  \item{dtime}{temporal support size (in seconds)}
  \item{extrude}{logical; extrude GPS vertices?}
  \item{start.icon}{start icon name (\code{3Dballyellow.png})}
  \item{end.icon}{destination icon name (\code{golfhole.png})}
  \item{LabelScale}{the default size of icons}
  \item{z.scale}{vertical exaggeration}
  \item{metadata}{(optional) specify the metadata object}
  \item{html.table}{optional description block (html) for each GPS point (vertices)}
  \item{\dots}{other optional arguments}
}
\details{The dateTime is defined as \code{yyyy-mm-ddThh:mm:sszzzzzz}, where \code{T} is the separator between the date and the time, and the time zone is either \code{Z} (for UTC) or \code{zzzzzz}, which represents \enc{±}{+-}hh:mm in relation to UTC. For more info on how Time Stamps work see \url{http://kml-samples.googlecode.com}. If the time is measured at block support, then:\cr 

\code{<TimeStamp><begin> </begin><end> </end></TimeStamp>} \cr

tags will be inserted. Temporal support (\eqn{\Delta t}{dt}) is estimated using the \code{xts::periodicity} function (median time between observations). The begin time is then derived as \eqn{t_1 = t - .5\cdot \Delta t}{t1 = t - .5 d t}.
}
\references{
\itemize{
\item Pebesma, E. (2011) Classes and Methods for Spatio-Temporal Data in R. Journal of Statistical Software.
\item spacetime package (\url{http://CRAN.R-project.org/package=spacetime})
}
}
\author{Tomislav Hengl }
\seealso{ \code{\link{readGPX}} }
\examples{
data(gpxbtour)
# format the time column:
gpxbtour$ctime <- as.POSIXct(gpxbtour$time, format="\%Y-\%m-\%dT\%H:\%M:\%SZ")
coordinates(gpxbtour) <- ~lon+lat
proj4string(gpxbtour) <- CRS("+proj=longlat +datum=WGS84")
\dontrun{# derive distance from origin:
library(fossil)
xy <- as.list(data.frame(t(coordinates(gpxbtour))))
gpxbtour$dist.km <- sapply(xy, function(x) 
  deg.dist(long1=x[1], lat1=x[2], long2=xy[[1]][1], lat2=xy[[1]][2]))
plot(gpxbtour$dist.km, gpxbtour$speed, type="l", 
  xlab="Distance from origin in km", ylab="speed in km per hour")
# convert to a STTDF class:
library(spacetime)
library(adehabitat)
gpx.ltraj <- as.ltraj(coordinates(gpxbtour), gpxbtour$ctime, id = "th")
gpx.st <- as(gpx.ltraj, "STTDF")
gpx.st$speed <- gpxbtour$speed
gpx.st@sp@proj4string <- CRS("+proj=longlat +datum=WGS84")
# open KML file:
kml_open("gpxbtour_speed.kml")
# plot trajectory with speed as colour attribute:
kml_layer(obj = gpx.st, colour = speed, z.lim = c(18,32))
# close KML:
kml_close("gpxbtour_speed.kml")
kml_compress("gpxbtour_speed.kml")
}
}
\keyword{spatial}

