\name{kml_layer.RasterBrick}
\alias{kml_layer.RasterBrick}
\alias{RasterBrick}
\title{Export a time series of images to KML}
\description{Writes a series of images to PNGs and uses them to create ground overlays. Works only with \code{"RasterBrick"} class objects with time dimension specified via the \code{"@zvalue"}.}
\usage{
kml_layer.RasterBrick(obj, plot.legend = TRUE, dtime = "", tz = "GMT", 
          z.lim = c(min(obj@data@min, na.rm = TRUE),
          max(obj@data@max, na.rm = TRUE)), 
          colour_scale = get("colour_scale_numeric", envir = plotKML.opts),
          home_url = get("home_url", envir = plotKML.opts),
          metadata = NULL, html.table = NULL, \dots)
}
\arguments{
  \item{obj}{object of class \code{"RasterBrick"} (a time series of images)}
  \item{plot.legend}{logical; specify whether a map legend should be generated automatically}
  \item{dtime}{temporal support (point or block) expressed in seconds}
  \item{tz}{referent time zone}
  \item{z.lim}{upper and lower limits (unique for all maps in the time series); the function by default uses the absolute minimum and maximum in values}
  \item{colour_scale}{color palette; by default uses the color scale for numeric variables}
  \item{home_url}{(optional) URL directory / location of the images}
  \item{metadata}{(optional) the metadata object}
  \item{html.table}{(optional) the description block (html)}
  \item{\dots}{additional arguments (see \code{aesthetics})}
}
\details{This method is recommended for visualization of numeric bands representing the same variable i.e. time series of images. To export a stack of images of different type see \code{\link{kml_layer.Raster}}. If the \code{"@zvalue"} slot is empty, dates will be added by subtracting days from the current day with 1--day increments.
}
\author{Tomislav Hengl }
\seealso{ \code{\link{kml-methods}}, \code{\link{kml_open}}, \code{\link{kml_layer.Raster}} }
\examples{
data(LST)
coordinates(LST) <- ~x+y
gridded(LST) <- TRUE
proj4string(LST) <- CRS("+proj=utm +zone=33 +datum=WGS84 +units=m")
\dontrun{# get the dates from the file names:
dates <- sapply(strsplit(names(LST), "LST"), function(x){x[[2]]})
LST <- brick(LST)
LST@title = "Time series of MODIS Land Surface Temperature images"
LST@zvalue <- format(as.Date(dates, "\%Y_\%m_\%d"), "\%Y-\%m-\%dT\%H:\%M:\%SZ")
data(SAGA_pal)
# time dimension defined via zvalue slot:
LST@zvalue[c(1,12,24)]
# subset to 3 images:
LSTs <- brick(raster(LST, 1), raster(LST, 12), raster(LST, 24))
LST_ll <- reproject(LSTs, program="FWTools")
# copy the dates:
LST_ll@zvalue <- LST@zvalue[c(1,12,24)]
# KML plot with block temporal support of 8 days:
kml(LST_ll, colour_scale=SAGA_pal[[1]], dtime=8*24*60^2)
}
}
\keyword{spatial}


