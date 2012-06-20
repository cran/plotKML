\name{spMetadata.Spatial}
\alias{spMetadata.Raster}
\alias{spMetadata.Spatial}
\title{Generate spatial metadata from an sp object}
\description{Tries to generate metadata from an external file. If no metadata file is available, tries to fill in missing columns based on the data properties.}
\usage{ spMetadata.Spatial(obj, xml.file, generate.missing = TRUE, Citation_title,  
    Target_variable, Attribute_Measurement_Resolution = 1, 
    Attribute_Units_of_Measure = "NA",
    Indirect_Spatial_Reference = "", GoogleGeocode = FALSE, 
    Enduser_license_URL = get("license_url", envir = plotKML.opts),
    signif.digit = 3, colour_scale, bounds, legend_names, icons, 
    validate.schema = FALSE)
}
\arguments{
  \item{obj}{some \code{"Spatial"} class object with \code{"data"} slot}
  \item{xml.file}{(optional) metadata file in the FGDC format}
  \item{generate.missing}{logical; species whether the program should try to generate missing metadata automatically}
  \item{Citation_title}{one-sentence title of the object}
  \item{Target_variable}{target variable i.e. column name in the data slot}
  \item{Attribute_Measurement_Resolution}{numeric resolution (typically estimated as half the measurement precision)}
  \item{Attribute_Units_of_Measure}{physical units of the target variable}
  \item{Indirect_Spatial_Reference}{name of the closest geographical location, town or village}
  \item{GoogleGeocode}{logical; specifies whether R should try to estimate Indirect Spatial Reference using Google maps API}
  \item{Enduser_license_URL}{end-user license specification (URL)}
  \item{signif.digit}{integer; a number of significant digits to round the numbers in the palette slot}
  \item{colour_scale}{collor scheme used to visualize this data}
  \item{bounds}{numeric vector; upper and lower limit bounds for the target variable}
  \item{legend_names}{(optional) manually inserted legend names}
  \item{icons}{(optional) manually inserted icon names for legend names}
  \item{validate.schema}{logical; specieis whether R should try to validate your xml file}  
}
\details{
\code{spMetadata.Spatial} tries to locate a metadata file in the working directory (it looks for a metadata file with the same name as the object name). If no \code{.xml} file exists, it will load the template xml file available in the system folder (\code{system.file("FGDC.xml", package="plotKML")}).
}
\author{Tomislav Hengl}
\seealso{ \code{\link{SpatialMetadata-class}}
}
\examples{
data(eberg)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
\dontrun{# (a) no metadata file exist:
eberg.md <- spMetadata(eberg, Target_variable="SNDMHT_A")
# (b) with localy prepared metadata file:
eberg.md <- spMetadata(eberg, xml.file=system.file("eberg.xml", package="plotKML"), 
Target_variable="SNDMHT_A")
}
}
\keyword{spatial}