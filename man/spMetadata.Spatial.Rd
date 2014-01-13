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
    validate.schema = FALSE, Target_variable_descr = "NA", 
    Geosp_Data_Presentation_Form, Edition, Purpose, Abstract, 
    Supplinf, Publisher, Pub_Place, Temp_Ext,
    Update_Freq = c("Unknown", "Continually", "Daily", "Weekly", 
    "Monthly", "Annually", "As needed", "Irregular", "None planned"), 
    Stat_Progress = c("Complete", "In work", "Planned"), 
    Spatial_Represent_Type = c("Point", "Vector", "Raster"),
    Keyw_theme, Keyw_place)
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
  \item{Target_variable_descr}{character; specifies target variable description for eainfo-node 'attrdef'}
  \item{Geosp_Data_Presentation_Form}{character; defines node 'geoform'}
  \item{Edition}{ defines the version of the title}
  \item{Purpose}{ defines the purpose of the given layer}
  \item{Abstract}{ defines the abstract of the given layer}
  \item{Supplinf}{ defines the supplemental information of the given layer}
  \item{Publisher}{ defines publishing organization (or individual)}
  \item{Pub_Place}{ defines publication place ('pubplace')}
  \item{Temp_Ext}{ defines temporal extent start/end}
  \item{Update_Freq}{ defines update frequency}
  \item{Stat_Progress}{ defines progress status}
  \item{Spatial_Represent_Type}{ defines direct node (direct spatial reference method)}
  \item{Keyw_theme}{ defines node(s) 'themekey'}
  \item{Keyw_place}{ defines node(s) 'placekey'}
}
\details{
\code{spMetadata.Spatial} tries to locate a metadata file in the working directory (it looks for a metadata file with the same name as the object name). If no \code{.xml} file exists, it will load the template xml file available in the system folder (\code{system.file("FGDC.xml", package="plotKML")}).
}
\author{Tomislav Hengl and Michael Blaschek}
\seealso{ \code{\link{SpatialMetadata-class}}
}
\examples{
\dontrun{
data(eberg)
library(sp)
library(rgdal)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
# (a) no metadata file exist:
eberg.md <- spMetadata(eberg, Target_variable="SNDMHT_A")
# (b) with localy prepared metadata file:
eberg.md <- spMetadata(eberg, xml.file=system.file("eberg.xml", package="plotKML"), 
Target_variable="SNDMHT_A")
}
}
\keyword{spatial}
