\name{spMetadata-methods}
\docType{methods}
\alias{spMetadata}
\alias{spMetadata,RasterLayer-method}
\alias{spMetadata,Spatial-method}
\alias{read.metadata}
\title{Methods to generate spatial metadata}
\description{
The \code{spMetadata} function will try to generate missing metadata (bounding box, location info, session info, metadata creator info and similar) for any \code{Spatial*} object (from the sp package) or \code{Raster*} object (from the raster package). The resulting object of class \code{\link{SpatialMetadata-class}} can be used e.g. to generate a Layer description documents (\code{<description>} tag).\cr
The \code{read.metadata} function can be used to read the formatted metadata (\code{.xml}), prepared following to the \href{http://www.fgdc.gov/metadata/csdgm/}{Federal Geographic Data Committee (FGDC) Content Standard for Digital Geospatial Metadata}, and convert them to a more readble format (data frame).
}
\usage{ 
\S4method{spMetadata}{RasterLayer}(obj, Target_variable, bounds, color, \dots )
\S4method{spMetadata}{Spatial}(obj, xml.file, generate.missing = TRUE, Citation_title,
    Target_variable, Attribute_Measurement_Resolution = 1, 
    Attribute_Units_of_Measure = "NA",
    Indirect_Spatial_Reference = "", GoogleGeocode = FALSE, 
    Enduser_license_URL = get("license_url", envir = plotKML.opts),
    signif.digit = 3, colour_scale, bounds, legend_names, icons, 
    validate.schema = FALSE)
}
\arguments{
  \item{obj}{some \code{"Spatial"} or \code{"Raster"} class object with \code{"data"} slot}
  \item{xml.file}{character; optional XML metadata file}
  \item{generate.missing}{logical; specifies whether to automatically generate missing fields}
  \item{Citation_title}{character; title of the bibliographic reference for this data set}
  \item{Target_variable}{character; target variable name}
  \item{Attribute_Measurement_Resolution}{numeric; detection limit i.e. measurement precision}
  \item{Attribute_Units_of_Measure}{character; measurement units}
  \item{Indirect_Spatial_Reference}{character; location name as on the topographic map}
  \item{GoogleGeocode}{logical; specifies whether the function should try to use GoogleGeocoding functionality to determine the location name}
  \item{Enduser_license_URL}{character; URL to the end user license}
  \item{signif.digit}{integer; the default number of significant digits (in the case of rounding)}  
  \item{colour_scale}{the colour scheme used to visualize this data}  
  \item{bounds}{numeric vector; upper and lower bounds used for visualization}  
  \item{color}{character; colour legend (must match the \code{bounds} vector)}
  \item{legend_names}{character; legend names in the order of bounds}
  \item{icons}{character; file name or URL used for icons (if applicable)}
  \item{validate.schema}{logical; specifies whether to validate the schema using the \code{xmlSchemaValidate}}
  \item{\dots}{optional arguments}
}
\details{
\code{spMetadata} tries to locate a metadata file in the working directory (it looks for a metadata file with the same name as the object name). If no \code{.xml} file exists, it will load the template xml file available in the system folder (\code{system.file("FGDC.xml", package="plotKML")}). The \code{FGDC.xml} file contains typical metadata entries with description and examples. For practical purposes, one metadata object in plotKML can be associated with only one variable i.e. one column in the \code{"data"} slot (specified via \code{Target_variable}). To prepare a metadata xml file for the layer of interest, consider using the \href{http://plotkml.r-forge.r-project.org/FGDC.xml}{FGDC.xml} template file; to enter the missing fields use e.g. the \href{http://geology.usgs.gov/tools/metadata/tools/doc/tkme.html}{Tkme} software: Another editor for formal metadata, by Peter N. Schweitzer (U.S. Geological Survey). Before commiting the metadata file, try also running a \href{http://geo-nsdi.er.usgs.gov/validation/}{validation test}.\cr
\code{spMetadata} tries to automatically generate the most usefull information, so that a user can easily find out about the input data and procedures followed to generate the visualization (KML). Typical metadata entries include e.g.:
\itemize{
\item \code{metadata[["idinfo"]][["native"]]} --- Session info e.g.: Produced using R version 2.12.2 (2011-02-25) running on Windows 7 x64.
\item \code{metadata[["spdoinfo"]][["indspref"]]} --- Indirect spatial reference estimated using the \href{http://code.google.com/apis/maps/documentation/webservices/}{Google Maps API Web Services}.
\item \code{metadata[["idinfo"]][["spdom"]][["bounding"]]} --- Bounding box in the WGS84 geographical coordinates estimated by reprojecting the original bounding box.
}
By default, plotKML uses the Creative Commons license, but this can be adjusted by setting the \code{Enduser_license_URL} argument above.
}
\references{
\itemize{
\item The Federal Geographic Data Committee, (2006) FGDC Don't Duck Metadata --- A short reference guide for writing quality metadata. Vers. 1, \url{http://www.fgdc.gov/metadata/documents/MetadataQuickGuide.pdf}
\item Content Standard for Digital Geospatial Metadata (\url{http://www.fgdc.gov/metadata/csdgm/})
\item Template metadata file (\url{http://plotkml.r-forge.r-project.org/FGDC.xml})
\item Tkme metadata editor (\url{http://geology.usgs.gov/tools/metadata/tools/doc/tkme.html})
\item Creative Commons licenses (\url{http://creativecommons.org/licenses/})
}
}
\author{Tomislav Hengl}
\seealso{ \code{\link{kml_metadata}}, \code{\link{SpatialMetadata-class}}, \code{\link{spMetadata.Spatial}}, \code{sp::Spatial}, \code{\link{kml_open}} }
\examples{
\dontrun{## read metadata from the system file:
x <- read.metadata(system.file("FGDC.xml", package="plotKML"))
str(x)
## generate missing metadata
data(eberg)
coordinates(eberg) <- ~X+Y
proj4string(eberg) <- CRS("+init=epsg:31467")
## no metadata file specified:
eberg.md <- spMetadata(eberg, Target_variable="SNDMHT_A")
## with localy prepared metadata file:
eberg.md <- spMetadata(eberg, xml.file=system.file("eberg.xml", package="plotKML"), 
Target_variable="SNDMHT_A")
## print the results of validation (it needs to be able to connect to a URL with schema):
eberg.md <- spMetadata(eberg, Target_variable="SNDMHT_A", validate.schema = TRUE)
## write the metadata to a file:
saveXML(metadata(eberg.md), "eberg_SAND.xml")
## export to SLD format:
metadata2SLD(eberg.md, "eberg.sld")
## plot the layer with the metadata:
kml(eberg, file.name = "eberg_md.kml", colour = SNDMHT_A, metadata = eberg.md, kmz = TRUE)
}
}
\keyword{spatial}
\keyword{methods}