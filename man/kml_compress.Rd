\name{kml_compress}
\alias{kml_compress}
\title{Compress a KML file with auxiliary files}
\description{Compresses the KML file toghether with the auxiliary files (images, models, textures) using the default ZIP program.}
\usage{
kml_compress(file.name, zip = "", files = "", rm = FALSE, \dots)
}
\arguments{
  \item{file.name}{KML file name}
  \item{zip}{(optional) location of an external ZIP program}
  \item{files}{a character vector specifying the list of auxiliary files}
  \item{rm}{logical; specify whether to remove temporary files}
  \item{\dots}{other \code{kml} arguments}
}
\references{
\itemize{
\item KMZ description (\url{http://code.google.com/apis/kml/documentation/})
}
}
\author{Pierre Roudier, Tomislav Hengl and Dylan Beaudette}
\details{The KMZ file can carry the model files (\code{.dae}), textures and ground overlay images. For practical purposes, we recommend that you, instead of compressing the images together with the KML file, consider serving the ground overlay images via a server i.e. as network links. \cr
If no internal ZIP program exists, the function looks for the system ZIP program:\cr 

\code{Sys.getenv("R_ZIPCMD", "zip")}\cr

External ZIP program can also be specified via the \code{zip} argument.}
\seealso{ \code{\link{kml-methods}}, \code{\link{kml_open}} }
\examples{
\dontrun{# get image info from Wikimedia:
imagename = "Soil_monolith.jpg"
x1 <- getWikiMedia.ImageInfo(imagename)
sm <- spPhoto(filename = x1$url$url, exif.info = x1$metadata)
kml_open("sm.kml")
kml_layer(sm, method="monolith")
kml_close("sm.kml")
# compress together both the KML file and the auxiliary COLLADA file:
kml_compress("sm.kml", files="Soil_monolith_jpg.dae")
}
}
\keyword{utilities}

