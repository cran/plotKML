# Purpose        : Generic methods to plot RasterBricks in Google Earth (simulations / Time Series)
# Maintainer     : Tomislav Hengl (tom.hengl@wur.nl); 
# Contributions  : Dylan Beaudette (debeaudette@ucdavis.edu); Pierre Roudier (pierre.roudier@landcare.nz);
# Dev Status     : Alpha
# Note           : it basically requires only a single input object;


plotKML.RasterBrickSimulations <- function(
  obj,
  folder.name = normalizeFilename(deparse(substitute(obj, env=parent.frame()))),
  file.name = paste(normalizeFilename(deparse(substitute(obj, env=parent.frame()))), ".kml", sep=""),
  obj.summary = TRUE,
  pngwidth = 680, 
  pngheight = 200,
  pngpointsize = 14,
  kmz = TRUE,
  ...
){

  # objects to plot:
  varname <- paste(obj@variable)
  prj <- obj@sampled@proj4string

  # summary properties of the RK model:
  if(obj.summary==TRUE){
    md <- data.frame(Names=c("variable", "N.realizations", "cellsize.x", "cellsize.y", "Min.value", "Max.value"), Values=c(varname, length(obj@realizations), res(obj@realizations)[1], res(obj@realizations)[2], signif(min(obj@realizations@data@min), 3),  signif(max(obj@realizations@data@max), 3)), stringsAsFactors = FALSE)
    html <- kml_description(md, asText = TRUE, cwidth = 120, twidth = 240)
  }
  
  kml_open(folder.name = folder.name, file.name = file.name)
  
  # add a description for the whole folder:
  kml.out <- get("kml.out", envir=plotKML.fileIO)
  description_txt <- sprintf('<description><![CDATA[%s]]></description>', html)
  parseXMLAndAdd(description_txt, parent=kml.out[["Document"]])  
  assign('kml.out', kml.out, envir=plotKML.fileIO)
  
  rel <- obj@realizations
  kml_layer(obj = rel, ...)

  # densify coordinates:
  smp <- obj@sampled
  tl <- spsample(smp, n=100, "regular")
  tl <- SpatialLines(list(Lines(list(Line(coordinates(tl))), ID="t")), prj)
  # parse the transect:
  kml_layer.SpatialLines(obj = tl, colours = rep(rgb(0,0,0), length(obj)), extrude = TRUE)  

  # plot the correlation graph and variogram:
  png(filename=paste(varname, "_cross_section.png", sep=""), width=pngwidth, height=pngheight, bg="white", pointsize=pngpointsize)
  par(mar=c(4.5,4.5,.8,.8))
  # extract values at the transect:
  ov <- extract(obj@realizations, obj@sampled)
  plot(1:length(ov[[1]][,1]), ov[[1]][,1], type="l", xlab="pixel index", ylab=varname, col="grey", lwd=2)
  for(i in 2:(length(ov[[1]][1,])-1)){
    lines(1:length(ov[[1]][,i]), ov[[1]][,i], col="grey", lwd=2)
  }
  lines(1:length(ov[[1]][,1]), ov[[1]][,length(ov[[1]][1,])], lwd=2)
  dev.off()
  
  # add the cross section plot:
  kml_screen(image.file = paste(varname, "_cross_section.png", sep=""), position = "LL", sname = "Transect with confidence limits")

  # close the file:
  kml_close(file.name = file.name)
  if (kmz == TRUE){
      kml_compress(file.name = file.name)
  }
  # open KML file in the default browser:
  kml_View(file.name)
  
}

setMethod("plotKML", "RasterBrickSimulations", plotKML.RasterBrickSimulations)


# end of script;