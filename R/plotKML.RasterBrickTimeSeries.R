# Purpose        : Generic method to plot time-series data in Google Earth 
# Maintainer     : Tomislav Hengl (tom.hengl@wur.nl); 
# Contributions  : ;
# Dev Status     : Alpha
# Note           : plots are in the description tag;

plotKML.RasterBrickTimeSeries <- function(
  obj,
  folder.name = normalizeFilename(deparse(substitute(obj, env=parent.frame()))),
  file.name = paste(normalizeFilename(deparse(substitute(obj, env=parent.frame()))), ".kml", sep=""),
  labsname = names(obj@sampled)[1],
  pngwidth = 680,
  pngheight = 180,
  pngpointsize = 14,
  kmz = TRUE,
  ...
){

  # sampling locations:
  varname <- paste(obj@variable)
  locs <- obj@sampled
  labs <- paste(locs@data[,labsname])
  # Begin end times:
  TimeSpan.begin <- obj@TimeSpan.begin
  TimeSpan.end <- obj@TimeSpan.end
  # copy mean times:
  obj@rasters@zvalue <- paste(as.POSIXct(unclass(as.POSIXct(TimeSpan.begin))+(unclass(as.POSIXct(TimeSpan.end))-unclass(as.POSIXct(TimeSpan.begin)))/2, origin="1970-01-01"))
  dtime = unclass(as.POSIXct(TimeSpan.end)) - unclass(as.POSIXct(TimeSpan.begin))

  # open KML for writing:  
  kml_open(folder.name = folder.name, file.name = file.name)
  
  # add a description for the whole folder:
  kml.out <- get("kml.out", envir=plotKML.fileIO)
  description_txt <- sprintf('<description>%s</description>', obj@rasters@title)
  parseXMLAndAdd(description_txt, parent=kml.out[["Document"]])  
  assign('kml.out', kml.out, envir=plotKML.fileIO)
  
  # extract values at point locations:
  ov <- extract(obj@rasters, obj@sampled)
  png_names <- paste(varname, "_timeseries_", 1:nrow(ov), ".png", sep="")
  html.table <- paste('<img src="', png_names, '" height="', pngheight, '" width="', pngwidth, '" align ="middle" />', sep = '')
  kml_layer.SpatialPoints(obj = locs, points_names = labs, html.table = html.table)
  
  # plot rasters:
  rel <- obj@rasters
  kml_layer(obj = rel, dtime=dtime, ...) 

  # plot the time-series data:
  for(i in 1:nrow(ov)){
    png(filename=png_names[i], width=pngwidth, height=pngheight, bg="white", pointsize=pngpointsize)
    par(mar=c(4.5,4.5,.8,.8))
    plot(as.Date(as.POSIXct(obj@rasters@zvalue)), ov[i,], type="l", xlab="Date", ylab=varname, col="grey", lwd=2)
    points(as.Date(as.POSIXct(obj@rasters@zvalue)), ov[i,], pch="+", cex=.6)
    dev.off()
  }
  
  # close the file:
  kml_close(file.name = file.name)
  if (kmz == TRUE){
      kml_compress(file.name = file.name)
  }
  # open KML file in the default browser:
  kml_View(file.name)
  
}

setMethod("plotKML", "RasterBrickTimeSeries", plotKML.RasterBrickTimeSeries)

# end of script;