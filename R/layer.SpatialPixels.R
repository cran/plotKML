# Purpose        : Write a SpatialPixels object to KML;
# Maintainer     : Tomislav Hengl (tom.hengl@wur.nl);
# Contributions  : ; 
# Status         : pre-alpha
# Note           : ;

kml_layer.SpatialPixels <- function(  
  obj,
  raster_name,
  plot.legend = TRUE,
  metadata = NULL,
  ...
  ){

  require(RSAGA)
  # get our invisible file connection from custom evnrionment
  kml.out <- get("kml.out", envir=plotKML.fileIO)

  # Checking the projection 
  prj.check <- check_projection(obj, control = TRUE)

  # Parsing the call:
  call <- substitute(list(...))
  call <- as.list(call)[-1]

  # Check if any attribute has been selected:
  if (is.na(charmatch("colour", names(call)))){
    stop("No attribute selected. Please use the colour = ... option.")
  }

  if(is.call(call[["colour"]])|is.name(call[["colour"]])){
    x <- data.frame(eval(call[["colour"]], obj@data))
    names(x) <- deparse(call[["colour"]])
    obj@data <- x
  } else { 
  if(is.numeric(call[["colour"]])) {
    i_layer <- call[["colour"]]
    if (nlayers(obj) > 1) {
      obj <- obj[i_layer]
    }
  } else { 
  if(is.character(call[["colour"]])) {
    i_layer <- which(names(obj) == call[["colour"]])
    if (nlayers(obj) > 1) {
      obj <- obj[i_layer]
    }
  }}}

  # Trying to reproject data if the check was not successful
  if(!prj.check) {  
    obj <- reproject(obj) 
  }

  # TH: this needs to be fixed
  r <- raster(obj)
  altitude <- charmatch("altitude", names(call))
  if(!is.na(altitude)){
    altitude <- eval(call[["altitude"]], length(r))
    altitude <- kml_altitude(r, altitude)
  } else {
    altitude <- kml_altitude(r, altitude=NULL)
  }
  altitudeMode <- kml_altitude_mode(altitude, GroundOverlay=TRUE) 

  # prepare the palette:
  if (!is.na(charmatch("colour_scale", names(call)))){
    pal <- eval(call[["colour_scale"]])
  } else {
  ## default colour palettes
    if (!is.factor(obj@data[,1])){
      pal <- get("colour_scale_numeric", envir = plotKML.opts)
    } else {
      pal <- get("colour_scale_factor", envir = plotKML.opts)
    }
  }

  if(is.factor(obj@data[,1])){
    colour_scale <- colorRampPalette(pal)(length(levels(obj@data[,1])))
  } else {
    colour_scale <- colorRampPalette(pal)(100)  
  }

  # Transparency
  alpha <- charmatch("alpha", names(call))
  if (!is.na(alpha)) {
    colour_scale <- kml_alpha(obj, alpha = eval(call[["alpha"]], obj@data), colours = colour_scale, RGBA = TRUE)
  }

  # Creating the PNG file
  if(missing(raster_name)){
    raster_name <- set.file.extension(as.character(call[["colour"]]), ".png")
  }

  # Plotting the image
  png(filename = raster_name, bg = "transparent", type="cairo-png")
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  if(!is.na(charmatch("z.lim", names(call)))){ 
    z.lim <- eval(call[["z.lim"]])
    r <- calc(r, fun=function(x){ x[x < z.lim[1]] <- z.lim[1]; return(x)}) 
    r <- calc(r, fun=function(x){ x[x > z.lim[2]] <- z.lim[2]; return(x)}) 
  }
  image(r, col = colour_scale, frame.plot = FALSE)
  dev.off()

  ## There is a bug in Google Earth that does not allow transparency of PNGs:
  # http://groups.google.com/group/earth-free/browse_thread/thread/1cd6bc29a2b6eb76/62724be63547fab7
  # Solution: add transparency using ImageMagick:
  convert <- get("convert", envir = plotKML.opts)
  if(nchar(convert)==0){
    plotKML.env(silent = FALSE, show.env = FALSE)
    convert <- get("convert", envir = plotKML.opts)
  } else {
    # if it does manages to find ImageMagick:
    if(!nchar(convert)==0){
      system(paste(convert, ' ', raster_name, ' -matte -transparent "#FFFFFF" ', raster_name, sep=""))
    } else {
    warning("PNG transparency possibly ineffective. Install ImageMagick and add to PATH. See ?kml_layer.SpatialPixels for more info.")
    }
  }

  # plot the legend (PNG)
  if(plot.legend == TRUE){
    if(missing(raster_name)){
      legend_name <- paste(as.character(call[["colour"]]), "legend.png", sep="_")
    } else {
      legend_name <- paste(strsplit(raster_name, "\\.")[[1]][1], "legend.png", sep="_")  
    }
    if(!is.na(charmatch("z.lim", names(call)))){
      kml_legend.bar(x = obj@data[,1], legend.file = legend_name, legend.pal = colour_scale, z.lim = eval(call[["z.lim"]]))
    } else {
      kml_legend.bar(x = obj@data[,1], legend.file = legend_name, legend.pal = colour_scale)
    }
  }

  message("Parsing to KML...")
  # Folder name
  pl1 = newXMLNode("Folder", parent=kml.out[["Document"]])
  pl2 <- newXMLNode("name", paste(class(obj)), parent = pl1)

  # Insert metadata:
  if(!is.null(metadata)){
    md.txt <- kml_metadata(metadata, asText = TRUE)
    txt <- sprintf('<description><![CDATA[%s]]></description>', md.txt)
    parseXMLAndAdd(txt, parent=pl1)
  }

  # Ground overlay
  # =====================
  pl2b <- newXMLNode("GroundOverlay", parent = pl1)
  # Creating a SpatialPixelsDataFrame object to be plotted
  pl3 <- newXMLNode("name", deparse(call[["colour"]]), parent = pl2b)
  pl3b <- newXMLNode("altitude", signif(altitude, 4), parent = pl2b)
  pl3b <- newXMLNode("altitudeMode", altitudeMode, parent = pl2b)
  pl3c <- newXMLNode("Icon", parent = pl2b)
  pl4 <- newXMLNode("href", raster_name, parent = pl3c)
  pl3d <- newXMLNode("LatLonBox", parent = pl2b)
  pl4b <- newXMLNode("north", bbox(obj)[2, 2], parent = pl3d)
  pl4c <- newXMLNode("south", bbox(obj)[2, 1], parent = pl3d)
  pl4d <- newXMLNode("east", bbox(obj)[1, 2], parent = pl3d)
  pl4e <- newXMLNode("west", bbox(obj)[1, 1], parent = pl3d)
  
  # Legend
  # ======================
  if(plot.legend == TRUE){
    txtso <- sprintf('<ScreenOverlay><name>Legend</name><Icon><href>%s</href></Icon><overlayXY x="0" y="1" xunits="fraction" yunits="fraction"/><screenXY x="0" y="1" xunits="fraction" yunits="fraction"/></ScreenOverlay>', legend_name)
    parseXMLAndAdd(txtso, parent=kml.out[["Document"]])
  }
  
  # save results: 
  assign("kml.out", kml.out, envir=plotKML.fileIO)

}

setMethod("kml_layer", "SpatialPixels", kml_layer.SpatialPixels)
setMethod("kml_layer", "SpatialGrid", kml_layer.SpatialPixels)

# end of script;