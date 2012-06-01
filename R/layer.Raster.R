# Purpose        : Write a raster layer to KML;
# Maintainer     : Pierre Roudier (pierre.roudier@landcare.nz);
# Contributions  : Tomislav Hengl (tom.hengl@wur.nl); Dylan Beaudette (debeaudette@ucdavis.edu); 
# Status         : pre-alpha
# Note           : Rasters can also be written as polygons; see "?grid2poly"

kml_layer.Raster <- function(
  obj,  
  plot.legend = TRUE,
  metadata = NULL,
  raster_name, 
  ...
  ){

  require(RSAGA)
  # get our invisible file connection from custom evnrionment
  kml.out <- get("kml.out", envir=plotKML.fileIO)

  # Checking the projection 
  prj.check <- check_projection(obj, control = TRUE)

  # Parsing the call for "colour"
  call <- substitute(list(...))
  call <- as.list(call)[-1]

  # Parse the current call
  colour <- charmatch("colour", names(call))

  if (is.na(colour))
    stop("No attribute selected. Please use the colour = ... option.")

  if (is.call(call[["colour"]])) {
    x <- data.frame(getValues(obj))
    x <- eval(call[["colour"]], x)
    obj <- raster(obj)
    values(obj) <- x
  }
  else if (is.name(call[["colour"]])) {
    if (nlayers(obj) > 1) {
      i_layer <- which(layerNames(obj) == as.character(call[["colour"]]))
      obj <- raster(obj, layer = i_layer)
    }
  }
  else if (is.numeric(call[["colour"]])) {
    i_layer <- call[["colour"]]
    if (nlayers(obj) > 1) {
      obj <- raster(obj, layer = i_layer)
    }
  }
  else if (is.character(call[["colour"]])) {
    i_layer <- which(layerNames(obj) == call[["colour"]])
    if (nlayers(obj) > 1) {
      obj <- raster(obj, layer = i_layer)
    }
  }

  # Trying to reproject data if the check was not successful
  if(!prj.check) {  obj <- reproject(obj) }
  x <- getValues(obj)

  #   altitude <- eval(call[["altitude"]], obj@data)
  altitude <- kml_altitude(obj, altitude = NULL)
  altitudeMode <- kml_altitude_mode(altitude)

  pal <- charmatch("colour_scale", names(call))
  if (!is.na(pal))
    pal <- eval(call[["colour_scale"]])
  else {
  ## default colour palettes
  .colour_scale_numeric = get("colour_scale_numeric", envir = plotKML.opts)
  .colour_scale_factor = get("colour_scale_factor", envir = plotKML.opts)
    if (!is.factor(obj))
      pal <- .colour_scale_numeric
    else
      pal <- .colour_scale_factor
  }

  colour_scale <- colorRampPalette(pal)(length(x))

  # Transparency
  alpha <- charmatch("alpha", names(call))
  if (!is.na(alpha)) {
    # - constant transparency
    # - raster index if a Stack
    # - name of a layer if a Stack
    colour_scale <- kml_alpha(obj, alpha = eval(call[["alpha"]], obj@data), colours = colour_scale, RGBA = TRUE)
  }

  # Creating a SpatialPixelsDataFrame object to be plotted
  call_name <- deparse(call[["colour"]])

  # Creating the PNG file
  if(missing(raster_name)){
    raster_name <- set.file.extension(as.character(call[["colour"]]), ".png")
  }

  # Plotting the image
  png(filename = raster_name, bg = "transparent")
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  image(obj, col = colour_scale, frame.plot = FALSE)
  dev.off()

  ## There is a bug in Google Earth that does not allow transparency of PNGs:
  # http://groups.google.com/group/earth-free/browse_thread/thread/1cd6bc29a2b6eb76/62724be63547fab7
  # Solution: add transparency using ImageMagick:
  convert <- get("convert", envir = plotKML.opts)
  if(nchar(convert)==0){
    plotKML.env(silent = FALSE, show.env = FALSE)
    convert <- get("convert", envir = plotKML.opts)
  }
  # if it does manages to find ImageMagick:
  if(!nchar(convert)==0){
      system(paste(convert, ' ', raster_name, ' -matte -transparent "#FFFFFF" ', raster_name, sep=""))
  }
  else{
  warning("PNG transparency possibly ineffective. Install ImageMagick and add to PATH. See ?kml_layer.Raster for more info.")
  }

  # plot the legend (PNG)
  if(plot.legend == TRUE){
    if(missing(raster_name)){
      legend_name <- paste(as.character(call[["colour"]]), "legend.png", sep="_")
    } else {
      legend_name <- paste(strsplit(raster_name, "\\.")[[1]][1], "legend.png", sep="_")      
    }
    kml_legend.bar(x = x, legend.file = legend_name, legend.pal = colour_scale) 
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
  pl3 <- newXMLNode("name", call_name, parent = pl2b)
  pl3b <- newXMLNode("altitude", altitude, parent = pl2b)
  pl3b <- newXMLNode("altitudeMode", altitudeMode, parent = pl2b)
  pl3c <- newXMLNode("Icon", parent = pl2b)
  pl4 <- newXMLNode("href", raster_name, parent = pl3c)
  pl3d <- newXMLNode("LatLonBox", parent = pl2b)
  pl4b <- newXMLNode("north", bbox(extent(obj))[2, 2], parent = pl3d)
  pl4c <- newXMLNode("south", bbox(extent(obj))[2, 1], parent = pl3d)
  pl4d <- newXMLNode("east", bbox(extent(obj))[1, 2], parent = pl3d)
  pl4e <- newXMLNode("west", bbox(extent(obj))[1, 1], parent = pl3d)
  
  # Legend
  # ======================
  if(plot.legend == TRUE){
  txtso <- sprintf('<ScreenOverlay><name>Legend</name><Icon><href>%s</href></Icon><overlayXY x="0" y="1" xunits="fraction" yunits="fraction"/><screenXY x="0" y="1" xunits="fraction" yunits="fraction"/></ScreenOverlay>', legend_name)
  parseXMLAndAdd(txtso, parent=kml.out[["Document"]])
  }
  
  # save results: 
  assign("kml.out", kml.out, envir=plotKML.fileIO)
  
}

setMethod("kml_layer", "RasterLayer", kml_layer.Raster)
setMethod("kml_layer", "RasterStack", kml_layer.Raster)

# end of script;