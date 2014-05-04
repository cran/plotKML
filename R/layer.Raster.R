# Purpose        : Write a raster layer to KML;
# Maintainer     : Pierre Roudier (pierre.roudier@landcare.nz);
# Contributions  : Tomislav Hengl (tom.hengl@wur.nl); Dylan Beaudette (debeaudette@ucdavis.edu); 
# Status         : pre-alpha
# Note           : Rasters can also be written as polygons; see "?grid2poly";

kml_layer.Raster <- function(
  obj,  
  plot.legend = TRUE,
  metadata = NULL,
  raster_name,
  png.width = ncol(obj), 
  png.height = nrow(obj),
  min.png.width = 800,
  ...
  ){

  # get our invisible file connection from custom environment
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
    x <- data.frame(getValues(obj))
    names(x) <- names(obj)
    x <- eval(call[["colour"]], x)
    obj <- raster(obj)
    values(obj) <- x
  } else { 
  if(is.numeric(call[["colour"]])) {
    i_layer <- call[["colour"]]
    if (nlayers(obj) > 1) {
      obj <- raster(obj, layer = i_layer)
    }
  } else { 
  if(is.character(call[["colour"]])) {
    i_layer <- which(names(obj) == call[["colour"]])
    if (nlayers(obj) > 1) {
      obj <- raster(obj, layer = i_layer)
    }
  }}}

  # TH: this needs to be fixed
  altitude <- charmatch("altitude", names(call))
  if(!is.na(altitude)){
    altitude <- eval(call[["altitude"]], nlayers(obj))
  } else {
    altitude <- rep(.all_kml_aesthetics[["altitude"]], length.out = nlayers(obj))
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

  # Trying to reproject data if the check was not successful
  if(!prj.check) {  
    obj <- reproject(obj) 
  }

  if(is.factor(obj)){
    if(length(labels(obj))==0){
      warning("RasterLayer of type factor missing labels")
      colour_scale <- colorRampPalette(pal)(length(levels(as.factor(getValues(obj)))))      
    } else {
      colour_scale <- colorRampPalette(pal)(length(labels(obj)[[1]]))
    }
  } else {
    colour_scale <- colorRampPalette(pal)(100)  
  }

  # Transparency
  alpha <- charmatch("alpha", names(call))
  if (!is.na(alpha)) {
    ## - constant transparency
    ## - raster index if a Stack
    ## - name of a layer if a Stack
    colour_scale <- kml_alpha(obj, alpha = eval(call[["alpha"]], obj@data), colours = colour_scale, RGBA = TRUE)
  }

  ## Creating the PNG file
  if(missing(raster_name)){
    raster_name <- paste(normalizeFilename(as.character(call[["colour"]])), ".png", sep="")
  }

  ## Plotting the image
  if(png.width<min.png.width){
     png.height <- round(min.png.width*png.height/png.width)
     png.width <- min.png.width  
  }
  png(filename = raster_name, bg = "transparent", type="cairo-png", width=png.width, height=png.height)
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  if(!is.na(charmatch("z.lim", names(call)))){ 
    z.lim <- eval(call[["z.lim"]])
    obj <- calc(obj, fun=function(x){ x[x < z.lim[1]] <- z.lim[1]; return(x)}) 
    obj <- calc(obj, fun=function(x){ x[x > z.lim[2]] <- z.lim[2]; return(x)})
    raster::image(obj, col = colour_scale, zlim = z.lim, frame.plot = FALSE, main="", maxpixels=ncell(obj)) 
  } else {
    raster::image(obj, col = colour_scale, frame.plot = FALSE, main="", maxpixels=ncell(obj))
  }
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
    warning("PNG transparency possibly ineffective. Install ImageMagick and add to PATH. See ?kml_layer.Raster for more info.")
    }
  }

  ## plot the legend (PNG)
  if(plot.legend == TRUE){
    if(missing(raster_name)){
      legend_name <- paste(as.character(call[["colour"]]), "legend.png", sep="_")
    } else {
      legend_name <- paste(strsplit(raster_name, "\\.")[[1]][1], "legend.png", sep="_")      
    }
    if(is.factor(obj)){
      x <- as.factor(getValues(obj))
      if(length(labels(obj))==0){
        levels(x) <- levels(as.factor(getValues(obj)))
      } else {
        levels(x) = labels(obj)[[1]]
      }      
      kml_legend.bar(x = x, legend.file = legend_name, legend.pal = colour_scale)
    } else {
      if(!is.na(charmatch("z.lim", names(call)))){
        kml_legend.bar(x = getValues(obj), legend.file = legend_name, legend.pal = colour_scale, z.lim = eval(call[["z.lim"]]))
       } else {
        kml_legend.bar(x = getValues(obj), legend.file = legend_name, legend.pal = colour_scale)
       } 
    }
  }

  message("Writing to KML...")
  ## Folder name
  pl1 = newXMLNode("Folder", parent=kml.out[["Document"]])
  pl2 <- newXMLNode("name", paste(class(obj)), parent = pl1)

  ## Insert metadata:
  if(!is.null(metadata)){
    md.txt <- kml_metadata(metadata, asText = TRUE)
    txt <- sprintf('<description><![CDATA[%s]]></description>', md.txt)
    parseXMLAndAdd(txt, parent=pl1)
  }

  ## Ground overlay
  ## =====================
  pl2b <- newXMLNode("GroundOverlay", parent = pl1)
  ## Creating a SpatialPixelsDataFrame object to be plotted
  pl3 <- newXMLNode("name", deparse(call[["colour"]]), parent = pl2b)
  pl3b <- newXMLNode("altitude", signif(altitude, 4), parent = pl2b)
  pl3b <- newXMLNode("altitudeMode", altitudeMode, parent = pl2b)
  pl3c <- newXMLNode("Icon", parent = pl2b)
  pl4 <- newXMLNode("href", raster_name, parent = pl3c)
  pl3d <- newXMLNode("LatLonBox", parent = pl2b)
  pl4b <- newXMLNode("north", bbox(extent(obj))[2, 2], parent = pl3d)
  pl4c <- newXMLNode("south", bbox(extent(obj))[2, 1], parent = pl3d)
  pl4d <- newXMLNode("east", bbox(extent(obj))[1, 2], parent = pl3d)
  pl4e <- newXMLNode("west", bbox(extent(obj))[1, 1], parent = pl3d)
  
  ## Legend
  ## ======================
  if(plot.legend == TRUE){
    txtso <- sprintf('<ScreenOverlay><name>Legend</name><Icon><href>%s</href></Icon><overlayXY x="0" y="1" xunits="fraction" yunits="fraction"/><screenXY x="0" y="1" xunits="fraction" yunits="fraction"/></ScreenOverlay>', legend_name)
    parseXMLAndAdd(txtso, parent=kml.out[["Document"]])
  }
  
  ## save results: 
  assign("kml.out", kml.out, envir=plotKML.fileIO)
  
}

setMethod("kml_layer", "RasterLayer", kml_layer.Raster)
setMethod("kml_layer", "RasterStack", kml_layer.Raster)

# end of script;
