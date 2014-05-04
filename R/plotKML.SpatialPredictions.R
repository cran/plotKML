# Purpose        : Generic methods to plot geostatistical mapping views in KML (a combination of objects)
# Maintainer     : Tomislav Hengl (tom.hengl@wur.nl); 
# Contributions  : Dylan Beaudette (debeaudette@ucdavis.edu); Pierre Roudier (pierre.roudier@landcare.nz);
# Dev Status     : Alpha
# Note           : standard geostat plot;


setMethod("plotKML", "SpatialPredictions", function(
  obj,
  folder.name = normalizeFilename(deparse(substitute(obj, env=parent.frame()))),
  file.name = paste(folder.name, ".kml", sep=""),
  colour,
  scale_svar = get("colour_scale_svar", envir = plotKML.opts),
  grid2poly = FALSE,
  obj.summary = TRUE,
  plot.svar = FALSE,
  pngwidth = 210, 
  pngheight = 580,
  pngpointsize = 14,
  metadata = NULL,
  kmz = get("kmz", envir = plotKML.opts),
  ...
){

  ## Guess aesthetics if missing:
  varname <- paste(obj@variable)
  if(missing(colour)){ 
    obj@predicted@data[,"colour"] <- obj@predicted@data[,varname]
  } else {
    if(is.name(colour)|is.call(colour)){
      obj@predicted@data[,"colour"] <- eval(colour, obj@predicted@data)
    } else {
      obj@predicted@data[,"colour"] <- obj@predicted@data[,as.character(colour)]      
    }
  }
  pred <- obj@predicted["colour"]
  
  ## plot the prediction variance?
  if(plot.svar == TRUE){
    svarname <- paste(obj@variable, ".", "svar", sep="")
    svar <- obj@predicted[svarname]
    names(svar) = "variance"
  }
  # sampling locations:
  locs <- obj@observed
  labs <- paste(signif(locs@data[,varname], 3))
    
  # summary properties of the RK model:
  if(obj.summary==TRUE){
    xx <- summary(obj)
    xd <- unlist(xx[!names(xx) %in% c("bonds", "breaks")])
    md <- data.frame(Names=attr(xd, "names"), Values=xd,  stringsAsFactors = FALSE)
    html <- kml_description(md, asText = TRUE, cwidth = 120, twidth = 240)
  } 
  
  if(grid2poly == TRUE){
    pol <- grid2poly(pred)
  }

  kml_open(folder.name = folder.name, file.name = file.name)
  
  if(obj.summary==TRUE){
    # add a description for the whole folder:
    kml.out <- get("kml.out", envir=plotKML.fileIO)
    description_txt <- sprintf('<description><![CDATA[%s]]></description>', html)
    parseXMLAndAdd(description_txt, parent=kml.out[["Document"]])  
    assign('kml.out', kml.out, envir=plotKML.fileIO)
  }
  
  ## Parsing the call:
  call.lst <- substitute(list(...))
  # call.lst <- as.list(call.lst)[-1]
  print(call.lst)
  
  if(grid2poly == TRUE){ 
    kml_layer(pol, colour = colour, ...)
  }
  else {
    kml_layer(pred, colour = colour, raster_name = paste(varname, "_predicted.png", sep=""), metadata = metadata, ...)
  }

  if(plot.svar==TRUE){
    kml_layer(obj = svar, colour = variance, colour_scale = colour_scale_svar, raster_name = paste(svarname, "_svar.png", sep=""), plot.legend = FALSE)  
  }
  
  kml_layer(obj = locs, points_names = labs)  

  # plot the correlation graph and variogram:
  if(all(!is.na(obj@validation$var1.pred)) & all(!is.na(obj@validation$observed))){
    png(filename=paste(varname, "_gstatplots.png", sep=""), width=pngwidth, height=pngheight, bg="white", pointsize=pngpointsize)
    par(mfrow=c(3,1))
    par(mar=c(4.5,4.5,.8,.8))
    plot(y=obj@validation$var1.pred, x=obj@validation$observed, pch=19, asp=1, col = "red", xlab='observed', col.main = rgb(0.99,0.99,0.99), ylab='predicted')
    vv <- variogram(as.formula("observed~1"), obj@validation)
    plot(x=vv$dist, y=vv$gamma, pch="+", col = "green", xlab='distance', cex=1.4, ylab='gamma', ylim = c(0, max(vv$gamma)))
    lines(x=c(0, vv$dist), y=rep(var(obj@validation$observed), length(vv$dist)+1), lty=2)
    # Residuals:
    vgmmodel = obj@vgmModel
    class(vgmmodel) <- c("variogramModel", "data.frame")
    vvres <- variogram(as.formula(paste(paste(varname, "residual", sep="."), "~ 1")), obj@observed[paste(varname, "residual", sep=".")]) # model residuals
    vve <- variogram(residual ~ 1, obj@validation["residual"]) # validation residuals
    plot(x=vvres$dist, y=vvres$gamma, pch="+", col = "green", xlab='distance', cex=1.4, ylab='gamma', ylim = c(0, max(vv$gamma)))
    points(x=vve$dist, y=vve$gamma, pch="+", col = "red", cex=1.4)
    require(gstat)
    vline <- variogramLine(vgmmodel, maxdist=max(vvres$dist), n=length(vvres$dist))
    lines(x=vline$dist, y=vline$gamma, col = "green", lwd=2)
    dev.off()
  
    # add the variogram plot:
    kml_screen(image.file = paste(varname, "_gstatplots.png", sep=""), position = "LL", sname = "gstatModel summary plot")
  }

  # close the file:
  kml_close(file.name = file.name)
  if (kmz == TRUE){
      kml_compress(file.name = file.name)
  }
  # open KML file in the default browser:
  kml_View(file.name)
})


# end of script;