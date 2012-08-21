# Purpose        : Generic method to plot simulated equiprobable vectors in Google Earth 
# Maintainer     : Tomislav Hengl (tom.hengl@wur.nl); 
# Contributions  : ;
# Dev Status     : Alpha
# Note           : it basically requires only a single input object;


## get a summary of an object for a list of lines:
setMethod("aggregate", signature(x = "GridTopology"), function(x, vectL, ...){
    # rasterize each line separately:
    sg <- SpatialGridDataFrame(x, proj4string = vectL[[1]]@proj4string, data=data.frame(observed=rep(NA, x@cells.dim[1]*x@cells.dim[2]), observed.sd=rep(NA, x@cells.dim[1]*x@cells.dim[2])))
    xv <- NULL
    for(i in 1:length(vectL)){
     vv <- vectL[[i]]
     vv$x <- rep(1, nrow(vv))
     xv[[i]]  <- vect2rast(vv, fname="x", cell.size=sg@grid@cellsize[1], bbox=sg@bbox, ...)@data
    }
    # bind all rasters:
    xv <- do.call(cbind, xv)
    
    sg$observed <- rowSums(xv, na.rm=T, dims=1)/length(xv)
    sg$observed.sd <- ifelse(sg$observed==0|sg$observed==1, 0, -sg$observed*log2(sg$observed)-(1-sg$observed)*log2(1-sg$observed))  ## information entropy (H) of a Bernoulli trial 
    new("SpatialVectorsSimulations", realizations = vectL, summaries = sg)
})


## plot object:
setMethod("plotKML", "SpatialVectorsSimulations", function(
  obj,
  folder.name = normalizeFilename(deparse(substitute(obj, env=parent.frame()))),
  file.name = paste(normalizeFilename(deparse(substitute(obj, env=parent.frame()))), ".kml", sep=""),
  colour_scale_svar = get("colour_scale_svar", envir = plotKML.opts),
  grid2poly = FALSE,
  obj.summary = TRUE,
  plot.svar = FALSE,  
  observed = names(obj@summaries)[1],
  observed.sd = names(obj@summaries)[2],
  kmz = TRUE,
  ...
){

  # mask out 0 pixels
  obj@summaries@data[,observed] <- ifelse(obj@summaries@data[,observed]==0, NA, obj@summaries@data[,observed])
  N.r <- length(obj@realizations)
  obs <- obj@summaries[observed]

  # summary properties of the RK model:
  if(obj.summary==TRUE){
    sel <- obj@summaries@data[,observed]>0
    md <- data.frame(Names=c("N.realizations", "avg.probability", "N.pixels"), Values=c(N.r, signif(mean(obj@summaries@data[sel,observed], na.rm=TRUE), 3), sum(sel)), stringsAsFactors = FALSE)
    html <- kml_description(md, asText = TRUE, cwidth = 120, twidth = 240)
  }
  
  if(grid2poly == TRUE){
    pol <- grid2poly(obs)
  }

  kml_open(folder.name = folder.name, file.name = file.name)
  
  # add a description for the whole folder:
  kml.out <- get("kml.out", envir=plotKML.fileIO)
  description_txt <- sprintf('<description><![CDATA[%s]]></description>', html)
  parseXMLAndAdd(description_txt, parent=kml.out[["Document"]])  
  assign('kml.out', kml.out, envir=plotKML.fileIO)
  
  if(grid2poly == TRUE){  
    kml_layer(obj = pol, colour = observed, ...)
  }
  else {
    kml_layer(obj = obs, z.lim = c(0,1), colour = observed, raster_name = paste(folder.name, "_observed.png", sep=""), ...)
  }

  if(plot.svar==TRUE){
    sums <- obj@summaries[observed.sd]
    kml_layer(obj = sums, colour = observed.sd, colour_scale = colour_scale_svar, raster_name = paste(folder.name, "_observed.sd.png", sep=""), plot.legend = FALSE)  
  }
  
  # Realizations:
  for(i in 1:N.r){
    rel <- obj@realizations[[i]]
    kml_layer(obj = rel, TimeSpan.begin = i, TimeSpan.end = i+1)
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