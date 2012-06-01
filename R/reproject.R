# Purpose        : Automatic reprojection of vector and raster features to geographic coordinates;
# Maintainer     : Pierre Roudier (pierre.roudier@landcare.nz);
# Contributions  : Tomislav Hengl (tom.hengl@wur.nl); Dylan Beaudette (debeaudette@ucdavis.edu); 
# Status         : tested
# Note           : in the case of gridded data, bounding box and cell size are estimated by the program (raster / FWTools);


reproject.SpatialPoints <- function(obj, CRS = get("ref_CRS", envir = plotKML.opts), ...) {
  require(rgdal)
  message(paste("Reprojecting to", CRS, "..."))
  res <- spTransform(x = obj, CRSobj = CRS(CRS))
  return(res)
}


reproject.RasterStack <- function(obj, CRS = get("ref_CRS", envir = plotKML.opts), ...) {
  rs <- stack(lapply(obj@layers, reproject, CRS = CRS, ...))
  return(rs)
}


reproject.RasterBrick <- function(obj, CRS = get("ref_CRS", envir = plotKML.opts), ...) {
  r <- stack(obj)
  rs <- brick(lapply(r@layers, reproject, CRS = CRS, ...))
  return(rs)
}


reproject.RasterLayer <- function(obj, CRS = get("ref_CRS", envir = plotKML.opts), program = "raster", tmp.file = TRUE, NAflag = get("NAflag", envir = plotKML.opts), show.output.on.console = FALSE, ...) {

  if (is.factor(obj)){  
  method <- "ngb" }
  else {  
  method <- "bilinear" }
  
  if(program=="raster"){
  message(paste("Reprojecting to", CRS, "..."))
  res <- projectRaster(obj, crs = CRS, method = method, progress='text', ...)
  layerNames(res) <- layerNames(obj)
  }
  
  if(program=="FWTools"){
  gdalwarp <- get("gdalwarp", envir = plotKML.opts)
  
  # look for FWTools path:  
  if(nchar(gdalwarp)==0){
    plotKML.env(silent = FALSE, show.env = FALSE)
    gdalwarp <- get("gdalwarp", envir = plotKML.opts)
  }
  
  if(tmp.file==TRUE){
        tf <- tempfile() 
        }
        else { 
        tf <- normalizeFilename(deparse(substitute(obj, env = parent.frame())))
        }
  
  if(!nchar(gdalwarp)==0){
    if(method == "ngb") { method <- "near" }
    writeRaster(obj, paste(tf, ".tif", sep=""), overwrite=TRUE, NAflag=NAflag)
    # resample to WGS84 system:
    message(paste("Reprojecting to", CRS, "..."))
    system(paste(gdalwarp, " ", tf, ".tif", " -t_srs \"", CRS, "\" ", tf, "_ll.tif -dstnodata \"", NAflag, "\" ", " -r ", method, sep=""), show.output.on.console = show.output.on.console)
    res <- raster(paste(tf, "_ll.tif", sep=""), silent = TRUE)
    layerNames(res) <- layerNames(obj)
  }
  else {
  stop("Could not locate FWTools. See 'plotKML.env()' for more info.") }
  }
  
  return(res)
}


reproject.SpatialGrid <- function(obj, CRS = get("ref_CRS", envir = plotKML.opts), tmp.file = TRUE, program = "raster", NAflag = get("NAflag", envir = plotKML.opts), show.output.on.console = FALSE, ...) {

  if(program=="raster"){

    # if multiple layers:
    if (ncol(obj) > 1) {

      r <- stack(obj)
      r <- stack(lapply(r@layers, reproject, CRS = CRS, ...))
      res <- as(r, "SpatialGridDataFrame")
    ## TH: time consuming but preferred:
    #  res <- as(res, "SpatialPixelsDataFrame")
      names(res) <- names(obj)
    }

    # single layer:
    else {
      r <- raster(obj)
      res <- as(reproject(r, CRS = CRS, ...), "SpatialGridDataFrame")
      names(res) <- names(obj)
    }
  }
  
  if(program=="FWTools"){
  gdalwarp <- get("gdalwarp", envir = plotKML.opts)
  require(rgdal)
  
  # look for FWTools path:  
  if(nchar(gdalwarp)==0){
    plotKML.env(silent = FALSE)
    gdalwarp <- get("gdalwarp", envir = plotKML.opts)
  }
  
  if(!nchar(gdalwarp)==0){
  
    for(i in 1:ncol(obj)){
  
    if(tmp.file==TRUE){
        tf <- tempfile() 
        }
        else { 
        tf <- paste(normalizeFilename(deparse(substitute(obj, env = parent.frame()))), names(obj)[i], sep="_")
       }

        # write SPDF to a file:
        if(is.factor(obj@data[,i])){
        x <- obj[i]
        x@data[,1] <- as.integer(x@data[,1])
        writeGDAL(x, paste(tf, ".tif", sep=""), "GTiff")
        }        
        else {
        writeGDAL(obj[i], paste(tf, ".tif", sep=""), "GTiff")
        }
        
        message(paste("Reprojecting to", CRS, "..."))
        # resample to WGS84 system:
        if(is.factor(obj@data[,i])){
        system(paste(gdalwarp, ' ', tf, '.tif', ' -t_srs \"', CRS, '\" ', tf, '_ll.tif -dstnodata \"', NAflag, '\" -r near', sep=""), show.output.on.console = show.output.on.console)
        }
        else {
        system(paste(gdalwarp, ' ', tf, '.tif', ' -t_srs \"', CRS, '\" ', tf, '_ll.tif -dstnodata \"', NAflag, '\" -r bilinear', sep=""), show.output.on.console = show.output.on.console)
        }
        # read images back to R:
        if(i==1){
        res <- readGDAL(paste(tf, "_ll.tif", sep=""), silent = TRUE)
        names(res) <- names(obj)[i]
        }
        else{
        res@data[names(obj)[i]] <- readGDAL(paste(tf, "_ll.tif", sep=""), silent = TRUE)$band1
        }
        
        # reformat to the original factors:
        if(is.factor(obj@data[,i])){
        res@data[,i] <- factor(res@data[,i], levels=levels(obj@data[,i]))
        }
      }
  } 
  
  else {
  stop("Could not locate FWTools. See 'plotKML.env()' for more info.") }
  
  } 
  
  ## TH: time consuming but preferred:
  # res <- as(res, "SpatialPixelsDataFrame")
  return(res)
}

# connect all methods and classes:
setMethod("reproject", signature="SpatialPoints", definition=reproject.SpatialPoints)
setMethod("reproject", signature="SpatialPolygons", definition=reproject.SpatialPoints)
setMethod("reproject", signature="SpatialLines", definition=reproject.SpatialPoints)
setMethod("reproject", signature="RasterStack", definition=reproject.RasterStack)
setMethod("reproject", signature="RasterLayer", definition=reproject.RasterLayer)
setMethod("reproject", signature="RasterBrick", definition=reproject.RasterBrick)
setMethod("reproject", signature="SpatialGridDataFrame", definition=reproject.SpatialGrid)
setMethod("reproject", signature="SpatialPixelsDataFrame", definition=reproject.SpatialGrid)


# end of script;