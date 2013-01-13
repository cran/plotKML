# Purpose        : Writing of spatio-temporal full data frame objects to KML
# Maintainer     : Tomislav Hengl (tom.hengl@wur.nl);
# Contributions  : Erin Hodges (hodgesse@uhd.edu); 
# Status         : Alpha
# Note           : An alternative is to use the RasterBrickTime series class

kml_layer.STFDF <- function(
  obj,
  dtime = "", # time support
  ...
  ){

  ## expand object to STIDF:
  if(!is(obj@sp, "SpatialPixels")){
    obj <- as(as(obj, "STSDF"), "STIDF")
  }
  
  # Format the time slot for writing to KML:
  if(all(dtime==0)) {
    TimeSpan.begin = format(time(obj@time), "%Y-%m-%dT%H:%M:%SZ") 
    TimeSpan.end = TimeSpan.begin
  } else {
    if(length(obj@time)>1&!nzchar(dtime)){
      if(!is(obj@sp, "SpatialPixels")){
        period <- periodicity(obj@time) # estimate the time support (if not indicated)
        dtime <- period$frequency
        TimeSpan.begin <- format(as.POSIXct(unclass(as.POSIXct(time(obj@time))) - dtime/2, origin="1970-01-01"), "%Y-%m-%dT%H:%M:%SZ")
        TimeSpan.end <- format(as.POSIXct(unclass(as.POSIXct(time(obj@time))) + dtime/2, origin="1970-01-01"), "%Y-%m-%dT%H:%M:%SZ")
      } else {
        message("Temporal support not indicated... using 'dtime=0'.")
        TimeSpan.begin = format(time(obj@time), "%Y-%m-%dT%H:%M:%SZ")
        TimeSpan.end = TimeSpan.begin
      }
    }
  }
  
  ## Check the data type:
  if(is(obj@sp, "SpatialPixels")){
  	# construct stack of rasters:
  	r <- brick(lapply(1:length(obj@time), function(x){raster(obj[,x])}))
    r <- setZ(r, paste(as.POSIXct(unclass(as.POSIXct(TimeSpan.begin))+(unclass(as.POSIXct(TimeSpan.end))-unclass(as.POSIXct(TimeSpan.begin)))/2, origin="1970-01-01")))
  	dtime = unclass(as.POSIXct(TimeSpan.end)) - unclass(as.POSIXct(TimeSpan.begin))
    kml_layer.RasterBrick(obj = r, dtime=dtime, ...)  
 	} else {
    if (is(obj@sp, "SpatialPoints")) { 
      sp <- SpatialPointsDataFrame(obj@sp, obj@data)
      kml_layer.SpatialPoints(obj = sp, TimeSpan.begin = TimeSpan.begin, TimeSpan.end = TimeSpan.end,  ...)
    } 
    else {
      if(class(obj@sp)=="SpatialPolygons"|class(obj@sp)=="SpatialPolygonsDataFrame"){
        sp <- SpatialPolygonsDataFrame(obj@sp, obj@data)   
        kml_layer.SpatialPolygons(obj = sp, TimeSpan.begin = TimeSpan.begin, TimeSpan.end = TimeSpan.end,  ...)  
      }
      else {
        if(class(obj@sp)=="SpatialLines"|class(obj@sp)=="SpatialLinesDataFrame"){
          sp <- SpatialLinesDataFrame(obj@sp, obj@data)   
          kml_layer.SpatialLines(obj = sp, TimeSpan.begin = TimeSpan.begin, TimeSpan.end = TimeSpan.end,  ...)
        }
  else { 
  stop("The STFDF object does not extend SpatialPixels*, SpatialPoints*, SpatialLines* or SpatialPolygons*")
  }}}}

}

setMethod("kml_layer", "STFDF", kml_layer.STFDF)

# end of script;
