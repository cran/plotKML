# Purpose        : Writing of irregular space-time objects to KML
# Maintainer     : Tomislav Hengl (tom.hengl@wur.nl);
# Contributions  : Pierre Roudier (pierre.roudier@landcare.nz); Dylan Beaudette (debeaudette@ucdavis.edu); 
# Status         : Pre-Alpha
# Note           : This method works only with the Space time irregular data frame class objects from the spacetime package;

kml_layer.STIDF <- function(
  obj,
  dtime = "", # time support
  ...
  ){

  require(xts)
  # Format the time slot for writing to KML:
  if(all(dtime==0)) {  
    TimeSpan.begin = format(time(obj@time), "%Y-%m-%dT%H:%M:%SZ")
    TimeSpan.end = TimeSpan.begin
  }
  else {
    if(length(obj@time)>1&!nzchar(dtime)){
      period <- periodicity(obj@time) # estimate the time support (if not indicated)
      dtime <- period$frequency
    }
    TimeSpan.begin <- format(as.POSIXct(unclass(as.POSIXct(time(obj@time))) - dtime/2, origin="1970-01-01"), "%Y-%m-%dT%H:%M:%SZ")
    TimeSpan.end <- format(as.POSIXct(unclass(as.POSIXct(time(obj@time))) + dtime/2, origin="1970-01-01"), "%Y-%m-%dT%H:%M:%SZ")
  }
  
  # Check the data type:
  if(class(obj@sp)=="SpatialPoints"|class(obj@sp)=="SpatialPointsDataFrame"){
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
  stop("The STIDF object does not extend SpatialPoints*, SpatialLines* or SpatialPolygons*")
  }}}

}

setMethod("kml_layer", "STIDF", kml_layer.STIDF)

# end of script;