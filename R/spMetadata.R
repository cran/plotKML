# Purpose        : Automated generation of (spatial) metadata
# Maintainer     : Tomislav Hengl (tom.hengl@wur.nl);
# Contributions  : Dylan Beaudette (debeaudette@ucdavis.edu); Pierre Roudier (pierre.roudier@landcare.nz); 
# Dev Status     : Pre-Alpha
# Note           : Based on the US gov sp metadata standards [http://www.fgdc.gov/metadata/csdgm/], which can be converted to "ISO 19139" XML schema;

## internal methods:
setMethod("GetNames", "SpatialMetadata", function(obj){paste(obj@field.names)})
setMethod("GetPalette", "SpatialMetadata", function(obj){obj@palette})

## Generate a spMetadata class object:
spMetadata.Spatial <- function(
    obj,   
    xml.file, # optional metadata file in the FGDC format
    generate.missing = TRUE,
    Citation_title,
    Target_variable,  
    Attribute_Measurement_Resolution = 1, # numeric resolution
    Attribute_Units_of_Measure = "NA", # measurement units
    Indirect_Spatial_Reference = "",
    GoogleGeocode = FALSE,
    Enduser_license_URL = get("license_url", envir = plotKML.opts),
    signif.digit = 3,
    colour_scale,
    bounds,
    legend_names,
    icons,
    validate.schema = FALSE
    )
    {
          
    # Use the first column for metadata: 
    if(missing(Target_variable)){ Target_variable <- names(obj)[1] }
    if(!("data" %in% slotNames(obj))){
      stop("'Data' slot required")
    }  
    if(!(Target_variable %in% names(obj@data))){
      stop("'Target_variable' not available in the attribute table")
    }
    if(missing(xml.file)){ xml.file <- set.file.extension(normalizeFilename(deparse(substitute(obj, env=parent.frame()))), ".xml") }
        
    if(generate.missing == TRUE){
      # Metadata template:
      fgdc <- xmlTreeParse(system.file("FGDC.xml", package="plotKML"), useInternalNodes = TRUE)
      top <- xmlRoot(fgdc)
      nx <- names(unlist(xmlToList(top, addAttributes=FALSE)))
    }
    
    # If the metadata file does not exit, use the template:
    if(!is.na(file.info(xml.file)$size)){
        message(paste("Reading the metadata file: ", xml.file, sep=""))
        ret <- xmlTreeParse(xml.file)  
        a <- xmlTreeParse(xml.file, useInternalNodes = TRUE)
        
        if(validate.schema == TRUE){
        message("Connecting to the FGDC schema...")
        try(xsd <- xmlTreeParse(get("fgdc_xsd", envir = plotKML.opts), isSchema =TRUE, useInternalNodes = TRUE))
        # validate if the schema is OK:
        try(xsd.val <- xmlSchemaValidate(xsd, a))
        # print the results of validation:
        try(x <- sapply(xsd.val$errors, function(x){x["msg"][[1]]}))
          if(nzchar(x)){
          warning(paste(x), call. = FALSE)
          }
        }
    }
    
    # If the metadata file does not exit, use the template:
    else {  
        warning(paste("Could not locate ", xml.file, ". Using FGDC.xml. See '?spMetadata' for more details.", sep=""), call.=FALSE)
        ret <- xmlTreeParse(system.file("FGDC.xml", package="plotKML"))
        a <- xmlTreeParse(system.file("FGDC.xml", package="plotKML"), useInternalNodes = TRUE)
    }  
    
    ml <- xmlRoot(ret)
    
    if(generate.missing == TRUE){
    # compare the actual xml file and the template:
    cross <- compareXMLDocs(a=a, b=fgdc)
            
    # Merge the existing Metadata file with FGDC and add missing nodes:
    if(length(cross[["inB"]])>0){
    for(i in 1:length(cross[["inB"]])){
        # position of the missing node in the target doc:
        nodn <- attr(cross[["inB"]], "dimnames")[[1]][i]
        x_l <- strsplit(nx[grep(nodn, nx)], "\\.")[[1]]
        
        # TH: This is not the best implementation :(  -> it takes ca 3-5 seconds;
        for(j in 1:length(x_l)){
          if(j==1 & !x_l[j] %in% names(ml)) { ml <- append.XMLNode(ml, xmlNode(x_l[j], "")) }
          if(j==2 & !x_l[j] %in% names(ml[[x_l[1]]])) { ml[[x_l[1]]] <- append.XMLNode(ml[[x_l[1]]], xmlNode(x_l[j], "")) }
          if(j==3 & !x_l[j] %in% names(ml[[x_l[1]]][[x_l[2]]])) { ml[[x_l[1]]][[x_l[2]]] <- append.XMLNode(ml[[x_l[1]]][[x_l[2]]], xmlNode(x_l[j], "")) }
          if(j==4 & !x_l[j] %in% names(ml[[x_l[1]]][[x_l[2]]][[x_l[3]]])) { ml[[x_l[1]]][[x_l[2]]][[x_l[3]]] <- append.XMLNode(ml[[x_l[1]]][[x_l[2]]][[x_l[3]]], xmlNode(x_l[j], "")) }    
          if(j==5 & !x_l[j] %in% names(ml[[x_l[1]]][[x_l[2]]][[x_l[3]]][[x_l[4]]])) { ml[[x_l[1]]][[x_l[2]]][[x_l[3]]][[x_l[4]]] <- append.XMLNode(ml[[x_l[1]]][[x_l[2]]][[x_l[3]]][[x_l[4]]], xmlNode(x_l[j], "")) } 
          if(j==6 & !x_l[j] %in% names(ml[[x_l[1]]][[x_l[2]]][[x_l[3]]][[x_l[4]]][[x_l[5]]])) { ml[[x_l[1]]][[x_l[2]]][[x_l[3]]][[x_l[4]]][[x_l[5]]] <- append.XMLNode(ml[[x_l[1]]][[x_l[2]]][[x_l[3]]][[x_l[4]]][[x_l[5]]], xmlNode(x_l[j], "")) }
          if(j==7 & !x_l[j] %in% names(ml[[x_l[1]]][[x_l[2]]][[x_l[3]]][[x_l[4]]][[x_l[5]]][[x_l[6]]])) { ml[[x_l[1]]][[x_l[2]]][[x_l[3]]][[x_l[4]]][[x_l[5]]][[x_l[6]]] <- append.XMLNode(ml[[x_l[1]]][[x_l[2]]][[x_l[3]]][[x_l[4]]][[x_l[5]]][[x_l[6]]], xmlNode(x_l[j], "")) }
        }
    }}
               
    message("Generating metadata...")
    if(xmlValue(ml[["idinfo"]][["citation"]][["citeinfo"]][["title"]]) == "") {
    if(missing(Citation_title)) {
      xmlValue(ml[["idinfo"]][["citation"]][["citeinfo"]][["title"]]) <- normalizeFilename(deparse(substitute(obj)))
      }
      else { 
      xmlValue(ml[["idinfo"]][["citation"]][["citeinfo"]][["title"]]) <- Citation_title
      }
    } 
    xmlValue(ml[["distinfo"]][["stdorder"]][["digform"]][["digtinfo"]][["formcont"]]) <- class(obj)[1]
    xmlValue(ml[["eainfo"]][["overview"]][["eadetcit"]]) <- paste("http://CRAN.R-project.org/package=", attr(class(obj), "package"), "/", sep="")
    
    xmlValue(ml[["eainfo"]][["detailed"]][["enttyp"]][["enttypl"]]) <- class(obj@data[,Target_variable])
    
    if(is.numeric(obj@data[,Target_variable])){
    xmlValue(ml[["eainfo"]][["detailed"]][["attr"]][["attrdomv"]][["rdom"]][["rdommin"]]) <- range(obj@data[,Target_variable], na.rm=TRUE)[1] 
    xmlValue(ml[["eainfo"]][["detailed"]][["attr"]][["attrdomv"]][["rdom"]][["rdommax"]]) <- range(obj@data[,Target_variable], na.rm=TRUE)[2]
    }  
    
    xmlValue(ml[["eainfo"]][["detailed"]][["attr"]][["attrdef"]]) <- Target_variable
    xmlValue(ml[["eainfo"]][["detailed"]][["attr"]][["attrdomv"]][["rdom"]][["attrmres"]]) <- Attribute_Measurement_Resolution
    xmlValue(ml[["eainfo"]][["detailed"]][["attr"]][["attrdomv"]][["rdom"]][["attrunit"]]) <- Attribute_Units_of_Measure
    xmlValue(ml[["distinfo"]][["stdorder"]][["digform"]][["digtinfo"]][["transize"]]) <-  paste(signif(object.size(obj@data[, Target_variable])/1024, 4), "Kb (uncompressed)") 
    
    if(xmlValue(ml[["metainfo"]][["metd"]]) == "") {
    xmlValue(ml[["metainfo"]][["metd"]]) <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
    }
    
    # estimate the bounding box:
    message("Estimating the bounding box coordinates...")
    obj.ll <- reproject(obj)
    xmlValue(ml[["idinfo"]][["spdom"]][["bounding"]][["westbc"]]) <- min(coordinates(obj.ll)[,1])
    xmlValue(ml[["idinfo"]][["spdom"]][["bounding"]][["eastbc"]]) <- max(coordinates(obj.ll)[,1])
    xmlValue(ml[["idinfo"]][["spdom"]][["bounding"]][["northbc"]]) <- max(coordinates(obj.ll)[,2])
    xmlValue(ml[["idinfo"]][["spdom"]][["bounding"]][["southbc"]]) <- min(coordinates(obj.ll)[,2])  
    xmlValue(ml[["spref"]][["horizsys"]][["geograph"]][["geogunit"]]) <- "Decimal degrees"
    xmlValue(ml[["spdoinfo"]][["ptvctinf"]][["sdtsterm"]][["ptvctcnt"]]) <- nrow(obj@data)

    if(Indirect_Spatial_Reference == "" & GoogleGeocode == TRUE){
    require(rjson)
    googleurl <- url(paste("http://maps.googleapis.com/maps/api/geocode/json?latlng=",  mean(obj.ll@bbox[,1]), ",", mean(obj.ll@bbox[,2]), "&sensor=false", sep=""))
    try(Indirect_Spatial_Reference <- fromJSON(file=googleurl)[["results"]][[1]][["formatted_address"]])
    close(googleurl)
    }
        
    xmlValue(ml[["spdoinfo"]][["indspref"]]) <- Indirect_Spatial_Reference
    xmlValue(ml[["idinfo"]][["native"]]) <- paste(R.version$version.string, "running on", Sys.info()[["sysname"]], Sys.info()[["release"]])
    if(xmlValue(ml[["metainfo"]][["metc"]][["cntinfo"]][["cntperp"]][["cntorg"]]) == ""){
      xmlValue(ml[["metainfo"]][["metc"]][["cntinfo"]][["cntperp"]][["cntorg"]]) <- Sys.getenv(c("USERDNSDOMAIN"))[[1]]  
    }
    if(xmlValue(ml[["metainfo"]][["metc"]][["cntinfo"]][["cntperp"]][["cntper"]]) == ""){
      xmlValue(ml[["metainfo"]][["metc"]][["cntinfo"]][["cntperp"]][["cntper"]]) <- paste(Sys.getenv(c("USERNAME"))[[1]], "(username)", Sys.getenv(c("COMPUTERNAME"))[[1]], "(computer name)")
    }
    }

    # attach friendly metadata names:
    ny <- unlist(xmlToList(ml, addAttributes=FALSE))
    # convert to a table:
    met <- data.frame(metadata=gsub("\\.", "_", names(ny)), value=paste(ny))
    # add friendly names:
    mdnames <- read.table(system.file("mdnames.csv", package="plotKML"), sep=";")
    field_names <- merge(met, mdnames[,c("metadata","field.names")], by="metadata", all.x=TRUE, all.y=FALSE)[,"field.names"]
    
    # generate metadata doc:
    f = tempfile()
    saveXML(ml, f)
    doc = xmlInternalTreeParse(f)
    
    # color palette:
    if(missing(bounds)){
    if(is.numeric(obj@data[,Target_variable])){
        bounds <- seq(range(obj@data[,Target_variable], na.rm = TRUE, finite = TRUE)[1], range(obj@data[,Target_variable], na.rm = TRUE, finite = TRUE)[2], Attribute_Measurement_Resolution/2)  ## half the numeric resolution!
        bounds.c <- signif((bounds[-1]+bounds[-length(bounds)])/2, signif.digit)
        if(missing(legend_names)) { legend_names <- as.character(bounds.c) }
    } 
    else { 
        x <- as.factor(obj@data[,Target_variable])
        bounds <- c(0, seq_along(levels(x)))
        bounds.c <- bounds[-1]
        if(missing(legend_names)) { legend_names <- as.character(levels(x)) }   
    } }
    
    # generate a palette:
    if(missing(colour_scale)){
    if(is.numeric(obj@data[,Target_variable])){
        colour_scale <- get("colour_scale_numeric", envir = plotKML.opts)
    }
    else { 
        colour_scale <- get("colour_scale_factor", envir = plotKML.opts) 
    }
    }
    
    if(missing(icons)){ 
        icons <- rep("", length(legend_names))
    }
    
    cols <- colorRamp(colour_scale, space = "rgb", interpolate = "linear")
    cdata <- scales::rescale(bounds.c)
    color <- rgb(cols(cdata)/255)
    
    # make a spatial palette:
    pal <- new("sp.palette", type=class(obj@data[,Target_variable]), bounds=bounds, color = color, names = legend_names, icons = icons)
    # make a SpatialMetadata object:
    spmd <- new("SpatialMetadata",  xml=doc, field.names=paste(field_names), palette=pal, sp=as(obj, "Spatial")) 
    return(spmd)

}

spMetadata.Raster <- function(obj, Target_variable, bounds = NULL, color = NULL, ...){
    
    if(!is.null(bounds)) {bounds <- obj@legend@values}
    if(!is.null(color)) {color <- obj@legend@color}

    # convert a Raster layer to SGDF:
    if(nlayers(obj) > 1){
      if(missing(Target_variable)) {
      i_layer <- 1
      }
      else {
      i_layer <- which(names(obj) == Target_variable)
      }
      obj <- raster(obj, layer = i_layer)
    }
    obj <- as(obj, "SpatialGridDataFrame") 
    
    spMetadata.Spatial(obj, ...)
}

setMethod("spMetadata", "Spatial", spMetadata.Spatial)
setMethod("spMetadata", "RasterLayer", spMetadata.Raster)


## Read metadata from a xml.file and convert to a table:
read.metadata <- function(xml.file, delim.sign = "_", full.names){

    if(missing(full.names)){    
      full.names = read.table(system.file("mdnames.csv", package="plotKML"), sep=";")      
    }
    ret <- xmlTreeParse(xml.file, useInternalNodes = TRUE)
    top <- xmlRoot(ret)
    nx <- unlist(xmlToList(top, addAttributes=FALSE))
    # convert to a table:
    met <- data.frame(metadata=gsub("\\.", delim.sign, attr(nx, "names")), value=paste(nx), stringsAsFactors = FALSE)
    # add more friendly names:
    metm <- merge(x=met, y=full.names[,c("metadata","field.names")], by="metadata", all.x=TRUE)
    
    return(metm[,c(1,3,2)])
}

# end of script;