# Purpose        : Initial settings;
# Maintainer     : Tomislav Hengl (tom.hengl@wur.nl)
# Contributions  : Pierre Roudier (pierre.roudier@landcare.nz); Dylan Beaudette (debeaudette@ucdavis.edu); 
# Dev Status     : Pre-Alpha
# Note           : for more info see [http://cran.r-project.org/doc/manuals/R-exts.html]; this code was prepared for SAGA GIS 2.0.8


################## STANDARD ENVIRONMENTS ##############

## setup our environment for storing file handles and the like
plotKML.fileIO <- new.env(hash=TRUE, parent = parent.frame())

## setup the plotKML environment:
plotKML.opts <- new.env(hash=TRUE, parent = parent.frame())

# Find paths to external packages;
paths <- function(gdalwarp = "", gdal_translate = "", convert = "", saga_cmd = "", python = "", show.paths = TRUE){ 

     #  Try locating SAGA GIS (R default setting)...
     if(saga_cmd==""){
     if(suppressWarnings(!is.null(x <- rsaga.env()))){ 
     if(.Platform$OS.type == "windows") {
        saga_cmd <- shortPathName(normalizePath(paste(rsaga.env()$path, rsaga.env()$cmd, sep="/"))) 
     }
     else { 
        saga_cmd <- paste(rsaga.env()$path, rsaga.env()$cmd, sep="/") 
     } 
        if(nzchar(saga_cmd)){
          saga.version <- rsaga.get.version()
        }
     } else {
        saga.version <- ""
     }}
     
     # Try locating path to ImageMagick (R default setting)...
     if(convert==""){
     if(require(animation)){
        convert <- ani.options("convert")
     }

     # If it does not work, try getting the path from the OS:     
     if(is.null(convert)){
        im.dir <- NULL
        if(.Platform$OS.type == "windows") {
        # get paths and check for ImageMagick
        paths <- strsplit(Sys.getenv('PATH')[[1]], ";")[[1]]
        x <- grep(paths, pattern="Magick")
        
        # if present
        if(!length(x) == 0) {
          im.dir <- paths[grep(paths, pattern="ImageMagick")[1]]
          convert = shQuote(normalizePath(file.path(im.dir, "convert.exe")))
          if(show.paths){ message(system(convert,  show.output.on.console = FALSE, intern = TRUE)[1]) }
          }
        } # end checking for Imagemagick on Windows
        
        # check for all other OS:
        else{
          if(!length(x <- grep(paths <- strsplit(Sys.getenv('PATH')[[1]], ":")[[1]], pattern="Magick"))==0) {
            im.dir <- paths[grep(paths, pattern="Magick")[1]]
            convert = "convert"
            if(show.paths){ message(system(convert,  show.output.on.console = FALSE, intern = TRUE)[1])
            message("Located ImageMagick from the path") }
          }
        }
    
        if(is.null(im.dir)){ 
          warning("Install ImageMagick and add to PATH. See http://imagemagick.org for more info.")
        convert = ""
        }
     } else { 
     if(show.paths){ message(system(convert,  show.output.on.console = FALSE, intern = TRUE)[1]) }
    }
    }  
  
    # try to locate FWTools / Patyhon:
    if(.Platform$OS.type == "windows") {
    if(gdalwarp==""|gdal_translate==""){
    
     reg.paths <- names(utils::readRegistry("SOFTWARE"))
     # 64-bit software directory:
     x <- grep(reg.paths, pattern="WOW6432Node", ignore.case = TRUE)
     
     if(length(x)>0 & !inherits(try({ fw.path = utils::readRegistry(paste("SOFTWARE", reg.paths[x], "FWTools", sep="\\"))$Install_Dir }, silent = TRUE), "try-error")) {      
       if (nzchar(fw.path))  { 
          gdalwarp = shQuote(shortPathName(normalizePath(file.path(fw.path, "bin/gdalwarp.exe"))))
          gdal_translate = shQuote(shortPathName(normalizePath(file.path(fw.path, "bin/gdal_translate.exe"))))
          if(show.paths){ message(paste("Located FWTools from the Registry Hive: \"", shortPathName(fw.path), "\"", sep="")) }
       } 
     } 

     else { if(nzchar(prog <- Sys.getenv("ProgramFiles")) &&
        length(fw.dir <- list.files(prog, "^FWTools.*")) &&
        length(fw.path <- list.files(file.path(prog, fw.dir), pattern = "^gdalwarp\\.exe$", full.names = TRUE, recursive = TRUE))  |
        length(fw.path2 <- list.files(file.path(prog, fw.dir), pattern = "^gdal_translate\\.exe$", full.names = TRUE, recursive = TRUE)) )  {
          gdalwarp = shQuote(shortPathName(normalizePath(fw.path[1])))
          gdal_translate = shQuote(shortPathName(normalizePath(fw.path2[1])))
          if(show.paths){ message(paste("Located FWTools from the 'Program Files' directory: \"", shortPathName(fw.path), "\"", sep="")) }
        } else if(nzchar(prog <- Sys.getenv("ProgramFiles(x86)")) &&
            length(fw.dir <- list.files(prog, "^FWTools.*")) &&
            length(fw.path <- list.files(file.path(prog, fw.dir), pattern = "^gdalwarp\\.exe$", full.names = TRUE, recursive = TRUE))  &&
            length(fw.path2 <- list.files(file.path(prog, fw.dir), pattern = "^gdal_translate\\.exe$", full.names = TRUE, recursive = TRUE))   )
       {
        gdalwarp = shQuote(shortPathName(normalizePath(fw.path[1])))
        gdal_translate = shQuote(shortPathName(normalizePath(fw.path2[1])))
        if(show.paths){ message(paste("Located FWTools from the 'Program Files' directory: \"", shortPathName(fw.path), "\"", sep="")) }
     } 
     
     else {
      warning("Could not locate FWTools! Install program and add it to the Windows registry. See http://fwtools.maptools.org for more info.")
        gdalwarp = ""
        gdal_translate = ""    
       } 
      }}
      
      # Python:
      if(python==""){
      x <- grep(reg.paths, pattern="WOW6432Node", ignore.case = TRUE)
      if(length(x)>0 & !inherits(try({ 
        py.paths <- utils::readRegistry(paste("SOFTWARE", reg.paths[x], "Python", sep="\\"), maxdepth=3)
        py.path = utils::readRegistry(paste("SOFTWARE", reg.paths[x], "Python", names(py.paths), names(py.paths[[1]]), "InstallPath", sep="\\"))[[1]] 
        }, silent = TRUE), "try-error")) {
          if (nzchar(py.path))  { 
            python = shQuote(shortPathName(normalizePath(file.path(py.path, "python.exe"))))
            if(show.paths){ message(paste("Located Python from the Registry Hive: \"", shortPathName(py.path), "\"", sep="")) }
          } 
      } else { 
      if(!inherits(try({ 
        py.paths <- utils::readRegistry(paste("SOFTWARE", "Python", sep="\\"), maxdepth=3)
        py.path = utils::readRegistry(paste("SOFTWARE", "Python", names(py.paths), names(py.paths[[1]])[1], "InstallPath", sep="\\"))[[1]] 
        }, silent = TRUE), "try-error")) {
        if (nzchar(py.path))  { 
          python = shQuote(shortPathName(normalizePath(file.path(py.path, "python.exe"))))
          if(show.paths){ message(paste("Located Python from the Registry Hive: \"", shortPathName(py.path), "\"", sep="")) }
        }
      } 
           
      else {
        warning("Could not locate Python! Install program and add it to the Windows registry. See http://python.org for more info.")
        python = ""
      }}
      }
       
      # 2nd chance to try to locate SAGA GIS (if not on a standard path):      
      if(saga_cmd==""){
        if(!nzchar(saga_cmd)&!nzchar(saga.version)){
        if(nzchar(prog <- Sys.getenv("ProgramFiles")) &&
          length(saga.dir <- list.files(prog, "^SAGA*"))>0 &&
          length(saga_cmd <- list.files(file.path(prog, saga.dir), pattern = "^saga_cmd\\.exe$", full.names = TRUE, recursive = TRUE))>0  
          ){
        if(suppressWarnings(!is.null(myenv <- rsaga.env(path=shQuote(normalizePath(saga.dir[1])))))){ 
          saga_cmd <- shortPathName(normalizePath(paste(myenv$path, myenv$cmd, sep="/")))
          saga.version <- myenv$version 
          if(show.paths){ message(paste("Located SAGA GIS ", saga.version, " from the 'Program Files' directory: \"", shortPathName(saga_cmd), "\"", sep="")) }
     }} else{ if(nzchar(prog <- Sys.getenv("ProgramFiles(x86)")) &&
          length(saga.dir <- list.files(prog, "^SAGA*"))>0 &&
          length(saga_cmd <- list.files(file.path(prog, saga.dir), pattern = "^saga_cmd\\.exe$", full.names = TRUE, recursive = TRUE))>0   
          ) {
        if(suppressWarnings(!is.null(myenv <- rsaga.env(path=shQuote(normalizePath(saga.dir[1])))))){ 
          saga_cmd <- shortPathName(normalizePath(paste(myenv$path, myenv$cmd, sep="/")))
          saga.version <- myenv$version 
          if(show.paths){ message(paste("Located SAGA GIS ", saga.version, " from the 'Program Files' directory: \"", shortPathName(saga_cmd), "\"", sep="")) }
     }}
     }
      
     if(!nzchar(saga_cmd)){
        warning("Could not locate SAGA GIS! Install program and add it to the Windows registry. See http://www.saga-gis.org/en/ for more info.")
        saga_vc = "" 
      }   
     }
     else {
        if(show.paths){ message(paste("Located SAGA GIS ", saga.version, " from the 'Program Files' directory: \"", shortPathName(saga_cmd), "\"", sep="")) }
     }
    }
    }
    
    ## UNIX:
    else {
    
    if(gdalwarp==""|gdal_translate==""){
    if(!length(x <- grep(paths <- strsplit(Sys.getenv('PATH')[[1]], ":")[[1]], pattern="FWTools"))==0) {
    fw.dir <- paths[grep(paths, pattern="FWTools")[1]]
    gdalwarp = "gdalwarp"
    gdal_translate = "gdal_translate"
    if(show.paths){ message(paste("Located FWTools from the path: \"", fw.dir, "\"", sep="")) }
      }
    else { 
        warning("Install FWTools and add to PATH. See http://fwtools.maptools.org for more info.")
      gdalwarp = ""
      gdal_translate = ""
    }
    }
    
    if(python==""){
    if(!length(x <- grep(paths <- strsplit(Sys.getenv('PATH')[[1]], ":")[[1]], pattern="Python"))==0) {
    py.dir <- paths[grep(paths, pattern="Python")[1]]
    python = "python"
    if(show.paths){ message(paste("Located Python from the path: \"", py.dir, "\"", sep="")) }
      }
    else { 
        warning("Install Python and add to PATH. See http://python.org for more info.")
        python = ""
    }
    }
    
    if(convert==""){
    if(is.null(im.dir)){ 
        warning("Install ImageMagick and add to PATH. See http://imagemagick.org for more info.")
        convert = ""
    }
    }

    if(saga_cmd==""){
    if(!nzchar(saga_cmd)){
        warning("Install SAGA GIS and add to PATH. See http://www.saga-gis.org for more info.")
        } 
    }
    }

    lt <- data.frame(gdalwarp, gdal_translate, convert, python, saga_cmd, stringsAsFactors = FALSE)
    return(lt)
}

################## STANDARD SETTINGS ##############

plotKML.env <- function(
    colour_scale_numeric = '',
    colour_scale_factor = '',
    colour_scale_svar = '',
    ref_CRS,
    NAflag,
    icon,
    LabelScale,
    size_range,
    license_url,
    metadata_sel,
    kmz,
    kml_xsd,
    kml_url,
    kml_gx,
    gpx_xsd,
    fgdc_xsd,
    convert,
    gdalwarp,
    gdal_translate,
    python,
    home_url,
    googleAPIkey,
    show.env = TRUE,
    silent = TRUE
    ){
    
	brewer1 = c("#D7191C","#FDAE61","#FFFFBF","#ABD9E9","#2C7BB6")
    #if(missing(colour_scale_numeric)) { colour_scale_numeric <- rev(brewer.pal(n = 5, name = "RdYlBu")) }
    if(missing(colour_scale_numeric)) { colour_scale_numeric <- rev(brewer1) }
	brewer2 = c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#FFFF33","#A65628","#F781BF","#999999")
    #if(missing(colour_scale_factor)) { colour_scale_factor <- brewer.pal(n = 9, name = "Set1") }
    if(missing(colour_scale_factor)) { colour_scale_factor <- brewer2 }
	brewer3 = c("#FEEDDE","#FDBE85","#FD8D3C","#E6550D","#A63603")
    #if(missing(colour_scale_svar)) { colour_scale_svar <- brewer.pal(n = 5, name = "Oranges") }
    if(missing(colour_scale_svar)) { colour_scale_svar <- brewer3 }
    if(missing(ref_CRS)) { ref_CRS <- "+proj=longlat +datum=WGS84" }
    if(missing(NAflag)) { NAflag <- -99999 }
    if(missing(icon)) { icon <- "icon3.png" }   # "http://maps.google.com/mapfiles/kml/shapes/donut.png"
    if(missing(LabelScale)) { LabelScale <- .5 }
    if(missing(size_range)) { size_range <- c(0.25, 2.5) }
    if(missing(license_url)) { license_url <- "http://creativecommons.org/licenses/by/3.0/" }
    if(missing(metadata_sel)) { metadata_sel <- c("idinfo_citation_citeinfo_title", "idinfo_descript_abstract", "spdoinfo_ptvctinf_sdtsterm_ptvctcnt", "idinfo_timeperd_timeinfo_rngdates_begdate", "idinfo_timeperd_timeinfo_rngdates_enddate", "distinfo_stdorder_digform_digtopt_onlinopt_computer_networka_networkr", "idinfo_citation_citeinfo_othercit", "idinfo_citation_citeinfo_onlink", "idinfo_datacred", "distinfo_distrib_cntinfo_cntorgp_cntorg", "distinfo_stdorder_digform_digtinfo_formcont", "idinfo_native") }
    if(missing(kmz)) { kmz <- FALSE }
    if(missing(kml_xsd)) { kml_xsd <- "http://schemas.opengis.net/kml/2.2.0/ogckml22.xsd" }
    if(missing(kml_url)) { kml_url <- "http://www.opengis.net/kml/2.2/" }
    if(missing(kml_gx)) { kml_gx <- "http://www.google.com/kml/ext/2.2" }
    if(missing(gpx_xsd)) { gpx_xsd <- "http://www.topografix.com/GPX/1/1/gpx.xsd" }
    if(missing(fgdc_xsd)) { fgdc_xsd <- "http://fgdcxml.sourceforge.net/schema/fgdc-std-012-2002/fgdc-std-012-2002.xsd" }
    
    if(silent == FALSE){
      pts <- paths(show.paths = TRUE)
    }
    else {
      pts <- data.frame(gdalwarp="", gdal_translate="", convert="", python="", saga_cmd="", stringsAsFactors = FALSE)
    }
    
    if(missing(convert)) { convert <- pts$convert[[1]] }
    if(missing(gdalwarp)) { gdalwarp <- pts$gdalwarp[[1]] }
    if(missing(gdal_translate)) { gdal_translate <- pts$gdal_translate[[1]] }
    if(missing(python)) { python <- pts$python[[1]] }
    if(missing(home_url)) { home_url <- "http://plotkml.r-forge.r-project.org/" }
    if(missing(googleAPIkey)) { googleAPIkey <- "ABQIAAAAqHzabFRj8QwDECupLUR4-hT53Nvo2rq6JtI9-sNzq2yJTiKUYBRN5VP8pfrIcMaRo0pNDvBhWJUQCA" }
 
    assign("colour_scale_numeric", colour_scale_numeric, envir=plotKML.opts)
    assign("colour_scale_factor", colour_scale_factor, envir=plotKML.opts)
    assign("colour_scale_svar", colour_scale_svar, envir=plotKML.opts)
    assign("ref_CRS", ref_CRS, envir=plotKML.opts)
    assign("NAflag", NAflag, envir=plotKML.opts)
    assign("icon", icon, envir=plotKML.opts)
    assign("LabelScale", LabelScale, envir=plotKML.opts)
    assign("size_range", size_range, envir=plotKML.opts)
    assign("license_url", license_url, envir=plotKML.opts)
    assign("metadata_sel", metadata_sel, envir=plotKML.opts)
    assign("kmz", kmz, envir=plotKML.opts)
    assign("kml_xsd", kml_xsd, envir=plotKML.opts)
    assign("kml_url", kml_url, envir=plotKML.opts)
    assign("kml_gx", kml_gx, envir=plotKML.opts)
    assign("gpx_xsd", gpx_xsd, envir=plotKML.opts)
    assign("fgdc_xsd", fgdc_xsd, envir=plotKML.opts)
    assign("convert", convert, envir=plotKML.opts)
    assign("gdalwarp", gdalwarp, envir=plotKML.opts)
    assign("gdal_translate", gdal_translate, envir=plotKML.opts)
    assign("python", python, envir=plotKML.opts)
    assign("home_url", home_url, envir=plotKML.opts)
    assign("googleAPIkey", googleAPIkey, envir=plotKML.opts)
    
    plotKML.opts <- list(colour_scale_numeric, colour_scale_factor, colour_scale_svar, ref_CRS, NAflag, icon, LabelScale, size_range, license_url, metadata_sel, kmz, kml_xsd, kml_url, kml_gx, gpx_xsd, fgdc_xsd, convert, gdalwarp, gdal_translate, python, home_url, googleAPIkey)
    names(plotKML.opts) <- c("colour scale for numeric variables", "colour scale for factor variables", "colour scale for the standardized prediction error", "referent CRS", "NA flag value", "default icon", "default label size", "default size ranges", "default license url", "print metadata", "compress to kmz", "kml xsd URL", "kml URL", "kml gx URL", "gpx xsd URL", "fgdc xsd URL", "location of convert program", "location of gdalwarp program", "location of gdal_translate program", "location of python program", "data repository URL", "google API key")
    
    if(show.env){  return(plotKML.opts)  }
 
}

# generate all environmental settings:
plotKML.env(show.env = FALSE)

# end of script;
