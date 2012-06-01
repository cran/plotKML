# Purpose        : Clean up / closing settings;
# Maintainer     : Tomislav Hengl (tom.hengl@wur.nl);
# Contributions  : ; 
# Status         : pre-alpha
# Note           : for more info see [http://cran.r-project.org/doc/manuals/R-exts.html];

.onLoad <- function(libname, pkgname)  {
  
  ## print on start-up:
	# pkg.info <- utils::packageDescription('plotKML')
	pkg.info <- drop(read.dcf(file=system.file("DESCRIPTION", package=pkgname,  lib.loc=libname), fields=c("Version","Date")))
	packageStartupMessage(paste(pkgname, " version ", pkg.info["Version"], " (", pkg.info["Date"], ")", sep=""))

	tst <- try( removeTmpFiles(), silent=TRUE )

  # create env variables:
  plotKML.env(show.env = FALSE)
  
  packageStartupMessage(paste("URL:", get("home_url", envir = plotKML.opts)))

  return(invisible(0))
  	
}

# end of script;