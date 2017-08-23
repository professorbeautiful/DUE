.onAttach = function(libname, pkgname) {

	desc <- packageDescription(pkgname)
	packageStartupMessage("This is ", pkgname, " ", desc$Version, " ", desc$Date, "\n")
	cat("This is ", pkgname, " ", desc$Version, " ", desc$Date, "\n")
	if(basename(getwd()) == 'inst') setwd('..')
	print(getwd())
	return(invisible(NULL))
}

