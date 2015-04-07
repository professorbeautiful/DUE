DUEinitGet <- function(objnames) {
	if(missing(objnames))
		objnames = names(.DUEenvironment$DUEinits)
	for(oname in objnames) assign(oname, .DUEenvironment$DUEinits[[oname]], envir=parent.frame())
}
