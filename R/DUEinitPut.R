DUEinitPut <- function(objnames) {
	if(missing(objnames))
		objnames = names(.DUEenvironment$DUEinits)
	for(oname in objnames) .DUEenvironment$DUEinits[[oname]] <- get(oname, envir=parent.frame())
}
