DUEcopy <- function(objnames) {
	# Purpose: to copy objects from DUEenv into local function frame, for convenience.
	if(missing(objnames)) {
		objnames = setdiff(ls(envir=DUEenv),
			c("DUEconfig", grep("external",sapply(DUEenv, mode),val=T)))
	}
	for(oname in objnames) assign(oname, get(oname, envir=DUEenv), envir=parent.frame())
	return(NULL)
}
