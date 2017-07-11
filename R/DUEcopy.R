DUEcopy <- function(objnames) {
  DUEenv = get("DUEenv", envir = parent.frame())
  # Purpose: to copy objects from DUEenv into local function frame, for convenience.
	if(missing(objnames)) {
		objnames = setdiff(ls(envir=DUEenv),
			c("DUEconfig", grep("external",sapply(DUEenv, mode), value=T)))
	}
	for(oname in objnames) assign(oname, DUEenv[[oname]], envir=parent.frame())
	return(NULL)
}
