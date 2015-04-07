DUEget <-
function(objname) {
	if(missing(objname)) {
		return(DUEget(ls(envir=DUEenv)))
	}
   	subx <- substitute(objname)
	if (is.name(subx)) 
		subx <- deparse(subx)
	if (!is.character(subx) || length(subx) != 1)
		stop("objname should be character")
	tryResult = try(get(subx, envir=DUEenv))
	if(	browseIf(class(tryResult) == "try-error", "browseIf: DUEget error")) browser()
	return(tryResult)
}
