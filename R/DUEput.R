DUEput <-
function(objname, value) {
	subx <- substitute(objname)
	if (is.name(subx)) 
		subx <- deparse(subx)
	if (!is.character(subx) || length(subx) != 1)
		stop("objname should be character") 
	if(! exists("DUEenv", where=1)) {
		assign("DUEenv", new.env(), pos=1)
		warning("creating new DUEenv")
	}
	assign(subx, value, envir=DUEenv)
}
