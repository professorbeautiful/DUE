load.value.into.entrybox <-
function(box, newcontents) {
	if(is.character(box)) {
		boxname = paste("entrybox", box, sep=".")
		the.box=get(boxname, envir=DUEenv)
	} else {
		the.box = box
	}
	if(missing(newcontents))
		newcontents=as.character(tkget(the.box))
	tkdelete(the.box, "0", "end")
	if(	browseIf(message="browseIf: load.value.into.entrybox")) browser()
	tkinsert(the.box, "0",  newcontents)
	#cat("inserting ", newcontents, " with length ", nchar(newcontents), "\n")
}

