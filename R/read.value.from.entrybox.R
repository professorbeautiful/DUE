read.value.from.entrybox <-
function(box) {
	if(is.character(box))
		the.box=get(paste(
			"DUEenv$",
			"entrybox.", box, sep=""))
	else
		the.box = box
	return(as.character(tkget(the.box)))
}

