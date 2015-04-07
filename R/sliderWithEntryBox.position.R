sliderWithEntryBox.position <-
function(objname, field, value) {
	if(is.character(objname))
		obj = get(objname, envir=DUEenv)
	else {
		obj = objname
		objname = as.character(substitute(obj))
	}
	sliderWithEntryBox.offset = c(delta.x=0, delta.y=35)
	if(!missing(field)) {
		tkplace(obj$the.slider, paste("-", field, sep=""), value) 
	}
	x = get.tkplaceinfo.x(obj$the.slider)
	x.entry = add(x, sliderWithEntryBox.offset["delta.x"])
	tkplace(obj$the.entry, "-x", x.entry)
	y = get.tkplaceinfo.y(obj$the.slider)
	y.entry = add(y, sliderWithEntryBox.offset["delta.y"])
	#print(sliderWithEntryBox.offset["delta.y"])
	#print(y.entry)
	tkplace(obj$the.entry, "-y", y.entry)
	entryHeights = 20
	tkplace(obj$the.entry, "-height", entryHeights)
	sliderWidth = as.character(tkcget(obj$the.slider, "-length")  )  #horizontal
	####   This is returning 100 for all of them, regardless of actual "length" (width)
	tkplace(obj$the.entry, "-width", add(sliderWidth, 6))
}

