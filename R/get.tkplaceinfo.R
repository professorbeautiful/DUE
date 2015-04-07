get.tkplaceinfo <-
function(wid, field) {
	tkplaceobject.tkObj = as.character(tkplace.info(wid))
	#print(tkplaceobject.tkObj)
	tkplaceobject = tkplaceobject.tkObj [seq(2, length(tkplaceobject.tkObj), by=2)]
	names(tkplaceobject) = tkplaceobject.tkObj [
		seq(1, length(tkplaceobject.tkObj), by=2) ]
	if(missing(field))
		return(tkplaceobject)
	return(tkplaceobject[field])
}

