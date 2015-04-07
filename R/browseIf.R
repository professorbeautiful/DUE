browseIf <-
function (value, message) 
{
###  cannot RUN browser in here-  can't get the right frame.
### TODO  fix this.
	shallWeBrowse = FALSE
	### browse if value is TRUE
	if(!missing(value)) {
	    if (is.logical(value)) 
		shallWeBrowse = value
	    else stop("browseIf takes a boolean")
	#### browse if the calling function is in the "browseUs" list,
	####  regardless of value.
	} else if (exists("browseUs")) {
	    shallWeBrowse = 
		(as.character(sys.call(-1)[1]) %in% browseUs) 
	   }
	if(shallWeBrowse) {
		if(!missing(message)) cat(message, "\n")
	}
	return(shallWeBrowse)
}
