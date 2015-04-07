read.value.from.slider <-
function(varname){
	slidername = paste("slider", varname, sep=".")
	the.new.value = as.numeric(tclvalue(varname))
	the.old.value = get(varname)
	is.changed =  (the.new.value != the.old.value)
	#cat("Reading ", the.new.value, " from ", slidername, "\n")
	cat("read.value.from.slider ", varname, "\n")
	if(is.changed)
		assign(varname, the.new.value, pos=1, immediate=TRUE)
	return(is.changed)
}

