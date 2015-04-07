load.value.into.slider <-
function(varname, value){

	if(  	browseIf(message="load.value.into.slider(1)")) browser()

	slidername = paste("slider", varname, sep=".")
	#cat("Loading ", value, " into ", slidername, "\n")
	#fun =	tkconfigure(get(slidername), "-command")
	#tkconfigure(get(slidername), command=function(){} )

	if(  	browseIf(message="load.value.into.slider(2)")) browser()
	is.logvar = DUEenv$DUEconfig[
	  paste("sliderWithEntryBox", varname, sep=".") , 'log']
	if(identical(is.logvar,"yes"))  value = log10(value)
 	tkset(get(slidername, envir=DUEenv), value)
	#tkconfigure(get(slidername), command=fun )
}

