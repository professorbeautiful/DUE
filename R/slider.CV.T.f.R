slider.CV.T.f <-
function(...){
	#browser()
#	cat("slider.CV.T.f: ")
#	cat("current value of varT is ",	
#		the.variances.pop [[thisPop]] [2,2],
#		",  new value string is ",  
#		tclvalue("CV.T"), 	
#		", & numeric  is", as.numeric(tclvalue("CV.T")),
#		"\n")
#	cat("BEFORE:\n")
#	print(the.variances.pop)


#  This is for the CVs.

	if(DUEenv$settingSliderValues)
		return()

	DUEenv$the.CVs.pop [[DUEenv$thisPop]] [2] <- as.numeric(tclvalue("CV.T"))
	recalculate.means.and.variances ()
	if (DUEenv$settingSliderValues == FALSE)
		load.and.replot.images()
}

