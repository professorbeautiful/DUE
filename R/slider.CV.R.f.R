slider.CV.R.f <-
function(...){
	#browser()
#	cat("slider.CV.R.f: ")
#	cat("current value of varR is ",	
#		the.variances.pop [[thisPop]] [1,1],
#		",  new value string is ",  
#		tclvalue("CV.R"), 	
#		", & numeric  is", as.numeric(tclvalue("CV.R")),
#		"\n")
#	cat("BEFORE:\n")
#	print(the.variances.pop)

#  This is for the CVs.
	if(DUEenv$settingSliderValues)
		return()

	DUEenv$the.CVs.pop [[DUEenv$thisPop]] [1] <- as.numeric(tclvalue("CV.R"))
#	cat("AFTER:\n")
#	print(the.variances.pop)
	recalculate.means.and.variances ()
	if (DUEenv$settingSliderValues == FALSE)
		load.and.replot.images()
}

