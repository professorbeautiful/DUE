slider.rho.RT.f <-
function(...){
	if(DUEenv$settingSliderValues)
		return()
	####   Correlation.
	rho = try(as.numeric(tclvalue("rho.RT")))
	#if(class(rho) == "try-error")  ## TODO
	if(rho <= -1.0) rho = -0.95
	if(rho >=  1.0) rho =  0.95
	DUEenv$the.correlations.pop [[DUEenv$thisPop]] <- rho
	recalculate.offdiagonals (rho, DUEenv$thisPop)
	load.and.replot.images()
}

