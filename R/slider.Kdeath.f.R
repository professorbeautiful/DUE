slider.Kdeath.f <-
function(...){

	if(DUEenv$settingSliderValues)
		return()

	DUEenv$Kdeath <- as.numeric(tclvalue("Kdeath"))
	load.and.replot.images()
}

