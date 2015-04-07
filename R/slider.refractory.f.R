slider.refractory.f <-
function(...){

	if(DUEenv$settingSliderValues)
		return()

	DUEenv$refractory <- as.numeric(tclvalue("refractory"))
	load.and.replot.images()
}

