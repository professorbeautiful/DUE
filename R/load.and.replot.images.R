load.and.replot.images <-
function(...) {
	#cat("Entering load.and.replot.images\n")
	try (expression(
		{cat("dotdotdot length is ", length(...), "\n");	print(...)}
	))

	if(DUEenv$settingSliderValues) {	### we don't want callbacks for the moment.
		cat("Skipping load.and.replot.images\n")
		return()  
	}
	
	tkrreplot(DUEenv$img.contour)

	DUEenv$utility <- data.frame(DUEenv$U.rt,DUEenv$U.rT,DUEenv$U.Rt,DUEenv$U.RT)
	if(exists(envir=DUEenv, "img.prob"))
		tkrreplot(DUEenv$img.prob)
	#cat("Exiting load.and.replot.images\n")
}

