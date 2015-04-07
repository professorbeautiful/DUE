DUEutilityControls = 
function() 
{	#### Utility control configuration.R 
	rt.outcome.colors =  DUEenv$rt.outcome.colors
	for(varname in c("U.rt","U.rT","U.Rt","U.RT"))  
		try({tclvalue(varname) <- get(envir=DUEenv, varname)})

	DUEenv$slider.U.rt <- 	tkscale(DUEenv$tkWindow, command=function(...)replot.img.prob(), from=-2.00, to=2.00, variable="U.rt",
					showvalue=TRUE, resolution=0.125, orient="horiz",width=7)
	DUEenv$slider.U.rT <-  	tkscale(DUEenv$tkWindow, command=function(...)replot.img.prob(), from=-2.00, to=2.00, variable="U.rT",
					showvalue=TRUE, resolution=0.125, orient="horiz",width=7)
	DUEenv$slider.U.Rt <-  	tkscale(DUEenv$tkWindow, command=function(...)replot.img.prob(), from=-2.00, to=2.00, variable="U.Rt",
					showvalue=TRUE, resolution=0.125, orient="horiz",width=7)
	DUEenv$slider.U.RT <-  	tkscale(DUEenv$tkWindow, command=function(...)replot.img.prob(), from=-2.00, to=2.00, variable="U.RT",
					showvalue=TRUE, resolution=0.125, orient="horiz",width=7)
	for(outcome in words("rt rT Rt RT")) {
		tkconfigure( get(paste("slider.U.", outcome, sep=""), envir=DUEenv), 
			"-fg", DUEenv$rt.outcome.colors[outcome],
#			"-borderwidth", "1",
			### sadly, -bg does not seem to work.
			"-font", DUEenv$fontForLabels,
			"-troughcolor", DUEenv$rt.outcome.colors[outcome],
			"-activebackground", DUEenv$rt.outcome.colors[outcome],
			"-highlightbackground", DUEenv$rt.outcome.colors[outcome],
			"-highlightcolor", DUEenv$rt.outcome.colors[outcome],
			label=paste("Utility of ", outcome, sep=""), bg="white")
	}
	uX <- 570 + 180
	uY <- DUEenv$uY <- 555
	uGapx <- 145
	uGapy <-  45
	tkplace(DUEenv$slider.U.Rt, "-x", uX, "-y", uY)
	tkplace(DUEenv$slider.U.rt, "-x", uX + uGapx, "-y", uY)
	tkplace(DUEenv$slider.U.RT, "-x", uX, "-y", uY + uGapy)
	tkplace(DUEenv$slider.U.rT, "-x", uX + uGapx, "-y", uY + uGapy)

	##### Create Utility Choice Buttons ####
	DUEenv$utilityChoices <- list(
		"Additive"   = data.frame(U.rt=0, U.rT=-1,  U.Rt=1, U.RT= 0),
		"Simple"     = data.frame(U.rt=0, U.rT= 0,  U.Rt=1, U.RT= 0),
		"Cautious"   = data.frame(U.rt=0, U.rT=-1,  U.Rt=1, U.RT=-1),
		"Aggressive" = data.frame(U.rt=0, U.rT= -1,  U.Rt=1, U.RT= 1)
	)
	DUEenv$utilityChoiceNames <- names(DUEenv$utilityChoices)
		
	DUEenv$button.Cautious <-  tkbutton(DUEenv$tkWindow,text="Cautious", 
		command=function()set.utilities.cautious()    ) 
	DUEenv$button.Additive <-  tkbutton(DUEenv$tkWindow,text="Additive", 
		command=function()set.utilities.additive()   ) 
	DUEenv$button.Simple <-  tkbutton(DUEenv$tkWindow,text="Simple", 
		command=function()set.utilities.simple()    ) 
	DUEenv$button.Aggressive <-  tkbutton(DUEenv$tkWindow,text="Aggressive ", 
		command=function()set.utilities.aggressive()    ) 
	tkconfigure(DUEenv$button.Cautious, "-width", 10)
	tkconfigure(DUEenv$button.Aggressive, "-width", 10)
	tkconfigure(DUEenv$button.Additive, "-width", 10)
	tkconfigure(DUEenv$button.Simple, "-width", 10)

	##### Position the Utility Choice Buttons ####
	#### The following are NOT in DUEconfig
	uYdel <- DUEenv$uYdel <- 35
	uButX <- 572
	tkplace(DUEenv$button.Additive, "-x", uButX, "-y", uY)
	tkplace(DUEenv$button.Simple, "-x", uButX, "-y", uY  + 1*uYdel)
	tkplace(DUEenv$button.Cautious, "-x", uButX, "-y", uY + 2*uYdel)
	tkplace(DUEenv$button.Aggressive, "-x", uButX, "-y", uY + 3*uYdel)
		
	set.utilities.additive()
}
