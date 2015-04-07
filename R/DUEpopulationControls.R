DUEpopulationControls <-
function(){
	if(	browseIf(message="browseIf:  DUEpopulationControls")) browser()
	### CREATE ENTRY BOXES FOR POP PARAMETERS
	DUEenv$thY <- 420; DUEenv$thDelY <- 54; DUEenv$thXoffset <- 90
	
	DUEenv$settingSliderValues <- TRUE
	
	DUEenv$label.nPops <- tklabel(DUEenv$tkWindow, text="#Pops", font=DUEenv$fontForLabels, bg="lightblue")
	DUEenv$label.thisPop <- tklabel(DUEenv$tkWindow, text="this\npop:", font=DUEenv$fontForLabels, bg="lightblue")
	DUEenv$label.popFraction <- tklabel(DUEenv$tkWindow, text="fraction", font=DUEenv$fontForLabels, bg="lightblue")
	DUEenv$label.whichFollows <- tklabel(DUEenv$tkWindow, text="which prop\nis \ndependent", font=DUEenv$fontForLabels, bg="lightblue")
	DUEenv$entrybox.nPops <- tkentry(DUEenv$tkWindow)
	DUEenv$entrybox.thisPop <- tkentry(DUEenv$tkWindow)
	DUEenv$entrybox.popFraction <- tkentry(DUEenv$tkWindow)
	DUEenv$entrybox.whichFollows <- tkentry(DUEenv$tkWindow)
	
	tkplace(DUEenv$label.nPops, 	
		"-x", 20, "-y", DUEenv$thY + 1.3*DUEenv$thDelY, 
		"-height", 30, "-width", 50)
	tkplace(DUEenv$label.thisPop, 
		"-x", 180, "-y", get.tkplaceinfo.y(DUEenv$label.nPops), 
		"-height", 40, "-width", 50)
	tkplace(DUEenv$label.popFraction, "-x", 290, "-y", get.tkplaceinfo.y(DUEenv$label.nPops), 
		"-height", 40, "-width", 50)
	tkplace(DUEenv$label.whichFollows, 
		"-x", get.tkplaceinfo.x(DUEenv$label.nPops), 
		"-y", add("5", add(get.tkplaceinfo.y(DUEenv$label.nPops), get.tkplaceinfo.height(DUEenv$label.nPops))),
		"-height", 40, "-width", 
		DUEenv$DUEconfig["label.whichFollows", "width"]
	)
	#attach.entrybox.to.label(label.nPops, entrybox.nPops)
	#attach.entrybox.to.label(label.thisPop, entrybox.thisPop)
	#attach.entrybox.to.label(label.popFraction, entrybox.popFraction)
	#attach.entrybox.to.label(label.whichFollows, entrybox.whichFollows)
	#tkconfigure(label.popFraction,  "-bg", "lightblue")
	#tkconfigure(label.nPops, "-bg", "lightblue")
	#tkconfigure(label.thisPop,  "-bg", "lightblue")
	#tkconfigure(label.whichFollows,  "-bg", "lightblue")
	
	respondToEntryBox(DUEenv$entrybox.nPops, function(...)entrybox.nPops.f(...))
	
	respondToEntryBox(DUEenv$entrybox.thisPop, function(...)entrybox.thisPop.f(...))
	
	respondToEntryBox(DUEenv$entrybox.popFraction, function(...)entrybox.popFraction.f(...))
	
	respondToEntryBox(DUEenv$entrybox.whichFollows, function(...)entrybox.whichFollows.f(...))
	
	if(browseIf()) cat("settingSliderValues = TRUE\n")
	DUEenv$settingSliderValues <- TRUE
	DUEenv$slider.mu.R <- tkscale(DUEenv$tkWindow,
		command=function(...)slider.mu.R.f(...), from=log10(DUEenv$minDose), to=log10(DUEenv$maxDose), variable="mu.R", label="theta R avg",
		showvalue=FALSE, resolution=0.05, orient="horiz",width=7)
	DUEenv$slider.mu.T <-  	tkscale(DUEenv$tkWindow,
		command=function(...)slider.mu.T.f(...),
		from=log10(DUEenv$minDose), to=log10(DUEenv$maxDose), variable="mu.T", label="theta T avg",
		showvalue=FALSE, resolution=0.05, orient="horiz",width=7)
	DUEenv$slider.CV.R <-  	tkscale(DUEenv$tkWindow,
		command=function(...)slider.CV.R.f(...),
		from=0, to=2.5, variable="CV.R", label="CV.R",
		showvalue=FALSE, resolution=0.05, orient="horiz",width=7)
	DUEenv$slider.CV.T <-  	tkscale(DUEenv$tkWindow,
		command=function(...)slider.CV.T.f(...),
		from=0, to=2.5, variable="CV.T", label="CV.T",
		showvalue=FALSE, resolution=0.05, orient="horiz",width=7)
	DUEenv$slider.rho.RT <-  tkscale(DUEenv$tkWindow,
		command=function(...)slider.CV.rho.RT.f(...),
		from=-1, to=1, variable="rho.RT", label="rho.RT",
		showvalue=FALSE, resolution=0.05, orient="horiz",width=7)
	DUEenv$slider.refractory <-  tkscale(DUEenv$tkWindow,
		command=function(...)slider.refractory.f(...),
		from=0, to=1, variable="refractory", label="Prob(refractory tumor)",
		showvalue=FALSE, resolution=0.05, orient="horiz",width=7)
	DUEenv$slider.Kdeath <-  tkscale(DUEenv$tkWindow,
		command=function(...)slider.Kdeath.f(...),
		from=0, to=2, variable="Kdeath", label="K(response-limiting)",
		showvalue=FALSE, resolution=0.05, orient="horiz",width=7)
	
	############
	
	#tkbind(slider.mu.R, "<Button-1>", function() cat("mu.R button"))
	
	for(slider.var in strsplit("mu.R mu.T CV.R CV.T rho.RT refractory Kdeath", split=" ")[[1]]){
		setup.sliderentryboxes(slider.var)
	}
	## DOESNT SEEM TO DO ANYTHING:  tkconfigure(entry.mu.R, "-textvariable", "mu.R")#
	## returns variable name:  tkcget(entry.mu.R, "-text")
	##  Return entryBox content:  tkget(entry.mu.R)

	tkconfigure(DUEenv$slider.mu.R, 
		"-troughcolor", DUEenv$rt.outcome.colors["R"],
		"-fg", DUEenv$rt.outcome.colors["R"])
	tkconfigure(DUEenv$slider.mu.T,  
		"-troughcolor", DUEenv$rt.outcome.colors["T"],
		"-fg", DUEenv$rt.outcome.colors["T"]) 
	tkconfigure(DUEenv$slider.CV.R,  
		"-troughcolor", DUEenv$rt.outcome.colors["R"],
		"-fg", DUEenv$rt.outcome.colors["R"])
	tkconfigure(DUEenv$slider.CV.T,  
		"-troughcolor", DUEenv$rt.outcome.colors["T"],
		"-fg", DUEenv$rt.outcome.colors["T"]) 
	tkconfigure(DUEenv$slider.rho.RT,  
		"-troughcolor", "black") 

	########################

	load.pop.values()

	DUEenv$label.aux.params <- tklabel(DUEenv$tkWindow, text="Auxiliary parameters", font=DUEenv$fontForLabels,
	 bg="darkblue", fg="white")
	tkplace(DUEenv$label.aux.params , "-x", 20, "-y", DUEenv$thY + DUEenv$thDelY*4+55 + 2,
		"-height", 40, "-width", 80)

	tkplace(DUEenv$slider.refractory, "-x", DUEenv$thXoffset + 20, "-y", DUEenv$thY + DUEenv$thDelY*4+55)
	tkplace(DUEenv$slider.Kdeath, "-x",   DUEenv$thXoffset + 135, "-y", DUEenv$thY + DUEenv$thDelY*4+55)

	DUEenv$settingSliderValues <- FALSE
}

