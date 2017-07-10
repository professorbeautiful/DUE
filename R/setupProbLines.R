setupProbLines <- function(DUEenv) {
  probLineNames <<- 
    c("R", "T", "rt","rT","Rt","RT","EU")
  probLabels  <<- list()
  probLineWidthChoices <<- c(0, 1, 5)
  DUEenv$probLineWidths <<- rep(probLineWidthChoices[1], 7) 
  names(DUEenv$probLineWidths) <- probLineNames
  
  # TODO: These initial line width choices should be stored in DUEenv.
  #DUEenv$probLineWidths["Rt"] <- probLineWidthChoices[2]  #1
  #DUEenv$probLineWidths["EU"] <- probLineWidthChoices[3] #5
}

setupProbLinesTK = function() {
  setupProbLines(DUEenv)
	delX_ProbLabels = 67
	#### TODO:  fix: this is Kludgy.
	getImgHeight <- function(img) 430 # / 0.9 * DUEconfig["img.prob", "vscale"]
	
	for(nam in probLineNames) {
		whichLine = which(nam==probLineNames)
		### Create the buttons.
		theCallback = eval(parse(text=paste(sep="",
			"function() probLineCallbackTK(\"", nam, "\")")))
		#print(theCallback)
		DUEenv$probLabels[[nam]] <- tkbutton(DUEenv$tkWindow,
			text=nam,
			fg=DUEenv$rt.outcome.colors[whichLine],
			font=DUEenv$fontForLabels,
			command= theCallback) 
		#assign(paste("probLabel", nam, sep="."), pos=1,
		#	probLabels[[nam]])
		probLabelBigHeight <- 35
		probLabelLittleHeight <- 28
		probLabelX = add(DUEenv$DUEconfig["img.prob", "x"]
			, delX_ProbLabels * (whichLine - 1))
		probLabelY = add(DUEenv$DUEconfig["img.prob", "y"]  			, getImgHeight(img.prob))
		probLabelY = add(probLabelY, 3)
 		lineShows = 	DUEenv$probLineWidths[whichLine] > probLineWidthChoices[1]
		tkplace(DUEenv$probLabels[[nam]],
			"-x", probLabelX,
			"-y", probLabelY, 
			"-width", 
				ifelse(lineShows, probLabelBigHeight, probLabelLittleHeight),
			"-height", 
				ifelse(lineShows, probLabelBigHeight, probLabelLittleHeight)
		)
		tkconfigure(DUEenv$probLabels[[nam]],
			font=get(ifelse(lineShows, "fontButtonX", "fontButtonNotX"), envir=DUEenv))
			###ifelse(lineShows, fontButtonX, fontButtonNotX))
			###  gives error "object of type 'externalptr' is not subsettable"
	
  }
}

