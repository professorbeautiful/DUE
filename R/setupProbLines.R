setupProbLines <-
function() {
 with(DUEenv, {
	probLineNames <- 
		c("R", "T", "rt","rT","Rt","RT","EU")
	probLabels  <- list()
	probLineWidthChoices <- c(0, 1, 5)
	probLineWidths <- rep(probLineWidthChoices[1], 7)
	names(probLineWidths) <- probLineNames

	# TODO: These initial line width choices should be stored in DUEenv.
	probLineWidths["Rt"] <- probLineWidthChoices[2]
	probLineWidths["EU"] <- probLineWidthChoices[3]
	#print(probLineWidths)
	
	delX_ProbLabels = 67
	#### TODO:  fix: this is Kludgy.
	getImgHeight <- function(img) 430 # / 0.9 * DUEconfig["img.prob", "vscale"]
	
	for(nam in probLineNames) {
		whichLine = which(nam==probLineNames)
		### Create the buttons.
		theCallback = eval(parse(text=paste(sep="",
			"function() probLineCallback(\"", nam, "\")")))
		#print(theCallback)
		probLabels[[nam]] <- tkbutton(tkWindow,
			text=nam,
			fg=rt.outcome.colors[whichLine],
			font=fontForLabels,
			command= theCallback) 
		#assign(paste("probLabel", nam, sep="."), pos=1,
		#	probLabels[[nam]])
		probLabelBigHeight <- 35
		probLabelLittleHeight <- 28
		probLabelX = add(DUEconfig["img.prob", "x"]
			, delX_ProbLabels * (whichLine - 1))
		probLabelY = add(DUEconfig["img.prob", "y"]  			, getImgHeight(img.prob))
		probLabelY = add(probLabelY, 3)
 		lineShows = 	probLineWidths[whichLine] > probLineWidthChoices[1]
		tkplace(probLabels[[nam]],
			"-x", probLabelX,
			"-y", probLabelY, 
			"-width", 
				ifelse(lineShows, probLabelBigHeight, probLabelLittleHeight),
			"-height", 
				ifelse(lineShows, probLabelBigHeight, probLabelLittleHeight)
		)
		tkconfigure(probLabels[[nam]],
			font=get(ifelse(lineShows, "fontButtonX", "fontButtonNotX")))
			###ifelse(lineShows, fontButtonX, fontButtonNotX))
			###  gives error "object of type 'externalptr' is not subsettable"
	}
  })
}

