probLineCallback <-
function(nam) {
### FIXED by replacing the "with" construct by explicit references.
	whichWidth = which(
		DUEenv$probLineWidths[nam]==DUEenv$probLineWidthChoices)
	whichWidth = whichWidth + 1
	if(whichWidth > length(DUEenv$probLineWidthChoices))
		whichWidth = 1
	DUEenv$probLineWidths[nam] <- DUEenv$probLineWidthChoices[
		whichWidth]
	#cat("probLineCallback: nam=", nam, "  whichWidth=", whichWidth, "\n")
	if(whichWidth > 1) {
		tkconfigure(DUEenv$probLabels[[nam]] , 
			font=DUEenv$fontButtonX)
		tkplace(DUEenv$probLabels[[nam]],
			"-width", DUEenv$probLabelBigHeight,
			"-height", DUEenv$probLabelBigHeight
		)
	}
	else {
		tkconfigure(DUEenv$probLabels[[nam]],
			font=DUEenv$fontButtonNotX)
		tkplace(DUEenv$probLabels[[nam]],
			"-width", DUEenv$probLabelLittleHeight,
			"-height", DUEenv$probLabelLittleHeight
		)
	}
	tkrreplot(DUEenv$img.prob)
}

