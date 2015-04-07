entrybox.popFraction.f <-
function(tryval) {
#	cat("entrybox.popFraction.f: Callback call: ", sys.call(), "\n")
	popFractionTemp = as.numeric(tryval) 
	if(is.na(popFractionTemp ) | (popFractionTemp < 0) 
                        | (popFractionTemp > 1 + DUEenv$proportions[DUEenv$whichFollows])) {
		tkmessageBox("-message",
			paste("entrybox.popFraction.f:  bad value: popFraction = ", popFractionTemp, ".  Reverting." ))
		load.value.into.entrybox(DUEenv$entrybox.popFraction, 
			as.character(DUEenv$proportions[DUEenv$thisPop]))
		return(FALSE)
	}
	proportionDifference = DUEenv$proportions[DUEenv$thisPop] - popFractionTemp
	DUEenv$proportions[DUEenv$thisPop] <- popFractionTemp 
#	print(proportions[thisPop])
#	print(proportions)
#	print(proportions[1:nPops][-whichFollows])
#	print(sum(proportions[1:nPops][-whichFollows]))
	proportionWhichFollows = 1 - sum(DUEenv$proportions[1:DUEenv$nPops][-DUEenv$whichFollows])
	cat("proportionDifference = ", proportionDifference, "\n")
	cat("proportionWhichFollows = ", proportionWhichFollows, "\n")
	if(proportionWhichFollows >= 0 & proportionWhichFollows <= 1)
		DUEenv$proportions[DUEenv$whichFollows] <- proportionWhichFollows
	else if(any(DUEenv$proportions[1:DUEenv$nPops][-DUEenv$thisPop] > 0))
		DUEenv$proportions[1:DUEenv$nPops][-DUEenv$thisPop] <-
			DUEenv$proportions[1:DUEenv$nPops][-DUEenv$thisPop] * (1 - DUEenv$proportions[DUEenv$thisPop])/sum(DUEenv$proportions[1:DUEenv$nPops][-DUEenv$thisPop])
	else {  #### all others are zero
		if(DUEenv$nPops == 1)
			DUEenv$proportions[1] <- 1.0
		else if (DUEenv$thisPop == 1)
			DUEenv$proportions[2] <- 1 - DUEenv$proportions[1]
		else
			DUEenv$proportions[1] <- 1 - DUEenv$proportions[DUEenv$thisPop]
	}
	if (DUEenv$settingSliderValues == FALSE) load.and.replot.images()
	return(TRUE)	
	#### When changing proportions (fractions), one has to be dependent.
	####  Callback needs to check ranges and modify the dependent fraction to add to 1.
}

