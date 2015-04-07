entrybox.thisPop.f <-
function(tryval, oldvalexpression=expression(as.character(thisPop))) {
	if(browseIf())
		cat("entrybox.thisPop.f: Callback call: ", as.character(sys.call()), "\n")
	DUEenv$settingSliderValues <- TRUE
	thisPopTemp <- as.integer(tryval)
	if(is.na(thisPopTemp) 
		| thisPopTemp <= 0 
		| thisPopTemp > DUEenv$nPops) {
		tkmessageBox("-message",
			paste("bad value: thisPop = ", thisPopTemp, ".  Reverting."))
		load.value.into.entrybox(DUEenv$entrybox.thisPop, oldvalexpression)
		return(FALSE)
	}
	DUEenv$thisPop <- thisPopTemp
	load.pop.values()  ### load thisPop values into slider/entryboxes.
	if(DUEenv$whichFollows == DUEenv$thisPop) {
		if(DUEenv$nPops > 1 & DUEenv$thisPop == 1) {
			DUEenv$whichFollows <- 2
		}
		else DUEenv$whichFollows <- 1
	}
	load.value.into.entrybox(DUEenv$entrybox.whichFollows, 
		as.character(DUEenv$whichFollows))
	DUEenv$settingSliderValues <- FALSE
	return(TRUE)
}

