entrybox.nPops.f <-
function(tryval) {
	#cat("entrybox.nPops.f: Callback call: ", sys.call(), "\n")
	#DUEcopy("nPops")
	#DUEcopy("proportions")
	#DUEcopy("thisPop")
	if(DUEenv$nPops != length(DUEenv$proportions)) {
		warning(paste(
			"entrybox.nPops.f: out of sync:  \nnPops (", 
			DUEenv$nPops, ") != length(DUEenv$proportions) = length(",
			DUEenv$proportions, ").  Setting proportions to ",
			paste(DUEenv$proportions<-c(1, rep(0,DUEenv$nPops-1)),
			collapse=""),
		collapse=""))
	}
	nPopsTemp = as.integer(tryval)
	if(is.na(nPopsTemp) | nPopsTemp <= 0) {
		tkmessageBox("-message", 
			paste("entrybox.nPops.f: bad value: thisPop = ", DUEenv$thisPop, "  Reverting."))
		load.value.into.entrybox(DUEenv$entrybox.nPops, as.character(DUEenv$nPops))
		## Reloading nPops.
		return(FALSE)
	}
	if(nPopsTemp < DUEenv$nPops) {
		DUEenv$proportions = DUEenv$proportions[1:nPopsTemp]/sum(DUEenv$proportions[1:nPopsTemp])
		if(DUEenv$thisPop > nPopsTemp) 
			DUEenv$thisPop <- nPopsTemp
		tkmessageBox("-message", paste(
			"entrybox.nPops.f: Rescaling proportions"
		))
	}
	if(nPopsTemp > DUEenv$nPops) {
		newPopIndices <- (DUEenv$nPops+1):nPopsTemp
		DUEenv$proportions[newPopIndices] <- 0 
		for(i in newPopIndices) {
			DUEenv$the.means.pop[[i]] <- DUEenv$the.means.pop[[DUEenv$nPops]]
			DUEenv$the.variances.pop[[i]] <- DUEenv$the.variances.pop[[DUEenv$nPops]]
			DUEenv$the.correlations.pop[[i]] <- DUEenv$the.correlations.pop[[DUEenv$nPops]]
		}
		tkmessageBox("-message", paste(
			"entrybox.nPops.f: Copying population parameters from population",
			DUEenv$nPops,
			"to",
			paste(newPopIndices, collapse=",")
		))
	}
	DUEenv$nPops <- nPopsTemp
	load.pop.values ()
	load.and.replot.images()
	return(TRUE)
}

