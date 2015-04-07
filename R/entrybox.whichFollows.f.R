entrybox.whichFollows.f <-
function(tryval, oldvalexpression=expression(whichFollows)) {
	#cat("entrybox.whichFollows.f: Callback call: ", as.character(sys.call()), "\n")
	whichFollowsTemp = as.integer(tryval)
	if(is.na(whichFollowsTemp) 
			| whichFollowsTemp <= 0 
			| whichFollowsTemp > DUEenv$nPops 
			| whichFollowsTemp == DUEenv$thisPop) {
		tkmessageBox("-message",
			paste("bad value: whichFollows = ", 
				whichFollowsTemp , 
				".\n One of these is true:\n",
				"\tis.na(whichFollowsTemp)\n", 
				"\twhichFollowsTemp <= 0\n", 
				"\twhichFollowsTemp > DUEenv$nPops\n", 
				"\twhichFollowsTemp == DUEenv$thisPop\n",	 		 "Reverting whichFollows to its previous value."))
		load.value.into.entrybox(DUEenv$entrybox.whichFollows, 
			as.character(get("whichFollows")))
		return(FALSE)
	}
	DUEenv$whichFollows <- whichFollowsTemp
	return(TRUE)
}

