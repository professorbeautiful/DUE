slider.mu.BOTH.f <-
function(index, ...){

#Whenever the value of the variable changes, the scale will update to reflect this value. Whenever the scale is manipulated interactively, the variable will be modified to reflect the scale's new value.
	
  with(DUEenv, {
	if(DUEenv$settingSliderValues) return()
	index = get("index", envir=parent.frame(3))
	if(	browseIf(message="browseIf: in slider.mu.BOTH.f")) browser()
	indexVector = c("R", "T")
	muString = paste("mu.", indexVector[index], sep="")
	muValue = get(muString)

	logInfo(logfilename="", paste(
		"(in ) (", thisPop, ")", muString,
		muValue, 
		"old the.mean", DUEenv$the.means.pop[[thisPop]] [index], 
		"old the.E", DUEenv$the.Ethresholds.pop[[thisPop]] [index], 
		"new tclvalue", tclvalue(muString)))
	
	###  The slider holds the.means.pop value; it  is changed.
	DUEenv$the.means.pop [[DUEenv$thisPop]] [index] <- as.numeric(tclvalue(muString))
	
	###  Now convert to Ethreshold value.
	DUEenv$the.Ethresholds.pop [[DUEenv$thisPop]] [index] <- 10^DUEenv$the.means.pop [[DUEenv$thisPop]] [index] 

	load.and.replot.images()
		
	logInfo(logfilename="", paste(
		"(out) (", thisPop, ")", muString,
		muValue, 
		"new the.mean", DUEenv$the.means.pop[[DUEenv$thisPop]] [index], 
		"new the.E", the.Ethresholds.pop[[DUEenv$thisPop]] [index], 
		"    tclvalue", tclvalue(muString)))
 })
}

