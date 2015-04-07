doseParameters <- 
function (minDose, maxDose, nDoses, nDoseTicks, favoriteDose, 
	resetToDefaults = FALSE) 
{
	if (resetToDefaults) {
		assign("minDose", 10, envir=DUEenv)
		assign("maxDose", 1000, envir=DUEenv)
		assign("nDoses", 50, envir=DUEenv)
		assign("nDoseTicks", 7, envir=DUEenv)
		assign("favoriteDose", 100, envir=DUEenv)
		assign("favoriteLogDose", 2, envir=DUEenv)
	}
	if (!missing(minDose)) 
		assign("minDose", minDose, envir=DUEenv)
	if (!missing(maxDose)) 
		assign("maxDose", maxDose, envir=DUEenv)
	if (!missing(nDoses)) 
		assign("nDoses", nDoses, envir=DUEenv)
	if (!missing(nDoseTicks)) 
		assign("nDoseTicks", nDoseTicks, envir=DUEenv)
	if (!missing(favoriteDose)) {
		assign("favoriteDose", favoriteDose, envir=DUEenv)
	}
	assign("favoriteLogDose", log10(DUEenv$favoriteDose), envir=DUEenv)
	with(DUEenv, {
		doseValues = 10^seq(log10(minDose), log10(maxDose), length= nDoses)
		assign("doseValues", doseValues, envir=DUEenv)
		doseTicks = round(10^seq(log10(minDose), log10(maxDose), length= nDoseTicks), digits=1)
		assign("doseTicks", doseTicks, envir=DUEenv)
		cat("Current dose parameters are:\n")
		print(doseValues)
		print(doseTicks)
	})
	returnvalue = with(DUEenv, c(minDose, maxDose, nDoses, nDoseTicks, favoriteDose))
	names(returnvalue) = c("minDose", "maxDose", "nDoses", "nDoseTicks", "favoriteDose")
	load.and.replot.images()
	return(returnvalue)
}

