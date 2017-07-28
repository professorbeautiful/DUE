doseParameters <- 
function (minDose, maxDose, nDoses, nDoseTicks, favoriteDose, 
	resetToDefaults = FALSE) 
{
  if (resetToDefaults) {
    minDose = 10
		maxDose = 1000
		nDoses = 50
		nDoseTicks =  7
		favoriteDose = 100 
		favoriteLogDose = 2
	}
	if (!missing(minDose)) 
		DUEput("minDose", minDose)
	if (!missing(maxDose)) 
		DUEput("maxDose", maxDose)
	if (!missing(nDoses)) 
		DUEput("nDoses", nDoses)
	if (!missing(nDoseTicks)) 
		DUEput("nDoseTicks", nDoseTicks)
	if (!missing(favoriteDose)) 
		DUEput("favoriteDose", favoriteDose)
	DUEput("favoriteLogDose", log10(favoriteDose))
	doseValues = 10^seq(log10((minDose)), log10((maxDose)), length= nDoses)
	DUEput("doseValues", doseValues)
	doseTicks = round(10^seq(log10((minDose)), log10((maxDose)), length= nDoseTicks), digits=1)
	DUEput("doseTicks", doseTicks)
	cat("Current dose parameters are:\n")
	print(doseValues)
	print(doseTicks)
	returnvalue = c(minDose, maxDose, nDoses, nDoseTicks, favoriteDose)
	names(returnvalue) = c("minDose", "maxDose", "nDoses", "nDoseTicks", "favoriteDose")
	# load.and.replot.images()
	return(returnvalue)
}

