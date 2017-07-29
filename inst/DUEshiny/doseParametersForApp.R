
    minDose = 10
		maxDose = 1000
		nDoses = 50
		nDoseTicks =  7
		favoriteDose = 100 
		favoriteLogDose = 2
		DUEput("minDose", minDose)
		DUEput("maxDose", maxDose)
		DUEput("nDoses", nDoses)
		DUEput("nDoseTicks", nDoseTicks)
		DUEput("favoriteDose", favoriteDose)
	DUEput("favoriteLogDose", log10(favoriteDose))
	doseValues = 10^seq(log10((minDose)), log10((maxDose)), length= nDoses)
	DUEput("doseValues", doseValues)
	doseTicks = round(10^seq(log10((minDose)), log10((maxDose)), length= nDoseTicks), digits=1)
	DUEput("doseTicks", doseTicks)

