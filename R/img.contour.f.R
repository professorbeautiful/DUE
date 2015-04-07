img.contour.f <-
function (xClick, yClick) 
{
	favoriteDose = DUEenv$favoriteDose
	favoriteLogDose = DUEenv$favoriteLogDose

	DUEenv$width = as.numeric(tclvalue(tkwinfo("reqwidth", DUEenv$img.contour)))
	DUEenv$height = as.numeric(tclvalue(tkwinfo("reqheight", DUEenv$img.contour)))
	if(browseIf()) cat("width = ", DUEenv$width, "\n")
	if(browseIf()) cat("height = ", DUEenv$height, "\n")
	parPlotSize = DUEenv$parPlotSize.contour
	usrCoords <- DUEenv$usrCoords.contour
	xMin <- parPlotSize[1] * DUEenv$width
	xMax <- parPlotSize[2] * DUEenv$width
	yMin <- parPlotSize[3] * DUEenv$height
	yMax <- parPlotSize[4] * DUEenv$height
	pixelRangeX <- xMax - xMin
	pixelRangeY <- yMax - yMin
	coordRangeX <- usrCoords[2] - usrCoords[1]
	coordRangeY <- usrCoords[4] - usrCoords[3]
	xClick <- as.numeric(xClick) + 0.5
	yClick <- as.numeric(yClick) + 0.5
	yClick <- DUEenv$height - yClick
	xyPixels = as.numeric(c(xClick, yClick))
	if(browseIf()) cat("xyPixels are ", xClick, yClick, "\n")
	rescale <- function(oldCoord, oldMin, oldRange, newMin, newRange, logMe = FALSE) {
			if (logMe) 
				oldCoord = log10 * oldCoord
			return((oldCoord - oldMin)/oldRange * newRange + newMin)
	}
	xPlotCoord <- rescale(xClick, xMin, pixelRangeX, usrCoords[1], coordRangeX)
	yPlotCoord <- rescale(yClick, yMin, pixelRangeY, usrCoords[3], coordRangeY)
	if(browseIf()) cat("xPlotCoord = ", xPlotCoord, "	 yPlotCoord = ", yPlotCoord, "\n")
	coords2pixels = function(xyCoords) {
		xCoord = xyCoords[1]
		yCoord = xyCoords[2]
		xPixels = rescale(xCoord, usrCoords[1], coordRangeX, 
				 xMin, pixelRangeX)
		yPixels = rescale(yCoord, usrCoords[3], coordRangeY, 
				 yMin, pixelRangeY)
		return(c(xPixels, yPixels))
	}
	pixels2coords = function(xyPixels) {
		xPixels = xyPixels[1]
		yPixels = xyPixels[2]
		xCoords = rescale(xPixels, xMin, pixelRangeX, usrCoords[1], coordRangeX)
		yCoords = rescale(yPixels, yMin, pixelRangeY, usrCoords[3], coordRangeY)
		return(c(xCoords, yCoords))
	}
	distances = sapply(1:DUEenv$nPops, function(i) {
			meansPixels = coords2pixels(DUEenv$the.means.pop[[i]]/log(10))
			if(browseIf()) cat("===========mean for #", i, ": ", DUEenv$the.means.pop[[i]], "\n")
			if(browseIf()) cat("pixels for mean for #", i, ": ", meansPixels, "\n")
			sqrt(sum((meansPixels - xyPixels)^2))
		  })
	if(browseIf()) cat("distances to means: ", distances, "\n")
	closest <- which(min(distances) == distances)[1]
		  if(browseIf()) cat("closest mean is #", closest, "\n")
	doseCoords <- pixels2coords(xyPixels)
	whereIclicked <- doseCoords
	#print((doseCoords))
	#print(10^(doseCoords))
	closestLogDose <- 10^mean(log10(doseCoords))
	closestDose <- 10^closestLogDose
	#print(closestLogDose)
	distanceToDiagonal = 
		sqrt(sum((xyPixels - coords2pixels(c(closestLogDose, closestLogDose)))^2))
	if(browseIf()) cat("distanceToDiagonal = ", distanceToDiagonal, "\n")
	if (distanceToDiagonal < min(distances)) {
		DUEenv$favoriteLogDose <- closestLogDose
		DUEenv$favoriteDose <- closestDose
		if(browseIf()) cat("setting favoriteDose to ", DUEenv$favoriteDose, "\n")
		load.and.replot.images()
	} else {
		load.value.into.entrybox(DUEenv$entrybox.thisPop, closest)
		if(browseIf()) cat("choosing population #", closest, "\n")
		entrybox.thisPop.f(closest)
	}
}

