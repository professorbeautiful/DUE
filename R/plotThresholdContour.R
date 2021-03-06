plotThresholdContour = function (theDUEenv=DUEenv, context='shiny') 
{
  #scoping error; had to assign DUEenv to theDUEenv -promise already under evaluation: recursive default argument reference or earlier problems?
  DUEenv=theDUEenv  
  cexValue = ifelse(context=='shiny', 3, 1); 
  # OKfont = c("sans serif", "bold")
    recalculate.means.and.variances(DUEenv)
    the.grid = as.matrix(expand.grid(log10(DUEenv$doseValues),
        log10(DUEenv$doseValues)))
    the.dmvnorms = apply(as.array(1:DUEenv$nPops), 1, function(i) {
        # cat("plotThresholdContour: the.logmedians.pop[[i]] = ", DUEenv$the.logmedians.pop[[i]], "\n")
        return(DUEenv$proportions[i] * dmvnorm(the.grid, mean = DUEenv$the.logmedians.pop[[i]]/log(10),
            sigma = DUEenv$the.variances.pop[[i]]))
    })
    the.dmvnorms = array(the.dmvnorms, dim = c(nrow(the.grid),
        DUEenv$nPops))
    contour.values = matrix(apply(the.dmvnorms, 1, sum), nrow = DUEenv$nDoses)
    contour(DUEenv$doseValues, DUEenv$doseValues, contour.values,
        xlim = range(DUEenv$doseValues), ylim = range(DUEenv$doseValues),
        log = "xy", axes = F, xlab="", ylab="")
    mtext(side=2, line=2, "Threshold of Toxicity", cex=cexValue) #font=4?
    mtext(side=1, line=2, "Threshold of Response", cex=cexValue)
    DUEenv$parPlotSize.contour <- par("plt")
    DUEenv$usrCoords.contour <- par("usr")
### vfont works for text but not for axis or title. (ERROR)
### font.main and family work for title, but not for axis.(not an error, just no effect).  HersheySans etc but not Sans or Serif. Or Arial.
### for axis, cex.axis  and font.axis affect the tick valuess
    ### for axis, cex.lab  and font.lab do NOT affect the labels 
    ###  This system stinks & is so poorly documented!
    axis(1, at = DUEenv$doseTicks,
          cex.lab=3)
    #font.axis=2, font.lab=2, family="Arial")
    axis(2, at = DUEenv$doseTicks,
          cex.lab=3)
    #font.axis=4, font.lab=2, family="HersheySans")
    #title(main = plot.title, cex.main = 1.5, col.main = "blue")
    #font.main=4, family="HersheySerif")
    ###  Works for title() not for axis().
    abline(a = 0, b = 1, lty = 2, col = "black", lwd = 3)
    drawQuadrants(cexQ = ifelse(context=='shiny', 4, 2))
    for (iPop in 1:DUEenv$nPops)
        text(DUEenv$the.medianThresholds.pop[[iPop]][1],
                DUEenv$the.medianThresholds.pop[[iPop]][2],
                as.character(iPop), font=4,
                cex = 5, col = "black")
    if (browseIf(message = "In callback plotThresholdContour"))
        browser()
}
