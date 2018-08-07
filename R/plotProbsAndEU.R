plotProbsAndEU <-function(DUEenv=DUEenv, context='shiny') {
  cexValue = ifelse(context=='shiny', 2, 1); 
  eightprobs = calculate.probabilities.allDoses(DUEenv)
  utilitySummaries = extractUtilitySummaries(eightprobs, 
                                             log10doseValues=DUEenv$log10doseValues, MTDtoxicity=DUEenv$MTDtoxicity)
  for(us in names(utilitySummaries))
    assign(us, utilitySummaries[[us]])
  # cat("Redrawing ProbsAndEU: utility = ", unlist(DUEenv$utility), "\n")
  convertEU <- function(x, isEU=TRUE) {
    #### Map EU on right axis from [-1, 1] to [0,1] on left axis.
    if(isEU) 
      (x+1)/2
    else x
  }
  par(mar =  c(5,4,4,6) + 0.1, xpd=NA )
  plot(DUEenv$doseValues, eightprobs[1,],type="l",col=0,lwd=1, 
       xlim=c(min(DUEenv$doseTicks), max(DUEenv$doseTicks)), 
              ylim=c(0,1),
       axes=FALSE,  log="x", xlab="", ylab=""
  )
  
  DUEenv$parPlotSize.ProbsAndEU <- par("plt")
  DUEenv$usrCoords.ProbsAndEU <- par("usr")
  if(browseIf(message="Just finished plot-- not yet done title-- check eightprobs[1,]")) browser()
  # plot.title="Probabilities and Expected Utility, E(U)"
  # title(main=plot.title, cex.main=2, col.main="blue")
  axis(side = 1, at = DUEenv$doseTicks)
  mtext(side=1, line=2.5, "Dose", cex=cexValue)
  axis(2, at=c(0, 0.33, 0.6, 0.8, 1))
  mtext(side=2, line=2.5, "Probability", cex=cexValue)

  axis(side = 4, line=1, lwd=1, at=(axisvalues<-c(0, 0.25, 0.5, 0.75, 1)), 
       col=rt.outcome.colors("EU"), labels=round(axisvalues*2-1, 1)
       , xpd=NA, cex=2)
  # Note: the "at" values are relative to axis 1, the labels are correct for EU.
  #xrange = par("usr")[1:2],
  # pushout = 1.02
  # EUlabelPosition = exp(log(xrange[1]) + (log(xrange[2])-log(xrange[1])*1.02) )
  mtext(side = 4, line = 4, text = "  E(Utility)", #outer=TRUE,
        xpd = NA, srt=90, cex=cexValue, col=rt.outcome.colors('EU'),
        ylbias = -0.5)  ### default ylbias = 0.2. Has no effect.
  
  linetypes = c(rep(1,6), 1, 1)
  if(!is.null(DUEenv$probLineWidths)) {
    linewidths = DUEenv$probLineWidths
  } else
    linewidths = c(1, 3, 1, 1, 1, 1, 3, 1)
  EUindex = 7
  nDoses = length(DUEenv$doseValues)
  shortlist <- c(1, round(nDoses/2), nDoses)
  for(i in 1:8) {
    if(linewidths[i] > 0) {
      outcome.string = rt.outcome.strings(i)
      lines(DUEenv$doseValues, convertEU(eightprobs[i,], i==EUindex),
            lty=linetypes[i], lwd=linewidths[i], 
            col=rt.outcome.colors(outcome.string))
      text(DUEenv$doseValues[shortlist],
           convertEU(eightprobs[i, shortlist], i==EUindex), 
           col=rt.outcome.colors(outcome.string),
           label=outcome.string,
           cex=1.2, xpd=NA, adj=c(0, -1))
    }
  }
  segments(5, DUEenv$MTDtoxicity, best.dose.p.T, DUEenv$MTDtoxicity, lty=2, lwd=2, col=rt.outcome.colors("T"))
  segments(best.dose.p.T, DUEenv$MTDtoxicity, best.dose.p.T, -0.1, lty=2, lwd=2, col=rt.outcome.colors("T"))
  segments(DUEenv$best.dose.EU, 0, DUEenv$best.dose.EU, convertEU(highest.EU, TRUE), lty=2, lwd=2, 
           col=rt.outcome.colors("EU"))
  abline(v=DUEenv$selectedDose, col="green")
}
