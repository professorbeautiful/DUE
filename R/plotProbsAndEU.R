plotProbsAndEU <-
  function() {
    if(DUEenv$settingSliderValues == TRUE)
      return
    read.Uvalues()  ### copies from the sliders to the vector "utility"
    DUEenv$sevenprobs <- DUEenv$sevenprobs <- 
      sapply(log10(DUEenv$doseValues), calculate.probabilities, DUEenv=DUEenv)
    plotProbsAndEUsimplified(DUEenv)
  }


calculate.probabilities.allDoses <- function( DUEenv) {
 # DUEenv$utility  ### To make sure the reactivity happens.
  #theDUEenv = DUEenv
  DUEenv$sevenprobs <- 
    sapply(log10(DUEenv$doseValues), 
           calculate.probabilities, DUEenv=DUEenv, utility=DUEenv$utility)
  
  DUEenv$highestprob..Rt <- max(DUEenv$sevenprobs["Rt",])
  DUEenv$highest.EU <- max(DUEenv$sevenprobs["EU",])
  DUEenv$best.dose.p.Rt <- DUEenv$doseValues[DUEenv$sevenprobs["Rt",]==DUEenv$highestprob..Rt]	
  DUEenv$best.dose.EU <- DUEenv$doseValues[DUEenv$sevenprobs["EU",]==DUEenv$highest.EU] [1]
  DUEenv$best.dose.p.T <- max(DUEenv$doseValues[(DUEenv$sevenprobs["T",]-DUEenv$MTDtoxicity)<=0])
}

####Code for app.R starts here####

plotProbsAndEUsimplified <- function(DUEenv) {
  calculate.probabilities.allDoses(DUEenv)
  # cat("Redrawing ProbsAndEU: utility = ", unlist(DUEenv$utility), "\n")
  convertEU <- function(x, isEU=TRUE) {
    #### Map EU on right axis from [-1, 1] to [0,1] on left axis.
    if(isEU) 
       (x+1)/2
    else x
  }
  par(mar =  c(5,4,4,6) + 0.1, xpd=NA )
  plot(DUEenv$doseValues, DUEenv$sevenprobs[1,],type="l",col=0,lwd=1, 
       xlim=c(DUEenv$minDose, DUEenv$maxDose), ylim=c(0,1),
       axes=FALSE,  log="x", xlab="", ylab=""
  )
  
  DUEenv$parPlotSize.ProbsAndEU <- par("plt")
  DUEenv$usrCoords.ProbsAndEU <- par("usr")
  if(browseIf(message="Just finished plot-- not yet done title-- check sevenprobs[1,]")) browser()
  # plot.title="Probabilities and Expected Utility, E(U)"
  # title(main=plot.title, cex.main=2, col.main="blue")
  axis(side = 1, at = DUEenv$doseTicks)
  mtext(side=1, line=2.5, "Dose", cex=2)
  axis(2, at=c(0, 0.33, 0.6, 0.8, 1))
  mtext(side=2, line=2.5, "Probability", cex=2)

  axis(side = 4, line=1, lwd=1, at=(axisvalues<-c(0, 0.25, 0.5, 0.75, 1)), 
       col=rt.outcome.colors["EU"], labels=round(axisvalues*2-1, 1)
       , xpd=NA, cex=2)
  # Note: the "at" values are relative to axis 1, the labels are correct for EU.
  #xrange = par("usr")[1:2],
  # pushout = 1.02
  # EUlabelPosition = exp(log(xrange[1]) + (log(xrange[2])-log(xrange[1])*1.02) )
  mtext(side = 4, line = 4, text = "  E(Utility)", #outer=TRUE,
        xpd = NA, srt=90, cex=2, col=rt.outcome.colors['EU'],
        ylbias = -0.5)  ### default ylbias = 0.2. Has no effect.
  
  linetypes = c(rep(1,6), 1, 1)
  if(!is.null(DUEenv$probLineWidths)) {
    linewidths = DUEenv$probLineWidths
  } else
    linewidths = c(1, 3, 1, 1, 1, 1, 3, 1)
  EUindex = 7
  for(i in 1:8) {
    shortlist <- c(1, round(DUEenv$nDoses/2), DUEenv$nDoses)
    if(linewidths[i] > 0) {
      lines(DUEenv$doseValues, convertEU(DUEenv$sevenprobs[i,], i==EUindex),
            lty=linetypes[i], lwd=linewidths[i], 
            col=rt.outcome.colors[i])
      text(DUEenv$doseValues[shortlist],
           convertEU(DUEenv$sevenprobs[i, shortlist], i==EUindex), 
           col=rt.outcome.colors[i],
           label=rt.outcome.strings[i],
           cex=1.2, xpd=NA, adj=c(0, -1))
    }
  }
  segments(5, DUEenv$MTDtoxicity, DUEenv$best.dose.p.T, DUEenv$MTDtoxicity, lty=2, lwd=2, col=rt.outcome.colors["T"])
  segments(DUEenv$best.dose.p.T, DUEenv$MTDtoxicity, DUEenv$best.dose.p.T, -0.1, lty=2, lwd=2, col=rt.outcome.colors["T"])
  segments(DUEenv$best.dose.EU, 0, DUEenv$best.dose.EU, convertEU(DUEenv$highest.EU, TRUE), lty=2, lwd=2, 
           col=DUEenv$rt.outcome.colors["EU"])
  abline(v=DUEenv$selectedDose, col="green")
}
