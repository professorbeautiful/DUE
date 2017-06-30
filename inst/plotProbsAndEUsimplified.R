plotProbsAndEUsimplified <- reactive( {
  DUEenv$utility  ### To make sure the reactivity happens.
  cat("Redrawing ProbsAndEU: utility = ", unlist(DUEenv$utility), "\n")
  convert <- function(x, i=7) {
    #### Map [-1, 1] to [0,1]  for plotting utility against right axis.
    if(i!=7) return(x);
    (x+1)/2
  }
  DUEenv$sevenprobs <- 
    sapply(log10(DUEenv$doseValues), 
           calculate.probabilities, utility=DUEenv$utility)
  
  highestprob..Rt <- max(DUEenv$sevenprobs["Rt",])
  highest.EU <- max(DUEenv$sevenprobs["EU",])
  best.dose.p.Rt <- DUEenv$doseValues[DUEenv$sevenprobs["Rt",]==highestprob..Rt]	
  DUEenv$best.dose.EU <- DUEenv$doseValues[DUEenv$sevenprobs["EU",]==highest.EU] [1]
  DUEenv$best.dose.p.T <- max(DUEenv$doseValues[(DUEenv$sevenprobs["T",]-DUEenv$MTDtoxicity)<=0])
  
  plot(DUEenv$doseValues, DUEenv$sevenprobs[1,],type="l",col=0,lwd=1, 
       xlim=c(10,3500), ylim=c(0,1),
       axes=FALSE, xlab="Dose", ylab="Probability", log="x"
  )
  DUEenv$parPlotSize.ProbsAndEU <- par("plt")
  DUEenv$usrCoords.ProbsAndEU <- par("usr")
  if(browseIf(message="Just finished plot-- not yet done title-- check sevenprobs[1,]")) browser()
  plot.title="Probabilities and E(U)"
  title(main=plot.title, cex.main=1.5, col.main="blue")
  axis(2, at=c(0, 0.33, 0.6, 0.8, 1))
  axis(1, at = DUEenv$doseTicks)
  axis(4, lwd=2, line=-3, at=(axisvalues<-c(0, 0.25, 0.5, 0.75, 1)), 
       col=DUEenv$rt.outcome.colors["EU"], labels=round(axisvalues*2-1, 1) )
  text(3800, 0.50, "  E(Utility)", srt=90) 
  
  linetypes = c(rep(1,6), 1)
  if(!is.null(DUEenv$probLineWidths)) {
    linewidths = DUEenv$probLineWidths
  } else
    linewidths = c(1, 3, 1, 1, 1, 1, 3)
  for(i in 1:7) {
    shortlist <- c(1, round(DUEenv$nDoses/2), DUEenv$nDoses)
    if(linewidths[i] > 0) {
      lines(DUEenv$doseValues, convert(DUEenv$sevenprobs[i,], i),
            lty=linetypes[i], lwd=linewidths[i], 
            col=DUEenv$rt.outcome.colors[i])
      text(DUEenv$doseValues[shortlist],
           convert(DUEenv$sevenprobs[i, shortlist], i), 
           col=DUEenv$rt.outcome.colors[i],
           label=DUEenv$rt.outcome.strings[i],				cex=1.2)
    }
  }
  segments(5, DUEenv$MTDtoxicity, DUEenv$best.dose.p.T, DUEenv$MTDtoxicity, lty=2, lwd=2, col=DUEenv$rt.outcome.colors["T"])
  segments(DUEenv$best.dose.p.T, DUEenv$MTDtoxicity, DUEenv$best.dose.p.T, -0.1, lty=2, lwd=2, col=DUEenv$rt.outcome.colors["T"])
  segments(DUEenv$best.dose.EU, 0, DUEenv$best.dose.EU, convert(highest.EU), lty=2, lwd=2, 
           col=DUEenv$rt.outcome.colors["EU"])
  abline(v=DUEget('favoriteDose'), col="green")
})

