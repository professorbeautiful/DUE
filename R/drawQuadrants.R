drawQuadrants <-
  function (cexQ = 4) 
  {
    DUEenv = get('DUEenv', envir=parent.frame())
    
    DUEcopy("selectedDose")
    if(is.null(selectedDose)) selectedDose = DUEenv$favoriteDose
    DUEcopy("rt.outcome.strings")
    #DUEcopy("rt.outcome.colors")
    DUEcopy("Kdeath")
    bold_italic = 4
    probs = calculate.probabilities(
      DUEenv, log10dose=log10(DUEenv$selectedDose))
    
    #probs.for.quadrant =  
    if(DUEenv$addProbsToQuadrants){
      xpos = DUEenv$xpos
      ypos = DUEenv$ypos
      text(15, selectedDose/2, round(digits=2,probs["RT"]), col = rt.outcome.colors("RT"), 
           font=bold_italic, cex = cexQ/2, adj=c(xpos, ypos))
      text(700, selectedDose/2, round(digits=2,probs["rT"]), col = rt.outcome.colors("rT"), 
           font=bold_italic, cex = cexQ/2, adj=c(xpos, ypos))
      text(15, 700, round(digits=2,probs["Rt"]), col = rt.outcome.colors("Rt"), 
           font=bold_italic, cex = cexQ/2, adj=c(xpos, ypos))
      text(700, 700, round(digits=2,probs["rt"]), col = rt.outcome.colors("rt"), 
           font=bold_italic, cex = cexQ/2, adj=c(xpos, ypos))
    }
      
    # rt.for.quadrant = rt.outcome.strings
    
    text(15, selectedDose/2, ("RT"), col = rt.outcome.colors("RT"), 
         font=bold_italic, cex = cexQ)
    text(700, selectedDose/2, ("rT"), col = rt.outcome.colors("rT"), 
         font=bold_italic, cex = cexQ)
    text(15, 700, ("Rt"), col = rt.outcome.colors("Rt"), 
         font=bold_italic, cex = cexQ)
    text(700, 700, ("rt"), col = rt.outcome.colors("rt"), 
         font=bold_italic, cex = cexQ)
    abline(h = selectedDose, lty = 1, col = "red", lwd = 3)
#  Kdeath line adjustment
    RTlowerbound = selectedDose/10^Kdeath
    lines(c(selectedDose, selectedDose), c(RTlowerbound, 1000), 
          lty = 1, col = "green", lwd = 3)
    lines(c(10^(par()$usr)[1], selectedDose), 
          c(RTlowerbound, RTlowerbound), lty = 1, 
          lwd = 3, col='red') #  col = rt.outcome.colors('RT'), )
  }

