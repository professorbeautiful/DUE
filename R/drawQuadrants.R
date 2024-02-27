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
    
    rt.for.quadrant = function(X)
      paste0(rt.outcome.strings(X), '\n', 
             ifelse(DUEenv$addProbsToQuadrants,
                    signif(digits = 4,probs[X]), ''))
             
    text(15, selectedDose/2, rt.for.quadrant("RT"), col = rt.outcome.colors("RT"), 
         font=bold_italic, cex = cexQ)
    text(700, selectedDose/2, rt.for.quadrant("rT"), col = rt.outcome.colors("rT"), 
         font=bold_italic, cex = cexQ)
    text(15, 700, rt.for.quadrant("Rt"), col = rt.outcome.colors("Rt"), 
         font=bold_italic, cex = cexQ)
    text(700, 700, rt.for.quadrant("rt"), col = rt.outcome.colors("rt"), 
         font=bold_italic, cex = cexQ)
    RTlowerbound = selectedDose/10^Kdeath
    lines(c(selectedDose, selectedDose), c(RTlowerbound, 1000), 
          lty = 1, col = "green", lwd = 3)
    lines(c(10^(par()$usr)[1], selectedDose), 
          c(RTlowerbound, RTlowerbound), lty = 1, 
          col = rt.outcome.colors('RT'), lwd = 3)
    abline(h = selectedDose, lty = 1, col = "#00FF00", lwd = 3)
  }

