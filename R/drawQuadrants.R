drawQuadrants <-
  function () 
  {
    DUEenv = get('DUEenv', envir=parent.frame())
    
    DUEcopy("favoriteDose")
    DUEcopy("favoriteLogDose")
    DUEcopy("rt.outcome.strings")
    DUEcopy("rt.outcome.colors")
    DUEcopy("Kdeath")
    cexQ = 4; OKfont = c("sans serif", "bold")
    text(15, favoriteDose/2, rt.outcome.strings["RT"], col = rt.outcome.colors["RT"], 
         vfont=OKfont, cex = cexQ)
    text(700, favoriteDose/2, rt.outcome.strings["rT"], col = rt.outcome.colors["rT"], 
         vfont=OKfont, cex = cexQ)
    text(15, 700, rt.outcome.strings["Rt"], col = rt.outcome.colors["Rt"], 
         vfont=OKfont, cex = cexQ)
    text(700, 700, rt.outcome.strings["rt"], col = rt.outcome.colors["rt"], 
         vfont=OKfont, cex = cexQ)
    RTlowerbound = favoriteDose/10^Kdeath
    lines(c(favoriteDose, favoriteDose), c(RTlowerbound, 1000), 
          lty = 1, col = "green", lwd = 3)
    lines(c(10^(par()$usr)[1], favoriteDose), c(RTlowerbound, 
                                                RTlowerbound), lty = 1, col = "green", lwd = 3)
    abline(h = favoriteDose, lty = 1, col = "green", lwd = 3)
  }

