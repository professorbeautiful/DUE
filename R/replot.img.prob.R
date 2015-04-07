replot.img.prob <- function() {
  cat("replot.img.prob: DUEenv$settingSliderValues = ", DUEenv$settingSliderValues, "\n")
  #        if( ! DUEenv$settingSliderValues) {
  # moving by hand, so disable the "standard utility" indicators.
  tkconfigure( DUEenv$label.utilitychoice, text = "")
  # }
  tkrreplot(DUEenv$img.prob)
  matchingLabel = ""
  sapply( DUEenv$utilityChoiceNames, function(aChoice) {
    utilitiesMatch = all(DUEenv$utility == DUEenv$utilityChoices[[aChoice]])
    aChoiceButton <- get(paste("button.", aChoice, sep=""), envir=DUEenv)
    if(utilitiesMatch){
      tkconfigure(aChoiceButton , fg="red")
      tkconfigure(aChoiceButton , font=DUEenv$fontButtonX)
      tkconfigure( DUEenv$label.utilitychoice, text = "X")
      assign("DUEenv$utilityChoiceNames", aChoice)
    }
    else {
      tkconfigure(aChoiceButton , fg="black")
      tkconfigure(aChoiceButton , font=DUEenv$fontButtonNotX)                      	
    }
  }
  ) 
  cat("utilities: ", DUEenv$utility, "  utilityMatches: ", matchingLabel, "\n")
  return(invisible(NULL))
}
