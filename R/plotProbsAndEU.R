plotProbsAndEU <-
  function() {
    if(DUEenv$settingSliderValues == TRUE)
      return
    read.Uvalues()  ### copies from the sliders to the vector "utility"
    DUEenv$sevenprobs <- DUEenv$sevenprobs <- sapply(log10(DUEenv$doseValues), calculate.probabilities)
    plotProbsAndEUsimplified()
  }


