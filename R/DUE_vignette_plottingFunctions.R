## @knitr plottingFunctions
##  ```{r ref.label=plottingFunctions}
##  source()  works! Skip the knitr:read_chunk way.
 
getExampleFile = function(string, dirString = system('find .. -name DUEshiny', intern = T)) {
  
  #print(dirString)
  # ifelse(basename(getwd()) == "www", '../', '../inst/DUEshiny/')
  filename = paste0(dirString,  '/', rev(grep(value=TRUE, string,
                                         dir(dirString)))[1]) 
  #print(filename)
  filename
}

plotThresholdInVignette= function(string, dirString = system('find .. -name DUEshiny', intern = T)) {
  envHolder = new.env()
  filename = getExampleFile(string, dirString) 
  load(filename, envir = envHolder)
  rt.outcome.colors = envHolder$rt.outcome.colors
  try({
    plotThresholdContour(theDUEenv = envHolder$DUEsaving, context='vignette')
    mtext(side = 3, string, cex=1)
  })
  return(envHolder)
}
plotEUprobsInVignette= function(string, envHolder) {
  #filename = getExampleFile(string) 
  #if(class(filename) != 'try-error') {
  #  load(filename, envir = envHolder)
    rt.outcome.colors = envHolder$rt.outcome.colors
    try({
      plotProbsAndEU(DUEenv = envHolder$DUEsaving, context='vignette')
      mtext(side = 3, string, cex=1)
    })
  #}
}
