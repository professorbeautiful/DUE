## @knitr plottingFunctions

library(DUE)

envHolder = new.env()
getExampleFile = function(string) {
  dirString = ifelse(basename(getwd()) == "www", '../', '../inst/DUEshiny/')
  filename = paste0(dirString,  rev(grep(value=TRUE, string,
                                         dir(dirString)))[1]) 
  if(class(filename) == 'try-error') {
    cat("file not found ", string, "\n")
    print(getwd())
    print(dir())
  }
  filename
}

plotThresholdInVignette= function(string) {
  filename = getExampleFile(string) 
  if(class(filename) != 'try-error') {
    load(filename, envir = envHolder)
    rt.outcome.colors = envHolder$rt.outcome.colors
    try({
      plotThresholdContour(theDUEenv = envHolder$DUEsaving, context='vignette')
      mtext(side = 3, string, cex=1)
    })
  }
}
plotEUprobsInVignette= function(string) {
  filename = getExampleFile(string) 
  if(class(filename) != 'try-error') {
    load(filename, envir = envHolder)
    rt.outcome.colors = envHolder$rt.outcome.colors
    try({
      plotProbsAndEU(DUEenv = envHolder$DUEsaving, context='vignette')
      mtext(side = 3, string, cex=1)
    })
  }
}
