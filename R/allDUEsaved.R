allDUEsaved = dir('inst/DUEshiny', pattern = 'DUEsaved*')
allREADMEs = sapply(allDUEsaved, function(f) {
  load(paste0('inst/DUEshiny/', f))
  return(DUEsaving$READMEtext)      
})