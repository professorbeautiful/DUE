
#### TODO:  work needed : save extractUtilitySummaries. ####
##  Parameters extracted from 
# calculate.probabilities() pmvnorm.mixture() 
# partialCumulative() 
#  recalculate.offdiagonals() ... when the correlation is changed.
# recalculate.means.and.variances() ... creates the.variances.pop

designParameterNames = 
  strsplit(
    split = "[ \n]",
'utility doseValues
nPops the.CVs.pop the.correlations.pop the.medianThresholds.pop
refractory Kdeath proportions
'  
)[[1]]

load(paste0('inst/DUEshiny/',
            'DUEsaved##------ 2017-08-19 15:25:20 ------##Pharmacokinetic-example-two-groups .rdata'
))
get(env=DUEsaving, 'mu.R')  ## omit
get(env=DUEsaving, 'log10dose')  ## omit.  just one dose
get(env=DUEsaving, 'doseValues')  ## omit
get(env=DUEsaving, 'the.variances.pop') 
    ## omit. Diagonals calculated from the.medianThresholds.pop and the.CVs.pop
  ## Off diagonals from the.correlations.pop and the diagonals.
get(env=DUEsaving, 'theLognormalParameters')  # omit

lapply(designParameterNames, function(p)
  (get(p, env=DUEsaving)) )

### Predicated on the value of nPops.
designParameterNamesExpanded = 
  c(names(DUEsaving$utility),
    paste0(
      paste0('the.CVs.pop[[', 1:DUEsaving$nPops, ']]'),
      '[', c('R','T'), ']')
    ,
    paste0(
      paste0('the.medianThresholds.pop[[', 1:DUEsaving$nPops, ']]'),
      '[', c('R','T'), ']')
    ,
    paste0('the.correlations.pop[', 1:DUEsaving$nPops, ']')
    ,
    
    'refractory',
    'Kdeath',
    paste0('proportions[', 1:DUEsaving$nPops, ']')
  )

 
  
eachRow = function(row){
  arglist = design[row, ]
  for(arg in names(arglist))  DUEsaving[[arg]] = arglist[[arg]]
  calculate.probabilities (DUEsaving, log10dose=2, utility) 
  ### TODO save extractUtilitySummaries
}
calculate.probabilities.design <- function(design, DUEenvRow=DUEinits.default, ...) {
  # For each row of the design matrix, place the values in calculate the 
  sapply(1:nrow(design), eachRow)
  DUEsaving$eightprobs <- 
    sapply(DUEsaving$log10doseValues, calculate.probabilities, DUEenv=DUEsaving)
}
