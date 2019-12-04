
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
get(env=DUEsaving, 'the.medianThresholds.pop')  # omit

lapply(designParameterNames, function(p)
  (get(p, env=DUEsaving)) )

### Predicated on the value of nPops.
RTindices = 1:2
DUEsaving$utility[1,'U.rt']
designParameterNamesExpanded_utilities = 
  paste0('utility[1,"', names(DUEsaving$utility), '"]')
designParameterNamesExpanded_pop = 
  c(apply(expand.grid(
      paste0('the.variances.pop[[', 1:DUEsaving$nPops, ']]'),
      paste0('[', RTindices, ']') ), 1, paste0, collapse='')
    ,
    apply(expand.grid(
      paste0('the.medianThresholds.pop[[', 1:DUEsaving$nPops, ']]'),
      paste0('[', RTindices, ']') ), 1, paste0, collapse='')
    , 
    paste0('the.correlations.pop[', 1:DUEsaving$nPops, ']')
    ,
    
    'refractory',
    'Kdeath',
    paste0('proportions[', 1:DUEsaving$nPops, ']')
  )
  # 
invisible(sapply(designParameterNamesExpanded, function(param) {
  cat( param, eval(parse(text=param), env=DUEsaving), '\n')
  (NULL)
  }))

calculate.probabilities.with.changes = function(
  newValues, log10dose=2){
  DUEtemp = DUEsaving
  for(param in names(newValues))  {
  #textToParse = paste0("assign('", param, "', value = ", newValue, ', envir =DUEtemp)')
#       eval(parse(text=paste0('assign(', param, ', value = , newValue, ', envir =DUEtemp') )
    textToParse = paste0("DUEtemp$", param, " = ", newValues[[param]])
    eval(parse(text=textToParse ))
    #cat(eval(parse(text=paste0('DUEtemp$', param))), '\n')
    #cat( param, eval(parse(text=param), env=DUEtemp), '\n') 
  }
  calculate.probabilities(DUEenv = DUEtemp, log10dose = log10dose, includeEU=FALSE)
}

designDF = data.frame(`the.variances.pop[[1]][1]` = 55,)
calculate.probabilities.with.changes(designDF )

log10doseValues = seq(log10(min(DUEsaving$doseTicks)), 
                             log10(max(DUEsaving$doseTicks)), 
                             length=25)
allProbs = sapply(log10doseValues, function(log10dose)
  calculate.probabilities.with.changes(designDF, log10dose = log10dose)
)
allEU = apply(allProbs, 2, calculate.all.EU, 
                          utilityChoices=DUEsaving$utilityChoices) 
summarize_allProbs = function(allProbs, thisDUEenv, MTDtoxicity=1/3,
                              doseValues = 10^log10doseValues
) {
  highest.Rt = max(allProbs["Rt",])
  lowestprob.Rt = min(allProbs["Rt",])
  best.dose.Rt = doseValues[allProbs["Rt",]==highest.Rt] [1]
  # MTDdose = 10^calibrate(log10doseValues, allProbs['T', ], MTDtoxicity)
  MTDdoseFind = uniroot(f = function(dose)
    calculate.probabilities(DUEenv=thisDUEenv, log10(dose))['T'] - MTDtoxicity,
    interval = range(thisDUEenv$doseValues))
  MTDdose = MTDdoseFind$root
  TatMTDdose = calculate.probabilities(DUEenv=thisDUEenv, log10(MTDdose))['T']
  return(data.frame(
    MTDdose,
    highest.Rt,
    best.dose.Rt,
    TatMTDdose
  ))  
}
summarize_allProbs(allProbs, DUEsaving)

summarize_allEU <- function(allEU=allEU, allProbs=allProbs,
                            thisDUEenv=DUEsaving, MTDtoxicity=1/3,
                            doseValues = 10^log10doseValues
) {
  summary_allProbs = summarize_allProbs(allProbs, thisDUEenv)
  apply(allEU, 1, function(EUrow) {
    highest.EU = max(EUrow)
    lowest.EU  = min(EUrow)
    OptDose.EU = doseValues[EUrow==highest.EU] [1]
    EUatMTDdose = calibrate( EUrow, log10doseValues, 
                             log10(summary_allProbs$MTDdose))
    return(data.frame(
      highest.EU,
      OptDose.EU,
      EUatMTDdose,
      lowest.EU
    ))  
  })
}
summarize_allEU (allEU, allProbs )
