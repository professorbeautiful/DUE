#' pmvnorm.mixture
#' partialCumulative
#' 
#' Cumulative probability of a lognormal mixture distribution.
#' 
#' @param DUEenv Environment object or reactive values object containing model parameters.
#' @param Rrange Range of values for response (R) thresholds.
#' @param Trange Range of values for toxicity (T) thresholds.
#' @param i (partialCumulative) Which subgroup.
#' @param RorT (partialCumulative) Which threshold dimension.
#' @param logdose (partialCumulative) Logarithm (natural) of dose.
#' @param log10dose (partialCumulative) Logarithm (base 10) of dose.
#' @param utility Utility table for treating.
#' @details Details for pmvnorm.mixture

pmvnorm.mixture = function(DUEenv, Rrange, Trange) {
  vEpsilon =  diag(rep(1e-8,2))  ### in case a variance got stuck at zero.
  sum(apply(as.array(1:DUEenv$nPops), 1, function(i)
    DUEenv$proportions[i] * pmvnorm(lower=c(Rrange[1], Trange[1]), upper=c(Rrange[2], Trange[2]), 
                                    mean=DUEenv$the.logmedians.pop[[i]],
                                    sigma=vEpsilon + DUEenv$the.variances.pop[[i]])) )
}

#' partialCumulative
#' 
#' @description Normal univariate cumulative for one subgroup
#' 
#' @details Details for pmvnorm.mixture

partialCumulative = 
  function(DUEenv, i, RorT, logdose) {
    DUEenv$proportions[i] * pnorm(logdose, 
                                  mean=DUEenv$the.logmedians.pop[[i]] [RorT], 
                                  sd=sqrt(DUEenv$the.variances.pop[[i]] [RorT,RorT]))
  }

#' calculate.probabilities
#' 
#' 
#' @description Produce a vector of probabilities and expected utility.
#' @details calculate.probabilities
#' 
#' @return A vector of length 8:
#' \code{
#' probability.vector <- c(
#' R=p.R.marginal, # marginal probability of response
#' T=p.T.marginal, # marginal probability of toxicity 
#' rt=p.rt,        # probability of non-response and non-toxicity 
#' rT=p.rT,        # probability of non-response and toxicity
#' Rt=p.Rt,        # probability of response and non-toxicity
#' RT=p.RT,        # probability of response and toxicity
#' EU=expected.utility,        # 
#' RLT=p.RLT       # probability of response-limiting toxicity event
#'                 #  (a toxicity so severe that R cannot happen, 
#'                 # or might as well not have;  RLT has occurred.)
#' )
#' }
#'
#' @examples
#'   DUEshinyHome = system.file(package='DUE'
#'   , 'DUEshiny')
#'   aFile = grep(v=T, 'Simple-One-Pop-tight-CV.rdata', dir(DUEshinyHome))
#'   load(paste(DUEshinyHome, aFile, sep='/'))
#'   calculate.probabilities(DUEsaving, log10dose = 2.3)

calculate.probabilities <-  ### We will remove utility & EU from this in the future.
  function(DUEenv, log10dose, logdose, utility, includeEU = TRUE, ...) {
    
    if(!missing(log10dose))
      logdose = log(10) * log10dose
    
    # if(! missing(changes))
    #   eval(parse(text=changes), envir = DUEenv)
    arglist = list(...)
    for(arg in names(arglist))  DUEenv[[arg]] = arglist[[arg]]
    
    
    p.R.marginal <-  sum(apply(as.array(1:DUEenv$nPops), 1, partialCumulative, DUEenv=DUEenv, RorT=1, logdose=logdose) )
    p.T.marginal <-  sum(apply(as.array(1:DUEenv$nPops), 1, partialCumulative, DUEenv=DUEenv, RorT=2, logdose=logdose) )
    
    p.rt <- pmvnorm.mixture(DUEenv=DUEenv, 
                            Rrange=c(logdose, Inf), Trange=c(logdose, Inf))
    p.rT <- pmvnorm.mixture(DUEenv=DUEenv, 
                            Rrange=c(logdose, Inf), Trange=c(-Inf, logdose))
    p.Rt <- pmvnorm.mixture(DUEenv=DUEenv, 
                            Rrange=c(-Inf, logdose), Trange=c(logdose, Inf))
    p.RT <- pmvnorm.mixture(DUEenv=DUEenv, 
                            Rrange=c(-Inf, logdose), Trange=c(-Inf, logdose))
    ## Adjustments for response-limiting events (RLT)
    p.RLT <- pmvnorm.mixture(DUEenv=DUEenv, 
                             Rrange=c(-Inf, logdose), 
                             Trange=c(-Inf, logdose - DUEenv$Kdeath/log(10)))

    ## Kdeath = 0 means that
    ## People whose tox threshold is below logdose will have toxicity.
    ## People whose tox threshold is below logdose - Kdeath will have 
    ## toxicity so severe that R cannot happen;  RLT has occurred.
    ## Thus Kdeath = 0 means that RT cannot happen.
    ## But Kdeath = Inf means that RLT never happens.
    
    #### Adjustments for response-limiting events (RLT) ####
    p.rT <- p.rT + p.RLT   #RLT converts RT events into rT events.
    p.RT <- p.RT - p.RLT
    
    negativeSanity = -1e-4
    
    #### Sanity tests ####
    if(any(c(p.RT, p.Rt, p.rT, p.rt, p.RLT) < negativeSanity))
      browser(text = 'negative probability problem')
    if( abs(p.RT + p.Rt + p.RLT - p.R.marginal)  >  0.01)
      browser(text = 'p.R problem')
    if( abs(p.RT + p.rT - p.T.marginal)  >  0.01)
      browser(text = 'p.T problem')
    
    ####  Adjustments for refractoriness: ####  
    # Every R category is converted proportionally to its corresponding "r". 
    p.rT <- p.rT + DUEenv$refractory*p.RT
    p.RT <- p.RT - DUEenv$refractory*p.RT
    p.rt <- p.rt + DUEenv$refractory*p.Rt
    p.Rt <- p.Rt - DUEenv$refractory*p.Rt
    p.RLT <- p.RLT - DUEenv$refractory*p.RLT
    p.R.marginal <- p.R.marginal - DUEenv$refractory*p.R.marginal

        #### Sanity tests ####
    if(any(c(p.RT, p.Rt, p.rT, p.rt, p.RLT) < negativeSanity))
      browser(text = 'negative probability problem')
    if( abs(p.RT + p.Rt + p.RLT - p.R.marginal)  >  0.01)
      browser(text = 'p.R problem')
    if( abs(p.RT + p.rT - p.T.marginal)  >  0.01)
      browser(text = 'p.T problem')
    
    pQuadrants <- c(p.rt,p.rT,p.Rt,p.RT)
    if(includeEU) {
      if(missing(utility))
        utility = DUEenv$utility
      else if(is.character(utility)) 
        utility = DUEenv$utilityChoices[utility]
      #read.Uvalues()  ### copies from the sliders to the vector "utility"
      expected.utility <- sum(pQuadrants*utility)
    }
    if(	browseIf(FALSE
                 #exp(logdose) > 950
                 , message="Let's check on utility")) {
      #browser()
      cat("---- ", exp(logdose), " ----\n")
      print(pQuadrants)
      print(utility)
      print(DUEenv$utility)
      print(expected.utility)
    }
    probability.vector <- c(
      R=p.R.marginal,
      T=p.T.marginal,
      rt=p.rt,
      rT=p.rT,
      Rt=p.Rt,
      RT=p.RT,
      RLT=p.RLT
    )
    if(includeEU) 
      probability.vector = c(probability.vector, EU=expected.utility)
    return(probability.vector)
  }

calculate.probabilities.and.EU <- 
  function(DUEenv, log10dose, utility, ...) {
    sevenprobs = calculate.probabilities(
      DUEenv, log10dose, utility, includeEU=FALSE,...)
    pQuadrants <- with(as.data.frame(t(sevenprobs)), c(rt, rT, Rt, RT))
    if(missing(utility))
      utility = DUEenv$utility
    else if(is.character(utility)) 
      utility = DUEenv$utilityChoices[utility]
#    expected.utility <- sum(pQuadrants*utility)
    cat('str(pQuadrants)', '\n')
    print(str(pQuadrants))
    expected.utility <- apply(
      pQuadrants,
      1, function(pQ) sum(pQ*utility)
    )
    cat('str(expected.utility)', '\n')
    print(str(expected.utility))
    browser()
    return(c(sevenprobs, EU=expected.utility) )
  }
    
calculate.all.EU <- 
  function(sevenprobs, utilityChoices) {
    pQuadrants <- with(as.data.frame(t(sevenprobs)), 
                       c(rt, rT, Rt, RT))
    browser()
    allEU = sapply(utilityChoices, function(utility)
      sum(pQuadrants*utility) )
    return(allEU )
  }


calculate.probabilities.allDoses <- function(DUEenv, ...) {
    if(is.null(DUEenv$log10doseValues))
      DUEenv$log10doseValues = log10(DUEenv$doseValues)
    sapply(DUEenv$log10doseValues, calculate.probabilities, DUEenv=DUEenv, ... = ...)
}

calibrate = function(inputs, outputs, target) {
  # assumes inputs are sorted.
  fit = loess(inputs ~ outputs)
  predict(fit, target)
} 

extractUtilitySummaries <- function(eightprobs, log10doseValues, 
                                    MTDtoxicity, thisDUEenv=DUEenv) {
  highest.Rt = max(eightprobs["Rt",])
  highest.EU = max(eightprobs["EU",])
  lowestprob.Rt = min(eightprobs["Rt",])
  lowest.EU = min(eightprobs["EU",])
  doseValues = 10^log10doseValues
  best.dose.Rt = doseValues[eightprobs["Rt",]==highest.Rt] [1]
  OptDose.EU = doseValues[eightprobs["EU",]==highest.EU] [1]
  
  # MTDdose = 10^calibrate(log10doseValues, eightprobs['T', ], MTDtoxicity)
  MTDdoseFind = uniroot(f = function(dose)
    calculate.probabilities(DUEenv=thisDUEenv, log10(dose))['T'] - MTDtoxicity,
    interval = range(thisDUEenv$doseValues))
  MTDdose = MTDdoseFind$root
  EUatMTDdose = calibrate( eightprobs['EU', ], log10doseValues, log10(MTDdose))
  TatMTDdose = calculate.probabilities(DUEenv=thisDUEenv, log10(MTDdose))['T']
  #calibrate( eightprobs['T', ], log10doseValues, log10(MTDdose))
  # EUatMTDdose = calculate.probabilities(
  #   DUEenv = DUEenv, logdose = MTDdose )['EU', ]
  return(data.frame(
    highest.EU,
    OptDose.EU,
    EUatMTDdose,
    MTDdose,
    lowest.EU,
    highest.Rt,
    best.dose.Rt,
    TatMTDdose
  ))  
}

checkcalcs=function(...){
  p8=calculate.probabilities(DUEenv=denv, 3, ...); 
  c(sum=sum(p8[3:6]), 
    Rsum=p8['Rt']+p8['RT']+p8['RLT'],
    Tsum=p8['RT']+p8['rT'],
    p8)
}
