pmvnorm.mixture = function(DUEenv, Rrange, Trange) {
  vEpsilon =  diag(rep(1e-8,2))  ### in case a variance got stuck at zero.
  sum(apply(as.array(1:DUEenv$nPops), 1, function(i)
    DUEenv$proportions[i] * pmvnorm(lower=c(Rrange[1], Trange[1]), upper=c(Rrange[2], Trange[2]), 
                                    mean=DUEenv$the.logmedians.pop[[i]],
                                    sigma=vEpsilon + DUEenv$the.variances.pop[[i]])) )
}

partialCumulative = 
  function(DUEenv, i, RorT) {
    DUEenv$proportions[i] * pnorm(logdose, 
                                  mean=DUEenv$the.logmedians.pop[[i]] [RorT], 
                                  sd=sqrt(DUEenv$the.variances.pop[[i]] [RorT,RorT]))
  }

calculate.probabilities <-
  function(DUEenv, log10dose, logdose, utility, changes) {
    
    ####  p.R.marginal :  marginal probability of response  ####
    ####  p.T.marginal :  marginal probability of toxicity  ####
    ####  p.rt:  probability of non-response and non-toxicity  ####
    ####  p.rT:  probability of non-response and toxicity      ####
    ####  p.Rt:  probability of response and non-toxicity      ####
    ####  p.RT:  probability of response and toxicity          ####
    ####  p.RLE:  probability of response-limiting toxicity event          ####
    ##      (a toxicity so severe that R cannot happen, or might as well not have;  RLE has occurred.)
    
    if(missing(utility))
      utility = DUEenv$utility
    else if(is.character(utility)) 
      utility = DUEenv$utilityChoices[utility]
    
    if(!missing(log10dose))
      logdose = log(10) * log10dose
    
    # syscall = sys.call(1)
    # print(as.list(syscall))
    if(! missing(changes))
      eval(parse(text=changes), envir = DUEenv)
    
    p.R.marginal <-  sum(apply(as.array(1:DUEenv$nPops), 1, partialCumulative, DUEenv=DUEenv, RorT=1) )
    p.T.marginal <-  sum(apply(as.array(1:DUEenv$nPops), 1, partialCumulative, DUEenv=DUEenv, RorT=2) )
    
    p.rt <- pmvnorm.mixture(DUEenv=DUEenv, 
                            Rrange=c(logdose, Inf), Trange=c(logdose, Inf))
    p.rT <- pmvnorm.mixture(DUEenv=DUEenv, 
                            Rrange=c(logdose, Inf), Trange=c(-Inf, logdose))
    p.Rt <- pmvnorm.mixture(DUEenv=DUEenv, 
                            Rrange=c(-Inf, logdose), Trange=c(logdose, Inf))
    p.RT <- pmvnorm.mixture(DUEenv=DUEenv, 
                            Rrange=c(-Inf, logdose), Trange=c(-Inf, logdose))
    
    ##  Adjustments for refractoriness
    p.R.marginal <- (1-DUEenv$refractory)*p.R.marginal
    p.rt <- p.rt + DUEenv$refractory*p.Rt
    p.Rt <- p.Rt - DUEenv$refractory*p.Rt
    p.rT <- p.rT + DUEenv$refractory*p.RT
    #p.RT <- p.RT - DUEenv$refractory*p.RT
    
    ## Adjustments for response-limiting events (RLE)
    ## Kdeath = 0 means that
    ## People whose tox threshold is below logdose will have toxicity.
    ## People whose tox threshold is below logdose - Kdeath will have 
    ## toxicity so severe that R cannot happen;  RLE has occurred.
    ## Thus Kdeath = 0 means that RT cannot happen.
    ## But Kdeath = Inf means that RLE never happens.
    p.RLE <- pmvnorm.mixture(DUEenv=DUEenv, 
                             Rrange=c(-Inf, logdose), 
                             Trange=c(-Inf, logdose - DUEenv$Kdeath/log(10)))
    #cat("p.RLE = ", p.RLE, "\n")
    p.rT <- p.rT + p.RLE   #RLE converts RT events into rT events.
    #p.RT <- p.RT - p.RLE
    p.RT <- (p.RT - p.RLE) * (1-DUEenv$refractory)
    
    browser(text = 'p.RLE problem', condition = (p.RT < 0))
    
    
    pQuadrants <- c(p.rt,p.rT,p.Rt,p.RT)
    #read.Uvalues()  ### copies from the sliders to the vector "utility"
    expected.utility <- sum(pQuadrants*utility)
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
      EU=expected.utility,
      RLE=p.RLE
    )
    return(probability.vector)
  }

checkcalcs=function(...){
  p7=calculate.probabilities(DUEenv=denv, 3, ...); 
  c(sum=sum(p7[3:6]), 
    Rsum=p7['Rt']+p7['RT']+p7['RLE'],
    Tsum=p7['RT']+p7['rT'],
    p7)
}
