# See  EurosarcBayes::freq_binom_two_bryantday_twostage

phase_one_exact = function(PrTox = c(.1,.2,.3,.4), N1=3,N2=3, MaxGo1 = 0, MinStop1 = 2, MaxToxGo = 1) {
  pr_stop_at = pr_enter_tier = pr_Go_1 = pr_Go_2 = pr_Term_1 = rep(NA, length(PrTox));
  level=1 ;
  pr_enter_tier[level]=1; 
  for ( level in 1:length(PrTox)){
    pr_Go_1[level] = pbinom(q = MaxGo1, size = N1, prob = PrTox[level]);
      # Pr(continue to the next cohort after the first half of the cohort | enter tier)
    pr_Term_1[level] = 1 - pbinom(MinStop1-1, N1, PrTox[level]);
      # Pr(terminate in the first half of the cohort | enter tier)
    
    k_neither = (MaxGo1+1):(MinStop1-1)
      # these are the first-group toxicity counts leaving the decision unclear. Default: k=1.
    pr_k_if_neither = dbinom(k_neither, N1, PrTox[level])
    maxOK_2 = MaxToxGo - k_neither    
      ### how many extra toxicities will be OK and still allow escalation.
      # For the default, this is 1 minus 1 = zero. 
    pr_ok_to_go =  dbinom(maxOK_2, N2, PrTox[level]) 
    # these are the probabilities of the ambiguous toxicity counts. Default: k=1.
    pr_Go_2[level] = sum(pr_k_if_neither * pr_ok_to_go )
      # Pr( entering 2nd half and then escalating | enter tier)
    
    pr_stop_at[level] = ( 1- ( pr_Go_1[level] + pr_Go_2[level])) * pr_enter_tier[level];  ## Marginal prob.
    if(level < length(PrTox))
       pr_enter_tier[level+1] = ( pr_Go_1[level] + pr_Go_2[level]) * pr_enter_tier[level];   ## Marginal prob. 
  }
  result = data.frame(pr_enter_tier, pr_Go_1, pr_Term_1, pr_Go_2, pr_stop_at)
  return(result)
}


phase_one_simulated = function(PrTox = c(.1,.2,.3,.4), N1=3,N2=3, MaxGo1 = 0, MinStop1 = 2, MaxToxGo = 1) {
  notDone = TRUE
  level = 1
  result = rep('NA', length(PrTox))
  while(notDone) {
    tox1 = rbinom(n = 1, size = N1, prob = PrTox[level])
    if(tox1 <= MaxGo1) 
      result[level] = 'escalate'
    else if(tox1 >= MinStop1)
      result[level] = 'MTDreached_1'
    else {
      tox2 = rbinom(n = 1, size = N2, prob = PrTox[level])
      if(tox1 + tox2 <= MaxToxGo) 
        result[level] = 'escalate'
      else 
        result[level] = 'MTDreached_2'
    }
    if(result[level]=='escalate') {
      if(level==length(PrTox)) {
        trialresult = 'no MTD'
        break
      }
      else
        level = level + 1
    }
    else { # MTD reached
      trialresult = paste('MTD at ', level)
      break
    }
  }
  return(list(results=result, trialresult=trialresult))
}


# TESTING

# sum(print(phase_one_exact()) [, 'pr_stop_at'] )
# 
# NREPS = 50000
# reps = lapply(1:NREPS, function(ignoreMe) phase_one_simulated())
# table(sapply(reps, '[[', 'trialresult'))/NREPS
# c(phase_one_exact() [, 'pr_stop_at'], 1 - sum((phase_one_exact()) [, 'pr_stop_at'] ) )
# ##  Very close.
# 
# c(phase_one_exact(PrTox = seq(.1,.9, by=.1)) [, 'pr_stop_at'], 1 - sum((phase_one_exact()) [, 'pr_stop_at'] ) )
# plot(seq(.1,1, by=.1), .Last.value)
