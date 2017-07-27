phase_one_exact = function(PrTox = c(.1,.2,.3,.4), N1=3,N2=3, MaxGo1 = 0, MinStop1 = 2, MaxToxGo = 1) {
  pr_stop_at = pr_enter_tier = pr_Go_1 = pr_Go_2 = pr_Term_1 = rep(NA, length(PrTox));
  level=1 ;
  pr_enter_tier[level]=1; 
  for ( level in 2:length(PrTox)){
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
  result = cbind(pr_enter_tier, pr_Go_1, pr_Term_1, pr_Go_2, pr_stop_at)
  return(result)
}
