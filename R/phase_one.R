MAXLEVEL=20;
N1 = 3;
N2 = 3;
MaxGo1 = 0;
MinStop1 = 2;
MaxToxGo = 1;
PrLength = 2;
verticalOffset = 60;
PrTox =  p_stop = p_go_on = p_sum = rep(NA, MAXLEVEL);
PrTox[1] = 0.1;
PrTox[2] = 0.2;

probbnml = function( pr,n, m)
  pbinom(m, n, pr)

run_phase_one = function(PrTox) {
  level=1 ;
  p_go_on[level]=1; 
  p_sum[level]=0;
  for ( level in 2:length(PrTox)){
    p_Go_1 = probbnml(PrTox[level-1],N1,MaxGo1);
    p_Term_1 = 1 - probbnml(PrTox[level-1],N1,MinStop1-1);
    p_Go_2 = 0;
    for (k in (MaxGo1+1):(MinStop1-1)){   ### k is #DLT in the unsure range
      p_Tent = dbinom(k, N1, PrTox[level-1])  ### Pr we got k 
      # Will we get few enough to continue?
      maxNumberOK = MaxToxGo -(MaxGo1+1)
      p_Go_2 = p_Go_2 +  p_Tent * probbnml(PrTox[level-1], N2, maxNumberOK);
    }
    p_stop[level] = ( 1- ( p_Go_1 + p_Go_2)) * p_go_on[level-1];  ## Marginal prob.
    p_go_on[level] = ( p_Go_1 + p_Go_2) * p_go_on[level-1];   ## Marginal prob. 
    p_sum[level] = p_sum[level-1] + p_stop[level]; 
  }
  p_go_on[level] = p_go_on[level-1];
  p_stop[level] = p_go_on[level];
  p_sum[level] =  p_sum[level-1] + p_stop[level];
  for(level in 1:length(PrTox))
    cat(" level ", level, "p_go_on", p_go_on[level], '\n',
        "  P_stop=", p_stop[level], '\n');
}
