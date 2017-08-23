recalculate.means.and.variances <-
  function(DUEenv=DUEenv){
    for(i in 1:DUEenv$nPops) {
      for(j in 1:nrow(DUEenv$the.variances.pop[[i]])) {
        DUEenv$theLognormalParameters = FromNormalToLognormal(
          DUEenv$the.medianThresholds.pop[[i]][j], 
          DUEenv$the.CVs.pop[[i]][j])
        DUEenv$the.logmedians.pop[[i]][j] <- 
          DUEenv$theLognormalParameters["uStar"]
        #cat("recalculate.means.and.variances: the.logmedians.pop[[i]] = ",   DUEenv$the.logmedians.pop[[i]], "\n")
        DUEenv$the.variances.pop[[i]][j,j] <- 
          DUEenv$theLognormalParameters["vStar"]
        if(DUEenv$the.variances.pop[[i]][j,j] < 1e-7 ) {
          MINVARIANCE = 1e-2
          cat("Bad value (zero) for jth diagonal of the.variances.pop[[", i, "]]\n")
          DUEenv$the.variances.pop[[i]] [j,j] <- MINVARIANCE
        }
      }
    }
    recalculate.offdiagonals()
  }

