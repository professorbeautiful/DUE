recalculate.offdiagonals <- function (correl, .thisPop, theDUEenv=DUEenv) 
{
  #DUEenv = theDUEenv
  DUEenv = get('DUEenv', envir=parent.frame())
  #DUEenv = get('DUEenv', envir = parent.frame(2))
  #DUEget(c(nPops,the.correlations.pop,the.variances.pop,the.logmedians.pop))
  if (missing(.thisPop)) {
    for (..thisPop in 1:DUEenv$nPops) 
      recalculate.offdiagonals(correl=correl, .thisPop = ..thisPop, theDUEenv=DUEenv )
    return(NULL)
  }
  if (!missing(correl)) {
    #cat("recalculate.offdiagonals:  correl=", correl, "\n")
    DUEenv$the.correlations.pop[[DUEenv$thisPop]] <<- correl
    print(DUEenv$the.correlations.pop)
  }
  
  DUEenv$the.variances.pop[[.thisPop]][2, 1] <<- DUEenv$the.variances.pop[[.thisPop]][1, 2] <<-
    DUEenv$the.correlations.pop[[.thisPop]] *
    sqrt(DUEenv$the.variances.pop[[.thisPop]][1, 1]) *
    sqrt(DUEenv$the.variances.pop[[.thisPop]][2, 2])
  return(NULL)
}

