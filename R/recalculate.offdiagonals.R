recalculate.offdiagonals <- function (correl, .thisPop) 
{
	#DUEget(c(nPops,the.correlations.pop,the.variances.pop,the.means.pop))
    if (missing(.thisPop)) {
        for (..thisPop in 1:DUEenv$nPops) recalculate.offdiagonals(.thisPop = ..thisPop)
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

