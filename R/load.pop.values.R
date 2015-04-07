load.pop.values <-
function() {
	if(	browseIf(message="browseIf: load.pop.values")) browser()

##   CV
##  the.means.pop corresponds to  uStar.   #### These are on log scales.
##  the.variances.pop corresponds to  vStar.
##  (and the covariances within the.variances.pop must be computed from recalculate.offdiagonals)

## The user will not see these.  They are computed, then used for the contours.

## The user sees:
##	     	the.Ethresholds.pop
##			the.CVs.pop
##			the.correlations.pop


	DUEenv$settingSliderValues <- TRUE
	load.value.into.entrybox("nPops", DUEenv$nPops)
	load.value.into.entrybox("thisPop", DUEenv$thisPop)
	load.value.into.entrybox("whichFollows", DUEenv$whichFollows)
	load.value.into.entrybox("popFraction", DUEenv$proportions[DUEenv$thisPop])

	load.value.into.entrybox("mu.R", round(DUEenv$the.Ethresholds.pop[[DUEenv$thisPop]] [1]))
	load.value.into.slider("mu.R", round(DUEenv$the.Ethresholds.pop[[DUEenv$thisPop]] [1]))

	load.value.into.entrybox("mu.T", round(DUEenv$the.Ethresholds.pop[[DUEenv$thisPop]] [2]))
	load.value.into.slider("mu.T", round(DUEenv$the.Ethresholds.pop[[DUEenv$thisPop]] [2]))

	load.value.into.entrybox("CV.R", DUEenv$the.CVs.pop[[DUEenv$thisPop]] [1])
	load.value.into.slider("CV.R", DUEenv$the.CVs.pop[[DUEenv$thisPop]] [1])

	load.value.into.entrybox("CV.T", DUEenv$the.CVs.pop[[DUEenv$thisPop]] [2])
	load.value.into.slider("CV.T", DUEenv$the.CVs.pop[[DUEenv$thisPop]] [2])

	load.value.into.entrybox("rho.RT", DUEenv$the.correlations.pop[[DUEenv$thisPop]])
	load.value.into.slider("rho.RT", DUEenv$the.correlations.pop[[DUEenv$thisPop]])

	load.value.into.slider ("refractory", DUEenv$refractory)
	load.value.into.slider ("Kdeath", DUEenv$Kdeath)
	DUEenv$settingSliderValues <- FALSE
# })
}

