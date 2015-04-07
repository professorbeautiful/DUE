setUtilitySliderValues <-
function(choice) {
        if(browseIf()) cat("Entering setUtilitySliderValues\n")
        DUEenv$utility <- DUEenv$utilityChoices[[choice]]
        DUEenv$settingSliderValues <-  TRUE   ### disable slider callbacks until all are done

        load.value.into.slider ("U.rt", unlist(DUEenv$utility["U.rt"]))
        load.value.into.slider ("U.rT", unlist(DUEenv$utility["U.rT"]))
        load.value.into.slider ("U.Rt", unlist(DUEenv$utility["U.Rt"]))
        load.value.into.slider ("U.RT", unlist(DUEenv$utility["U.RT"]))

		cat("setUtilitySliderValues: finished calls to load.value.into.slider\n ")
        #cat("Placing X\n")
        bigXoffset <- -15
        bigYoffset <-  5

        tkplace(DUEenv$label.utilitychoice,
                "-x", get.tkplaceinfo.x(DUEenv$img.prob),
                "-y", DUEenv$uY + 3 - DUEenv$uYdel +
                        DUEenv$uYdel*which(choice == DUEenv$utilityChoiceNames)
                        + bigYoffset,
                "-width", 25, "-height", 25)
        tkconfigure(DUEenv$label.utilitychoice, text="X", fg="red", bg=DUEenv$bgWindow, font=DUEenv$fontX)
        cat("Just set the label.utilitychoice text to X\n")
        for (aChoice in DUEenv$utilityChoiceNames) {
                aChoiceButton <- get(paste("button.", aChoice, sep=""), envir=DUEenv)
                if(aChoice == choice){
                        tkconfigure(aChoiceButton , fg="red")
                        tkconfigure(aChoiceButton , font=DUEenv$fontButtonX)
                }
                else {
                        tkconfigure(aChoiceButton , fg="black")
                        tkconfigure(aChoiceButton , font=DUEenv$fontButtonNotX)
                }
        }
        DUEenv$settingSliderValues <-  FALSE   ### re-enable slider callbacks.
        if(browseIf()) cat("Exiting setUtilitySliderValues\n")
}

