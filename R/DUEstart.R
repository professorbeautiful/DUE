
DUEstart = 
  function (inits, config) 
  {
    logdose<<-1
    require("mvtnorm")
    require("tcltk")
    require("tkrplot")
    if (!exists("DUEenv")) {
      data(DUEenvironmentDefault)
      ### Copy from initializations into working env.
      assign("DUEenv", DUEenvironmentDefault, pos=1)
      cat("NOTE: No DUEenv is found in your .GlobalEnv, \n")
      cat(" so it has been created for you by copying DUEenvironmentDefault\n")
      for(objname in names(DUEenv$DUEinits.default))
        assign(objname, get(objname, DUEenv$DUEinits.default), envir=DUEenv)
      # This copies objects from the default DUEinits, for ease of code reading/writing
    }
    if (!missing(inits)) 
      assign("DUEinits", inits, envir = DUEenv)
    else if (!exists("DUEinits", envir = DUEenv)) {
      if (exists("DUEinits", 1)) 
        assign("DUEinits", DUEenv$DUEinits, envir = DUEenv)
      else {
        assign("DUEinits", DUEenv$DUEinits.default, 
               envir = DUEenv)
      }
    }
    if (!missing(config)) 
      assign("DUEconfig", config, envir = DUEenv)
    else if (!exists("DUEconfig", envir = DUEenv)) {
      if (exists("DUEconfig", 1)) 
        assign("DUEconfig", DUEenv$DUEconfig, envir = DUEenv)
      else {
        assign("DUEconfig", DUEenv$DUEconfig.default, 
               envir = DUEenv)
      }
    }
    for (image in c("img.prob", "img.contour")) for (field in c("vscale", 
                                                                "hscale")) DUEenv$DUEconfig[image, field] <- ifelse(substring(version$os, 
                                                                                                                              1, 7) == "mingw32", 1.2, 0.9)
# TODO: make sure that X11 is running, can be run, or die gracefully.
#     if (substring(version$os, 1, 6) == "darwin") {
#       returnval = try(system("/usr/bin/open /Applications/Utilities/X11.app &"))
#       if (class(returnval) == "try-error") 
#         stop("X11 is required.  Make sure it is installed in Applications/Utilities")
#     }
    DUEenv$tkWindow <- tktoplevel()
    tkbind(DUEenv$tkWindow, "<Button-1>", function(x, y) cat(x, y, "\n"))
    tktitle(DUEenv$tkWindow) <- "Dose Choice: threshold parameters, personal utilities, probabilities, and E(U)"
    tkconfigure(DUEenv$tkWindow,
                height = DUEenv$DUEconfig["tkWindow", "height"],
                width = DUEenv$DUEconfig["tkWindow", "width"])
    DUEenv$bgWindow <- "darkblue"
    tkconfigure(DUEenv$tkWindow, bg = DUEenv$bgWindow)
    DUEenv$fontButtonX <- tkfont.create(family = "times", 
                                        size = 12, weight = "bold")
    DUEenv$fontButtonNotX <- tkfont.create(family = "times", 
                                           size = 12, weight = "normal")
    DUEenv$fontForLabels <- tkfont.create(family = "times", 
                                          size = 12, weight = "bold")
    DUEenv$fontX <- tkfont.create(family = "times", 
                                  size = 20, weight = "bold")
    DUEenv$settingSliderValues <- TRUE
    DUEenv$img.contour <- tkrplot(DUEenv$tkWindow, 
                                  fun=function() plotThresholdContour(), hscale = DUEenv$DUEconfig["img.contour", 
                                                                                                   "hscale"], vscale = DUEenv$DUEconfig["img.contour", "vscale"])
    tkplace(DUEenv$img.contour, "-x", DUEenv$DUEconfig["img.contour", 
                                                       "x"], "-y", DUEenv$DUEconfig["img.contour", "y"])
    tkbind(DUEenv$img.contour, "<Button-1>", function(x, y) img.contour.f(x, y))
    DUEenv$printEntryBoxNames <- TRUE
    DUEenv$label.thresholds <- tklabel(DUEenv$tkWindow, 
                                       text = "Population thresholds", font = DUEenv$fontForLabels, 
                                       bg = "darkblue", fg = "white")
    tkplace(DUEenv$label.thresholds, "-x", DUEenv$DUEconfig["label.thresholds", 
                                                            "x"], "-y", DUEenv$DUEconfig["label.thresholds", 
                                                                                         "y"], "-height", DUEenv$DUEconfig["label.thresholds", 
                                                                                                                           "height"], "-width", tkwinfo("width", DUEenv$img.contour))
    DUEpopulationControls()
    DUEenv$settingSliderValues <- TRUE
    DUEenv$img.prob <- tkrplot(DUEenv$tkWindow, 
                               fun=function() plotProbsAndEU(), hscale = DUEenv$DUEconfig["img.prob", 
                                                                                          "hscale"], vscale = DUEenv$DUEconfig["img.prob", "vscale"])
    tkplace(DUEenv$img.prob, "-x", DUEenv$DUEconfig["img.prob", 
                                                    "x"], "-y", DUEenv$DUEconfig["img.prob", "y"])
    DUEenv$label.utilitychoice <- tklabel(DUEenv$tkWindow, 
                                          text = "X", fg = "red", font = DUEenv$fontForLabels)
    DUEutilityControls()
    setupProbLines()
    DUEenv$label.utilityTitle <- tklabel(DUEenv$tkWindow, 
                                         text = "Utility functions", font = DUEenv$fontForLabels, 
                                         bg = "darkblue", fg = "white")
    DUEenv$pressedAUtilityButton <- FALSE
    loadDUEconfig(2:dim(DUEenv$DUEconfig)[1])
    loadDUEconfig(2:dim(DUEenv$DUEconfig)[1], "text")
    loadDUEconfig(2:3, words("vscale hscale"))
    DUEenv$settingSliderValues <- FALSE
  }
