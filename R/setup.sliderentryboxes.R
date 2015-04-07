setup.sliderentryboxes <-
function(slider.var) {
#NOTES  
#  The "sliders" are tk scale objects.
#  There is no 	 '-command' tkconfigure option for entry boxes
#	Therefore we bind to <RETURN> inside respondToEntryBox.

	verbose = FALSE

### This sets up the entrybox to send info to the slider.
###  load.value.into.slider worries about whether to log the value.
	the.text = 
		 replace.in.string(s1="slider.var", s2=slider.var,
		"
			DUEenv$entrybox.slider.var <- tkentry(DUEenv$tkWindow)
			respondToEntryBox(DUEenv$entrybox.slider.var, 
				function(tryval){
					DUEenv$settingSliderValues <- TRUE
					load.value.into.slider('slider.var', tryval); 
					function(...)slider.slider.var.f()
					DUEenv$settingSliderValues <- FALSE
				}
			);
		"
	)
	if(verbose) cat(the.text)
	
	eval(eval(substitute(parse(text=the.text)
		)))
		
######  This puts the slider and the entry into one structure.
	the.text = 
		 replace.in.string(s1="slider.var", s2=slider.var,
		"
			DUEenv$sliderWithEntryBox.slider.var <- sliderWithEntryBox(
					the.slider=DUEenv$slider.slider.var, the.entry=DUEenv$entrybox.slider.var);		"
	)
	if(verbose) cat(the.text)
	eval(eval(substitute(parse(text=the.text)
		)))
		
#######  This sets up the slider to send info to the entry box.
	the.text = 
		 replace.in.string(s1="slider.var", s2=slider.var,
		"
			tkconfigure(DUEenv$slider.slider.var, command=function(...){
					tkSetText(DUEenv$entrybox.slider.var, tkget(DUEenv$slider.slider.var), 
							DUEenv$DUEconfig['sliderWithEntryBox.slider.var', 'log']); 
					slider.slider.var.f()
			});
		" 
	)
					#cat('will call slider.slider.var.f\n')
					#cat('finished call slider.slider.var.f\n')
	if(verbose) cat(the.text)
	eval(eval(substitute(parse(text=the.text)
		)))
		
####### This ?   Apparently puts initial info into entrybox.
	the.text = 
		 replace.in.string(s1="slider.var", s2=slider.var,
		"
			tkSetText(DUEenv$entrybox.slider.var, tkget(DUEenv$slider.slider.var), 
							DUEenv$DUEconfig['sliderWithEntryBox.slider.var', 'log']); 
		"
	)
	if(verbose) cat(the.text)
	eval(eval(substitute(parse(text=the.text)
		)))
		
######  This configures the slider on the screen.
	the.text = 
		 replace.in.string(s1="slider.var", s2=slider.var,
		"
			loadDUEconfig('sliderWithEntryBox.slider.var');
		"
	)
	if(verbose) cat(the.text)
	eval(eval(substitute(parse(text=the.text)
		)))
		
######  This sets the slider label font.
	the.text = 
		 replace.in.string(s1="slider.var", s2=slider.var,
		"
			tkconfigure(DUEenv$slider.slider.var, font=DUEenv$fontForLabels);
		"
	)
	if(verbose) cat(the.text)
	eval(eval(substitute(parse(text=the.text)
		)))
}

