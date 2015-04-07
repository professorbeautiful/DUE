loadDUEconfig <-
function(objnames,
		fields=words("x y width height"),
		values=NULL) {
	if(!is.null(values) & length(fields)!=length(values))
		stop("Number of fields and values not equal.")
	if(missing(objnames))
		objnames = DUEenv$DUEconfig$names [menu(DUEenv$DUEconfig$names)]
	else if(is.numeric(objnames))
		objnames = DUEenv$DUEconfig$names[objnames]
	for(objname in objnames) {
		#print(DUEconfig[objname,]) 
		for(field in fields) { 
			if(is.null(values))
				value=DUEenv$DUEconfig[objname, field] 
			else {
				value = values[match(field,fields)]
				DUEenv$DUEconfig[objname, field] <- value
			}
			if(as.character(value) != "error"
								& !is.na(value)) {
				if(DUEenv$DUEconfig[objname, "type"] == "sliderWithEntryBox") {
					if(field == "text") 
						tkconfigure(get(objname, envir=DUEenv)$the.slider, "-label", 
							value)
					else sliderWithEntryBox.position(objname, field, value)
#					eval(eval(substitute(parse(text=paste("
#					sliderWithEntryBox.position(get(objname),", field,
#						"=", value, ")"	
#						 )))))
				}
				else if(field %in% c("vscale", "hscale")) {
					the.expression = 
						parse(text=paste(
						"DUEenv$",
						objname, "[[\"", field, "\"]] <- value",
						sep=""))
					print(the.expression)
					eval(the.expression)
					tkrreplot(get(objname, envir=DUEenv))
				}
				else if(field == "text") {
						tkconfigure(get(objname, envir=DUEenv), "-text", value)
				}
				else
					tkplace(get(objname, envir=DUEenv), 
						paste("-",field, sep=""), value)
			}
		}
	}
}

