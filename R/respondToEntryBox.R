respondToEntryBox <-
function(the.box, fun, oldvalexpression) {
	### currently not using oldvalExpression,
	### which would be used to revert to old value
	###  if the callback returns an error.
		### fun processes the new value, makes sure it is aceptable.
		### oldvalexpression
	my.entrybox = the.box
	tkbind(the.box, "<Return>", 
		function(...) {
			#cat("my.entrybox = ", as.character(my.entrybox)[1],  "\n")
			#cat("the.box = ", as.character(the.box)[1],  "\n")
			contents =  read.value.from.entrybox(the.box)
			tryval = try( numval <- as.numeric(contents))
			if(DUEenv$printEntryBoxNames) {
				cat("respondToEntryBox: responding to ", entryBoxCatalog(the.box), 
				"  contents = ", contents, "  tryval = ", tryval, "\n")
			}
#			if (class(tryval) != "try-error") {
#				cat("SUCCESS, numval is ", numval, "\n")
#			}
			return(fun(tryval))
		}
	)
}

