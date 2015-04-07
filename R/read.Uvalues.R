read.Uvalues <-
function() {
	utility.new = sapply( 
			paste("U.", c("rt", "rT", "Rt", "RT"), sep=""),
		function(uname) as.numeric(tclvalue( uname )))
	DUEenv$utility <- utility.new
#	cat("New Uvalues are ", DUEenv$utility, "\n")
}

