replace.in.string <-
function(s, s1, s2, all=TRUE) 
	if(all==TRUE) 
		paste(strsplit(s, s1)[[1]], sep="", collapse=s2)

