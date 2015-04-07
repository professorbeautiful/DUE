replace.in.strings <-
function(s, s1, s2, all=TRUE, extended.=FALSE, perl.=FALSE) {
	### fix a bug!! does not replace if the last char in string.
		sapply(s, function(s,s1,s2) 
			paste(strsplit(s, s1,, extended=extended., perl=perl.)[[1]], sep="", collapse=s2),
			 s1=s1, s2=s2)
}

