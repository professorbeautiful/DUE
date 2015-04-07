FromLognormalToNormal <-
function(uStar, vStar) {
	### Converts from the hidden parameters to the visible ones.  Typical arguments are:
	# the.means.pop
	# the.variances.pop 
	c(	mean=exp(uStar + vStar/2), 
		cv=sqrt(exp(vStar) - 1))
}

