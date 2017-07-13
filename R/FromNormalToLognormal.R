FromNormalToLognormal <-
function(median, cv) {
	### Converts from the visible user parameters to the log-scale ones.
	vStar = log(cv^2 + 1)
	c(uStar=log(median),
		vStar=vStar)
}

