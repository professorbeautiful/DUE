attach.entrybox.to.label <-
function(the.label, the.entrybox) {
	tkplace(the.entrybox, 
		"-x", add(get.tkplaceinfo.x(the.label), get.tkplaceinfo.width(the.label)),
		"-y", get.tkplaceinfo.y(the.label), "-height", 20, "-width", 35)
}

