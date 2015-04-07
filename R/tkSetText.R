tkSetText <-
function(the.entry, value, log){
	#browser()
	tkdelete(the.entry, "0", "10000")
	if(log=="yes") value=format(10^(as.numeric(value)), digits=1)
	tkinsert(the.entry, "0", value)
}

