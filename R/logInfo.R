logInfo <-
function(s, logfilename="Log", initialize=FALSE, addTimeStamp=TRUE) {
  ####  there are problems on linux, possibly with date formatting.
	if(substring(version$os, 1, 6) != "darwin")
		return(invisible(NULL))
  ####  CURRENTLY FOR USE ON MAC OSX
	funcname = as.character(sys.call(-1))[[1]]; 
	if(!exists("functionsToLog")) functionsToLog <<- character(0)
	if(!is.na(functionsToLog[funcname]))
		if( ! (functionsToLog[funcname]))
			return(invisible(NULL))
	if(initialize) {
		if(logfilename %in% ls(1)) {
			assign(paste(logfilename, "BACKUP", sep="."),
				get(logfilename, pos=1),
				pos=1)
			rm(list=logfilename, pos=1)
		}
	}
#	if(addTimeStamp) s = paste(timestamp(quiet=TRUE), s)
	newRow = data.frame(F=funcname, s=s, t=Sys.time())
	if(logfilename=="")
		print(newRow)
	else if(logfilename %in% ls(1)) 
		assign(logfilename, pos=1,
			rbind(get(logfilename), newRow))
	else assign(logfilename, newRow, 1)
	invisible(NULL)
}

