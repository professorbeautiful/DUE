entryBoxCatalog <-
function(index){
	index = as.character(index)
	
	catalog = sapply(ls(pattern="^entrybox.*[^f]$", pos=1), function(n)try(get(n)[[1]]))	
	result = try(
		names(catalog)[which(catalog %in% index)] )
	if(class(result) ==  "try-error"){
		cat("entryBoxCatalog: error: catalog = \n\t",
			catalog,
			"\n  index = ", index, "\n")
	}
	return(result)
}

