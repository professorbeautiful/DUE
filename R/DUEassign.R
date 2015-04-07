DUEassign <-
function(objname, value) {
   subx <- substitute(objname)
   if (is.name(subx)) 
       subx <- deparse(subx)
   if (!is.character(subx) || length(subx) != 1)
   	stop("objname should be character") 
   assign(subx, value, envir=DUEenv)
}

