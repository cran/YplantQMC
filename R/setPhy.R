

# Generic function for setting up a leaf physiology object.
setPhy <- function(leafmodel, leafpars=list()){

	l <- list()
		
	# Store leafmodel
	l$leaffunction <- get(leafmodel)  # function
	l$leafmodel <- leafmodel          # name of function
	
	# Store leaf parameters.
	l$leafpars <- leafpars
	
	class(l) <- "ypphy"

return(l)
}


