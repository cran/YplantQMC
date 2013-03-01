
modifypfile <- function(  pfile=NA,
					      whichvar=NA,
						  outputfile = "tmp.p",
					      newvalues = NULL
						){

						
	if(is.na(pfile)){
		if(.Platform$OS.type != "windows" || !interactive())
			stop("Please provide a plant (.P) file")
		pfile <- file.choose()
	}					
						
	if(is.na(whichvar))stop("Need a variable name (set 'whichvar' argument).")

	prestuff <- readLines(pfile, warn=FALSE)[1:5]

	# Read old values
	pdata <- readp(pfile)
	oldvals <- pdata[,whichvar]
	N <- length(oldvals)
	
	# Sample from distribution object, or use provided angles.
	if(!is.null(newvalues)){
		if(length(newvalues) < N)
			if(length(newvalues)==1)newvalues <- rep(newvalues, N)
			if(length(newvalues)>1)newvalues <- sample(newvalues, N, replace=TRUE)
		else
			newvalues <- newvalues[1:N]
	}
	
	# Replace values.
	pdata[, whichvar] <- newvalues
	
	# Write the data.
	writeLines(prestuff, outputfile)
	
	write.table(pdata, outputfile, append=TRUE, sep=" ", row.names=FALSE, col.names=FALSE)

}
