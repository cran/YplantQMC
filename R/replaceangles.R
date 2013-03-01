replaceangles <- function(whichangle="An.3",    # Or An.2 (petiole), Or (midrib orientation), Az (azimuth of steepest angle)
						  pfile=NA,
						  outputfile = NA,
					      newangles = NULL,
						  distobj = NULL
							  ){

	if(is.na(pfile)){
		if(.Platform$OS.type != "windows" || !interactive())
			stop("Please provide a plant (.P) file")
		pfile <- file.choose()
	}						  
	
	if(is.na(outputfile)){
		filer <- gsub("\\.P$","",pfile,ignore.case=TRUE)
		outputfile <- paste0(filer, "-Modified.p")
	}
	
	prestuff <- readLines(pfile, warn=FALSE)[1:5]

	# Read angles
	pdata <- readp(pfile)
	angles <- getangles(pfile, whichangle)
	N <- length(angles)
	
	# Sample from distribution object, or use provided angles.
	if(!is.null(distobj)){
		newangles <- drawsample(distobj, N, degrees=TRUE)
	}
	if(!is.null(newangles)){
		if(length(newangles) < N){
			if(length(newangles)==1)newangles <- rep(newangles, N)
			if(length(newangles)>1)newangles <- sample(newangles, N, replace=TRUE)
		} else
			newangles <- newangles[1:N]
	}
	if(is.null(newangles)&is.null(distobj))stop("No new angles specified\n")
	
	# Replace angles with new angles.
	pdata[pdata$Lt >= 1, whichangle] <- round(newangles,2)
	
	# Write the data.
	writeLines(prestuff, outputfile)
	
	write.table(pdata, outputfile, append=TRUE, sep=" ", row.names=FALSE, col.names=FALSE)

}
