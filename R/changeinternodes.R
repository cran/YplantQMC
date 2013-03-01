
changeinternodes <- function(pfile=NA,
						     outputfile = "tmp.p",
							 method=c("perc","constant"),   # 'perc' or 'constant'
							 changeperc=50,
							 consvalue = NA
							 ){

	if(is.na(pfile)){
		if(.Platform$OS.type != "windows" || !interactive())
			stop("Please provide a plant (.P) file")
		pfile <- file.choose()
	}						 
							 
	method <- match.arg(method)
							  
	prestuff <- readLines(pfile, warn=FALSE)[1:5]
	pdata <- readp(pfile)

	# Internode lengths are both branch and stem lengths:
	branchlens <- pdata$L.1[pdata$L.1 > 0]
	stemlens <- pdata$L[pdata$L > 0.1]  # 0.1 is used for dummy stem segments.

	if(method == "perc"){
		branchlens <- branchlens * changeperc/100
		stemlens <- stemlens * changeperc/100
	}
	if(method == "constant"){
		branchlens <- consvalue
		stemlens <- consvalue
	}
	
	# replace.
	pdata$L.1[pdata$L.1 > 0] <- round(branchlens,4)
	pdata$L[pdata$L > 0.1] <- round(stemlens,4)
	
	# Write the data.
	writeLines(prestuff, outputfile)
	
	write.table(pdata, outputfile, append=TRUE, sep=" ", row.names=FALSE, col.names=FALSE)

}
