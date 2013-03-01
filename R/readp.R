readp <- function(pfile=NA){

	if(is.na(pfile)){
		if(.Platform$OS.type != "windows" || !interactive())
			stop("Please provide a plant (.P) file")
		pfile <- file.choose()
	}

    r <- tolower(readLines(pfile, warn=FALSE))
    r <- gsub("[[:space:]]", "", r)
	
	# Find labels (N MN STE ...)
	headerline <- grep("nmnste",r)
	
	# If still no success, find the line that starts with 0 and 0
	if(any(substr(r, 1,2) == "00")){
		headerline <- which(substr(r, 1,2) == "00")[1] - 1
	}
	
	# If not found, find previous line that says "Petiole", amongst other things
	if(length(headerline) == 0){
		gr <- grep("petiole", r)
		if(length(gr) > 0)headerline <- gr[length(gr)] + 1
	}
	
	# If still no success, assume header is in 5th line (kind of defaultish location)
	if(length(headerline)==0)headerline <- 5
	
	pdata <- read.table(pfile, skip=headerline)
	
	# If not read correctly, try this;
	if(ncol(pdata) != 20)
		 pdata <- read.table(pfile, skip=headerline-1, fill=TRUE,header=TRUE)
	
	if(ncol(pdata) != 20)stop("Need 20 columns exactly.\n")
	
	pdata[is.na(pdata)] <- 0
	
	names(pdata) <- c("N", "MN", "ste", "Lt", "Az", "An", "L", "D", "Az.1", "An.1", "L.1",
                      "D.1", "Az.2", "An.2", "L.2", "D.2", "Or", "Az.3", "An.3", "L.3")

pdata
}