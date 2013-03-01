
setHemi <- function(canfile, canopytrans=0.02){
	
	
	if(!is.matrix(canfile)){
		
		extension <- tolower(substr(canfile, nchar(canfile)-2, nchar(canfile)))
		if(extension == "can")
			hemimat <- readCAN(canfile)
		if(extension == "csv"){	
			hemimat <- as.matrix(read.csv(canfile))	
			if(ncol(hemimat) == 1)
				hemimat <- as.matrix(read.table(canfile, header=TRUE))
			if(ncol(hemimat) == 1)
				stop("Error reading 'canfile' - save as comma or space delimited text file.")
			colnames(hemimat) <- NULL
			zenith <- tolower(hemimat[1,1]) == "zenith" # TRUE if zenith given.
			if(!zenith && tolower(hemimat[1,1]) != "altitude")
				stop("Row 1, Column 1 must be 'zenith' or 'altitude'.")
			hemimat <- hemimat[-1,-1]
			storage.mode(hemimat) <- "numeric"
			if(zenith)hemimat <- hemimat[nrow(hemimat):1,]
		}
		if(!extension %in% c("csv","can"))
			stop("Only .CSV or .CAN are currently supported by setHemi")
		
	} else {
		# canfile is already a matrix.
		hemimat <- canfile 
	}	

	# As in Yplant, set the minimum transmission to a parameter,
	# the 'Canopy Transmission Coefficient'.
	hemimat <- pmax(hemimat, canopytrans)
	
	# Number of bins in altitude and azimuth directions.
	nalt <- nrow(hemimat)
	naz <- ncol(hemimat)
	
	# Assume in increasing order; starting at zero (altitude and azimuth);
	# equally spaced bins (angle-wise). Values in 1st col and row NOT read,
	# but should be there anyway (can be empty).
	azbins <- seq(0,2*pi,length=naz+1)
	altbins <- seq(0,pi/2,length=nalt+1)

	hemi <- list()
	hemi$m <- hemimat
	hemi$nalt <- nalt
	hemi$naz <- naz
	hemi$azbins <- azbins
	hemi$altbins <- altbins

	# Set weights for diffuse calculations (i.e., the size of the hemi-tile).
	# Tiles are larger for lower solar altitude.
	w <- c()
	for(i in 1:hemi$nalt)w[i] <- sin(hemi$altbins[i+1]) - sin(hemi$altbins[i])
	weight <- matrix(rep(w,naz),ncol=naz)
	weight <- weight / naz
	
	
	if(!is.matrix(canfile))
		hemi$canfile <- canfile
	else
		hemi$canfile <- NA
	
	class(hemi) <- c("yphemi")
	
return(hemi)
}

