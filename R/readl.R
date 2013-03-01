readl <- function(lfile=NA){

  options(warn=-1)

  if(is.na(lfile)){
    if(.Platform$OS.type != "windows" || !interactive())
			stop("Please provide a leaf (.L or .LF) file")
		lfile <- file.choose()
	}
	
	r <- readLines(lfile, warn=FALSE)
	r <- r[r != ""]

	# Find out how many leaf types in this leaffile.
	leafloc <- grep("leaf",r,ignore.case=TRUE)
	nleaftypes <- length(leafloc)
	
	npoints <- as.numeric(r[leafloc+1])
	
	leaves <- list()
	for(i in 1:nleaftypes){
		
    # old version
# 		dfr <- try(read.table(lfile, skip=leafloc[i]+1, nrows=npoints[i]), silent=TRUE)
    # More robust (when there are blank lines in the L file.)
    dfr <- try(read.table(text=paste(r[(leafloc[i]+2):(leafloc[i]+npoints[i]+1)], 
                                     collapse="\n")), silent=TRUE)
    
    # If reading failed, delete last row, try again (until it does work).
    # This takes care of some files that don't have as many points as expected based on the first line in the L file.
		if(inherits(dfr, "try-error") && grepl("did not have 2",dfr)){
			k <- 1
			while(inherits(dfr, "try-error")){
				dfr <- try(read.table(lfile, skip=leafloc[i]+1, nrows=npoints[i]-k), silent=TRUE)
				k <- k + 1
			}
			warning("Number of points in",lfile,"does not match - read in first",nrow(dfr),"points.")
		}
		
		l <- list()
		if(!sum(dfr[nrow(dfr),] == 0))dfr <- rbind(dfr, c(0,0))
		
    l$XYZ <- matrix(cbind(dfr[,1],dfr[,2],rep(0,nrow(dfr))), ncol=3)
    
		colnames(l$XYZ) <- c("X","Y","Z")
		
		# Is there a point along the midrib?
		p1 <- 1	# First point always on midrib by convention (0,0).
		jj <- 2:(nrow(l$XYZ)-1)  # do not look in first and last points (generally 0,0 both)
		zerox <- l$XYZ[jj,1] == 0 & l$XYZ[jj,2] > 0
		hasmidrib <- any(zerox)
		if(!hasmidrib)stop("Leaf needs a point on the midrib where X=0")
		# Sometimes more than one x=0 point ('ossaea problem') : pick the one with max Y value.
		p2 <- which(zerox)[which.max(l$XYZ[zerox,2])] + 1 # Add one : we deleted first point!
		l$midribpoints <- c(p1,p2)
		
		if(grepl("\\.lf$",lfile))
			npars <- 9
		else
			npars <- 6
		
		parbegin <- leafloc[i] + 2 + npoints[i]
		parend <- parbegin + npars - 1
		
		l$leafpars <- as.numeric(r[parbegin:parend])
		# Name parameters if light response curve (not actually used as of July 2012).
		if(length(l$leafpars) == 6)
			names(l$leafpars) <- c("Amax","Rd","QY","shape","absorp","reflec")
		
		# If missing leaf pars, put NAs at the end (typical?)
		LP <- na.omit(l$leafpars)
		if(length(LP) < length(l$leafpars))
			l$leafpars <- c(LP, rep(NA, length(l$leafpars) - length(LP)))
		
		l$leaftype <- i #as.numeric(trim(gsub("leaf","",tolower(r[1]))))
		l$nleaftypes <- nleaftypes  #length(grep("leaf", readLines(lfile, warn=FALSE), ignore.case=TRUE))
			
		# Get leaf area factor
		leafpoly <- cbind(l$XYZ[,1],l$XYZ[,2])
		leafarea <- areapoly(leafpoly)
		l$midriblen <- unname(abs(l$XYZ[l$midribpoints[2],2] - l$XYZ[l$midribpoints[1],2] ))
		l$leafshape <- leafarea / l$midriblen^2
		
		class(l) <- "leaffile"
		leaves[[i]] <- l
	}
	
	options(warn=0)
    class(leaves) <- "leaffile"
    return(leaves)
}