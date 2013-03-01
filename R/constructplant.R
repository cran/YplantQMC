constructplant <- function(pfile=NULL,  lfile=NULL,  qfile=NULL, multiplier=1.0,
					X0=0, Y0=0, Z0=0, warn=FALSE, quiet=FALSE){
			
	inputformat <- "P"

	# Decide whether a Q format is given:
	isQformat <- function(x){
		if(is.character(x) && grepl("\\.Q$", x))
			return(TRUE)
		
		if(is.data.frame(x) && all(sort(tolower(names(x))) == c("ang","az","len","or","x","y","z")))
			return(TRUE)
		
		# else
		return(FALSE)
	}
	
	if(!is.null(pfile) && isQformat(pfile))inputformat <- "Q"
	if(is.null(pfile) & !is.null(qfile))inputformat <- "Q"
	
	# Choose file if none provided.
	if(is.null(pfile) && is.null(qfile)){
		if(.Platform$OS.type != "windows" || !interactive())
				stop("Please specify a plant file.")
		pdata <- readp(choose.files(caption="Select .P file")[1])
	}

	# Read data if file name provided:
	if(is.character(pfile) & inputformat=="P")pdata <- readp(pfile)
	
	if(inputformat == "Q" & !is.null(pfile))qfile <- pfile
	
	# Else, use qdata format.
	if(!is.null(qfile) || inputformat=="Q"){
		# If not character, 'qfile' is already a dataset.
		if(is.character(qfile))
			qdata <- read.table(qfile, header=TRUE)
		else
			if(is.data.frame(qfile) && ncol(qfile) == 7)qdata <- qfile
			
		names(qdata) <- c("X","Y","Z","An.3","Az.3","Or","L.3")
		
		# For now, only one leaf type allowed...
		qdata$Lt <- 1
		inputformat <- "Q"
		#stems <- FALSE
	}
	
	# If leaf length is zero, make sure leaf type (Lt) = 0.
	if(inputformat == "P")pdata$Lt[pdata$L.3 == 0] <- 0
	
	# Convert units (if needed).
	if(multiplier != 1.0 && inputformat == "P"){
			pdata$L <- multiplier * pdata$L
			pdata$L.1 <- multiplier * pdata$L.1
			pdata$L.2 <- multiplier * pdata$L.2
			pdata$L.3 <- multiplier * pdata$L.3
			pdata$D <- multiplier * pdata$D
			pdata$D.1 <- multiplier * pdata$D.1
			pdata$D.2 <- multiplier * pdata$D.2
	}
	
	# Any L == 0? Cannot be - set to 0.1.
	if(inputformat == "P")pdata$L[pdata$L == 0] <- 0.1
	
	# If file name given , read it:
	if(is.character(lfile))ldata <- readl(lfile)
	
	# If a leaf object is given, use that.
	if(inherits(lfile, "leaffile"))ldata <- lfile
	
	# Leaf data
	if(is.null(lfile)){
			if(.Platform$OS.type != "windows" || !interactive())
				stop("Please specify a leaf file.")

			# .... needs to be updated again (readl has changed).
		  if(!quiet)message("Using built-in triangle leaf.")
			ldata <- structure(list(structure(list(XYZ = structure(c(0, 5, 0, -5, 
    0, 0, 10, 0, 0, 0, 0, 0), .Dim = c(4L, 3L), .Dimnames = list(
        NULL, c("X", "Y", "Z"))), midribpoints = c(1, 3), leafpars = structure(c(-999, 
    -999, -999, -999, -999, NA), .Names = c("Amax", "Rd", "QY", "shape", 
    "absorp", "")), leaftype = 1L, nleaftypes = 1L, midriblen = 10, 
        leafshape = 0.5), .Names = c("XYZ", "midribpoints", "leafpars", 
    "leaftype", "nleaftypes", "midriblen", "leafshape"), class = "leaffile")), class = "leaffile")
			lfile <- "triangle"
			
      # Old option to choose file from menu (not platform-independent!)
#       lfile <- choose.files(caption="Select .L or .LF file")
# 			if(length(lfile) > 0)
# 				ldata <- readl(lfile[1])
# 			else
# 				ldata <- NA
	}
	
	# If still no leaf read, stop here:
	if(is.null(lfile)){
		stop("Need a leaf file to continue.\n")
	}
	
	k <- pi/180
	
	getxyz <- function(azim = 45, angle=30, len=3, origin=list(x=0,y=0,z=0)){
		
			azim <- azim * k
			angle <- angle * k

			z <- origin$z + len*sin(angle)  # z coordinates of line starting and endpoint
			d <- len*cos(angle)				# distance of line from origin, as seen from above.
			x <- origin$x + d*sin(azim)     # x coordinates
			y <- origin$y + d*cos(azim)     # y coordinates

		return(list(x=x,y=y,z=z))
	}
	
	if(inputformat == "P"){
	
	endcoordinates_stem <- vector("list", nrow(pdata))
	endcoordinates_branch <- vector("list", nrow(pdata))
	Petioles <- vector("list", nrow(pdata))
	Stems <- vector("list", nrow(pdata))
	Branches <- vector("list", nrow(pdata))
	
	for(i in 1:nrow(pdata)){

		# Note the +1 : in the p format, 0 is the first node.
		node <- pdata$N[i] + 1
		mothernode <- pdata$MN[i] + 1
		thismothernode <- which(pdata$N == mothernode-1) # actual storage nr. in list
		
		# If mothernode does not exist, find the closest one in rank that does exist:
		if(length(thismothernode) == 0){
			thismothernode <- which.min(abs(pdata$N - mothernode))
		}
		
		if(length(thismothernode) > 1){
			if(warn)warning("Duplicated node number. First one used.\n")
			thismothernode <- thismothernode[1]
		}
		
		ste <- pdata$ste[i]
		# cat(i, "\n")
		# if(i == 25)browser()
		# Find coordinates of starting point for current segment.
		if(i == 1){
			origin <- list(x=X0,y=Y0,z=Z0)   # Use zero for first segment.
		} else {
			if(ste==1)origin <- endcoordinates_stem[[thismothernode]]
			if(ste==2)origin <- endcoordinates_branch[[thismothernode]]
			
			# For robustness: if a node is missing from the file, connect segment to
			# previous node. 
			if(is.null(origin)){
				if(ste==1)origin <- endcoordinates_stem[[thismothernode-1]]
				if(ste==2)origin <- endcoordinates_branch[[thismothernode-1]]
				if(warn)warning("Origin NULL at node", node, "- current node connected to previous node")
			}	
		}
		
		# Get end coordinates of a stem segment ('origin' is the start coordinate)
		endcoordinates_stem[[i]] <- getxyz(azim = pdata$Az[i], angle=pdata$An[i],
			len=pdata$L[i], origin)
		Stems[[i]] <- list()
		Stems[[i]]$xyz <- list(from=unname(unlist(origin)), 
			to=unname(unlist(endcoordinates_stem[[i]])))
		Stems[[i]]$mothernode <- mothernode
		Stems[[i]]$node <- node
		Stems[[i]]$diam <- pdata$D[i]
		
		# Branch segment.
		endcoordinates_branch[[i]] <- getxyz(azim = pdata$Az.1[i], angle=pdata$An.1[i], 
			len=pdata$L.1[i], origin)
		Branches[[i]] <- list()
		Branches[[i]]$xyz <- list(from=unname(unlist(origin)), 
			to=unname(unlist(endcoordinates_branch[[i]])))
		Branches[[i]]$mothernode <- mothernode
		Branches[[i]]$node <- node
		Branches[[i]]$diam <- pdata$D.1[i]
		
		# Get petiole coordinates, if leaf length of leaf greater than zero.
		# Note that angles of petiole and leaf are multiplied by -1, by definition.
		if(pdata$L.3[i] > 0){
		petiole_endpoint <- getxyz(azim = pdata$Az.2[i], angle=pdata$An.2[i],
			len=pdata$L.2[i], origin)
		Petioles[[i]] <- list()
		Petioles[[i]]$xyz <- list(from=unname(unlist(origin)), 
			to=unname(unlist(petiole_endpoint)))
		Petioles[[i]]$mothernode <- mothernode
		Petioles[[i]]$node <- node
		}
	}
	}
	
	# Normalize ldata, so that 'length' (length of the midrib!) = 1
	# ldata$XYZ <- ldata$XYZ / max(ldata$XYZ[,"Y"])
	ldata_scaled <- ldata
	for(i in 1:length(ldata)){
		ldata_scaled[[i]]$XYZ <- ldata[[i]]$XYZ / ldata[[i]]$midriblen
	}
	
	# This list will store xyz coordinates of all leaf edges.
	newleaves <- list()

	# Data with only the leaf information.
	if(inputformat == "P"){
		whichleaves <- which(pdata$Lt >= 1)
		pdatL <- pdata[whichleaves,]
	} else {
		pdatL <- qdata
		whichleaves <- 1:nrow(pdatL)
	}
	
	OR <- pdatL$Or
	AZ <- pdatL$Az.3
	AN <- pdatL$An.3	
	AZ[AZ < 0] <- AZ[AZ < 0] + 360
	AN[AN==90] <- 89.999
	LEN <- pdatL$L.3
	nleaves <- nrow(pdatL)
	
	# Get leaf base (=petiole end) and leaf tip coordinates	
	leafbasecoor <- list()
	leaftipcoor <- list()
	leafnodenumber <- list()
	ROLL <- INC <- c()
	
	if(nleaves > 0){
	if(inputformat == "Q"){
		
		# Shift the plant base.
		qdata$X <- qdata$X + X0
		qdata$Y <- qdata$Y + Y0
		qdata$Z <- qdata$Z + Z0
		
		for(i in 1:nrow(qdata)){
			leafbasecoor[[i]] <- c(qdata$X[i],qdata$Y[i],qdata$Z[i])
		}
	}
	
	# Save the coordinates of the leaf base, and the node numbers.
	if(inputformat == "P"){
		for(i in 1:nleaves){
			leafbasecoor[[i]] <- Petioles[[whichleaves[i]]]$xyz$to
			leafnodenumber[[i]] <- Petioles[[whichleaves[i]]]$node
		}
	}
	
	# Calculate leaf edge coordinates, and tip coordinate. 
	# (based on 'madeleafdirection' and 'leaf.init' in 'Dpunit.pas' in YPLANT.)
	mds <- list()
	for(i in 1:nleaves){

		mds[[i]] <- madeleafdirection(k*OR[i], k*AZ[i], k*AN[i])
		leaftipcoor[[i]] <- mds[[i]]$ld * LEN[i] + leafbasecoor[[i]]
	
		ld <- mds[[i]]$ld
		wd <- mds[[i]]$wd
		
		# find leaftype (Only supported for P files at the moment).
		if(inputformat == "P")
			leaft <- pdatL$Lt[i]
		else
			leaft <- 1
		
		ldatcur <- ldata_scaled[[leaft]]
		
		leafxy <- ldatcur$XYZ[,1:2]
		npoints <- nrow(leafxy)	
		X <- Y <- Z <- c()
	
		for(j in 1:npoints){
		
			tw <- leafxy[,"X"][j]
			tl <- leafxy[,"Y"][j]
			
			X[j] <- (tw*wd[1] + tl*ld[1]) * LEN[i] + leafbasecoor[[i]][1]
			Y[j] <- (tw*wd[2] + tl*ld[2]) * LEN[i] + leafbasecoor[[i]][2]
			Z[j] <- (tw*wd[3] + tl*ld[3]) * LEN[i] + leafbasecoor[[i]][3]

		}
		newleaves[[i]] <- ldatcur
		newleaves[[i]]$XYZ <- cbind(X,Y,Z)
		newleaves[[i]]$leafnodenumber <- ifelse(inputformat == "P", leafnodenumber[[i]], NA)
		newleaves[[i]]$leafnormal <- getleafnormal(newleaves[[i]])
		
	}
	} #END if(nleaves > 0)
	
	
	l <- list()
	l$leaves <- newleaves
	l$nleaves <- nleaves
	if(inputformat=="P")l$stems <- Stems else l$stems <- NA
	if(inputformat=="P")l$branches <- Branches else l$branches <- NA
	if(inputformat=="P")l$petioles <- Petioles else l$petioles <- NA
	if(inputformat=="P")l$pdata <- pdata else l$pdata <- NA
	l$ldata <- ldata
	l$inputformat <- inputformat
	if(is.character(pfile))
		l$pfile <- pfile
	else
		l$pfile <- paste0("Plant",format(Sys.time(), "%Y-%m-%d_%H-%M"),".txt")
		
	l$lfile <- lfile
	if(inputformat =="Q")l$qdata <- qdata else l$qdata <- NA
	if(inputformat == "P")l$qfile <- NA else l$qfile <- as.vector(na.omit(c(pfile,qfile)))
	l$leaftipcoor <- do.call("rbind",leaftipcoor)
	l$leafbasecoor <- do.call("rbind",leafbasecoor)
	l$leafdata <- leafdata(l)

	# Make QMC input files.
	m <- makeQMCinput(l, writefile=FALSE)
	l$qmcinput <- m$qmcinput
	l$qmcinputfile <- m$inputfile
	l$qmcoutputfile <- m$outputfile
	
class(l) <- "plant3d"
return(l)
}
