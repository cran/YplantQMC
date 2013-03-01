# Project all leaves on a plant onto the viewing plane.
projectplant <- function(plant, azimuth, altitude){

	if(class(plant) != "plant3d")stop("Need object of class 'plant3d'\n")

	# Function to get min and max x,y,z values of the transformed plant.
	# This is to place the grid correctly.
	getviewbound <- function(leaflist){
		maxx <- max(sapply(leaflist, function(x)max(x$XYZ[,1])))
		minx <- min(sapply(leaflist, function(x)min(x$XYZ[,1])))
		maxy <- max(sapply(leaflist, function(x)max(x$XYZ[,2])))
		miny <- min(sapply(leaflist, function(x)min(x$XYZ[,2])))
		maxz <- max(sapply(leaflist, function(x)max(x$XYZ[,3])))
		minz <- min(sapply(leaflist, function(x)min(x$XYZ[,3])))
	return(list(minx=minx,maxx=maxx,miny=miny,maxy=maxy,minz=minz,maxz=maxz))
	}

	oldleaves <- plant$leaves
	N <- length(oldleaves)
	p <- makeviewplane(azimuth,altitude)

	# nleafpoints <- nrow(oldleaves[[1]]$XYZ)
	nleafpoints <- sapply(plant$leaves, function(x)nrow(x$XYZ))
	
	xyzs <- do.call("rbind",lapply(oldleaves, function(x)x$XYZ))

	# Get coordinates in viewing plane, using cross-products between current
	# coordinates and the vectors that represent the new system.
	
	#- vector that represents the beam (i.e. z axis in the viewing plane).
	nrm <- p$z 
	
	#- Convert coordinates for all leaf edges.
	vx <- as.vector(xyzs %*% p$x)
	vy <- as.vector(xyzs %*% p$y)
	vv <- as.vector(xyzs %*% nrm)
	
	#- Find acos angle between viewing direction (solar ray) and leaf normal.
	acosangles <- c()
	for(i in 1:N){
		leafn <- plant$leaves[[i]]$leafnormal
		acosangles[i] <- acosangle(leafn, nrm)
	}
	
	#- newleaves : list of matrices with view plane coordinates of leaf edges.
	newxyzs <- cbind(vx,vy,vv)
	colnames(newxyzs) <- c("VX","VY","VZ")
	newleaves <- vector("list",N)
	
	fromi <- cumsum(c(1,nleafpoints[-length(nleafpoints)]))
	toi <- cumsum(nleafpoints)
	
	for(i in 1:N){                           
		newleaves[[i]]$XYZ <- newxyzs[fromi[i]:toi[i],]
		#newxyzs[(1+(i-1)*nleafpoints[i]):(i*nleafpoints[i]) ,]
		newleaves[[i]]$midribpoints <- plant$leaves[[i]]$midribpoints
	}

	l <- list()
	l$leaves <- newleaves
	l$acosangle <- acosangles
	l$viewbound <- getviewbound(newleaves)
	l$viewangle <- list(azimuth=azimuth, altitude=altitude)
	class(l) <- "projectedplant3d"
	
return(l)
}
