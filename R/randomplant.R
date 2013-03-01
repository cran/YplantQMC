
randomplant <- function(nleaves=500, radius=250, height=500,
						shape=c("BOX","CONE","HELIP","PARA","ELIP","CYL"),
						crownbase=0,
						LAD=angledist("ellipsoid",1),
						leaflen=30,
						LA=NULL, lfile=NULL,
						writefile=FALSE, quiet=FALSE
						){
						
					
	shape <- match.arg(shape)

	# Get leaf shape from lfile, otherwise it will use triangleleaf (shape=0.5).
	if(!is.null(lfile)){
	  l <- readl(lfile)
	  if(length(l)>1 && !quiet)
	    message("Using first leaf type in leaf file only.")
	  l <- l[[1]]
	  leafshape <- l$leafshape
	} else {
	  leafshape <- 0.5
	}
  
	if(!is.null(LA)){
			
		laleaf <- leaflen^2 * leafshape

		nleaves <- 10^6 * LA / laleaf
		if(!quiet)message("Aiming for leaf area = ", signif(LA,4), " m2")
	}
	

	w <- radius
	
	# Make better guess of number of leaves:
	# (crown volume relative to box).
	boxvol <- height * (radius*2)^2
	crownvol <- switch(shape,
			BOX = boxvol,
			CONE = 1/3 * pi * height * radius^2,   
			HELIP = 2/3 * pi * height * radius^2,
			PARA = 1/2 * pi * height * radius^2,
			ELIP = 4/3 * pi * (height/2) * radius^2,
			CYL = pi * radius^2 * height)
	relcrownvol <- crownvol / boxvol
	
	n <- floor(nleaves / relcrownvol)  # approximate!
	
	xyz <- data.frame(X=runif(n, -w,w), 
                  Y = runif(n, -w,w),
				  Z = crownbase + runif(n, 0,height),
				  ang = drawsample(LAD,n,degrees=TRUE),
				  AZ = runif(n,0,360),
				  OR = runif(n,0,360),
				  len = leaflen)

	if(shape != "BOX"){

		# SURFACE from Maestra:
		crownrh <- function(H,JSHAPE=c("CONE","HELIP","PARA","ELIP","CYL")){
			JSHAPE <- match.arg(JSHAPE)
			
			relr <- switch(JSHAPE,
				CONE = 1-H,
				HELIP = sqrt(1-H^2),
				PARA = sqrt(1-H),
				ELIP = sqrt(1 - ((H-1/2)^2)/((1/2)^2)),
				CYL = 1)
			return(relr)
		}
		
		# Select only leaves that fall within the shaped hull.
		maxh <- max(xyz$Z)
		relr <- sqrt(xyz$X^2 + xyz$Y^2) / w
		crownr <- crownrh(xyz$Z / maxh, shape)
		xyz <- xyz[relr < crownr,]
		
	}

	#
	LAactual <- 10^-6 * sum(leafshape * xyz$len^2)
	if(!quiet){
		message("Actual leaf area = ", signif(LAactual,4), " m2")
		message("Number of leaves = ", nrow(xyz))
	}		  
	
	# Q file format (see constructplant()).	
	qfile <- xyz

	if(!writefile)
		return(qfile)
	else {
		filen <- paste(shape,"_",n,".Q")
		write.table(qfile, filen, row.names=FALSE, col.names=TRUE)
		return(invisible(qfile))
	}

}
