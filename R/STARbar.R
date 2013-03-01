	STARbar <- function(object,...){

	UseMethod("STARbar")
}


STARbar.plant3d <- function(object, method=c("gridtracer","exact","QuasiMC","slowquasimc"),
	integration=c("Turtlesky","Yplant","Turtle244"), progressbar=TRUE, returnldr=FALSE, quiet=FALSE,
	npside=50, silhouette=TRUE, azimuth=NA, altitude=NA, ...
	){

	plant <- object
	method <- match.arg(method)
	integration <- match.arg(integration)

        # Windows only. # MC 4/12/2012 - updated to include Mac OS X
	if(method %in% c("QuasiMC","slowquasimc") && .Platform$OS.type != "windows" && (Sys.info()[['sysname']] != "Darwin"))
		stop("Select different method: QuasiMC is currently only available in Windows and Mac OS X.")
	
	if(quiet)progressbar <- FALSE
	
	# Get viewing angles to calculate DA and PA from.
	if(all(is.na(altitude)) | all(is.na(azimuth))){
		if(integration == "Yplant"){
			altaz <- yplantaltaz
		}
		if(integration == "Turtlesky"){
			altaz <- turtle
		}
		if(integration == "Turtle244"){
			altaz <- turtle244
		}
		AZ <- altaz$azimuth
		ALT <- altaz$altitude
		nangles <- length(AZ)
		sphericalSTAR <- TRUE
	} else {
		AZ <- azimuth
		ALT <- altitude
		nangles <- length(AZ)
		quiet <- TRUE
		integration <- "Turtlesky"  # because STARbar is then simply average of specified angles.
		sphericalSTAR <- FALSE
		if(method=="QuasiMC")
			stop("Cannot use QuasiMC to calculate STAR for pre-defined angles (yet).")
		
		if(!quiet)message("Calculating STAR for pre-specified angles.")
	}

	# Total plant leaf area.
	LA <- sum(plant$leafdata$area)
	
	# init.
	DAs <- PAs <- Hs <- vector("numeric", nangles)
	
	# Msg.
	if(!quiet){
		message("Calculating STARbar with method: ",method,", integration scheme: ",integration,".")
		flush.console()
	}
	
	# Now assumed quasimc.exe sits in C:\quasimc
	# if(method %in% c("QuasiMC","slowquasimc") && !file.exists("QuasiMC.exe"))
		# stop("Need quasimc.exe in the working directory to continue.")
	
	if(method == "QuasiMC"){ 
	
		# 160 angles not yet supported (will perhaps be?)
		if(integration=="Yplant")stop("QuasiMC with Yplant integration not implemented.")

		# Run QMC in UOC mode (with 'black' leaves).
		qmcdapa <- runQMCUOC(plant, transmit=0, reflec=0,...)
	
		# Total displayed & projected area, STARbar.
		DA <- sum(qmcdapa$sunlit_area)
	    PA <- sum(qmcdapa$mean_projected_area)
	    LA <- sum(qmcdapa$actual_leaf_area)
		starbar <- DA / LA
		pabar <- PA / LA
		l <- list(STARbar = starbar, DAbar = DA, PAbar = PA, LA = LA, PALAbar = pabar)
		l$ldr <- NA
		l$sphericalSTAR <- sphericalSTAR
	}
	
	
	# Run QuasiMC once for every angle. (Slow, for testing only).
	if(method == "slowquasimc"){
	
		# Initialize.
		if(progressbar){
			wp <- txtProgressBar(title = "Calculating STAR", 
			label = "", min = 0, max = nangles, initial = 0, width = 50,style=3)
		}	
	
		# Don't do Yplant integration - will be much too slow.
		if(integration %in% c("Yplant","Turtle244"))
			stop("slowquasimc with Yplant integration not implemented (too slow).")
		
		# Extract QMC file from the plant object (it is generated in constructplant()).
		infile <- plant$qmcinputfile
		outfile <- plant$qmcoutputfile
		writeLines(plant$qmcinput, infile)
	
		for(i in 1:nangles){
		
			res <- DAPAQMC(plant,AZ[i],ALT[i], infile,outfile)
			DAs[i] <- res$DA
			PAs[i] <- res$PA
			Hs[i] <- res$H
		
		if(progressbar)setTxtProgressBar(wp, i)
		}
		if(progressbar)close(wp)
		
		# The LDR format.
		ldr <- data.frame(azimuth=AZ, altitude=ALT, DA=DAs, PA=PAs)
		
		# If turtle sky, each grid point has the same weight. Simple averages.
		DAbar <- mean(DAs)
		PAbar <- mean(PAs)
		
		starbar <- DAbar / LA
		pabar <- PAbar / LA
		l <- list(STARbar = starbar, DAbar = DAbar, PAbar = PAbar, LA = LA, PALAbar = pabar)
		l$ldr <- ldr
		l$sphericalSTAR <- sphericalSTAR
	}
	
  if(method == "exact")
    stop("Method temporarily unavailable. Contact package maintainer.")
  
	if(method == "gridtracer"){
		
		# Initialize.
		if(progressbar){
			wp <- txtProgressBar(title = "Calculating STAR", 
			label = "", min = 0, max = nangles, initial = 0, width = 50,style=3)
		}	
# 		exactbool <- ifelse(method == "exact",TRUE,FALSE)
 
		# Calculate DA, PA for all angles.
		for (i in 1:nangles) {
		
			res <- DAPA(plant, azimuth=AZ[i], altitude=ALT[i], exact=FALSE, npside=npside)
			DAs[i] <- res$DA
			PAs[i] <- res$PA
			Hs[i] <- res$H
			
		if(progressbar)setTxtProgressBar(wp, i)
		}
		if(progressbar)close(wp)
		
		# The result is identical to the LDR file in Yplant.
		ldr <- data.frame(azimuth=AZ, altitude=ALT, DA=DAs, PA=PAs, H=Hs)
		
		# Make average, Duursma et al. 2012 (NewPhyt) method.
		if(integration == "Yplant"){
			ldr$az <- rep(1:8,20)
			ldr$zen <- rep(1:20, each=8)
			zenmeans <- aggregate(ldr[,c("DA","PA")], list(ldr$zen), mean)
			s <- seq(0,90,length=21)
			zenmeans$alpha <- (pi/180)*(s[2:21] + s[1:20])/2
			
			da <- 2.25*pi/180
			zenmeans$Weight <- with(zenmeans, sin(alpha+da) - sin(alpha-da))
			
			DAbar <- with(zenmeans, weighted.mean(DA, Weight))
			PAbar <- with(zenmeans, weighted.mean(PA, Weight))
			ldr$az <- ldr$zen <- NULL
		}	
		
		# If turtle sky, each grid point has the same weight. Simple averages.
		if(integration %in%  c("Turtlesky","Turtle244")){
			DAbar <- mean(ldr$DA)
			PAbar <- mean(ldr$PA)
		}
		
	starbar <- DAbar / LA
	pabar <- PAbar / LA
	l <- list(STARbar = starbar, DAbar = DAbar, PAbar = PAbar, LA = LA, PALAbar = pabar)
	l$ldr <- ldr
	l$sphericalSTAR <- sphericalSTAR
	}

	class(l) <- "STAR"
return(l)
}

