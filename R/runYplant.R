
runYplant <- function(x,...)UseMethod("runYplant")

runYplant.plant3d <- function(x, 
			phy=NULL,
			hemi=NULL,
			reldiff=NULL,  # vector of length nleaves (diffuse radiation, relative units).
			reldir=NULL,  # vector of length nleaves (direct radiation, relative units).
			# Solar parameters.
			altitude=90, 
			azimuth=0, 
			fbeam=1.0,
			# Environmental parameters.
			VPD=1.5, 
			PAR0=1,
			PARwhere=c("above","below"),
			Ca=390,
			Tair=25,
			Patm=101,
			# Leaf parameters.
			reflec=c(0.1,0.1),
			transmit=c(0.1,0.1),
			# Other options
			runphoto=TRUE,
			intern=TRUE,
			debug=FALSE,  
			delfiles=TRUE,
			rewriteplantfile=TRUE,   # Not for users! Only for YplantDay.
			...    # parameters to writecfg (for QuasiMC)
			){
	
	plant <- x
  
	# Windows only. # MC 7/11/2012 - updated to include Mac OS X
        # .Platform$OS.type returns "unix" for Mac, so need to check Sys.info()
	if((.Platform$OS.type != "windows") && (Sys.info()[['sysname']] != "Darwin"))
		stop("QuasiMC is currently available for Windows and Mac OS X only.")
	
	# If 'above', PAR0 is measured above the canopy (and so must be downscaled with hemi),
	# if 'below', PAR0 is already reduced by canopy shading.
	PARwhere <- match.arg(PARwhere)
	
	# Extract the phy object from the plant - if it exists.
	if(!is.null(plant$phy))phy <- plant$phy
	
	if(is.null(phy)){
		runphoto <- FALSE
	}
		
	# If the phy object has 'transmit' and 'reflec' : use those.
	if("reflec" %in% names(phy$leafpars))reflec <- phy$leafpars$reflec
	if("transmit" %in% names(phy$leafpars))transmit <- phy$leafpars$transmit	
	
	# Top and bottom reflec,transmit can be input.
	if(length(reflec) == 1)reflec <- rep(reflec,2)
	if(length(transmit) == 1)transmit <- rep(transmit,2)
	
	# Extract QMC file from the plant object (it is generated in constructplant()).
	infile <- plant$qmcinputfile		
	outfile <- plant$qmcoutputfile
	if(rewriteplantfile){
		writeLines(plant$qmcinput, infile)
	}
	
	# Input file for QuasiMC - DIRECT radiation only.
	writecfg(cfgfile="autoquasimc.cfg", 
		sunazimuth=azimuth, 
		sunaltitude=altitude, 
		outputfile="qmcarea.out",
		returntype="D",
		leaftop=c(reflec[1],0,transmit[1],0,1),
		leafbottom=c(reflec[2],0,transmit[2],0,1),
		...)
	# writecfg(cfgfile="autoquasimc.cfg", 
		# outputfile="qmcarea.out",
		# returntype="D",
		# leaftop=c(reflec[1],0,transmit[1],0,1),
		# leafbottom=c(reflec[2],0,transmit[2],0,1),
		# lightsourcefile="directqmcin.dat",
		# ...
	# )
		
	# Run the QuasiMC model for direct radiation (unless fbeam is zero - use only diffuse PAR).
	# Output is in relative units (horizontal unshaded area = 1.0).
	# Possibly, relative direct PAR absorption (reldir) was given as input,
	# based on output by this very function.
	if(is.null(reldir) && fbeam > 0){
		tm1 <- system.time(runquasimc("autoquasimc.cfg",infile,outfile,debug=debug,intern=intern))

		# Read output.
		# reldir <- readQMCout(plant$qmcoutputfile)  # E() module; obsolete.
		outres <- read.table("qmcarea.out", header=TRUE)
		reldir <- outres$rel_mean_flux0  # APAR relative to horizontal surface.
		LAproj <- outres$mean_projected_area
		LAsunlit <- outres$sunlit_area
		LAtot <- outres$actual_leaf_area
		
	} else {
		LAproj <- NA
		LAsunlit <- NA
		LAtot <- NA
		if(fbeam == 0)reldir <- 0
	}
	

	# Run QuasiMC for diffuse radiation (unless it is given as input).
	if(is.null(reldiff) && fbeam == 1.0){
		reldiff <- 0.0
	}
	if(is.null(reldiff) && fbeam < 1){
		# APAR relative to horizontal surface.
		reldiff <- runQMCUOC(plant, hemi=hemi, transmit=transmit, 
			reflec=reflec, ...)$rel_mean_flux0  
	}
	
	# Gap fraction.
	if(!is.null(hemi)){
		# Gap fraction for direct light
		gapfracdir <- evalHemi(hemi, altitude, azimuth)$gapfraction
		
		# Weighted mean gap fraction for diffuse light
		dif <- evalHemi(hemi, altitude=turtle482$altitude, azimuth=turtle482$azimuth,
			degrees=FALSE)
		gapfracdiff <- weighted.mean(dif$gapfraction, sin(dif$altitude))
		
		# Reduce PAR due to canopy shading (if PAR 'measured' above canopy).
		if(PARwhere == "above"){
			PARinc <- PAR0 * (fbeam*gapfracdir + (1-fbeam)*gapfracdiff)
		}
			
	} else {
		    
		PARinc <- PAR0
	
	}
		
	# Actual PPFD on leaves (vector of length nleaves).
	PARdir <- reldir * fbeam * PARinc
	PARdiff <- reldiff * (1 - fbeam) * PARinc
	PARs <- PARdir + PARdiff

	# Results by leaf.
	ypresults <- data.frame(PAR0=PAR0, PARinc=PARinc, PARleaf=PARs, PARdir=PARdir, 
		PARdiff=PARdiff, reldiff=reldiff, reldir=reldir)
	
  if(nrow(ypresults) != plant$nleaves)stop("Critical failure. nrow(yplant) != plant$nleaves")
  
	ypresults$LA <- plant$leafdata$area
	ypresults$LAproj <- LAproj
	ypresults$LAsunlit <- LAsunlit
	
	if(runphoto){
		# Run photosynthesis module.
		metvars <- list(PAR=PARs, Tair=Tair, Ca=Ca, VPD=VPD, Patm=Patm)
		# tm2 <- system.time(
		photorun <- do.call(phy$leaffunction, c(phy$leafpars, metvars))

		# Run photosynthesis module for a flat unshaded leaf.
		metvars <- list(PAR=PARinc, Tair=Tair, Ca=Ca, VPD=VPD, Patm=Patm)
		photorunflat <- do.call(phy$leaffunction, c(phy$leafpars, metvars))

		# Add unshaded photosynthetic rate:
		photorun$A0 <- photorunflat$A
		
		# cbind.
		ypresults <- cbind(ypresults, photorun)
	} 
	
	# # elapsed times.
	# ypresults$raytracetime <- tm1[3]
	# ypresults$leafsimtime <- tm2[3]
	
	if(delfiles){
		unlink(infile)
		unlink(outfile)
		unlink("qmcarea.out")
		unlink("autoquasimc.cfg")
		unlink("turtlediffuse.dat")
	}
	
return(ypresults)
}



	





	
