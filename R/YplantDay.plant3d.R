
YplantDay.stand3d <- function(x,...){
  
  stop("No method exists yet for a stand3d object. Will be added soon.")
  
}



YplantDay.plant3d <- function(x, met, phy=NULL, hemi=NULL, quiet=FALSE,
	                          writePSR=TRUE, writeOUT=FALSE, ...){  
	
	plant <- x
	
	# Windows only. # MC 4/12/2012 - updated to include Mac OS X
	if(.Platform$OS.type != "windows" && (Sys.info()[['sysname']] != "Darwin"))
		stop("QuasiMC runs on Windows and Mac OS X only.")
	
	# Check input
	if(!inherits(met, "ypmet"))stop("Need object of class 'ypmet'; see ?setMet.")
	if(!is.null(phy) && !inherits(phy, "ypphy"))stop("Need object of class 'ypphy'; see ?setPhy.")
	if(!is.null(hemi) && !inherits(hemi, "yphemi"))stop("Need object of class 'yphemi'; see ?setHemi.")
	
	# Extract some data from met object.
	nsteps <- nrow(met$dat)
	daylength <- met$daylength
	hours <- met$dat$timeofday
	
	#
	if(!quiet){
		message("Yplant - simulation started.\n")
		cat(paste(c(rep("-",30),"\n"),collapse=""))  # a horizontal line.
		flush.console()
	}
	starttime <- proc.time()[3]

	# Run ray-tracer for every timestep.
	res <- list()
	message("Running QuasiMC and leaf gas exchange model.");flush.console()
	
	# First timestep: Calculate everything.
	res[[1]] <- runYplant(plant, phy, hemi, 
			altitude=met$dat$altitude[1],
			azimuth=met$dat$azimuth[1],PAR0=met$dat$PAR[1], 
			Tair=met$dat$Tair[1], 
			VPD=met$dat$VPD[1],Ca=met$dat$Ca[1], fbeam=met$dat$fbeam[1],
			rewriteplantfile=TRUE,   # do not rewrite plant file; unless 1st timestep.
			delfiles=FALSE,      # do not delete QuasiMC file; unless last timestep.
			...)
	othervars <- data.frame(timeofday = hours[1],
		                        leafnr = plant$leafdata$leafnr,
		                        timestep = ifelse(is.numeric(daylength),60*60 * daylength / nsteps,NA))

	res[[1]] <- cbind(othervars, res[[1]])
	reldiffPAR <- res[[1]]$reldiff
	if(!quiet){
		message("Timestep ",1," out of ",nsteps," completed.")
		flush.console()
	}
	
	# All other timesteps : only run QuasiMC if solar angle has changed (to save computing speed).
	if(nsteps > 1){
	for(i in 2:nsteps){

		# If solar altitude&azimuth same as in last timestep, use reldir from last timestep.
		if(met$dat$altitude[i] == met$dat$altitude[i-1] && 
			met$dat$azimuth[i] == met$dat$azimuth[i-1]){
		
			reldir <- res[[i-1]]$reldir
		} else {
			reldir <- NULL
		}
		
		res[[i]] <- runYplant(plant, phy, hemi, 
			reldiff=reldiffPAR, 
			reldir=reldir,
			altitude=met$dat$altitude[i],
			azimuth=met$dat$azimuth[i],PAR0=met$dat$PAR[i], 
			Tair=met$dat$Tair[i], 
			VPD=met$dat$VPD[i],Ca=met$dat$Ca[i], fbeam=met$dat$fbeam[i],
			rewriteplantfile=FALSE,
			delfiles=ifelse(i==nsteps, TRUE, FALSE),      # do not delete QuasiMC file; unless last timestep.
			...)
		if(!quiet){
			message("Timestep ",i," out of ",nsteps," completed.")
			flush.console()
		}
		
		othervars <- data.frame(timeofday = hours[i],
		                        leafnr = plant$leafdata$leafnr,
		                        timestep = ifelse(is.numeric(daylength),60*60 * daylength / nsteps,NA))

		res[[i]] <- cbind(othervars, res[[i]])
	}
	}

	# Return; should also include simulation settings in here.
	l <- list()
	l$plant <- plant
	if(!is.null(phy))l$phy <- phy
	if(!is.null(hemi))l$hemi <- hemi
	l$outdata <- do.call("rbind", res)
	l$nsteps <- nsteps

	# 'Plant Summary Report' data : sums and averages by timestep.
	makepsrdata <- function(out, plant, 
		sumvars=c("A","A0","E","PARleaf","PAR0","PARinc","PARdiff","PARdir")){
		
		LAplant <- sum(plant$leafdata$area)*10^-6  # m2
		
		# All are in (mu/m)mol m-2(leaf) s-1.
		sumvars <- intersect(names(out), sumvars)
		dfr1 <- out[,sumvars] * 10^-6 * out$LA
		dfr1$timeofday <- out$timeofday
		psrdata <- aggregate(dfr1,by=list(dfr1$timeofday),FUN=sum)  # mu mol s-1
		psrdata <- psrdata / LAplant  # convert back to mu mol m-2 s-1
		psrdata$timeofday <- as.vector(with(out, tapply(timeofday, timeofday, unique)))
		psrdata$timestep <- as.vector(with(out, tapply(timestep, timeofday, mean)))
		psrdata$LAplant <- LAplant
		
		# Projected and sunlit (displayed) leaf area.
		agla <- aggregate(out[,c("LAproj","LAsunlit")],by=list(out$timeofday),FUN=sum)*10^-6
		psrdata$LAproj <- agla[,2]
		psrdata$LAsunlit <- agla[,3]
		
		# clean up
		# Lame way to do this .... but it works, for now.
		tod <- psrdata$timeofday
		psrdata$timeofday <- NULL
		psrdata$Group.1 <- tod
		names(psrdata)[1] <- "timeofday"
		psrdata$Group.1 <- NULL
		
	return(psrdata)
	}
	l$psrdata <- makepsrdata(l$outdata, plant)
	

	l$met <- met
	endtime <- proc.time()[3]
	l$elapsedtime <- endtime - starttime 
	if(!quiet)
		message("\nSimulation completed in ", round(endtime-starttime,2)," seconds.")
	
	class(l) <- "yplantsim"
	
	if(writePSR)writePSRfile(l)
	if(writeOUT)writeOUTfile(l)
	
	
	return(l)

}
