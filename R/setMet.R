setMet <- function(location=NULL, 
                      metdat=NULL,
	                  year=2012,   # not so important; can use default.
					  month=NA, 
					  day=NA,  
					  nsteps=10, 
					  PARday=22,
					  AtmTrans=0.76,
					  fbeamday=NA,
					  fbeammethod=c("spitters","constant"),
					  Tmin=10,
					  Tmax=25,
					  VPDmax=NA,
					  maxlag=0.1,
					  Ca=390,
					  Patm=101
					  ){
	
	
	fbeammethod <- match.arg(fbeammethod)

	if(!is.na(month) && !is.na(day)){
		z <- zenaz(year, month, day, 
			location$lat, location$long, location$tzlong)
		sunrise <- z$sunset - z$daylength
		sunset <- z$sunset
		dh <- z$daylength / nsteps / 2
		
		# Time of day (in hrs local time).
		hours <- seq(sunrise+dh, sunset-dh, length=nsteps)

		# Zenith, azimuth of sun at many times; then interpolate.
		# Have to take this long route : some problem with fewer timesteps!
		sunposall <- zenaz(year, month, day, 
			location$lat, location$long, location$tzlong,
			timeofday=seq(sunrise+dh, sunset-dh, length=151))
		faz <- approxfun(x=sunposall$hour, y=sunposall$azimuth)
		falt <- approxfun(x=sunposall$hour, y=sunposall$altitude)
	}
	
	# Generate weather data, using a simple built-in routine.
	if(is.null(metdat)){
		if(is.null(location))stop("Need at least a location object, or a 'metdat' dataframe.")
		if(!inherits(location, "yplocation"))stop("location should be generated with setLocation().")
		if(is.na(month) || is.na(day))
			stop("Need month and day, to calculate solar path.")
		
		# Find azimuth and altitude.
		Azimuth <- faz(hours)
		Altitude <- falt(hours)
		Zenrad <- pi/2 - Altitude*pi/180

		# Simple estimate of available PAR (Yplant; Pearcy and Yang 1996).
		# a <- sunpos$altitude * pi/180
		# PARnorm <- solcons * AtmTrans^(1/sin(a))
		# PAR <- PARnorm * sin(a)
		DOY <- as.POSIXlt(ISOdate(year,month,day))$yday + 1
		
		pardfr <- calcparhrly(Zenrad, DOY, PARday, nsteps, 
			AtmTrans, fbeamday, fbeammethod)
		
		# VPD and T diurnal.
		vpdt <- VPDTdiurnal(Tmax=Tmax,Tmin=Tmin,reltime=(hours-sunrise)/z$daylength,
							maxlag=maxlag,VPDmax=VPDmax)
		
		dfr <- data.frame(timeofday=hours, altitude=Altitude, 
			azimuth=Azimuth, PAR=pardfr$PAR0, fbeam=pardfr$fbeam, 
			Tair=vpdt$Tair, VPD=vpdt$VPD, Ca=Ca, Patm=Patm)
	}
	if(!is.null(metdat)){
	
		# Some error checking might be nice here...
		if(!is.data.frame(metdat))metdat <- read.csv(metdat)
		nsteps <- nrow(metdat)
		hours <- 1:nsteps   # can be reset below, but is not always done.
	
		# If fbeam not 0 (diffuse only), azimuth and altitude not in metdat,
		# OR timeofday AND a location object, we cannot continue!
		if("fbeam" %in% names(metdat))
			diffonly <- all(metdat$fbeam == 0)  # TRUE if all fbeams in metdat = 0.
		else
			diffonly <- FALSE
		
		# Not a setting that would be recommended, but it is possible!
		if(fbeamday==0 & fbeammethod=="constant"){
			diffonly <- TRUE
			metdat$fbeam <- 0
		}
		azimaltset <- ("azimuth" %in% names(metdat)) && ("altitude" %in% names(metdat))
		if(!diffonly & !azimaltset & is.null(location))
			stop("** - Either need altitude&azimuth in metdat, or provide a location object!.")
		
		if(!azimaltset & !diffonly & !("timeofday" %in% names(metdat)))
			stop("Need solar altitude & azimuth in metdat, or provide 'timeofday' to calculate them.")
		if(!azimaltset & !diffonly & (is.na(month) & is.na(day)))
			stop("Need month and day set to calculate solar path.")
		
		if(azimaltset || diffonly){
			# For consistency with print.ypmet.
			z <- list()
			z$daylength <- "not calculated"
			z$sunset <- "not calculated"
			sunrise <- "not calculated"
			
			if(diffonly){
				metdat$azimuth <- 0
				metdat$altitude <- 0
			}
			
			#
			if(!("PAR0" %in% names(metdat)))
				stop("You provided altitude&azimuth, but not PAR0. No current method to estimate PAR0.")
			
		} else {
			# z <- zenaz(year, month, day, 
			# location$lat, location$long, location$tzlong)
			# sunrise <- z$sunset - z$daylength
			# sunset <- z$sunset
			# dh <- z$daylength / nsteps / 2
			
			# Time of day (in hrs local time).
			hours <- seq(sunrise+dh, sunset-dh, length=nsteps)

			# # Zenith, azimuth of sun at those times (list).
			# sunpos <- zenaz(year, month, day, 
				# location$lat, location$long, location$tzlong,
				# timeofday=hours)
			metdat$altitude <- falt(hours) #sunpos$altitude
			metdat$azimuth <- faz(hours) #sunpos$azimuth
			Zenrad <- pi/2 - Altitude*pi/180
			
			# 
			if(!("PAR0" %in% names(metdat)) |  !("fbeam" %in% names(metdat))){
				
				DOY <- as.POSIXlt(ISOdate(year,month,day))$yday + 1
				pardfr <- calcparhrly(Zenrad, DOY, PARday, nsteps, 
				AtmTrans, fbeamday, fbeammethod)
				if(!("PAR0" %in% names(metdat))){
					message("Calculating incident PAR (because PAR0 not given).")
					metdat$PAR0 <- pardfr$PAR0
				}
				if(!("fbeam" %in% names(metdat))){
					message("Calculating beam fraction (because fbeam not given)")
					metdat$fbeam <- pardfr$fbeam
				}	
			}
		}
		
		# VPD and T diurnal. Do NOT calculate if not given (too much trouble).
		if(!("VPD" %in% names(metdat))){
			metdat$VPD <- 1.5
			message("Setting VPD=1.5 kPa, because VPD not in met data.")
		}
		if(!("Tair" %in% names(metdat))){
			message("Setting Tair=25 degC, because Tair not in met data.")
			metdat$Tair <- 25
		}
		if(!("Ca" %in% names(metdat)))
			metdat$Ca <- Ca
		if(!("Patm" %in% names(metdat)))
			metdat$Patm <- Ca

		dfr <- data.frame(timeofday=hours, altitude=metdat$altitude, 
			azimuth=metdat$azimuth, PAR=metdat$PAR0, fbeam=metdat$fbeam, 
			Tair=metdat$Tair, VPD=metdat$VPD, Ca=metdat$Ca, Patm=metdat$Patm)

	}
	
l <- list()
l$dat <- dfr
l$method <- ifelse(is.null(metdat),"generated","input")
l$daylength <- z$daylength
l$sunset <- z$sunset
l$sunrise <- sunrise
l$location <- location
l$year <- year
l$month <- month
l$day <- day
class(l) <- "ypmet"	


return(l)	
}

