makereport <- function(plant=NULL, phy=NULL, met=NULL, hemi=NULL, ypsim=NULL, filename=NA){

	r <- suppressPackageStartupMessages(require(gplots, quietly=TRUE))
	if(!r)stop("To make a PDF report, install the 'gplots' package.")

	if(is.na(filename)){
		filename <- paste0("YplantQMC_report_",as.Date(Sys.time()),".pdf")
	}

	# open pdf
	pdf(filename, onefile=TRUE)

	emptyplot <- function(){
		par(mar=c(0,0,0,0),xaxs="i",yaxs="i")
		plot(1, type='n', ann=FALSE,axes=FALSE,xlim=c(0,1),ylim=c(0,1))
	}

	# First page.
	emptyplot()
	text(0.5,0.9, expression(bold("YplantQMC Report")), cex=2)
	text(0.1,0.8, paste("Generated on : ",Sys.time()), cex=1.2, font=3, pos=4)
	abline(h=0.75)

	text(0.1,0.6, "Objects included:", font=2, pos=4, cex=1.3)
	# ypos <- c(0.5,0.4,0.3,0.2,0.1)
	ypos <- seq(0.5,0.15, length=5)
	k <- 1
	if(!is.null(plant)){text(0.1,ypos[k],paste("Plant      :", deparse(substitute(plant))), cex=1.2, pos=4);k <- k + 1}
	if(!is.null(phy)){text(0.1,ypos[k],  paste("Physiology :", deparse(substitute(phy))), cex=1.2, pos=4);k <- k + 1}
	if(!is.null(met)){text(0.1,ypos[k],  paste("Weather    :", deparse(substitute(met))), cex=1.2, pos=4);k <- k + 1}
	if(!is.null(hemi)){text(0.1,ypos[k], paste("Hemiphoto  :", deparse(substitute(hemi))), cex=1.2, pos=4);k <- k + 1}
	if(!is.null(ypsim)){text(0.1,ypos[k],paste("Simulation :", deparse(substitute(ypsim))), cex=1.2, pos=4)}

	if(!is.null(plant) && !is.null(ypsim)){
		if(!identical(plant, ypsim$plant))warning("**-Plant object and plant used for simulation are not the same!")
	}
	if(!is.null(plant) && !is.null(hemi)){
		if(!identical(hemi, ypsim$hemi))warning("**-Hemiphoto object and hemiphoto used for simulation are not the same!")
	}
	
	# PLANT
	if(!is.null(plant)){
	
		# Page 1. Print plant object.
		par(mfrow=c(2,2), mar=c(5,5,2,2), cex.axis=0.7)
		textplot(capture.output(print(plant,hint=FALSE)), halign="right", cex=0.9)
		viewplot(plant)
		
		# Page 2. Leaf plot.
		par(mfrow=c(1,1), xaxs="r", yaxs="r", cex.axis=0.9)
		plot(plant$ldata)
		title("Leaf")
		
		# Page 3. Plant summary.
		textplot(capture.output(summary(plant)), halign="left", cex=0.8)
		title("Plant summary", line=1)
	
		# Page 4. Leaf angle distribution.
		ang <- plant$leafdata$ang
		f <- fitdistribution(ang, "twoparbeta")
		par(yaxs="r", xaxs="r")
		plot(f, main="Leaf angle distribution with Beta fit", col="grey")
		
	}
	
	# PHYSIOLOGY
	if(!is.null(phy)){
		# Page 1.
		par(mar=c(5,5,2,2))
		textplot(capture.output(phy), cex=1, halign="center")
		title("Physiology object")
		
		# Page 2.
		# light response curve.
		# parvals <- seq(0, 1800, length=101)
		# 'lightresponse' easy.
		# 'Farquhar' : typical, midday, average met variables (Tair, VPD)?
		# Or one light response curve by timestep? (and label curves with labcurve (package Hmisc)?
		# also put histogram below the x-axis showing PARleaf distribution? (but for what timestep?)
		# timestep with highest available PAR?
	}
	
	# MET
	if(!is.null(met)){
		# Page 1.
		par(mar=c(3,3,2,2))
		textplot(capture.output(met), cex=0.7, halign="center")
		title("Weather (met) object")
		
		# Page 2.
		par(mar=c(3,3,5,2))
		plot(met)
		title("Weather (met) object", line=2)
	}
	
	# HEMI
	if(!is.null(hemi)){
		# Page 1.
		par(mar=c(3,3,2,2))
		if(!is.null(met)){
			plot(hemi, met, warn=FALSE)
		} else {
			plot(hemi)
		}
		title("Hemi photo")
	}
	
	# Yplant Simulation.
	if(!is.null(ypsim)){
		
		# Page 1.
		par(mar=c(3,3,3,2))
		textplot(capture.output(ypsim), cex=0.8, halign="center")
		title("Yplant day simulation")
		
		# Page 2. Sunlit leaf area.
		par(mar=c(5,5,2,2), cex.lab=1.3)
		plot(ypsim, type="LAsunlit", setpar=FALSE)
		
		# Page 3.
		par(xaxs="r", yaxs="r")
		plot(ypsim, openwin=FALSE)
	
	}
	
	dev.off()
	message("Report generation probably successful.")
	message("See file \'",filename,"\' in directory :\n  ",getwd())
	
}
# makereport(phy=eucphy, met=aprilday, plant=epil, ypsim=epilrun, hemi=myhemi)
# makereport(ypsim=epilrun)
# makereport(hemi=myhemi, met=aprilday)

