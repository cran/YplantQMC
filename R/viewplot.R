

viewplot <- function(plant, side=c("east","south","above"), stems=TRUE, autopar=TRUE){

	side <- tolower(side)
	
	if(plant$inputformat == "Q")stems <- FALSE
	
	addbranches <- function(i1, i2){
		x <- plant
		for(i in 1:length(x$stems)){

			l <- x$stems[[i]]
			segments(x0=l$xyz$from[i1], x1=l$xyz$to[i1],
				y0=l$xyz$from[i2], l$xyz$to[i2], col="brown", lwd=1)
		}
		for(i in 1:length(x$branches)){
			l <- x$branches[[i]]
			segments(x0=l$xyz$from[i1], x1=l$xyz$to[i1],
				y0=l$xyz$from[i2], l$xyz$to[i2], col="brown", lwd=1)
		}
	}

	if(autopar){
		if(length(side) == 3 && all(par()$mfrow==c(1,1)))
			par(mfrow=c(2,2), pty='s')
		if(length(side) == 2 && all(par()$mfrow==c(1,1)))
			par(mfrow=c(1,2), pty='s')
	}
	
	if("south" %in% side){
		pp <- projectplant(plant, altitude=0, azimuth=180)
		plot(pp, leaffill=TRUE,
			xlab="X",ylab="Z",main="View from South",
			ylim=c(0,pp$viewbound$maxy))
		if(stems)addbranches(1,3)
	}
	
	if("east" %in% side){
		pp <- projectplant(plant, altitude=0, azimuth=90)
		plot(pp, leaffill=TRUE,
			xlab="Y",ylab="Z",main="View from East",
			ylim=c(0,pp$viewbound$maxy))
		if(stems)addbranches(2,3)
	}
	
	if("above" %in% side){
		plot(projectplant(plant, altitude=90, azimuth=180), leaffill=TRUE,
			xlab="X",ylab="Y",main="View from above")
		if(stems)addbranches(1,2)
	}

}




