plot.plant3dlist <- function(x, whichrows=NA, png=FALSE, pngsuff="", keepopen=!png,
	windowrect=c(50,50,800,800), squarewidth=25, skipexisting=FALSE, addfilename=FALSE, ...){
	
	plants <- x
	pfiles <- attr(plants, "pfiles")
	lfiles <- attr(plants, "lfiles")
	nplants <- attr(plants, "nplants")
	
	if(whichrows == "all" || is.na(whichrows))whichrows <- 1:nplants

# nplants is in attributes of plants
	
	if(keepopen && length(whichrows)>10)stop("You don't want this many windows open ... set png=TRUE or reduce number of plots.")
	
	for(k in whichrows){
		
		pfilename <- pfiles[k]
		lfilename <- lfiles[k]
		
		outputname <- paste(pfilename,pngsuff,".png",sep="")
		
		# skipexisting=TRUE skips PNGs that are already produced.
		if(skipexisting && file.exists(outputname))next
		
		open3d(windowRect=windowrect)
		p <- try(plot(plants[[k]], add=TRUE, squarewidth=squarewidth, ...))
		if(inherits(p,"try-error")){
			warning("Failed to plot",pfilename)
			rgl.close()
			next
		}
		if(addfilename)title3d(sub=pfiles[k], line=-1, col="black")
		if(png)snapshot3d(outputname)
		if(!keepopen)rgl.close()
	}
}
