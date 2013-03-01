checkHemi <- function(hemi){
	gapfracs <- evalHemi(hemi, altitude=turtle482$altitude, azimuth=turtle482$azimuth,degrees=FALSE)	
	hX <- cos(turtle482$altitude) * sin(turtle482$azimuth)
	hY <- cos(turtle482$altitude) * cos(turtle482$azimuth)
	plot(hemi)
	points(hX,hY,col='lightgrey',cex=0.5, pch=19)
	points(hX,hY,col='red',cex=3*gapfracs$gapfraction)
}