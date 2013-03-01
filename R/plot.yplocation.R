plot.yplocation <- function(x, add=FALSE, col="black", ...){

	r <- require(maps)
	if(!r) stop("Need to install package 'maps' to plot a Yplant location.")
	
	if(!add)map('world', col=col, ...)
	points(x$long, x$lat, pch=19, col="red", cex=1.1)
	
}
