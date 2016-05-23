#'@method plot yplocation
#'@S3method plot yplocation
#'@importFrom maps map
plot.yplocation <- function(x, add=FALSE, col="black", ...){

	if(!add)map('world', col=col, ...)
	points(x$long, x$lat, pch=19, col="red", cex=1.1)
	
}
