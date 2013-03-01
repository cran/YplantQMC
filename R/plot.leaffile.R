plot.leaffile <- function(x, nleaf=1, edgepoints=TRUE, edgecex=0.8, ...){
	xyz <- x[[nleaf]]$XYZ
	leaflen <- max(xyz[,"Y"]) - min(xyz[,"Y"])
	leafwid <- max(xyz[,"X"]) - min(xyz[,"X"])
	span <- max(leaflen, leafwid)
	neglen <- min(xyz[,"Y"])
	Ylim <- c(neglen, span-neglen)
	Xlim <- c(-span/2, span/2)
	
	par(pty='s')
	plot(xyz[,"X"], xyz[,"Y"], type='n', pch=19, xlim=Xlim, ylim=Ylim,
	xlab="Width (mm)", ylab="Length (mm)",...)
	polygon(xyz[,"X"], xyz[,"Y"], col="forestgreen")
	if(edgepoints)points(xyz[,"X"], xyz[,"Y"], pch=19, cex=edgecex)
}



