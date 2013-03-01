


Silhouette <- function(obj, azimuth=NA, altitude=NA){

	if(inherits(obj, "plant3d")){
		if(is.na(azimuth) | is.na(altitude))stop("Need azimuth and altitude viewing angles.")
		newplant <- projectplant(obj, azimuth, altitude)
     } else	
		newplant <- obj
		
	P <- do.call("rbind", lapply(newplant$leaves, function(x)x$XYZ[,1:2]))
	ch <- chull(P)
	chp <- c(ch, ch[1])
	H <- areapoly(P[chp,])
	
	l <- list()
	l$H <- H
	l$xyz <- P[chp,]
	
return(l)
}

