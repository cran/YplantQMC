summary.plant3d <- function(object, nKErepeat=10, nsignif=3, calcSTARbar=FALSE, ...){

	plant <- object
    nleaves <- plant$nleaves
	pdata <- plant$pdata
	if(plant$inputformat == "P")
		pdatL <- plant$pdata[plant$pdata$Lt >= 1, ]
	else # if(plant$inputformat == "Q")
		pdatL <- plant$qdata
	
	if(nleaves > 0){
	
	XYZ <- do.call("rbind", lapply(plant$leaves, function(x)x$XYZ))
	
	# Crown surface and volume
    if(nleaves > 3){
		ch <- crownhull(XYZ, plotit=FALSE)
		crownvol <- ch$crownvolume * 1E-09  # m3
		crownsurf <- ch$crownsurface * 1E-06  # m2
	} else {
		crownvol <- NA
		crownsurf <- NA
    }

	# Crown radius, length and shape factor
	meanR <- getR(XYZ)
	cw <- 2*meanR / 1000
	cl <- (max(XYZ[,3]) - min(XYZ[,3])) / 1000
	cshape <- crownsurf / (cw*cl)
	
	# total plant height
	htot <- max(XYZ[,3])/1000
	
	# leaf dispersion
	ld <- leafdispersion(plant, crownvol = crownvol, nleaves=nleaves)
	if(all(is.na(ld))){
		disp <- disp2 <- Ek <- Ek2 <- Ok <- NA
	} else {
		disp <- ld$disp_noedge
		disp2 <- ld$disp_edge
		Ek <- ld$Ek_noedge
		Ek2 <- ld$Ek_edge
		Ok <- ld$Ok
	}
	
	# mean leaf angle
    angs <- getangles(plant)
	meanang <- mean(angs)
	
	# leaf angle weighed by leaf area (more appropriate to relate to PA, for example)
	L <- pdatL$L.3
	wmeanang <- weighted.mean(angs, L^2)
    
	# Fit ellipsoidal distribution
	if(nleaves > 3){
		X <- fitdistribution(angs, "ellipsoid")$distpars
	} else {
		X <- NA
    }
    
	# Total leaf area:
	LA <- 10^-6 * sum(plant$leafdata$area)
	meanleafsize <- 10^4 * LA / nleaves  # cm
	leaflen <- 0.1 * mean(L)
	
	# stem surface area (m2), stem volume (m3),
	# stem base diameter (mm), mean (and SD) path length from leaf to stembase (mm).  
	# and path length from leaf to soil.
	if(plant$inputformat == "P"){
		stemsurf <- 10^-6 * with(pdata, sum(D * pi * L) + sum(D.2 * pi * L.2))
		stemvol <- 10^-9 * with(pdata, sum((D/2)^2 * pi *L) + sum((D.2/2)^2 * pi * L.2))
		stembasediam <- pdata$D[1]
		if(stembasediam == 0)stembasediam <- pdata$D[2]
		if(nleaves >0){
			path <- try(pathlen(plant), silent=TRUE)
			if(inherits(path, "try-error")){
				path <- data.frame(totlen=NA)
				warning("Could not calculate path length for ",plant$pfile)
			}
		} else { 
			path <- data.frame(totlen=NA)
		}
		meanpath <- mean(path$totlen,na.rm=TRUE)
		sdpath <- sd(path$totlen,na.rm=TRUE)
		totlen <- sum(pdata$L) + sum(pdata$L.1)
	} else {
		stemsurf <- NA
		stemvol <- NA
		stembasediam <- NA
		meanpath <- NA
		totlen <- NA
		sdpath <- NA
	}	
	
	
	# For plants without leaves (yes, that's right).
	} else {
	
		stemsurf <- 10^-6 * with(pdata, sum(D * pi * L) + sum(D.2 * pi * L.2))
		stemvol <- 10^-9 * with(pdata, sum((D/2)^2 * pi *L) + sum((D.2/2)^2 * pi * L.2))
		stembasediam <- pdata$D[1]
		
		crownvol <- crownsurf <- leaflen <- 
		meanang <- wmeanang <- X <- Ek <- Ek2 <- Ok <- disp <- disp2 <- 
		meanpath <- sdpath <- totlen <- cw <- cl <- cshape <- LA <- meanleafsize <-
		NA
	
	} # end if(nleaves > 0)
	
	# STARbar calculation, if requested.
	if(calcSTARbar){
		run <- STARbar(plant,...) 
		STARbar_est <- run$DAbar / run$LA
	} else STARbar_est <- NA
	
	# (convex) crown projected area.
	AP <- 10^-6 * Silhouette(plant,azimuth=0,altitude=90)$H
	
	LFILE <- if(is.character(plant$lfile))plant$lfile else "unknown"
	
	obj <- list(plant$pfile, LFILE, crownvol,crownsurf,AP,nleaves,leaflen,
		meanang,wmeanang,X,Ek,Ek2,Ok,disp,disp2,
		stemsurf,stemvol,stembasediam,meanpath,
		sdpath,totlen,cw,cl,htot,cshape,LA,meanleafsize,STARbar_est)
	names(obj) <- c("pfile","lfile","crownvol","crownsurf","crownproj","nleavesp","leaflen",
		"meanleafang","wmeanleafang",
		"Xellipsoid","Ek","Ek2","Ok","disp","disp2",
		"stemsurf","stemvol","stemdiam","meanpath","sdpath","totlen",
		"cw","cl","htot","cshape","LA","meanleafsize","STARbar")
		
	obj$nsignif <- nsignif
		
	class(obj) <- "summary.plant3d"
	
	return(obj)
}


