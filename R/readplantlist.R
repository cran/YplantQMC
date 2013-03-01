readplantlist <- function(pfiles=NA, lfiles=NA, lpk="leafplantkey.txt", multiplier=1.0){
	
  # Init for 'bad' plants that cannot be read
	nbad <- 0
	whichbad <- c()
  
	if(length(pfiles) != length(lfiles))
		stop("Provide equal number of plant and leaf files.")
	# Read from leafplantkey
	if(all(is.na(pfiles)) && file.exists(lpk)){
		lpk <- read.csv(lpk, header=FALSE, stringsAsFactors=FALSE, strip.white=TRUE)
		pfiles <- lpk[,1]
		lfiles <- lpk[,2]
	} 

	# in case pfiles,lfiles were provided as factors.
	pfiles <- as.character(pfiles)
	lfiles <- as.character(lfiles)
	
	# Multiplier (to change units, or fix strange files)
	if(length(multiplier) != length(pfiles))
		multipliers <- rep(multiplier, length(pfiles))
	else
		multipliers <- multiplier
	
	plants <- list()
	for(i in 1:length(pfiles)){
		tm <- system.time(plants[[i]] <- try(constructplant(pfiles[i], lfiles[i], 
			multiplier=multipliers[i]), silent=TRUE))
		if(inherits(plants[[i]], "try-error")){
			warning("Error constructing from ",pfiles[i],";",lfiles[i]," - plant skipped.")
			nbad <- nbad + 1
			whichbad <- c(whichbad, i)
			next
		} else {
			cat("Plant",i,"of",length(pfiles),"(", pfiles[i], ") constructed in", unname(tm[3]),"sec.\n")
			flush.console()
		}
	}
	
	plants[whichbad] <- NULL
	pfilesnotread <- pfiles[whichbad]
  lfilesnotread <- lfiles[whichbad]
  
  if(length(whichbad) > 0)
    message("Some plants could not be read. See attributes(MyPlants)$notread")
  
  # pfiles, lfiles that were actually read in will be stored as attributes
	pfiles <- sapply(plants, "[[", "pfile")
	lfiles <- sapply(plants, "[[", "lfile")
	
	attributes(plants) <- list(pfiles=pfiles, lfiles=lfiles, nplants=length(plants),
                             notread=data.frame(pfile=pfilesnotread,lfile=lfilesnotread))
	class(plants) <- "plant3dlist"
	
	return(plants)
}

