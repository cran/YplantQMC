STARbar.plant3dlist <- function(object, quiet=FALSE, ...){

	plants <- object
	nplants <- attributes(plants)$nplants
	pfiles <- attributes(plants)$pfiles
	lfiles <- attributes(plants)$lfiles
	
	dapas <- list()
	for(i in 1:nplants){
		
		tm <- system.time(dapas[[i]] <- try(STARbar(plants[[i]],quiet=TRUE,...)))
		elaps <- unname(tm[3])
		
		if(inherits(dapas[[i]], "try-error")){
			warning("STARbar returned error for plant ",pfiles[i])
			dapas[[i]] <- NA
			next
		}
		if(!quiet && nplants>1){
			
			cat("Plant",i,"of",nplants,"done in", elaps ,"sec.\n")
			flush.console()
		}
		dapatmp <- as.data.frame(dapas[[i]][1:4]) 
		# if(writeLDR & is.character(pfiles[i]))write.table(dapatmp, gsub("\\.p$",".LDR",pfiles[i],ignore.case=TRUE))
		# if(writeLDR & ! is.character(pfiles[i]))warning("Unable to write LDR.")
		dapas[[i]]$elapsed <- elaps
		dapas[[i]]$pfile <- pfiles[i]
		dapas[[i]]$lfile <- lfiles[i]
	}

if(length(dapas)==1)
	return(dapas[[1]])
else {
	class(dapas) <- "STARbarlist"
	return(dapas)
}
}
