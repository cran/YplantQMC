YplantDay.plant3dlist <- function(x, met, phy=NULL, hemi=NULL, 
	...){

	plants <- x
	nplants <- attributes(x)$nplants
	
	ypres <- list()
	for(i in 1:nplants){
		
		cat(paste(c(rep("=",35),"\n"),collapse=""))
		cat("Plant",i,"out of",nplants,"\n")
		cat(paste(c(rep("-",35),"\n"),collapse=""))
		ypres[[i]] <- YplantDay(plants[[i]], met, phy, hemi,  ...)
	}

class(ypres) <- "yplantsimlist"

return(invisible(ypres))
}