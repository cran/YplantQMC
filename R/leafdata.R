leafdata <- function(plant,...){

	
	if(plant$inputformat == "P"){
		leaflen <- plant$pdata$L.3[plant$pdata$Lt >0]
		petiolelen <- plant$pdata$L.2[plant$pdata$Lt >0]
		an <- plant$pdata$An.3[plant$pdata$Lt >0]
		az <- plant$pdata$Az.3[plant$pdata$Lt >0]
		or <- plant$pdata$Or[plant$pdata$Lt >0]
		Lts <- plant$pdata$Lt[plant$pdata$Lt>0]
	}
	
	if(plant$inputformat == "Q"){
		leaflen <- plant$qdata$L.3
		petiolelen <- rep(0, plant$nleaves)
		an <- plant$qdata$An.3
		az <- plant$qdata$Az.3
		or <- plant$qdata$Or
		Lts <- plant$qdata$Lt
	}
	
	# Find leaf shape (may vary by leaftype).
	lsh <- c()
	for(i in 1:length(Lts))lsh[i] <- plant$ldata[[Lts[i]]]$leafshape
	
	leafarea <- leaflen^2 * lsh

	# Z coordinate of leaf base.
	# f <- function(x)unname(x$XYZ[x$midribpoints[1],3])
	# heightbase <- sapply(plant$leaves,f)
	heightbase <- plant$leafbasecoor[,3]
	
return(data.frame(leafnr=1:length(leaflen), heightbase=heightbase,
	len=leaflen, area=leafarea, 
	ang=an, az=az, or=or,
	petilen=petiolelen, 
	leafshape=lsh))

}

