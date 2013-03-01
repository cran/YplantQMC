


xmastime <- function(plant,
					      palette=c("red","blue2","yellow","darkorchid3","gold1"),
                          zadj = 1.5,
						  nballsfrac = 0.2,
						  ballradiusrel = 0.25){
						  
						  
	basexyz <- do.call("rbind", lapply(plant$leaves, function(x)x$XYZ[1,]))

	meanleafsize <- mean(plant$leafdata$len)

	nballs <- floor(nballsfrac * plant$nleaves)
	xyz <- basexyz[sample(1:nrow(basexyz),nballs),]
	r <- rnorm(nballs, mean=ballradiusrel * meanleafsize, sd=(meanleafsize/4)/10)
	l <- zadj * r

	cols <- sample(palette,plant$nleaves,replace=TRUE)

	spheres3d(x=xyz[,1],y=xyz[,2],z=xyz[,3] - l, radius=r, color=cols, add=TRUE)
	message("Happy holidays!")

}

# plot(plant)
# christmastime(plant)

