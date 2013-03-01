
makeStand <- function(plants=list(),
                      xyz=data.frame(x=0,y=0,z=0),
                      plotbox=NULL
){
  
  
  if(nrow(xyz) != length(plants))
    stop("Must provide X,Y,Z of stem position for each plant.")
  
  
  # Shift plants.
  for(i in 1:length(plants)){
    plants[[i]] <- shiftplant(plants[[i]], xyz[i,1], xyz[i,2], xyz[i,3])
  }
  
  # Find plotbox, if not provided already.
  if(is.null(plotbox)){
    
    maxx <- maxy <- minx <- miny <- 0
    for(i in 1:length(plants)){
      minx <- min(minx, min(plants[[i]]$leafbasecoor[,1]))
      minx <- min(minx, min(plants[[i]]$leaftipcoor[,1]))    
      
      maxx <- max(maxx, max(plants[[i]]$leafbasecoor[,1]))
      maxx <- max(maxx, max(plants[[i]]$leaftipcoor[,1])) 
      
      miny <- min(miny, min(plants[[i]]$leafbasecoor[,2]))
      miny <- min(miny, min(plants[[i]]$leaftipcoor[,2]))    
      
      maxy <- max(maxy, max(plants[[i]]$leafbasecoor[,2]))
      maxy <- max(maxy, max(plants[[i]]$leaftipcoor[,2]))   
    }
    plotbox <- c(minx,miny,maxx,maxy)
    
  }
  
  # LAI:
  la <- c()
  for(i in 1:length(plants)){
    la[i] <- sum(plants[[i]]$leafdata$area)
  }
  LA <- sum(la)
  b <- plotbox
  area <- (b[3] - b[1]) * (b[4] - b[2])
  LAI <- LA / area
  
  
  
  l <- list()
  l$plants <- plants
  l$nplants <- length(plants)
  l$nleaves <- sapply(plants, "[[","nleaves")
  l$xyz <- xyz
  l$plotbox <- plotbox
  l$LAI <- LAI
  
  ld <- lapply(plants, "[[", "leafdata")
  l$leafdata <- do.call(rbind,ld)
  
  class(l) <- "stand3d"
  return(l)
}
