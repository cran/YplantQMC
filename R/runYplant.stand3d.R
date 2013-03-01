runYplant.stand3d <- function(x,...){
  
  
  stand <- x  
  
  # List of leaves
  leaves <- list()
  for(i in 1:stand$nplants){
    leaves[[i]] <- stand$plants[[i]]$leaves
  }
  leaves <- do.call(c,leaves)
  
  # Fake 'plant' object; a link of all plants in the stand.
  p <- list(leaves=leaves)
  p$nleaves <- sum(stand$nleaves)
  
  # Make QuasiMC input file
  m <- makeQMCinput(p, writefile=FALSE)
  p$qmcinput <- m$qmcinput
  p$qmcinputfile <- m$inputfile
  p$qmcoutputfile <- m$outputfile
  
  p$phy <- NULL
  
  # $leafdata$area
  ld <- lapply(stand$plants, function(x)x$leafdata)
  ld <- do.call(rbind,ld)
  p$leafdata <- ld
  
  run <- runYplant.plant3d(p,...)

  run$plantnr <- rep(1:stand$nplants, stand$nleaves)

return(run)
}

