setLocation <- function(lat=NA, 
                        long=0, 
						tzlong=NA){

	
	if(is.na(tzlong)){
		tzlong <- long
		tzlongset <- FALSE
	} else {
		tzlongset <- TRUE
	}
	
	if(is.na(lat)){
	  r <- require(maps)
	  if(!r) stop("Need to install package 'maps' to select location from a map.")
	  map('world')
	  latlong <- locator(1)
	  lat <- latlong$y
	  long <- latlong$x
	  tzlong <- long
	  tzlongset <- FALSE
	  message("Time of sunrise will be local apparent time.")
	  message("Location successfully selected.")
	  flush.console()
	}
  
	l <- list()
	l$lat <- lat
	l$long <- long
	l$tzlong <- tzlong
	l$tzlongset <- tzlongset
	
	class(l) <- "yplocation"
	
return(l)	
}



