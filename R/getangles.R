getangles <- function(plant, whichangle="An.3"){
	
	if(class(plant) == "plant3d"){
		if(plant$inputformat == "P"){
			pdata <- plant$pdata 
			inputformat <- "P"
		} else {
			pdata <- plant$qdata
			inputformat <- "Q"
		}
	} else if(file.exists(plant)){
		pdata <- readp(plant)
		inputformat <- "P"
	}
	
	if(inputformat == "P")
		angles <- pdata[pdata$Lt >= 1, whichangle]
	else
		angles <- pdata$An.3
	
	# Fix angles > 90
	if(whichangle == "An.3"){
	angles[angles > 90 & angles < 180] <- angles[angles > 90 & angles < 180] - 90
	angles[angles > 180 & angles < 270] <- 270 - angles[angles > 180 & angles < 270]
	angles[angles > 270 & angles < 360] <- angles[angles > 270 & angles < 360] - 270
	
	angles[angles < 0] <- -angles[angles < 0]
	}
	
	if(max(angles) > 360)warning("Angle >360deg in file", plant$pfile)
	
return(angles)
}