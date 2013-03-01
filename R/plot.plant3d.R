plot.plant3d <- function(x,
					  noleaves=FALSE,
					  squarewidth=250,
					  addcrownhull=FALSE,
					  hullalpha=0.4,
					  leaffill=TRUE,
					  leafoutline=TRUE,
					  stems=TRUE,
					  cylinderstems=stems,
					  leafcolor="forestgreen",
					  markleaves=NULL,
					  markcolor="red",
					  stemcol="black",
					  branchcol="black",
					  petiolecol="brown",
					  add=FALSE,shiftxyz=c(0,0,0),
					  ...
					  ){  

  
  if(leaffill & !noleaves){
    
    r <- suppressWarnings(require(ypaddon, quietly=TRUE))
    if(!r){
      ypaddonMessage()
      leaffill <- FALSE
    }
      
    
  }
  
	plant <- x
  
  if(!all(shiftxyz==0)){
    
    plant <- shiftplant(plant, shiftxyz[1],shiftxyz[2],shiftxyz[3])
    
  }
  
	if(noleaves){
		leaffill <- FALSE
		leafoutline <- FALSE
	}
	
	inputformat <- plant$inputformat
	if(stems && inputformat == "Q")stems <- FALSE  # Q format does not support stems.

	
	# Plot grey square on the ground:
	if(!add)open3d()
	s <- squarewidth
	M <- matrix(c(-s,-s,0,
				  s,-s,0,
				  s,s,0,
				  -s,s,0,
				  -s,-s,0), ncol=3, byrow=TRUE)
	if(s > 0)lines3d(M, col="darkgrey", add=add)

	# Add crownhull, maybe
	if(addcrownhull){
        g <- require(geometry)
        if(g)
			ch <- crownhull(plant, alpha=hullalpha)
        else
            warning("Cannot add convex hull. Install package 'geometry'.\n")
    }
	
	
	# Plot stems, branches, and petioles.
	if(plant$inputformat == "P"){
	
	Nnodes <- nrow(plant$pdata)
	
	# Stem segments.
	if(stems){
		Ms <- list()
		for(i in 1:Nnodes){
			Ms[[i]] <- rbind(rbind(plant$stems[[i]]$xyz$from, plant$stems[[i]]$xyz$to))
		}
		Ms <- do.call("rbind", Ms)
		segments3d(Ms, col=stemcol)
			
		# Branches.
		Ms <- list()
		for(i in 1:Nnodes){
			Ms[[i]] <- rbind(rbind(plant$branches[[i]]$xyz$from, plant$branches[[i]]$xyz$to))
		}
		Ms <- do.call("rbind", Ms)
		segments3d(Ms, col=branchcol)
	}
	# Add cylinder sections.
	if(stems & cylinderstems)plotstemsections(plant)

	
	# Petioles.
	Ms <- list()
	for(i in 1:Nnodes){
		Ms[[i]] <- rbind(rbind(plant$petioles[[i]]$xyz$from, plant$petioles[[i]]$xyz$to))
	}
	Ms <- do.call("rbind", Ms)
	segments3d(Ms, col=petiolecol)
	}
	
	if(leafoutline){
		LM <- list()
		np <- nrow(plant$leaves[[1]]$XYZ)
		for(i in 1:plant$nleaves){
			LM[[i]] <- plant$leaves[[i]]$XYZ
			LM[[i]] <- rbind(LM[[i]], LM[[i]][1,])  # duplicate first point to complete polygon.
			if(np %% 2 > 0)LM[[i]] <- rbind(LM[[i]], LM[[i]][np,])
			nr <- nrow(LM[[i]])
			LM[[i]] <- LM[[i]][rep(1:nr,each=2),]
			LM[[i]] <- LM[[i]][-c(1,nr*2),]
		}
		LM <- do.call("rbind", LM)
		segments3d(LM, col="black")
	}
	

	if(leaffill & !noleaves){
    
		if(is.null(markleaves))
			ypaddon::fillLeaves(plant,1:plant$nleaves, leafcolor)
		else {
			if(max(markleaves) > plant$nleaves)
				stop("Max. markleaves > number of leaves on plant.")
			otherleaves <- setdiff(1:plant$nleaves, markleaves)
			ypaddon::fillLeaves(plant,otherleaves, leafcolor)
			ypaddon::fillLeaves(plant,markleaves, markcolor)
		}
		
	}

options(warn=0)
  return(invisible(LM))
}



