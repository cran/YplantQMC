
plot.stand3d <- function(x,...){
  
  n <- length(x$plants)
  
  # open rgl canvas
  open3d()
  
  # Draw plot boundary
  pb <- x$plotbox
  M <- matrix(c(pb[1],pb[2],0,
                pb[3],pb[2],0,
                pb[3],pb[4],0,
                pb[1],pb[4],0,
                pb[1],pb[2],0), ncol=3, byrow=TRUE)
  lines3d(M, col="darkgrey")
  
  # Add plants
  for(i in 1:n)plot(x$plants[[i]], 
                    add=TRUE, 
                    squarewidth=0, 
                    ...)
  
}