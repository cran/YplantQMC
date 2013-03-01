
# Non-rectangular hyperbola
lightresponse <- function(PAR, Amax, phi, theta, Rd, ...){
                     

   A <- (phi*PAR+Amax-((phi*PAR+Amax)^2-4*theta*phi*PAR*Amax)^0.5)/(2*theta) - Rd
	   
return(data.frame(A=A))	   
}
