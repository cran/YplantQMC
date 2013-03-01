Farquhar <- function(PAR, Tair, Ca, VPD, RH=0, Patm=101, SWP=0,
                     Vcmax, Jmax, Rd0, G1,   # Minimum required parameters.
                     ...){    # All other leaf parameters passed anonymously. Defaults set below.

					 
	# Default parameters.				 
    # These go into default parameter file. Where to save it though??
	# Respiration parameters
	Q10F = 0.67		 # logarithm of the Q10 (Equation for respiration : 
					 # RESP =  RD0 * EXP(Q10F * (TLEAF-RTEMP)/10) * DAYRESP
	RTEMP = 25       # Reference temperature (T at which RD0 was measured)
	DAYRESP = 1.0    # Respiration in the light as fraction of that in the dark.   
	TBELOW = -100.0  # No respiration occurs below this temperature (degC).

	# Stomatal conductance parameters
	MODELGS = 4      # model : 4 = Medlyn et al. 2011; 6 = Tuzet et al. 2003.
	EMAXLEAF = 999   # Only used when considering soil water stress and MODELGS is not 6.
	KTOT = 2         # Leaf-specific hydraulic conductance (mmol m-2 s-1 MPa-1)

	G0 = 0.03        # Stomatal leakiness (gs when photosynthesis is zero).

	D0L = 5          # Parameter for the Leuning model (MODELGS=3)
	GAMMA = 0        # Gamma for all Ball-Berry type models
	# G1 = 7           # Parameter for all Ball-Berry type models
	GK = 0.3         # Parameter for three-parameter Medlyn et al. 2011 model (MODELGS=5)

	SF = 3.2         # Tuzet model parameters (MODELGS=6)
	PSIV = -1.9

	# Light-response parameters of electron transport rate.
	THETA = 0.4      # Shape parameter of the non-rectangular hyperbola.
	AJQ = 0.324      # Quantum yield of electron transport.
	HMSHAPE = 0.999  # Shape of the hyperbolic minimum function (no need to change)

	# Temperature response parameters.
	# Parameters for Jmax.
	EAVJ = 37259     # Ha in Medlyn et al. (2002)
	EDVJ = 200000    # Hd in Medlyn et al. (2002)
	DELSJ = 640.02   # DELTAS in Medlyn et al. (2002)

	# Parameters for Vcmax.
	EAVC = 47590     # Ha in Medlyn et al. (2002)
	EDVC = 0.0       # Hd in Medlyn et al. (2002)
	DELSC = 0.0      # DELTAS in Medlyn et al. (2002)

	# Evaluate arguments.
	# Override with parameters given:
	l <- as.list(match.call())
	
	if(length(l) > 1){
		for(i in 2:length(l)){
			assign(names(l)[i], l[[i]])
		}
	}
	
	# Rename aliases.
	# Names in the 'photosyn' function in GasExchangeR
	TLEAF <- Tair
	CS <- Ca
	PATM <- Patm * 1000
	VCMAX25 <- Vcmax
	JMAX25 <- Jmax
	RD0 <- Rd0
	WEIGHTEDSWP <- SWP
  
    # Note: G1 is for CO2 in photosyn(); but for H2O in Farquhar (as it should be).
    photorun <- photosyn(PAR=PAR,TLEAF=TLEAF,CS=CS,VPD=VPD,RH=RH,PATM=PATM,VCMAX25=VCMAX25,
                         JMAX25=JMAX25,RD0=RD0,Q10F=Q10F,RTEMP=RTEMP,DAYRESP=DAYRESP,
                         TBELOW=TBELOW,MODELGS=MODELGS,EMAXLEAF=EMAXLEAF,KTOT=KTOT,
                         WEIGHTEDSWP=WEIGHTEDSWP,G0=G0,D0L=D0L,GAMMA=GAMMA,
						 G1=G1/1.6,GK=GK,SF=SF,PSIV=PSIV,THETA=THETA,AJQ=AJQ,
						 HMSHAPE=HMSHAPE,EAVJ=EAVJ,EDVJ=EDVJ,DELSJ=DELSJ,
						 EAVC=EAVC,EDVC=EDVC,DELSC=DELSC)
	
	# Make sure that output variables are standardized
	A <- photorun$ALEAFHM
	E <- photorun$ELEAF
	gs <- photorun$GS
	
	dfr <- data.frame(A=A,E=E,gs=gs)
	
return(dfr)
}
