

!**********************************************************************
      SUBROUTINE CALCPARHRLY(RADBM,RADDF,ZEN,RADABV,FBEAM,TAU,KHRS)
! Calculate daily course of incident PAR from daily totals of
! beam and diffuse PAR (in MJ m-2 d-1).
! INPUTS:
! RADBM - daily total beam PAR (MJ m-2 d-1)
! RADDF - daily total diffuse PAR (MJ m-2 d-1)
! ZEN - zenith angle (radians)
! OUTPUTS:
! RADABV - array of hourly incident PAR (J m-2 s-1)
! FBEAM - array of hourly beam fractions
!**********************************************************************

    !USE maestcom
    IMPLICIT NONE
    INTEGER I,KHRS
    DOUBLE PRECISION ZEN(KHRS),RADABV(KHRS),FBEAM(KHRS)
    DOUBLE PRECISION COSBM(KHRS),COSDF(KHRS),SPERHR
    DOUBLE PRECISION SUMBM,SUMDF,COSZEN,HRTIME,RDBM,RADBM,RDDF,RADDF
    DOUBLE PRECISION PI,PID2,PID180, TAU, UMOLPERJ
	
  	PI = 3.1412653
  	PID2 = PI/2
  	PID180 = PI/180
  	UMOLPERJ = 4.57     ! Conversion from J to umol quanta
    SUMBM = 0.0
    SUMDF = 0.0
  	  
  	SPERHR = 24*60*60/DBLE(KHRS)

      DO I=1,KHRS
        COSBM(I) = 0.0
        COSDF(I) = 0.0
        HRTIME = I-0.5
        COSZEN = DCOS(ZEN(I))
        IF (COSZEN.GT.0.0) THEN
          IF (ZEN(I).LT.80*PID180) THEN  !Set FBM = 0.0 for ZEN > 80 degrees
            COSBM(I)=COSZEN*TAU**(1.0/COSZEN)
          ELSE
            COSBM(I)=0.0
          END IF
          COSDF(I)=COSZEN
          SUMBM=SUMBM+COSBM(I)
          SUMDF=SUMDF+COSDF(I)
        ENDIF
      ENDDO

      DO I=1,KHRS
        IF (SUMBM.GT.0.0) THEN
          RDBM = RADBM*COSBM(I)/SUMBM
        ELSE
          RDBM = 0.0
        END IF
        IF (SUMDF.GT.0.0) THEN
          RDDF = RADDF*COSDF(I)/SUMDF
        ELSE
          RDDF = 0.0
        END IF
		
! Convert from MJ m-2 hr-1 to mu mol m-2 s-1
        RADABV(I) = UMOLPERJ * (RDDF+RDBM)*1E6 / SPERHR
        IF ((RDBM+RDDF).GT.0.0) THEN
          FBEAM(I) = RDBM/(RDBM+RDDF)
        ELSE
          FBEAM(I)= 0.00
        END IF
      ENDDO

      RETURN
      END !CalcPARHrly



!**********************************************************************
      SUBROUTINE CALCFBMD(DOY,ZEN,PAR,FBM,KHRS)
! Calculate the beam fraction from the total daily incident radiation.
! Use the formula of Spitters et al. (1986) Agric For Met 38:217-229.
! INPUTS:
! DOY - day of year.
! ZEN - array of hourly sun zenith angle (radians)
! PAR - daily total incident PAR (MJ m-2 d-1)
! OUTPUTS:
! FBM - daily beam fraction
!**********************************************************************

    !USE maestcom
    IMPLICIT NONE
    INTEGER DOY,IHR,KHRS
    DOUBLE PRECISION ZEN(KHRS),PAR,FBM,FPAR,SPERHR
    DOUBLE PRECISION S0,SINB,TRANS,ETRAD,FDIF
    DOUBLE PRECISION PI,PID2
	
  	FPAR = 0.5
  	PI = 3.1415926
  	PID2 = PI/2
  	SPERHR = 24*60*60/DBLE(KHRS)
  	
! Calculate extra-terrestrial radiation
    S0 = 0.0
    DO IHR = 1,KHRS
      SINB = SIN(PID2-ZEN(IHR))
      S0 = S0 + ETRAD(DOY,SINB)*SPERHR/1E6
	  ENDDO

! Spitter's formula
    TRANS = (PAR/FPAR) / S0
    IF (TRANS.LT.0.07) THEN
      FDIF = 1.
    ELSE IF (TRANS.LT.0.35) THEN
      FDIF = 1. - 2.3*(TRANS-0.07)**2
    ELSE IF (TRANS.LT.0.75) THEN
      FDIF = 1.33 - 1.46*TRANS
    ELSE
      FDIF = 0.23   
    END IF   
    FBM = 1. - FDIF

    RETURN
    END !CalcFBMD


!**********************************************************************
     DOUBLE PRECISION FUNCTION ETRAD(DOY,SINB)
! Calculate the radiation incident on the atmosphere.
! Using formulae from Spitters et al (1986) Agric For Met 38:217
! Returns value in J m-2 s-1.
!**********************************************************************

    !USE maestcom
    IMPLICIT NONE
    INTEGER DOY
    DOUBLE PRECISION SINB,PI,SOLARC
  
    PI = 3.1415926
	  SOLARC = 1370.0       ! Solar constant (J m-2 s-1)
	
    ! Spitters' formula
    IF (SINB.GT.0.0) THEN
      ETRAD = SOLARC * (1 + 0.033*DCOS(2*PI*DBLE(DOY)/365.0)) * SINB
    ELSE
      ETRAD = 0.0
    END IF

    RETURN
    END !ETRad
