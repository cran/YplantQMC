!--------------------------------------------------------------------

! Subroutines and functions for Yplant in R (/Fortran). Experimental stuff.
! Remko Duursma, January 2011.

!--------------------------------------------------------------------      
C        SUBROUTINE PNPOLY                                              
C                                                                       
C        PURPOSE                                                        
C           TO DETERMINE WHETHER A POINT IS INSIDE A POLYGON            
C                                                                       
C        USAGE                                                          
C           CALL PNPOLY (PX, PY, XX, YY, N, INOUT )                     
C                                                                       
C        DESCRIPTION OF THE PARAMETERS                                  
C           PX      - X-COORDINATE OF POINT IN QUESTION.                
C           PY      - Y-COORDINATE OF POINT IN QUESTION.                
C           XX      - N LONG VECTOR CONTAINING X-COORDINATES OF         
C                     VERTICES OF POLYGON.                              
C           YY      - N LONG VECTOR CONTAING Y-COORDINATES OF           
C                     VERTICES OF POLYGON.                              
C           N       - NUMBER OF VERTICES IN THE POLYGON.                
C           INOUT   - THE SIGNAL RETURNED:                              
C                     -1 IF THE POINT IS OUTSIDE OF THE POLYGON,        
C                      0 IF THE POINT IS ON AN EDGE OR AT A VERTEX,     
C                      1 IF THE POINT IS INSIDE OF THE POLYGON.         
C                                                                       
C        REMARKS                                                        
C           THE VERTICES MAY BE LISTED CLOCKWISE OR ANTICLOCKWISE.      
C           THE FIRST MAY OPTIONALLY BE REPEATED, IF SO N MAY           
C           OPTIONALLY BE INCREASED BY 1.                               
C           THE INPUT POLYGON MAY BE A COMPOUND POLYGON CONSISTING      
C           OF SEVERAL SEPARATE SUBPOLYGONS. IF SO, THE FIRST VERTEX    
C           OF EACH SUBPOLYGON MUST BE REPEATED, AND WHEN CALCULATING   
C           N, THESE FIRST VERTICES MUST BE COUNTED TWICE.              
C           INOUT IS THE ONLY PARAMETER WHOSE VALUE IS CHANGED.         
C           THE SIZE OF THE ARRAYS MUST BE INCREASED IF N > MAXDIM      
C           WRITTEN BY RANDOLPH FRANKLIN, UNIVERSITY OF OTTAWA, 7/70.   
C           !! Modifications by Remko Duursma, January 2011.
C                                                                       
C        SUBROUTINES AND FUNCTION SUBPROGRAMS REQUIRED                  
C           NONE                                                        
C                                                                       
C        METHOD                                                         
C           A VERTICAL LINE IS DRAWN THRU THE POINT IN QUESTION. IF IT  
C           CROSSES THE POLYGON AN ODD NUMBER OF TIMES, THEN THE        
C           POINT IS INSIDE OF THE POLYGON.                             

      SUBROUTINE PNPOLY(PX,PY,XX,YY,N,INOUTR)
      IMPLICIT NONE
	INTEGER N,J,I,INOUTR
	DOUBLE PRECISION X(N),Y(N),XX(N),YY(N)
      DOUBLE PRECISION PX,PY
      LOGICAL MX,MY,NX,NY
6     DO 1 I=1,N
      X(I)=XX(I)-PX
1     Y(I)=YY(I)-PY
      INOUTR = -1
      DO 2 I=1,N
      J=1+MOD(I,N)
      MX=X(I).GE.0.0
      NX=X(J).GE.0.0
      MY=Y(I).GE.0.0
      NY=Y(J).GE.0.0
      IF(.NOT.((MY.OR.NY).AND.(MX.OR.NX)).OR.(MX.AND.NX)) GO TO 2
      IF(.NOT.(MY.AND.NY.AND.(MX.OR.NX).AND..NOT.(MX.AND.NX))) GO TO 3
      INOUTR = -INOUTR

      GO TO 2
3     IF((Y(I)*X(J)-X(I)*Y(J))/(X(J)-X(I))) 2,4,5
4     INOUTR = 0
      RETURN
5     INOUTR = -INOUTR
2     CONTINUE
      RETURN
      END
      
      
!--------------------------------------------------------------------------------

! Wrapper to QSORTI (see below), returns sort order in increasing value.
      SUBROUTINE INCQSORTI(A,N,ORD)
      
      IMPLICIT NONE
      INTEGER N,i
      INTEGER ORDD(N),ORD(N)
      DOUBLE PRECISION A(N)

      CALL QSORTI(A,N,ORDD)
      
      DO I =1,N
          ORD(I) = ORDD(N-I+1)
      ENDDO
      
      END


!--------------------------------------------------------------------------------
! A fast sorter, taken from www.fortran.com/quick_sort1.f
! Takes array A (double), of length N, and returns the order in ORD.
      SUBROUTINE QSORTI (A,N,ORD)
!
!==============SORTS THE ARRAY A(I),I=1,2,...,N BY PUTTING THE
!   ASCENDING ORDER VECTOR IN ORD.  THAT IS ASCENDING ORDERED A
!   IS A(ORD(I)),I=1,2,...,N; DESCENDING ORDER A IS A(ORD(N-I+1)),
!   I=1,2,...,N .  THIS SORT RUNS IN TIME PROPORTIONAL TO N LOG N .
!
!
!     ACM QUICKSORT - ALGORITHM #402 - IMPLEMENTED IN FORTRAN 66 BY
!                                 WILLIAM H. VERITY, WHV@PSUVM.PSU.EDU
!                                 CENTER FOR ACADEMIC COMPUTING
!                                 THE PENNSYLVANIA STATE UNIVERSITY
!                                 UNIVERSITY PARK, PA.  16802
!
!      IMPLICIT INTEGER (A-Z)
      IMPLICIT NONE
      INTEGER X,XX,Z,ZZ,Y,N
      INTEGER NDEEP,I,IZ,IX,YP,IQ
      INTEGER ORD(N),POPLST(2,20)
      INTEGER U1,L1,L,U,P,Q,IP

!     TO SORT DIFFERENT INPUT TYPES, CHANGE THE FOLLOWING
!     SPECIFICATION STATEMENTS; FOR EXAMPLE, FOR FORTRAN CHARACTER
!     USE THE FOLLOWING:  CHARACTER *(*) A(N)
      DOUBLE PRECISION A(N)

      NDEEP=0
      U1=N
      L1=1
      DO 1  I=1,N
    1 ORD(I)=I
    2 IF (U1.LE.L1) RETURN

    3 L=L1
      U=U1

! PART
    4 P=L
      Q=U
!     FOR CHARACTER SORTS, THE FOLLOWING 3 STATEMENTS WOULD BECOME
!     X = ORD(P)
!     Z = ORD(Q)
!     IF (A(X) .LE. A(Z)) GO TO 2
!
!     WHERE "CLE" IS A LOGICAL FUNCTION WHICH RETURNS "TRUE" IF THE
!     FIRST ARGUMENT IS LESS THAN OR EQUAL TO THE SECOND, BASED ON "LEN"
!     CHARACTERS.

      X=A(ORD(P))
      Z=A(ORD(Q))
      IF (X.LE.Z) GO TO 5
      Y=X
      X=Z
      Z=Y
      YP=ORD(P)
      ORD(P)=ORD(Q)
      ORD(Q)=YP
    5 IF (U-L.LE.1) GO TO 15
      XX=X
      IX=P
      ZZ=Z
      IZ=Q

! LEFT

    6 P=P+1
      IF (P.GE.Q) GO TO 7
      X=A(ORD(P))
      IF (X.GE.XX) GO TO 8
      GO TO 6
    7 P=Q-1
      GO TO 13

! RIGHT

    8 Q=Q-1
      IF (Q.LE.P) GO TO 9
      Z=A(ORD(Q))
      IF (Z.LE.ZZ) GO TO 10
      GO TO 8
    9 Q=P
      P=P-1
      Z=X
      X=A(ORD(P))

! DIST

   10 IF (X.LE.Z) GO TO 11
      Y=X
      X=Z
      Z=Y
      IP=ORD(P)
      ORD(P)=ORD(Q)
      ORD(Q)=IP
   11 IF (X.LE.XX) GO TO 12
      XX=X
      IX=P
   12 IF (Z.GE.ZZ) GO TO 6
      ZZ=Z
      IZ=Q
      GO TO 6

! OUT

   13 CONTINUE
      IF (.NOT.(P.NE.IX.AND.X.NE.XX)) GO TO 14
      IP=ORD(P)
      ORD(P)=ORD(IX)
      ORD(IX)=IP
   14 CONTINUE
      IF (.NOT.(Q.NE.IZ.AND.Z.NE.ZZ)) GO TO 15
      IQ=ORD(Q)
      ORD(Q)=ORD(IZ)
      ORD(IZ)=IQ
   15 CONTINUE
      IF (U-Q.LE.P-L) GO TO 16
      L1=L
      U1=P-1
      L=Q+1
      GO TO 17
   16 U1=U
      L1=Q+1
      U=P-1
   17 CONTINUE
      IF (U1.LE.L1) GO TO 18

! START RECURSIVE CALL

      NDEEP=NDEEP+1
      POPLST(1,NDEEP)=U
      POPLST(2,NDEEP)=L
      GO TO 3
   18 IF (U.GT.L) GO TO 4

! POP BACK UP IN THE RECURSION LIST

      IF (NDEEP.EQ.0) GO TO 2
      U=POPLST(1,NDEEP)
      L=POPLST(2,NDEEP)
      NDEEP=NDEEP-1
      GO TO 18

! END SORT
! END QSORT
      END
      
      
!--------------------------------------------------------------------
! Returns the min value of an array (needed in For77 only)

      DOUBLE PRECISION FUNCTION MINVAL(ARR,ARRLEN)
      
      IMPLICIT NONE
      INTEGER ARRLEN,I
      DOUBLE PRECISION ARR(ARRLEN)
       
      MINVAL = ARR(1)
      DO I = 1,ARRLEN
          IF(ARR(I).LT.MINVAL)MINVAL = ARR(I)
      ENDDO
          
      END
!--------------------------------------------------------------------
! Returns the max value of an array (needed in For77 only)

      DOUBLE PRECISION FUNCTION MAXVAL(ARR,ARRLEN)
        
      IMPLICIT NONE
      INTEGER ARRLEN,I
      DOUBLE PRECISION ARR(ARRLEN)
                
      MAXVAL = ARR(1)
      DO I = 1,ARRLEN
          IF(ARR(I).GT.MAXVAL)MAXVAL = ARR(I)
      ENDDO
          
      END

!--------------------------------------------------------------------
! Equation of a plane, given by three points.
! Note that if two or more points are co-linear, all coefficients
! A-D will be zero.
      SUBROUTINE EQPLANE(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,A,B,C,D)
      
      IMPLICIT NONE
      DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,A,B,C,D
      
      A = Y1*(Z2 - Z3) + Y2*(Z3 - Z1) + Y3*(Z1 - Z2) 
	B = Z1*(X2 - X3) + Z2*(X3 - X1) + Z3*(X1 - X2) 
	C = X1*(Y2 - Y3) + X2*(Y3 - Y1) + X3*(Y1 - Y2) 
	D = -(X1*(Y2*Z3 - Y3*Z2) + X2*(Y3*Z1 - Y1*Z3) + 
     &      X3*(Y1*Z2 - Y2*Z1))
      
      END
      
!-------------------------------------------------------------------- 
! Z-value of the intersection between point XP,YP and a plane,
! specified by three points (P1,P2,P3). Uses equation of plane.
      DOUBLE PRECISION FUNCTION ZINTERSECT(XP,YP,XPOL,YPOL,ZPOL,N)

      IMPLICIT NONE
      INTEGER N,I1,I2,I3
      DOUBLE PRECISION XPOL(N),YPOL(N),ZPOL(N)
      DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,A,B,C,D
      DOUBLE PRECISION XP,YP
      
      ! Choose points on the leaf polygon that are very likely not co-linear.
      I1 = 1
      I2 = FLOOR(0.5 * REAL(N))
      I3 = FLOOR(0.75 * REAL(N))
      X1 = XPOL(I1)
      X2 = XPOL(I2)
      X3 = XPOL(I3)
      Y1 = YPOL(I1)
      Y2 = YPOL(I2)
      Y3 = YPOL(I3)
      Z1 = ZPOL(I1)
      Z2 = ZPOL(I2)
      Z3 = ZPOL(I3)
      
      CALL EQPLANE(X1,Y1,Z1,X2,Y2,Z2,X3,Y3,Z3,A,B,C,D)
      
      IF(A+B+C+D.EQ.0)THEN
        ZINTERSECT = -999.99
      ELSE
        ZINTERSECT = (-A*XP - B*YP - D)/C 
      ENDIF
      
      END FUNCTION

!--------------------------------------------------------------------
! 

      SUBROUTINE RAYTRACE(PXS,PYS,XXS,YYS,ZZS,N,NP,NS,TOTONLY,TOTARR,
     &                    INTLEAVES,ZINTER)
	  ! PXS,PYS - vectors of x and y test coordinates 
	  ! XXS and YYS - matrices with polygon outline coordinates.
	  	  
	  ! N - number of points per polygon
	  ! NP - number of polygons
	  ! NS - number of test points
	  ! TOTARR - array of length NS : 1 if intersects polygon, -2 if test point
	  ! falls outside rectangular boundary of the polygon, -1 if falls inside but
	  ! not within the polygon itself.
	  ! TOTONLY - 1 if only the 
	  
	IMPLICIT NONE
	INTEGER I,J,K,N,NP,NS,INTMP,TOTONLY,NINTERS
	INTEGER TOTARR(NS),INCORD(NP)
	INTEGER INTLEAVES(NS,NP)
	DOUBLE PRECISION ZINTER(NS,NP)
	DOUBLE PRECISION XXS(N,NP),YYS(N,NP),ZZS(N,NP)
	DOUBLE PRECISION XTMP(N),YTMP(N),ZTMP(N)
	DOUBLE PRECISION PXS(NS),PYS(NS)
      DOUBLE PRECISION, EXTERNAL :: MINVAL,MAXVAL,ZINTERSECT

	DO I = 1,NS  !  Loop through test points ('beams').
	  INTMP = 0
	  NINTERS = 0
	  K = 1
		DO J = 1,NP  ! Loop through polygons ('leaves').

		  XTMP = XXS(1:N,J)   ! polygon's X coordinates
		  YTMP = YYS(1:N,J)   !           Y
          ZTMP = ZZS(1:N,J)   !           Z

		  IF((PXS(I).GT.MAXVAL(XTMP,N).OR.PXS(I).LT.MINVAL(XTMP,N)).OR.
     &      (PYS(I).GT.MAXVAL(YTMP,N).OR.PYS(I).LT.MINVAL(YTMP,N)) )
     &    THEN
            ! Condition is reached when the test point is entirely outside
            ! the current polygon's bounding box.
		    INTMP = -2
	    ELSE		  
		    ! PNPOLY returns 1 for intersection, -1 for no intersection.
		    ! It returns 0 for 'edge', but we don't do anything with that at the moment.
		    CALL PNPOLY(PXS(I),PYS(I),XTMP,YTMP,N,INTMP)
		  ENDIF
			  
		  ! If TOTONLY=1, halts calculations if at least one leaf is intersected
		  ! This increases speed (although not so much), if only total intersections
		  ! are required.
		  IF(TOTONLY.EQ.1.AND.INTMP.EQ.1)GOTO 10
	    
	    ! Setup a counter here to keep track of how many leaves are intersected by the current beam,
	    ! as well as a vector of indices that tells us which leaves were intersected by the current beam.
          ! something like if INTMP.EQ.1, intersectedleaves[k] = j, k = k + 1 (k = nintersected).
          IF(INTMP.EQ.1)THEN
              NINTERS = NINTERS + 1   ! Number of intersected polygons
                                      ! by current beam.
                                      
			  ! Store the leaf number that is intersected by this beam
			  INTLEAVES(I,K) = J  
			  
			  ! Store the 'Z' value (in view coordinates) of the intersection.
			  ZINTER(I,K) = ZINTERSECT(PXS(I),PYS(I),XTMP,YTMP,ZTMP,N)

! HERE:  sort ZINTER in decreasing value of Z, for the current row, AND:
! sort INTLEAVES in that same order. This can be achieved with the quick_sort1.f code;
! it does not sort the array, but returns an order array (which we can use to re-index both array slices).
! Should be real fast, too...

			  K = K + 1
          ENDIF

          ! Then here, if more than one leaf was intersected, 
          ! sort intersectedleaves by their z values.
          ! First find z values of the intersection (note that Z values of polygons has to be input to this function...).
          ! Then summarize this by 1) current leaf is the nth leaf in the 'leaf intersection chain' for this gridpoint.
          ! If no intersection, this value can simply be one, two if one intersection, and so on.	    
	    
		ENDDO
		
		
10       TOTARR(I) = INTMP

        ! Store number of intersections, unless TOTONLY=1
        IF(TOTONLY.EQ.0.AND.NINTERS.GT.0)TOTARR(I) = NINTERS

        ! Sort ZINTER, so that sunlit leaves are on top, shaded leaves below (in order of shading).
        IF(NINTERS.GT.0)THEN
            CALL INCQSORTI(ZINTER(I,1:NP),NP,INCORD)
            ZINTER(I,1:NP) = ZINTER(I,INCORD)
            INTLEAVES(I,1:NP) = INTLEAVES(I,INCORD)
        ENDIF

		ENDDO  ! DO I = 1,NS


	  RETURN
	  END


