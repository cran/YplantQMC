      subroutine kn(x,y,z,nleaf,res)
		 
      integer nleaf
      integer i,j
      double precision x(nleaf), y(nleaf), z(nleaf)	  
      double precision distan
      double precision res(nleaf,nleaf)
	  
      do 10 i = 1,nleaf
         do 20 j = 1,nleaf
		 
           res(i,j) = SQRT((x(j) - x(i))**2 + (y(j) - 
     &          y(i))**2 + (z(j) - z(i))**2)
		 
20    continue		 
10    continue		 
		 		 
      return
      end
