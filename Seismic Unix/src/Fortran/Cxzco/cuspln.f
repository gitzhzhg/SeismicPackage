c---------------------------------------------------------------------
      subroutine cuspln(nint,x,z,npts,a0,a1,a2,a3)
c...................................................................
c     This subroutine fits a cubic spline through the points
c     defining each interface.  The curvatures at each end are
c     taken to be zero.  The remaining curvatures are found by
c     inverting a tridiagonal matrix.  Knowing the curvatures we
c     evaluate the spline coefficients.
c     Reference: Numerical Computing and Mathematical Analysis
c                by Stephen M. Pizer.  Science Research Associates, Inc.
c                Pages 307-311.
c...................................................................
      integer     maxn,                    mxspm1
c     MXSPM1 should equal MAXSPL in main, less one
c     MAXN   is maximum value of N
      parameter ( maxn   = 50,
     :            mxspm1 = 150)
      real        x(maxn,0:mxspm1),        z(maxn,0:mxspm1),
     :            a0(maxn,mxspm1),         a1(maxn,mxspm1),
     :            a2(maxn,mxspm1),         a3(maxn,mxspm1)
      integer     nint,                    npts(nint)
c...................................................................
cc    local   variables
c     B()     right hand side of matrix equation for curvatures
c     C()     subdiagonal of matrix to be solved for curvatures
c     CV()    curvatures at spline points
c     D()     diagonal of matrix to be solve for curvatues
c     E()     superdiagonal of matrix to be solved for curvatures
c     I       loop variable
c     INFO    indicates if matrix is singular
c     J       loop variable over interfaces
c     M       loop variable
c     MAXN    maximum value of N
c     MXSPM1  maximum number of points in interface, less one
c     N       number of unknown curvatures
c     STDOUT  unit number of standard output for error messages
c...................................................................
      real      c(mxspm1),               d(mxspm1),
     :          e(mxspm1),               b(mxspm1),
     :          cv(0:mxspm1)
      integer   i,     info,     j,      n,     m,    stdout
      parameter ( stdout = 0)
c
      do 200 j = 1,  nint
         if(npts(j).le.1) then
            write(stdout,'(a)') 'CUSPLN : error defining  interface.'
            stop
         end if
c        Interface is a straight line.
         if(npts(j).eq.2) then
            a0(j,1) = (z(j,0)*x(j,1)-z(j,1)*x(j,0))/(x(j,1)-x(j,0))
            a1(j,1) = ( z(j,1) - z(j,0) ) / ( x(j,1) - x(j,0) )
            a2(j,1) = 0.
            a3(j,1) = 0.
            go to 200
         end if
c
c        evaluating bands of tridiagonal matrix, to be inverted
c        for the curvatures ( see reference, eqn. (142) )
         n = npts(j) - 2	!n is the number of unknown curvatures
         do 50  i = 1,  n	!diagonal
            d(i) = 2. * ( x(j,i+1) - x(j,i-1) )
50       continue
         do 60  i = 1,  n-1	!superdiagonal
            e(i) = x(j,i+1) - x(j,i)
60       continue
         do 70  i = 2,  n       !subdiagonal
            c(i) = x(j,i) - x(j,i-1)
70       continue
         do 80  i = 1,  n	!right hand side
            b(i) = 6. * ( (z(j,i+1)-z(j,i)) / (x(j,i+1)-x(j,i))
     :                - (z(j,i)-z(j,i-1)) / (x(j,i)-x(j,i-1)) )
80       continue
c
         call tridi(n,c,d,e,b,info)	!invert the matrix
         if(info.ne.0) then
            write(stdout,'(a)') 'CUSPLN : failed to fit spline.'
            stop
         end if
c        curvature at end points is set to zero
         cv(0) = 0.
         cv(n+1) = 0.
c        set the remaining curvatures found by tridi.
         do 90  i = 1, n
            cv(i) = b(i)
90       continue
c
c        using the curvatures, solve for the spline coefficients
c        here we have expanded the product terms in reference eqn. (132)
         do 100  m = 0,  n
            a0(j,m+1) = ( cv(m)*x(j,m+1)**3 - cv(m+1)*x(j,m)**3 
     :                + 6.*z(j,m)*x(j,m+1)
     :                - cv(m)*x(j,m+1)*(x(j,m+1)-x(j,m))**2
     :                - 6.*z(j,m+1)*x(j,m) + cv(m+1)*x(j,m)
     :                *(x(j,m+1)-x(j,m))**2)/(6.*(x(j,m+1)-x(j,m)))
            a1(j,m+1) = (-.5*cv(m)*x(j,m+1)**2+.5*cv(m+1)*x(j,m)**2
     :                -z(j,m)+(cv(m)*(x(j,m+1)-x(j,m))**2)/6.
     :		      +z(j,m+1)-(cv(m+1)*(x(j,m+1)-x(j,m))**2)/6.)
     :		      /(x(j,m+1)-x(j,m))
            a2(j,m+1) = (cv(m)*x(j,m+1)-cv(m+1)*x(j,m) )
     :                /(2.*(x(j,m+1)-x(j,m)))
            a3(j,m+1) = ( cv(m+1) - cv(m) ) / (6.*(x(j,m+1)-x(j,m)))
100      continue
200   continue
      return
      end
