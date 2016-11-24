
      subroutine rayone(xr,xs,zs,n,v,c,x,noconv)


c     This subroutine performs continuation in interfaces
c     Starting with a stratified earth, subroutine rayone deforms the
c     interfaces, using the continuation parameter alambd, until the
c     desired model is achieved.


      integer     n

      real        xr,        xs,           zs,
     :            v(n+1),    c(0:n+1),     x(0:n+1)
 
      logical     noconv


cc    local    variables
c     ALAMBD   continuation parameter
c     DELTAL   increment in continuation parameter
c     DXDL()   derivative of X w.r.t. ALAMBD
c     EPS      lower limit on DELTAL
c     FAIL     true when Newton's method breaks down
c     I        counts the number of Newton iterations
c     INFO     indicates if jacobian is singular
c     K        loop variable
c     MAXIT    maximum number of newton iterations at each
c              continuation step
c     MAXN     max allowed value of N
c     RESID    residual of the system of equations to be solved
c     SAVEX()  stores solution of last continuation step
c     SOLN     residual must fall below this for a solution
c     SPLNOK   indicates whether X is within range of splined interfaces



      integer   i,   info,   k,   maxit,    maxn

      parameter ( maxit = 6,
     :            maxn  = 50)


      real      savex(maxn+1),    dxdl(maxn),
     :          deltal,           eps,
     :          resid,            alambd,
     :          soln

      logical   fail,             splnok

      parameter ( eps  = 0.065)

 


c     Initialising some variables.
      noconv = .false.
      alambd = 0.0
      deltal = 1.0
      soln = v(1)*0.01


c     Finding the flat earth solution.
      call plasol(n,xr,xs,zs,c,v,x,noconv)
      if(noconv) return


c     Evaluate some frequently used quantities for a known solution.
      call eval(x,alambd,splnok)

c     Finding the derivative of x with respect to alambd.
50    call dxdlam(x,alambd,dxdl,info)

      if(info.ne.0) then
         noconv = .true.
         return
      end if

c     Save the value of X.  If Newton's method fails to converge
c     we will return to this value and reduce the continuation step.
      do 75  k = 1, n
         savex(k) = x(k)
75       continue


c     Calculate the first guess to be put into Newton's method.
      call guessx(x,n,deltal,dxdl)

c     Increment the continuation parameter.
      alambd = alambd + deltal


c     Start the Newton iterations.
c     After each iteration we check to see if the
c     iteration has been carried out successfully, and 
c     if so, check to see if it has found a solution.

      i = 0

      call newton(x,n,alambd,resid,fail)

100   if(fail.or.resid.gt.soln) then

         i = i + 1

         if(.not.fail.and.i.lt.maxit) then

            call newton(x,n,alambd,resid,fail)

         else

c           Newton has failed.  Reduce the continuation step.
            alambd = alambd - deltal
            deltal = deltal / 2.0

c           If deltal is not too small, return to the previous 
c           solution and proceed with the continuation.
            if(deltal.gt.eps) then

               do 150  k = 1,  n
                  x(k) = savex(k)
150               continue

               call guessx(x,n,deltal,dxdl)

               alambd = alambd + deltal
c              reset the iteration counter
               i = 0

               call newton(x,n,alambd,resid,fail)

            else

c              The continuation procedure has failed.
               noconv = .true.

               return

            end if

         end if
         

         go to 100

      end if

c     Continuation is not done until ALAMBD = 1.
      if(alambd.lt.1.0) go to 50

      return

      end


c--------------------------------------------------------------------

      subroutine reccon(x,n,xr,geoinc,v1,noconv)



c     This subroutine performs receiver continuation.
c     Using a ray from some source to receiver, subroutine reccon
c     finds the ray from that same source position to an adjacent
c     receiver. The continuation parameter is rlambd.


      integer    n

      real       x(0:n+1),    xr,    geoinc,      v1

      logical    noconv



cc    local    variables
c     DELTAL   increment in continuation parameter
c     DXDL()   derivative of X r.r.t. RLAMBD
c     EPS      lower limit on DELTAL
c     FAIL     true when Newton's method breaks down
c     I        counts the number of Newton iterations
c     INFO     indicates if jacobian is singular
c     K        loop variable
c     MAXIT    maximum number of newton iterations at each
c              continuation step
c     MAXN     max allowed value of N
c     RESID    residual of the system of equations to be solved
c     RLAMBD   continuation parameter
c     SAVEX()  stores solution of last continuation step
c     SOLN     residual must fall below this for a solution


      integer   i,   info,   k,   maxit,    maxn

      parameter ( maxit = 4,
     :            maxn  = 50)


      real      savex(maxn+1),    dxdl(maxn),
     :          deltal,           eps,
     :          resid,            rlambd,
     :          soln

      logical   fail

      parameter ( eps  = 0.065)


c     Initialise some variables.
      noconv = .false.
      deltal = 1.0
      rlambd = 0.0
      soln = v1*0.01


c     Finding the derivative of x with respect to rlambd.
50    call dxdrl(geoinc,dxdl,info)
      if(info.ne.0) then
         noconv = .true.
         return
      end if

c     Save the value of X.  If Newton's method fails to converge
c     we will return to this value and reduce the continuation step.
      do 75  k = 1, n
         savex(k) = x(k)
75       continue

c     Calculate the first guess to put in Newton's method.
      call guessx(x,n,deltal,dxdl)

c     Increment the continuation parameter and the end point of the ray.
      rlambd = rlambd + deltal
      x(0) = xr + geoinc * rlambd

c     Start the Newton iterations.
c     After each iteration we check to see if the
c     iteration has been carried out successfully, and
c     if so, check to see if it has found a solution.

      i = 0

c     Since the interfaces are now fixed in their true positions,
c     we set ALAMBD equal to one.
      call newton(x,n,1.,resid,fail)

100   if(fail.or.resid.gt.soln) then

         i = i + 1

         if(.not.fail.and.i.lt.maxit) then

            call newton(x,n,1.,resid,fail)

         else

c           Newton has failed.  Reduce the continuation step.
            rlambd = rlambd - deltal
            deltal = deltal / 2.0

c           If deltal is not too small, return to the previous
c           solution and proceed with the continuation.
            if(deltal.gt.eps) then

               do 150 k = 1,  n
                  x(k) = savex(k)
150               continue

               call guessx(x,n,deltal,dxdl)

               rlambd = rlambd + deltal
               x(0) = xr + geoinc * rlambd
c              reset the iteration counter
               i = 0

               call newton(x,n,1.,resid,fail)

            else

c              The continuation procedure has failed.
               noconv = .true.
               return

            end if

         end if

         go to 100

      end if

c     Continuation is not done until RLAMBD = 1.
      if(rlambd.lt.1.0) go to 50

c     Updating the receiver position

      xr = xr + geoinc

      return

      end


c---------------------------------------------------------------------


      subroutine plasol(n,xr,xs,zs,c,v,x,noconv)

c     Plasol finds the coordinates of the ray in the stratified
c     medium. it follows the procedure outlined in appendix a of
c     John Fawcett's thesis ( 3D Ray Tracing and Ray inversion
c     in Layered Media, Caltech, 1983).


      integer     n

      real        xr,      xs,       zs,    c(0:n+1),
     :            v(n+1),  x(0:n+1)

      logical noconv


cc    Local    variables
c     A()      ratio of layer velocity to velocity in layer one
c     AMIN     largest value of sin(takeoff angle) - one over
c              maximum value of A.
c     CLOSE    a measure of how close ray is to receiver
c     DELTAX   change in sin(takeoff angle)- see reference above
c     DFDX     derivative of x distance travelled
c     DIST     distance between source and end of ray
c     FX       function that calculates the distance travelled (DIST)
c              by the ray
c     IBIS     counts number of bisections
c     I        loop variable
c     K        loop variable
c     INEWT    counts number of newton iterations
c     IBIS     counts number of bisections
c     INTVAL   number of intervals to divide range of values 
c              of sine of takeoff angle
c     MAXBIS   maximum allowed number of bisections
c     MAXNP1   maximum value of N plus one
c     MAXNWT   maximum allowed number of newton iterations
c     NP1      N plus one
c     OFFSET   distance (positive) between source and receiver
c     SIGN     + or - one, depending on receiver location
c     THICK()  thickness of layers
c     VCLOSE   if the ray is this close to the receiver then
c              we've found the solution.
c     VMAX     maximum layer velocity
c     X1,X2    values of sin(takeoff angle) used in bisection
c     XNEW     next value to use in bisection or newton iteration


      integer  maxnp1

      parameter( maxnp1 = 51)

      real     a(maxnp1),    thick(maxnp1)

      real     fx

      real     amin,         close,     deltax,
     :         dfdx,         dist,      offset,     sign,
     :         vclose,       vmax,
     :         x1,           x2,        xnew


      parameter (  close = 10.,
     :            vclose = 1. )


      integer  i,            inewt,     intval,     maxbis,
     :         maxnwt,       ibis,      np1,        k


      parameter ( maxbis = 100,
     :            maxnwt  = 100,
     :            intval = 10 )







      noconv = .false.

      np1 = n + 1

c     initialize the iteration counters
      ibis = 0
      inewt = 0


c     The depths of the interfaces at the x coordinate of the source
c     are supplied by the main program. C(1) is the shallowest, etc.

c     Calculate the layer thicknesses from depths.
      thick(1) = c(1)

      do 30 i = 2, n
         thick(i) = c(i) - c(i-1)
30       continue

c     Bottom of last layer set at depth of source.
      thick(np1) = zs - c(n)



c     If receiver is above source then all x coordinates are equal.
      if(xr.eq.xs) then
         do 40  i = 0,  np1
            x(i) = xr
40          continue
         return
      end if


c     Setting alpha (in Fawcett's thesis).
      a(1) = 1.
      do 50 i = 2,  np1
         a(i) = v(i) / v(1)
50       continue

c     Find the maximum velocity.
      vmax = v(1)
      do 60 i = 2,  np1
         if(vmax.lt.v(i)) then
            vmax = v(i)
         end if
60       continue

c     Setting minimum of 1 / alpha
      amin = v(1) / vmax

      offset = abs( xs - xr )


c     Divide up the interval.
      deltax = amin / intval

c     Find part of interval on which solution lies.
      x2 = deltax
      dist = fx(x2,a,thick,np1)
      i = 2
80    if(dist.lt.offset.and.i.lt.intval) then    
         x2 = x2 + deltax
         dist = fx(x2,a,thick,np1)
         i  = i + 1
         go to 80
      end if

      if(dist.lt.offset) then
c        X lies inside last interval.
         x2 = .9999 * amin
         dist = fx(x2,a,thick,np1)
         if(dist.lt.offset) then
c           ray is too close to grazing
            noconv = .true.
            return
         end if
      end if

      x1 = ( i - 2 ) * deltax


c     Use bisection to get close.
      if(abs(dist-offset).lt.close) then
      else
         xnew =  ( x2 + x1 ) / 2.
         dist = fx(xnew,a,thick,np1)
100      if(abs(dist-offset).lt.close) then
         else
            ibis = ibis + 1
            if(ibis.gt.maxbis) then
               noconv = .true.
               return
            end if

            if((dist-offset).lt.0.) then
               x1 = xnew
            else
               x2 = xnew
            end if
            xnew =  ( x2 + x1 ) / 2.
            dist = fx(xnew,a,thick,np1)
            go to 100
        end if

      end if


c     Use newton's method to get very close.
140   if(abs(dist-offset).le.vclose) then
      else

         inewt = inewt + 1
         if(inewt.gt.maxnwt) then
            noconv = .true.
            return
         end if

         dfdx = 0.0
         do 150 k = 1,  np1
            dfdx = dfdx + thick(k) * a(k) / 
     :      ( 1 - ( a(k) * xnew )**2 )**1.5
150         continue

         xnew = xnew - ( dist - offset ) / dfdx
         do 160 k = 1,  np1
            if(abs(a(k)*xnew).ge.1.0) then
c              Newton's method can be unpredictable.
c              If the above product is greater than 1., we
c              get into trouble with square roots below.
               noconv = .true.
               return
            end if
160         continue

         dist = fx(xnew,a,thick,np1)

         go to 140

      end if



      if((xs-xr).lt.0.) then 
         sign = - 1.0
      else
         sign = 1.0
      end if

      x(1) = xr + sign * thick(1) * a(1) * xnew /
     :       sqrt( 1. - ( a(1) * xnew )**2 )

      do 200 i = 2,  n
         x(i) = x(i-1) + sign * thick(i) * a(i) * xnew /
     :          sqrt( 1. - ( a(i) * xnew )**2 )
200      continue


      x(np1) = xs
      x(0)   = xr


      return

      end



      real function fx(x,a,thick,np1)

c     Calculates distance travelled by ray (see Fawcett eqn. (A-3)).

      integer  np1

      real     x,    a(np1),     thick(np1)

cc    local    variables
c     j        loop variable

      integer  j
      

      fx = 0.0
      do 500 j = 1,  np1
         fx = fx + thick(j) * a(j) * x /
     :   sqrt( 1.- a(j) * a(j) * x * x )
500      continue

      return

      end

c------------------------------------------------------------------------

      subroutine guessx(x,n,deltal,dxdl)


c     Calculates the initial value of x to be used in newton's method.
c     See reference, eqn. (9).


      integer   n

      real      x(0:n+1),      dxdl(n),      deltal



cc    local variables
c     K     loop variable

      integer   k


      do 100  k = 1,  n
         x(k) = x(k) + deltal * dxdl(k)
100      continue


      return

      end


c---------------------------------------------------------------------



      subroutine newton(x,n,alambd,resid,fail)

c     Carries out the newton iteration for the continuation
c     procedures RAYONE and RECCON.


      integer    n

      real       x(0:n+1),    alambd,     resid

      logical    fail


cc    local    variables
c     DJ()     diagonal of the tridiagonal jacobian matrix
c     INFO     tells us if the jacobian is singular
c     K        loop variable
c     MAXN     max value of N
c     PHI()    system of equations to be solved
c     SPLNOK   tells us if X lies within range of the interfaces
c     SUBDJ()  subdiagonal of tridiagonal jacobian matrix
c     SUM      sum of squares of PHI
c     SUPDJ    superdiagonal of tridiagonal jacobian matrix


      integer   k,     maxn,     info

      parameter ( maxn = 50)

      real      subdj(maxn),    dj(maxn),
     :          supdj(maxn),    phi(maxn),
     :          sum

      logical splnok

      
      fail = .false.

c     Evaluate frequently used quantities.
      call eval(x,alambd,splnok)

c     If X outside range of splined interfaces, return.
      if(.not.splnok) then
         fail = .true.
         return
      end if

c     Evaluate the elements of the tridiagonal jacobian.
      call jacob(alambd,subdj,dj,supdj)

c     Evaluate phi.
      call dophi(phi,alambd)


c     Do the Newton iteration.

      if(n.eq.1) then
          x(1) = x(1) - phi(1) / dj(1)
      else
          call tridi(n,subdj,dj,supdj,phi,info)

          if(info.ne.0) then
             fail = .true.
             return
          end if

c         Tridi solves the matrix equation jx = phi for x.
c         The solution, x, is returned as the vector phi .

          do 100  k = 1,  n
             x(k) = x(k) - phi(k)
100          continue

      end if


c     Evaluate necessary quantities with new value of X
c     from the Newton iteration.
      call eval(x,alambd,splnok)

c     New X has to lie within range of splined interfaces.
      if(.not.splnok) then
         fail = .true.
         return
      end if

c     Evaluating PHI again.
      call dophi(phi,alambd)

c     Finding the l2 norm of phi.
      sum = 0.0

      do 200  k = 1,  n
         sum = sum + phi(k)**2
200      continue

      resid = sqrt( sum )


      return

      end


c--------------------------------------------------------------------


      subroutine eval(x,alambd,splnok)

c     Evaluates quantities used frequently by other subroutines.



      real        x(0:*),      alambd

      logical     splnok

      integer     maxn,        maxnp1,       maxnp3,
     :            maxspl,      mxspm1

      parameter ( maxn   = 50,
     :            maxspl = 151)

      parameter ( maxnp1 = maxn + 1,
     :            maxnp3 = maxn + 3,
     :            mxspm1 = maxspl - 1)


      real        xint(maxn,maxspl),        zint(maxn,maxspl),
     :            a0(maxn,mxspm1),          a1(maxn,mxspm1),
     :            a2(maxn,mxspm1),          a3(maxn,mxspm1)

      integer     npts(maxn),    nint

      common /a/  xint,          zint,      npts,       nint,
     :            a0,            a1,        a2,         a3


      real        f(0:maxnp1),     df(maxn),      ddf(maxn),
     :            c(0:maxnp1),     d(maxnp1),
     :            deltac(maxnp1),  deltaf(maxnp1),
     :            deltax(maxnp1),  deltaz(maxnp1),
     :            v(maxnp1)

      integer     n


      common /b/  f,               df,            ddf,
     :            c,               d,
     :            deltac,          deltaf,
     :            deltax,          deltaz,
     :            v,
     :            n
      common /coor/cx,cy

cc    local variables
c     I     loop variable over intersection points
c     K     loop variable over intersection points
c     J     identifies section of interface

      integer     i,               j,            k



      splnok = .true.


c     If x falls outside the range of definition of the splined
c     interface, then return.

      do 3 i = 1,  n
         if(x(i).lt.xint(i,1).or.x(i).ge.xint(i,npts(i))) then
            splnok = .false.
            return
         end if
3        continue


c     Finding the section of the spline on which x lies and evaluating
c     the function and derivatives.

      do 10 i = 1,  n
         j = 1
5        if(x(i).ge.xint(i,j)) then
            j = j + 1
            go to 5
         end if
         j = j - 1

         f(i)   = a0(i,j) - c(i) + a1(i,j) * x(i) + a2(i,j) * x(i)**2
     $          + a3(i,j) * x(i)**3

         df(i)  = a1(i,j)  +   2.* a2(i,j) * x(i) + 3.*a3(i,j) * x(i)**2
         ddf(i) =              2.* a2(i,j)        + 6.*a3(i,j) * x(i)

10       continue




      do 100 k = 1,  n + 1

         deltax(k) = x(k) - x(k-1)
         deltaf(k) = f(k) - f(k-1)
         deltaz(k) = deltac(k) + alambd * deltaf(k)
         d(k) = sqrt( deltax(k)**2 + deltaz(k)**2 )

100      continue


      return


      end


c---------------------------------------------------------------------


      subroutine strat(xs,c,exit)

c     Each time the source is moved it is necessary to 
c     calculate the depths of the interfaces in the horizontally
c     stratified model.


      real        xs,          c(0:*)

      logical     exit


      integer     maxn,        maxspl,      mxspm1

      parameter ( maxn   = 50,
     :            maxspl = 151)

      parameter ( mxspm1 = maxspl - 1)


      real        xint(maxn,maxspl),        zint(maxn,maxspl),
     :            a0(maxn,mxspm1),          a1(maxn,mxspm1),
     :            a2(maxn,mxspm1),          a3(maxn,mxspm1)

      integer     npts(maxn),    nint

      common /a/  xint,          zint,      npts,       nint,
     :            a0,            a1,        a2,         a3


cc    local   variables
c     I       loop variable over interfaces
c     J       identifies section of interface

      integer    i,             j




c     Finding the depth of each interface at the
c     x coordinate of the source.  The shallowest depth is c(1)
c     and c(nint) is the deepest. These depths will be used 
c     in the stratified model.


      do 10 i = 1,  nint

         if(xs.lt.xint(i,1).or.xs.ge.xint(i,npts(i))) then

            exit = .true.
            return
         end if

         j = 1
5        if(xs.ge.xint(i,j)) then
            j = j + 1
            go to 5
         end if
         j = j - 1

         c(i) = a0(i,j) + a1(i,j) * xs + a2(i,j) * xs**2
     $            + a3(i,j) * xs**3

10       continue


      return

      end

c---------------------------------------------------------------------


      subroutine setn(zs,c,nint,n,znear,deltac,f)


c     The number of interfaces between source and receiver
c     may change when the depth of the source changes,
c     ( ie the source passes through an interface ).
c     Setn finds the number of interfaces between source
c     and receiver, and evaluates a few other things
c     that remain constant until the source is moved again.


      integer    n,          nint

      real       c(0:n+1),   f(0:n+1),     deltac(n+1),
     :           zs


cc    local   variables
c     I       loop variable
c     ZNEAR   If the source point is below the interface but less
c             than ZNEAR vertical feet from it, then the interface
c             is ignored.

      integer    i

      real       znear     



c     Calculating the number of interfaces between source and receiver
c     for the direct ray.  If the source is on or just below the
c     interface then the interface is ignored.

      do 20 i = 1,  nint
         if(zs.le.(c(i)+znear)) then
            n = i - 1
            go to 25
         end if
20       continue

      n = nint
25    continue



c     Setting values at the end of the ray.
      f(n+1) = 0.
      deltac(n+1) = zs - c(n)

      do 40 i = 1,  n
         deltac(i) = c(i) - c(i-1)
40       continue



50    return

      end

c---------------------------------------------------------------------

      subroutine jacob(alambd,subdj,dj,supdj)

c     Calculates the elements of the jacobian (dphi / dx ).
c     Note that this matrix is tridiagonal.
c     The jacobian is inverted in the Newton iteration and
c     in calculating dX/drlambd and dX/dalambd.


      real        alambd,      subdj(*),     dj(*),    supdj(*)


      integer     maxn,        maxnp1,       maxnp3

      parameter ( maxn   = 50)

      parameter ( maxnp1 = maxn + 1,
     :            maxnp3 = maxn + 3)

      real        f(0:maxnp1),     df(maxn),      ddf(maxn),
     :            c(0:maxnp1),     d(maxnp1),
     :            deltac(maxnp1),  deltaf(maxnp1),
     :            deltax(maxnp1),  deltaz(maxnp1),
     :            v(maxnp1)

      integer     n


      common /b/  f,               df,            ddf,
     :            c,               d,
     :            deltac,          deltaf,
     :            deltax,          deltaz,
     :            v,
     :            n



cc    local variables
c     K     loop variable

      integer     k



c     Calculating the diagonal.

      do 100  k = 1, n

         dj(k) = v(k+1) * ( 1.0 + ( alambd * df(k) )**2 +
     $    alambd * deltaz(k) * ddf(k) - ( ( deltax(k) + alambd * df(k)
     $    * deltaz(k) ) / d(k) )**2 ) / d(k)
     $    + v(k) * ( 1.0 + ( alambd * df(k) )**2 - alambd * deltaz(k+1)
     $    * ddf(k) - ( ( deltax(k+1) + alambd * df(k) * deltaz(k+1) ) /
     $    d(k+1) )**2 ) / d(k+1)

100      continue


      if(n.eq.1) go to 500


c     Calculating the subdiagonal

      do 200  k = 2, n

         subdj(k) = - v(k+1) * ( 1.0 + ( alambd**2 ) * df(k) *
     $    df(k-1) - ( deltax(k) + alambd * df(k) * deltaz(k) ) *
     $    ( deltax(k) + alambd * df(k-1) * deltaz(k) ) / d(k)**2 )
     $    / d(k)

200      continue


c     Calculating the superdiagonal

      do 300  k = 1, n-1

         supdj(k) = - v(k) * ( 1.0 + ( alambd**2 ) * df(k) * df(k+1)
     $    - ( deltax(k+1) + alambd * df(k) * deltaz(k+1) ) *
     $    ( deltax(k+1) + alambd * df(k+1) * deltaz(k+1) ) /
     $    d(k+1)**2 ) / d(k+1)

300      continue



500   return


      end


c--------------------------------------------------------------------


      subroutine dophi(phi,alambd)

c     Evaluates the system of equations, Phi.
c     For X to be a solution ( a raypath ), PHI must be
c     zero ( actually, its residual must be close to zero ).


      real        phi(*),      alambd


      integer     maxn,        maxnp1,       maxnp3

      parameter ( maxn   = 50)

      parameter ( maxnp1 = maxn + 1,
     :            maxnp3 = maxn + 3)

      real        f(0:maxnp1),     df(maxn),      ddf(maxn),
     :            c(0:maxnp1),     d(maxnp1),
     :            deltac(maxnp1),  deltaf(maxnp1),
     :            deltax(maxnp1),  deltaz(maxnp1),
     :            v(maxnp1)

      integer     n


      common /b/  f,               df,            ddf,
     :            c,               d,
     :            deltac,          deltaf,
     :            deltax,          deltaz,
     :            v,
     :            n


cc    local  variables
c     K      loop variable

      integer     k



      do 100  k = 1, n

         phi(k) = v(k+1) * ( deltax(k) + alambd * df(k) * deltaz(k) ) /
     $    d(k) - v(k) * (deltax(k+1) + alambd * df(k) * deltaz(k+1) ) /
     $    d(k+1)

100      continue


      return

      end


c----------------------------------------------------------------------
c x is an argument that is not really used, as far I can see.  jkc

      subroutine dxdlam(x,alambd,dxdl,info)

c     Calculates the derivative of x with respect to alambd,
c     used in finding the first ray.


      real        x(0:*),  alambd,       dxdl(*)


      integer     info


      integer     maxn,        maxnp1

      parameter ( maxn   = 50)

      parameter ( maxnp1 = maxn + 1)

      real        f(0:maxnp1),     df(maxn),      ddf(maxn),
     :            c(0:maxnp1),     d(maxnp1),
     :            deltac(maxnp1),  deltaf(maxnp1),
     :            deltax(maxnp1),  deltaz(maxnp1),
     :            v(maxnp1)

      integer     n


      common /b/  f,               df,            ddf,
     :            c,               d,
     :            deltac,          deltaf,
     :            deltax,          deltaz,
     :            v,
     :            n



cc    local    variables
c     DJ()     diagonal of the tridiagonal jacobian matrix
c     DPHIDL() derivative of phi w.r.t. ALAMBD
c     K        loop variable
c     SUBDJ()  subdiagonal of tridiagonal jacobian matrix
c     SUPDJ    superdiagonal of tridiagonal jacobian matrix


      real        dj(maxn),        dphidl(maxn),
     :            subdj(maxn),     supdj(maxn)


      integer     k





c     Evaluate the jacobian matrix.
      call jacob(alambd,subdj,dj,supdj)

c     Evaluate the derivative with respect to ALAMBD.

      do 325 k = 1, n

         dphidl(k) = v(k+1) * ( df(k) * ( deltaz(k) + alambd*deltaf(k) )
     :    - ( deltaz(k) / d(k)**2 ) * ( deltax(k) + alambd * df(k) *
     :    deltaz(k) ) * deltaf(k) ) / d(k)
     :    - v(k) * ( df(k) * ( deltaz(k+1) + alambd * deltaf(k+1) ) -
     :    ( deltaz(k+1) / d(k+1)**2 ) * ( deltax(k+1) + alambd * df(k) *
     :    deltaz(k+1) ) * deltaf(k+1) ) / d(k+1)

325      continue


      if(n.eq.1) then
         dxdl(1) = - dphidl(1) / dj(1)
      else
         call tridi(n,subdj,dj,supdj,dphidl,info)

         if(info.ne.0) return

c        tridi returns the value of -dxdl in the variable dphidl.

         do 350  k = 1, n
            dxdl(k) = - dphidl(k)
350         continue

      end if


      return


      end


c------------------------------------------------------------------


      subroutine dxdrl(geoinc,dxdl,info)

c     Finds the derivative of x with respect to rlambd,
c     used in receiver continuation.


      real        geoinc,      dxdl(*)

      integer     info

      integer     maxn,        maxnp1,       maxnp3

      parameter ( maxn   = 50)

      parameter ( maxnp1 = maxn + 1,
     :            maxnp3 = maxn + 3)

      real        f(0:maxnp1),     df(maxn),      ddf(maxn),
     :            c(0:maxnp1),     d(maxnp1),
     :            deltac(maxnp1),  deltaf(maxnp1),
     :            deltax(maxnp1),  deltaz(maxnp1),
     :            v(maxnp1)

      integer     n


      common /b/  f,               df,            ddf,
     :            c,               d,
     :            deltac,          deltaf,
     :            deltax,          deltaz,
     :            v,
     :            n


cc    local    variables
c     DJ()     diagonal of the tridiagonal jacobian matrix
c     DPHIDR() derivative of PHI w.r.t. x-coordinate of receiver
c     K        loop variable
c     SUBDJ()  subdiagonal of tridiagonal jacobian matrix
c     SUPDJ    superdiagonal of tridiagonal jacobian matrix



      real       dj(maxn),        dphidr(maxn),
     :           subdj(maxn),     supdj(maxn)

      integer    k



c     Evaluste the jacobian matrix.
      call jacob(1.,subdj,dj,supdj)


c     Only PHI(1) is a function of the receiver location, X(0).
c     Also including here the receiver spacing (reference, eqn.(15)).

      dphidr(1) = - v(2) * ( 1.0 - ( deltax(1) / d(1) )**2 -
     $ ( df(1) * deltax(1) * deltaz(1) ) / d(1)**2 ) *
     $ geoinc / d(1)


      if(n.eq.1) then
         dxdl(1) = - dphidr(1) / dj(1)
      else
         do 325  k = 2, n
            dphidr(k) = 0.0
325         continue


         call tridi(n,subdj,dj,supdj,dphidr,info)
         if(info.ne.0) return

c        tridi returns the value of -dxdl as dphidr.
         do 350  k = 1, n
            dxdl(k) = - dphidr(k)
350         continue

      end if


      return

      end



c---------------------------------------------------------------------


      subroutine ttime(d,v,n,time)

c     Calculates the traveltime along a ray.

      integer    n

      real       d(n+1),      v(n+1),       time

cc    local  variables
c     K      loop variable

      integer    k

      time = 0.0

      do 100 k = 1, n + 1
         time  = time  + d(k) / v(k)
100      continue



      return

      end


c----------------------------------------------------------------------

      subroutine amplit(sinmax,amp,sigma,dxdb,icontr)

c     Calculates the amplitude weighting factor to be applied to
c     the data, given the raypath.  The amplitude factor is made up
c     of transmission coefficients, change in area of ray tube at each
c     interface, emergence angle of ray, and the ray parameter sigma.

      real        sinmax,      amp,          sins,       dxdb
      integer     maxn,        maxnp1,       maxnp3
      parameter ( maxn   = 50)
      parameter ( maxnp1 = maxn + 1,
     :            maxnp3 = maxn + 3)
      real        f(0:maxnp1),     df(maxn),      ddf(maxn),
     :            c(0:maxnp1),     d(maxnp1),
     :            deltac(maxnp1),  deltaf(maxnp1),
     :            deltax(maxnp1),  deltaz(maxnp1),
     :            v(maxnp1)
      integer     n,        icontr

      common /b/  f,               df,            ddf,
     :            c,               d,
     :            deltac,          deltaf,
     :            deltax,          deltaz,
     :            v,
     :            n

cc    local     variables
c     ARG       argument of square root used in the expression
c               for the transmission coefficient
c     COSBF     cosine of emergence angle of ray and the vertical
c     COSINC    cosine of angle between incident ray and normal
c     COSTRA    cosine of angle between transmitted ray and normal
c     K         loop variable
c     SIGMA     running parameter along the ray
c     TRANS     product of transmission coefficients
c     VALNOR    magnitude of normal to interface
c     XSECTN    factor due to changes in cross-sectional area
c               of ray tube at interfaces
c     DAB, DFB, P    aux. varibles to compute geometry sperading factor

      real        arg,           cosbf,
     :            cosinc,        costra,
     :            sigma,         trans,
     :            valnor,        xsectn,
     :            dab,           dfb,             p

      integer     k


c     Initialising
      trans  = 1.
      xsectn = 1.

      if(icontr.eq.1)then

c     Checking for aliasing.
c     Only true for rays to receivers.

         if(abs(deltax(1)/d(1)).gt.sinmax) then
            amp = 0.
            return
         end if
      end if

      if(n.eq.0) then
      else
c        Find transmission coefficient at each
c        intersection point.

         do 10  k = 1,  n

c           Take scalar product of ray with normal to get
c           cosine of angle between.
  
            valnor = sqrt( 1. + df(k)**2 )  
            costra = abs( ( deltax(k) * df(k) - deltaz(k) )
     :                 / ( d(k) * valnor ) )
            cosinc = abs( ( deltax(k+1) * df(k) - deltaz(k+1) )
     :                 / ( d(k+1) * valnor ) )



            if(cosinc.lt.0.1.or.costra.lt.0.1) then
               amp = 0.
               return
            end if
            arg = ( v(k+1) / v(k) )**2 - 1. + cosinc**2  

            if(arg.lt.0.) then
c              no transmitted ray at this angle of incidence
               amp = 0.
               return
            else
               trans = trans * 2. * cosinc /
     :                 ( cosinc + sqrt(arg) )
            end if

            xsectn = xsectn * costra / cosinc

10          continue


      end if


      if(n.eq.0) then
         dxdb = - d(1) * d(1) / deltaz(1)
      else

         dxdb = - (n+1)**2 / ( deltaz(n+1) - df(n) * deltax(n+1) )
         dab = 1.

         do 20  k = n,  1,  -1

            valnor = sqrt( 1. + df(k)**2 )  
            costra = abs( ( deltax(k) * df(k) - deltaz(k) )
     :                 / ( d(k) * valnor ) )
            cosinc = abs( ( deltax(k+1) * df(k) - deltaz(k+1) )
     :                 / ( d(k+1) * valnor ) )

         dfb = -ddf(k) * dxdb / valnor**2
         dab = dfb + v(k) / v(k+1) * cosinc / costra * (dab - dfb)

         if(k.eq.1)then
           p = 1.
         else
           p = 1. - df(k-1) * deltax(k) / deltaz(k)
           if(p.eq.0) then
             amp = 0.
             return
           end if
         end if

         dxdb = ( -d(k)**2 / deltaz(k) * dab + ( 1 - df(k) * 
     :      deltax(k) / deltaz(k) ) * dxdb ) / p


20          continue


      end if


c     Calculating sigma

      sigma = 0.

      do 100  k = 1,  n + 1
         sigma = sigma + d(k) * v(k)
100      continue



c     Need the cosine of the emergence angle ray makes with vertical.
      cosbf = abs( deltaz(1) / d(1) )


c     Do the amplitude calculation for this ray.

      amp = sqrt(cosbf) / 
     :      ( sqrt(v(n+1) * xsectn ) * trans)


      return

      end

c-----------------------------------------------------------------

      subroutine cuspln(nint,x,z,npts,a0,a1,a2,a3)


c     This subroutine fits a cubic spline through the points
c     defining each interface.  The curvatures at each end are
c     taken to be zero.  The remaining curvatures are found by
c     inverting a tridiagonal matrix.  Knowing the curvatures we
c     evaluate the spline coefficients.
c     Reference: Numerical Computing and Mathematical Analysis
c                by Stephen M. Pizer.  Science Research Associates, Inc.
c                Pages 307-311.



      integer     maxn,                    mxspm1

c     MXSPM1 should equal MAXSPL in main, less one
c     MAXN   is maximum value of N
      parameter ( maxn   = 50,
     :            mxspm1 = 150)

      real        x(maxn,0:mxspm1),        z(maxn,0:mxspm1),
     :            a0(maxn,mxspm1),         a1(maxn,mxspm1),
     :            a2(maxn,mxspm1),         a3(maxn,mxspm1)

      integer     nint,                    npts(nint)




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




      real      c(mxspm1),               d(mxspm1),
     :          e(mxspm1),               b(mxspm1),
     :          cv(0:mxspm1)

      integer   i,     info,     j,      n,     m,    stdout

      parameter ( stdout = 0)



      do 200 j = 1,  nint


         if(npts(j).le.1) then
            write(stdout,'(a)') 'CUSPLN : error defining  interface.'
            stop
         end if

         if(npts(j).eq.2) then

c           Interface is a straight line.

            a0(j,1) = ( z(j,0)*x(j,1) - z(j,1)*x(j,0) ) /
     :                                   ( x(j,1) - x(j,0) )
            a1(j,1) = ( z(j,1) - z(j,0) ) / ( x(j,1) - x(j,0) )
            a2(j,1) = 0.
            a3(j,1) = 0.

            go to 200

         end if

c        evaluating bands of tridiagonal matrix, to be inverted
c        for the curvatures ( see reference, eqn. (142) )

c        n is the number of unknown curvatures
         n = npts(j) - 2

c        diagonal
         do 50  i = 1,  n
            d(i) = 2. * ( x(j,i+1) - x(j,i-1) )
50          continue

c        superdiagonal
         do 60  i = 1,  n-1
            e(i) = x(j,i+1) - x(j,i)
60          continue

c        subdiagonal
         do 70  i = 2,  n
            c(i) = x(j,i) - x(j,i-1)
70          continue

c        right hand side
         do 80  i = 1,  n
            b(i) = 6. * ( (z(j,i+1)-z(j,i)) / (x(j,i+1)-x(j,i))
     :                - (z(j,i)-z(j,i-1)) / (x(j,i)-x(j,i-1)) )
80          continue



c        invert the matrix
         call tridi(n,c,d,e,b,info)
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
90          continue




c        using the curvatures, solve for the spline coefficients
c        here we have expanded the product terms in reference eqn. (132)

         do 100  m = 0,  n


            a0(j,m+1) = ( cv(m)*x(j,m+1)**3 - cv(m+1)*x(j,m)**3 
     :                + 6.*z(j,m)*x(j,m+1)
     :                - cv(m)*x(j,m+1)*(x(j,m+1)-x(j,m))**2
     :                - 6.*z(j,m+1)*x(j,m) + cv(m+1)*x(j,m)
     :             *(x(j,m+1)-x(j,m))**2 ) / ( 6.*(x(j,m+1)-x(j,m)) )



            a1(j,m+1) = (-.5*cv(m)*x(j,m+1)**2 + .5*cv(m+1)*x(j,m)**2
     :          -z(j,m) + (cv(m)*(x(j,m+1)-x(j,m))**2)/6. + z(j,m+1)
     :          -(cv(m+1)*(x(j,m+1)-x(j,m))**2)/6.)/(x(j,m+1)-x(j,m))


            a2(j,m+1) = (cv(m)*x(j,m+1)-cv(m+1)*x(j,m) )
     :                           /(2.*(x(j,m+1)-x(j,m)))


            a3(j,m+1) = ( cv(m+1) - cv(m) ) / (6.*(x(j,m+1)-x(j,m)))



100         continue



200      continue


      return

      end

*-------------------------------------------------------------------


      SUBROUTINE TRIDI(N,C,D,E,B,info)

c     Tridi solves the equation JX = B for X.  J is the tridiagonal
c     jacobian whose bands here are C (subdiagonal), D (diagonal),
c     and E (superdiagonal). The solution is returned as B.
c     The code is from a LINPAK listing.

c     REFERENCE : LINPACK USER'S GUIDE,  J.J. DONGARRA et al,
c                 SIAM, 1979.


      integer    n,      info

      real       c(n),   d(n),    e(n),    b(n)


      integer    k,      kb,      kp1,     nm1,     nm2

      real       t




      INFO = 0
      C(1) = D(1)
      NM1 = N - 1
      IF(NM1.LT.1) GO TO 40
         D(1) = E(1)
         E(1) = 0.0E0
         E(N) = 0.0E0

         DO 30  K = 1,  NM1
            KP1 = K + 1
            IF(ABS(C(KP1)).LT.ABS(C(K))) GO TO 10
               T = C(KP1)
               C(KP1) = C(K)
               C(K) = T
               T = D(KP1)
               D(KP1) = D(K)
               D(K) = T
               T = E(KP1)
               E(KP1) = E(K)
               E(K) = T
               T = B(KP1)
               B(KP1) = B(K)
               B(K) = T
10          CONTINUE


            IF(C(K).NE.0.0E0) GO TO 20
               INFO = K
               GO TO 100
20          CONTINUE

            T = -C(KP1)/C(K)
            C(KP1) = D(KP1) + T*D(K)
            D(KP1) = E(KP1) + T*E(K)
            E(KP1) = 0.0E0
            B(KP1) = B(KP1) + T*B(K)
30          CONTINUE
40    CONTINUE

      IF(C(N).NE.0.0E0) GO TO 50
         INFO = N
         GO TO 90
50    CONTINUE

      NM2 = N - 2
      B(N) = B(N)/C(N)
      IF(N.EQ.1) GO TO 80
         B(NM1) = (B(NM1) - D(NM1)*B(N))/C(NM1)
         IF(NM2.LT.1) GO TO 70
            DO 60  KB = 1,  NM2
               K = NM2 - KB + 1
               B(K) = (B(K) - D(K)*B(K+1) - E(K)*B(K+2))/C(K)
60             CONTINUE
70       CONTINUE
80    CONTINUE
90    CONTINUE
100   CONTINUE



      RETURN
      END

c-------------------------------------------------------------------

        subroutine anglesv(deltax,deltaz,angle)

c    subroutine to determine the angle of the ray from output
c    to source or receiver from vertical.

        real deltax, deltaz, angle
        angle=atan(deltax/deltaz)
        return
        end

c --------------------------------------------------

        subroutine magnitude(angles,angler,gradtau2)

c    this subroutine is to determine the magnitude of the sum of
c    the two slowness vectors at the output points.

        real angles, angler, gradtau2, root2

        root2=sqrt(2.)
        gradtau2=root2*sqrt(1.+cos(angles-angler))/2.
c        write(0,*)"gradtau2=",gradtau2
        return
        end
