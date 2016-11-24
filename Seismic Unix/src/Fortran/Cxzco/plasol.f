c...................................................................
      subroutine plasol(n,xr,xs,zs,c,v,x,noconv)
c...........................................................................
c     Plasol finds the coordinates of the ray in the stratified
c     medium. it follows the procedure outlined in appendix a of
c     John Fawcett's thesis ( 3D Ray Tracing and Ray inversion
c     in Layered Media, Caltech, 1983).
c...........................................................................
      integer     n
      real        xr,      xs,       zs,    c(0:n+1),
     :            v(n+1),  x(0:n+1)
      logical     noconv
c...........................................................................
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
c...........................................................................
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
c
      noconv = .false.
      np1 = n + 1
c     initialize the iteration counters
      ibis = 0
      inewt = 0
c
c     The depths of the interfaces at the x coordinate of the source
c     are supplied by the main program. C(1) is the shallowest, etc.
c     Calculate the layer thicknesses from depths.
c
      thick(1) = c(1)
      do 30 i = 2, n
         thick(i) = c(i) - c(i-1)
30    continue
c     Bottom of last layer set at depth of source.
      thick(np1) = zs - c(n)
c     If receiver is above source then all x coordinates are equal.
      if(xr.eq.xs) then
         do 40  i = 0,  np1
            x(i) = xr
40       continue
         return
      end if
c
c     Setting alpha (in Fawcett's thesis).
c
      a(1) = 1.
      do 50 i = 2,  np1
         a(i) = v(i) / v(1)
50    continue
c     Find the maximum velocity.
      vmax = v(1)
      do 60 i = 2,  np1
         if(vmax.lt.v(i)) then
            vmax = v(i)
         end if
60    continue
c
c     Setting minimum of 1 / alpha
c
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
c
c     Use bisection to get close.
c
      if(abs(dist-offset).lt.close) then
c        ??????
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
c
c     Use newton's method to get very close.
c
140   if(abs(dist-offset).le.vclose) then
c        ???????
      else
         inewt = inewt + 1
         if(inewt.gt.maxnwt) then
            noconv = .true.
            return
         end if
         dfdx = 0.0
         do 150 k = 1,  np1
            dfdx = dfdx+thick(k)*a(k)/(1-(a(k)*xnew)**2)**1.5
150      continue
         xnew = xnew - ( dist - offset ) / dfdx
c
         do 160 k = 1,  np1
            if(abs(a(k)*xnew).ge.1.0) then
c              Newton's method can be unpredictable.
c              If the above product is greater than 1., we
c              get into trouble with square roots below.
               noconv = .true.
               return
            end if
160      continue
         dist = fx(xnew,a,thick,np1)
         go to 140
      end if
c
      if((xs-xr).lt.0.) then 
         sign = - 1.0
      else
         sign = 1.0
      end if
c
      x(1) = xr + sign * thick(1) * a(1) * xnew /
     :       sqrt( 1. - ( a(1) * xnew )**2 )
      do 200 i = 2,  n
         x(i) = x(i-1) + sign * thick(i) * a(i) * xnew /
     :          sqrt( 1. - ( a(i) * xnew )**2 )
200   continue
      x(np1) = xs
      x(0)   = xr
      return
      end
