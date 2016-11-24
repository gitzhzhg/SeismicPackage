c
      subroutine amplit(sinmax,sins,amp,sigma,cosbf,icontr)
c...................................................................
c     Calculates the amplitude weighting factor to be applied to
c     the data, given the raypath.  The amplitude factor is made up
c     of transmission coefficients, change in area of ray tube at each
c     interface, emergence angle of ray, and the ray parameter sigma.
c...................................................................
      real        sinmax,      amp
      integer     maxn,        maxnp1,       maxnp3
      parameter ( maxn   = 50)
      parameter ( maxnp1 = maxn + 1,
     :            maxnp3 = maxn + 3)
      real        f(0:maxnp1),     df(maxn),      ddf(maxn),
     :            c(0:maxnp1),     d(maxnp1),
     :            deltac(maxnp1),  deltaf(maxnp1),
     :            deltax(maxnp1),  deltaz(maxnp1),
     :            v(maxnp1) 
      integer     n,               icontr 
      common /b/  f,               df,            ddf,
     :            c,               d,
     :            deltac,          deltaf,
     :            deltax,          deltaz,
     :            v,
     :            n
c...................................................................
c    local     variables
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
c...................................................................
      real        arg,           cosbf,
     :            cosinc,        costra,
     :            sigma,         trans,
     :            valnor,        xsectn
      integer     k

c     Initialising
      trans  = 1.
      xsectn = 1.
c
      if(icontr.eq.1)then
c       Checking for aliasing, Only true for rays to receivers.
      	if(deltax(1).eq.0.) then
c		?????
      	else
        	if(abs(deltax(1)/d(1)+sins).gt.sinmax) then
        		amp = 0.
            		return
         	end if
      	end if
      end if
c
      if(n.eq.0) then
c	??????
      else
c        Find transmission coefficient at each  intersection point.
c        Take scalar product of ray with normal to get
c        cosine of angle between.
         do 10  k = 1,  n
            valnor = sqrt( 1. + df(k)**2 )  
            costra = abs( ( deltax(k) * df(k) - deltaz(k) )
     :                 / ( d(k) * valnor ) )
            cosinc = abs( ( deltax(k+1) * df(k) - deltaz(k+1) )
     :                 / ( d(k+1) * valnor ) )
            if(cosinc.lt..1.or.costra.lt..1) then
               amp = 0.
               return
            end if
            arg = ( v(k+1) / v(k) )**2 - 1. + cosinc**2  
c           no transmitted ray at this angle of incidence
            if(arg.lt.0.) then
               amp = 0.
               return
            else
               trans = trans*2.*cosinc/(cosinc + sqrt(arg))
            end if
            xsectn = xsectn * costra / cosinc
10       continue
      end if
c
c     Calculating sigma
      sigma = 0.
      do 100  k = 1,  n + 1
         sigma = sigma + d(k) * v(k)
100   continue
c     Need the cosine of the emergence angle ray makes with vertical.
      cosbf = abs( deltaz(1) / d(1) )
c     Do the amplitude calculation for this ray.
      amp = sqrt(cosbf)/( sqrt(v(n+1)*xsectn ) * trans)
c
      return
      end
c
