c-------------------------------------------------------------------
      subroutine jacob(alambd,subdj,dj,supdj)
c...................................................................
c     Calculates the elements of the jacobian (dphi / dx ).
c     Note that this matrix is tridiagonal.
c     The jacobian is inverted in the Newton iteration and
c     in calculating dX/drlambd and dX/dalambd.
c...................................................................
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
c...................................................................
cc    local variables
c     K     loop variable
c...................................................................
      integer     k
c
c     Calculating the diagonal.
c
      do 100  k = 1, n
         dj(k) = v(k+1) * ( 1.0 + ( alambd * df(k) )**2 +
     :	       alambd*deltaz(k)*ddf(k)-((deltax(k)+alambd*df(k)
     :	       *deltaz(k))/d(k))**2)/d(k)+v(k)*(1.0+(alambd*df(k))**2 
     :	       -alambd*deltaz(k+1)*ddf(k)-((deltax(k+1)+alambd*df(k)
     :	       *deltaz(k+1))/d(k+1))**2)/d(k+1)
100   continue
      if(n.eq.1) go to 500
c
c     Calculating the subdiagonal
c
      do 200  k = 2, n
         subdj(k) = - v(k+1)*(1.0+(alambd**2)*df(k)*df(k-1) 
     :	          -(deltax(k)+alambd*df(k)*deltaz(k))*
     :		  (deltax(k)+alambd*df(k-1)*deltaz(k))/d(k)**2)/d(k)
200   continue
c
c     Calculating the superdiagonal
c
      do 300  k = 1, n-1
         supdj(k) = - v(k)*(1.0+(alambd**2)*df(k)*df(k+1)
     :		  -(deltax(k+1)+alambd*df(k)*deltaz(k+1))*
     :		  (deltax(k+1)+alambd*df(k+1)*deltaz(k+1))/
     :		  d(k+1)**2 ) / d(k+1)
300   continue
500   return
      end
