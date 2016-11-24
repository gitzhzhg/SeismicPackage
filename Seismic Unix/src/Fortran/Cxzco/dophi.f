c----------------------------------------------------------------------
      subroutine dophi(phi,alambd)
c........................................................................
c     Evaluates the system of equations, Phi.
c     For X to be a solution ( a raypath ), PHI must be
c     zero ( actually, its residual must be close to zero ).
c.........................................................................
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
c........................................................................
cc    local  variables
c     K      loop variable
c........................................................................
      integer     k
c
      do 100  k = 1, n
         phi(k) = v(k+1) * ( deltax(k) + alambd * df(k) * deltaz(k) ) /
     :    d(k) - v(k) * (deltax(k+1) + alambd * df(k) * deltaz(k+1) ) /
     :    d(k+1)
100   continue
      return
      end
