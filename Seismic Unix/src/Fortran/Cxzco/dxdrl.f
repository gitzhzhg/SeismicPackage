c------------------------------------------------------------------
      subroutine dxdrl(geoinc,dxdl,info)
c...................................................................
c     Finds the derivative of x with respect to rlambd,
c     used in receiver continuation.
c...................................................................
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
c...................................................................
cc    local    variables
c     DJ()     diagonal of the tridiagonal jacobian matrix
c     DPHIDR() derivative of PHI w.r.t. x-coordinate of receiver
c     K        loop variable
c     SUBDJ()  subdiagonal of tridiagonal jacobian matrix
c     SUPDJ    superdiagonal of tridiagonal jacobian matrix
c...................................................................
      real       dj(maxn),        dphidr(maxn),
     :           subdj(maxn),     supdj(maxn)
      integer    k
c
c     Evaluste the jacobian matrix.
      call jacob(1.,subdj,dj,supdj)
c     Only PHI(1) is a function of the receiver location, X(0).
c     Also including here the receiver spacing (reference, eqn.(15)).
      dphidr(1) = -v(2)*(1.0-(deltax(1)/d(1))**2-
     :		(df(1)*deltax(1)*deltaz(1))/d(1)**2)*geoinc/d(1)
      if(n.eq.1) then
         dxdl(1) = - dphidr(1) / dj(1)
      else
         do 325  k = 2, n
            dphidr(k) = 0.0
325      continue
         call tridi(n,subdj,dj,supdj,dphidr,info)
         if(info.ne.0) return
c        tridi returns the value of -dxdl as dphidr.
         do 350  k = 1, n
            dxdl(k) = - dphidr(k)
350      continue
      end if
      return
      end
