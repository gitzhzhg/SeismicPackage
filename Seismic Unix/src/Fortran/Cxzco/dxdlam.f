c----------------------------------------------------------------------
      subroutine dxdlam(x,alambd,dxdl,info)
c...................................................................
c     x is an argument that is not really used, as far I can see.  jkc
c     Calculates the derivative of x with respect to alambd,
c     used in finding the first ray.
c...................................................................
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
c...................................................................
cc    local    variables
c     DJ()     diagonal of the tridiagonal jacobian matrix
c     DPHIDL() derivative of phi w.r.t. ALAMBD
c     K        loop variable
c     SUBDJ()  subdiagonal of tridiagonal jacobian matrix
c     SUPDJ    superdiagonal of tridiagonal jacobian matrix
c...................................................................
      real        dj(maxn),        dphidl(maxn),
     :            subdj(maxn),     supdj(maxn)
      integer     k
c
c     Evaluate the jacobian matrix.
      call jacob(alambd,subdj,dj,supdj)
c     Evaluate the derivative with respect to ALAMBD.
      do 325 k = 1, n
         dphidl(k) = v(k+1)*(df(k)*(deltaz(k)+alambd*deltaf(k))
     :    	   -(deltaz(k)/d(k)**2)*(deltax(k)+alambd*df(k)*
     :             deltaz(k))*deltaf(k))/d(k)-v(k)*(df(k)*(
     :		   deltaz(k+1)+alambd*deltaf(k+1))-(deltaz(k+1)
     :		   /d(k+1)**2)*(deltax(k+1)+alambd*df(k)*
     :    	   deltaz(k+1))*deltaf(k+1))/d(k+1)
325   continue
      if(n.eq.1) then
         dxdl(1) = - dphidl(1) / dj(1)
      else
         call tridi(n,subdj,dj,supdj,dphidl,info)
         if(info.ne.0) return
c        tridi returns the value of -dxdl in the variable dphidl.
         do 350  k = 1, n
            dxdl(k) = - dphidl(k)
350      continue
      end if
      return
      end
