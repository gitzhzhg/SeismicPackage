c------------------------------------------------------------------------
      subroutine guessx(x,n,deltal,dxdl)
c........................................................................
c     Calculates the initial value of x to be used in newton's method.
c     See reference, eqn. (9).
c........................................................................
      integer   n
      real      x(0:n+1),      dxdl(n),      deltal
c........................................................................
cc    local variables
c     K     loop variable
c........................................................................
      integer   k
c
      do 100  k = 1,  n
         x(k) = x(k) + deltal * dxdl(k)
100   continue
      return
      end
