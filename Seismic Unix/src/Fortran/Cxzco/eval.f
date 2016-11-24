c---------------------------------------------------------------------
      subroutine eval(x,alambd,splnok)
c.....................................................................
c     Evaluates quantities used frequently by other subroutines.
c.....................................................................
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
      common /a/  xint,          zint,
     :            a0,            a1,        a2,         a3,
     :            npts,          nint
      real        f(0:maxnp1),     df(maxn),      ddf(maxn),
     :            c(0:maxnp1),     d(maxnp1),
     :            deltac(maxnp1),  deltaf(maxnp1),
     :            deltax(maxnp1),  deltaz(maxnp1),
     :            v(maxnp1)
      integer     n,               na
      common /b/  f,               df,            ddf,
     :            c,               d,
     :            deltac,          deltaf,
     :            deltax,          deltaz,
     :            v,
     :            n
      common /coor/cx,cy
c.....................................................................
cc    local variables
c     I     loop variable over intersection points
c     K     loop variable over intersection points
c     J     identifies section of interface
c.....................................................................
      integer     i,j,k
c
      splnok = .true.
c     If x falls outside the range of definition of the splined
c     interface, then return.
      do 3 i = 1,  n
         if(x(i).lt.xint(i,1).or.x(i).ge.xint(i,npts(i))) then
            splnok = .false.
            return
         end if
3     continue
c
c     Finding the section of the spline on which x lies and evaluating
c     the function and derivatives.
c
      do 10 i = 1,  n
         j = 1
5        if(x(i).ge.xint(i,j)) then
            j = j + 1
            go to 5
         end if
         j = j - 1
c
         f(i)   = a0(i,j) - c(i) + a1(i,j) * x(i) + a2(i,j) * x(i)**2
     :		+ a3(i,j) * x(i)**3
         df(i)  = a1(i,j)  +   2.* a2(i,j) * x(i) + 3.*a3(i,j) * x(i)**2
         ddf(i) =              2.* a2(i,j)        + 6.*a3(i,j) * x(i)
10    continue
      do 100 k = 1,  n + 1
         deltax(k) = x(k) - x(k-1)
         deltaf(k) = f(k) - f(k-1)
         deltaz(k) = deltac(k) + alambd * deltaf(k)
         d(k) = sqrt( deltax(k)**2 + deltaz(k)**2 )
100   continue
      return
      end
c---------------------------------------------------------------------------
      real function fx(x,a,thick,np1)
c...........................................................................
c     Calculates distance travelled by ray (see Fawcett eqn. (A-3)).
c...........................................................................
      integer  np1
      real     x,    a(np1),     thick(np1)
c...........................................................................
cc    local    variables
c     j        loop variable
c...........................................................................
      integer  j
c
      fx = 0.0
      do 500 j = 1,  np1
         fx = fx + thick(j) * a(j) * x /
     :   sqrt( 1.- a(j) * a(j) * x * x )
500   continue
      return
      end
