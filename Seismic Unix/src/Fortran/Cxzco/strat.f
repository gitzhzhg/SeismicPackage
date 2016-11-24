c...................................................................
      subroutine strat(xs,c,exit)
c........................................................................
c     Each time the source is moved it is necessary to 
c     calculate the depths of the interfaces in the horizontally
c     stratified model.
c........................................................................
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
      common /a/  xint,          zint,
     :            a0,            a1,        a2,         a3,
     :            npts,          nint
c........................................................................
cc    local   variables
c     I       loop variable over interfaces
c     J       identifies section of interface
c........................................................................
      integer    i,             j
c
c     Finding the depth of each interface at the
c     x coordinate of the source.  The shallowest depth is c(1)
c     and c(nint) is the deepest. These depths will be used 
c     in the stratified model.
c
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
     :            + a3(i,j) * xs**3
10    continue
c
      return
      end
