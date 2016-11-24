c---------------------------------------------------------------------
      subroutine setn(zs,c,nint,n,znear,deltac,f)
c.......................................................................
c     The number of interfaces between source and receiver
c     may change when the depth of the source changes,
c     ( ie the source passes through an interface ).
c     Setn finds the number of interfaces between source
c     and receiver, and evaluates a few other things
c     that remain constant until the source is moved again.
c.......................................................................
      integer    n,          nint
      real       c(0:n+1),   f(0:n+1),     deltac(n+1),
     :           zs
c.......................................................................
cc    local   variables
c     I       loop variable
c     ZNEAR   If the source point is below the interface but less
c             than ZNEAR vertical distance from it, then the interface
c             is ignored. ZNEAR = LAMDA/4. = Cmax/Fmax/4.
c.......................................................................
      integer    i
      real       znear
c
c     Calculating the number of interfaces between source and receiver
c     for the direct ray.  If the source is on or just below the
c     interface then the interface is ignored.
c
      do 20 i = 1,  nint
         if(zs.le.(c(i)+znear)) then
            n = i - 1
            go to 25
         end if
20    continue
      n = nint
25    continue
c
c     Setting values at the end of the ray.
      f(n+1) = 0.
      deltac(n+1) = zs - c(n)
      do 40 i = 1,  n
         deltac(i) = c(i) - c(i-1)
40    continue
50    return
      end
