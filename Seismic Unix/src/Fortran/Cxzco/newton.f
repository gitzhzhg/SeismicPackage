c...................................................................
      subroutine newton(x,n,alambd,resid,fail)
c.......................................................................
c     Carries out the newton iteration for the continuation
c     procedures RAYONE and RECCON.
c.......................................................................
      integer    n
      real       x(0:n+1),    alambd,     resid
      logical    fail
c.......................................................................
cc    local    variables
c     DJ()     diagonal of the tridiagonal jacobian matrix
c     INFO     tells us if the jacobian is singular
c     K        loop variable
c     MAXN     max value of N
c     PHI()    system of equations to be solved
c     SPLNOK   tells us if X lies within range of the interfaces
c     SUBDJ()  subdiagonal of tridiagonal jacobian matrix
c     SUM      sum of squares of PHI
c     SUPDJ    superdiagonal of tridiagonal jacobian matrix
c.......................................................................
      integer   k,     maxn,     info
      parameter ( maxn = 50)
      real      subdj(maxn),    dj(maxn),
     :          supdj(maxn),    phi(maxn),
     :          sum
      logical splnok
c
      fail = .false.
c     Evaluate frequently used quantities.
      call eval(x,alambd,splnok)
c
c     If X outside range of splined interfaces, return.
      if(.not.splnok) then
         fail = .true.
         return
      end if
c
c     Evaluate the elements of the tridiagonal jacobian.
      call jacob(alambd,subdj,dj,supdj)
c
c     Evaluate phi.
      call dophi(phi,alambd)
c     Do the Newton iteration.
      if(n.eq.1) then
          x(1) = x(1) - phi(1) / dj(1)
      else
          call tridi(n,subdj,dj,supdj,phi,info)
          if(info.ne.0) then
             fail = .true.
             return
          end if
c         Tridi solves the matrix equation jx = phi for x.
c         The solution, x, is returned as the vector phi .
          do 100  k = 1,  n
             x(k) = x(k) - phi(k)
100       continue
      end if
c
c     Evaluate necessary quantities with new value of X
c     from the Newton iteration.
c
      call eval(x,alambd,splnok)
c     New X has to lie within range of splined interfaces.
      if(.not.splnok) then
         fail = .true.
         return
      end if
c     Evaluating PHI again.
      call dophi(phi,alambd)
c     Finding the l2 norm of phi.
      sum = 0.0
      do 200  k = 1,  n
         sum = sum + phi(k)**2
200   continue
      resid = sqrt( sum )
      return
      end
