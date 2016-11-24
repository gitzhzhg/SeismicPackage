c-----------------------------------------------------------------
      subroutine reccon(x,n,xr,geoinc,v1,noconv)
c.................................................................
c     This subroutine performs receiver continuation.
c     Using a ray from some source to receiver, subroutine reccon
c     finds the ray from that same source position to an adjacent
c     receiver. The continuation parameter is rlambd.
c.................................................................
      integer    n
      real       x(0:n+1),    xr,    geoinc,	v1
      logical    noconv
c.................................................................
cc    local    variables
c     DELTAL   increment in continuation parameter
c     DXDL()   derivative of X r.r.t. RLAMBD
c     EPS      lower limit on DELTAL
c     FAIL     true when Newton's method breaks down
c     I        counts the number of Newton iterations
c     INFO     indicates if jacobian is singular
c     K        loop variable
c     MAXIT    maximum number of newton iterations at each
c              continuation step
c     MAXN     max allowed value of N
c     RESID    residual of the system of equations to be solved
c     RLAMBD   continuation parameter
c     SAVEX()  stores solution of last continuation step
c     SOLN     residual must fall below this for a solution
c.................................................................
      integer   i,   info,   k,   maxit,    maxn
      parameter ( maxit = 6,
     :            maxn  = 50)
      real      savex(maxn+1),    dxdl(maxn),
     :          deltal,           eps,
     :          resid,            rlambd,
     :          soln
      logical   fail
      parameter ( eps  = 0.065)
c
c     Initialise some variables.
c
      soln = v1*0.005
      noconv = .false.
      deltal = 1.0
      rlambd = 0.0
c     Finding the derivative of x with respect to rlambd.
50    call dxdrl(geoinc,dxdl,info)
      if(info.ne.0) then
         noconv = .true.
         return
      end if
c
c     Save the value of X.  If Newton's method fails to converge
c     we will return to this value and reduce the continuation step.
c
      do 75  k = 1, n
         savex(k) = x(k)
75    continue
c     Calculate the first guess to put in Newton's method.
      call guessx(x,n,deltal,dxdl)
c     Increment the continuation parameter and the end point of the ray.
      rlambd = rlambd + deltal
      x(0) = xr + geoinc * rlambd
c
c     Start the Newton iterations.
c     After each iteration we check to see if the
c     iteration has been carried out successfully, and
c     if so, check to see if it has found a solution.
c
      i = 0
c     Since the interfaces are now fixed in their true positions,
c     we set ALAMBD equal to one.
      call newton(x,n,1.,resid,fail)
100   if(fail.or.resid.gt.soln) then
         i = i + 1
         if(.not.fail.and.i.lt.maxit) then
            call newton(x,n,1.,resid,fail)
         else
c           Newton has failed.  Reduce the continuation step.
            rlambd = rlambd - deltal
            deltal = deltal / 2.0
c           If deltal is not too small, return to the previous
c           solution and proceed with the continuation.
            if(deltal.gt.eps) then
               do 150 k = 1,  n
                  x(k) = savex(k)
150            continue
               call guessx(x,n,deltal,dxdl)
               rlambd = rlambd + deltal
               x(0) = xr + geoinc * rlambd
c              reset the iteration counter
               i = 0
               call newton(x,n,1.,resid,fail)
            else
c              The continuation procedure has failed.
               noconv = .true.
               return
            end if
         end if
         go to 100
      end if
c     Continuation is not done until RLAMBD = 1.
      if(rlambd.lt.1.0) go to 50
c     Updating the receiver position
      xr = xr + geoinc
      return
      end
