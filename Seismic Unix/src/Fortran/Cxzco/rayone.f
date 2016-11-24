c-----------------------------------------------------------------
      subroutine rayone(xr,xs,zs,n,v,c,x,noconv)
c.......................................................................
c     This subroutine performs continuation in interfaces
c     Starting with a stratified earth, subroutine rayone deforms the
c     interfaces, using the continuation parameter alambd, until the
c     desired model is achieved.
c.......................................................................
cc    local    variables
c     ALAMBD   continuation parameter
c     DELTAL   increment in continuation parameter
c     DXDL()   derivative of X w.r.t. ALAMBD
c     EPS      lower limit on DELTAL
c     FAIL     true when Newton's method breaks down
c     I        counts the number of Newton iterations
c     INFO     indicates if jacobian is singular
c     K        loop variable
c     MAXIT    maximum number of newton iterations at each
c              continuation step
c     MAXN     max allowed value of N
c     RESID    residual of the system of equations to be solved
c     SAVEX()  stores solution of last continuation step
c     SOLN     residual must fall below this for a solution
c     SPLNOK   indicates whether X is within range of splined interfaces
c.......................................................................
      integer     n
      real        xr,        xs,           zs,
     :            v(n+1),    c(0:n+1),     x(0:n+1)
      logical     noconv
c
      integer   i,   info,   k,   maxit,    maxn
      parameter ( maxit = 6,
     :            maxn  = 50)
      real      savex(maxn+1),    dxdl(maxn),
     :          deltal,           eps,
     :          resid,            alambd,
     :          soln
      logical   fail,             splnok
      parameter ( eps  = 0.065)
c
c     Initialising some variables.
c
      soln   = v(1)*0.005
      noconv = .false.
      alambd = 0.0
      deltal = 1.0
c
c     Finding the flat earth solution.
c
      call plasol(n,xr,xs,zs,c,v,x,noconv)
      if(noconv) return
c
c     Evaluate some frequently used quantities for a known solution.
c
      call eval(x,alambd,splnok)
c
c     Finding the derivative of x with respect to alambd.
c
50    call dxdlam(x,alambd,dxdl,info)
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
c
c     Calculate the first guess to be put into Newton's method.
c
      call guessx(x,n,deltal,dxdl)
c     Increment the continuation parameter.
      alambd = alambd + deltal
c
c     Start the Newton iterations.
c     After each iteration we check to see if the
c     iteration has been carried out successfully, and 
c     if so, check to see if it has found a solution.
c
      i = 0
      call newton(x,n,alambd,resid,fail)
100   if(fail.or.resid.gt.soln) then
         i = i + 1
         if(.not.fail.and.i.lt.maxit) then
            call newton(x,n,alambd,resid,fail)
         else
c           Newton has failed.  Reduce the continuation step.
            alambd = alambd - deltal
            deltal = deltal / 2.0
c
c           If deltal is not too small, return to the previous 
c           solution and proceed with the continuation.
c
            if(deltal.gt.eps) then
               do 150  k = 1,  n
                  x(k) = savex(k)
150            continue
               call guessx(x,n,deltal,dxdl)
               alambd = alambd + deltal
c              reset the iteration counter
               i = 0
               call newton(x,n,alambd,resid,fail)
            else
c              The continuation procedure has failed.
               noconv = .true.
               return
            end if
         end if
         go to 100
      end if
c     Continuation is not done until ALAMBD = 1.
      if(alambd.lt.1.0) go to 50
      return
      end
