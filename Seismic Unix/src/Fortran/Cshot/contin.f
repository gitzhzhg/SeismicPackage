cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c    Subroutines for program CSHOT
c
c    This program belongs to the Center for Wave Phenomena
c    Colorado School of Mines
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c    These subroutines are used in the continuation
c    method of ray tracing.
c    Reference: A Fast Ray Tracing Routine for Laterally
c               Inhomogeneous Media, by Paul Docherty, CWP-018.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc



      SUBROUTINE RECCON(X,z,N,GEOINC,geoz,PI,BETNEW,NOCONV,s0)

c     This subroutine performs receiver continuation.
c     Using a ray from some source to receiver, subroutine reccon
c     finds the ray from that same source position to an adjacent
c     receiver. The continuation parameter is rlambd.


      INTEGER N

      REAL     X(0:N+1),     GEOINC,     geoz,   PI,
     :         BETNEW,       z(0:n+1),   s0

      LOGICAL  NOCONV

cc    local   variables
c     DELTAL  increment in continuation parameter
c     DXDL()  derivative of X w.r.t. RLAMBD
c     EPS     lower limit on DELTAL
c     FAIL    true when Newton's method breaks down
c     I       counts the number of Newton iterations
c     INFO    indicates if jacobian is singular
c     K       loop variable
c     MAXIT   maximum number of newton iterations at each
c             continuation step
c     MAXN    max allowed value of N
c     NP1     N + 1
c     RESID   residual of the system of equations to be solved
c     RLAMBD  continuation parameter
c     SAVEX() stores solution of last continuation step
c     SAVEZ1  stores Z1 from last continuation step
c     SOLN    residual must fall below this for a solution
c     SPLNOK  indicates whether X is within range of splined interfaces
c     XR      fixed x-location of previous ray
c     Z1      depth of first intersection point
c     zr      fixed z-location of previous ray



      INTEGER    MAXIT,    MAXN,   NP1,   I,    INFO,   K

      PARAMETER (MAXIT = 5,
     :           MAXN  = 40)

      REAL      SAVEX(MAXN+1),  DXDL(MAXN),
     :          DELTAL,         EPS,           zr,
     :          SAVEZ1,         SOLN,          RESID,
     :          RLAMBD,         XR,            Z1

      LOGICAL    FAIL,          SPLNOK

      PARAMETER ( EPS  =   0.065)

      soln = float(n)

c     Initialise some variables.
      np1 = N + 1
      NOCONV = .FALSE.
      DELTAL = 1.
      RLAMBD = 0.0
      XR = X(np1)
      zr = z(np1)

c     Evaluate some frequently used quantities, for a known solution.
      CALL EVAL(X,z,SPLNOK)

c     Finding the derivative of x with respect to rlambd.
50    CALL DXDRL(GEOINC,geoz,DXDL,INFO)

      IF(INFO.NE.0) THEN
         CALL FINDBN(X,z,PI,BETNEW,s0)
         NOCONV = .TRUE.
         RETURN
      END IF

c     Save the value of x. If Newton's method fails to converge
c     we will return to this value and reduce the continuation step.
      DO 75  K = 1, np1
         SAVEX(K) = X(K)
75       CONTINUE

c     Save previous value of z1.
      SAVEZ1 = z(1)

c     Calculate the first guess to put into Newton's method.
      CALL GUESSX(X,N,DELTAL,DXDL)

c     Increment the continuation parameter and the end point of the ray.
      RLAMBD = RLAMBD + DELTAL
      X(np1) = XR + GEOINC * RLAMBD
      z(np1) = zr + geoz * rlambd


c     Start the Newton iterations.
c     After each iteration we check to see if the
c     iteration has been carried out successfully, and
c     if so, check to see if it has found a solution.

      I = 0
      CALL NEWTON(X,z,N,RESID,FAIL,.FALSE.,0.,0.)


100   IF(FAIL.OR.RESID.GT.SOLN) THEN

         I = I + 1
         IF(.NOT.FAIL.AND.I.LT.MAXIT) THEN
            CALL NEWTON(X,z,N,RESID,FAIL,.FALSE.,0.,0.)
         ELSE
c           Newton has failed.  Reduce the continuation step
c           and return to the solution for the previous value
c           of rlambd.
            RLAMBD = RLAMBD - DELTAL
            DELTAL = DELTAL / 2.0
            DO 150 K = 1,  N
               X(K) = SAVEX(K)
150            CONTINUE
c           If deltal is not too small, proceed with the continuation.
            IF(DELTAL.GT.EPS) THEN
               CALL GUESSX(X,N,DELTAL,DXDL)
               RLAMBD = RLAMBD + DELTAL
               X(np1) = XR + GEOINC * RLAMBD
               z(np1) = zr + geoz * rlambd
c              reset the iteration counter
               I = 0
               CALL NEWTON(X,z,N,RESID,FAIL,.FALSE.,0.,0.)
            ELSE
c              The continuation procedure has failed.
               NOCONV = .TRUE.
c              Set some values from the last good continuation step.
               X(np1) = SAVEX(np1)
               z(1) = SAVEZ1
c              Calculate the takeoff angle for the last good ray.
               CALL FINDBN(X,z,PI,BETNEW,s0)
               RETURN
            END IF
         END IF

         GO TO 100

      END IF


c     Continuation is not done until rlambd = 1.
      IF(RLAMBD.LT.1.0) GO TO 50

c     Calculate the takeoff angle of the new ray solution.
      CALL FINDBN(X,z,PI,BETNEW,s0)


      RETURN
      END

*---------------------------------------------------------------------

      SUBROUTINE DXDRL(GEOINC,geoz,DXDL,INFO)

c     Calculates the derivative of x with respect to the
c     continuation parameter RLAMBD.
c     See reference: CWP-018, equation(14).

      REAL       GEOINC,          DXDL(*),          geoz

      INTEGER    INFO

      INTEGER    MAXN,            MAXNP1

      PARAMETER(MAXN   = 40)
      PARAMETER(MAXNP1 = MAXN + 1)

      REAL       DZ(MAXN),       DDZ(MAXN),
     :           D(MAXNP1),      DELTAX(MAXNP1),   DELTAZ(MAXNP1),
     :           V(MAXNP1)

      INTEGER    N

      COMMON /B/   DZ,           DDZ,
     :             D,            DELTAX,        DELTAZ,
     :             V,            N


cc    local    variables
c     DJ()     diagonal of the tridiagonal jacobian matrix
c     DPHIDR() derivative of phi w.r.t. receiver location
c     K        loop variable
c     NP1      N + 1
c     SGNDET   tracks the sign of the jacobian (not used here)
c     SUBDJ()  subdiagonal of tridiagonal jacobian matrix
c     SUPDJ()  superdiagonal of tridiagonal jacobian matrix

      REAL          DPHIDR(MAXN),       SUBDJ(MAXN),
     :              DJ(MAXN),           SUPDJ(MAXN),
     :              SGNDET

      INTEGER       K,   NP1


      np1 = N + 1

c     Evaluate the Jacobian matrix.
      CALL JACOB(SUBDJ,DJ,SUPDJ,.FALSE.)

c     Only phi(n) is a function of the receiver location, x(n+1).
c     Also including here the receiver spacing (reference, eqn.(15))

      DPHIDR(N) = - ( V(N) / d(np1)**3 )  *

     :              (  ( d(np1)**2 - DELTAX(Np1)**2
     :                - DZ(N) * DELTAX(Np1) * DELTAZ(Np1) ) * geoinc 
     :                 +
     :                 ( dz(n) * d(np1)**2 - dz(n) * deltaz(np1)**2
     :                   - deltax(np1) * deltaz(np1) ) * geoz    )


      IF(N.EQ.1) THEN
         DXDL(1) = - DPHIDR(1) / DJ(1)
      ELSE
         DO 325  K = 1,  N - 1
            DPHIDR(K) = 0.0
325         CONTINUE
         CALL TRIDI(N,SUBDJ,DJ,SUPDJ,DPHIDR,SGNDET,INFO)
         IF(INFO.NE.0)  RETURN
c        tridi returns the value of -dxdl as dphidr.
         DO 350  K = 1, N
            DXDL(K) = - DPHIDR(K)
350         CONTINUE
      END IF


      RETURN
      END

*---------------------------------------------------------------------

      SUBROUTINE GUESSX(X,N,DELTAL,DXDL)

c     Calculates the initial value of x to be used in newton's method.
c     See reference, eqn. (9).

      INTEGER   N

      REAL      X(0:N+1),     DXDL(N),      DELTAL

cc    local  variables
c     K      loop variable

      INTEGER K


      DO 100  K = 1,  N
         X(K) = X(K) + DELTAL * DXDL(K)
100      CONTINUE


      RETURN
      END

*---------------------------------------------------------------------

      SUBROUTINE NEWTON(X,z,N,RESID,FAIL,head,sgnoff,sinthc)

c     Performs the Newton iteration for subroutine reccon.
c     The Newton iteration is considered to have failed if
c     the jacobian becomes singular or if the value of x
c     lies outside the range of definition of the model.
c     See reference, eqn.(11).

      INTEGER    N

      REAL       X(0:N+1),   z(0:n+1),    RESID,   sgnoff,   sinthc

      LOGICAL    FAIL,       head

cc    local    variables
c     DJ()     diagonal of the tridiagonal jacobian matrix
c     INFO     tells us if the jacobian is singular
c     K        loop variable
c     MAXN     max value of N
c     PHI()    system of equations to be solved
c     SPLNOK   tells us if X lies within the range of the interfaces
c     SGNDET   required by sub. tridi, not used here
c     SUBDJ()  subdiagonal of tridiagonal jacobian matrix
c     SUM      sum of squares of PHI
c     SUPDJ()  superdiagonal of tridiagonal jacobian matrix

      INTEGER    K,      MAXN,       INFO

      PARAMETER ( MAXN = 40)

      REAL      SUBDJ(MAXN),     DJ(MAXN),
     :          SUPDJ(MAXN),     PHI(MAXN),
     :          SGNDET,          SUM

      LOGICAL   SPLNOK


      FAIL = .FALSE.

c     Evaluate frequently used quantities.
      CALL EVAL(X,z,SPLNOK)

c     If x outside range of splined interfaces, return.
      IF(.NOT.SPLNOK) THEN
         FAIL = .TRUE.
         RETURN
      END IF

c     Evaluate the elements of the tridiagonal jacobian.
      CALL JACOB(SUBDJ,DJ,SUPDJ,head)

c     Evaluate phi.
      CALL DOPHI(PHI,head,sgnoff,sinthc)

c     Do the Newton iteration.
      IF(N.EQ.1) THEN
         X(1) = X(1) - PHI(1) / DJ(1)
      ELSE
         CALL TRIDI(N,SUBDJ,DJ,SUPDJ,PHI,SGNDET,INFO)
         IF(INFO.NE.0) THEN
            FAIL = .TRUE.
            RETURN
         END IF
c        Tridi solves the matrix equation jx = phi for x.
c        The solution, x, is returned as the vector phi .
         DO 100  K = 1, N
            X(K) = X(K) - PHI(K)
100         CONTINUE
      END IF

c     Evaluate necessary quantities with new value of x
c     from the Newton iteration.
      CALL EVAL(X,z,SPLNOK)

c     New x has to lie inside range of splined interfaces.
      IF(.NOT.SPLNOK) THEN
         FAIL = .TRUE.
         RETURN
      END IF

c     Evaluating phi again.
      CALL DOPHI(PHI,head,sgnoff,sinthc)

c     Finding the l2 norm of the new phi.
      SUM = 0.0

      DO 200  K = 1, N
         SUM = SUM + PHI(K)**2
200      CONTINUE

      RESID = SQRT( SUM )


      RETURN
      END

*---------------------------------------------------------------------

      SUBROUTINE JACOB(SUBDJ,DJ,SUPDJ,head)

c     Calculates the elements of the jacobian matrix (dphi/dx).
c     Note that this matrix is tridiagonal.
c     The jacobian is inverted in the Newton iteration and
c     in calculating dX/dlambda.
c     See reference, eqns. (11) and (14).

      REAL       SUBDJ(*),        DJ(*),           SUPDJ(*)
      logical    head

      INTEGER    MAXN,            MAXNP1

      PARAMETER (MAXN   = 40)
      PARAMETER (MAXNP1 = MAXN + 1)

      REAL       DZ(MAXN),       DDZ(MAXN),
     :           D(MAXNP1),      DELTAX(MAXNP1),   DELTAZ(MAXNP1),
     :           V(MAXNP1)

      INTEGER    N

      COMMON /B/   DZ,           DDZ,
     :             D,            DELTAX,        DELTAZ,
     :             V,            N

cc    local variables
c     K     loop variable
c     KMAX  upper limit of K
c     KP1   K + 1
c     T     magnitude of tangent to interface
c     TMP   scalar product of tangent and ray


      INTEGER K,  kmax,  kp1
      real    t,  tmp

      if(head) then
         kmax = n - 1
      else
         kmax = n
      end if

c     Evaluate the diagonal.
      DO 100  K = 1,  kmax
         kp1 = k + 1
         DJ(K) = V(Kp1) * ( 1.0 + ( DZ(K) )**2 +
     :      DELTAZ(K) * DDZ(K) - ( ( DELTAX(K) + DZ(K)
     :    * DELTAZ(K) ) / D(K) )**2 ) / D(K)
     :    + V(K) * ( 1.0 + ( DZ(K) )**2 - DELTAZ(Kp1)
     :    * DDZ(K) - ( ( DELTAX(Kp1) + DZ(K) * DELTAZ(Kp1) ) /
     :      D(Kp1) )**2 ) / D(Kp1)
100      CONTINUE


      IF(N.EQ.1) GO TO 500

c     Now the subdiagonal.
      DO 200  K = 2,  kmax
         SUBDJ(K) = - V(K+1) * ( 1.0 + DZ(K) *
     :    DZ(K-1) - ( DELTAX(K) + DZ(K) * DELTAZ(K) ) *
     :    ( DELTAX(K) + DZ(K-1) * DELTAZ(K) ) / D(K)**2 )
     :    / D(K)
200      CONTINUE

c     Last, the superdiagonal.
      DO 300  K = 1,  kmax - 1
         kp1 = k + 1
         SUPDJ(K) = - V(K) * ( 1.0 + DZ(K) * DZ(Kp1)
     :    - ( DELTAX(Kp1) + DZ(K) * DELTAZ(Kp1) ) *
     :    ( DELTAX(Kp1) + DZ(Kp1) * DELTAZ(Kp1) ) /
     :    D(Kp1)**2 ) / D(Kp1)
300      CONTINUE

500   continue

      if(head) then
c        now do diagonal and subdiagonal for last equation
         t = sqrt(1. + dz(n)**2)
         tmp = ( deltax(n) + deltaz(n)*dz(n) ) 

         dj(n) = -1. * deltax(n) * tmp / ( t * d(n)**3 )
     :           -1. * dz(n) * ddz(n) * tmp / ( d(n) * t**3 )
     :           + ( 1. + deltaz(n) * ddz(n) ) / ( t * d(n) )
         dj(n) = dj(n) * v(1)

         subdj(n) = deltax(n) * tmp / ( t * d(n)**3 )
     :            -1. / ( t * d(n) )
         subdj(n) = subdj(n) * v(1)
      end if

      RETURN
      END

*--------------------------------------------------------------------

      SUBROUTINE DOPHI(PHI,head,sgnoff,sinthc)

c     Evaluates the system of equations, phi .
c     For X to be a solution ( a raypath ), phi must be
c     zero ( actually, its residual must be close to zero ).

      REAL      PHI(*),  sgnoff,  sinthc
      logical   head

      INTEGER    MAXN,         MAXNP1

      PARAMETER ( MAXN   = 40)
      PARAMETER ( MAXNP1 = MAXN + 1)

      REAL       DZ(MAXN),       DDZ(MAXN),
     :           D(MAXNP1),      DELTAX(MAXNP1),   DELTAZ(MAXNP1),
     :           V(MAXNP1)

      INTEGER    N

      COMMON /B/   DZ,           DDZ,
     :             D,            DELTAX,        DELTAZ,
     :             V,            N


c     local  variables
c     K      loop variable
c     KMAX   max value of K
c     KP1    K + 1
c     T      magnitude of tangent to interface

      INTEGER   K,   kmax,  kp1
      real      t

      if(head) then
         kmax = n - 1
      else 
         kmax = n
      end if

      DO 100  K = 1, kmax
         kp1 = k + 1
         PHI(K) = V(Kp1) * ( DELTAX(K) + DZ(K) * DELTAZ(K) ) /
     $    D(K) - V(K) * (DELTAX(Kp1) + DZ(K) * DELTAZ(Kp1) ) /
     $    D(Kp1)
100      CONTINUE

      if(head) then
c        for head waves, last equation specifies angle ray 
c        makes with normal at interface
c        multiplication by v(1) here makes this equation of the same
c        order of magnitude as the others in the system
         t = sqrt(1. + dz(n)**2 )
         phi(n) = ( deltax(n) + deltaz(n) * dz(n) ) / ( d(n) * t )
     :            - sgnoff * sinthc
         phi(n) = phi(n) * v(1)
      end if

      RETURN
      END

*----------------------------------------------------------------------

      SUBROUTINE EVAL(X,z,SPLNOK)

c     Evaluates quantities used frequently by other subroutines
c     in the continuation procedure.

      REAL       X(0:*),       z(0:*)

      LOGICAL    SPLNOK

      INTEGER    MAXINT,       MAXSPL,         MXSPM1,
     :           MAXN,         MAXNP1

      PARAMETER ( MAXINT = 20,
     :            MAXSPL = 2001,
     :            MAXN   = 40)

      PARAMETER ( MAXNP1 = MAXN + 1,
     :            MXSPM1 = MAXSPL - 1)

      REAL        XINT(0:MAXINT,MAXSPL),     ZINT(0:MAXINT,MAXSPL),
     :            A0(0:MAXINT,MXSPM1),       A1(0:MAXINT,MXSPM1),
     :            A2(0:MAXINT,MXSPM1),       A3(0:MAXINT,MXSPM1),
     :            SIGN(0:MAXN),              CV(0:MAXINT,MAXSPL)

      INTEGER     NPTS(0:MAXINT),   NINT,      NORDER(MAXN)

      COMMON /A/   XINT,          ZINT,
     :             A0,            A1,        A2,          A3,
     :             SIGN,
     :             NPTS,          NINT,      NORDER,      CV


      REAL       DZ(MAXN),       DDZ(MAXN),
     :           D(MAXNP1),      DELTAX(MAXNP1),   DELTAZ(MAXNP1),
     :           V(MAXNP1),      DXM1XM,           DXM1X,
     :           DXXM,           AM1,              BM1

      INTEGER    N

      COMMON /B/   DZ,           DDZ,
     :             D,            DELTAX,        DELTAZ,
     :             V,            N


c     local  variables
c     I      loop variable over intersection points
c     J      identifies section of interface
c     L      identifies interface
c     K      loop variable over intersection points

      INTEGER     I,       J,       K,       L


      SPLNOK = .TRUE.

c     Finding the depth of each intersection point and the
c     slope and second z derivative at that point.

      DO 10 I = 1,  N

c        interface number of next intersection
         L = NORDER(I)
c        finding the section of the spline on which x lies and evaluating
c        the function and derivatives.
c        if x falls outside the range of definition of the
c        splined interface, then return.
         IF(X(I).LE.XINT(L,1).OR.X(I).GT.XINT(L,NPTS(L))) THEN
            SPLNOK = .FALSE.
            RETURN
         END IF

         J = 1
5        IF(X(I).GT.XINT(L,J)) THEN
            J = J + 1
            GO TO 5
         END IF
         J = J - 1


c **************************************************************************
c  The following calculation of the position of the interface was changed by
c  E.Jenner and T.Salinas, CWP July 1996, to increase the accuracy. 
c  See comments in 'splines.f', subroutine CUSPLN for details.
c ************************************************************************** 
c        Z(I)   = A0(L,J) + A1(L,J) * X(I) + A2(L,J) * X(I)**2
c    $          + A3(L,J) * X(I)**3

c        DZ(I)  = A1(L,J) + 2.* A2(L,J) * X(I) + 3.*A3(L,J) * X(I)**2
c        DDZ(I) = 2.* A2(L,J) + 6.*A3(L,J) * X(I)

         IF(NPTS(L).EQ.2) THEN

            Z(I)   = A0(L,J) + A1(L,J) * X(I) + A2(L,J) * X(I)**2
     $             + A3(L,J) * X(I)**3

            DZ(I)  = A1(L,J) + 2.*A2(L,J)*X(I) + 3.*A3(L,J)*X(I)**2
            DDZ(I) = 2.* A2(L,J) + 6.*A3(L,J) * X(I)

         ELSE

            DXM1XM = XINT(L,J+1) - XINT(L,J)
            BM1 = ZINT(L,J+1) / DXM1XM - CV(L,J+1) * DXM1XM / 6
            AM1 = ZINT(L,J) / DXM1XM - CV(L,J) * DXM1XM / 6
            DXM1X = XINT(L,J+1) - X(I)
            DXXM = X(I) - XINT(L,J)

            Z(I)=(CV(L,J)*DXM1X**3+CV(L,J+1)*DXXM**3)/(6*DXM1XM)
     $           + AM1 * DXM1X + BM1 * DXXM

            DZ(I)=(-CV(L,J)*DXM1X**2+CV(L,J+1)*DXXM**2)/(2*DXM1XM)
     $             - AM1 + BM1

            DDZ(I) = (CV(L,J)*DXM1X + CV(L,J+1)*DXXM)/DXM1XM

         ENDIF

10       CONTINUE


      DO 20 K = 1, N + 1
         DELTAX(K) = X(K) - X(K-1)
         DELTAZ(K) = Z(K) - Z(K-1)
         D(K) = SQRT( DELTAX(K)**2 + DELTAZ(K)**2 )
20      CONTINUE

      RETURN
      END

*---------------------------------------------------------------------

      SUBROUTINE FINDBN(X,Z,PI,BETNEW,s)

c     Given the raypath, this subroutine finds its takeoff angle

      REAL    X(0:*),    Z(0:*),    PI,      BETNEW,  s

c     local  variables
c     s      sign(0)
c     ximx0  magnitude of x1 minus x0
c     Z1MZ0  magnitude of Z1 minus Z0

      REAL    x1mx0,  Z1MZ0

      Z1MZ0 = (Z(1) - Z(0))
      x1mx0 = (x(1) - x(0))

      if(s.lt.0.) then
c        takeoff angle measured from downward vertical
c        ray is downgoing (generally)
         if(z1mz0.gt.0.) then
c           in range -90 to 90 degrees - ray really going down
            BETNEW = 180. * ATAN2( X1mX0 , Z1MZ0 ) / PI
         else if(z1mz0.lt.0.) then
c           ray going up
            if(x1mx0.lt.0.) then
               BETNEW = -180.+180.*ATAN2(abs(X1mX0),abs(Z1MZ0))/PI
            else if(x1mx0.gt.0.) then
              BETNEW = +180. - 180. * ATAN2(X1mX0,abs(Z1MZ0)) / PI
            else if(x1mx0.eq.0.) then
               betnew = 180.
            end if
         else if (z1mz0.eq.0.) then
c           ray horizontal
            if(x1mx0.lt.0.) betnew = -90.
            if(x1mx0.gt.0.) betnew = +90.
         end if
      end if
         
      if(s.gt.0.) then
c        takeoff angle measured from upward vertical
c        ray is upgoing (generally)
         if(z1mz0.lt.0.) then
c           in range -90 to 90 degrees - ray really going up
            BETNEW = 180. * ATAN2( X1mX0 , abs(Z1MZ0) ) / PI
         else if(z1mz0.gt.0.) then
c           ray going down
            if(x1mx0.lt.0.) then
               BETNEW = -180. + 180. * ATAN2(abs(X1mX0),Z1MZ0) / PI
            else if(x1mx0.gt.0.) then
              BETNEW = +180. - 180. * ATAN2( X1mX0 , Z1MZ0 ) / PI
            else if(x1mx0.eq.0.) then
               betnew = 180.
            end if
         else if (z1mz0.eq.0.) then
c           ray horizontal
            if(x1mx0.lt.0.) betnew = -90.
            if(x1mx0.gt.0.) betnew = +90.
         end if
      end if
 

      RETURN
      END

c---------------------------------------------------------------------

      SUBROUTINE TRIDI(N,C,D,E,B,SGNDET,INFO)

c     Tridi solves the equation JX = B for X.  J is the tridiagonal
c     jacobian whose bands here are C (subdiagonal), D (diagonal),
c     and E (superdiagonal). The solution is returned as B.
c     See reference, eqns. (11) and (14).
c     The code is from a LINPAK listing.
c     REFERENCE : LINPACK USER'S GUIDE,  J.J. DONGARRA et al,
c                 SIAM, 1979.

      INTEGER   N,        INFO

      REAL      C(N),     D(N),     E(N),      B(N),
     :          SGNDET

      INTEGER   K,        KB,       KP1,       NM1,       NM2

      REAL      T


c     initialising the sign of the determinant
      SGNDET = 1.

      INFO = 0
      C(1) = D(1)
      NM1 = N - 1
      IF(NM1.LT.1) GO TO 40
         D(1) = E(1)
         E(1) = 0.0E0
         E(N) = 0.0E0

         DO 30  K = 1,  NM1
            KP1 = K + 1
            IF(ABS(C(KP1)).LT.ABS(C(K))) GO TO 10
c              sign changes
               SGNDET = - SGNDET
               T = C(KP1)
               C(KP1) = C(K)
               C(K) = T
               T = D(KP1)
               D(KP1) = D(K)
               D(K) = T
               T = E(KP1)
               E(KP1) = E(K)
               E(K) = T
               T = B(KP1)
               B(KP1) = B(K)
               B(K) = T
10          CONTINUE

            IF(C(K).NE.0.0E0) GO TO 20
               INFO = K
               GO TO 100
20          CONTINUE

            T = -C(KP1)/C(K)
            C(KP1) = D(KP1) + T*D(K)
            D(KP1) = E(KP1) + T*E(K)
            E(KP1) = 0.0E0
            B(KP1) = B(KP1) + T*B(K)
30          CONTINUE
40    CONTINUE


      IF(C(N).NE.0.0E0) GO TO 50
         INFO = N
         GO TO 90
50    CONTINUE

      NM2 = N - 2
      B(N) = B(N)/C(N)
      IF(N.EQ.1) GO TO 80
         B(NM1) = (B(NM1) - D(NM1)*B(N))/C(NM1)
         IF(NM2.LT.1) GO TO 70
            DO 60  KB = 1,  NM2
               K = NM2 - KB + 1
               B(K) = (B(K) - D(K)*B(K+1) - E(K)*B(K+2))/C(K)
60             CONTINUE
70       CONTINUE
80    CONTINUE
90    CONTINUE
100   CONTINUE


      RETURN
      END

c-----------------------------------------------------------------------
c     End of continuation subroutines.
c-----------------------------------------------------------------------
