
c     This set of subroutines is used in the shooting method.
*---------------------------------------------------------------------

      SUBROUTINE SHOOT(X,Z,BETA,PI,TRUGEO,NNEW,XSTART,XEND,
     :                 LREF,SINTHR)

cc    This subroutine shoots a ray from a given
c     source location at a given takeoff angle.  The
c     end point of the ray is not constrained to be at a
c     receiver location.
c     Snell's law is solved at each interface in turn.
c     The direction of the ray with reference to the x-z
c     coordinate system is found after each intersection
c     with an interface.  This direction is used by another
c     subroutine to find the coordinates of the next
c     intersection point.

      REAL       X(0:*),       Z(0:*),       BETA,        TRUGEO,
     :           XSTART,       XEND,         PI

      INTEGER    MAXINT,       MAXSPL,         MXSPM1,
     :           MAXN,         MAXNP1,         NNEW

      PARAMETER ( MAXINT = 20,
     :            MAXSPL = 51,
     :            MAXN   = 40)

      PARAMETER ( MAXNP1 = MAXN + 1,
     :            MXSPM1 = MAXSPL - 1)

      REAL         XINT(0:MAXINT,MAXSPL),    ZINT(0:MAXINT,MAXSPL),
     :             A0(0:MAXINT,MXSPM1),      A1(0:MAXINT,MXSPM1),
     :             A2(0:MAXINT,MXSPM1),      A3(0:MAXINT,MXSPM1),
     :             SIGN(0:MAXN)

      INTEGER      NPTS(0:MAXINT),  NINT,      NORDER(MAXN)

      COMMON /A/   XINT,          ZINT,
     :             A0,            A1,        A2,          A3,
     :             SIGN,
     :             NPTS,          NINT,      NORDER

      REAL       DZ(MAXN),        DDZ(MAXN),
     :           D(MAXNP1),       DELTAX(MAXNP1),   DELTAZ(MAXNP1),
     :           V(MAXNP1)

      INTEGER    N

      COMMON /B/   DZ,            DDZ,
     :             D,             DELTAX,        DELTAZ,
     :             V,             N

cc    Local   variables
c     DELX    step in search for next intersection point
c     DENOM   denominator in expressions for next ray segment
c     HUGE    A large number
c     J       section of splined interface
c     K       loop index
c     L       identifies splined interface
c     SGNTAN  determines the quadrants in which we search for
c             solutions (ie, whether takeoff angle is measured
c             from the upward or downward pointing vertical)
c     SINTH   sine of angle between normal and ray leaving interface
c     TANTH   tangent of angle ray makes with downward vertical
c     TRANSX  x-component of ray leaving interface
c     TRANSZ  z-component of ray leaving interface
c     XFAIL   true if can't find next intersection point

      REAL       DELX,     DENOM,     SINTH,      TANTH,
     :           TRANSX,   TRANSZ,    SGNTAN,     HUGE

      INTEGER    J,        K,         L

      LOGICAL    XFAIL

      PARAMETER( HUGE = 10000000.)


c     Find first intersection point.

      IF(ABS(BETA).EQ.180.) THEN
c        invalid angle
         NNEW = 0
         RETURN
      END IF
      IF(ABS(BETA).EQ.90.) THEN
c        tanth = beta * huge / beta
         TANTH = ABS(BETA) * HUGE / BETA
      ELSE
         TANTH = TAN( PI * BETA / 180. )
      END IF
c     measure from upward or downward vertical?
      SGNTAN = - SIGN(0)

c     use geophone spacing as step size in search
c     direction of search is direction of beta
      IF(TANTH.EQ.0.) THEN
      ELSE
         DELX = ABS( BETA * TRUGEO ) / BETA
      END IF

      CALL FINDX(X(0),Z(0),TANTH,SGNTAN,DELX,1,
     :           X(1),Z(1),XFAIL)

      IF(XFAIL) THEN
c        can't find first intersection point
c        extend ray as straight line to edge of model
         IF(DELX.GT.0.) THEN
            X(1) = XEND
         ELSE
            X(1) = XSTART
         END IF
         Z(1) = Z(0) + SGNTAN * ( X(1)-X(0) ) / TANTH
         NNEW = 1
         RETURN
      END IF

c     Find remaining intersections.

      DO 100 K = 1,  N

c        number of interface for next intersection
         L = NORDER(K)
c        find section of interface on which previous
c        intersection point lies
         J = 1
50       IF(X(K).GT.XINT(L,J)) THEN
            J = J + 1
            GO TO 50
         END IF
         J = J - 1
c        find slope at previous intersection
         DZ(K) = A1(L,J) + 2. * A2(L,J) * X(K)
     :         + 3. * A3(L,J) * X(K)**2

         DELTAX(K) = X(K) - X(K-1)
         DELTAZ(K) = Z(K) - Z(K-1)
         D(K) = SQRT( DELTAX(K)**2 + DELTAZ(K)**2 )

c        calculate sine of angle between normal and ray
c        leaving the intersection point
         DENOM = SQRT( 1. + DZ(K)**2 )

         SINTH = V(K+1) * ( DELTAX(K) + DELTAZ(K) * DZ(K) ) /
     :           ( V(K) * D(K) * DENOM )

         IF(K.EQ.LREF) THEN
c           return angle ray makes at last reflection
            SINTHR = SINTH
         END IF

         IF(ABS(SINTH).GE.1.) THEN
c           no transmitted ray beyond critical
            NNEW = K
            RETURN
         END IF

c        find component of transmitted ray in x-direction
         TRANSX = ( SINTH + SIGN(K) * SQRT( 1. - SINTH**2 )
     :            * DZ(K) ) / DENOM

c        find z-component of transmitted ray
         TRANSZ = ( SINTH * DZ(K) - SIGN(K) *
     :             SQRT( 1. - SINTH**2 ) ) / DENOM

c        find tangent with downward vertical
         IF(ABS(TRANSZ).GT.0.) THEN
            TANTH = TRANSX / TRANSZ
         ELSE
c           ray propagating horizontally (a problem for taking tangent)
            TANTH = HUGE
         END IF

c        search for next intersection in the direction
c        of the x-component
         IF(TANTH.EQ.0.) THEN
         ELSE
            DELX = ABS( TRANSX * TRUGEO ) / TRANSX
         END IF
         CALL FINDX(X(K),Z(K),TANTH,1.,DELX,K+1,
     :   X(K+1),Z(K+1),XFAIL)

         IF(XFAIL) THEN
c           failed to find next intersection
            IF(DELX.GT.0.) THEN
               X(K+1) = XEND
            ELSE
               X(K+1) = XSTART
            END IF
            Z(K+1) = Z(K) + ( X(K+1)-X(K) ) / TANTH
            NNEW = K+1
            RETURN
         END IF


100      CONTINUE



      IF(X(N+1).LT.XSTART.OR.X(N+1).GT.XEND) THEN
c        end point of ray lies outside line of receivers
         WRITE(*,*)'SHOOT - shouldn''t be here'
         NNEW = N
      END IF
      NNEW = N+1

      RETURN
      END

*---------------------------------------------------------------------

      SUBROUTINE FINDX(XK,ZK,TANTH,SGNTAN,DELX,KP1,XKP1,ZKP1,XFAIL)

c     Given a known intersection point and a ray direction,
c     this subroutine finds the intersection of the ray with
c     the next interface.  It tries for a quick solution first, using
c     Newton's method. If this fails it resorts to a simple bisection procedure
c     to solve for the intersection of a straight line (ray)
c     and a curve (interface).

      REAL       XK,      ZK,     TANTH,     DELX,     XKP1,   ZKP1,
     :           SGNTAN

      INTEGER    KP1

      LOGICAL    XFAIL

cc    local   variables
c     CLOSE   a small number
c     G       the equation we're trying to solve
c     G0      the value of g at the beginning of the search
c     ITER1   first iteration counter
c     ITER2   second iteration counter
c     MAXIT   max value for the iteration counters
c     X1,X2   bisection points
c     XKP1A   temporary storage of XKP1

      REAL      G,       G0,       X1,         X2,
     :          CLOSE,   XKP1A

      INTEGER   ITER1,   ITER2,    MAXIT

      PARAMETER ( MAXIT = 1000,
     :            CLOSE = 1. )

      XFAIL = .FALSE.
c     initialise the iteration counters
      ITER1 = 0
      ITER2 = 0
c     begin at the known intersection point
      XKP1 = XK
c     evaluate the function we're trying to make zero
      CALL FUNCG(XK,ZK,TANTH,SGNTAN,KP1,XKP1,ZKP1,G0,XFAIL)
      IF(XFAIL.OR.G0.EQ.0.) RETURN

      G = G0
c     first, look for a zero crossing
5     IF(G/G0.GT.0.) THEN
         ITER1 = ITER1 + 1
         IF(ITER1.GT.MAXIT) THEN
            XFAIL = .TRUE.
            RETURN
         END IF
         XKP1 = XKP1 + DELX
         CALL FUNCG(XK,ZK,TANTH,SGNTAN,KP1,XKP1,ZKP1,G,XFAIL)
         IF(XFAIL.OR.G.EQ.0.) RETURN
         GO TO 5
      END IF


c     try for a quick solution using newton's method
      XKP1A = XKP1
      XKP1 = XKP1 - DELX / 2.
      CALL QUICK(XK,ZK,TANTH,SGNTAN,KP1,XKP1,ZKP1,XFAIL)
      IF(.NOT.XFAIL) RETURN

      XFAIL = .FALSE.
      XKP1 = XKP1A
c     now try bisection to approach solution
      X2 = XKP1
      X1 = XKP1 - DELX

10    IF(ABS(X1-X2).GT.CLOSE) THEN
         ITER2 = ITER2 + 1
         IF(ITER2.GT.MAXIT) THEN
            XFAIL = .TRUE.
            RETURN
         END IF
         XKP1  = ( X1 + X2 ) / 2.
         CALL FUNCG(XK,ZK,TANTH,SGNTAN,KP1,XKP1,ZKP1,G,XFAIL)
         IF(XFAIL.OR.G.EQ.0.) RETURN
         IF(G/G0.GT.0.) THEN
            X1 = XKP1
         ELSE
            X2 = XKP1
         END IF
         GO TO 10
      END IF

      RETURN
      END

*-----------------------------------------------------------------

      SUBROUTINE FUNCG(XK,ZK,TANTH,SGNTAN,KP1,
     :                 XKP1,ZKP1,G,XFAIL)

c     Evaluates the function g ( the equation for the
c     intersection point of the ray and the next interface )
c     for subroutine findx.

      REAL       XK,        ZK,       SGNTAN,
     :           XKP1,      ZKP1,     TANTH,         G

      INTEGER    KP1

      LOGICAL    XFAIL

      INTEGER    MAXINT,    MAXSPL,         MXSPM1,
     :           MAXN

      PARAMETER ( MAXINT = 20,
     :            MAXSPL = 51,
     :            MAXN   = 40)

      PARAMETER ( MXSPM1 = MAXSPL - 1)

      REAL        XINT(0:MAXINT,MAXSPL),     ZINT(0:MAXINT,MAXSPL),
     :            A0(0:MAXINT,MXSPM1),       A1(0:MAXINT,MXSPM1),
     :            A2(0:MAXINT,MXSPM1),       A3(0:MAXINT,MXSPM1),
     :            SIGN(0:MAXN)

      INTEGER     NPTS(0:MAXINT),   NINT,      NORDER(MAXN)

      COMMON /A/   XINT,          ZINT,
     :             A0,            A1,        A2,          A3,
     :             SIGN,
     :             NPTS,          NINT,      NORDER

cc    local   variables
c     J       identifies section of interface
c     L       identifies interface

      INTEGER   J,           L


c     look up the number of the interface
      L = NORDER(KP1)

c     Check to see if the given value of x lies within range
c     of the model.
      IF(XKP1.LE.XINT(L,1).OR.XKP1.GT.XINT(L,NPTS(L))) THEN
         XFAIL = .TRUE.
         RETURN
      END IF

c     calculate the depth of the interface for this x
      J = 1
5     IF(XKP1.GT.XINT(L,J)) THEN
         J = J + 1
         GO TO 5
      END IF
      J = J - 1

      ZKP1 = A0(L,J) + A1(L,J) * XKP1 +
     :       A2(L,J) * XKP1**2 + A3(L,J) * XKP1**3 

      IF(TANTH.EQ.0.) THEN
         G = 0.
         RETURN
      END IF

      G = ZKP1 - ZK - SGNTAN * (XKP1 - XK) / TANTH

      RETURN
      END

c-----------------------------------------------------------------------

      SUBROUTINE QUICK(XK,ZK,TANTH,SGNTAN,KP1,
     :                 XKP1,ZKP1,XFAIL)

c     This subroutine solves for the intersection of the ray
c     with the next interface useing Newton's method.

      REAL       XK,           ZK,       
     :           XKP1,         ZKP1,     TANTH,
     :           SGNTAN

      INTEGER    KP1

      LOGICAL    XFAIL

      INTEGER    MAXINT,       MAXSPL,         MXSPM1,
     :           MAXN

      PARAMETER ( MAXINT = 20,
     :            MAXSPL = 51,
     :            MAXN   = 40)

      PARAMETER ( MXSPM1 = MAXSPL - 1)

      REAL        XINT(0:MAXINT,MAXSPL),     ZINT(0:MAXINT,MAXSPL),
     :            A0(0:MAXINT,MXSPM1),       A1(0:MAXINT,MXSPM1),
     :            A2(0:MAXINT,MXSPM1),       A3(0:MAXINT,MXSPM1),
     :            SIGN(0:MAXN)

      INTEGER     NPTS(0:MAXINT),   NINT,      NORDER(MAXN)

      COMMON /A/   XINT,          ZINT,
     :             A0,            A1,        A2,          A3,
     :             SIGN,
     :             NPTS,          NINT,      NORDER

cc    local   variables
c     BURIED  depth of the receivers below the surface when we are
c             looking for the final point on the ray. Otherwise zero.
c     DZKP1   derivative of ZKP1 w.r.t. XKP1
c     G       equation we're solving 
c     GPRIME  derivative of G w.r.t. XKP1
c     ITER    iteration counter
c     J       identifies section of interface
c     L       identifies interface
c     MAXIT   max number os Newton iterations
c     SOLN    residual of equation to be solved must be < SOLN for
c             a solution

      INTEGER   ITER, J,       L,      MAXIT
      REAL      SOLN, BURIED,  DZKP1,  G,  GPRIME

      PARAMETER(MAXIT = 10)
      PARAMETER(SOLN  = 1.)


c     look up the number of the interface
      L = NORDER(KP1)

      G = 2.*SOLN
      ITER = 0
100   IF(ABS(G).GT.SOLN) THEN
         ITER = ITER + 1

         IF(ITER.LE.MAXIT) THEN
c           Check to see if the given value of x lies within range
c           of the model.
            IF(XKP1.LE.XINT(L,1).OR.XKP1.GT.XINT(L,NPTS(L))) THEN
               XFAIL = .TRUE.
               RETURN
            END IF
c           calculate the depth of the interface for this x
            J = 1
5           IF(XKP1.GT.XINT(L,J)) THEN
               J = J + 1
               GO TO 5
            END IF
            J = J - 1
            ZKP1 = A0(L,J) + A1(L,J) * XKP1 +
     :             A2(L,J) * XKP1**2 + A3(L,J) * XKP1**3 
            DZKP1 = A1(L,J) + 2. * A2(L,J) * XKP1 +
     :              3. * A3(L,J) * XKP1**2

            G = ZKP1 - ZK - SGNTAN * (XKP1 - XK) / TANTH
            GPRIME = DZKP1 - SGNTAN / TANTH
            XKP1 = XKP1 - G / GPRIME
            GO TO 100
         ELSE
            XFAIL = .TRUE.
c           write(*,*)'quick fail'
         END IF
      END IF

      
      IF(XKP1.LE.XINT(L,1).OR.XKP1.GT.XINT(L,NPTS(L))) THEN
c        beyond limits of interface
         XFAIL = .TRUE.
         RETURN
      END IF

      RETURN
      END

c     End of shooting method routines.
c-----------------------------------------------------------------------
