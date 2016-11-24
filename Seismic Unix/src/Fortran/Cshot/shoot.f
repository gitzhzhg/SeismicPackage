
c     This set of subroutines is used in the shooting method.
*---------------------------------------------------------------------

      SUBROUTINE SHOOT(X,Z,NOCONV,BETA,PI,TRUGEO,XSTART,XEND,RECDPT,
     :                 HEAD,KREFRA,SINTHC,SGNOFF,DANOLD)

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
     :           XSTART,       XEND,         PI,          RECDPT,
     :           SINTHC,       SGNOFF,       DANOLD

      LOGICAL    NOCONV,   HEAD

      INTEGER    MAXINT,       MAXSPL,         MXSPM1,
     :           MAXN,         MAXNP1,         KREFRA

      PARAMETER ( MAXINT = 20,
     :            MAXSPL = 2001,
     :            MAXN   = 40)

      PARAMETER ( MAXNP1 = MAXN + 1,
     :            MXSPM1 = MAXSPL - 1)

      REAL         XINT(0:MAXINT,MAXSPL),    ZINT(0:MAXINT,MAXSPL),
     :             A0(0:MAXINT,MXSPM1),      A1(0:MAXINT,MXSPM1),
     :             A2(0:MAXINT,MXSPM1),      A3(0:MAXINT,MXSPM1),
     :             SIGN(0:MAXN),             CV(0:MAXINT,MAXSPL)

      INTEGER      NPTS(0:MAXINT),  NINT,      NORDER(MAXN)

      COMMON /A/   XINT,          ZINT,
     :             A0,            A1,        A2,          A3,
     :             SIGN,
     :             NPTS,          NINT,      NORDER,      CV

      REAL       DZ(MAXN),        DDZ(MAXN),
     :           D(MAXNP1),       DELTAX(MAXNP1),   DELTAZ(MAXNP1),
     :           V(MAXNP1)

      INTEGER    N

      COMMON /B/   DZ,            DDZ,
     :             D,             DELTAX,        DELTAZ,
     :             V,             N

cc    Local   variables
c     DANGLE  difference between critical angle and angle of incidence
c             of ray
c     DELX    step in search for next intersection point
c     DENOM   denominator in expressions for next ray segment
c     HUGE    A large number
c     J       section of splined interface
c     K       loop index
c     L       identifies splined interface
c     SGNCHK  there are usually 2 ray segments leaving the source which
c             intersect the refracting interface at critical.  The one
c             we need depends on the relative offset of the receivers from
c             the source.  SGNCHK makes sure the source segment is the
c             correct one for the current receivers.
c     SGNTAN  determines the quadrants in which we search for
c             solutions (ie, whether takeoff angle is measured
c             from the upward or downward pointing vertical)
c     SINTH   sine of angle between normal and ray leaving interface
c     TANTH   tangent of angle ray makes with downward vertical
c     TRANSX  x-component of ray leaving interface
c     TRANSZ  z-component of ray leaving interface
c     XFAIL   true if can't find next intersection point

      REAL       DELX,     DENOM,     SINTH,      TANTH,
     :           TRANSX,   TRANSZ,    SGNTAN,     HUGE,
     :           DANGLE,   SGNCHK,    DXM1XM,     DXM1X,
     :           DXXM,     AM1,       BM1

      INTEGER    J,        K,         L

      LOGICAL    XFAIL

      PARAMETER( HUGE = 10000000.)


      NOCONV = .FALSE.
c     Find first intersection point.

      IF(ABS(BETA).EQ.180.) THEN
c        invalid angle
         NOCONV = .TRUE.
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

      CALL FINDX(X(0),Z(0),TANTH,SGNTAN,DELX,1,N,
     :           X(1),Z(1),XFAIL,RECDPT)

      IF(XFAIL) THEN
c        can't find first intersection point
         NOCONV = .TRUE.
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
c **************************************************************************
c  The following calculation of the position of the interface was changed by
c  E.Jenner and T.Salinas, CWP July 1996, to increase the accuracy.
c  See comments in 'splines.f', subroutine CUSPLN for details.
c **************************************************************************
c        DZ(K) = A1(L,J) + 2. * A2(L,J) * X(K)
c    :         + 3. * A3(L,J) * X(K)**2

         IF(NPTS(L).EQ.2) THEN

            DZ(K) = A1(L,J) + 2. * A2(L,J) * X(K)
     :            + 3. * A3(L,J) * X(K)**2

         ELSE

            DXM1XM = XINT(L,J+1) - XINT(L,J)
            BM1 = ZINT(L,J+1) / DXM1XM - CV(L,J+1) * DXM1XM / 6
            AM1 = ZINT(L,J) / DXM1XM - CV(L,J) * DXM1XM / 6
            DXM1X = XINT(L,J+1) - X(K)
            DXXM = X(K) - XINT(L,J)

            DZ(K)=(-CV(L,J)*DXM1X**2+CV(L,J+1)*DXXM**2)/(2*DXM1XM)
     $             - AM1 + BM1

         ENDIF

         DELTAX(K) = X(K) - X(K-1)
         DELTAZ(K) = Z(K) - Z(K-1)
         D(K) = SQRT( DELTAX(K)**2 + DELTAZ(K)**2 )

c        calculate sine of angle between normal and ray
c        leaving the intersection point
         DENOM = SQRT( 1. + DZ(K)**2 )

         SINTH = V(K+1) * ( DELTAX(K) + DELTAZ(K) * DZ(K) ) /
     :           ( V(K) * D(K) * DENOM )

ccc------The following is a patch for headwaves
         IF(HEAD.AND.K.EQ.KREFRA) THEN
c           this is the intersection at the refracting interface
c           check for correct source segment for this set of receivers
c           sinth must have same sign as offset
            SGNCHK = SINTH / SGNOFF
            IF(SGNCHK.LT.0.) THEN
               NOCONV = .TRUE.
            ELSE
c              now check to see how close to critical 
c              look for ray to pass through the critical angle
               DANGLE = (ASIN(ABS(SINTH)) - ASIN(SINTHC))
               DANGLE = DANGLE * 180. / PI
               IF(DANGLE.EQ.0.)THEN
c                 sign change - actually at critical
               ELSE
                  IF(DANOLD/DANGLE.LT.0.) THEN
c                    sign change - passed through critical
                  ELSE
c                    haven't passed through critical yet
c                    shoot another ray
                     DANOLD = DANGLE
                     NOCONV = .TRUE.
                  END IF
               END IF
            END IF
            RETURN
         END IF
ccc------End of head wave patch
               
         IF(ABS(SINTH).GE.1.) THEN
c           no transmitted ray beyond critical
            NOCONV = .TRUE.
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
         CALL FINDX(X(K),Z(K),TANTH,1.,DELX,K+1,N,
     :   X(K+1),Z(K+1),XFAIL,RECDPT)

         IF(XFAIL) THEN
c           failed to find next intersection
            NOCONV = .TRUE.
            RETURN
         END IF


100      CONTINUE


c     if(headwv) return

      IF(X(N+1).LT.XSTART.OR.X(N+1).GT.XEND) THEN
c        end point of ray lies outside line of receivers
         NOCONV = .TRUE.
      END IF

      RETURN
      END

*---------------------------------------------------------------------

      SUBROUTINE FINDX(XK,ZK,TANTH,SGNTAN,DELX,KP1,N,XKP1,ZKP1,XFAIL,
     :                 RECDPT)

c     Given a known intersection point and a ray direction,
c     this subroutine finds the intersection of the ray with
c     the next interface.  It tries for a quick solution first, using
c     Newton's method. If this fails it resorts to a simple bisection procedure
c     to solve for the intersection of a straight line (ray)
c     and a curve (interface).

      REAL       XK,      ZK,     TANTH,     DELX,     XKP1,   ZKP1,
     :           SGNTAN,  RECDPT

      INTEGER    KP1,   N

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
      CALL FUNCG(XK,ZK,TANTH,SGNTAN,KP1,N,XKP1,ZKP1,G0,XFAIL,RECDPT)
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
         CALL FUNCG(XK,ZK,TANTH,SGNTAN,KP1,N,XKP1,ZKP1,G,XFAIL,RECDPT)
         IF(XFAIL.OR.G.EQ.0.) RETURN
         GO TO 5
      END IF


c     try for a quick solution using newton's method
      XKP1A = XKP1
      XKP1 = XKP1 - DELX / 2.
      CALL QUICK(XK,ZK,TANTH,SGNTAN,KP1,N,XKP1,ZKP1,XFAIL,RECDPT)
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
         CALL FUNCG(XK,ZK,TANTH,SGNTAN,KP1,N,XKP1,ZKP1,G,XFAIL,RECDPT)
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

      SUBROUTINE FUNCG(XK,ZK,TANTH,SGNTAN,KP1,N,
     :                 XKP1,ZKP1,G,XFAIL,RECDPT)

c     Evaluates the function g ( the equation for the
c     intersection point of the ray and the next interface )
c     for subroutine findx.

      REAL       XK,        ZK,       RECDPT,   SGNTAN,
     :           XKP1,      ZKP1,     TANTH,         G

      INTEGER    KP1,       N

      LOGICAL    XFAIL

      INTEGER    MAXINT,    MAXSPL,         MXSPM1,
     :           MAXN

      PARAMETER ( MAXINT = 20,
     :            MAXSPL = 2001,
     :            MAXN   = 40)

      PARAMETER ( MXSPM1 = MAXSPL - 1)

      REAL        XINT(0:MAXINT,MAXSPL),     ZINT(0:MAXINT,MAXSPL),
     :            A0(0:MAXINT,MXSPM1),       A1(0:MAXINT,MXSPM1),
     :            A2(0:MAXINT,MXSPM1),       A3(0:MAXINT,MXSPM1),
     :            SIGN(0:MAXN),              CV(0:MAXINT,MAXSPL)

      INTEGER     NPTS(0:MAXINT),   NINT,      NORDER(MAXN)

      COMMON /A/   XINT,          ZINT,
     :             A0,            A1,        A2,          A3,
     :             SIGN,
     :             NPTS,          NINT,      NORDER,      CV

cc    local   variables
c     BURIED  depth of the receivers below the surface when we are
c             looking for the final point on the ray. Otherwise zero.
c     J       identifies section of interface
c     L       identifies interface

      REAL      BURIED,    DXM1XM,     DXM1X,
     :          DXXM,      AM1,        BM1
      INTEGER   J,           L


      IF(KP1.EQ.N+1) THEN
         L = 0
         BURIED = RECDPT
      ELSE
c        look up the number of the interface
         L = NORDER(KP1)
         BURIED = 0.
      END IF

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
c **************************************************************************
c  The following calculation of the position of the interface was changed by
c  E.Jenner and T.Salinas, CWP July 1996, to increase the accuracy.
c  See comments in 'splines.f', subroutine CUSPLN for details.
c **************************************************************************
c     ZKP1 = A0(L,J) + A1(L,J) * XKP1 +
c    :       A2(L,J) * XKP1**2 + A3(L,J) * XKP1**3 + buried

      IF(NPTS(L).EQ.2) THEN

         ZKP1 = A0(L,J) + A1(L,J) * XKP1 +
     :          A2(L,J) * XKP1**2 + A3(L,J) * XKP1**3 + BURIED

      ELSE

         DXM1XM = XINT(L,J+1) - XINT(L,J)
         BM1 = ZINT(L,J+1) / DXM1XM - CV(L,J+1) * DXM1XM / 6
         AM1 = ZINT(L,J) / DXM1XM - CV(L,J) * DXM1XM / 6
         DXM1X = XINT(L,J+1) - XKP1
         DXXM = XKP1 - XINT(L,J)

         ZKP1=(CV(L,J)*DXM1X**3+CV(L,J+1)*DXXM**3)/(6*DXM1XM)
     $        + AM1 * DXM1X + BM1 * DXXM + BURIED

      ENDIF

      IF(TANTH.EQ.0.) THEN
         G = 0.
         RETURN
      END IF

      G = ZKP1 - ZK - SGNTAN * (XKP1 - XK) / TANTH

      RETURN
      END

c-----------------------------------------------------------------------

      SUBROUTINE QUICK(XK,ZK,TANTH,SGNTAN,KP1,N,
     :                 XKP1,ZKP1,XFAIL,RECDPT)

c     This subroutine solves for the intersection of the ray
c     with the next interface, when shooting.  It uses Newton's method.

      REAL       XK,           ZK,       
     :           XKP1,         ZKP1,     TANTH,
     :           SGNTAN,       RECDPT,   DXM1XM,   DXM1X,
     :           DXXM,         AM1,      BM1

      INTEGER    KP1,          N

      LOGICAL    XFAIL

      INTEGER    MAXINT,       MAXSPL,         MXSPM1,
     :           MAXN

      PARAMETER ( MAXINT = 20,
     :            MAXSPL = 2001,
     :            MAXN   = 40)

      PARAMETER ( MXSPM1 = MAXSPL - 1)

      REAL        XINT(0:MAXINT,MAXSPL),     ZINT(0:MAXINT,MAXSPL),
     :            A0(0:MAXINT,MXSPM1),       A1(0:MAXINT,MXSPM1),
     :            A2(0:MAXINT,MXSPM1),       A3(0:MAXINT,MXSPM1),
     :            SIGN(0:MAXN),              CV(0:MAXINT,MAXSPL)

      INTEGER     NPTS(0:MAXINT),   NINT,      NORDER(MAXN)

      COMMON /A/   XINT,          ZINT,
     :             A0,            A1,        A2,          A3,
     :             SIGN,
     :             NPTS,          NINT,      NORDER,      CV

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


      IF(KP1.EQ.N+1) THEN
c        looking for end point of ray at receivers
         L = 0
         BURIED = RECDPT
      ELSE
c        look up the number of the interface
         L = NORDER(KP1)
         BURIED = 0.
      END IF

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
c **************************************************************************
c  The following calculation of the position of the interface was changed by
c  E.Jenner and T.Salinas, CWP July 1996, to increase the accuracy.
c  See comments in 'splines.f', subroutine CUSPLN for details.
c **************************************************************************
c           ZKP1 = A0(L,J) + A1(L,J) * XKP1 +
c    :             A2(L,J) * XKP1**2 + A3(L,J) * XKP1**3 + buried
c           dZKP1 = A1(L,J) + 2. * A2(L,J) * XKP1 +
c    :              3. * A3(L,J) * XKP1**2

           IF(NPTS(L).EQ.2) THEN

               ZKP1 = A0(L,J) + A1(L,J) * XKP1 +
     :                A2(L,J) * XKP1**2 + A3(L,J) * XKP1**3 + BURIED
               DZKP1 = A1(L,J) + 2. * A2(L,J) * XKP1 +
     :                 3. * A3(L,J) * XKP1**2

            ELSE

               DXM1XM = XINT(L,J+1) - XINT(L,J)
               BM1 = ZINT(L,J+1) / DXM1XM - CV(L,J+1) * DXM1XM / 6
               AM1 = ZINT(L,J) / DXM1XM - CV(L,J) * DXM1XM / 6
               DXM1X = XINT(L,J+1) - XKP1
               DXXM = XKP1 - XINT(L,J)

               ZKP1=(CV(L,J)*DXM1X**3+CV(L,J+1)*DXXM**3)/(6*DXM1XM)
     $               + AM1 * DXM1X + BM1 * DXXM + BURIED

               DZKP1=(-CV(L,J)*DXM1X**2+CV(L,J+1)*DXXM**2)/(2*DXM1XM)
     $               - AM1 + BM1


            ENDIF

            G = ZKP1 - ZK - SGNTAN * (XKP1 - XK) / TANTH
            GPRIME = DZKP1 - SGNTAN / TANTH
            XKP1 = XKP1 - G / GPRIME
            GO TO 100
         ELSE
            XFAIL = .TRUE.
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
