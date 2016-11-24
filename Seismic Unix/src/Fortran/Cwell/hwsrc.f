      SUBROUTINE HWSRC(X1,SINTHC,IRFRCT,DS,X2,Z2,LREF,
     :UP,DOWN,BETA,SIGNDX,PI,SLAYER,XSTART,XEND,FAIL,
     :XCROSS,LEFT,RIGHT)

c     When finding head wave rays, the source is move along the interface
c     and rays are shot off at the critical angle.  This routine
c     finds the location of the next source position in this procedure.

      INTEGER    IRFRCT,   LREF,     SLAYER
      REAL       X1,  X2,  Z2,  SINTHC, DS, BETA, SIGNDX,
     :           PI,  XEND,     XSTART, XCROSS(0:*)
      LOGICAL    UP,       DOWN,     FAIL,     LEFT,   RIGHT

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

c     Local Variables:
      INTEGER   L,  J
      REAL      SLOPE,   DENOM,   DX,   TRANSX,   TRANSZ


      FAIL = .FALSE.
      L = IRFRCT
      J = 1
50    IF(X1.GT.XINT(L,J)) THEN
         J = J + 1
         GO TO 50
      END IF
      J = J - 1
c     find slope of interface
      SLOPE = A1(L,J) + 2. * A2(L,J) * X1
     :   + 3. * A3(L,J) * X1**2

      DX = DS / SQRT( 1. + SLOPE*SLOPE )
         
c     x-location of next source point on the interface
      X2 = X1 + SIGNDX * DX
      IF(X2.LE.XSTART.OR.X2.GE.XEND) THEN
         FAIL = .TRUE.
         RETURN
      END IF

c     which side of well is source on ?
      IF(X2.GE.XCROSS(IRFRCT)) THEN
         RIGHT = .TRUE.
         LEFT = .FALSE.
      ELSE
         RIGHT = .FALSE.
         LEFT = .TRUE.
      END IF

c     find z-coordinate on interface at x2        
      J = 1
100   IF(X2.GT.XINT(L,J)) THEN
         J = J + 1
         GO TO 100
      END IF
      J = J - 1

      Z2 = A0(L,J) + A1(L,J) * X2 +
     :       A2(L,J) * X2**2 + A3(L,J) * X2**3 

c     and new slope
      SLOPE = A1(L,J) + 2. * A2(L,J) * X2
     :   + 3. * A3(L,J) * X2**2


c     find component of transmitted ray in x-direction
c     -ray leaves at critical angle
      DENOM = SQRT( 1. + SLOPE**2 )
      TRANSX = ( SIGNDX * SINTHC + SIGN(LREF) * SQRT( 1. - SINTHC**2 )
     :         * SLOPE ) / DENOM

c     find z-component of transmitted ray
      TRANSZ = ( SIGNDX * SINTHC * SLOPE - SIGN(LREF) *
     :          SQRT( 1. - SINTHC**2 ) ) / DENOM


c     ...is ray going up or down from interface...
c     also set source layer number
      SIGN(0) = SIGN(LREF)
      IF(SIGN(0).GT.0.) THEN
         UP = .TRUE.
         DOWN = .FALSE.
         SLAYER = IRFRCT
      ELSE
         UP = .FALSE.
         DOWN = .TRUE.
         SLAYER = IRFRCT + 1
      END IF

c     find takeoff angle in (x,z) coordinate system
      CALL FINDBN(TRANSX,TRANSZ,PI,BETA,SIGN(0))

      RETURN
      END
c-----------------------------------------------------------

      SUBROUTINE FINDBN(X1MX0,Z1MZ0,PI,BETNEW,S)

c     Given the raypath, this subroutine finds its takeoff angle

      REAL    X1MX0,     Z1MZ0,     PI,      BETNEW,  S

c     local  variables
c     s      sign(0)


      IF(S.LT.0.) THEN
c        takeoff angle measured from downward vertical
c        ray is downgoing (generally)
         IF(Z1MZ0.GT.0.) THEN
c           in range -90 to 90 degrees - ray really going down
            BETNEW = 180. * ATAN2( X1MX0 , Z1MZ0 ) / PI
         ELSE IF(Z1MZ0.LT.0.) THEN
c           ray going up
            IF(X1MX0.LT.0.) THEN
               BETNEW = -180.+180.*ATAN2(ABS(X1MX0),ABS(Z1MZ0))/PI
            ELSE IF(X1MX0.GT.0.) THEN
              BETNEW = +180. - 180. * ATAN2(X1MX0,ABS(Z1MZ0)) / PI
            ELSE IF(X1MX0.EQ.0.) THEN
               BETNEW = 180.
            END IF
         ELSE IF (Z1MZ0.EQ.0.) THEN
c           ray horizontal
            IF(X1MX0.LT.0.) BETNEW = -90.
            IF(X1MX0.GT.0.) BETNEW = +90.
         END IF
      END IF
         
      IF(S.GT.0.) THEN
c        takeoff angle measured from upward vertical
c        ray is upgoing (generally)
         IF(Z1MZ0.LT.0.) THEN
c           in range -90 to 90 degrees - ray really going up
            BETNEW = 180. * ATAN2( X1MX0 , ABS(Z1MZ0) ) / PI
         ELSE IF(Z1MZ0.GT.0.) THEN
c           ray going down
            IF(X1MX0.LT.0.) THEN
               BETNEW = -180. + 180. * ATAN2(ABS(X1MX0),Z1MZ0) / PI
            ELSE IF(X1MX0.GT.0.) THEN
              BETNEW = +180. - 180. * ATAN2( X1MX0 , Z1MZ0 ) / PI
            ELSE IF(X1MX0.EQ.0.) THEN
               BETNEW = 180.
            END IF
         ELSE IF (Z1MZ0.EQ.0.) THEN
c           ray horizontal
            IF(X1MX0.LT.0.) BETNEW = -90.
            IF(X1MX0.GT.0.) BETNEW = +90.
         END IF
      END IF
 

      RETURN
      END

c---------------------------------------------------------------------
