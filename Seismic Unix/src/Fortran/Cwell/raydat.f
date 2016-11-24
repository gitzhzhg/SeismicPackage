      SUBROUTINE RAYDAT(ZW,VREF,IREFL,IEVENT,AMP,PHASE,
     :           MAXEVT,MAXREF,WR1,WR2,WR3,ZRWELL,NRWELL,
     :           ICROSS,ICP1,NORDER,TC,HDWAVE)

c     Calculates those amplitude factors that depend only on
c     the raypath itself, that is, out-of-plane spreading
c     and transmission effects.  In-plane
c     spreading is calculated in the main program.
c     Reference: Two-and-one-half dimensional common shot
c                modeling, by Paul Docherty, CWP-050.

      INTEGER    IEVENT,      MAXEVT,     MAXREF,  NRWELL

      REAL       ZW,          VREF(*),    AMP,     PHASE,   
     :           WR1(NRWELL), WR2(NRWELL),         WR3(NRWELL),
     :           ZRWELL(0:NRWELL),  TC(ICROSS)

      INTEGER    IREFL(MAXEVT,0:MAXREF),   NORDER(*)

      LOGICAL    HDWAVE

      INTEGER    MAXN,       MAXNP1

      PARAMETER ( MAXN   = 40)
      PARAMETER ( MAXNP1 = MAXN + 1)

      REAL       DZ(MAXN),       DDZ(MAXN),
     :           D(MAXNP1),      DELTAX(MAXNP1),   DELTAZ(MAXNP1),
     :           V(MAXNP1)

      INTEGER    N

      COMMON /B/   DZ,           DDZ,
     :             D,            DELTAX,        DELTAZ,
     :             V,            N


cc    local     variables
c     ARG       argument of square root used in the expression
c               for the transmission coefficient
c     COSBF     cosine of emergence angle of ray and the vertical
c     COSINC()  cosine of angle between incident ray and normal
c     COSTRA()  cosine of angle between transmitted ray and normal
c     IREF      index of the array kref
c     K         loop variable
c     NP1       N + 1
c     PCRIT     argument of square root used in the expression
c               for the reflection coefficient; tells us if the
c               reflection is pre or post critical
c     R         product of reflection coefficients
c     SIGMA     running parameter along the ray
c     SQRTPC    square root of pcrit
c     T         product of transmission coefficients
c     TINY      a small velocity - close to zero
c     VALNOR    magnitude of normal to interface
c     XSECTN    factor due to changes in cross-sectional area
c               of ray tube at interfaces


      REAL       COSINC(MAXN),         COSTRA(MAXN),   COSBF,
     :           ARG,     PCRIT,
     :           R,       SIGMA,       SQRTPC,
     :           T,       TINY,        VALNOR,
     :           XSECTN,  DZW

      INTEGER    K,       IREF,    NP1,    J

      PARAMETER ( TINY = .1 )

c     Initialising
      T = 1.
      R = 1.
      PHASE = 0.
      IREF = 1
      NP1 = N + 1

c     Find reflection or transmission coefficient at each
c     intersection point.

      DO 10  K = 1,  ICROSS

c        Take scalar product of ray with normal to get
c        cosine of angle between.
         VALNOR    = SQRT( 1. + DZ(K)**2 )
         COSINC(K) = ABS( ( DELTAX(K) * DZ(K) - DELTAZ(K) )
     :              / ( D(K) * VALNOR ) )
         COSTRA(K) = ABS( ( DELTAX(K+1) * DZ(K) - DELTAZ(K+1) )
     :              / ( D(K+1) * VALNOR ) )

         IF(NORDER(K).EQ.IREFL(IEVENT,IREF)) THEN
c           this is a reflection
            IF(VREF(IREF).LE.TINY) THEN
c              a (ghost) reflection from the surface
               R = - R
               TC(K) = -1
            ELSE
               PCRIT = ( V(K) / VREF(IREF) )**2 - 1. + COSINC(K)**2
               IF(PCRIT.GE.0.) THEN
c                 pre-critical reflection
                  SQRTPC = SQRT(PCRIT)
                  TC(K) = ( COSINC(K) - SQRTPC ) / (COSINC(K)+SQRTPC)
c                 R = R * ( COSINC(K) - SQRTPC ) / (COSINC(K)+SQRTPC)
                  R = R * TC(K)
               ELSE
c                 post critical reflection
                  PHASE = PHASE - 2. * ATAN2(SQRT(-PCRIT),COSINC(K))
                  TC(K) = 1.
               END IF
            END IF
            IREF = IREF + 1
         ELSE
c           a transmission
            ARG = ( V(K)/V(K+1) )**2 - 1. + COSINC(K)**2
            IF(ARG.LT.0.) THEN
c              No transmitted ray at this angle of incidence.
c              Since the ray has already been found, this case
c              should not arise.
               AMP = 0.
               PHASE = 0.
               RETURN
            ELSE
               TC(K) = 2. * COSINC(K) / ( COSINC(K) + SQRT(ARG) )
c              T = T * 2. * COSINC(K) / ( COSINC(K) + SQRT(ARG) )
               T = T * TC(K)
            END IF
         END IF

10       CONTINUE

c     Calculating factor due to abrupt changes in cross sectional
c     area of ray tube at interfaces.
      XSECTN = 1.
      DO 120  K = 1,  ICROSS 
         XSECTN = XSECTN * COSTRA(K) / COSINC(K)
         TC(K) = TC(K) * SQRT( COSTRA(K) / COSINC(K) )
120      CONTINUE

      IF(HDWAVE)RETURN

c     Calculating sigma.
      SIGMA = 0.
      DO 100  K = 1,  ICP1
         SIGMA = SIGMA + D(K) * V(K)
100      CONTINUE

c     Finally, need the cosine of the emergence angle ray makes with normal to
c     the well.
c     if ray intersection falls outside the range of definition of the
c     well, then return (should not occur at this point).
      IF(ZW.LE.ZRWELL(0).OR.ZW.GT.ZRWELL(NRWELL-1)) THEN
         AMP = 0.
         RETURN
      END IF
      J = 1
150   IF(ZW.GT.ZRWELL(J-1)) THEN
         J = J + 1
         GO TO 150
      END IF
      J = J - 1
      DZW  = WR1(J) + 2.*WR2(J)*ZW + 3.*WR3(J)*ZW*ZW

      COSBF = ABS( DELTAX(ICP1) - DZW*DELTAZ(ICP1) ) /
     :         ( D(ICP1) * SQRT( 1. + DZW*DZW ) )

c     Do the amplitude calculation for this ray.
      AMP = R * T * SQRT( XSECTN / ( SIGMA * COSBF ) )


      RETURN
      END

*---------------------------------------------------------------

      SUBROUTINE TTIME(N,D,V,T,ICROSS)
c     Calculates the traveltime along a ray.

      INTEGER    N
      REAL       D(N+1),       V(N+1),      T

cc    local variables
c     I     loop variable
      INTEGER    I

      T = 0.
      DO 10 I = 1,  ICROSS + 1
         T = T + D(I) / V(I)
10       CONTINUE

      RETURN
      END

*---------------------------------------------------------------------
