c-----------------------------------------------------------------

      SUBROUTINE RAYDAT(XNP1,VREF,IREFL,IEVENT,AMP,PHASE,TCOEFF,
     :                  MAXEVT,MAXREF)

c     Calculates those amplitude factors that depend only on
c     the raypath itself, that is, out-of-plane spreading
c     and transmission effects.  In-plane
c     spreading is calculated in the main program.
c     Reference: Two-and-one-half dimensional common shot
c                modeling, by Paul Docherty, CWP-050.

      REAL       XNP1,       VREF(*),    AMP,     PHASE,   TCOEFF
      INTEGER    IEVENT,     MAXEVT,     MAXREF
      INTEGER    IREFL(MAXEVT,0:MAXREF)

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


cc    local     variables
c     ARG       argument of square root used in the expression
c               for the transmission coefficient
c     COSBF     cosine of emergence angle of ray and the vertical
c     COSINC()  cosine of angle between incident ray and normal
c     COSTRA()  cosine of angle between transmitted ray and normal
c     IREF      index of the array kref
c     J,K       counters
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
     :           XSECTN

      INTEGER    J,  K,   IREF,    NP1

      PARAMETER ( TINY = .1 )

c     Initialising
      T = 1.
      R = 1.
      PHASE = 0.
      IREF = 1
      NP1 = N + 1

c     Find reflection or transmission coefficient at each
c     intersection point.

      DO 10  K = 1,  N

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
            ELSE
               PCRIT = ( V(K) / VREF(IREF) )**2 - 1. + COSINC(K)**2
               IF(PCRIT.GE.0.) THEN
c                 pre-critical reflection
                  SQRTPC = SQRT(PCRIT)
                  R = R * ( COSINC(K) - SQRTPC ) / (COSINC(K) + SQRTPC)
               ELSE
c                 post critical reflection
                  PHASE = PHASE - 2. * ATAN2( SQRT(-PCRIT),COSINC(K) )
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
               T = T * 2. * COSINC(K) / ( COSINC(K) + SQRT(ARG) )
            END IF
         END IF

10       CONTINUE


c     Calculating sigma.
      SIGMA = 0.
      DO 100  K = 1,  NP1  
         SIGMA = SIGMA + D(K) * V(K)
100      CONTINUE

c     Calculating factor due to abrupt changes in cross sectional
c     area of ray tube at interfaces.
      XSECTN = 1.
      DO 120  K = 1,  N
         XSECTN = XSECTN * COSTRA(K) / COSINC(K)
120      CONTINUE

c     Finally, need the cosine of the emergence angle ray makes with normal to
c     upper surface.
c     Next line works only for flat upper surface
c     COSBF = ABS( DELTAZ(N+1) / D(N+1) )
c     Need to do following for curvy upper surface...
c     finding the section of splined upper surface where ray emerges
c     and evaluating slope there 
c     if x falls outside the range of definition of the
c     splined interface, then return (should not occur at this point).
      IF(XNP1.LE.XINT(0,1).OR.XNP1.GT.XINT(0,NPTS(0))) THEN
         AMP = 0.
         RETURN
      END IF
      J = 1
150   IF(XNP1.GT.XINT(0,J)) THEN
         J = J + 1
         GO TO 150
      END IF
      J = J - 1
c **************************************************************************
c  The following calculation of the position of the interface was changed by
c  E.Jenner and T.Salinas, CWP July 1996, to increase the accuracy.
c  See comments in 'splines.f', subroutine CUSPLN for details.
c **************************************************************************
c     DZ(np1)  = A1(0,J) + 2.* A2(0,J) * Xnp1 + 
c    :           3. * A3(0,J) * Xnp1**2

      IF(NPTS(0).EQ.2) THEN

         DZ(NP1)  = A1(0,J) + 2.* A2(0,J) * XNP1 +
     :              3. * A3(0,J) * XNP1**2

      ELSE

         DXM1XM = XINT(0,J+1) - XINT(0,J)
         BM1 = ZINT(0,J+1) / DXM1XM - CV(0,J+1) * DXM1XM / 6
         AM1 = ZINT(0,J) / DXM1XM - CV(0,J) * DXM1XM / 6
         DXM1X = XINT(0,J+1) - XNP1
         DXXM = XNP1 - XINT(0,J)

         DZ(NP1)=(-CV(0,J)*DXM1X**2+CV(0,J+1)*DXXM**2)/(2*DXM1XM)
     $         - AM1 + BM1

      ENDIF

      COSBF = ABS( DELTAX(NP1)*DZ(NP1) - DELTAZ(NP1) ) /
     :         ( D(NP1) * SQRT( 1. + DZ(NP1)*DZ(NP1) ) )

c     Do the amplitude calculation for this ray.
      AMP = R * T * SQRT( XSECTN / ( SIGMA * COSBF ) )
c     need tcoeff for head waves
      TCOEFF = T * SQRT(XSECTN)


      RETURN
      END

*---------------------------------------------------------------

      SUBROUTINE TTIME(N,D,V,T)
c     Calculates the traveltime along a ray.

      INTEGER    N
      REAL       D(N+1),       V(N+1),      T

cc    local variables
c     I     loop variable
      INTEGER    I

      T = 0.
      DO 10 I = 1,  N + 1
         T = T + D(I) / V(I)
10       CONTINUE

      RETURN
      END

*---------------------------------------------------------------------

      SUBROUTINE DETJAC(X,Z,N,DETJ)
c     Evaluates the determinant of the Jacobian.  The sign of this
c     determinant tells us if the ray has gone through a caustic.

      INTEGER   N

      REAL      X(0:N+1),     Z(0:N+1),      DETJ

cc    local    variables
c     DUMMY    array required by subroutine tridi, not used here.
c     DJ()     diagonal of the tridiagonal matrix
c     I        loop variable
c     INFO     tells us if the jacobian is singular
c     MAXN     max value of N
c     SGNDET   tracks the sign of the jacobian
c     SPLNOK   true if X lies within range of splined interfaces
c     SUBDJ()  subdiagonal of tridiagonal matrix
c     SUPDJ()  superdiagonal of tridiagonal matrix
c     Z1       needed for the call to eval, not used here.

      INTEGER    I,     MAXN,      INFO

      PARAMETER ( MAXN = 40)

      REAL      SUBDJ(MAXN),     DJ(MAXN),
     :          SUPDJ(MAXN),     DUMMY(MAXN),
     :          SGNDET,          Z1

      LOGICAL   SPLNOK


c     Evaluate some necessary quantities.
      CALL EVAL(X,Z,SPLNOK)

c     Evaluate the three bands of the Jacobian.
      CALL JACOB(SUBDJ,DJ,SUPDJ,.FALSE.)

c     Calculate the determinant of the Jacobian.

      IF(N.EQ.1) THEN
         DETJ = DJ(1)
      ELSE
         DO 10  I = 1,  N
            DUMMY(I) = 0.
10          CONTINUE
         CALL TRIDI(N,SUBDJ,DJ,SUPDJ,DUMMY,SGNDET,INFO)
         DETJ = 1.
         DO 20  I = 1,  N
            DETJ = DETJ * SUBDJ(I)
20          CONTINUE
         DETJ = DETJ * SGNDET
      END IF

      RETURN
      END

c     End of routines that calculate ray information.
*-----------------------------------------------------------------------

