      SUBROUTINE LININT(ZEND,T,AMP,PHASE,WORK,NT,D0,D1,SPFAIL,
     :           FIRSTR,LASTR,NRLAYR,ZREC,NREC,TREC,
     :           AMPREC,PHSREC,NREC1,NRECN,TURNUP,TURNDN)

c     Calculates the interpolated values of traveltime, amplitude
c     and phase at the receiver locations given tese quanitities
c     at the end points of the rays.

      INTEGER     NT,    NREC,   NRLAYR,   FIRSTR,   LASTR,
     :            NREC1, NRECN

      REAL ZEND(NT),  T(NT),    AMP(NT),     PHASE(NT),  WORK(NT),
     :     D0(NT-1),  D1(NT-1), ZREC(NREC),  TREC(NRLAYR),  
     :     PHSREC(NRLAYR),      AMPREC(NRLAYR)

      LOGICAL   SPFAIL,         TURNUP,      TURNDN

      INTEGER   I

      SPFAIL = .FALSE.
      
c     prep for line interpolation...
      CALL PREPLN(ZEND,T,AMP,PHASE,WORK,NT)

c     first do traveltimes
      CALL LINFIT(ZEND,T,NT,D0,D1,SPFAIL)
      IF(SPFAIL) RETURN

      CALL INTERP(ZEND,NT,D0,D1,
     :NRLAYR,FIRSTR,LASTR,ZREC,NREC,TREC,
     :NREC1,NRECN,TURNUP,TURNDN)
c     avoid extrapolation to negative times...
      DO 10 I = 1, NRECN - NREC1 + 1
         IF(TREC(I).LT.0.) THEN
            TREC(I) = 0.
c           write(*,*)'adjusting negative times'
         END IF
10       CONTINUE


c     now do amplitudes
      CALL LINFIT(ZEND,AMP,NT,D0,D1,SPFAIL)
      IF(SPFAIL) RETURN

      CALL INTERP(ZEND,NT,D0,D1,
     :NRLAYR,FIRSTR,LASTR,ZREC,NREC,AMPREC,
     :NREC1,NRECN,TURNUP,TURNDN)
c     avoid sign change in amplitudes caused by extrapolation
      IF(AMP(NT/2).GE.0.) THEN
c        we want all amplitudes to be positive
c        (amplitude should not change sign within a branch)
         DO 20 I = 1,  NRECN - NREC1 + 1
            IF(AMPREC(I).LT.0.) THEN 
            AMPREC(I) = 0.
c           write(*,*)'adjusting amplitudes'
            END IF
20          CONTINUE
      ELSE
         DO 30 I = 1,  NRECN - NREC1 + 1
            IF(AMPREC(I).GT.0.) THEN
            AMPREC(I) = 0.
c           write(*,*)'adjusting amplitudes'
            END IF
30          CONTINUE
      END IF

c     finally, do phase
      CALL LINFIT(ZEND,PHASE,NT,D0,D1,SPFAIL)
      IF(SPFAIL) RETURN

      CALL INTERP(ZEND,NT,D0,D1,
     :NRLAYR,FIRSTR,LASTR,ZREC,NREC,PHSREC,
     :NREC1,NRECN,TURNUP,TURNDN)

      RETURN
      END

c------------------------------------------------------------------------
      SUBROUTINE PREPLN(Z,T,AMP,PHASE,WORK,NT)
c     Orders the values from shallow to deep in the well

      INTEGER NT
      REAL    Z(NT),  T(NT), AMP(NT),  PHASE(NT),  WORK(NT)

      INTEGER  I

      IF(Z(NT).GT.Z(1)) RETURN

      DO 10 I = 1,  NT
         WORK(I) = Z(I)
10       CONTINUE
      DO 20 I = 1,  NT
         Z(I) = WORK(NT-I+1)
20       CONTINUE

      DO 30 I = 1,  NT
         WORK(I) = T(I)
30       CONTINUE
      DO 40 I = 1,  NT
         T(I) = WORK(NT-I+1)
40       CONTINUE

      DO 50 I = 1,  NT
         WORK(I) = AMP(I)
50       CONTINUE
      DO 60 I = 1,  NT
         AMP(I) = WORK(NT-I+1)
60       CONTINUE

      DO 70 I = 1,  NT
         WORK(I) = PHASE(I)
70       CONTINUE
      DO 80 I = 1,  NT
         PHASE(I) = WORK(NT-I+1)
80       CONTINUE

      RETURN
      END

c--------------------------------------------------------------

      SUBROUTINE LINFIT(Z,T,NT,D0,D1,SPFAIL)

c     Calculates the coefficients for linear interpolation.

      INTEGER       NT

      REAL  Z(NT),  T(NT),   D0(NT-1),  D1(NT-1)

      LOGICAL SPFAIL

      INTEGER I
      REAL    DENOM


      DO 10 I = 1,  NT - 1
         DENOM = Z(I+1) - Z(I)
         IF(DENOM.EQ.0.) THEN
            SPFAIL = .TRUE.
            RETURN
         END IF
c        intercept...
         D0(I) = ( T(I)*Z(I+1)-T(I+1)*Z(I) ) / DENOM
c        slope...
         D1(I) = ( T(I+1)-T(I) ) / DENOM
10       CONTINUE

      RETURN
      END

c-----------------------------------------------------------------------
      SUBROUTINE INTERP(Z,NT,D0,D1,NRLAYR,FIRSTR,LASTR,
     :           ZREC,NREC,TREC,NREC1,NRECN,TURNUP,TURNDN)

c     Linearly interpolates values using the coefficients calculated
c     by sub linfit.

      INTEGER  NT,      NRLAYR, FIRSTR,  LASTR,   NREC,
     :         NREC1,   NRECN

      REAL   Z(NT),             D0(NT-1),         D1(NT-1),
     :       ZREC(NREC),        TREC(NRLAYR)

      LOGICAL  TURNUP,  TURNDN

      INTEGER  I,       IT,     J
      LOGICAL  FIRST

c     first check for turning point at end of branch (if so, we
c     will not extrapolate beyond turning point)
      IF(TURNUP.OR.TURNDN) GO TO 200

      IT = 0
c     firstr and lastr are are the top and bottom receiver in the layer
      DO 100 I = FIRSTR,  LASTR 

c        first find the right section of the line
         IF(ZREC(I).LE.Z(1)) THEN
c           receiver is above first ray intersection
            J = 1
         ELSE IF(ZREC(I).GE.Z(NT)) THEN
c           receiver is below last ray intersection
            J = NT - 1
         ELSE
            J = 1
5           IF(ZREC(I).GT.Z(J)) THEN
              J = J + 1
              GO TO 5
            END IF
            J = J - 1
         END IF

c        linearly interpolate
         IT = IT + 1
         TREC(IT) = D0(J) + D1(J) * ZREC(I) 
      
100      CONTINUE

c     range of interpolated values
      NREC1 = FIRSTR
      NRECN = LASTR
      RETURN

200   CONTINUE

      FIRST = .TRUE.
      IT = 0
      DO 300 I = FIRSTR,  LASTR 

c        first find the right section of the line
         IF(ZREC(I).LE.Z(1)) THEN
c           receiver is above first ray intersection
            IF(TURNDN) GO TO 300
            J = 1
         ELSE IF(ZREC(I).GE.Z(NT)) THEN
c           receiver is below last ray intersection
            IF(TURNUP) GO TO 300
            J = NT - 1
         ELSE
            J = 1
10          IF(ZREC(I).GT.Z(J)) THEN
              J = J + 1
              GO TO 10
            END IF
            J = J - 1
         END IF
         IF(FIRST) THEN
c           first receiver to be interpolated to
            NREC1 = I
            FIRST = .FALSE.
         END IF

         IT = IT + 1
         TREC(IT) = D0(J) + D1(J) * ZREC(I) 
      
300      CONTINUE

c     range of interpolated values
      NRECN = NREC1 + IT - 1

      RETURN
      END

c-----------------------------------------------------------------
