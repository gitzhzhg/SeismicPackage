      SUBROUTINE CHKCST(XRAY1,ZRAY1,XRAY2,ZRAY2,N,NSEG,ICAUST)
c     Checks for caustic by looking to see if two adjacent rays
c     intersect each other on one of their segments.
c     Does not support multiple caustics (ie looks only for
c     one intersection).

      INTEGER  N,           ICAUST,      NSEG

      REAL     XRAY1(0:N),  ZRAY1(0:N),  XRAY2(0:N),  ZRAY2(0:N),
     :         DENOM,       LARGE,       SMALL

      PARAMETER( LARGE = 999999.,
     :           SMALL = .1)

cc    Local variables
      INTEGER  I
      REAL     SR1,   CR1,   SR2,   CR2,   X,   Z,
     :         X1,    X2


c     first set caustic identifier to 'no caustic'
      ICAUST = 1

c     (Note: rays can't intersect in first segment)
      DO 100 I = 2,  NSEG

c        eqn of ray segment for first ray...
         DENOM = ( ZRAY1(I) - ZRAY1(I-1))
         IF(ABS(DENOM).LT.SMALL) THEN
c           ray is horizontal
            SR1 = LARGE
            CR1 = ZRAY1(I)
         ELSE
c           slope
            SR1 = ( XRAY1(I) - XRAY1(I-1) ) / DENOM
c           intercept
            CR1 = ( XRAY1(I-1)*ZRAY1(I) - XRAY1(I)*ZRAY1(I-1) ) 
     :      / DENOM 
         END IF

c        eqn of ray segment for second ray...
         DENOM = ZRAY2(I) - ZRAY2(I-1)
         IF(ABS(DENOM).LT.SMALL) THEN
c           ray is horizontal
            SR2 = LARGE
            CR2 = ZRAY2(I)
         ELSE
c           slope
            SR2 = ( XRAY2(I) - XRAY2(I-1) ) / DENOM
c           intercept
            CR2 = ( XRAY2(I-1)*ZRAY2(I) - XRAY2(I)*ZRAY2(I-1) ) 
     :         / DENOM
         END IF


         IF(SR1.EQ.SR2) THEN
c           ray segments parallel, no intersection
         ELSE
c           intersection point
            Z = ( CR2 - CR1 ) / ( SR1 - SR2 )
            X = SR1 * Z + CR1

            X1 = XRAY1(I-1)
            X2 = XRAY1(I)
c           check to see if intersection occurs within ray segment
c           two cases, depending on whether ray is shooting to left or right
            IF(X.GE.(X1))THEN
               IF(X.LE.X2) THEN
                  ICAUST = 0
c                 not supporting multiple caustics...
                  RETURN
               END IF
            ELSE IF(X.LE.(X1)) THEN
               IF(X.GE.X2) THEN
                  ICAUST = 0
c                 not supporting multiple caustics...
                  RETURN
               END IF
            END IF
         END IF

100      CONTINUE

      RETURN
      END
