      DOUBLE PRECISION FUNCTION PYTHAG(A,B) 
      DOUBLE PRECISION A,B 
C 
C     FINDS DSQRT(A**2+B**2) WITHOUT OVERFLOW OR DESTRUCTIVE UNDERFLOW 
C 
      DOUBLE PRECISION P,Q,R,S,T 
      P = DMAX1(DABS(A),DABS(B)) 
      Q = DMIN1(DABS(A),DABS(B)) 
      IF (Q .EQ. 0.0D0) GO TO 20 
   10 CONTINUE 
         R = (Q/P)**2 
         T = 4.0D0 + R 
         IF (T .EQ. 4.0D0) GO TO 20 
         S = R/T 
         P = P + (2.0D0*S)*P 
         Q = S*Q 
      GO TO 10 
   20 PYTHAG = P 
      RETURN 
      END 
