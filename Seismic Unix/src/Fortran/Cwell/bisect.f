      SUBROUTINE BISECT(BETA,DELTAB,THETAC,SINTHR,PI,
     :            XSTART,XEND,TRUGEO,LREF,X,Z)


      REAL       X(0:*),       Z(0:*),       BETA,        TRUGEO,
     :           XSTART,       XEND,         PI,          DELTAB,
     :           THETAC,       SINTHR

      INTEGER    LREF


cc    local   variables
c     CLOSE   a small number
c     G       the equation we're trying to solve
c     G0      the value of g at the beginning of the search
c     ITER1   first iteration counter
c     ITER2   second iteration counter
c     MAXIT   max value for the iteration counters
c     X1,X2   bisection points

      REAL      G,       G0,       X1,         X2,
     :          CLOSE,   XNEW

      INTEGER   ITER1,   ITER2,    MAXIT,      NNEW

      LOGICAL    XFAIL

      PARAMETER ( MAXIT = 1000,
     :            CLOSE = .01 )


      XFAIL = .FALSE.
c     initialise the iteration counters
      ITER1 = 0
      ITER2 = 0

      G0 = 180. * ( THETAC - ASIN(ABS(SINTHR)) )/PI
      IF(G0.EQ.0.) RETURN

      G = G0
      XFAIL = .FALSE.
c     use bisection to approach solution
      X2 = BETA - DELTAB
      X1 = BETA 

10    IF(ABS(X1-X2).GT.CLOSE) THEN
         ITER2 = ITER2 + 1
         IF(ITER2.GT.MAXIT) THEN
            XFAIL = .TRUE.
            WRITE(*,*)'max iter',ITER2
            RETURN
         END IF
         XNEW  = ( X1 + X2 ) / 2.
         CALL SHOOT(X,Z,XNEW,PI,TRUGEO,NNEW,XSTART,XEND,
     :   LREF,SINTHR)
         IF(NNEW.LE.LREF) XFAIL = .TRUE.
         G = 180. * ( THETAC - ASIN(ABS(SINTHR)) )/PI
         IF(XFAIL.OR.G.EQ.0.) THEN
c           write(*,*)'xfail or 0'
            RETURN
         END IF
         IF(G/G0.GT.0.) THEN
            X1 = XNEW
         ELSE
            X2 = XNEW
         END IF
         GO TO 10
      END IF

      RETURN
      END

*-----------------------------------------------------------------
