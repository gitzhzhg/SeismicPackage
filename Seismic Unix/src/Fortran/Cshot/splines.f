      SUBROUTINE CUSPLN(NINT,X,Z,NPTS,A0,A1,A2,A3,FAIL,CV)


c     This subroutine fits a cubic spline through the points
c     defining each interface.  The curvatures at each end are
c     taken to be zero.  The remaining curvatures are found by
c     inverting a tridiagonal matrix.  Knowing the curvatures we
c     evaluate the spline coefficients.
c     Reference: Numerical Computing and Mathematical Analysis
c                by Stephen M. Pizer.  Science Research Associates, Inc.
c                Pages 307-311.



      INTEGER     MAXINT,                  MXSPM1

c     MXSPM1 should equal MAXSPL in main, less one
      PARAMETER ( MAXINT = 20,
     :            MXSPM1 = 2000)

      REAL        X(0:MAXINT,0:MXSPM1),    Z(0:MAXINT,0:MXSPM1),
     :            A0(0:MAXINT,MXSPM1),     A1(0:MAXINT,MXSPM1),
     :            A2(0:MAXINT,MXSPM1),     A3(0:MAXINT,MXSPM1),
     :            CV(0:MAXINT,0:MXSPM1)


      INTEGER     NINT,                    NPTS(0:NINT)

      LOGICAL     FAIL



cc    local   variables
c     B()     right hand side of matrix equation for curvatures
c     C()     subdiagonal of matrix to be solved for curvatures
c     CV()    curvatures at spline points
c     D()     diagonal of matrix to be solve for curvatues
c     DX      difference in x-coordinates
c     DX2     DX sqaured
c     E()     superdiagonal of matrix to be solved for curvatures
c     I       loop variable
c     INFO    indicates if matrix is singular
c     J       loop variable over interfaces
c     M       loop variable
c     MXSPM1  maximum number of points in interface, less one
c     N       number of unknown curvatures
c     SGNDET  required by sub. tridi, not used here
c     STDOUT  unit number of standard output for error messages




      REAL      C(MXSPM1),               D(MXSPM1),
     :          E(MXSPM1),               B(MXSPM1),
     :          SGNDET,                  DX,   DX2
c    :          CV(0:MXSPM1),            SGNDET,     dx,   dx2

      INTEGER   I,     INFO,     J,      N,     M


      FAIL = .FALSE.

      DO 200 J = 0,  NINT


         IF(NPTS(J).EQ.2) THEN

c           Interface is a straight line.

            A0(J,1) = ( Z(J,0)*X(J,1) - Z(J,1)*X(J,0) ) /
     :                                   ( X(J,1) - X(J,0) )
            A1(J,1) = ( Z(J,1) - Z(J,0) ) / ( X(J,1) - X(J,0) )
            A2(J,1) = 0.
            A3(J,1) = 0.

            GO TO 200

         END IF

c        evaluating bands of tridiagonal matrix, to be inverted
c        for the curvatures ( see reference, eqn. (142) )

c        N is the number of unknown curvatures
         N = NPTS(J) - 2

c        diagonal
         DO 50  I = 1,  N
            D(I) = 2. * ( X(J,I+1) - X(J,I-1) )
50          CONTINUE

c        superdiagonal
         DO 60  I = 1,  N-1
            E(I) = X(J,I+1) - X(J,I)
60          CONTINUE

c        subdiagonal
         DO 70  I = 2,  N
            C(I) = X(J,I) - X(J,I-1)
70          CONTINUE

c        right hand side
         DO 80  I = 1,  N
            B(I) = 6. * ( (Z(J,I+1)-Z(J,I)) / (X(J,I+1)-X(J,I))
     :                - (Z(J,I)-Z(J,I-1)) / (X(J,I)-X(J,I-1)) )
80          CONTINUE



c        invert the matrix
         CALL TRIDI(N,C,D,E,B,SGNDET,INFO)
         IF(INFO.NE.0) THEN
c           failed to fit spline
            FAIL = .TRUE.
            RETURN
         END IF


c        curvature at end points is set to zero
c        CV(0) = 0.
c        CV(N+1) = 0.

         CV(J,0) = 0.
         CV(J,N+1) = 0.

c        set the remaining curvatures found by tridi.
         DO 90  I = 1, N
c           CV(I) = B(I)
            CV(J,I) = B(I)
90          CONTINUE

c ***********************************************************************
c Calculation of the interface positions was changed by E.Jenner and 
c T.Salinas, CWP, July 1996.  Expanding the product terms in reference
c equ. (132) results in inaccuracies in the calculation at large values
c of X.  This is because X**3 can get very large and so can the product
c a3*(X**3).  This means that a0 must get large to compensate and in fact
c be extremely accurate.
c In some of our models (e.g. a sinusoidal layer) a0 became greater than 
c 10e+10 resulting in a desired accuracy of 12 significant figures to place
c the interface to the nearest cm or so.  The inaccuracies in the positioning
c of the reflector resulted in the code being unable to find rays to some
c receivers.
c Therefore instead of calculating coefficients for each point
c on the interface and multiplying by x, x**2, X**3, the position of the
c interface is calculated using the curvatures.  Instead of using the 
c value of X**3 which can get large, (X-Xn)**3 and (Xn+1 - X)**3 are used.
c ************************************************************************
c        using the curvatures, solve for the spline coefficients
c        here we have expanded the product terms in reference eqn. (132)

c        DO 100  M = 0,  N

c           dx = x(j,m+1) - x(j,m)
c           dx2 = dx * dx

c           a0(j,m+1) = ( CV(M)*X(J,M+1)**3 - CV(M+1)*X(J,M)**3
c    :                + 6.*Z(J,M)*X(J,M+1)
c    :                - CV(M)*X(J,M+1) * dx2
c    :                - 6.*Z(J,M+1)*X(J,M) + CV(M+1)*X(J,M)
c    :                * dx2 ) / ( 6.* dx )


c           a1(j,m+1) = (-.5*CV(M)*X(J,M+1)**2 + .5*CV(M+1)*X(J,M)**2
c    :          -Z(J,M) + (CV(M) * dx2 ) / 6. + Z(J,M+1)
c    :          - CV(M+1) * dx2 / 6. ) / dx


c           a2(j,m+1) = (CV(M)*X(J,M+1)-CV(M+1)*X(J,M) )
c    :                           /(2.* dx)


c           a3(j,m+1) = ( CV(M+1) - CV(M) ) / (6.*dx)


c100         CONTINUE



200      CONTINUE


      RETURN

      END



*------------------------------------------------------------------------


      SUBROUTINE CUSPLW(X,Z,NPTS,A0,A1,A2,A3,FAIL)


c     This subroutine fits a cubic spline through the points
c     defining the well.  The code is the same as in CUSPLN
c     except that the spline coeffs are 1-D arrays and that x and z
c     are reversed in the calling sequence



      INTEGER       MXSPM1

c     MXSPM1 should equal MAXSPL in main, less one
      PARAMETER ( MXSPM1 = 50)

      REAL        X(0:MXSPM1),    Z(0:MXSPM1),
     :            A0(MXSPM1),     A1(MXSPM1),
     :            A2(MXSPM1),     A3(MXSPM1)

      INTEGER     NPTS

      LOGICAL   FAIL



cc    local   variables
c     B()     right hand side of matrix equation for curvatures
c     C()     subdiagonal of matrix to be solved for curvatures
c     CV()    curvatures at spline points
c     D()     diagonal of matrix to be solve for curvatues
c     E()     superdiagonal of matrix to be solved for curvatures
c     I       loop variable
c     INFO    indicates if matrix is singular
c     J       loop variable over interfaces
c     M       loop variable
c     MXSPM1  maximum number of points in well, less one
c     N       number of unknown curvatures
c     SGNDET  required by sub. tridi, not used here
c     STDOUT  unit number of standard output for error messages




      REAL      C(MXSPM1),               D(MXSPM1),
     :          E(MXSPM1),               B(MXSPM1),
     :          CV(0:MXSPM1),            SGNDET,    DX,   DX2

      INTEGER   I,     INFO,     J,      N,     M



      FAIL = .FALSE.

      IF(NPTS.EQ.2) THEN

c        well is a straight line.

         A0(1) = ( Z(0)*X(1) - Z(1)*X(0) ) /
     :                                ( X(1) - X(0) )
         A1(1) = ( Z(1) - Z(0) ) / ( X(1) - X(0) )
         A2(1) = 0.
         A3(1) = 0.

         GO TO 200

      END IF

c     evaluating bands of tridiagonal matrix, to be inverted
c     for the curvatures ( see reference, eqn. (142) )

c     n is the number of unknown curvatures
      N = NPTS - 2

c     diagonal
      DO 50  I = 1,  N
         D(I) = 2. * ( X(I+1) - X(I-1) )
50       CONTINUE

c     superdiagonal
      DO 60  I = 1,  N-1
         E(I) = X(I+1) - X(I)
60       CONTINUE

c     subdiagonal
      DO 70  I = 2,  N
         C(I) = X(I) - X(I-1)
70       CONTINUE

c     right hand side
      DO 80  I = 1,  N
         B(I) = 6. * ( (Z(I+1)-Z(I)) / (X(I+1)-X(I))
     :             - (Z(I)-Z(I-1)) / (X(I)-X(I-1)) )
80       CONTINUE



c     invert the matrix
      CALL TRIDI(N,C,D,E,B,SGNDET,INFO)
      IF(INFO.NE.0) THEN
c        failed to fit spline.
         FAIL = .TRUE.
         RETURN
      END IF


c     curvature at end points is set to zero
      CV(0) = 0.
      CV(N+1) = 0.

c     set the remaining curvatures found by tridi.
      DO 90  I = 1, N
         CV(I) = B(I)
90       CONTINUE




c     using the curvatures, solve for the spline coefficients
c     here we have expanded the product terms in reference eqn. (132)

      DO 100  M = 0,  N

         DX = X(M+1)-X(M)
         DX2 = DX * DX

         A0(M+1) = ( CV(M)*X(M+1)**3 - CV(M+1)*X(M)**3
     :             + 6.*Z(M)*X(M+1)
     :             - CV(M)*X(M+1)*DX2
     :             - 6.*Z(M+1)*X(M) + CV(M+1)*X(M)
     :             * DX2 ) / ( 6.*DX )


         A1(M+1) = ( -.5*CV(M)*X(M+1)*X(M+1) + .5*CV(M+1)*X(M)*X(M)
     :       -Z(M) + ( CV(M)*DX2 ) / 6. + Z(M+1)
     :       - CV(M+1)*DX2 / 6. ) / DX


         A2(M+1) = (CV(M)*X(M+1)-CV(M+1)*X(M) )
     :                        /(2.*DX)


         A3(M+1) = ( CV(M+1) - CV(M) ) / (6.*DX)


100      CONTINUE



200   CONTINUE


      RETURN

      END

*-------------------------------------------------------------------


