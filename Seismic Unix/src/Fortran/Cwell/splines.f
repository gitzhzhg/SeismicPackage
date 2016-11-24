      SUBROUTINE CUSPLN(NINT,X,Z,NPTS,A0,A1,A2,A3,FAIL)


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
     :            MXSPM1 = 50)

      REAL        X(0:MAXINT,0:MXSPM1),    Z(0:MAXINT,0:MXSPM1),
     :            A0(0:MAXINT,MXSPM1),     A1(0:MAXINT,MXSPM1),
     :            A2(0:MAXINT,MXSPM1),     A3(0:MAXINT,MXSPM1)

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
     :          CV(0:MXSPM1),            SGNDET,     DX,   DX2

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
         CV(0) = 0.
         CV(N+1) = 0.

c        set the remaining curvatures found by tridi.
         DO 90  I = 1, N
            CV(I) = B(I)
90          CONTINUE




c        using the curvatures, solve for the spline coefficients
c        here we have expanded the product terms in reference eqn. (132)

         DO 100  M = 0,  N

            DX = X(J,M+1) - X(J,M)
            DX2 = DX * DX

            A0(J,M+1) = ( CV(M)*X(J,M+1)**3 - CV(M+1)*X(J,M)**3
     :                + 6.*Z(J,M)*X(J,M+1)
     :                - CV(M)*X(J,M+1) * DX2
     :                - 6.*Z(J,M+1)*X(J,M) + CV(M+1)*X(J,M)
     :                * DX2 ) / ( 6.* DX )


            A1(J,M+1) = (-.5*CV(M)*X(J,M+1)**2 + .5*CV(M+1)*X(J,M)**2
     :          -Z(J,M) + (CV(M) * DX2 ) / 6. + Z(J,M+1)
     :          - CV(M+1) * DX2 / 6. ) / DX


            A2(J,M+1) = (CV(M)*X(J,M+1)-CV(M+1)*X(J,M) )
     :                           /(2.* DX)


            A3(J,M+1) = ( CV(M+1) - CV(M) ) / (6.*DX)


100         CONTINUE



200      CONTINUE


      RETURN

      END



*------------------------------------------------------------------------


      SUBROUTINE CUSPLW(X,Z,NPTS,A0,A1,A2,A3,FAIL,
     :                  C,D,E,B,CV)


c     This subroutine fits a cubic spline through the points
c     defining the well.  The code is the same as in CUSPLN
c     except that the spline coeffs are 1-D arrays and that x and z
c     are reversed in the calling sequence

      INTEGER     NPTS

      REAL        X(0:NPTS-1),    Z(0:NPTS-1),
     :            A0(NPTS-1),     A1(NPTS-1),
     :            A2(NPTS-1),     A3(NPTS-1)


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




      REAL      C(NPTS-1),               D(NPTS-1),
     :          E(NPTS-1),               B(NPTS-1),
     :          CV(0:NPTS-1),            SGNDET,    DX,   DX2

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


