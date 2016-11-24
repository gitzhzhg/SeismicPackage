c                  CSHOT1 GRAPHICS ROUTINES

c---------------------------------------------------------------------------

      SUBROUTINE PLOTI()
c     Initialize the plot

      RETURN
      END

c---------------------------------------------------------------------------

      SUBROUTINE PLOTE()
c     Close the plot

      RETURN
      END

c---------------------------------------------------------------------------

      SUBROUTINE PLOTIN(IPEN)

c     Plots the interfaces
      INTEGER  IPEN

      INTEGER    MAXINT,       MAXSPL,         MXSPM1,
     :           MAXN

      PARAMETER ( MAXINT = 20,
     :            MAXSPL = 2001,
     :            MAXN   = 40)

      PARAMETER ( MXSPM1 = MAXSPL - 1)

      REAL        XINT(0:MAXINT,MAXSPL),     ZINT(0:MAXINT,MAXSPL),
     :            A0(0:MAXINT,MXSPM1),       A1(0:MAXINT,MXSPM1),
     :            A2(0:MAXINT,MXSPM1),       A3(0:MAXINT,MXSPM1),
     :            SIGN(0:MAXN),              CV(0:MAXINT,MAXSPL),
     :           DXXM,           AM1,              BM1,
     :		  DXM1XM,	DXM1X

      INTEGER     NPTS(0:MAXINT),   NINT,      NORDER(MAXN)

      COMMON /A/   XINT,          ZINT,
     :             A0,            A1,        A2,          A3,
     :             SIGN,
     :             NPTS,          NINT,      NORDER,      CV


cc    Local variables:
c     DX      X spacing between spline points
c     I       loop variable
c     IERR    error flag required by plot routines
c     J,K     loop variables
c     NXPTS   number of points between spline points, through which
c             to draw a line representing the interface
c     X()     array of interface x coordinates to plot
c     Z()     array of interface z coordinates to plot



      INTEGER     I,      IERR,        J,     K,    NXPTS
      PARAMETER ( NXPTS = 10)
      REAL        DX,   X(0:NXPTS),  Z(0:NXPTS)


      DO 40 I = 0,  NINT

         DO 30  J = 1, NPTS(I) - 1

            DX = XINT(I,J+1) - XINT(I,J)
            BM1 = ZINT(I,J+1) / DX - CV(I,J+1) * DX / 6
            AM1 = ZINT(I,J) / DX - CV(I,J) * DX / 6

            DO 20   K = 0,  NXPTS
               X(K) = XINT(I,J) + K * DX / NXPTS

c **************************************************************************
c  The following calculation of the position of the interface was changed by
c  E.Jenner and T.Salinas, CWP July 1996, to increase the accuracy.
c  See comments in 'splines.f', subroutine CUSPLN for details.
c **************************************************************************
c              Z(K) = A0(I,J) + A1(I,J)*X(K) + A2(I,J)*X(K)**2
c    :                                       + A3(I,J)*X(K)**3

               IF(NPTS(I).EQ.2) THEN

                  Z(K) = A0(I,J) + A1(I,J)*X(K) + A2(I,J)*X(K)**2
     :                   + A3(I,J)*X(K)**3

               ELSE

                  DXM1X = XINT(I,J+1) - X(K)
                  DXXM = X(K) - XINT(I,J)

                  Z(K)=(CV(I,J)*DXM1X**3+CV(I,J+1)*DXXM**3)/(6*DX)
     $                 + AM1 * DXM1X + BM1 * DXXM

               ENDIF

20             CONTINUE

            CALL LINE(X,Z,NXPTS+1,IPEN)

30          CONTINUE

40       CONTINUE


      RETURN
      END

c-------------------------------------------------------------------

      SUBROUTINE PLOTWL(ZWELL,NWELL,W0,W1,W2,W3,IPEN)

c     Plots the well
      INTEGER  NWELL,          IPEN
      REAL     ZWELL(0:NWELL)
      REAL     W0(NWELL),      W1(NWELL),   W2(NWELL),  W3(NWELL)


cc    Local variables
c     DZ      z-distance between spline points
c     J,K     counters
c     NXPTS   number of points between spline points in line plot
c     X()     array of well x coordinates to plot
c     Z()     array of well z coordinates to plot

      INTEGER   NXPTS,         J,               K
      PARAMETER(NXPTS = 10 )
      REAL      X(0:NXPTS),  Z(0:NXPTS),    DZ


      DO 30  J = 1, NWELL - 1

         DZ = ZWELL(J) - ZWELL(J-1)

         DO 20   K = 0,  NXPTS
            Z(K) = ZWELL(J-1) + K * DZ / NXPTS
            X(K) = W0(J) + W1(J)*Z(K) + W2(J)*Z(K)**2
     :                                + W3(J)*Z(K)**3
20          CONTINUE

         CALL LINE(X,Z,NXPTS+1,IPEN)

30       CONTINUE

      RETURN
      END

c---------------------------------------------------------------------------

      SUBROUTINE RAYPLT(X,Z,N,IPEN)

c     Plots the raypaths.

      INTEGER      N,          IPEN
      REAL         X(0:N+3),   Z(0:N+3)

      CALL LINE(X,Z,N+2,IPEN)

      RETURN
      END

c------------------------------------------------------------------------

      SUBROUTINE PLTSYM(XSYM,ZSYM,NSYM,TRUGEO,IPEN)

c     Plots a symbol (a square) at the each of the locations specified

      INTEGER   NSYM,        IPEN
      REAL      XSYM(NSYM),  ZSYM(NSYM),   TRUGEO

cc    Local variables:
c     I     counter
c     SIDE  length of side of square in model units
c     X()   x-coordinates of square
c     Z()   z-coordinates of square

      INTEGER   I,     NPIX
      REAL      X(5),  Z(5),  SIDE,  SMAG

ccc   Increase symmag for bigger symbol
      PARAMETER( SYMMAG= .1 )


      SIDE = SYMMAG * TRUGEO 
      DO 10 I = 1,  NSYM
         X(1) = XSYM(I) - SIDE / 2.
         Z(1) = ZSYM(I) 
         X(2) = X(1)
         Z(2) = Z(1) + SIDE 
         X(3) = X(2) + SIDE
         Z(3) = Z(2)
         X(4) = X(3)
         Z(4) = Z(1) 
         X(5) = X(1)
         Z(5) = Z(1)
         CALL LINE(X,Z,5,IPEN)
10       CONTINUE

      RETURN
      END

c---------------------------------------------------------------------------

      SUBROUTINE LINE(X,Z,N,IPEN)

c     Draws straight line segments through coordinates in
c     x and z arrays.

      INTEGER N,    IPEN,     STDOUT
      REAL X(N),  Z(N)
      PARAMETER ( STDOUT = 6)

c     Local variables:
c     I     Counter

      WRITE(STDOUT,*)N,IPEN
      DO 10 I = 1, N
	 WRITE(STDOUT,*)X(I),Z(I)
10       CONTINUE

      RETURN
      END

c---------------------------------------------------------------------------
