      SUBROUTINE SETREF(EVENT,IREFL,NREFLS,IEVENT,VALID,MAXEVT,
     :                  MAXREF,NINT)

c     Given character input (event), this subroutine sets the list
c     of reflectors for this event and counts the number of reflections
c     that occur in this event.  Assumes that 2 digits are enough to 
c     specify a reflector (ie, 99 is max interface number)

      CHARACTER    EVENT*30
      LOGICAL      VALID
      INTEGER      NREFLS,    IEVENT,    MAXEVT,   MAXREF,   NINT

      INTEGER   IREFL(MAXEVT,0:MAXREF)

cc    Local variables:
c     DIGITS   The digits 0 - 9, and space
c     IDIG1    First digit of pair
c     IDIG2    Second digit of pair
c     J,K      Counters

      CHARACTER  DIGITS*11
      INTEGER    IDIG1,    IDIG2,   J,   K


      DIGITS = ' 0123456789'
      VALID = .TRUE.
      NREFLS = 1

c     look at characters - max of 30
c     pass over blank spaces

      J = 1
10    IF(J.LE.30) THEN

         IF(EVENT(J:J).EQ.DIGITS(1:1)) THEN
c           this is a space
            J = J + 1
         ELSE   
c           calculate the digit
            K = 2
20          IF(EVENT(J:J).NE.DIGITS(K:K)) THEN
               K = K + 1
               IF(K.GT.11) THEN
c                 character not 0 - 9
                  VALID = .FALSE.
                  RETURN
               END IF
               GO TO 20 
            END IF
c           got the first digit of possible pair
            IDIG1 = K - 2
   
c           next character
            J = J + 1
            IF(J.GT.30) THEN
c              only one digit
c              set reflector
               IREFL(IEVENT,NREFLS) = IDIG1
            ELSE  
c              set number (maximum of two digits)
               IF(EVENT(J:J).EQ.DIGITS(1:1)) THEN
c                 space - only one digit
                  IREFL(IEVENT,NREFLS) = IDIG1
               ELSE
                  K = 2
30                IF(EVENT(J:J).NE.DIGITS(K:K)) THEN
                     K = K + 1
                     IF(K.GT.11) THEN
                        VALID = .FALSE.
                        RETURN
                     END IF
                     GO TO 30
                  END IF
c                 got  the second digit
                  IDIG2 = K - 2
c                 set reflector
                  IREFL(IEVENT,NREFLS) = 10 * IDIG1 + IDIG2
c                 make sure next character is a space (2 digits max)
                  IF(EVENT(J+1:J+1).NE.DIGITS(1:1)) THEN
                     VALID = .FALSE.
                     RETURN
                  END IF
               END IF

               J = J + 1
            END IF
            NREFLS = NREFLS + 1

         END IF
         GO TO 10 

      END IF

c     number of reflections in this event
      NREFLS = NREFLS - 1

c     make sure these are valid reflector numbers
      DO 50 J = 1,  NREFLS
         IF(IREFL(IEVENT,J).GT.NINT) THEN
            VALID = .FALSE.
            RETURN
         END IF
50       CONTINUE

      RETURN 
      END

c-----------------------------------------------------------------------

      SUBROUTINE ORDER(KREF,NREFLS,IS,IEVENT,VEL,NORDER,V,SIGN,
     : VREF,N,VALID,RECDPT,LIST,RAYLST,STDERR,MAXEVT,MAXREF)

c     Calculates the order in which the ray meets the interfaces.
c     Sets velocities on ray segments.
c     Sets direction in which a ray leaves an interface (through sign()).
c     Finds the velocity of the reflecting layers.

      REAL     VEL(0:*),  V(*),  SIGN(0:*),  VREF(*),  RECDPT
      INTEGER  IEVENT,    IS,    RAYLST,     NREFLS,   STDERR,   N
      INTEGER  KREF(MAXEVT,0:MAXREF),        NORDER(*)
      LOGICAL  VALID,     LIST

cc    Local variables:
c     DOWN     TRUE when ray is going down
c     I        Counter
c     IMAX     Max interface number at ends of ray segment
c     INC      +-1, depending on whether ray is going up or down
c     I1       Interface number of previous reflector
c     I2       Interface number of next reflector
c     ITRAK    Tracks the number of intersections
c     K,L      Counters
c     UP       TRUE when ray is going up

      INTEGER  I,   IMAX,   INC,   I1,   I2,   ITRAK,  K,  L
      LOGICAL  UP,  DOWN


      VALID = .TRUE.
c     first set order of intersections

      ITRAK = 0
c     source layer
      KREF(IEVENT,0) = IS

      IF(KREF(IEVENT,1).GE.KREF(IEVENT,0)) THEN
c        ray is going down from source
         NORDER(1) = KREF(IEVENT,0)
         ITRAK = ITRAK + 1
         DOWN = .TRUE.
         UP = .FALSE.
      ELSE
c        ray is going up from source
         DOWN = .FALSE.
         UP = .TRUE.
      END IF

      DO 100 L = 1,  NREFLS
         IF(KREF(IEVENT,L).GE.KREF(IEVENT,L-1).AND.DOWN) THEN
c           ray is going down
            INC = 1
            I1 = KREF(IEVENT,L-1) + 1
            I2 = KREF(IEVENT,L)
         ELSE IF(KREF(IEVENT,L).LT.KREF(IEVENT,L-1).AND.UP) THEN
c           ray is going up   
            INC = - 1
            I1 = KREF(IEVENT,L-1)  - 1
            I2 = KREF(IEVENT,L)
         ELSE
            VALID = .FALSE.
            IF(LIST) THEN
               WRITE(RAYLST,'(2X,A)')
     :         'Invalid event.'
               WRITE(RAYLST,'(2X,A)')
     :         'May be due to source location or the reflector list.'
            END IF
            RETURN
         END IF
      
c        set order of intersections betweeen ray and interfaces
         DO 50 K = I1, I2, INC
            ITRAK = ITRAK + 1
            NORDER(ITRAK) = K
50          CONTINUE

         IF(DOWN) THEN
            DOWN = .FALSE.
            UP   = .TRUE.
         ELSE
            DOWN = .TRUE.
            UP   = .FALSE.
         END IF

100      CONTINUE

      IF(DOWN.AND.KREF(IEVENT,NREFLS).NE.0) THEN
c        last reflection sent ray downwards and was not from
c        the upper surface - therefore this must be an invalid
c        event description since ray never makes it to receivers
         VALID = .FALSE.     
         IF(LIST) THEN
            WRITE(RAYLST,'(2X,A)')
     :      'Invalid event.'
            WRITE(RAYLST,'(2X,A)')
     :      'May be due to source location or the reflector list.'
         END IF
         RETURN
      END IF

      IF(KREF(IEVENT,NREFLS).EQ.0.AND.RECDPT.LE.0.) THEN
c        asked for a receiver ghost, but receivers are not
c        buried below the surface - so invalid event
         VALID = .FALSE.
         WRITE(STDERR,'(1X,A,1X,I2)')
     :   'Invalid event - number',IEVENT
         WRITE(STDERR,'(1X,A)')
     :   'You must bury the receivers to get a receiver ghost.'
         IF(LIST) THEN
            WRITE(RAYLST,'(1X,A)')
     :      'Invalid event.'
            WRITE(RAYLST,'(1X,A)')
     :      'You must bury the receivers to get a receiver ghost.'
         END IF
         RETURN
      END IF

c     extend ray back to upper surface (receivers are always in layer 1)
      IF(KREF(IEVENT,NREFLS).EQ.0) THEN
c        last reflection was from upper surface
      ELSE
c        last reflection deep in model
         DO 110 K = KREF(IEVENT,NREFLS) - 1, 1, -1
            ITRAK = ITRAK + 1
            NORDER(ITRAK) = K
110         CONTINUE
      END IF

c     set n
      N = ITRAK

c     now set velocities on ray segments
      V(1) = VEL(IS)
      DO 210 I = 2,  N
         IMAX = MAX(NORDER(I),NORDER(I-1))
         V(I) = VEL(IMAX)
210      CONTINUE
c     last segment is always in layer 1
      V(N+1) = VEL(1)
    
c     set sign() ie, direction of ray leaving an interface
      DO 250 I = 1,  N - 1
         IF(NORDER(I+1).GT.NORDER(I)) THEN
c           ray going down
            SIGN(I) = -1.
         ELSE
c           ray going up
            SIGN(I) = 1.
         END IF
250      CONTINUE

      IF(NORDER(1).LT.IS) THEN
c        ray goes up from source
         SIGN(0) = 1.
      ELSE
c        ray goes down from source
         SIGN(0) = -1.
      END IF

      IF(NORDER(N).EQ.1) THEN
c        last segment going up
         SIGN(N) = 1.
      ELSE
c        last segment going down (receiver ghost)
         SIGN(N) = -1.
      END IF

c     set velocities of reflecting layers - vref()
      DO 300 I = 1,  NREFLS - 1
         IF(KREF(IEVENT,I+1).GT.KREF(IEVENT,I)) THEN
c           reflecting down
            VREF(I) = VEL(KREF(IEVENT,I))
         ELSE
c           reflecting up
            VREF(I) = VEL(KREF(IEVENT,I)+1)
         END IF
300      CONTINUE
      IF(KREF(IEVENT,NREFLS).EQ.0) THEN
c        ghost from upper surface
         VREF(NREFLS) = VEL(0)
      ELSE
         VREF(NREFLS) = VEL(KREF(IEVENT,NREFLS)+1)
      END IF

c     write(*,*)'s0',sign(0)
      DO 500 I = 1, N
c        write(*,*)i,norder(i),sign(i),v(i),vref(i)
500      CONTINUE
c     write(*,*)'vn+1',v(n+1)

      RETURN
      END

c---------------------------------------------------------------------

      SUBROUTINE GAP(IREC,NTR1B,NTRNB,NTR1F,INGAP,NTRABS)

c     Checks to see if a receiver is located in the gap

      INTEGER  IREC,  NTR1B,  NTRNB,  NTR1F,  NTRABS
      LOGICAL  INGAP

      NTRABS = IREC + NTR1B - 1
      IF(NTRABS.GT.NTRNB.AND.NTRABS.LT.NTR1F) THEN
         INGAP = .TRUE.
      ELSE
         INGAP = .FALSE.
      END IF

      RETURN
      END

c--------------------------------------------------------------

      SUBROUTINE BRACKT(XNP1,XREC,NREC,SGNOFF,NXTREC,NOHDWV)

c     Brackets emergence point of critical ray

      INTEGER  NREC,  NXTREC
      REAL     XNP1,  XREC(NREC),  SGNOFF
      LOGICAL  NOHDWV

cc    Local variables:
c     I     Counter


      INTEGER I


      NOHDWV = .FALSE.

      IF(SGNOFF.LT.0.) THEN
c        shot to right of receivers

         IF(XNP1.LT.XREC(1)) THEN
c           no headwaves for this half of spread
            NOHDWV = .TRUE.
            RETURN   
         ELSE IF(XNP1.GE.XREC(NREC)) THEN
c           try to get to last receiver 
            NXTREC = NREC
            RETURN    
         ELSE
c           ray emerges inside line, bracket below
         END IF

      ELSE

c        sgnoff > 0.
c        shot to left of receivers
         IF(XNP1.LT.XREC(1)) THEN
c           try to get to first receiver
            NXTREC = 1
            RETURN     
         ELSE IF(XNP1.GT.XREC(NREC)) THEN
c           no headwaves for this half of spread
            NOHDWV = .TRUE.
            RETURN   
         ELSE
c           ray emerges inside line, bracket below
         END IF
      END IF
            
c     bracketing ray when it emerges within line
      DO 963 I = 1,  NREC
         IF(XNP1.GE.XREC(I).AND.XNP1.LT.XREC(I+1))THEN
            IF(SGNOFF.LT.0.) THEN
               NXTREC = I
            ELSE
               NXTREC = I + 1
            END IF 
            RETURN   
         END IF
963      CONTINUE

      RETURN
      END

c--------------------------------------------------------------

      SUBROUTINE XZOUT(X,Z,N,IUNIT)

c     Writes ray coordinates to listing file

      INTEGER   N,       IUNIT
      REAL      X(0:N),  Z(0:N)

cc    Local variables:
c     I     Counter

      INTEGER I

      DO 10 I = 0,  N
         WRITE(IUNIT,'(2F10.2)') X(I),Z(I)
10       CONTINUE

      RETURN
      END
c------------------------------------------------------

      SUBROUTINE XZSRC(S1,NSRC,DSRC,ZWELL,NWELL,W0,W1,W2,W3,
     :                XSRC,ZSRC,STDERR)
c     Calculates the x-z coordinates of the source in the well.

      INTEGER   NSRC,       NWELL,      STDERR

      REAL      S1,         DSRC,        ZWELL(0:NWELL-1),
     :          W0(NWELL),  W1(NWELL),   W2(NWELL),   W3(NWELL),
     :          XSRC(NSRC), ZSRC(NSRC)


cc    Local variables:
c     DELS    integration step size (arc length down the well)
c     DELZ    change in Z due to step DELS
c     DXDZ    slope of well (dx/dz)
c     ISI     lower limit of integration
c     ISF     upper limit of integration
c     J,K,L   loop variables
c     XTRAK   x-coordinate in well
c     ZTRAK   z-coordinate in well

      REAL     DELS,  DELZ,  DXDZ,  XTRAK,  ZTRAK
      INTEGER  ISI,   ISF,   J,      K,     L    
      PARAMETER( DELS = 1.)

      ZTRAK = ZWELL(0)
      
      
c     First find x,z-coordinates of sources in well
      DO 300 K = 1,  NSRC

c        set limits of integration
         IF(K.EQ.1) THEN
            ISI = 0
            ISF = S1
         ELSE
            ISI = S1 + ( K - 2 ) * DSRC
            ISF = ISI + DSRC
         END IF
c        integrate down the well to next source
         DO 250 L = ISI + 1,  ISF

            IF(ZTRAK.LT.ZWELL(0).OR.
     :      ZTRAK.GE.ZWELL(NWELL-1)) THEN
               WRITE(STDERR,'(A)')'XZREC: receivers outside well.'
               STOP
            END IF
            J = 1
210         IF(ZTRAK.GE.ZWELL(J-1)) THEN
               J = J + 1
               GO TO 210
            END IF
            J = J - 1
c           slope of well
            DXDZ =       W1(J)
     :           +  2. * W2(J) * ZTRAK
     :           +  3. * W3(J) * ZTRAK**2
   
c           change in z brought about by dels increment in distance (arc
c           length) down well
            DELZ = DELS / SQRT( 1. + DXDZ**2 )
c           z coordinate at this distance down the well
            ZTRAK = ZTRAK + DELZ

250         CONTINUE

c           find x-coordinate at this source
            J = 1
260         IF(ZTRAK.GE.ZWELL(J-1)) THEN
               J = J + 1
               GO TO 260
            END IF
            J = J - 1
            XTRAK = W0(J) + W1(J) * ZTRAK
     :                    + W2(J) * ZTRAK**2
     :                    + W3(J) * ZTRAK**3

c           set x,z-coordinates of source
            XSRC(K) = XTRAK
            ZSRC(K) = ZTRAK
 
300         CONTINUE

         RETURN
         END

c----------------------------------------------------------------------

      SUBROUTINE SETVAR(P,LOGIC1,LOGIC2,LOGIC3,LOGIC4,
     :                  C1,C1CAP,C2,C2CAP,C3,C3CAP)
c     Sets logical variables given character input.
      
      CHARACTER P*3, C1*1, C2*1, C3*1, C1CAP*1, C2CAP*1, C3CAP*1

      LOGICAL LOGIC1, LOGIC2, LOGIC3, LOGIC4
   
cc    Local variables:
c     J   counter
      INTEGER  J

      LOGIC1 = .FALSE.
      LOGIC2 = .FALSE.
      LOGIC3 = .FALSE.

      DO 200 J = 1,  3
         IF(P(J:J).EQ.C1.OR.P(J:J).EQ.C1CAP) THEN
            LOGIC1 = .TRUE.
            LOGIC4 = .TRUE.
         ELSE IF(P(J:J).EQ.C2.OR.P(J:J).EQ.C2CAP) THEN
            LOGIC2 = .TRUE.
            LOGIC4 = .TRUE.
         ELSE IF(P(J:J).EQ.C3.OR.P(J:J).EQ.C3CAP) THEN
            LOGIC3 = .TRUE.
         END IF
200      CONTINUE

         RETURN
         END

c----------------------------------------------------------------------

      SUBROUTINE LAYER(X,Z,SLAYER,INSIDE)
c     Given coordinates of source point, computes layer in which
c     source is located

      REAL       X,            Z
      INTEGER    SLAYER
      LOGICAL    INSIDE

      INTEGER    MAXINT,       MAXSPL,         MXSPM1,
     :           MAXN

      PARAMETER ( MAXINT = 20,
     :            MAXSPL = 2001,
     :            MAXN   = 40)

      PARAMETER ( MXSPM1 = MAXSPL - 1)

      REAL        XINT(0:MAXINT,MAXSPL),     ZINT(0:MAXINT,MAXSPL),
     :            A0(0:MAXINT,MXSPM1),       A1(0:MAXINT,MXSPM1),
     :            A2(0:MAXINT,MXSPM1),       A3(0:MAXINT,MXSPM1),
     :            SIGN(0:MAXN),              CV(0:MAXINT,MAXSPL)

      INTEGER     NPTS(0:MAXINT),   NINT,      NORDER(MAXN)

      COMMON /A/   XINT,          ZINT,
     :             A0,            A1,        A2,          A3,
     :             SIGN,
     :             NPTS,          NINT,      NORDER,      CV


cc    Local variables:
c     IINT    interface counter
c     J       counter
c     ZINTK   depth of interface IINT at x-coordinate of source

      INTEGER  IINT,       J
      REAL    ZINTK,  DXM1XM,           DXM1X,
     :         DXXM,     AM1,              BM1

      INSIDE = .TRUE.

      IINT = 1
5     IF(X.LE.XINT(IINT,1).OR.X.GT.XINT(IINT,NPTS(IINT))) THEN
c        source outside model
         INSIDE = .FALSE.
         RETURN
      END IF

c     find depth of interface at this source location
      J = 1
10    IF(X.GT.XINT(IINT,J)) THEN
         J = J + 1
         GO TO 10
      END IF
      J = J - 1
c **************************************************************************
c  The following calculation of the position of the interface was changed by
c  E.Jenner and T.Salinas, CWP July 1996, to increase the accuracy.
c  See comments in 'splines.f', subroutine CUSPLN for details.
c **************************************************************************
c     interface depth...
c     zintk = a0(iint,j) + a1(iint,j) * x
c    :                   + a2(iint,j) * x**2
c    :                   + a3(iint,j) * x**3

      IF(NPTS(IINT).EQ.2) THEN

         ZINTK = A0(IINT,J) + A1(IINT,J) * X
     :                      + A2(IINT,J) * X**2
     :                      + A3(IINT,J) * X**3

      ELSE

         DXM1XM = XINT(IINT,J+1) - XINT(IINT,J)
         BM1 = ZINT(IINT,J+1) / DXM1XM - CV(IINT,J+1) * DXM1XM / 6
         AM1 = ZINT(IINT,J) / DXM1XM - CV(IINT,J) * DXM1XM / 6
         DXM1X = XINT(IINT,J+1) - X
         DXXM = X - XINT(IINT,J)

         ZINTK=(CV(IINT,J)*DXM1X**3+CV(IINT,J+1)*DXXM**3)/(6*DXM1XM)
     $        + AM1 * DXM1X + BM1 * DXXM

      ENDIF

      IF(ZINTK.GT.Z) THEN
c        source above interface
         SLAYER = IINT
      ELSE
c        source below interface
         IINT = IINT + 1
         IF(IINT.GT.NINT) THEN
c           source below last interface
            SLAYER = NINT + 1
         ELSE
c           next interface
            GO TO 5
         END IF
      END IF

      RETURN
      END

c----------------------------------------------------------------------

      SUBROUTINE ELEVS(XREC,NREC,RECDPT,ZREC,SPLNOK)
c     Given the x-coordinates, finds the z-coordinates (elevations less
c     station depth) of NREC stations

      INTEGER    NREC
      REAL       XREC(NREC),   RECDPT,         ZREC(NREC)
      LOGICAL    SPLNOK

      INTEGER    MAXINT,       MAXSPL,         MXSPM1,
     :           MAXN

      PARAMETER ( MAXINT = 20,
     :            MAXSPL = 2001,
     :            MAXN   = 40)

      PARAMETER ( MXSPM1 = MAXSPL - 1)


      REAL        XINT(0:MAXINT,MAXSPL),     ZINT(0:MAXINT,MAXSPL),
     :            A0(0:MAXINT,MXSPM1),       A1(0:MAXINT,MXSPM1),
     :            A2(0:MAXINT,MXSPM1),       A3(0:MAXINT,MXSPM1),
     :            SIGN(0:MAXN),              CV(0:MAXINT,MAXSPL)

      INTEGER     NPTS(0:MAXINT),   NINT,      NORDER(MAXN)

      COMMON /A/   XINT,          ZINT,
     :             A0,            A1,        A2,          A3,
     :             SIGN,
     :             NPTS,          NINT,      NORDER,      CV

cc    Local variables:
c     I,J   counters
c     X     x-coordinate of station
c     Z     elevation of station

      REAL         X,      Z,      DXM1XM,           DXM1X,
     :          DXXM,    AM1,          BM1
      INTEGER      I,      J

      SPLNOK = .TRUE.

      DO 10 I = 1,  NREC
         X = XREC(I)
c        finding the section of the spline on which x lies and evaluating
c        the function.
c        if x falls outside the range of definition of the
c        upper surface, then return.
         IF(X.LE.XINT(0,1).OR.X.GT.XINT(0,NPTS(0))) THEN
            SPLNOK = .FALSE.
            RETURN
         END IF

         J = 1
5        IF(X.GT.XINT(0,J)) THEN
            J = J + 1
            GO TO 5
         END IF
         J = J - 1
c **************************************************************************
c  The following calculation of the position of the interface was changed by
c  E.Jenner and T.Salinas, CWP July 1996, to increase the accuracy.
c  See comments in 'splines.f', subroutine CUSPLN for details.
c **************************************************************************
c        Z = A0(0,J) + A1(0,J) * X + A2(0,J) * X**2
c    $          + A3(0,J) * X**3

         IF(NPTS(0).EQ.2) THEN

            Z = A0(0,J) + A1(0,J) * X + A2(0,J) * X**2
     $          + A3(0,J) * X**3

         ELSE

            DXM1XM = XINT(0,J+1) - XINT(0,J)
            BM1 = ZINT(0,J+1) / DXM1XM - CV(0,J+1) * DXM1XM / 6
            AM1 = ZINT(0,J) / DXM1XM - CV(0,J) * DXM1XM / 6
            DXM1X = XINT(0,J+1) - X
            DXXM = X - XINT(0,J)

            Z = (CV(0,J)*DXM1X**3+CV(0,J+1)*DXXM**3)/(6*DXM1XM)
     $          + AM1 * DXM1X + BM1 * DXXM

         ENDIF

c        z-coordinate is elevation less station depth
         ZREC(I) = Z + RECDPT

10       CONTINUE

      RETURN
      END

c-----------------------------------------------------------------------

      SUBROUTINE CHKCRD(A,NNUM)
c     Counts how many numbers have been specified in the record, up to 
c     the first noninteger character (excluding blank spaces, 
c     commas and decimal points)

      CHARACTER A*81
      INTEGER NNUM

c     Local variables
c     DIGITS   character containing the digits and decimal point
c     I, J     counters

      CHARACTER DIGITS*15
      INTEGER I, J

      DIGITS = '1234567890.+-Ee'

      A(81:81) = ' '
      NNUM = 0
      J = 1
      I = 1

50    IF(J.LE.80.AND.I.LE.15) THEN

         IF(A(J:J).EQ.' '.OR.A(J:J).EQ.',') THEN
c           this is a space or comma
            J = J + 1

         ELSE IF(A(J:J).EQ.DIGITS(I:I)) THEN
c           a digit or decimal point
            I = 1
            J = J + 1
            IF(A(J:J).EQ.' '.OR.A(J:J).EQ.',') THEN
c              end of number
               NNUM = NNUM + 1
            END IF

         ELSE
            I = I + 1

         END IF

         GO TO 50

      END IF

      RETURN
      END
c-----------------------------------------------------------------
