
      SUBROUTINE ORDER(KREF,NREFLS,IS,IEVENT,VEL,NORDER,V,SIGN,
     : VREF,N,VALID,LIST,RAYLST,STDERR,MAXEVT,MAXREF,NINT,LREF,
     : EVTYPE,SINTHC)

c     Calculates the order in which the ray meets the interfaces.
c     Sets velocities on ray segments.
c     Sets direction in which a ray leaves an interface (through sign()).
c     Finds the velocity of the reflecting layers.
ccc   (Note: this subroutine has been modified from the one used in
ccc    CSHOT.)

      REAL     VEL(0:*),  V(*),  SIGN(0:*),  VREF(*)
      INTEGER  IEVENT,    IS,    RAYLST,     NREFLS,   STDERR,   N,
     :         NINT,      LREF
      INTEGER  KREF(MAXEVT,0:MAXREF),        NORDER(*)
      LOGICAL  VALID,     LIST
      CHARACTER EVTYPE*1

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
c     VREFCT   Velocity of refractor

      INTEGER  I,   IMAX,   INC,   I1,   I2,   ITRAK,  K,  L
      LOGICAL  UP,  DOWN
      REAL     VREFCT


      VALID = .TRUE.
c     first set order of intersections

      ITRAK = 0
c     source layer
      KREF(IEVENT,0) = IS

      IF(KREF(IEVENT,NREFLS).GE.KREF(IEVENT,NREFLS-1)) THEN
c        send ray back to upper surface
         KREF(IEVENT,NREFLS+1) = 0
         VREFCT = VEL(KREF(IEVENT,NREFLS)+1)
      ELSE
c        send ray to deepest interface
         KREF(IEVENT,NREFLS+1) = NINT
         VREFCT = VEL(KREF(IEVENT,NREFLS))
      END IF

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

      DO 100 L = 1,  NREFLS + 1
         IF(KREF(IEVENT,L).GE.KREF(IEVENT,L-1).AND.DOWN) THEN
c           ray is going down
            INC = 1
            I1 = KREF(IEVENT,L-1) + 1
            I2 = KREF(IEVENT,L)
            VREF(L) = VEL(I2+1)
         ELSE IF(KREF(IEVENT,L).LT.KREF(IEVENT,L-1).AND.UP) THEN
c           ray is going up   
            INC = - 1
            I1 = KREF(IEVENT,L-1)  - 1
            I2 = KREF(IEVENT,L)
            VREF(L) = VEL(I2)
         ELSE
            VALID = .FALSE.
            WRITE(STDERR,'(2X,A)')
     :      'Invalid event.'
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

c        intersection number of last reflection...
         IF(L.EQ.NREFLS) LREF = ITRAK

100      CONTINUE


c     set n
      N = ITRAK - 1

c     now set velocities on ray segments
      V(1) = VEL(IS)
      DO 210 I = 2,  N + 1
         IMAX = MAX(NORDER(I),NORDER(I-1))
         V(I) = VEL(IMAX)
210      CONTINUE
    
c     set sign() ie, direction of ray leaving an interface
      DO 250 I = 1,  N 
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

c     check for valid head wave
      IF(EVTYPE.EQ.'h') THEN
         IF(VREFCT.GT.V(LREF)) THEN
            SINTHC = V(LREF)/VREFCT
         ELSE
            VALID = .FALSE.
            WRITE(STDERR,'(2X,A)')
     :      'No head wave.'
            RETURN
         END IF
      END IF

      RETURN
      END

c---------------------------------------------------------------------
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
         IF(IREFL(IEVENT,J).GT.NINT.OR.IREFL(IEVENT,J).LT.0) THEN
            VALID = .FALSE.
            RETURN
         END IF
50       CONTINUE

      RETURN 
      END

c-----------------------------------------------------------------------
      SUBROUTINE XZSRC(S1,NSRC,DSRC,ZWELL,NWELL,W0,W1,W2,W3,
     :                XSRC,ZSRC,FAIL)
c     Calculates the x-z coordinates of sources (or receivers)
c     in the well.

      INTEGER   NSRC,       NWELL

      REAL      S1,         DSRC,        ZWELL(0:NWELL-1),
     :          W0(NWELL),  W1(NWELL),   W2(NWELL),   W3(NWELL),
     :          XSRC(NSRC), ZSRC(NSRC)

      LOGICAL   FAIL

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

      FAIL = .FALSE.
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
               FAIL = .TRUE.
               RETURN
c              write(stderr,'(a)')'XZREC: receivers outside well.'
c              stop
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
      SUBROUTINE XZWINT(XWELL,ZWELL,NWELL,W0,W1,W2,W3,XINT,
     :           A0,A1,A2,A3,MAXINT,MAXSPL,MXSPM1,NINT,
     :           XCROSS,ZCROSS,FAIL,STDERR)

c     Calculates the intersections of the well with the interfaces

      REAL  XWELL(0:NWELL-1),          ZWELL(0:NWELL-1)
      REAL  W0(NWELL),                 W1(NWELL), 
     :      W2(NWELL),                 W3(NWELL),
     :      XINT(0:MAXINT,MAXSPL),     
     :      A0(0:MAXINT,MXSPM1),       A1(0:MAXINT,MXSPM1),
     :      A2(0:MAXINT,MXSPM1),       A3(0:MAXINT,MXSPM1)

      INTEGER    NINT,                 STDERR

      REAL  XCROSS(0:NINT),            ZCROSS(0:NINT)
      LOGICAL FAIL

cc    Local variables
      INTEGER S,  DS,    IINT,   J
      REAL    ZTRAK,    XTRAK,  DXDZ,   DELZ,   ZINTK

c     max length of well
c     parameter( smax = 50000)
      PARAMETER(   DS = 1)
c     parameter( smax = 10000)
c     parameter(   ds = 5)

c     write(*,*)'xzwint, using new smax and ds'

c     intersection of well and upper surface
      XCROSS(0) = XWELL(0)
      ZCROSS(0) = ZWELL(0)


      IINT = 1
      S = 0
      ZTRAK = ZWELL(0)
      

c100   if(iint.le.nint.and.s.lt.smax) then
100   IF(IINT.LE.NINT) THEN
c        while there are more interfaces at which to calculate
c        well intersection (well must cut all interfaces)

         S = S + DS

c        calculate z at this s
         J = 1
210      IF(ZTRAK.GE.ZWELL(J-1)) THEN
            J = J + 1
            GO TO 210
         END IF
         J = J - 1

         DXDZ =       W1(J)
     :        +  2. * W2(J) * ZTRAK
     :        +  3. * W3(J) * ZTRAK**2
  
         DELZ = DS / SQRT( 1. + DXDZ**2 )
         ZTRAK = ZTRAK + DELZ
         IF(ZTRAK.GE.ZWELL(NWELL-1)) THEN
             WRITE(STDERR,'(1X,A,1X,A,1X,I2)')
     :      'XZREC: failed to find well intersection',
     :      'with interface',IINT
            FAIL = .TRUE.
            RETURN
         END IF

c        calculate x-coordinate at this z
         J = 1
260      IF(ZTRAK.GE.ZWELL(J-1)) THEN
            J = J + 1
            GO TO 260
         END IF
         J = J - 1
         XTRAK = W0(J) + W1(J) * ZTRAK
     :                 + W2(J) * ZTRAK**2
     :                 + W3(J) * ZTRAK**3


c        calculate depth of interface at this x
         J = 1
270      IF(XTRAK.GT.XINT(IINT,J)) THEN
            J = J + 1
            GO TO 270
         END IF
         J = J - 1
         ZINTK = A0(IINT,J) + A1(IINT,J) * XTRAK  
     :                      + A2(IINT,J) * XTRAK**2
     :                      + A3(IINT,J) * XTRAK**3

         IF(ABS(ZTRAK-ZINTK).LE.DS) THEN
c           approaching interface
c           compute approx x-coord of intersection
            XCROSS(IINT) = XTRAK + DXDZ * (ZINTK - ZTRAK)
            J = 1
290         IF(XTRAK.GT.XINT(IINT,J)) THEN
               J = J + 1
               GO TO 290
            END IF
            J = J - 1
c           compute z-coord of intersection
            ZCROSS(IINT) = A0(IINT,J) + A1(IINT,J) * XTRAK  
     :                         + A2(IINT,J) * XTRAK**2
     :                         + A3(IINT,J) * XTRAK**3
            IINT = IINT + 1
         END IF

         GO TO 100

      END IF

c     if(iint.le.nint) then
c        couldn't find all intersection points of well
c        with interfaces
c        try reducing dsmax in parameter statement above
c        write(stderr,'(1x,a)')
c    :   'XZREC: failed to find all well-interface intersections'
c        fail = .true.
c        return
c     end if



      RETURN

      END

c------------------------------------------------------------------
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
     :            MAXSPL = 51,
     :            MAXN   = 40)

      PARAMETER ( MXSPM1 = MAXSPL - 1)

      REAL        XINT(0:MAXINT,MAXSPL),     ZINT(0:MAXINT,MAXSPL),
     :            A0(0:MAXINT,MXSPM1),       A1(0:MAXINT,MXSPM1),
     :            A2(0:MAXINT,MXSPM1),       A3(0:MAXINT,MXSPM1),
     :            SIGN(0:MAXN)

      INTEGER     NPTS(0:MAXINT),   NINT,      NORDER(MAXN)

      COMMON /A/   XINT,          ZINT,
     :             A0,            A1,        A2,          A3,
     :             SIGN,
     :             NPTS,          NINT,      NORDER


cc    Local variables:
c     IINT    interface counter
c     J       counter
c     ZINTK   depth of interface IINT at x-coordinate of source

      INTEGER  IINT,  J
      REAL     ZINTK


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
c     interface depth...
      ZINTK = A0(IINT,J) + A1(IINT,J) * X
     :                   + A2(IINT,J) * X**2
     :                   + A3(IINT,J) * X**3

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

      SUBROUTINE TRIDI(N,C,D,E,B,SGNDET,INFO)

c     Tridi solves the equation JX = B for X.  J is the tridiagonal
c     jacobian whose bands here are C (subdiagonal), D (diagonal),
c     and E (superdiagonal). The solution is returned as B.
c     See reference, eqns. (11) and (14).
c     The code is from a LINPAK listing.
c     REFERENCE : LINPACK USER'S GUIDE,  J.J. DONGARRA et al,
c                 SIAM, 1979.

      INTEGER   N,        INFO

      REAL      C(N),     D(N),     E(N),      B(N),
     :          SGNDET

      INTEGER   K,        KB,       KP1,       NM1,       NM2

      REAL      T


c     initialising the sign of the determinant
      SGNDET = 1.

      INFO = 0
      C(1) = D(1)
      NM1 = N - 1
      IF(NM1.LT.1) GO TO 40
         D(1) = E(1)
         E(1) = 0.0E0
         E(N) = 0.0E0

         DO 30  K = 1,  NM1
            KP1 = K + 1
            IF(ABS(C(KP1)).LT.ABS(C(K))) GO TO 10
c              sign changes
               SGNDET = - SGNDET
               T = C(KP1)
               C(KP1) = C(K)
               C(K) = T
               T = D(KP1)
               D(KP1) = D(K)
               D(K) = T
               T = E(KP1)
               E(KP1) = E(K)
               E(K) = T
               T = B(KP1)
               B(KP1) = B(K)
               B(K) = T
10          CONTINUE

            IF(C(K).NE.0.0E0) GO TO 20
               INFO = K
               GO TO 100
20          CONTINUE

            T = -C(KP1)/C(K)
            C(KP1) = D(KP1) + T*D(K)
            D(KP1) = E(KP1) + T*E(K)
            E(KP1) = 0.0E0
            B(KP1) = B(KP1) + T*B(K)
30          CONTINUE
40    CONTINUE


      IF(C(N).NE.0.0E0) GO TO 50
         INFO = N
         GO TO 90
50    CONTINUE

      NM2 = N - 2
      B(N) = B(N)/C(N)
      IF(N.EQ.1) GO TO 80
         B(NM1) = (B(NM1) - D(NM1)*B(N))/C(NM1)
         IF(NM2.LT.1) GO TO 70
            DO 60  KB = 1,  NM2
               K = NM2 - KB + 1
               B(K) = (B(K) - D(K)*B(K+1) - E(K)*B(K+2))/C(K)
60             CONTINUE
70       CONTINUE
80    CONTINUE
90    CONTINUE
100   CONTINUE


      RETURN
      END

c-----------------------------------------------------------------------

      SUBROUTINE WELLX(X,Z,XCROSS,LREF,NORDER,N,NNEW,NINT,CROSS,
     :                 ICROSS,IINT,IINTP1,LAYSRC,LEFT,RIGHT,
     :                 ZWELL,NWELL,W0,W1,W2,W3)

c     Checks to see if ray intersects well.

      INTEGER     N,  NNEW,   NINT,  ICROSS,  IINT,  IINTP1,  
     :            LREF,       NWELL,  LAYSRC
      INTEGER     NORDER(N+1)

      REAL        X(0:N+1),    Z(0:N+1),  XCROSS(0:NINT),
     :            ZWELL(0:*),  W0(*),     W1(*),   W2(*),   W3(*)

      LOGICAL     CROSS,       LEFT,      RIGHT

cc    Local variables
      REAL        XW,  DXL,    DXU
      INTEGER     I,   IP1,    J


      DO 50 I = LREF,  NNEW-1
         IP1 = I + 1

         IF(I.EQ.0) THEN

c           This only occurs for direct wave - first ray segment
c           Calculate x-coord. of well at z(1)   
            IF(Z(1).GE.ZWELL(NWELL-1)) THEN
c              ray hit last interface below where well did
               J=NWELL-1
               GO TO 15
            END IF
            J = 1
10          IF(Z(1).GE.ZWELL(J-1)) THEN
               J = J + 1
               GO TO 10
            END IF
            J = J - 1
            IF(J.EQ.0) THEN
c              curved upper surface - ray hitting above z(0)
c              use first well segment
               J = 1
            END IF
15          XW = W0(J) + W1(J) * Z(1) + 
     :           W2(J) * Z(1) * Z(1) + W3(J) * Z(1)**3

            IF(LEFT.AND.(X(1)-XW).GE.0.) THEN
c              crossing
               CROSS = .TRUE.
               IINT = LAYSRC
               IINTP1 = LAYSRC - 1
               ICROSS = 0
               RETURN
            ELSE IF(RIGHT.AND.(X(1)-XW).LE.0.) THEN
c              crossing
               CROSS = .TRUE.
               IINT = LAYSRC
               IINTP1 = LAYSRC - 1
               ICROSS = 0
               RETURN
            ELSE
c              look for intersection on later ray segments
            END IF
            
         ELSE

            DXL = X(I) - XCROSS(NORDER(I))
            DXU = X(IP1) - XCROSS(NORDER(IP1))
            IF(DXL.EQ.0.) THEN
c              crossing
               CROSS = .TRUE.
               IINT = NORDER(I)
               IINTP1 = NORDER(I+1)
               ICROSS = I
               RETURN
            END IF
            IF(DXU/DXL.LE.0.) THEN
c              crossing
               CROSS = .TRUE.
               IINT = NORDER(I)
               IINTP1 = NORDER(I+1)
               ICROSS = I
               RETURN
            END IF

         END IF

50       CONTINUE

      CROSS = .FALSE.

      RETURN
      END

c--------------------------------------------------------------

      SUBROUTINE SRCPOS(XS,ZS,ZWELL,XWELL0,W0,W1,W2,W3,LEFT,RIGHT)
c     Checks to see if source is to the left or right of the
c     receiver well.

      REAL XS, ZS, ZWELL(0:*), XWELL0, W0(*), W1(*), W2(*), W3(*)

      LOGICAL   LEFT,  RIGHT

cc    Local variables
      INTEGER J
      REAL XW


c     find x coord. of well at source depth
      IF(ZS.LT.ZWELL(0)) THEN
c        source located above top of receiver well (due to topography)
         IF(XS.LT.XWELL0) THEN
c           source to left of well
            LEFT = .TRUE.
            RIGHT = .FALSE.
         ELSE
c           source to right of well
            LEFT = .FALSE.
            RIGHT = .TRUE.
         END IF
         RETURN
      END IF

      J = 1
10    IF(ZS.GE.ZWELL(J-1)) THEN
         J = J + 1
         GO TO 10
      END IF
      J = J - 1
      XW = W0(J) + W1(J) * ZS + W2(J) * ZS * ZS + W3(J) *ZS * ZS * ZS

      IF((XW-XS).GT.0.) THEN
          LEFT = .TRUE.
          RIGHT = .FALSE.
c         source to left of well
      ELSE
          LEFT = .FALSE.
          RIGHT = .TRUE.
c         source to right of well
      END IF

      RETURN
      END

c-------------------------------------------------

      SUBROUTINE UPDOWN(UP,DOWN,BETA,XS,ZS,SLAYER,
     :          NINT,XCROSS,ZCROSS,PI)

c     If a ray is going up from the source (as defined by takeoff angle)
c     but first strikes an interface which lies below the source (at
c     the source location) then it is considered
c     a downgoing ray.  This can happen for curvy layers.  Similarly a ray
c     going down can hit an interface above the source first.  If so, it
c     is considered upgoing.
c     Example: A source is located between interfaces 1 and 2.  The ray
c     leaves the source going up.  It hits interface 2 first.  This is
c     taken to be a downgoing ray by the program.
c     This subroutine checks for this kind of pathology.

      LOGICAL  UP,      DOWN
      REAL     BETA,    XS,   ZS,  PI,  XCROSS(0:*),  ZCROSS(0:*)
      INTEGER  SLAYER,  NINT

cc    Local variables
      INTEGER  I
      REAL     MR,    CR,   ZR

      IF(BETA.EQ.0.) THEN
c        ray is vertical
         RETURN
      END IF
      
c     coeffs of eqn of straight line leaving source at angle beta
      IF(ABS(BETA).EQ.90.) THEN
         MR = 0.
      ELSE
         IF(UP) THEN
            MR = -1. / TAN(PI*BETA/180.)
         ELSE
            MR = 1. / TAN(PI*BETA/180.)
         END IF
      END IF
      CR = ZS - MR*XS

      I = 0
      ZR = MR * XCROSS(I) + CR

c     next cases occur when takeoff angle is outside range -90 to 90.
      IF(ZR.GT.ZS.AND.UP) THEN
         RETURN
      END IF
      IF(ZR.LT.ZS.AND.DOWN)THEN
         RETURN
      END IF

100   IF(ZR.LE.ZCROSS(I)) THEN
         IF(I.LE.SLAYER) THEN
            UP = .TRUE.
            DOWN = .FALSE.
         ELSE
            UP = .FALSE.
            DOWN= .TRUE.
         END IF
         RETURN
      ELSE
         I = I + 1
         IF(I.GT.NINT) RETURN
         ZR = MR * XCROSS(I) + CR
         IF(ZR.GE.ZS.AND.UP) THEN
            RETURN
         END IF
         IF(ZR.LE.ZS.AND.DOWN)THEN
            RETURN
         END IF
         GO TO 100
      END IF

      RETURN
      END

c------------------------------------------------------------------

      SUBROUTINE SETDIR(LEFT,UP,DOWN,BOLD,BETA,SIGN,XS,ZS,
     :           SLAYER,NINT,XCROSS,ZCROSS,PI,N,NP1,
     :           NORDER,V,VEL,IREFL,MAXEVT,IEVENT,EVTYPE)

c     Sets the order of intersections for direct waves (this
c     may change with takeoff angle).

      INTEGER    NINT,  N,  NP1,  MAXEVT,  IEVENT,  SLAYER,
     :           NORDER(*), IREFL(MAXEVT,0:*)

      REAL       BOLD,        BETA,  XS,  ZS,  PI,
     :           SIGN(0:*),   XCROSS(0:NINT),  ZCROSS(0:NINT),
     :           V(*),        VEL(0:NINT+1)

      CHARACTER EVTYPE*1

      LOGICAL    LEFT,  UP,  DOWN

cc    Local variables
      INTEGER  I,  K

c     store beta (beta is rotated for direct waves)
      BOLD = BETA
      IF(EVTYPE.EQ.'h') GO TO 50

      IF(LEFT) THEN
c        source to left of well
         IF(BETA.LE.0.) THEN
            UP = .TRUE.
            DOWN = .FALSE.
            BETA = BETA + 90.
            SIGN(0) = 1.
         ELSE
            UP = .FALSE.
            DOWN = .TRUE.
            BETA = 90. - BETA
            SIGN(0) = -1.
         END IF
      ELSE
c        source to right of well
         IF(BETA.LE.0.) THEN
            UP = .FALSE.
            DOWN = .TRUE.
            BETA = - BETA - 90.
            SIGN(0) = -1.
         ELSE
            UP = .TRUE.
            DOWN = .FALSE.
            BETA = BETA - 90.
            SIGN(0) = 1.
         END IF
      END IF
         
c     check for pathology...
      CALL UPDOWN(UP,DOWN,BETA,XS,ZS,
     :            SLAYER,NINT,XCROSS,ZCROSS,PI)

50    CONTINUE

      IF(UP) THEN
c        ray is upgoing
c        set number of intersections
         N = SLAYER - 1
         NP1 = N + 1

c        set order, velocities, etc.
         DO 10 I = 1,  N
            K = SLAYER - I
            NORDER(I) = K
            SIGN(I) = 1.
            V(I) = VEL(K+1)
10          CONTINUE
         V(N+1) = VEL(1)
         NORDER(N+1) = 0

c        set this to an invalid reflector so that sub RAYDAT will
c        not compute a reflection coefficient
         IREFL(IEVENT,1) = -1
      ELSE
c        ray is going down
c        set number of intersections
         N = NINT - SLAYER
         NP1 = N + 1

c        set order, velocities, etc.
         DO 20 I = 1,  N
            K = SLAYER + I - 1
            NORDER(I) = K
            SIGN(I) = -1.
            V(I) = VEL(K)
20          CONTINUE
         V(N+1) = VEL(NINT)
         NORDER(N+1) = NINT

c        set this to an invalid reflector so that sub RAYDAT will
c        not compute a reflection coefficient
         IREFL(IEVENT,1) = -1
      END IF

      RETURN
      END
c----------------------------------------------------------------

      SUBROUTINE CHKREF(IS,IFIRST,ILAST,NREFLS,VALID)

c     Checks that reflection event read from param1 is valid.

      INTEGER  IS,    IFIRST,   ILAST,   NREFLS
      LOGICAL  VALID

cc    Local variables
      INTEGER  NBY2,  N

      IF(ILAST.EQ.0) THEN
         NBY2 = ( NREFLS - 1 ) / 2
      ELSE
         NBY2 = NREFLS / 2
      END IF
      N = 2 * NBY2
      IF(N.EQ.NREFLS) THEN
c        even number of reflections
         IF(IS.GT.IFIRST) THEN
            VALID = .TRUE.
         ELSE
            VALID = .FALSE.
         END IF
      ELSE
c        odd number of reflections
         IF(IS.LE.IFIRST) THEN
            VALID = .TRUE.
         ELSE
            VALID = .FALSE.
         END IF
      END IF

      RETURN
      END
c------------------------------------------------------------------

      SUBROUTINE WCOORD(W0,W1,W2,W3,XR1,ZR1,XR2,ZR2,XW1,ZW1,
     :           XW2,ZW2,ZWELL,NWELL,X1,Z1,FAIL,
     :           ZMIN,ZMAX,DELTAX,DELTAZ,D)

c     Determines coordinates where ray intersects well.

      INTEGER  NWELL
      REAL     ZWELL(0:NWELL-1),
     :         W0(NWELL), W1(NWELL), W2(NWELL), W3(NWELL),
     :         XR1,    XR2,     ZR1,   ZR2,  XW1,  XW2,
     :         ZW1,    ZW2,     X1,    Z1,   ZMIN, ZMAX,
     :         DELTAX, DELTAZ,  D

      LOGICAL FAIL

c     Local variables
      REAL     SOLN,   SMALL,   EPS,   SR,   SW,   CR,   CW,
     :         X0,     Z0,      FZ,    DFDZ

      INTEGER   MAXIT,  J,       ITER

      PARAMETER(SOLN  = 1.,
     :          MAXIT = 10,
     :          SMALL = .1,
     :          EPS   = .00001)

      FAIL = .FALSE.


      IF(ABS(ZR2-ZR1).LE.SMALL) THEN
c        ray is horizontal
c        evaluate x-coord. of well at this depth
         Z1 = ZR2
c        find out which segment of well this point lies on
         J = 1
10       IF(Z1.GE.ZWELL(J-1)) THEN
            J = J + 1
            GO TO 10
         END IF
         J = J - 1
         X1 = W0(J) + W1(J)*Z1 + W2(J)*Z1*Z1 + W3(J)*Z1*Z1*Z1
         DELTAX = X1 - XR1
         DELTAZ = Z1 - ZR1
         D = SQRT( DELTAX*DELTAX + DELTAZ*DELTAZ )
         RETURN
      END IF


c     calculate eqn of ray segment
c     first the slope...
      SR = ( XR2 - XR1 ) / ( ZR2 - ZR1 )
c     intercept...  
      CR = ( XR1*ZR2 - XR2*ZR1 ) / ( ZR2 - ZR1 ) 

c     now do straight line that cuts interfaces where well does...
c     slope
      SW = ( XW2 - XW1 ) / ( ZW2 - ZW1 )
c     intercept
      CW = ( XW1*ZW2 - XW2*ZW1 ) / ( ZW2 - ZW1 )

c     where these two lines intersect will be first guess in newton's mthd.
c     if well is a straight line then this will be the true intersection

      IF((SR-SW).EQ.0.) THEN
c        lines are parallel
         FAIL = .TRUE.
         RETURN
      END IF

      Z0 = ( CW - CR ) / ( SR - SW )
      X0 = SR * Z0 + CR
c     evaluate the function and its derivative
      Z1 = Z0
      CALL FWELL(Z1,ZWELL,W0,W1,W2,W3,NWELL,SR,CR,FZ,DFDZ,FAIL)
      IF(FAIL) RETURN

c     use newton's method to converge on soln
      ITER = 1
100   IF(ABS(FZ).GT.SOLN.AND.ABS(DFDZ).GT.EPS) THEN
         IF(ITER.LE.MAXIT) THEN
            Z1 = Z0 - FZ / DFDZ
            CALL FWELL(Z1,ZWELL,W0,W1,W2,W3,NWELL,SR,CR,FZ,DFDZ,
     :      FAIL)
            IF(FAIL)RETURN
            ITER = ITER + 1
            Z0 = Z1
         ELSE
            FAIL = .TRUE.
            RETURN
         END IF
         GO TO 100
      END IF

      IF(Z1.LT.ZMIN.OR.Z1.GT.ZMAX) THEN
c        intersection outside well limits
         FAIL = .TRUE.
      ELSE
         X1 = SR * Z1 + CR
         DELTAX = X1 - XR1
         DELTAZ = Z1 - ZR1
         D = SQRT( DELTAX*DELTAX + DELTAZ*DELTAZ )
      END IF
 
      RETURN
      END

*-----------------------------------------------------------------

      SUBROUTINE FWELL(Z0,ZWELL,W0,W1,W2,W3,NWELL,SR,CR,FZ,DFDZ,
     :                 FAIL)

      INTEGER  NWELL

      REAL     ZWELL(0:NWELL-1),  W0(NWELL), W1(NWELL), 
     :         W2(NWELL),         W3(NWELL),
     :         Z0,   SR,   CR,   FZ,   DFDZ

      LOGICAL FAIL

cc    Local variables
      INTEGER J

     
      IF(Z0.LT.ZWELL(0).OR.Z0.GT.ZWELL(NWELL-1)) THEN
         FAIL = .TRUE.
         RETURN
      END IF

c     find out which segment of well this point lies on
      J = 1
10    IF(Z0.GE.ZWELL(J-1)) THEN
         J = J + 1
         GO TO 10
      END IF
      J = J - 1

c     now evaluate the function and its derivative
      FZ   =  W0(J) + W1(J)*Z0 + W2(J)*Z0*Z0 + W3(J)*Z0*Z0*Z0
     :     -  CR    -    SR*Z0

      DFDZ =       W1(J)
     :     +  2. * W2(J) * Z0
     :     +  3. * W3(J) * Z0**2
     :     -  SR
  
      RETURN
      END

*-----------------------------------------------------------------

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

      SUBROUTINE OUTPUT(FIRSTR,LASTR,ZREC,NREC,TREC,ZEND,T,NT,
     :           NRLAYR,RAYOUT,ISRC,IEVENT)

      INTEGER   FIRSTR,      LASTR,   NREC,   NT,   RAYOUT
      REAL      ZREC(NREC),  TREC(NRLAYR),   ZEND(NT),  T(NT)

      INTEGER   I,     IC
C     REAL      FDUMMY

      DO 10 I = 1,  NT
         WRITE(RAYOUT,*)ISRC,FIRSTR,LASTR,T(I),
     :   IEVENT,ZEND(I)
10       CONTINUE

      IC = 1
      DO 987 I = FIRSTR, LASTR
         WRITE(RAYOUT,*)ISRC,FIRSTR,LASTR,TREC(IC),
     :   IEVENT+5,ZREC(I)
         IC=IC+1
987      CONTINUE


      RETURN
      END

c-----------------------------------------------------------------
      SUBROUTINE WRTSHT(FIRSTR,LASTR,ZREC,NREC,TREC,AMPREC,
     :           PHSREC,CAUSTC,NRLAYR,SHTOUT,ISRC,IEVENT,UNFORM,
     :           HDWAVE)

      INTEGER   FIRSTR,   LASTR,   NREC,     SHTOUT,
     :          NRLAYR,   ISRC,    IEVENT

      REAL      ZREC(NREC),  TREC(NRLAYR),   AMPREC(NRLAYR),
     :          PHSREC(NRLAYR)

      LOGICAL   CAUSTC,   UNFORM,   HDWAVE

      INTEGER   IRECV,   I
      REAL      FDUMMY

      FDUMMY = 0.0

      IRECV = 1
      IF(UNFORM) THEN
         DO 10 I = FIRSTR, LASTR
            WRITE(SHTOUT)ISRC,I,I,TREC(IRECV),
     :      FDUMMY,IEVENT,ZREC(I),
     :      AMPREC(IRECV),PHSREC(IRECV),CAUSTC,HDWAVE
            IRECV=IRECV+1
10          CONTINUE
      ELSE
         DO 20 I = FIRSTR, LASTR
            WRITE(SHTOUT,*)ISRC,I,I,TREC(IRECV),
     :      FDUMMY,IEVENT,ZREC(I),
     :      AMPREC(IRECV),PHSREC(IRECV),CAUSTC,HDWAVE
            IRECV=IRECV+1
20          CONTINUE
      END IF


      RETURN
      END

c------------------------------------------------------------------
      SUBROUTINE XZWELL(X,Z,XCROSS,ZCROSS,LREF,NORDER,N,NNEW,NINT,FAIL,
     :           ICROSS,IINT,IINTP1,LAYSRC,LEFT,RIGHT,
     :           ZWELL,NWELL,W0,W1,W2,W3,XW,ZW,DELTAX,DELTAZ,D,ICP1)

c     Checks to see if ray intersects well and, if so, calcuates
c     the coordinates of the intersection point.

      INTEGER  N,  NNEW,   NINT,  ICROSS,  IINT,  IINTP1,  
     :         LREF,       NWELL,  LAYSRC, ICP1,  NORDER(N+1)

      REAL     X(0:N+1),   Z(0:N+1),  XCROSS(0:NINT), ZCROSS(0:NINT),
     :         ZWELL(0:*), W0(*),     W1(*),   W2(*), W3(*),
     :         DELTAX(*),  DELTAZ(*), D(*)

      LOGICAL  FAIL,       LEFT,      RIGHT

      LOGICAL  CROSS

      IF(NNEW.LE.LREF) THEN
c        ray did not reach target reflection
         FAIL = .TRUE.
         RETURN
      END IF

c     last intersection occurred after reflection from target
c     look to see if ray intersects well

      CALL WELLX(X,Z,XCROSS,LREF,NORDER,N,NNEW,NINT,
     :CROSS,ICROSS,IINT,IINTP1,LAYSRC,LEFT,RIGHT,
     :ZWELL,NWELL,W0,W1,W2,W3)

      IF(.NOT.CROSS) THEN
c        ray does not cross well
         FAIL = .TRUE.
         RETURN
      END IF

c     ray crosses well, now look for coordinates of the crossing
      ICP1 = ICROSS + 1
      CALL WCOORD(W0,W1,W2,W3,X(ICROSS),Z(ICROSS),
     :X(ICP1),Z(ICP1),XCROSS(IINT),ZCROSS(IINT),XCROSS(IINTP1),
     :ZCROSS(IINTP1),ZWELL,NWELL,XW,ZW,FAIL,ZWELL(0),ZCROSS(NINT),
     :DELTAX(ICP1),DELTAZ(ICP1),D(ICP1))

      RETURN
      END

c----------------------------------------------------------------------
      SUBROUTINE TINTEG(X1IN,Z1IN,IRFRCT,VREF,X2IN,Z2IN,T,XINT,
     :           A0,A1,A2,A3,MAXINT,MAXSPL,MXSPM1)

c     Integrates traveltime along refractor.

      REAL  XINT(0:MAXINT,MAXSPL),     
     :      A0(0:MAXINT,MXSPM1),       A1(0:MAXINT,MXSPM1),
     :      A2(0:MAXINT,MXSPM1),       A3(0:MAXINT,MXSPM1)

cc    Local variables
      REAL  ZTRAK,    XTRAK,  X1,  Z1,  X2,  Z2,  DXSQD,
     :      DX1,      DX2,    DX,  DXBY10,   DZ,  ZOLD,   D

      INTEGER  J1,  J2,  J

      T = 0.

      IF(X2IN.EQ.X1IN) RETURN

c     work with x2>x1
      IF(X2IN.LT.X1IN) THEN
         X1 = X2IN
         X2 = X1IN
         Z1 = Z2IN
         Z2 = Z1IN
      ELSE
         X2 = X2IN
         X1 = X1IN
         Z2 = Z2IN
         Z1 = Z1IN
      END IF

        

c     find which section of spline x1 and x2 are on
      J1 = 1
50    IF(X1.GT.XINT(IRFRCT,J1)) THEN
         J1 = J1 + 1
         GO TO 50 
      END IF
      J1 = J1 - 1

      J2 = J1
60    IF(X2.GT.XINT(IRFRCT,J2)) THEN
         J2 = J2 + 1
         GO TO 60 
      END IF
      J2 = J2 - 1

c     use about 10 points per spline section for integration...
      DX1 = 0.1 * ( X2 - X1 ) / ( J2 - J1 + 1 )

c     but don't use less than 10 points
      DX2 = 0.1 * (X2 - X1)
      DX = MIN(DX1,DX2)
      DXBY10 = DX / 10.
      DXSQD = DX * DX

      XTRAK = X1 + DX
      J = J1
      D = 0.
      ZOLD = Z1
100   IF(XTRAK.LE.(X2+DXBY10)) THEN

110      IF(XTRAK.GT.XINT(IRFRCT,J)) THEN
            J = J + 1
            GO TO 110
         END IF
         J = J - 1

         ZTRAK = A0(IRFRCT,J) + A1(IRFRCT,J) * XTRAK  
     :                        + A2(IRFRCT,J) * XTRAK**2
     :                        + A3(IRFRCT,J) * XTRAK**3

         DZ = ZTRAK - ZOLD
         D = D + SQRT( DXSQD + DZ*DZ )

         XTRAK = XTRAK + DX

         ZOLD = ZTRAK
         GO TO 100

      END IF

      T = D / VREF

      RETURN
      END

c------------------------------------------------------------------
