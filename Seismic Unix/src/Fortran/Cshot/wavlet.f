      SUBROUTINE WAVLET(FLL,FHL,FLR,FHR,NPTWAV,DT,
     :           MAXWAV,PI,XREAL,XIMAG,XHEAD,STDERR)

c     Calculates a zero phase time wavelet and its Hilbert Transform,
c     given the amplitude spectrum.  Also calculates the head wave wavelet.

      INTEGER  MAXWAV,  NPTWAV,   STDERR

      REAL     FLL,     FHL,     FLR,       FHR,
     :         DT,      PI,
     :         XREAL(2*MAXWAV),    XIMAG(MAXWAV),
     :         XHEAD(MAXWAV)

cc    Local     variables
c     C         constant used when tapering frequency spectrum
c     DF        increment in frequency 
c     I         loop variable
c     IFLL,IFHL,IFLR,IFHR  integer sample numbers of frequency spectrum
c     IFREQ     loop variable
c     IPOW2     power of two corresponding to N
c     N         number of points to fft
c     NDIFF     difference between N and NPTBY2
c     NPTBY2    half the number of points in the wavelet
c     PIBY2     PI over two
c     SINRAD    sine used in tapering amplitude spectrum
c     TMP       temporary storage of sample of wavelet
c     XIMAG()   imaginary part of spectrum before fft ( zero ),
c               imaginary part of wavelet after fft
c     XK        height of flat part of amplitude spectrum
c     XREAL()   real part of spectrum before fft,
c               real part of wavelet after


      INTEGER     I,          IFREQ,    IPOW2,
     :            IFLL,       IFHL,     IFLR,      IFHR,
     :            N,          NDIFF,    NPTBY2

      REAL        C,          DF,       PIBY2,     TMP,
     :            SINRAD,     XK


c     Initialising the frequency array.
      DO 5 I = 1, MAXWAV
         XIMAG(I) = 0.
5        CONTINUE
      DO 7 I = 1, 2*MAXWAV
         XREAL(I) = 0.
7        CONTINUE


c     Calculating n for the fft.
      CALL FACT2(NPTWAV,IPOW2)
      N = 2**IPOW2
      DF = 1. / (N*DT)

      IF(N+1.GT.MAXWAV) THEN
         WRITE(STDERR,'(1X,A,1X,A)')
     :   'WAVLET : too many points for fft.',
     :   'Reduce wavelet length.'
      END IF

c     Integer values of the frequency spectrum ( + 1 to include
c     the value at f = 0 ).
      IFLL = FLL / DF + .5 + 1
      IFHL = FHL / DF + .5 + 1
      IFLR = FLR / DF + .5 + 1
      IFHR = FHR / DF + .5 + 1

c     Wavelet should have an odd number of points.
      NPTBY2 = NPTWAV / 2
      NPTWAV = 2 * NPTBY2 + 1

c     The output wavelet is usually smaller than the wavelet
c     calculated by the fft.
      NDIFF = N - NPTBY2

c-----------------------------------------
cccc  Calculate the head wave wavelet first
      XK = 1.

c     Set the frequency spectrum
      DO 20 I = IFHL,  IFLR
         XREAL(2*I-1) = XK / I
20       CONTINUE

c     For the sides of the filter use sin(x)*2 curves or at
c     least jump only to 0.5 to lessen ringing.
      PIBY2 = PI / 2.

c     First the left side
      IF(IFLL.EQ.IFHL) THEN
c        XREAL(2*IFHL-1) = .5 * XK / ifll
      ELSE
         C = PIBY2 / ( IFHL - IFLL )
         DO 30 IFREQ = IFLL,  IFHL
            SINRAD = SIN( C * ( IFREQ - IFLL ) )
            XREAL(2*IFREQ-1) = SINRAD * SINRAD * XK / IFREQ
30          CONTINUE
      END IF

c     Now the right side
      IF(IFLR.EQ.IFHR) THEN
        XREAL(2*IFLR-1) = .5 * XK / IFLR
      ELSE
         C = PIBY2 / ( IFHR - IFLR )
         DO 40 IFREQ = IFLR,  IFHR
            SINRAD = SIN( C * ( IFHR - IFREQ ) )
            XREAL(2*IFREQ-1)= SINRAD * SINRAD * XK / IFREQ
40          CONTINUE
      END IF

c     Find the headwave time wavelet.
      CALL FOUR1(XREAL,N,-1)

c     Set the head wave wavelet (this is real - not supporting 
c     head wave caustics, so won't need imaginary part).
      DO 70 I = 1,  NPTBY2
         J = 2 * ( I + NDIFF )
         XHEAD(I) = - 2. * XREAL(J)
         XHEAD(I+NPTBY2) = -2. * XREAL(2*I)
70       CONTINUE
      XHEAD(NPTWAV) =  - XHEAD(1)

c-----------------------------------------------------

ccc   Now do standard wavelet
      XK = DF

c     Initialising the frequency array.
      DO 15 I = 1, MAXWAV
         XIMAG(I) = 0.
15       CONTINUE
      DO 17 I = 1, 2*MAXWAV
         XREAL(I) = 0.
17       CONTINUE

c     Set the frequency spectrum
      DO 25 I = IFHL,  IFLR
         XREAL(2*I-1) = XK
25       CONTINUE

c     For the sides of the filter use sin(x)*2 curves or at
c     least jump only to 0.5 to lessen ringing.
      PIBY2 = PI / 2.

c     First the left side
      IF(IFLL.EQ.IFHL) THEN
         XREAL(2*IFHL-1) = .5 * XK
      ELSE
         C = PIBY2 / ( IFHL - IFLL )
         DO 35 IFREQ = IFLL,  IFHL
            SINRAD = SIN( C * ( IFREQ - IFLL ) )
            XREAL(2*IFREQ-1) = SINRAD * SINRAD * XK
35          CONTINUE
      END IF

c     Now the right side
      IF(IFLR.EQ.IFHR) THEN
        XREAL(2*IFLR-1) = .5 * XK
      ELSE
         C = PIBY2 / ( IFHR - IFLR )
         DO 45 IFREQ = IFLR,  IFHR
            SINRAD = SIN( C * ( IFHR - IFREQ ) )
            XREAL(2*IFREQ-1)= SINRAD * SINRAD * XK
45          CONTINUE
      END IF


c     inverse fft...
      CALL FOUR1(XREAL,N,-1)

c     Set the output wavelet.
c     Imaginary part first.
      DO 75 I = 1,  NPTBY2
         J = 2 * ( I + NDIFF )
         XIMAG(I) = 2. * XREAL(J)
         XIMAG(I+NPTBY2) = 2. * XREAL(2*I)
75       CONTINUE

c     Now the real part - have to be careful how we put this together
c     Need to keep index of output > index of input
      DO 80 I =  NPTBY2, 1, -1
         XREAL(I+NPTBY2) = 2. * XREAL(2*I-1)
80       CONTINUE

c     don't have to worry about index here (j>nptwav, since ndiff>nptby2)
      DO 85 I =  1,  NPTBY2
         J = 2 * ( I + NDIFF ) - 1
         XREAL(I) = 2. * XREAL(J)
85       CONTINUE

      XREAL(NPTWAV) = XREAL(1)
      XIMAG(NPTWAV) = -XIMAG(1)


      RETURN
      END
c----------------------------------------------------------------------
