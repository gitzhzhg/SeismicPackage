      SUBROUTINE ADDWAV(PHASE,CAUSTC,NPTWAV,NPTBY2,T,NTPTS,TRACE,
     :           AMP,WIMAG,WREAL,WHEAD,PI,NTRACE,DT,DELTAT,HEAD,
     :           RESAMP,MAXTR,MAXTPT)

c     Adds the wavelet to trace ntrace.  Uses a resampled wavelet
c     to avoid jumps in dipping events (ie, arrival time is not just
c     rounded to nearest sample time).

      INTEGER  NPTWAV,    NPTBY2,    NTRACE,     RESAMP,
     :         NTPTS,     MAXTR,    MAXTPT

      REAL     WREAL(*),  WIMAG(*),  WHEAD(*),   DELTAT,
     :         PHASE,     AMP,       DT,         T,
     :         PI,        TRACE(MAXTPT,MAXTR)

      LOGICAL  CAUSTC,    HEAD


c  Local variables:
c  COSPHS  Cosine of PHASE
c  J,K,L   Counters
c  MAXTPT  Maximum samples/trace
c  MAXTR   Maximum number of traces per output panel
c  NPTS    Number of output samples in wavelet
c  NRSAMP  Nearest sample time at rate DELTAT to the arrival time
c  NSAMP   Nearest lesser output sample to arrival time
c  NSHIFT  Number of DELTAT samples between arrival time and output sample
c  NSTART  First output sample number to start adding wavelet to trace
c  SINPHS  Sine of PHASE
c  TNEW    Closest DELTAT sample time to actual arrival time.  TNEW is
c          then treated as the arrival time of the event.
c  TTMP    Output sample time corresponding to sample NSAMP

      REAL     COSPHS,   SINPHS,   TNEW,   TTMP

      INTEGER J, K, L,   NPTS,     NRSAMP, NSAMP,
     :        NSHIFT,    NSTART



c     nearest sample to time t (at sample rate deltat)
      NRSAMP = NINT(T/DELTAT + 1)

c     tnew is sample time closest to t at sample interval deltat
      TNEW = ( NRSAMP - 1 ) * DELTAT

c     this is the output sample <= tnew
      NSAMP = ( TNEW + DELTAT/10. ) / DT + 1

c     calculate time difference between out sample time and tnew...
      TTMP = ( NSAMP - 1 ) * DT

c     nshift is number of deltat samples between output sample and tnew
      NSHIFT = NINT( (TNEW-TTMP)/DELTAT )


      IF(NSHIFT.EQ.0) THEN
c        tnew falls right on output sample
         NSTART = NSAMP - NPTBY2
         NPTS   = NPTWAV
      ELSE
c        tnew is between 2 output samples
c        nshift now becomes # deltat samples that tnew is later
c        than output sample time
         NSHIFT = RESAMP - NSHIFT
         NSTART = NSAMP - NPTBY2 + 1
c        we get one less point in output wavelet for this case
         NPTS   = NPTWAV - 1
      END IF
         

      IF(HEAD) THEN
c        headwave event
c        the extra shift by resamp/2 here is necessary because
c        resampled wavelet is longer than output wavelet (by one
c        output sample interval)

         L = NSHIFT + 1 + RESAMP / 2
         DO 100 K = 0,  NPTS - 1
            J = NSTART + K 
            IF(J.LT.1.OR.J.GT.NTPTS) THEN
            ELSE
               TRACE(J,NTRACE) = TRACE(J,NTRACE) + AMP * WHEAD(L)
            END IF
            L = L + RESAMP
100         CONTINUE

      ELSE

         IF(PHASE.EQ.0.) THEN
c           no phase changes due to post critical reflections
   
            IF(CAUSTC) THEN
c              phase shift is - pi / 2
               L = NSHIFT + 1 + RESAMP / 2
               DO 200 K = 0,  NPTS - 1
                  J = NSTART + K 
                  IF(J.LT.1.OR.J.GT.NTPTS) THEN
                  ELSE
                     TRACE(J,NTRACE) = TRACE(J,NTRACE)
     :               + AMP * WIMAG(L)
                  END IF
                  L = L + RESAMP
200               CONTINUE
            ELSE
c              wavelet has no phase shifts
               L = NSHIFT + 1 + RESAMP / 2
               DO 300 K = 0,  NPTS - 1
                  J = NSTART + K
                  IF(J.LT.1.OR.J.GT.NTPTS) THEN
                  ELSE
                     TRACE(J,NTRACE) = TRACE(J,NTRACE)
     :               + AMP * WREAL(L)
                  END IF
                  L = L + RESAMP
300               CONTINUE
            END IF

         ELSE

c           some post critical reflections
            IF(CAUSTC) THEN
c              also gone through a caustic
               PHASE = PHASE - PI / 2.
            END IF
            COSPHS = COS(PHASE)
            SINPHS = SIN(PHASE)
            L = NSHIFT + 1 + RESAMP / 2
            DO 400 K = 0,  NPTS - 1
               J = NSTART + K
               IF(J.LT.1.OR.J.GT.NTPTS) THEN
c                 beyond end of trace
               ELSE
                  TRACE(J,NTRACE) = TRACE(J,NTRACE) + AMP *
     :            ( COSPHS * WREAL(L) - SINPHS * WIMAG(L) )
               END IF
               L = L + RESAMP
400            CONTINUE

         END IF

      END IF

      RETURN
      END
c---------------------------------------------------------------------
