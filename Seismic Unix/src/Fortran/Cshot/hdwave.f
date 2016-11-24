      SUBROUTINE HDWAVE(RAYLST,SHTOUT,STDERR,LIST,SHTREC,TINFO,PLTRAY,
     :                  X,Z,XS,ZS,XSTART,XEND,XREC,ZREC,NREC,RECDPT,
     :                  TRUGEO,SLAYER,VEL,VREF,BETAI,BETAF,DELTAB,
     :                  IREFL,IEVENT,IRECD,PI,
     :                  NTR1B,NTRNB,NTR1F,NGAP,HEAD,MAXEVT,MAXREF,MU,
     :                  IPEN)

      INTEGER  RAYLST,    SHTOUT,   STDERR,  IRECD,
     :         SLAYER,    IEVENT,   NTR1B,   NTRNB,    NTR1F,
     :         MAXEVT,    MAXREF,   MU,      IPEN
      INTEGER  IREFL(MAXEVT,0:MAXREF)


      REAL     XREC(NREC),  ZREC(NREC),
     :         XS,  ZS,  XSTART,  XEND,  RECDPT,  TRUGEO, 
     :         PI
      REAL     X(0:*),   Z(0:*),  VEL(0:*),        VREF(*)  
      LOGICAL  LIST,    SHTREC,  TINFO,  PLTRAY,   HEAD


      INTEGER    MAXINT,       MAXSPL,         MXSPM1,
     :           MAXN,         MAXNP1

      PARAMETER ( MAXINT = 20,
     :            MAXSPL = 2001,
     :            MAXN   = 40)
      PARAMETER ( MAXNP1 = MAXN + 1,
     :            MXSPM1 = MAXSPL - 1)

      REAL        XINT(0:MAXINT,MAXSPL),     ZINT(0:MAXINT,MAXSPL),
     :            A0(0:MAXINT,MXSPM1),       A1(0:MAXINT,MXSPM1),
     :            A2(0:MAXINT,MXSPM1),       A3(0:MAXINT,MXSPM1),
     :            SIGN(0:MAXN),              CV(0:MAXINT,MAXSPL)

      INTEGER     NPTS(0:MAXINT),   NINT,      NORDER(MAXN)

      COMMON /A/   XINT,          ZINT,
     :             A0,            A1,        A2,          A3,
     :             SIGN,
     :             NPTS,          NINT,      NORDER,      CV


      REAL       DZ(MAXN),       DDZ(MAXN),
     :           D(MAXNP1),      DELTAX(MAXNP1),   DELTAZ(MAXNP1),
     :           V(MAXNP1)

      INTEGER    N

      COMMON /B/   DZ,           DDZ,
     :             D,            DELTAX,        DELTAZ,
     :             V,            N


c  Local Variables:

c  AMP    Amplitude of the wavelet
c  AMP1   Required by sub RAYDAT, not used here
c  BETA   Takeoff angle in shooting search for critical ray
c  BETA1  Initial takeoff angle in search
c  BETAC  Takeoff angle of critical ray
c  CAUSTC FALSE in this subroutine - head waves that pass through a
c         caustic are not taken into account in this version of the program
c  DANOLD Difference between incident angle and critical angle at the
c         refracting interface.  When this difference changes sign then
c         we are close to critical.
c  DONE   TRUE when receiver rays have been found for this source ray
c         segment
c  DR     Distance travelled along the refractor
c  DX     x-distance between end point of ray and next receiver
c  EXIT   TRUE when near critical angle for this source segment
c  HDAMP  Constant part of head wave amplitude
c  I      Counter
c  INC    + or - 1, depending on relative x-location of next receiver
c  INGAP  TRUE if receiver is located in the gap
c  IREFRA The refracting interface
c  ISEG   Source segment counter
c  ISRSEG Number of source segments 
c  ITRACE Trace number corresponding to this receiver
c  KREFRA The intersection number of the refractor
c  L      Counter
c  LAST   Number of the last receiver for this source segment
c  NOCONV TRUE if can' find the ray or ray not near critical (shooting)
c  NOHDWV TRUE if critical ray emerges betond limits of spread
c  NOLD   Temporary storage of N 
c  NTRABS Station number of this receiver
c  NXTREC Next receiver at which to find ray
c  PHASE,1 Zero here. (No phase shifts on head wave wavelet.)
c  SGNOFF 1. if receivers are to right of source; -1. if to left
c  SINTHC Sine of critical angle
c  TCOEFF Transmission effects along the ray
c  TIME1  Traveltime along the ray
c  TR     Traveltime along receiver segment of the ray
c  TREF   Traveltime along the refractor segment of the ray
c  TS     Traveltime along source segment of the ray
c  VALID  TRUE if this is a valid event
c  VREF() Not used here (required by sub ORDER)
c  X1MX0  x(1) minus x(0)
c  XOLD() Temporary storage of receiver segment
c  XTMP   x-coordinate where previous ray segment hits interface
c  Z1MZ0  z(1) minus z(0)
c  ZTMP   z-coordinate where previous ray segment hits interface  


      REAL   AMP,    AMP1,   BETA,   BETA1,   BETAC,   DANOLD,
     :       DR,     DX,     HDAMP,  PHASE,   PHASE1,  SGNOFF,
     :       SINTHC, TCOEFF, TIME1,  TR,      TREF,    TS,
     :       X1MX0,  XTMP,   Z1MZ0,  ZTMP

      REAL   XOLD(MAXN/2)

      INTEGER  I,      INC,    IREFRA,   ISEG,   ISRSEG,  ITRACE,
     :         KREFRA, L,      LAST,     NOLD,   NTRABS,  NXTREC

      LOGICAL  NOCONV,  CAUSTC,  NOHDWV,  INGAP,   VALID,  EXIT,
     :         DONE

      

c     This is the refracting interface
      IREFRA = IREFL(IEVENT,1)

      IF(IREFRA.LT.SLAYER) THEN
         IF(LIST) WRITE(RAYLST,'(2X,A,1X,I2)')
     :     'Invalid refraction - event number', IEVENT
         RETURN     
      END IF

c     calculate and check the sine of the critical angle
      SINTHC = VEL(IREFRA) / VEL(IREFRA+1)
      IF(SINTHC.GE.1.) THEN
c        no valid refraction
         IF(LIST) WRITE(RAYLST,'(A)')
     :     'Invalid refraction - see layer velocities.'
         RETURN    
      ELSE
c        calculate constant used in amplitude factor
         HDAMP = VEL(IREFRA) * SINTHC / ( 2. * PI * (1.-SINTHC**2) )
      END IF

c     isrseg is number of required source segments
      ISRSEG = 0
c     first assume all receivers are to right of source
      SGNOFF = 1.
      IF(XREC(1).LT.XS) THEN
c        at least some of the receivers are to left of shot
c        consider these first
         ISRSEG = ISRSEG + 1
         SGNOFF = -1.
      END IF
      IF(XREC(NREC).GT.XS) THEN
c        some receivers are to right of shot
         ISRSEG = ISRSEG + 1
      END IF

      PHASE = 0.
      CAUSTC = .FALSE.
      DANOLD = 0.
      BETA1 = BETAI
c     this is the intersection number of the refractor
      KREFRA = IREFRA - SLAYER + 1

      DO 1000 ISEG = 1,  ISRSEG
c        isrseg is either 1 or 2 depending on locations of
c        receivers relative to source

c        first set necessary arrays, depending on order of intersections
         CALL ORDER(IREFL,1,SLAYER,IEVENT,VEL,NORDER,V,SIGN,VREF,N,
     :              VALID,RECDPT,LIST,RAYLST,STDERR,MAXEVT,MAXREF)
         IF(.NOT.VALID) THEN
c           this should not occur at this point
            IF(LIST) WRITE(RAYLST,'(2X,A)')'Invalid refraction.'
            RETURN     
         END IF


c        beta = betai
         BETA = BETA1
         EXIT = .FALSE.

500      IF(.NOT.EXIT.AND.BETA.LE.BETAF) THEN

c           shoot rays until one hits refracting interface near
c           the critical angle (SHOOT returns noconv=true if not
c           near critical)


            CALL SHOOT(X,Z,NOCONV,BETA,PI,TRUGEO,XSTART,XEND,
     :                 RECDPT,.TRUE.,KREFRA,SINTHC,SGNOFF,DANOLD)
            
            IF(NOCONV) THEN
               BETA = BETA + DELTAB
            ELSE
c              don't shoot any more for this segment
               EXIT = .TRUE.
c              near critical, converge on solution
               NOLD = N
               N = IREFRA - SLAYER + 1
c              looking for precise ray that hits at critical
               CALL REFRACT(X,Z,N,NOCONV,SGNOFF,SINTHC)
               IF(NOCONV) THEN
c                 do nothing - can't find source segment
                  BETA1 = BETA - DELTAB
               ELSE
c                 found this source segment
                  IF(LIST) THEN
                     WRITE(RAYLST,'(2X,A)')'Source segment:'
                     CALL XZOUT(X,Z,N,RAYLST)
                     WRITE(RAYLST,'(/2X,A)')'Receiver segments:'
                  END IF
                  IF(PLTRAY) CALL RAYPLT(X,Z,N-1,IPEN)
                  IF(TINFO) CALL TTIME(N-1,D,V,TS)
c                 calculate takeoff angle at source of the ray that
c                 intersects refractor at critical angle
                  X1MX0 = X(1) - X(0)
                  Z1MZ0 = Z(1) - Z(0)
                  XTMP = X(N)
                  ZTMP = Z(N)
                  BETAC = 180. * ATAN2(X1MX0,Z1MZ0) / PI
                  BETA1 = BETAC
c                 now shoot a ray at this takeoff angle so we can
c                 see where it emerges on the line
                  N = NOLD
                  CALL SHOOT(X,Z,NOCONV,BETAC,PI,TRUGEO,XSTART,
     :            XEND,RECDPT,.FALSE.,0,0.,0.,0.)
                  IF(NOCONV) THEN
c                    ray probably emerges outside model - so no
c                    refractions for this source ray
                     IF(LIST) WRITE(RAYLST,'(2X,A/)')
     :               'No head waves found'
                  ELSE
                     DELTAX(N+1) = X(N+1) - X(N)
                     DELTAZ(N+1) = Z(N+1) - Z(N)
                     D(N+1) = SQRT(DELTAX(N+1)**2 + DELTAZ(N+1)**2)
c                    calculate some amplitude information
                     IF(SHTREC) CALL RAYDAT(X(N+1),VREF,IREFL,
     :               IEVENT,AMP1,PHASE1,TCOEFF,MAXEVT,MAXREF)
c                    now go on and find receiver segments
c                    first need to bracket emergence point of critical ray
                     CALL BRACKT(X(N+1),XREC,NREC,SGNOFF,NXTREC,NOHDWV)
                     IF(NOHDWV) THEN
c                       ray emerges beyond farthest receiver
c                       no refractions recorded 
                        IF(LIST) WRITE(RAYLST,'(2X,A/)')
     :                  'No head waves found'
                     ELSE
c                       temporary storage of receiver portion of ray
                        DO 550 I = 1, IREFRA
                           XOLD(I) = X(N-I+1)
550                        CONTINUE
c                       now set x to receiver portion of ray
c                       also need to reset a few things (ray starts at 
c                       receiver in the following)
                        DX = XREC(NXTREC) - X(N+1)
                        DO 600 I = 1, IREFRA
                           X(I) = XOLD(I) + DX
                           V(I) = VEL(I)
                           SIGN(I) = -1.
                           NORDER(I) = I
600                        CONTINUE
c                       now find rays to remaining receivers
                        IF(SGNOFF.LT.0.) THEN
                           LAST = 1
                           INC = -1
                        ELSE
                           LAST = NREC
                           INC = 1
                        END IF
                        N = IREFRA
c                       initialize distance traveled along refractor
                        DR = 0.
                        I = NXTREC
                        DONE  = .FALSE.
700                     IF(.NOT.DONE) THEN
                           IF(I.EQ.LAST) DONE = .TRUE.
                           X(0) = XREC(I)
                           Z(0) = ZREC(I)
                           CALL REFRACT(X,Z,N,NOCONV,-SGNOFF,SINTHC)
c                          watch for turning ponts...
                           IF(SGNOFF.LT.0..AND.X(N).GT.XTMP) THEN
                              NOCONV=.TRUE.
                           END IF
                           IF(SGNOFF.GT.0..AND.X(N).LT.XTMP) THEN
                              NOCONV=.TRUE.
                           END IF
                           IF(NOCONV) THEN
c                             one more attempt at solution using
c                             a different first guess
                              DO 725 L = 1, N
                                 X(L) = XREC(I)
725                              CONTINUE
                              CALL REFRACT(X,Z,N,NOCONV,-SGNOFF,SINTHC)
c                             watch for turning ponts...
                              IF(SGNOFF.LT.0..AND.X(N).GT.XTMP) THEN
                                 NOCONV=.TRUE.
                              END IF
                              IF(SGNOFF.GT.0..AND.X(N).LT.XTMP) THEN
                                 NOCONV=.TRUE.
                              END IF
                           END IF
                           IF(NOCONV) THEN
                              DO 750 L = 1, N
                                 X(L) = XREC(I)
750                              CONTINUE
                           ELSE
c                             calculating distance travelled along refractor
                              DR = DR + SQRT((XTMP-X(N))**2 
     :                        + (ZTMP-Z(N))**2)
                              CALL GAP(I,NTR1B,NTRNB,NTR1F,INGAP,
     :                        NTRABS)
                              IF(.NOT.INGAP) THEN
                                 IF(PLTRAY) THEN
                                    CALL RAYPLT(X,Z,N-1,IPEN)
                                 END IF
                                 IF(TINFO) THEN
                                    CALL TTIME(N-1,D,V,TR)
                                    TREF = DR / VEL(IREFRA+1)
                                    TIME1 = TS + TR + TREF
                                 END IF
                                 IF(SHTREC) THEN
c                                   ntrabs = i + ntr1b - 1
                                    IF(NTRABS.LE.NTRNB) THEN
                                       ITRACE = I 
                                    ELSE
                                       ITRACE = I - NGAP
                                    END IF
                                    AMP = HDAMP * TCOEFF / 
     :                              (XS-XREC(I))**2
                                    WRITE(SHTOUT)IRECD,ITRACE,
     :                              NTRABS+MU,TIME1,
     :                              XREC(I)-XS,IEVENT,ZS,
     :                              AMP,PHASE,CAUSTC,HEAD
                                 END IF
                                 IF(LIST) THEN
                                    CALL XZOUT(X,Z,N,RAYLST)
                                    WRITE(RAYLST,'(3X,A,F10.6/)')
     :                              't = ',TIME1
                                 END IF
                              ELSE
c                                in the gap
                              END IF
                              XTMP = X(N)
                              ZTMP = Z(N)
                           END IF
c                          first guess for next receiver segment
                           DX = TRUGEO * SGNOFF
                           DO 775 L = 1, N
                              X(L) = X(L) + DX
775                           CONTINUE
c                          end if
                           I = I + INC
                           GO TO 700
                        END IF
                     END IF
                  END IF
               END IF
            END IF
            GO TO 500
         END IF
         SGNOFF = 1.
         N = NOLD
         X(0) = XS
         Z(0) = ZS
         DANOLD = 0.

1000     CONTINUE



c     reset source location (was set at receiver location)
      X(0) = XS
      Z(0) = ZS
      RETURN     
      END

c----------------------------------------------------------------------

      SUBROUTINE REFRACT(X,Z,N,NOCONV,SGNOFF,SINTHC)

c     This subroutine uses Newton's method to find a refracted ray
c     segment.  It must be given a first guess.
c     No continuation is used here.

      INTEGER N
      REAL     X(0:N+1),   Z(0:N+1),  SGNOFF,  SINTHC
      LOGICAL  NOCONV

cc    Local Variables:
c     I     Counter
c     FAIL  TRUE if failure in Newton iteration
c     MAXIT Maximum number of Newton iterations
c     SOLN  residual must fall below this for a solution
c     RESID residual of system of equations to be solved

      REAL       SOLN,      RESID
      INTEGER    I,         MAXIT
      LOGICAL    FAIL

c     PARAMETER (MAXIT = 10)
      PARAMETER (MAXIT = 50)

      SOLN = FLOAT(N)
      NOCONV = .FALSE.

c     Start the Newton iterations.
c     After each iteration we check to see if the
c     iteration has been carried out successfully, and
c     if so, check to see if it has found a solution.
      I = 0
      CALL NEWTON(X,Z,N,RESID,FAIL,.TRUE.,SGNOFF,SINTHC)
100   IF(FAIL.OR.RESID.GT.SOLN) THEN
         I = I + 1
         IF(.NOT.FAIL.AND.I.LT.MAXIT) THEN
            CALL NEWTON(X,Z,N,RESID,FAIL,.TRUE.,SGNOFF,SINTHC)
         ELSE
c           Newton has failed.
c           Quit
            NOCONV = .TRUE.
            RETURN
         END IF
         GO TO 100
      ELSE
          RETURN
      END IF

      RETURN
      END

c---------------------------------------------------------------------
