c--------------------------------------------------------------------


      INTEGER     PARIN,       SHTOUT,     INTERF,
     :            RAYOUT,      RAYLST,
     :            COLSIN,      RWELL,
     :            SRC,         SWELL,      STDERR

      PARAMETER ( STDERR = 0,
     :            PARIN  = 10,
     :            INTERF = 12,
     :            SHTOUT = 13,
     :            RAYOUT = 15,
     :            RAYLST = 16,
     :            COLSIN = 17,
     :            RWELL  = 18,
     :            SWELL  = 20,
     :            SRC    = 19)


      INTEGER    MAXINT,       MAXSPL,         MXSPM1,
     :           MAXN,         MAXNP1,         MAXNP3,
     :           MAXREF,       MAXEVT,         MAXREC,
     :           MAXSRC

      PARAMETER ( MAXINT = 20,
     :            MAXSPL = 51,
     :            MAXN   = 40,
     :            MAXREF = 20,
     :            MAXEVT = 20,
     :            MAXREC = 300,
     :            MAXSRC = 100)


      PARAMETER ( MAXNP1 = MAXN + 1,
     :            MAXNP3 = MAXN + 3,
     :            MXSPM1 = MAXSPL - 1)


      REAL       RW0(MXSPM1),                 RW1(MXSPM1),
     :           RW2(MXSPM1),                 RW3(MXSPM1),
     :           SW0(MXSPM1),                 SW1(MXSPM1),
     :           SW2(MXSPM1),                 SW3(MXSPM1)

c     max #times in a branch within a layer
      INTEGER    MAXT
      PARAMETER( MAXT = 500)
      REAL       T(MAXT),     ZEND(MAXT),  TREC(MAXREC),
     :           PHASE(MAXT),              PHSREC(MAXREC),
     :           AMP(MAXT),   WORK(MAXT),  AMPREC(MAXREC),
     :           TC(MAXN)
      REAL       C(MAXT),     DD(MAXT),    E(MAXT),
     :           B(MAXT),     CV(0:MAXT),
     :           D0(MAXT),    D1(MAXT)

      REAL        XINT(0:MAXINT,MAXSPL),     ZINT(0:MAXINT,MAXSPL),
     :            A0(0:MAXINT,MXSPM1),       A1(0:MAXINT,MXSPM1),
     :            A2(0:MAXINT,MXSPM1),       A3(0:MAXINT,MXSPM1),
     :            SIGN(0:MAXN)

      INTEGER     NPTS(0:MAXINT),   NINT,      NORDER(MAXN)

      COMMON /A/   XINT,          ZINT,
     :             A0,            A1,        A2,          A3,
     :             SIGN,
     :             NPTS,          NINT,      NORDER



      REAL       DZ(MAXN),       DDZ(MAXN),
     :           D(MAXNP1),      DELTAX(MAXNP1),   DELTAZ(MAXNP1),
     :           V(MAXNP1)

      INTEGER    N

      COMMON /B/   DZ,           DDZ,
     :             D,            DELTAX,        DELTAZ,
     :             V,            N



      REAL       X(0:MAXNP3),       VEL(0:MAXINT+1),   VREF(MAXREF),
     :           XREC(MAXREC),      ZREC(MAXREC),
     :           Z(0:MAXNP3),
     :           XRWELL(0:MAXSPL),   ZRWELL(0:MAXSPL),
     :           XSWELL(0:MAXSPL),   ZSWELL(0:MAXSPL),
     :           XCROSS(0:MAXINT),  ZCROSS(0:MAXINT),
     :           XOLD(0:MAXNP1),    ZOLD(0:MAXNP1),
     :           XS(MAXSRC),        ZS(MAXSRC),
     :           AMPTMP,     XSTART, 
     :           BETA,       BETAI,       BETAF
      REAL       DELTAB, 
     :           GEOINC,    INPLAN,
     :           PI,  
     :           XEND,        XLAST,
     :           ZS1,       ZSINC,
     :           R1,      DREC,     S1,    DSRC,    DELTAG,
     :           RAYDEN,  FOURPI,   DB,    BOLD,    XW,
     :           ZW,      XWPREV,   ZWPREV, DW,
     :           TTMP,    PHSTMP,   
     :           SINTHC,  DXBIG,    DXSMLL,  DX,
     :           THETAC,  DANOLD,   SINTHR,  DANGLE,
     :           T1,      X1,       Z1,      HDAMP,
     :           X0,      T2,       T3,      DT2,   SIGNDX


      INTEGER    ICOLOR(MAXINT)

      INTEGER    SLAYER(MAXSRC),  RLAYER(MAXREC), NRLAYR(MAXINT),
     :           FIRSTR(MAXINT),  LASTR(MAXINT),  RLPREV


      INTEGER    I,         IEVENT,     IERR,
     :           J,         K,          NCHARC,      NDUMMY,
     :           NEVENT,    NMULTS,
     :           IPEN,      NREC,       NRWELL,     NSWELL,
     :           NSRC,      IRECD,      LREF,       NNEW,
     :           NP1,       NT,         ICROSS,     IINT,
     :           IINTP1,    ICP1,       INTLAY,     ILPREV,
     :           ICOLD,     ICAUS1,     ICAUS2,     NSEG,
     :           IDUMMY,    NREC1,      NRECN,      IRFRCT,
     :           NEWSLY


      CHARACTER EVENT*30,   EVTYPE(MAXEVT)*1


      INTEGER  IREFL(MAXEVT,0:MAXREF),  NREFLS(MAXEVT)
   


      CHARACTER   MODEL*20,    OUTNAM*20,     OUTFIL*20,
     :            GEOMFL*20,
     :            YORN*1,      COLORS*20,   WELLR*20,   PLTYPE*5,
     :            SOURCE*20,   JOBTYP*3,    MODE*3,     WELLS*20,
     :            RORH*1



      LOGICAL     FAIL,       CAUSTC,       
     :            PLTMOD,     PLTGEO,       PLTSRC,   UNFORM,
     :            PLTSW,      PLTRW,        RDSRC,
     :            PLTRAY,     BEGPLT,       QTPLOT,   LIST,
     :            SHTREC,     QTPLT2,       QTPLT3,        
     :            DOWNHL,     GENRAL,       RDGEOM,
     :            DUMML,      VALID,      
     :            TINFO,      LEFT,         RIGHT,
     :            LEFTHW,     RIGHTHW,
     :            UP,         DOWN,     
     :            SPFAIL,     INSIDE,       
     :            NEWBRN,     TURNUP,       TURNDN,   
     :            OUTT,       OUTA,         OUTP,
     :            SEARCH,     SAMEBR,       RESTART,
     :            RWOPN,      SWOPN,        ALLRAY,
     :            NEXT,       FIRST

      PARAMETER ( PI = 3.141592653589)
      PARAMETER ( FOURPI = 4.*PI)

c     parameter(pixpi = 60.,
c    :          xscrn = 10.,
c    :          zscrn = 8.)


c**************************************************************


c     Reading from the file PARAM.
      OPEN(UNIT=PARIN,FILE='param1',STATUS ='old',ERR=5)
      GO TO 10
5     WRITE(STDERR,'(1X,A)') 'Can''t open file param.'
      STOP
10    REWIND PARIN

c     READ(PARIN,'(A)') YORN
c     IF(YORN.EQ.'y') THEN
c        allray = .true.
c     else
         ALLRAY = .FALSE.
c     end if

c     read x and z dimensions of any plots
c     read(parin,*) xdim,zdim

      READ(PARIN,'(A)') MODEL
      OPEN(UNIT=INTERF,FILE=MODEL,STATUS='old',ERR=15)
      GO TO 20
15    WRITE(STDERR,'(A)') 'Can''t open file :',MODEL
      STOP
20    REWIND INTERF

      READ(PARIN,*) NINT
      IF(NINT.GT.MAXINT) THEN
         WRITE(STDERR,'(A)') 'MAIN : too many interfaces'
         STOP
      END IF

c     Reading the points defining each interface.
c     A large and negative z value defines the end of an interface.
      DO 30 I = 0,  NINT
         J = 1
25       READ(INTERF,*) XINT(I,J),ZINT(I,J)
         IF(ZINT(I,J).LT.-9999.) THEN
            NPTS(I) = J - 1
            IF(NPTS(I).LT.2) THEN
               WRITE(STDERR,'(A,I2)')
     :        'MAIN : not enough points defining interface ',I
               STOP
            END IF
         ELSE
            J = J + 1
            IF(J.GT.MAXSPL) THEN
               WRITE(STDERR,'(A,I2)')
     :        'MAIN : too many points in interface ',I
               STOP
            END IF
            GO TO 25
         END IF
30       CONTINUE


      XSTART = XINT(0,1)
      XEND   = XINT(0,NPTS(0))     
c-----------------------------------------------------------
c
      READ(PARIN,'(A)') COLORS
      OPEN(UNIT=COLSIN,FILE=COLORS,STATUS='old',ERR=32)
      GO TO 35
32    WRITE(STDERR,'(A)') 'Can''t open file :',COLORS
      STOP
35    REWIND COLSIN
      DO 40 I = 1, 6
         READ(COLSIN,*,END=48) ICOLOR(I)
40       CONTINUE
      GO TO 50
48    WRITE(STDERR,*) 'Not enough colors'
      STOP

50    CONTINUE


c------------------------------------------------------------
c     Calculating the cubic spline coefficients of each interface.
      CALL CUSPLN(NINT,XINT,ZINT,NPTS,A0,A1,A2,A3,SPFAIL)
      IF(SPFAIL) THEN
         WRITE(STDERR,'(1X,A)')
     :   'MAIN: Failed to fit spline through interfaces.'
         STOP
      END IF

c     Read first plot descriptor 
      READ(PARIN,'(A)') PLTYPE
      BEGPLT = .FALSE.
      CALL SETVAR(PLTYPE,PLTMOD,DUMML,QTPLOT,BEGPLT,
     :'m','M','m','M','q','Q')
      IF(QTPLOT) GO TO 100

c     Read filename containing receiver well description
      READ(PARIN,'(A)',END=100) WELLR

c     Read second plot descriptor 
      READ(PARIN,'(A)',END=100) PLTYPE
      CALL SETVAR(PLTYPE,PLTRW,PLTGEO,QTPLT2,BEGPLT,
     :'w','W','g','G','q','Q')
      IF(PLTRW) RWOPN = .TRUE.
      IF(PLTGEO) RWOPN = .TRUE.
      IF(PLTGEO) RDGEOM = .TRUE.
      IF(QTPLT2) GO TO 100

c     Read shooting mode  
      READ(PARIN,'(A)',END=100) MODE
      CALL SETVAR(MODE,DOWNHL,DUMML,GENRAL,DUMML,
     :'d','D',' ',' ','g','G')
      IF(DOWNHL) SWOPN = .TRUE.

c     Read filename containing source well description
      READ(PARIN,'(A)',END=100) WELLS

c     Read source file    
      READ(PARIN,'(A)',END=100) SOURCE

c     Read third plot descriptor 
      READ(PARIN,'(A)',END=100) PLTYPE
      CALL SETVAR(PLTYPE,PLTSW,PLTSRC,QTPLT3,BEGPLT,
     :'w','W','s','S','q','Q')
      IF(PLTSW) SWOPN = .TRUE.
      IF(PLTSRC) RDSRC = .TRUE.
      IF(QTPLT3) GO TO 100

c     Read job descriptor 
      READ(PARIN,'(A)',END=100) JOBTYP
      CALL SETVAR(JOBTYP,LIST,PLTRAY,SHTREC,DUMML,
     :'l','L','r','R','t','T')
      IF(LIST)   RDGEOM = .TRUE.
      IF(SHTREC) RDGEOM = .TRUE.
      IF(PLTRAY) RDGEOM = .TRUE.
      IF(LIST)   RDSRC  = .TRUE.
      IF(SHTREC) RDSRC  = .TRUE.
      IF(PLTRAY) RDSRC  = .TRUE.
      IF(PLTRAY) RWOPN  = .TRUE.
      IF(PLTRAY) BEGPLT = .TRUE.
      IF(RDGEOM) RWOPN  = .TRUE.


100   CONTINUE

      IF(RWOPN) THEN
         OPEN(UNIT=RWELL,FILE=WELLR,STATUS='old',ERR=160)
         GO TO 165
160      WRITE(STDERR,'(A)') 'Can''t open well file :',WELLR
         STOP
165      REWIND RWELL
c        Read the x,z coordinates of the well 
c        track the number of points defining the well
         NRWELL = 0
         ZRWELL(0) = 0.
         READ(RWELL,*,END=180) XRWELL(0)
170      IF(ZRWELL(NRWELL).GT.-9999.) THEN
            NRWELL = NRWELL + 1
            IF(NRWELL.GT.MAXSPL) THEN
               WRITE(STDERR,'(1X,A,I2)')
     :         'MAIN: Too many points defining the well - max is:',
     :          MAXSPL
                STOP
            END IF
            READ(RWELL,*,END=180) XRWELL(NRWELL),ZRWELL(NRWELL)
            GO TO 170
         END IF

180      IF(NRWELL.LT.2) THEN
            WRITE(STDERR,'(1X,A)') 
     :      'Not enough points defining receiver well in file :', 
     :       WELLR
            STOP
         END IF
         IF(XRWELL(0).LE.XSTART.OR.XRWELL(0).GT.XEND) THEN
c          well outside model
           WRITE(STDERR,'(1X,A)') 'MAIN: receiver well outside model.'
           STOP
         END IF

c        calculate z-coordinate of well at surface
         J = 1
185      IF(XRWELL(0).GT.XINT(0,J)) THEN
            J = J + 1
            GO TO 185
         END IF
         J = J - 1
         ZRWELL(0) = A0(0,J) + A1(0,J) * XRWELL(0)
     :                       + A2(0,J) * XRWELL(0)**2
     :                       + A3(0,J) * XRWELL(0)**3

c        fit cubic spline to receiver well
         CALL CUSPLW(ZRWELL,XRWELL,NRWELL,RW0,RW1,RW2,RW3,SPFAIL,
     :               C,DD,E,B,CV)
         IF(SPFAIL) THEN
            WRITE(STDERR,'(1X,A)')
     :      'MAIN: Failed to fit spline through receiver well.'
            STOP
         END IF

         IF(RDGEOM) THEN
            READ(RWELL,*) R1
            READ(RWELL,*) NREC, DREC
            IF(NREC.GT.MAXREC) THEN
               WRITE(STDERR,'(1X,A,I3)') 
     :         'MAIN: too many receivers in the well - max is: ',
     :         MAXREC
               STOP
            END IF
c           Calculate x-z coordinates of receivers in well
            CALL XZSRC(R1,NREC,DREC,ZRWELL,NRWELL,RW0,RW1,
     :      RW2,RW3,XREC,ZREC,FAIL)
            IF(FAIL) THEN
               WRITE(STDERR,'(1X,A)')
     :         'MAIN: Failed to find receiver coordinates in well.'
                STOP
            END IF
            RLPREV = 0
            DO 190 I = 1,  NREC
               CALL LAYER(XREC(I),ZREC(I),RLAYER(I),INSIDE)
               IF(.NOT.INSIDE) THEN
                  WRITE(STDERR,'(1X,A)')
     :            'MAIN: Receiver outside model.'
                  STOP
               END IF
               IF(RLAYER(I).EQ.RLPREV) THEN
c                 receiver in same layer
                  NRLAYR(RLAYER(I)) = NRLAYR(RLAYER(I)) + 1
                  LASTR(RLAYER(I)) = I
               ELSE
c                 new layer
                  NRLAYR(RLAYER(I)) = 1
                  FIRSTR(RLAYER(I)) = I
                  LASTR(RLAYER(I)) = I
                  RLPREV = RLAYER(I)
               END IF
190            CONTINUE

c           find where receiver well cuts interfaces
            CALL XZWINT(XRWELL,ZRWELL,NRWELL,RW0,RW1,RW2,RW3,
     :      XINT,A0,A1,A2,A3,MAXINT,MAXSPL,MXSPM1,NINT,
     :      XCROSS,ZCROSS,FAIL,STDERR)
            IF(FAIL) THEN
               WRITE(STDERR,'(1X,A,1X,A)')
     :         'MAIN: Failed to find well-interface intersections.',
     :         'Well must cross all interfaces in model.'
                STOP
            END IF

         END IF

      END IF


      IF(SWOPN) THEN
        
         OPEN(UNIT=SWELL,FILE=WELLS,STATUS='old',ERR=200)
         GO TO 202
200      WRITE(STDERR,'(A)') 'Can''t open well file :',WELLS
         STOP

202      REWIND SWELL
c        Read the x,z coordinates of the source well 
c        track the number of points defining the well
         NSWELL = 0
         ZSWELL(0) = 0.
         READ(SWELL,*,END=210) XSWELL(0)
205      IF(ZSWELL(NSWELL).GT.-99999.) THEN
            NSWELL = NSWELL + 1
            READ(SWELL,*,END=210) XSWELL(NSWELL),ZSWELL(NSWELL)
            GO TO 205
         END IF
210      CONTINUE

         IF(NSWELL.LT.2) THEN
            WRITE(STDERR,'(1X,A)') 
     :      'Not enough points defining source well in file :', 
     :       WELLS
            STOP
         END IF

         IF(XSWELL(0).LE.XSTART.OR.XSWELL(0).GT.XEND) THEN
c          well outside model
           WRITE(STDERR,'(1X,A)') 'MAIN: source well outside model.'
           STOP
         END IF

c        find z-coordinate of source well at surface
         J = 1
215      IF(XSWELL(0).GT.XINT(0,J)) THEN
            J = J + 1
            GO TO 215
         END IF
         J = J - 1
         ZSWELL(0) = A0(0,J) + A1(0,J) * XSWELL(0)
     :                       + A2(0,J) * XSWELL(0)**2
     :                       + A3(0,J) * XSWELL(0)**3

c        fit cubic spline to source well
         CALL CUSPLW(ZSWELL,XSWELL,NSWELL,SW0,SW1,SW2,SW3,SPFAIL,
     :               C,DD,E,B,CV)
         IF(SPFAIL) THEN
            WRITE(STDERR,'(1X,A)')
     :      'MAIN: Failed to fit spline through source well.'
            STOP
         END IF
         
         IF(RDSRC) THEN
            READ(SWELL,*) S1
            READ(SWELL,*) NSRC, DSRC
c           Calculate x-z coordinates of sources in source well
            CALL XZSRC(S1,NSRC,DSRC,ZSWELL,NSWELL,SW0,SW1,
     :      SW2,SW3,XS,ZS,FAIL)
            IF(FAIL) THEN
               WRITE(STDERR,'(1X,A)')
     :         'MAIN: Failed to find source coordinates in well.'
                STOP
            END IF
            DO 225 I = 1,  NSRC
               CALL LAYER(XS(I),ZS(I),SLAYER(I),INSIDE)
               IF(.NOT.INSIDE) THEN
                  WRITE(STDERR,'(1X,A)')
     :            'MAIN: Source outside model.'
                   STOP
               END IF
225            CONTINUE
         END IF

      END IF

      IF(GENRAL) THEN

         OPEN(UNIT=SRC,FILE=SOURCE,STATUS='old',ERR=235)
         GO TO 240
235      WRITE(STDERR,'(A)') 'Can''t open sources file :',SOURCE
         STOP
240      REWIND SRC

         NSRC = 1
245      READ(SRC,*,END=248) XS(NSRC),ZS(NSRC)
         NSRC = NSRC + 1
         GO TO 245
248      NSRC = NSRC - 1

c        calculate which layer each source belongs in
         DO 250 I = 1,  NSRC
            CALL LAYER(XS(I),ZS(I),SLAYER(I),INSIDE)
            IF(.NOT.INSIDE) THEN
               WRITE(STDERR,'(1X,A)')
     :         'MAIN: Source outside model.'
               STOP
            END IF
250         CONTINUE


      END IF
        

      IF(BEGPLT) THEN
c        Plotting

         CALL PLOTI

c?       call scale(xor,zor,uppix,rwopn,zrwell,nrwell,xdim,zdim,
c?   :   xint,zint,npts,nint,xscrn,zscrn,pixpi,maxint,maxspl)

         IF(PLTMOD) THEN
            IPEN = ICOLOR(6)
c           call pen(ipen)
c           CALL PLOTIN(xor,zor,uppix,ipen,icolor)
            CALL PLOTIN(IPEN)
         END IF

         IF(QTPLOT) THEN
            CALL PLOTE
            STOP
         END IF

         IF(PLTRW) THEN
            IPEN = ICOLOR(3)
c           call pen(ipen)
c           call plotwl(xor,zor,uppix,zrwell,nrwell,rw0,
            CALL PLOTWL(ZRWELL,NRWELL,RW0,RW1,RW2,RW3,IPEN)
         END IF
         IF(PLTGEO) THEN
            IPEN = ICOLOR(1)
c           call pen(ipen)
c?          call pltsym(xrec,zrec,nrec,xor,zor,uppix,ipen)
            CALL PLTSYM(XREC,ZREC,NREC,2.*DREC,IPEN)
         END IF
         IF(QTPLT2) THEN
            CALL PLOTE
            STOP
         END IF

         IF(PLTSW) THEN
            IPEN =  ICOLOR(3)
c           call pen(icolor(3))
c           call pen(ipen)
            CALL PLOTWL(ZSWELL,NSWELL,SW0,SW1,SW2,SW3,IPEN)
         END IF
         IF(PLTSRC) THEN
            IPEN = ICOLOR(2)
c           call pen(ipen)
            CALL PLTSYM(XS,ZS,NSRC,2.*DREC,IPEN)
         END IF
         IF(QTPLT3) THEN
            CALL PLOTE
            STOP
         END IF


         IF(.NOT.PLTRAY) CALL PLOTE
      
      END IF


      IF(PLTRAY.OR.LIST) THEN
c        proceed
      ELSE IF(SHTREC) THEN
c        proceed
      ELSE
c        nothing else to do
         STOP
      END IF

      IF(LIST)   TINFO = .TRUE.
      IF(SHTREC) TINFO = .TRUE.

c     Read the remainder of param1

c     Read the name to be given the output files, if there are to be any.
      READ(PARIN,'(A)',END=500) OUTNAM

      IF(LIST.OR.SHTREC) THEN
c        Count the number of characters in the name, up to first blank.
         J = 1
300      IF(OUTNAM(J:J).EQ.' ') THEN
         ELSE
            J = J + 1
            GO TO 300
         END IF
         NCHARC = J - 1
      END IF

      IF(LIST) THEN
         OUTFIL = OUTNAM(1:NCHARC)//'data'
         OPEN(UNIT=RAYOUT,FILE=OUTFIL,ERR=320)
         GO TO 325
320      WRITE(STDERR,'(1X,A)')'Can''t open ray data file.'
         STOP
325      REWIND RAYOUT

         OUTFIL = OUTNAM(1:NCHARC)//'listing'
         OPEN(UNIT=RAYLST,FILE=OUTFIL,ERR=330)
         GO TO 335 
330      WRITE(STDERR,'(A)') 'Can''t open ray listing file.'
         STOP
335      REWIND RAYLST
      END IF


c     Want to generate a shot record ?
      UNFORM = .TRUE.
      IF(SHTREC) THEN
         OUTFIL = OUTNAM(1:NCHARC)//'shot'
         IF(UNFORM) THEN
            OPEN(UNIT=SHTOUT,FORM='UNFORMATTED',FILE=OUTFIL,
     :      ERR=370)
            GO TO 375
         ELSE
            OPEN(UNIT=SHTOUT,FILE=OUTFIL,ERR=370)
            GO TO 375
         END IF
370      WRITE(STDERR,'(1X,A)') 'Error creating output trace file.'
         STOP
375      REWIND SHTOUT
      END IF

      READ(PARIN,*) BETAI,BETAF

      IF(BETAI.GT.BETAF) THEN
         WRITE(STDERR,'(1X,A)') 'MAIN : must have betaf > betai'
         STOP
      END IF

c     read search angles and ray density
      READ(PARIN,*) DELTAB,DELTAG,RAYDEN
      IF(DELTAB.LE.0.) THEN
         WRITE(STDERR,'(1X,A)') 'MAIN : deltab must be positive'
         STOP
      END IF
      IF(DELTAG.LE.0.) THEN
         WRITE(STDERR,'(1X,A)') 'MAIN : deltag must be positive'
         STOP
      END IF
      IF(RAYDEN.LE.0.) THEN
         WRITE(STDERR,'(1X,A)') 'MAIN : ray density must be positive'
         STOP
      END IF

c     read layer velocities
      READ(PARIN,*,END=400) (VEL(I),I=1,NINT+1)
      GO TO 405
400   WRITE(STDERR,'(1X,A)')
     :'Not enough velocities - need one more than #interfaces.'
      IF(BEGPLT) CALL PLOTE
      STOP
405   CONTINUE


c     Now read in events
      NEVENT = 0
c     want direct wave?
      READ(PARIN,'(A)') YORN 
      IF(YORN.EQ.'y') THEN
         NEVENT = 1
         EVTYPE(NEVENT) = 'd'
      END IF

c     want all primaries?
      READ(PARIN,'(A)',END=500) YORN
      IF(YORN.EQ.'y') THEN
c        calculate primary reflections
         DO 410 K = 1, NINT
            NEVENT = NEVENT + 1
            EVTYPE(NEVENT) = 'r'
            NREFLS(NEVENT) = 1
            IREFL(NEVENT,1) = K
410         CONTINUE
      END IF

c     want all head waves?
      READ(PARIN,'(A)',END=500) YORN
      IF(YORN.EQ.'y') THEN
c        calculate head waves
         DO 415 K = 1, NINT
            NEVENT = NEVENT + 1
            EVTYPE(NEVENT) = 'h'
            NREFLS(NEVENT) = 1
            IREFL(NEVENT,1) = K
415         CONTINUE
      END IF

c     now come the extra events
420   READ(PARIN,'(A,A)',END=425) RORH,EVENT
      IF(RORH.EQ.'H') RORH = 'h'
      IF(RORH.EQ.'R') RORH = 'r'
      NEVENT = NEVENT + 1
      IF(RORH.EQ.'r'.OR.RORH.EQ.'h') THEN
         CALL SETREF(EVENT,IREFL,NREFLS(NEVENT),NEVENT,
     :   VALID,MAXEVT,MAXREF,NINT)
         IF(NREFLS(NEVENT).EQ.0) THEN
c           not an event - probably a blank line
            NEVENT = NEVENT - 1
         ELSE IF(.NOT.VALID) THEN
            NEVENT = NEVENT - 1
            WRITE(STDERR,'(1X,A)')
     :      'Invalid extra event specification'
         ELSE
            EVTYPE(NEVENT) = RORH
         END IF
      ELSE
         NEVENT = NEVENT - 1
         WRITE(STDERR,'(1X,A)')
     :   'Extra event must begin with r or h'
      END IF
      GO TO 420
425   CONTINUE

      GO TO 505
500   WRITE(STDERR,'(1X,A)')
     :'MAIN: param1 not filled-out.'
      STOP
505   CONTINUE


      IF(LIST) THEN

         WRITE(RAYLST,'(/,20X,A/)')
     :   'XWELL Listing File'
         WRITE(RAYLST,'(/2X,A)') 'Velocities:'
         DO 605 I = 1,  NINT + 1
            WRITE(RAYLST,'(2X,A,I2,1X,F8.1)')
     :      'layer ',I,VEL(I)
605         CONTINUE

         IF(DOWNHL) THEN
            WRITE(RAYLST,'(/2X,A,F8.2,A,F8.2)')
     :      'Top of receiver well is at coordinates ',XRWELL(0),',',
     :      ZRWELL(0)
         END IF
         IF(DOWNHL) THEN
            WRITE(RAYLST,'(2X,A,F8.2,A,F8.2/)')
     :      'Top of source well is at coordinates ',XSWELL(0),',',
     :      ZSWELL(0)
         END IF
         WRITE(RAYLST,'(/,2X,A,I4)') 'Number of shots = ',NSRC
         WRITE(RAYLST,'(2X,A,I4)') 'Number of events per shot = ',
     :   NEVENT

         WRITE(RAYLST,'(/2X,A,11X,A,9X,A)')'shot',
     :   'x-z coordinates',
     :   'layer number'
         DO 350 K = 1,  NSRC
            WRITE(RAYLST,'(2X,I3,10X,F8.2,2X,F8.2,12X,I2)')
     :      K,XS(K),ZS(K),SLAYER(K)
350         CONTINUE

         WRITE(RAYLST,'(/2X,A,7X,A,9X,A)')'receiver',
     :   'x-z coordinates',
     :   'layer number'
         DO 355 K = 1,  NREC
            WRITE(RAYLST,'(2X,I3,10X,F8.2,2X,F8.2,12X,I2)')
     :      K,XREC(K),ZREC(K),RLAYER(K)
355         CONTINUE

      END IF




c     VEL(0) identifies reflections from the surface of the
c     earth ( reflection coefficient is then set to -1 ).
      VEL(0) = 0.

ccc   Main loop over shots 
c-------------------------

      DO 2000 IRECD = 1,  NSRC

      X(0) = XS(IRECD)
      Z(0) = ZS(IRECD)

      CALL SRCPOS(XS(IRECD),ZS(IRECD),
     :ZRWELL,XRWELL(0),RW0,RW1,RW2,RW3,LEFT,RIGHT)



ccc   Do for each event
c----------------------

      DO 1000 IEVENT = 1,  NEVENT

      IF(LIST) THEN
         WRITE(RAYLST,'(3(/),2X,A,I4,6X,A,I4/)')
     :   'Shot ',IRECD,'event ',IEVENT
      END IF

      IF(EVTYPE(IEVENT).EQ.'d') THEN
         IF(LIST) WRITE(RAYLST,'(2X,A/)')'This is a direct wave.'
         LREF = 0
      ELSE
         IF(EVTYPE(IEVENT).EQ.'r') THEN
            IF(LIST) WRITE(RAYLST,'(2X,A/)')'This is a reflection.'
         ELSE
            IF(LIST) WRITE(RAYLST,'(2X,A/)')'This is a head wave.'
            X(0) = XS(IRECD)
            Z(0) = ZS(IRECD)
         END IF
         CALL ORDER(IREFL,NREFLS(IEVENT),SLAYER(IRECD),IEVENT,
     :           VEL,NORDER,V,SIGN,VREF,N,
     :           VALID,LIST,RAYLST,STDERR,MAXEVT,MAXREF,
     :           NINT,LREF,EVTYPE(IEVENT),SINTHC)
         IF(.NOT.VALID) GO TO 1000
      END IF



c     Initialize
      NNEW = 0
      NP1 = N + 1
      BETA = BETAI
      DB = DELTAB
      NT = 0
      TURNUP = .FALSE.
      TURNDN = .FALSE.
      NEWBRN = .FALSE.
      SEARCH = .FALSE.
      SAMEBR = .FALSE.
      RESTART = .TRUE.
      

      IF(EVTYPE(IEVENT).EQ.'h') THEN

c     Head Wave
c     After critical ray is found, source point runs
c     along refractor shooting rays at critical angle.
c     dxbig is step size along refractor while searching for 
c     a well intersection. dxmall is used in vicinity of well.

      DXBIG = DREC
      DXSMLL = DXBIG/10.
      DX = DXBIG

c     Search for critical intersection point
c     First find where incident angle passes through critical
      THETAC = ASIN(ABS(SINTHC))
      DANOLD = 0.
      SINTHR = 0.
      NEXT = .FALSE.
      FAIL = .FALSE.

650   IF(BETA.LE.BETAF) THEN

         CALL SHOOT(X,Z,BETA,PI,DREC,NNEW,XSTART,XEND,
     :   LREF,SINTHR)

cpaul    if(allray) CALL RAYplt(X,Z,nnew-1,ipen)

c        check to see if ray reached target refractor..
         IF(NNEW.LE.LREF) NEXT = .TRUE.

         IF(LEFT.AND.SINTHR.LE.0.) NEXT = .TRUE.
         IF(RIGHT.AND.SINTHR.GT.0.)NEXT = .TRUE.

         IF(NEXT) THEN
            NEXT = .FALSE.
            BETA = BETA + DELTAB
            IF(ALLRAY) CALL RAYPLT(X,Z,LREF-1,IPEN)
            GO TO 650
         END IF

c        difference between angle ray makes with normal at
c        refractor and critical angle (thetac)
         DANGLE = 180. * (ASIN(ABS(SINTHR)) - THETAC)/PI

         IF(DANGLE.NE.0.) THEN
            IF(DANOLD/DANGLE.GE.0.) THEN
               IF(ALLRAY) CALL RAYPLT(X,Z,LREF-1,IPEN)
               DANOLD = DANGLE
               BETA = BETA + DELTAB
               GO TO 650
            END IF
         END IF

c        ...passed though critical
c        Converge on critical ray...

         IF(DANGLE.EQ.0.) THEN
c           at critical
         ELSE 
c           danold/dangle < 0., ie sign change
c           if(pltray) then
c              ipen = icolor(5)
c              CALL RAYplt(X,Z,nnew-1,ipen)
c           end if
c           now use bisection to zero in on critical ray
            CALL BISECT(BETA,DELTAB,THETAC,SINTHR,PI,
     :      XSTART,XEND,DREC,LREF,X,Z)
         END IF

ccc      Found Critical Ray

         IF(PLTRAY) THEN
            IPEN = ICOLOR(5)
            CALL RAYPLT(X,Z,LREF-1,IPEN)
c           CALL RAYplt(X,Z,nnew-1,ipen)
         END IF

         IF(LIST) THEN
            WRITE(RAYLST,'(2X,A/)')'Source segment of raypath:'
            CALL XZOUT(X,Z,LREF,RAYLST)
            WRITE(RAYLST,'(/2X,A/)')'Receiver segments follow...'
         END IF
      ELSE

c        couldn't find critical intersection
         GO TO 1000

      END IF




ccc   Calculate traveltime between source and refractor
      CALL TTIME(N,D,V,T1,LREF-1)
c     save coordinates of intersection with refractor
      X1 = X(LREF)
      Z1 = Z(LREF)

      FIRST = .TRUE.

c     calculate amplitude (transmission effects) down
c     to refractor
      CALL RAYDAT(ZW,VREF,IREFL,IEVENT,AMPTMP,PHSTMP,
     :MAXEVT,MAXREF,RW1,RW2,RW3,ZRWELL,
     :NRWELL,LREF-1,ICP1,NORDER,TC,.TRUE.)

c     constant used in head wave amplitude calculations...
      HDAMP = V(LREF) * SINTHC / 
     :        ( 2. * PI * (1.-SINTHC*SINTHC) )

c     multiply by transmission effects between source and refractor
      DO 660 I = 1,  LREF - 1
         HDAMP = HDAMP * TC(I)
660      CONTINUE
      

      IF(LEFT) THEN
c        source to left of well, move to right along interface
         SIGNDX = 1.
      ELSE
c        source to right of well, move to left along interface
         SIGNDX = -1.
      END IF

ccc   NOTE
c     Caustics in head wave field not supported (ie a phase shifted
c     head wave wavelet is NOT used---CSHOT2 does not have this 
c     capability at present.  The caustic rays are calculated here
c     but a normal head wave wavelet will be placed on wiggle trace.)
      CAUSTC = .FALSE.
ccc


c     Next, move along interface shooting rays at critical angle.

c     Interface number of refractor (refractor number is the last
c     interface in the specification - there may be many reflections
c     before ray reaches the refractor)...
      IRFRCT = IREFL(IEVENT,NREFLS(IEVENT))

c     calculate point on refractor for source of receiver portion
c     of head wave ray, also calculate takeoff angle relative to vertical
      CALL HWSRC(X(LREF),SINTHC,IRFRCT,DX,X(0),Z(0),LREF,
     :UP,DOWN,BETA,SIGNDX,PI,NEWSLY,XSTART,XEND,FAIL,
     :XCROSS,LEFTHW,RIGHTHW)

      IF(FAIL) GO TO 1000

c     receiver portion of ray is viewed as a direct ray, the source
c     of which lies on the refractor
c     set some parameters for this direct ray...
      IR1TMP = IREFL(IEVENT,1)
      CALL SETDIR(LEFTHW,UP,DOWN,BOLD,BETA,SIGN,
     :X(0),Z(0),NEWSLY,NINT,XCROSS,ZCROSS,PI,N,NP1,
     :NORDER,V,VEL,IREFL,MAXEVT,IEVENT,'h')


c     Now start shooting rays from source points on the refractor
c     Check that source is within limits of model

1750  IF(X(0).GT.XSTART.AND.X(0).LT.XEND) THEN

         FAIL = .FALSE.

         IF(RESTART) THEN

c           shoot a ray from this point on interface
            CALL SHOOT(X,Z,BETA,PI,DREC,NNEW,XSTART,XEND,0,SINTHR)

c           check for intersection with receiver well...
            CALL XZWELL(X,Z,XCROSS,ZCROSS,0,NORDER,N,NNEW,NINT,FAIL,
     :               ICROSS,IINT,IINTP1,NEWSLY,LEFTHW,RIGHTHW,
     :               ZRWELL,NRWELL,RW0,RW1,RW2,RW3,XW,ZW,DELTAX,
     :               DELTAZ,D,ICP1)

            IF(.NOT.FAIL) THEN
c              Found a ray that hits the well.
c              Backspace to previous source location and
c              approach well more carefully...
               X0 = X(0) - DXBIG 
               DX = DXSMLL
c              begin search for next ray
               SEARCH = .TRUE. 
               RESTART = .FALSE.
               X(ICP1) = XW
               Z(ICP1) = ZW
               IF(PLTRAY) THEN
c                 plot first ray a different color
c                 ipen = icolor(7)
c                 call pen(ipen)
                  CALL RAYPLT(X,Z,ICROSS,IPEN)
               END IF
            ELSE
c              continue to search for well
               IF(ALLRAY) THEN
                  IPEN = ICOLOR(4)
c                 call pen(ipen)
                  CALL RAYPLT(X,Z,NNEW-1,IPEN)
               END IF
               X0 = X(0) 
               DX = DXBIG
            END IF

c           move to next source location on refractor
            CALL HWSRC(X0,SINTHC,IRFRCT,DX,X(0),Z(0),LREF,
     :      UP,DOWN,BETA,SIGNDX,PI,NEWSLY,XSTART,XEND,FAIL,
     :      XCROSS,LEFTHW,RIGHTHW)

            GO TO 1750

         END IF


         IF(SEARCH) THEN

c           Search for the first ray on branch
c           shoot a ray from this source location
            CALL SHOOT(X,Z,BETA,PI,DREC,NNEW,XSTART,XEND,0,SINTHR)

c           check for well intersection
            CALL XZWELL(X,Z,XCROSS,ZCROSS,0,NORDER,N,NNEW,NINT,FAIL,
     :                 ICROSS,IINT,IINTP1,NEWSLY,LEFTHW,RIGHTHW,
     :                 ZRWELL,NRWELL,RW0,RW1,RW2,RW3,XW,ZW,DELTAX,
     :                 DELTAZ,D,ICP1)

            IF(.NOT.FAIL) THEN
c              found first ray on branch
               NT = 0
               SEARCH = .FALSE.
               SAMEBR = .TRUE.
               INTLAY = MAX(IINT,IINTP1)
               ILPREV = INTLAY
               X(ICP1) = XW
               Z(ICP1) = ZW
               XWPREV = XW
               ZWPREV = ZW
               IF(PLTRAY) THEN
                  IPEN = ICOLOR(5)
c                 call pen(ipen)
                  CALL RAYPLT(X,Z,ICROSS,IPEN)
               END IF
               IF(FIRST.AND.TINFO) THEN
c                 calculate traveltime along refractor for first ray
                  CALL TINTEG(X1,Z1,IRFRCT,
     :            VEL(IRFRCT+1),X(0),Z(0),T2,
     :            XINT,A0,A1,A2,A3,MAXINT,MAXSPL,MXSPM1)
                  FIRST = .FALSE.
                  XOLD(0) = X(0)
                  ZOLD(0) = Z(0)
               END IF
            ELSE
               IF(ALLRAY) THEN
                  IPEN = ICOLOR(4)
c                 call pen(ipen)
                  CALL RAYPLT(X,Z,NNEW-1,IPEN)
               END IF
            END IF

c           move to next source position
            X0 = X(0)
            DX = DXSMLL
            CALL HWSRC(X0,SINTHC,IRFRCT,DX,X(0),Z(0),LREF,
     :      UP,DOWN,BETA,SIGNDX,PI,NEWSLY,XSTART,XEND,FAIL,
     :      XCROSS,LEFTHW,RIGHTHW)

            GO TO 1750

         END IF



         IF(SAMEBR) THEN

c           look for next ray on this branch
c           shoot a ray from this source location
            CALL SHOOT(X,Z,BETA,PI,DREC,NNEW,XSTART,XEND,
     :      0,SINTHR)

c           check for well intersection
            CALL XZWELL(X,Z,XCROSS,ZCROSS,0,NORDER,N,NNEW,NINT,FAIL,
     :                 ICROSS,IINT,IINTP1,NEWSLY,LEFTHW,RIGHTHW,
     :                 ZRWELL,NRWELL,RW0,RW1,RW2,RW3,XW,ZW,DELTAX,
     :                 DELTAZ,D,ICP1)

            IF(.NOT.FAIL) THEN
               X(ICP1) = XW
               Z(ICP1) = ZW
               INTLAY = MAX(IINT,IINTP1)
               IF(INTLAY.EQ.ILPREV) THEN
                  NEWBRN = .FALSE.
                  ILPREV = INTLAY
               ELSE
c                 write(*,*)'New Branch',intlay,ilprev
c                 read(*,'(a)')yorn
                  NEWBRN = .TRUE.
               END IF

               DW = SQRT( (XW-XWPREV)**2 + (ZW-ZWPREV)**2 )
   
               IF(TINFO) THEN
c                 integrate traveltime along refractor from previous
c                 ray to current ray
                  CALL TINTEG(XOLD(0),ZOLD(0),IRFRCT,
     :            VEL(IRFRCT+1),X(0),Z(0),DT2,
     :            XINT,A0,A1,A2,A3,MAXINT,MAXSPL,MXSPM1)
                  T2 = T2 + DT2
                  XOLD(0) = X(0)
                  ZOLD(0) = Z(0)
               END IF

               IF(.NOT.NEWBRN) THEN
                  IF(TINFO) THEN
                     CALL TTIME(N,D,V,T3,ICROSS)
c                    this is the total traveltime...
                     TTMP = T1 + T2 + T3
                  END IF
                  IF(SHTREC) THEN
c                    compute amplitude factors on receiver segment of ray
                     CALL RAYDAT(ZW,VREF,IREFL,IEVENT,AMPTMP,PHSTMP,
     :               MAXEVT,MAXREF,RW1,RW2,RW3,ZRWELL,
     :               NRWELL,ICROSS,ICP1,NORDER,TC,.TRUE.)

                     AMPTMP = HDAMP / (XS(IRECD)-XW)**2
c                    apply receiver segment transmission factors
                     DO 1650 I = 1,  ICROSS
                        AMPTMP = AMPTMP * TC(I)
1650                    CONTINUE
                     IF(NT.EQ.MAXT) THEN
                        WRITE(STDERR,'(1X,A)')'Too many rays in layer'
                        GO TO 990 
                     END IF
                     NT = NT + 1
                     T(NT) = TTMP
                     AMP(NT) = AMPTMP 
                     PHASE(NT) = 0.
                     ZEND(NT) = ZW
                  END IF
                  IF(LIST) THEN
c                    write(rayout,*)n+2,ttmp
c                    call xzout(x,z,icross+1,rayout)
                     CALL XZOUT(X,Z,ICROSS+1,RAYLST)
                     WRITE(RAYLST,'(3X,A,F10.6/)') 't = ',TTMP
                  END IF
               END IF

c              now calculate dx from change in end points of ray
c              don't let dx get too big though
               IF(DW.GT.0.)DX = DX * DREC / (DW*RAYDEN)
               DX = MIN(DX,DXBIG)

               XWPREV = XW
               ZWPREV = ZW
               IF(PLTRAY) THEN
                  IPEN = ICOLOR(5)
                  CALL RAYPLT(X,Z,ICROSS,IPEN)
               END IF
            ELSE
c              couldn't find well intersection
c              read(*,'(a)')yorn
               NEWBRN = .TRUE.
               SAMEBR = .FALSE.
               RESTART = .TRUE.
               DX = DXBIG
               IF(ALLRAY) THEN
                  IPEN = ICOLOR(4)
                  CALL RAYPLT(X,Z,NNEW-1,IPEN)
               END IF
            END IF

            X0 = X(0)
            CALL HWSRC(X0,SINTHC,IRFRCT,DX,X(0),Z(0),LREF,
     :      UP,DOWN,BETA,SIGNDX,PI,NEWSLY,XSTART,XEND,FAIL,
     :      XCROSS,LEFTHW,RIGHTHW)

         END IF


         IF(NEWBRN) THEN

c           Ouput data from previous branch

            IF(NT.GE.2.AND.NRLAYR(ILPREV).GT.0) THEN
               CALL LININT(ZEND,T,AMP,PHASE,WORK,NT,
     :         D0,D1,SPFAIL,FIRSTR(ILPREV),LASTR(ILPREV),
     :         NRLAYR(ILPREV),ZREC,NREC,TREC,
     :         AMPREC,PHSREC,NREC1,NRECN,TURNUP,TURNDN)

               IF(SHTREC) THEN
c                 if(outt) then
c                    call output(nrec1,nrecn,
c    :               zrec,nrec,trec,zend,t,
c    :               nt,nrlayr(ilprev),rayout,irecd,ievent)
c                 else if(outa) then
c                    call output(nrec1,nrecn,
c    :               zrec,nrec,amprec,zend,amp,
c    :               nt,nrlayr(ilprev),rayout,irecd,ievent)
c                 else if(outp) then
c                    call output(nrec1,nrecn,
c    :               zrec,nrec,phsrec,zend,phase,
c    :               nt,nrlayr(ilprev),rayout,irecd,ievent)
c                 end if

                  CALL WRTSHT(NREC1,NRECN,ZREC,NREC,
     :            TREC,AMPREC,PHSREC,CAUSTC,
     :            NRLAYR,SHTOUT,IRECD,IEVENT,UNFORM,.TRUE.)
               END IF

            END IF
            NT = 0
            NEWBRN = .FALSE.
            TURNUP = .FALSE.
            TURNDN = .FALSE.
            ILPREV = INTLAY
      
         END IF

         GO TO 1750

      END IF
            

990   CONTINUE
      IREFL(IEVENT,1) = IR1TMP

ccc   END OF HEAD WAVE IF
      END IF


      IF(EVTYPE(IEVENT).EQ.'h') GO TO 1000



c------
c     Begin searching for a ray that intersects the well


750   IF(BETA.LE.BETAF) THEN

         FAIL = .FALSE.
         IF(EVTYPE(IEVENT).EQ.'d') THEN
c           for a direct wave, the order of intersections may
c           change with takeoff angle...
            CALL SETDIR(LEFT,UP,DOWN,BOLD,BETA,SIGN,
     :      XS(IRECD),ZS(IRECD),SLAYER(IRECD),NINT,
     :      XCROSS,ZCROSS,PI,N,NP1,
     :      NORDER,V,VEL,IREFL,MAXEVT,IEVENT,'d')
         END IF


         IF(RESTART) THEN

c           shoot a ray at takeoff angle beta
            CALL SHOOT(X,Z,BETA,PI,DREC,NNEW,XSTART,XEND,
     :      LREF,SINTHR)

            CALL XZWELL(X,Z,XCROSS,ZCROSS,LREF,NORDER,N,NNEW,NINT,FAIL,
     :                    ICROSS,IINT,IINTP1,SLAYER(IRECD),LEFT,RIGHT,
     :                    ZRWELL,NRWELL,RW0,RW1,RW2,RW3,XW,ZW,DELTAX,
     :                    DELTAZ,D,ICP1)

            IF(.NOT.FAIL) THEN
c              Found a ray that hits the well.
c              Backspace to previous takeoff angle and
c              approach well more carefully...
               IF(EVTYPE(IEVENT).EQ.'d') THEN
                  BETA = BOLD - DELTAB + DELTAG
               ELSE
                  BETA = BETA - DELTAB + DELTAG
               END IF
c              begin search for next ray
               SEARCH = .TRUE. 
               RESTART = .FALSE.
               DB = DELTAG
               X(ICP1) = XW
               Z(ICP1) = ZW
               IF(PLTRAY) THEN
c                 plot first ray a different color
c                 ipen = icolor(7)
                  IPEN = ICOLOR(5)
                  CALL RAYPLT(X,Z,ICROSS,IPEN)
               END IF
            ELSE
               IF(ALLRAY) THEN
                  IPEN = ICOLOR(4)
                  CALL RAYPLT(X,Z,NNEW-1,IPEN)
               END IF
               IF(EVTYPE(IEVENT).EQ.'d') BETA = BOLD
               BETA = BETA + DELTAB
            END IF

            GO TO 750

         END IF


         IF(SEARCH) THEN

c           Search for the first ray on branch
c           shoot a ray at takeoff angle beta
            CALL SHOOT(X,Z,BETA,PI,DREC,NNEW,XSTART,XEND,
     :      LREF,SINTHR)

            CALL XZWELL(X,Z,XCROSS,ZCROSS,LREF,NORDER,N,NNEW,NINT,FAIL,
     :                 ICROSS,IINT,IINTP1,SLAYER(IRECD),LEFT,RIGHT,
     :                 ZRWELL,NRWELL,RW0,RW1,RW2,RW3,XW,ZW,DELTAX,
     :                 DELTAZ,D,ICP1)

            IF(.NOT.FAIL) THEN
c              found first ray on branch
               NT = 0
               SEARCH = .FALSE.
               SAMEBR = .TRUE.
               INTLAY = MAX(IINT,IINTP1)
               ILPREV = INTLAY
               X(ICP1) = XW
               Z(ICP1) = ZW
               XWPREV = XW
               ZWPREV = ZW
               ICOLD = ICP1
               ICAUS1 = 1
               DO 600 I = 0, ICP1
                  XOLD(I) = X(I)
                  ZOLD(I) = Z(I)
600               CONTINUE
               IF(PLTRAY) THEN
                  IPEN = ICOLOR(5)
                  CALL RAYPLT(X,Z,ICROSS,IPEN)
               END IF
            ELSE
               IF(ALLRAY) THEN
                  IPEN = ICOLOR(4)
                  CALL RAYPLT(X,Z,NNEW-1,IPEN)
               END IF
            END IF
            IF(EVTYPE(IEVENT).EQ.'d') BETA = BOLD

            BETA = BETA + DELTAG

            GO TO 750

         END IF



         IF(SAMEBR) THEN

c           look for next ray on this branch
c           shoot a ray at takeoff angle beta
            CALL SHOOT(X,Z,BETA,PI,DREC,NNEW,XSTART,XEND,
     :      LREF,SINTHR)

            CALL XZWELL(X,Z,XCROSS,ZCROSS,LREF,NORDER,N,NNEW,NINT,FAIL,
     :                 ICROSS,IINT,IINTP1,SLAYER(IRECD),LEFT,RIGHT,
     :                 ZRWELL,NRWELL,RW0,RW1,RW2,RW3,XW,ZW,DELTAX,
     :                 DELTAZ,D,ICP1)

            IF(.NOT.FAIL) THEN
               X(ICP1) = XW
               Z(ICP1) = ZW
c              check for caustic
               INTLAY = MAX(IINT,IINTP1)
               NSEG = MIN(ICP1,ICOLD)
               CALL CHKCST(XOLD,ZOLD,X,Z,ICP1,NSEG,ICAUS2)
               IF(ICAUS1.EQ.ICAUS2.AND.INTLAY.EQ.ILPREV) THEN
                  NEWBRN = .FALSE.
                  ILPREV = INTLAY
               ELSE
                  NEWBRN = .TRUE.
               END IF
               IF(ICAUS1.EQ.0) THEN
                  CAUSTC = .TRUE.
               ELSE
                  CAUSTC = .FALSE.
               END IF
               IF(ICAUS1.NE.ICAUS2) THEN
                  IF(ZOLD(ICOLD).GT.Z(ICP1)) THEN
                     TURNUP = .TRUE.
                  ELSE
                     TURNDN = .TRUE.
                  END IF
               END IF

               DW = SQRT( (XW-XWPREV)**2 + (ZW-ZWPREV)**2 )
   
               IF(.NOT.NEWBRN) THEN
                  IF(TINFO) THEN
                     CALL TTIME(N,D,V,TTMP,ICROSS)
                  END IF
                  IF(SHTREC) THEN
c                    compute in-plane spreading
                     IF(DW.GT.0.)THEN
                        INPLAN = SQRT( V(1)*DB*PI / (DW*180.) )/FOURPI
                     ELSE
                        INPLAN = 0.
                     END IF
c                    compute rest of amplitude factors
                     CALL RAYDAT(ZW,VREF,IREFL,IEVENT,AMPTMP,PHSTMP,
     :               MAXEVT,MAXREF,RW1,RW2,RW3,ZRWELL,
     :               NRWELL,ICROSS,ICP1,NORDER,TC,.FALSE.)

                     NT = NT + 1
                     T(NT) = TTMP
                     AMP(NT) = AMPTMP * INPLAN
                     PHASE(NT) = PHSTMP
                     ZEND(NT) = ZW
                  END IF
                  IF(LIST) THEN
c                    write(rayout,*)n+2,ttmp
c                    call xzout(x,z,n+1,rayout)
                     CALL XZOUT(X,Z,N+1,RAYLST)
                     WRITE(RAYLST,'(3X,A,F10.6/)') 't = ',TTMP
                  END IF
               END IF

c              now calculate db from change in end points of ray
c              don't let db get too big though
               IF(DW.GT.0.)DB = DB * DREC / (DW*RAYDEN)
               DB = MIN(DB,DELTAB)
ccc            watch out for db=0.

               XWPREV = XW
               ZWPREV = ZW
               ICOLD = ICP1
               ICAUS1 = ICAUS2
               DO 700 I = 0, ICP1
                  XOLD(I) = X(I)
                  ZOLD(I) = Z(I)
700               CONTINUE
               IF(PLTRAY) THEN
                  IPEN = ICOLOR(5)
                  CALL RAYPLT(X,Z,ICROSS,IPEN)
               END IF
            ELSE
               NEWBRN = .TRUE.
               SAMEBR = .FALSE.
               RESTART = .TRUE.
               DB = DELTAB
               IF(ALLRAY) THEN
                  IPEN = ICOLOR(4)
                  CALL RAYPLT(X,Z,NNEW-1,IPEN)
               END IF
            END IF

            IF(EVTYPE(IEVENT).EQ.'d') BETA = BOLD
            BETA = BETA + DB

         END IF


         IF(NEWBRN) THEN

c           Ouput data from previous branch

            IF(NT.GE.2.AND.NRLAYR(ILPREV).GT.0) THEN
               CALL LININT(ZEND,T,AMP,PHASE,WORK,NT,
     :         D0,D1,SPFAIL,FIRSTR(ILPREV),LASTR(ILPREV),
     :         NRLAYR(ILPREV),ZREC,NREC,TREC,
     :         AMPREC,PHSREC,NREC1,NRECN,TURNUP,TURNDN)

               IF(SHTREC) THEN
c                 if(outt) then
c                    call output(nrec1,nrecn,
c    :               zrec,nrec,trec,zend,t,
c    :               nt,nrlayr(ilprev),rayout,irecd,ievent)
c                 else if(outa) then
c                    call output(nrec1,nrecn,
c    :               zrec,nrec,amprec,zend,amp,
c    :               nt,nrlayr(ilprev),rayout,irecd,ievent)
c                 else if(outp) then
c                    call output(nrec1,nrecn,
c    :               zrec,nrec,phsrec,zend,phase,
c    :               nt,nrlayr(ilprev),rayout,irecd,ievent)
c                 end if

                  CALL WRTSHT(NREC1,NRECN,ZREC,NREC,
     :            TREC,AMPREC,PHSREC,CAUSTC,
     :            NRLAYR,SHTOUT,IRECD,IEVENT,UNFORM,.FALSE.)
               END IF

            END IF
            NT = 0
            NEWBRN = .FALSE.
            TURNUP = .FALSE.
            TURNDN = .FALSE.
            ILPREV = INTLAY
      
         END IF

         GO TO 750

      END IF

      IF(LIST) THEN
         WRITE(RAYLST,'(/2X,A/)')
     :   'End of Event'
      END IF


1000  CONTINUE

      IF(LIST) THEN
         WRITE(RAYLST,'(/2X,A//)')
     :   'End of Shot'
      END IF

2000  CONTINUE

c     close files
      IF(PLTRAY) THEN
         CALL PLOTE
      END IF

      IF(SHTREC) THEN
         CLOSE(UNIT=SHTOUT,STATUS='keep')
      END IF

      IF(LIST) THEN
         CLOSE(UNIT=RAYOUT,STATUS='keep')
         CLOSE(UNIT=RAYLST,STATUS='keep')
      END IF


      STOP

      END

*--------------- end of main program -------------------------------
