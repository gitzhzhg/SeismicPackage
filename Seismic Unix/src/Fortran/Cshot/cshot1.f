c--------------------------------------------------------------------

cc List of variables:

c  A0(,),A1(,)  Cubic spline coefficients of the interfaces.  First index
c  A2(,),A3(,)  defines the interface, second index the portion of the 
c               interface.
c  AMP      Amplitude of the wave (wavelet is scaled by this amount)
c  AMP1,2   Part of the amplitude that can be calculated from knowledge
c           of the raypath alone
c  BEGPLT   TRUE to begin plotting
c  BELAST   Takeoff angle of last good ray found by continuation (this
c           ray does not necessarily end at a receiver location)
c  BETA     Takeoff angle at the source
c  BETA1,2,3  Values of three adjacent rays making up the ray tube 
c           in the amplitude calculatiuon
c  BETAF    Final takeoff angle in shooting search
c  BETAI    Initial takeoff angle in shooting search
c  BETNEW   Takeoff angle of new ray found by continuation procedure
c  CARD     Character record read from geometry file (depending on the
c           record, either land or marine shooting is specified)
c  CAUSTC   True if the ray has passed through a caustic
c  COLORS   Name of file containing plot colors
c  COLSIN   Unit number of plot colors file
c  CONST    Constant used in the amplitude calculation
c  D()      Length of ray segment
c  DBETA    Change in BETA for raya making up the ray tube
c  DBMAX    Max allowed change in BETA. If DBMAX and DX1MAX exceeded
c           then suspect a missing branch of ray solutions
c  DDZ()    Second derivative of each interface at intersection point
c  DELTAB   Increment in takeoff angle in shooting search
c  DELTAX() x-distance travelled within a layer
c  DELTAZ() z-distance travelled within a layer
c  DETJ     Value of the determinant of the jacobian
c  DOWNHL   TRUE if shooting in downhole mode
c  DSRC     Spacing (arc length) between sources down the well
c  DUMML    Logical dummy variable (value not used)  
c  DUMMY    Real dummy variable (value not used)
c  DX1      Change in X(1) from one ray to next.  Used, along with the 
c           change in takeoff angle, to check for a missing branch - both
c           will be large when a branch has been skipped.
c           (When a source is located near an interface the change in
c           takeoff angle may be large even though no branches are
c           missing.  DX1, on the other hand, will not be large.)
c  DX1FAC   DX1FAC times TRUGEO is the maximum allowed change in DX1
c  DX1MAX   Maximum allowed change in DX1 (when DX1MAX and DBMAX are
c           exceeded then program suspects a missing branch)
c  DZ()     First derivative of each interface at intersection point
c  EOF      TRUE if end of file has been reached (cards file)
c  EVENT    Character variable.  May be the list of refracting interfaces
c           or a list of reflectors making up an extra event (e.g. a multiple)
c  EVTYPE() Character array describing type of event (direct, head wave,
c           or reflection
c  FIRST    TRUE if this is the first ray in a branch of solutions
c  FSHOT    Shot location, referenced to receiver stations.  FSHOT is a float,
c           thus allowing the shots to be located between receiver stations.
c  GEOINC   x-distance between end point of ray and next receiver
c  GEOMFL   Name of file containing shot cards
c  GEOMS    Unit number of shot cards file
c  GEOZ     z-distance between end point of ray and next receiver
c  HEAD     TRUE if this is a headwave
c  I        Loop variable
c  ICOLOR() Integer array containing colors for the plot
c  IDUMMY   Dummy integer variable (value not used)
c  IEVENT   Event number
c  IHEAD    Number of head waves counter
c  INGAP    TRUE if current station lies within the gap
c  INSIDE   TRUE if station lies inside limits of model
c  INTERF   Unit number of model file
c  IPEN     Pen color for plotting
c  IREC     Receiver number or counter
c  IRECD    Shot (record) counter
c  IRECP    Receiver number of previous ray
c  IREFL(,) First dimension is the event number.  Second dimension 
c           is a list of the reflecting interfaces met by the ray.
c  ITRACE   Trace number
c  J        Loop variable
c  JOBTYP   Job descriptor. May be r or R for ray plot, l or L for 
c           listing,  or T for time section
c  JUMP     TRUE if a missing branch of solution is suspected (ie if
c           DBMAX and DX1MAX have both been exceeded)
c  K        Loop variable
c  LAND     TRUE if land-type shooting (depends on how geometry is described)
c  LIST     TRUE if listings are to be generated
c  MARINE   TRUE if marine-type shooting (depends on how geometry is described)
c  MAXEVT   Maximum number of events / shot
c  MAXINT   Maximum number of interfaces in the model
c  MAXN     MAximum value of N
c  MAXNP1   MAXN plus 1
c  MAXNP3   MAXN plus 3
c  MAXREC   Maximum number of receivers / shot
c  MAXREF   Maximum number of reflections in any event
c  MAXSPL   Maximum number of points defining an interface
c  MAXSRC   Maximum number of shots in the job
c  MODE     Shooting mode: surface (s or S) or downhole (d or D)
c  MODEL    Name of the file containing the interface coordinates
c  MU       Move-up in number of stations from first shot (marine shooting)
c  MXSPM1   MAXSPL minus one
c  N        Number of intersection points between source and receiver
c  NCHARC   Number of characters in OUTNAM (up to first blank)
c  NDUMMY   Dummy integer (value not used)
c  NEVENT   Number of events in the shot (some may be invalid)
c  NGAP     Number of stations in the gap
c  NHEADS   Number of head wave events / shot
c  NINT     Number of interfaces in the model (not counting the upper surface)
c  NNUM     Number of integers and floats in the character variable CARD
c  NOCONV   TRUE if can't find ray or ray exits line
c  NORDER() Order of intersections in current event.
c  NPTS()   Number of points defining each interface.
c  NREC     Number of receivers for this shot
c  NREFER   Reference station number
c  NREFLS() Number of reflections that occur within each event
c  NSRC     Number of shots
c  NTR1B    Station number of firts receiver for this shot
c  NTR1F    Station number of first receiver after the gap
c  NTRABS   Station number of this receiver or trace
c  NTRMAX   Maximum receiver station number
c  NTRMIN   Minimum receiver station number
c  NTRNB    Station number of last receiver before the gap
c  NTRNF    Station number of last receiver for this shot
c  NTRREC   Number of traces/shot (must be the same for all shots
c           when generating shot data)
c  NWELL    Number of points defining the well
c  OUTFIL   Full output file name
c  OUTNAM   First part of name given to all output files
c  PARIN    Unit number of file PARAM
c  PHASE1,2 Phase due to postcritical reflections and caustics
c  PI       3.1415927
c  PIXPI    Number of pixels/inch on the screen
c  PLTGEO   If TRUE then the receiver locations will be plotted    
c  PLTMOD   TRUE if the model is to be plotted
c  PLTRAY   TRUE if rays are o be plotted
c  PLTSRC   TRUE if source locations are to be plotted
c  PLTWEL   TRUE if well is to be plotted
c  PLTYPE   PLot descriptor
c  QTPLOT   Quit plotting after first plot descriptor
c  QTPLT2   Quit plotting after second plot descriptor
c  R        Length of straight raypath 
c  RAYLST   Unit number of listing file
c  RAYOUT   Unit number of ray data file
c  RAYTRC   TRUE if ray tracing will be necessary
c  RDGEOM   TRUE if shot cards are to be read
c  RECDPT   Depth of (all) receivers below the upper surface
c  S1       Depth (arc length) down the well to the first source location
c  SHTDPT   Shot depth below upper surface
c  SHTOUT   Unit number of shot data file (this data used by CSHOT2)
c  SHTREC   TRUE to generate shot data (used by cshot2 to build time section)
c  SIGN()   Indicates the direction of the ray leaving each intersection
c           point. 1. if in the direction of the upward normal to the
c           interface. Otherwise, -1.
c  SLAYER() Integer array giving layer number of each source location
c  SPACIN   Sets the x-width of the ray tube as 1 or 2 receiver spacings
c  SPFAIL   TRUE if cannot fit spline through interfaces or well
c  STDERR   Unit number of all error messages
c  SURFAC   TRUE for surface shooting; FALSE for downhole shooting
c  TCOEFF   Transmission effects along a ray (used in headwave calculation)
c  TIME1,2  Traveltime along a ray
c  TINFO    True if traveltimes are to be calculated
c  TOTLMU   Total move-up in ft or m from first shot in marine shooting
c  TRUGEO   Station spacing 
c  V()      Interval velocity on each ray segment
c  VALID    TRUE if this is a valid event
c  VEL      Interval velocities of the layers. VEL(1) is the velocity
c           of the shallowest layer.
c  VREF()   Velocities of the layers from which the ray reflects
c  W0,W1,W2,W3  Cubic spline coefficients of the well
c  WELL     Unit number of file describing the well and downhole sources
c  WELLFL   Name of file containing well coordinates and source locations
c  WELOPN   TRUE if the well file is to be opened
c  X()      x-coordinates of ray intersections with interfaces
c  X1LAST   x(1) of last good ray
c  XDIM     x dimension of plot in inches
c  XEND     Maximum x-coordinate in model (taken from upper surface)
c  XINT(,)  x-coordinates of points defining each interface
c  XLAST    x-coordinate of last good ray found by continuation
c           method.  This ray has takeoff angle BELAST.
c  XR       x-coordinate of receiver (for plotting)
c  XREC()   x-coordinates of the receivers for this shot
c  XREF0    x-coordinate of station number zero
c  XREFER   x-coordinate of station reference station NREFER 
c  XRMAX    Maximum receiver x-coordinate
c  XRMIN    Minimum receiver x-coordinate
c  XS()     x-coordinates of sources
c  XSCRN    x-dimension of screen in inches
c  XSTART   Minimum x-coordinate in model (taken from upper surface)
c  XWELL()  Array of x-coordinates defining the well
c  YORN     Character - y or n
c  Z        z-coordinates of ray intersections with interfaces
c  ZDIM     x dimension of plot in inches
c  ZINT(,)  z-coordinates of points defining each interface 
c  ZR       z-coordinate of receiver (for plotting)
c  ZREC()   z-coordinates of the receivers for this shot
c  ZS()     z-coordinates of sources
c  ZSCRN    z-dimension of screen in inches
c  ZWELL()  Array of z-coordinates defining the well

c-----------------------------------------------------------------------


      INTEGER     PARIN,   SHTOUT,  INTERF,  RAYOUT,  RAYLST,
     :            COLSIN,  WELL,    GEOMS,   STDERR

      PARAMETER ( STDERR = 0,
     :            PARIN  = 10,
     :            INTERF = 12,
     :            SHTOUT = 13,
     :            RAYOUT = 15,
     :            RAYLST = 16,
     :            COLSIN = 17,
     :            WELL   = 18,
     :            GEOMS  = 21)


      INTEGER    MAXINT,       MAXSPL,         MXSPM1,
     :           MAXN,         MAXNP1,         MAXNP3,
     :           MAXREF,       MAXEVT,         MAXREC,     MAXSRC

      PARAMETER ( MAXINT = 20,
     :            MAXSPL = 2001,
     :            MAXN   = 40,
     :            MAXREF = 20,
     :            MAXEVT = 20,
     :            MAXREC = 1024,
     :            MAXSRC = 2000)

      PARAMETER ( MAXNP1 = MAXN + 1,
     :            MAXNP3 = MAXN + 3,
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



      REAL       X(0:MAXNP3),       Z(0:MAXNP3),
     :           VEL(0:MAXINT+1),   VREF(MAXREF),
     :           XREC(MAXREC),      ZREC(MAXREC),
     :           XWELL(0:MAXSPL),   ZWELL(0:MAXSPL),
     :           XS(MAXSRC),        ZS(MAXSRC),
     :           W0(MXSPM1),        W1(MXSPM1),
     :           W2(MXSPM1),        W3(MXSPM1)

      REAL       AMP,       AMP1,       AMP2,        R,
     :           BELAST,    BETA,       BETAI,       BETAF,
     :           BETA1,     BETA2,      BETA3,       BETNEW,
     :           CONST,     DBETA,      DBMAX,       DX1FAC,
     :           DELTAB,    DETJ,       PI,          DX1MAX,
     :           GEOINC,    PHASE1,     PHASE2,      GEOZ,
     :           SPACIN,    TIME1,      TIME2,       TCOEFF,
     :           TRUGEO,    XEND,       XLAST,       XSTART,
     :           X1LAST,    XDIM,       ZDIM,        S1,
     :           DSRC,      XREFER,     RECDPT,      XREF0,
     :           SHTDPT,    XRMIN,      XRMAX,      
     :           XR,        ZR,
     :           DX1,       DUMMY,      FSHOT,       FMOVE,
     :           TOTLMU,    DXM1XM,     DXM1X,
     :           DXXM,      AM1,        BM1



      INTEGER    IREFL(MAXEVT,0:MAXREF),  NREFLS(MAXEVT),
     :           ICOLOR(7),        SLAYER(MAXSRC)

      INTEGER    I,       K,       J,        IEVENT,   IPEN,
     :           NCHARC,  NDUMMY,  NEVENT,   NREC,     NWELL,
     :           NSRC,    NREFER,  NTRMIN,   NTRMAX,   
     :           NTR1B,   NTRNB,   NTR1F,    NTRNF,    
     :           NTRREC,  NHEADS,  IHEAD,    IDUMMY,
     :           IRECD,   NGAP,    ITRACE,   IREC,     NTRABS,
     :           IRECP,   MU,      NNUM


      CHARACTER   MODEL*20,    OUTNAM*20,   OUTFIL*20,
     :            GEOMFL*20,   EVENT*30,
     :            YORN*1,      COLORS*20,   WELLFL*20,   PLTYPE*3,
     :            JOBTYP*3,    MODE*3,      CARD*81

      CHARACTER EVTYPE(MAXEVT)*1
     

      LOGICAL     NOCONV,     CAUSTC,       
     :            FIRST,      RAYTRC,       JUMP,
     :            PLTMOD,     PLTWEL,       PLTGEO,        PLTSRC,
     :            PLTRAY,     BEGPLT,       QTPLOT,        LIST,
     :            SHTREC,     QTPLT2,       WELOPN,
     :            DOWNHL,     SURFAC,       RDGEOM,
     :            DUMML,      INSIDE,       EOF,           INGAP,
     :            VALID,      TINFO,        HEAD,          SPFAIL,
     :            LAND,       MARINE


      PARAMETER ( PI = 3.141592653589)
      PARAMETER ( DBMAX  = 10.,
     :            DX1FAC = 2.)


ccc   Begin...

c     Reading from the file PARAM1.
      OPEN(UNIT=PARIN,FILE='param1',STATUS ='old',ERR=5)
      GO TO 10
5     WRITE(STDERR,'(1X,A)') 'Can''t open file param1.'
      STOP

10    REWIND PARIN

      READ(PARIN,'(A)') MODEL
      OPEN(UNIT=INTERF,FILE=MODEL,STATUS='old',ERR=15)
      GO TO 20
15    WRITE(STDERR,'(1X,A)') 'Can''t open file :',MODEL
      STOP

20    REWIND INTERF

      READ(PARIN,*) NINT
      IF(NINT.GT.MAXINT) THEN
         WRITE(STDERR,'(1X,A)') 'MAIN : too many interfaces'
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
               WRITE(STDERR,'(1X,A,I2)')
     :        'MAIN : not enough points defining interface ',I
               STOP
            END IF
         ELSE
            J = J + 1
            IF(J.GT.MAXSPL) THEN
               WRITE(STDERR,'(1X,A,I2)')
     :        'MAIN : too many enough points defining interface ',I
               WRITE(STDERR,'(1X,A,I3)')
     :        'Maximum points/interface = ',MAXSPL
               STOP
            END IF
            GO TO 25
         END IF

30       CONTINUE

c     edges of the model (taken as edges of upper surface)
      XSTART = XINT(0,1)
      XEND   = XINT(0,NPTS(0))     

c-----------------------------------------------------------
      READ(PARIN,'(A)') COLORS

      OPEN(UNIT=COLSIN,FILE=COLORS,STATUS='old',ERR=32)
      GO TO 35
32    WRITE(STDERR,'(1X,A)') 'MAIN: Can''t open file :',COLORS
      STOP

35    REWIND COLSIN
      DO 40 I = 1,  6
         READ(COLSIN,*,END=48) ICOLOR(I)
40       CONTINUE
      GO TO 50

48    WRITE(STDERR,'(1X,A,1X,A)') 
     : 'MAIN: not enough colors defined in file',COLORS
      STOP

50    CONTINUE

c------------------------------------------------------------
c     Calculating the cubic spline coefficients of each interface.
      CALL CUSPLN(NINT,XINT,ZINT,NPTS,A0,A1,A2,A3,SPFAIL,CV)
      IF(SPFAIL) THEN
         WRITE(STDERR,'(1X,A)') 
     :   'MAIN: Failed to fit spline to interfaces.'
         STOP
      END IF


c     Read plot descriptor 
      READ(PARIN,'(A)') PLTYPE
      BEGPLT = .FALSE.
      CALL SETVAR(PLTYPE,PLTMOD,PLTWEL,QTPLOT,BEGPLT,
     :'m','M','w','W','q','Q')

c     Read filename containing well description
      READ(PARIN,'(A)',END=100) WELLFL

      IF(PLTWEL) WELOPN = .TRUE.
      IF(QTPLOT) GO TO 100

c     Read shooting mode  
      READ(PARIN,'(A)',END=100) MODE
      CALL SETVAR(MODE,DOWNHL,SURFAC,DUMML,DUMML,
     :'d','D','s','S',' ',' ')
      IF(DOWNHL) THEN
         WELOPN = .TRUE.
      ELSE IF(SURFAC) THEN
      ELSE
         WRITE(STDERR,'(1X,A)')
     :   'MAIN: Invalid shooting mode'
         STOP
      END IF


c     Read receiver geometries file
      READ(PARIN,'(A)',END=100) GEOMFL

c     Read second plot descriptor 
      READ(PARIN,'(A)',END=100) PLTYPE
      CALL SETVAR(PLTYPE,PLTSRC,PLTGEO,QTPLT2,BEGPLT,
     :'s','S','g','G','q','Q')
      IF(PLTGEO) RDGEOM = .TRUE.
      IF(QTPLT2) GO TO 100

c     Read job descriptor 
      READ(PARIN,'(A)',END=100) JOBTYP
      CALL SETVAR(JOBTYP,LIST,PLTRAY,SHTREC,DUMML,
     :'l','L','r','R','t','T')
      IF(LIST)   RDGEOM = .TRUE.
      IF(SHTREC) RDGEOM = .TRUE.
      IF(PLTRAY) RDGEOM = .TRUE.
      IF(PLTRAY) BEGPLT = .TRUE.
      IF(LIST)   TINFO = .TRUE.
      IF(SHTREC) TINFO = .TRUE.


100   CONTINUE


      IF(WELOPN) THEN
        
         OPEN(UNIT=WELL,FILE=WELLFL,STATUS='old',ERR=150)
         GO TO 155
150      WRITE(STDERR,'(1X,A,A)') 'MAIN: Can''t open well file :',WELLFL
         STOP

155      REWIND WELL
c        Read the x,z coordinates of the well 
c        track the number of points defining the well
         NWELL = 0
         ZWELL(0) = 0.
         READ(WELL,*,END=180) XWELL(0)
170      IF(ZWELL(NWELL).GT.-9999.) THEN
            NWELL = NWELL + 1
            IF(NWELL.GT.MAXSPL) THEN
               WRITE(STDERR,'(1X,A,I2)') 
     :         'MAIN: Too many points defining the well - max is:',
     :          MAXSPL
               STOP
            END IF
            READ(WELL,*,END=180) XWELL(NWELL),ZWELL(NWELL)
            GO TO 170
         END IF

180      IF(NWELL.LT.2) THEN
            WRITE(STDERR,'(1X,A,A)') 
     :      'MAIN: Not enough points defining the well in file :',
     :       WELLFL
            STOP
         END IF

c        calculate z-coordinate of well at surface
         IF(XWELL(0).LE.XSTART.OR.XWELL(0).GT.XEND) THEN
c          well outside model
           WRITE(STDERR,'(1X,A)') 'MAIN: well outside model'
           STOP
         END IF
         J = 1
185      IF(XWELL(0).GT.XINT(0,J)) THEN
            J = J + 1
            GO TO 185
         END IF
         J = J - 1
c **************************************************************************
c  The following calculation of the position of the interface was changed by
c  E.Jenner and T.Salinas, CWP July 1996, to increase the accuracy.
c  See comments in 'splines.f', subroutine CUSPLN for details.
c **************************************************************************
c        zwell(0) = a0(0,j) + a1(0,j) * xwell(0)
c    :                      + a2(0,j) * xwell(0)**2
c    :                      + a3(0,j) * xwell(0)**3

         IF(NPTS(0).EQ.2) THEN
            ZWELL(0) = A0(0,J) + A1(0,J) * XWELL(0)
     :                         + A2(0,J) * XWELL(0)**2
     :                         + A3(0,J) * XWELL(0)**3
         ELSE

            DXM1XM = XINT(0,J+1) - XINT(0,J)
            BM1 = ZINT(0,J+1) / DXM1XM - CV(0,J+1) * DXM1XM / 6
            AM1 = ZINT(0,J) / DXM1XM - CV(0,J) * DXM1XM / 6
            DXM1X = XINT(0,J+1) - XWELL(0)
            DXXM = XWELL(0) - XINT(0,J)

            ZWELL(0)=(CV(0,J)*DXM1X**3+CV(0,J+1)*DXXM**3)/(6*DXM1XM)
     $               + AM1 * DXM1X + BM1 * DXXM

         ENDIF

c        fit cubic spline to well
         CALL CUSPLW(ZWELL,XWELL,NWELL,W0,W1,W2,W3,SPFAIL)
         IF(SPFAIL) THEN
            WRITE(STDERR,'(1X,A)') 
     :      'MAIN: Failed to fit spline to well.'
            STOP
         END IF

         IF(DOWNHL) THEN
c           downhole mode
c           source locations specified in well file
            READ(WELL,*) S1
            READ(WELL,*) NSRC, DSRC
            IF(NSRC.GT.MAXSRC) THEN
               WRITE(STDERR,'(1X,A,I3)') 
     :         'MAIN: too many sources in the well - max is: ',
     :         MAXSRC
               STOP
            END IF
           
c           Calculate x-z coordinates of sources in well
            CALL XZSRC(S1,NSRC,DSRC,ZWELL,NWELL,W0,W1,W2,W3,
     :                 XS,ZS,STDERR)
c           Find which layer each source is in
            DO 190 J = 1,  NSRC
               CALL LAYER(XS(J),ZS(J),SLAYER(J),INSIDE)
               IF(.NOT.INSIDE) THEN
                  WRITE(STDERR,'(1X,A,I4)')
     :            'MAIN: Can''t find source layer - source #',J
                   STOP
               END IF
190            CONTINUE
            
         END IF

      END IF
        

      IF(RDGEOM) THEN
c        read shooting gemetry

         OPEN(UNIT=GEOMS,FILE=GEOMFL,STATUS='old',ERR=200)
         REWIND GEOMS
         GO TO 205
200      WRITE(STDERR,'(1X,A)')'MAIN: Can''t open geometry file'
         STOP
205      CONTINUE

c        read location of a reference group
         READ(GEOMS,*) NREFER, XREFER
c        read receiver spacing and receiver depth
         READ(GEOMS,*) TRUGEO,RECDPT
c        Next is the maximum change in x(1) from one ray
c        to the next - if a branch of solutions is missed
c        then the change in x(1) should be large and we can
c        detect the jump
         DX1MAX = DX1FAC * TRUGEO

         XREF0 = XREFER - TRUGEO * NREFER

c        count number of cards in file
c        if surface shooting then calculate source z-coord. and source layer
         IF(SURFAC) THEN
            READ(GEOMS,*,END=215) NTR1B,NTRNB,NTR1F,NTRNF,FSHOT,SHTDPT
            NSRC = 1
            XS(1) = XREF0 + FSHOT * TRUGEO
            CALL ELEVS(XS(1),1,SHTDPT,ZS(1),INSIDE)
            IF(.NOT.INSIDE) THEN
               WRITE(STDERR,'(1X,A,F6.2)')
     :         'MAIN: Shot located outside model - at station ',
     :          FSHOT
                STOP
            END IF
            CALL LAYER(XS(1),ZS(1),SLAYER(1),INSIDE)
            IF(.NOT.INSIDE) THEN
c              this only happens if source is deep in model
c              and interfaces are not fully defined - can't tell
c              which layer source is in
               WRITE(STDERR,'(1X,A,F6.2)')
     :         'MAIN: Can''t find source layer - source at station ',
     :          FSHOT
                STOP
            END IF
         ELSE
c           Downhole shooting mode - read receiver layout only (sources
c           are decribed in well file)
            READ(GEOMS,*,END=215) NTR1B,NTRNB,NTR1F,NTRNF
         END IF

c        track min and max station numbers
         NTRMIN = NTR1B
         NTRMAX = NTRNF

         GO TO 220

215      WRITE(STDERR,'(1X,A,1X,A)')'No shot cards defined in file',
     :   GEOMFL
         STOP

c        number of traces per record
220      NTRREC = NTRNB - NTR1B  +  NTRNF - NTR1F  +  2
         IF(NTRREC.GT.MAXREC) THEN
            WRITE(STDERR,'(1X,A,I4,A)') 
     :      'MAIN: too many receivers/shot - max is: ',
     :      MAXREC,' - check first geometry card.'
            STOP
         END IF


c        For surface shooting there are 3 possibilities at this point:
c        (1) Only 1 shot (ie, 1 card) is defined in the file
c        (2) There are many shots, each described by a card
c        (3) The number of shots and shot moveup is defined by the 
c            next (which is the last) record of the file.
c        Cases (1) and (2) are considered land shooting.
c        We can think of Case (3) as marine shooting (or a quick way
c        to specify a land survey where the recording geometry is fixed).
c        The next piece of code is a patch to allow for Case 3.

c        first assume land shooting
c        check to see what comes next in geometry file 
         LAND = .TRUE.
         MARINE = .FALSE.
         IF(SURFAC) THEN
c           read in next record as a character variable
            READ(GEOMS,'(A)',END=235) CARD
c           find out how many numbers have been specified
            CALL CHKCRD(CARD,NNUM)
c           if less than 2 numbers are in the record then treat as eof
            IF(NNUM.LT.2) GO TO 235
            BACKSPACE GEOMS
            IF(NNUM.GE.6) THEN
c              assume land shooting
            ELSE
c              assume marine shooting
c              try to read #shots and moveup
               READ(GEOMS,*,ERR=235,END=235)NSRC,FMOVE
c              successfull read, this is marine shooting
               MARINE = .TRUE.
               LAND = .FALSE.
            END IF
         END IF

224      CONTINUE


         IF(SURFAC.AND.MARINE) THEN
c           marine shooting
c           set coordinates of sources, check that receivers stay in model
            IF(NSRC.GT.MAXSRC) THEN
               WRITE(STDERR,'(1X,A,I3)') 
     :         'MAIN: too many sources - max is: ',
     :         MAXSRC
               STOP
            END IF
            DO 226 I = 2,  NSRC 
               XS(I) = XS(I-1) + FMOVE
               CALL ELEVS(XS(I),1,SHTDPT,ZS(I),INSIDE)
               IF(.NOT.INSIDE) THEN
                  WRITE(STDERR,'(1X,A,I4)')
     :            'MAIN: Shot located outside model - source #',I
                   STOP
               END IF
               CALL LAYER(XS(I),ZS(I),SLAYER(I),INSIDE)
               IF(.NOT.INSIDE) THEN
                  WRITE(STDERR,'(1X,A,I4)')
     :            'MAIN: Can''t find source layer - source #',I
                   STOP
               END IF
226            CONTINUE
         END IF


         IF(SURFAC.AND.LAND) THEN
230         READ(GEOMS,*,END=231)NTR1B,NTRNB,NTR1F,NTRNF,FSHOT,SHTDPT
            NSRC = NSRC + 1
            IF(NSRC.GT.MAXSRC) THEN
               WRITE(STDERR,'(1X,A,I3)') 
     :         'MAIN: too many sources - max is: ',
     :         MAXSRC
               STOP
            END IF
            NTRREC = NTRNB - NTR1B  +  NTRNF - NTR1F  +  2
            IF(NTRREC.GT.MAXREC) THEN
               WRITE(STDERR,'(1X,A,I4,A,I4)') 
     :         'MAIN: too many receivers/shot - max is: ',
     :         MAXREC,' - check shot ',NSRC
               STOP
            END IF
            XS(NSRC) = XREF0 + FSHOT * TRUGEO
            CALL ELEVS(XS(NSRC),1,SHTDPT,ZS(NSRC),INSIDE)
            IF(.NOT.INSIDE) THEN
               WRITE(STDERR,'(1X,A,F6.2)')
     :         'MAIN: Shot located outside model - at station ',
     :          FSHOT
                STOP
            END IF
            CALL LAYER(XS(NSRC),ZS(NSRC),SLAYER(NSRC),INSIDE)
            IF(.NOT.INSIDE) THEN
               WRITE(STDERR,'(1X,A,F6.2)')
     :         'MAIN: Can''t find source layer - source at station ',
     :          FSHOT
                STOP
            END IF

            IF(NTR1B.LT.NTRMIN) NTRMIN = NTR1B
            IF(NTRNF.GT.NTRMAX) NTRMAX = NTRNF
            GO TO 230

231         CONTINUE    
         END IF


         IF(DOWNHL) THEN
232         READ(GEOMS,*,END=233) NTR1B,NTRNB,NTR1F,NTRNF
            IF(NTR1B.LT.NTRMIN) NTRMIN = NTR1B
            IF(NTRNF.GT.NTRMAX) NTRMAX = NTRNF
            GO TO 232
233         CONTINUE
         END IF


235      REWIND GEOMS

c        check that all receivers lie inside limits of model
         IF(MARINE) THEN
            IF(FMOVE.GE.0.)THEN
               XRMIN = XREF0 + NTRMIN * TRUGEO
               XRMAX = XREF0 + NTRMAX * TRUGEO + (NSRC-1) * FMOVE
            ELSE
               XRMIN = XREF0 + NTRMIN * TRUGEO - (NSRC-1) * FMOVE
               XRMAX = XREF0 + NTRMAX * TRUGEO 
            END IF
         ELSE
c           land or downhole
            XRMIN = XREF0 + NTRMIN * TRUGEO
            XRMAX = XREF0 + NTRMAX * TRUGEO
         END IF

         IF(XRMIN.LE.XSTART.OR.XRMAX.GE.XEND) THEN
            WRITE(STDERR,'(1X,A)')
     :     'MAIN: Receiver stations outside limits of model.'
            STOP
         END IF


      END IF

     
      IF(BEGPLT) THEN

c        Plotting
c        initialize plot
         CALL PLOTI

         IF(PLTMOD) THEN
c           plot the model
            IPEN = ICOLOR(6)
            CALL PLOTIN(IPEN)
         END IF

         IF(PLTWEL) THEN
c           plot the well
            IPEN = ICOLOR(3)
            CALL PLOTWL(ZWELL,NWELL,W0,W1,W2,W3,IPEN)
         END IF

         IF(QTPLOT) THEN
c           quit after plotting the model and/or the well
            CALL PLOTE
            STOP
         END IF

         IF(PLTGEO) THEN
c           plot the receiver locations
            IPEN = ICOLOR(1)
            IF(LAND) THEN
               DO 250 K = NTRMIN,  NTRMAX
c                 first calculate x and z coords. of receiver stations
                  XR = XREF0 + TRUGEO * K
                  CALL ELEVS(XR,1,RECDPT,ZR,INSIDE)
                  CALL PLTSYM(XR,ZR,1,TRUGEO,IPEN)
250               CONTINUE
            ELSE
               DO 260 I = 1,  NSRC
                  XR = XREF0 + NTRMIN * TRUGEO + (I-1) * FMOVE
                  DO 255 K = NTRMIN,  NTRMAX
c                    first calculate x and z coords. of receiver stations
                     CALL ELEVS(XR,1,RECDPT,ZR,INSIDE)
                     CALL PLTSYM(XR,ZR,1,TRUGEO,IPEN)
                     XR = XR + TRUGEO
255                  CONTINUE
260               CONTINUE
            END IF

         END IF

        IF(PLTSRC) THEN
c           plot the source locations
            IPEN = ICOLOR(2)
            CALL PLTSYM(XS(1),ZS(1),NSRC,TRUGEO,IPEN)
         END IF

         IF(QTPLT2) THEN
c           quit after above plot options
            CALL PLOTE
            STOP
         END IF

         IF(.NOT.PLTRAY) THEN
c           no more plotting
            CALL PLOTE
         END IF
      
      END IF


      IF(PLTRAY.OR.LIST) THEN
c        proceed
      ELSE IF(SHTREC) THEN
c        proceed
      ELSE
c        nothing else to do
         STOP
      END IF


c     Read remainder of param

c     Read the name to be given the output files, if there are to be any.
      READ(PARIN,'(A)',END=300) OUTNAM
      GO TO 305
300   WRITE(STDERR,'(1X,A)')
     :'PARAM1 not filled out - stopped at output file name.'
      IF(BEGPLT) CALL PLOTE
      STOP

305   IF(LIST.OR.SHTREC) THEN
c        Count the number of characters in the name, up to first blank.
         J = 1
310      IF(OUTNAM(J:J).EQ.' ') THEN
         ELSE
            J = J + 1
            GO TO 310
         END IF
         NCHARC = J - 1
      END IF

      IF(LIST) THEN
         OUTFIL = OUTNAM(1:NCHARC)//'data'
         OPEN(UNIT=RAYOUT,FILE=OUTFIL,ERR=350)
         GO TO 355
350      WRITE(STDERR,'(1X,A)')'Can''t open ray data file.'
         IF(BEGPLT) CALL PLOTE
         STOP
355      REWIND RAYOUT

         OUTFIL = OUTNAM(1:NCHARC)//'listing'
         OPEN(UNIT=RAYLST,FILE=OUTFIL,ERR=360)
         GO TO 365
360      WRITE(STDERR,'(1X,A)')'Can''t open ray listing file.'
         IF(BEGPLT) CALL PLOTE
         STOP
365      REWIND RAYLST
      END IF


c     Want to generate a shot record ?
      IF(SHTREC) THEN
         OUTFIL = OUTNAM(1:NCHARC)//'shot'
c        OPEN(UNIT=SHTOUT,FILE=OUTFIL,
         OPEN(UNIT=SHTOUT,FORM='UNFORMATTED',FILE=OUTFIL,
     :   ERR=400)
         GO TO 405
400      WRITE(STDERR,'(1X,A)')'Error creating output trace file.'
         IF(BEGPLT) CALL PLOTE
         STOP
405      REWIND SHTOUT
      END IF


c     continue reading from PARAM file
      READ(PARIN,*,END=420) BETAI,BETAF
      GO TO 425
420   WRITE(STDERR,'(1X,A)')
     :'PARAM1 not filled out - stopped at range of takeoff angles.'
      IF(BEGPLT) CALL PLOTE
      STOP

425   IF(BETAI.GT.BETAF) THEN
         WRITE(STDERR,'(1X,A)') 'MAIN : must have betaf > betai'
         STOP
      END IF

      READ(PARIN,*,END=430) DELTAB
      GO TO 435
430   WRITE(STDERR,'(1X,A)')
     :'PARAM1 not filled out - stopped at change in takeoff angle.'
      IF(BEGPLT) CALL PLOTE
      STOP
435   IF(DELTAB.LE.0.) THEN
         WRITE(STDERR,'(1X,A)') 'MAIN : deltab must be positive'
         STOP
      END IF


c     read layer velocities
      READ(PARIN,*,END=450) (VEL(I),I=1,NINT+1)
      GO TO 455
450   WRITE(STDERR,'(1X,A)')
     :'Not enough velocities - need one more than #interfaces.'
      IF(BEGPLT) CALL PLOTE
      STOP
455   CONTINUE

c     now read in events
c     initializing
      NEVENT = 0

c     want direct wave?
      READ(PARIN,'(A)',END=465) YORN 
      GO TO 470
465   WRITE(STDERR,'(1X,A)')
     :'MAIN: PARAM1 not filled out - no events specified.'
      IF(BEGPLT) CALL PLOTE
      STOP
470   CONTINUE
      IF(YORN.EQ.'y') THEN
         NEVENT = 1
         EVTYPE(NEVENT) = 'd'
      END IF

c     want headwaves?
      READ(PARIN,'(A)',END=475) EVENT
      GO TO 480
475   WRITE(STDERR,'(1X,A)')
     :'MAIN: PARAM1 not filled out - need head wave and primary specs.'
      IF(BEGPLT) CALL PLOTE
      STOP
480   CONTINUE
      NEVENT = NEVENT + 1
      CALL SETREF(EVENT,IREFL,NREFLS(NEVENT),NEVENT,VALID,
     :            MAXEVT,MAXREF,NINT)
c     setref was originally designed to deal with extra event
c     specification - this patch is for the head wave specification
      IF(VALID) THEN
c        setref places interface numbers contained in "event" 
c        into irefl(nevent,here)
c        the number of interfaces specified is given by nrefls(nevent)
c        for headwaves each interface# specifies a refractor

c        #headwaves
         NHEADS = NREFLS(NEVENT)
c        now consider each head wave as separate event
         IHEAD = 1
         DO 500 K = NEVENT,  NEVENT + NHEADS - 1
            NREFLS(K) = 1
            IREFL(K,1) = IREFL(NEVENT,IHEAD)
            EVTYPE(K) = 'h'
            IHEAD = IHEAD + 1
500         CONTINUE

c        this is the true number of events
         NEVENT = NEVENT + NHEADS - 1
      ELSE
c        error in specifying refractors - noninteger characters used or more
c        than 3 digits for an interface (max interface is 99 in sub setref -
c        might be less, depending on dimensions of program)
         NEVENT = NEVENT - 1
         NHEADS = 0
         WRITE(STDERR,'(/,2X,A)') 
     :   'Invalid headwave description.'
      END IF
         

c     want all primaries?
      READ(PARIN,'(A)',END=505) YORN
      GO TO 510
505   WRITE(STDERR,'(1X,A)')
     :'MAIN: PARAM1 not filled out - specify y or n for primaries.'
      IF(BEGPLT) CALL PLOTE
      STOP
510   CONTINUE
      IF(YORN.EQ.'y') THEN
c        calculate primary reflections
         DO 520 K = 1, NINT
            NEVENT = NEVENT + 1
            NREFLS(NEVENT) = 1
            IREFL(NEVENT,1) = K
            EVTYPE(NEVENT) = 'r'
520         CONTINUE
      END IF


c     now come the extra events (specified by the reflecting interfaces)
c     there are as many extra events as there are records left in param
550   READ(PARIN,'(A)',END=600) EVENT
      NEVENT = NEVENT + 1
      CALL SETREF(EVENT,IREFL,NREFLS(NEVENT),NEVENT,VALID,
     :            MAXEVT,MAXREF,NINT)
      IF(NREFLS(NEVENT).EQ.0) THEN
c        not an event - probably a blank line
         NEVENT = NEVENT - 1
      ELSE IF(.NOT.VALID) THEN
c        not a valid event - noninteger characters used or more
c        than 3 digits for an interface (max interface is 99)
         NEVENT = NEVENT - 1
         WRITE(STDERR,'(/,2X,A)') 
     :   'Invalid extra event specification.'
      ELSE
         EVTYPE(NEVENT) = 'r'
      END IF
      GO TO 550
600   CONTINUE

      IF(LIST) THEN

         WRITE(RAYLST,'(/,20X,A/)')
     :   'CSHOT1 Listing File'
         WRITE(RAYLST,'(/2X,A)') 'Velocities:'
         DO 605 I = 1,  NINT + 1
            WRITE(RAYLST,'(2X,A,I2,1X,F8.1)')
     :      'layer ',I,VEL(I)
605         CONTINUE

         WRITE(RAYLST,'(/,2X,A,I4)') 'Number of shots = ',NSRC
         WRITE(RAYLST,'(2X,A,I4)') 'Number of events per shot = ',
     :   NEVENT

         IF(DOWNHL) THEN
            WRITE(RAYLST,'(1X,A,F8.2,A,F8.2/)')
     :      'Top of well is at coordinates ',XWELL(0),',',ZWELL(0)
            WRITE(RAYLST,'(3X,A,11X,A,8X,A)')'shot',
     :      'x-z coordinates',
     :      'layer number'
            DO 610 K = 1,  NSRC
               WRITE(RAYLST,'(2X,I3,10X,F8.2,5X,F8.2,9X,I2)')
     :         K,XS(K),ZS(K),SLAYER(K)
610            CONTINUE
         END IF

      END IF


      IF(SHTREC) THEN
c        Need this constant for amplitude calculations.
c        CONST = ( 1. / ( 4. * PI ) ) * SQRT( VEL(1) / TRUGEO )
         CONST = 1. / ( 4. * PI * SQRT(TRUGEO) )
c        VEL(0) identifies reflections from the surface of the
c        earth ( reflection coefficient is then set to -1 ).
         VEL(0) = 0.
      END IF

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccc   Main loop over shots   ccccccccccccccccccccccccccccccc
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c     read shot/receiver geometry cards
c     skip over first two records
      READ(GEOMS,*) IDUMMY
      READ(GEOMS,*) DUMMY
c     set pen color for rays
      IF(PLTRAY) THEN
         IPEN = ICOLOR(5)
      END IF

      EOF = .FALSE.
      IF(MARINE) THEN
         READ(GEOMS,*) NTR1B,NTRNB,NTR1F,NTRNF
         XREF0 = XREF0 - FMOVE
         TOTLMU = - FMOVE
      ELSE
         MU = 0
      END IF

      DO 2000 IRECD = 1,  NSRC

      IF(LAND) THEN
c        Check for eof is necessary here because when downhole shooting there
c        may be more shots than geometry cards.  If so, keep the geometry
c        of the last card until all shots are done.
         IF(.NOT.EOF) READ(GEOMS,*,END=650) NTR1B,NTRNB,NTR1F,NTRNF
         GO TO 655
650      EOF = .TRUE.
655      CONTINUE
      ELSE
         XREF0 = XREF0 + FMOVE
         TOTLMU = TOTLMU + FMOVE
         MU = ( TOTLMU + TRUGEO/10. ) / TRUGEO
      END IF

c     ray begins at source location
      X(0) = XS(IRECD)
      Z(0) = ZS(IRECD)

c     number of receivers
      NREC = NTRNF - NTR1B + 1
c     number of receivers in the gap
      NGAP = NTR1F - NTRNB  - 1

c     set x-coordinates of receivers for this shot
      XREC(1) = XREF0 + TRUGEO * NTR1B
      DO 670 I = 1,  NREC - 1
         XREC(I+1) = XREC(I) + TRUGEO
670      CONTINUE

c     calculate z-coords. of receivers
      CALL ELEVS(XREC(1),NREC,RECDPT,ZREC(1),INSIDE)
      IF(INSIDE) THEN
      ELSE
         WRITE(STDERR,'(1X,A)')
     :   'MAIN: warning - receivers outside model.'
      END IF


      IF(LIST) THEN
         WRITE(RAYLST,'(1(/),2X,A,I4)') 'x and z coordinates of shot',
     :   IRECD
         WRITE(RAYLST,5005) X(0),Z(0)
         WRITE(RAYLST,'(/,2X,A,I4)') 'Number of receivers = ',NREC
         WRITE(RAYLST,'(2X,A)') 'x and z coordinates of receivers :'
         DO 680 I = 1,  NREC
            IF(I.GT.NTRNB.AND.I.LT.NTR1F) THEN
c              receiver in the gap
            ELSE
               WRITE(RAYLST,5005) XREC(I),ZREC(I)
            END IF
680         CONTINUE
      END IF


ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
ccccccccccccccc   Do for each event   ccccccccccccccccccccccccccccccc
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      DO 1000 IEVENT = 1,  NEVENT


c     Initializing
      BETA = BETAI
      NOCONV = .TRUE.

      IF(LIST) THEN
         WRITE(RAYLST,'(2(/),2X,A,I4,6X,A,I4)')
     :   'Shot ',IRECD,'event ',IEVENT
      END IF


c     An event if of type d (direct wave), h (headwave), or
c     r (reflected event).

      IF(EVTYPE(IEVENT).EQ.'d') THEN
c        DIRECT WAVE

         HEAD = .FALSE.
         IF(LIST) WRITE(RAYLST,'(2X,A/)')'This is a direct wave.'
         IF(SLAYER(IRECD).EQ.1) THEN

c           source in same layer as receivers (layer 1)
c           use free space green's function and straight rays
            ITRACE = 1
            X(0) = XS(IRECD)
            Z(0) = ZS(IRECD)
            DO 700 IREC = 1,  NREC
               X(1) = XREC(IREC)
               Z(1) = ZREC(IREC)
               CALL GAP(IREC,NTR1B,NTRNB,NTR1F,INGAP,NTRABS)
               IF(.NOT.INGAP) THEN
                  IF(TINFO) THEN
c                    length of raypath
                     R = SQRT( (X(1)-X(0))**2 + (Z(1)-Z(0))**2 )
c                    put wavelet on trace
                     TIME1 = R / VEL(1)
                     IF(SHTREC) THEN
                        IF(R.EQ.0.) THEN
c                          amplitude infinite - set it to 0!
                           AMP = 0.
                        ELSE
                           AMP = 1. / (4. * PI * R)
                        END IF
                        PHASE1 = 0.
                        CAUSTC = .FALSE.
                        WRITE(SHTOUT)IRECD,ITRACE,NTRABS+MU,TIME1,
     :                  X(1)-X(0),IEVENT,ZS(IRECD),
     :                  AMP,PHASE1,CAUSTC,HEAD
                        ITRACE = ITRACE + 1
                     END IF
                     IF(LIST) THEN
                        WRITE(RAYOUT,*)2,TIME1
                        CALL XZOUT(X,Z,1,RAYOUT)
                        CALL XZOUT(X,Z,1,RAYLST)
                        WRITE(RAYLST,'(3X,A,F10.6/)') 
     :                  't = ',TIME1
                     END IF
                  END IF
                  IF(PLTRAY) CALL RAYPLT(X,Z,0,IPEN)
               END IF

700            CONTINUE

c           done with this event
            RAYTRC = .FALSE.

         ELSE

c           source is deeper than layer 1
c           direct ray must go up
            SIGN(0) = 1.
c           set number of intersections
            N = SLAYER(IRECD) - 1
c           set order, velocities, etc.
            DO 750 I = 1,  N
               K = SLAYER(IRECD) - I
               NORDER(I) = K
               SIGN(I) = 1.
               V(I) = VEL(K+1)
750            CONTINUE
            V(N+1) = VEL(1)
c           set this to an invalid reflector so that sub RAYDAT will
c           not compute a reflection coefficient
            IREFL(IEVENT,1) = -1
c           now ready to enter ray tracing routines
            RAYTRC = .TRUE.

         END IF


      ELSE IF(EVTYPE(IEVENT).EQ.'h') THEN
c        HEADWAVE EVENT

         HEAD = .TRUE.
         IF(LIST) WRITE(RAYLST,'(2X,A/)')'This is a head wave event.'
         CALL HDWAVE(RAYLST,SHTOUT,STDERR,
     :            LIST,SHTREC,TINFO,PLTRAY,X,Z,XS(IRECD),ZS(IRECD),
     :            XSTART,XEND,XREC,ZREC,NREC,RECDPT,TRUGEO,
     :            SLAYER(IRECD),VEL,VREF,BETAI,BETAF,DELTAB,
     :            IREFL,IEVENT,IRECD,PI,
     :            NTR1B,NTRNB,NTR1F,NGAP,HEAD,MAXEVT,MAXREF,MU,IPEN)

c        do not need any more ray tracing for this event
         RAYTRC = .FALSE.



      ELSE IF(EVTYPE(IEVENT).EQ.'r') THEN
c        REFLECTED EVENT

         HEAD = .FALSE.
         IF(LIST) WRITE(RAYLST,'(2X,A/)')'This is a reflection event.'
         CALL ORDER(IREFL,NREFLS(IEVENT),SLAYER(IRECD),IEVENT,
     :   VEL,NORDER,V,SIGN,VREF,N,VALID,RECDPT,LIST,RAYLST,STDERR,
     :   MAXEVT,MAXREF)
         IF(VALID) THEN
c           enter ray tracing routines
            RAYTRC = .TRUE.
         ELSE
            RAYTRC = .FALSE.
         END IF
        

      END IF


      IF(.NOT.RAYTRC) GO TO 999 

c     Begin Ray Tracing 

c     Begin search for the first ray.
c     Shoot rays until one emerges within the line of receivers.
c     Continue that ray onto the first receiver location.

800   IF(NOCONV) THEN

         CALL SHOOT(X,Z,NOCONV,BETA,PI,TRUGEO,
     :   XREC(1),XREC(NREC),RECDPT,.FALSE.,0,0.,0.,0.)

         IF(NOCONV) THEN
c           shooting procedure failed or ray exited the model
c           increment the takeoff angle and try again
            BETA = BETA + DELTAB
            IF(BETA.GT.BETAF) THEN
               WRITE(STDERR,'(1X,A)')
     :        'Reached max takeoff angle without finding a ray:'
               WRITE(STDERR,'(1X,A,1X,I4,4X,A,1X,I2)')
     :        'Shot number',IRECD,'Event number',IEVENT
c              try next event
               GO TO 999 
            END IF
         ELSE
c           find sign of jacobian
            CALL DETJAC(X,Z,N,DETJ)
            IF(DETJ.LT.0.) THEN
c              ray has gone through a caustic
c              assume this branch starts at the end of the line
               IREC = NREC
               GEOINC = XREC(IREC) - X(N+1)
               GEOZ = ZREC(IREC) - Z(N+1)
            ELSE
c              go to beginning of line
               IREC = 1
               GEOINC = XREC(IREC) - X(N+1)
               GEOZ = ZREC(IREC) - Z(N+1)
            END IF
            CALL RECCON(X,Z,N,GEOINC,GEOZ,PI,
     :      BETNEW,NOCONV,SIGN(0))
            IF(NOCONV) THEN
c              continuation didn't make it all the way
c              proceed to nearest receiver
               IF(DETJ.LT.0.) THEN
c                 go to the left
                  IREC = ( X(N+1) - XREC(1) ) / TRUGEO + 1
                  GEOINC = XREC(IREC) - X(N+1)
                  GEOZ = ZREC(IREC) - Z(N+1)
               ELSE
c                 go to the right
                  IREC = ( X(N+1) - XREC(1) ) / TRUGEO + 2
                  GEOINC = XREC(IREC) - X(N+1)
                  GEOZ = ZREC(IREC) - Z(N+1)
               END IF
               CALL RECCON(X,Z,N,GEOINC,GEOZ,PI,
     :         BETNEW,NOCONV,SIGN(0))
            END IF
            IF(NOCONV) THEN
c              will have to shoot another ray
               BETA = BETA + DELTAB
            ELSE
               IF(DETJ.LT.0.) THEN
c                 future rays will emerge at decreasing x coordinates
c                 prepare to continue to the left
c                 set pen color for caustic ray
                  GEOINC = - TRUGEO
                  CAUSTC = .TRUE.
                  IPEN = ICOLOR(4)
               ELSE
c                 prepare to continue to the right
c                 set pen for ordinary ray
                  GEOINC = TRUGEO
                  CAUSTC = .FALSE.
                  IPEN = ICOLOR(5)
               END IF
            END IF
         END IF

         GO TO 800

      END IF

ccccccccccccccccccccc   Found the first ray   ccccccccccccccccccc

      BETA = BETNEW
      BELAST = BETA
      X1LAST = X(1)

      IF(PLTRAY) THEN
c        plot first ray
         CALL RAYPLT(X,Z,N,IPEN)
      END IF

      IF(TINFO) THEN
c        calculate traveltime for this ray
         CALL TTIME(N,D,V,TIME1)
      END IF

      IF(SHTREC) THEN
c        get amplitude data for first ray
         CALL RAYDAT(X(N+1),VREF,IREFL,IEVENT,
     :   AMP1,PHASE1,TCOEFF,MAXEVT,MAXREF)
c        identify this ray as the first in a branch
         FIRST = .TRUE.
         BETA2 = BETA
      END IF

      IF(LIST) THEN
c        output data for first ray
         WRITE(RAYOUT,*)N+2,TIME1
         CALL XZOUT(X,Z,N+1,RAYOUT)
         CALL XZOUT(X,Z,N+1,RAYLST)
         WRITE(RAYLST,'(3X,A,F10.6/)') 't = ',TIME1
      END IF


c     Entering main ray tracing loop.  We use continuation
c     methods until they break down or we reach the end
c     of the line.  In these situations we switch to a
c     shooting procedure, either to get away from the trouble
c     spot for the continuation methods or to search for more
c     ray solutions occurring at larger takeoff angles.


900   IF(BETA.LE.BETAF) THEN
c        while takeoff angle is within range

         IF(NOCONV) THEN
c           enter shooting scheme

            CALL SHOOT(X,Z,NOCONV,BETA,PI,TRUGEO,
     :      XREC(1),XREC(NREC),RECDPT,.FALSE.,0,0.,0.,0.)

            IF(NOCONV) THEN
c              shoot again
               BETA = BETA + DELTAB
            ELSE
c              find sign of jacobian
               CALL DETJAC(X,Z,N,DETJ)
               IF(DETJ.LT.0.) THEN
c                 caustic rays

                  IF(X(N+1).GE.XLAST) THEN
c                    an unusual situation
c                    might have lost a branch
c                    try to continue to nearest receiver to left
                     IREC = ( X(N+1) - XREC(1) ) / TRUGEO + 1
                     GEOINC = XREC(IREC) - X(N+1)
                     GEOZ = ZREC(IREC) - Z(N+1)
                     CALL RECCON(X,Z,N,GEOINC,GEOZ,PI,
     :               BETNEW,NOCONV,SIGN(0))
                  ELSE
c                    x(n+1) < xlast
                     IF(XREC(IREC).EQ.XREC(NREC).OR.GEOINC.GT.0.) THEN
                        GEOINC = XREC(IREC) - X(N+1)
                        GEOZ = ZREC(IREC) - Z(N+1)
                     ELSE
                        IREC = IREC - 1
                        GEOINC = XREC(IREC) - X(N+1)
                        GEOZ = ZREC(IREC) - Z(N+1)
                     END IF
                     CALL RECCON(X,Z,N,GEOINC,GEOZ,PI,
     :               BETNEW,NOCONV,SIGN(0))
                     IF(NOCONV) THEN
c                       try to get to nearest receiver to left
                        IREC = ( X(N+1) - XREC(1) ) / TRUGEO + 1
                        GEOINC = XREC(IREC) - X(N+1)
                        GEOZ = ZREC(IREC) - Z(N+1)
                        CALL RECCON(X,Z,N,GEOINC,GEOZ,PI,
     :                  BETNEW,NOCONV,SIGN(0))
                     END IF
                     IF(BETNEW.LE.BELAST) THEN
c                       trouble - takeoff angle should always
c                       increase; go back to shooting
                        CALL SHOOT(X,Z,NOCONV,BETA,PI,
     :                  TRUGEO,XREC(1),XREC(NREC),RECDPT,
     :                  .FALSE.,0,0.,0.,0.)
c                       continue to nearest receiver to left
                        IREC= ( X(N+1) - XREC(1) ) / TRUGEO + 1
                        GEOINC = XREC(IREC) - X(N+1)
                        GEOZ = ZREC(IREC) - Z(N+1)
                        CALL RECCON(X,Z,N,GEOINC,GEOZ,PI,
     :                  BETNEW,NOCONV,SIGN(0))
                     END IF

                  END IF

                  DX1 = ABS(X1LAST-X(1))
                  IF((BETNEW-BETA).GE.DBMAX.AND.DX1.GE.DX1MAX) THEN
                     JUMP = .TRUE.
                  ELSE
                     JUMP = .FALSE.
                  END IF
                  IF(BETNEW.LE.BELAST.OR.JUMP) THEN
                     NOCONV = .TRUE.
                  END IF
                  IF(NOCONV) THEN
c                    all efforts failed - shoot again
                     BETA = BETA + DELTAB
                  ELSE
c                    prepare to go back to continuation
                     BETA = BETNEW
                     GEOINC = - TRUGEO
                     CAUSTC = .TRUE.
                     IPEN = ICOLOR(4)
                  END IF

               ELSE
c                 detj > 0.

                  IF(X(N+1).LT.XLAST) THEN
c                    unusual situation
c                    continue to nearest receiver to right
                     IREC = ( X(N+1) - XREC(1) ) / TRUGEO + 2
                     GEOINC = XREC(IREC) - X(N+1)
                     GEOZ = ZREC(IREC) - Z(N+1)
                     CALL RECCON(X,Z,N,GEOINC,GEOZ,PI,
     :               BETNEW,NOCONV,SIGN(0))
                  ELSE
c                    x(n+1) > xlast
                     IF(XREC(IREC).EQ.XREC(1).OR.GEOINC.LT.0.) THEN
                        GEOINC = XREC(IREC) - X(N+1)
                        GEOZ = ZREC(IREC) - Z(N+1)
                     ELSE
                        IREC = IREC + 1
                        GEOINC = XREC(IREC) - X(N+1)
                        GEOZ = ZREC(IREC) - Z(N+1)
                     END IF
                     CALL RECCON(X,Z,N,GEOINC,GEOZ,PI,
     :               BETNEW,NOCONV,SIGN(0))
                     IF(NOCONV) THEN
c                       try to get to nearest receiver to right
                        IREC = ( X(N+1) - XREC(1) ) / TRUGEO + 2
                        GEOINC = XREC(IREC) - X(N+1)
                        GEOZ = ZREC(IREC) - Z(N+1)
                        CALL RECCON(X,Z,N,GEOINC,GEOZ,PI,
     :                  BETNEW,NOCONV,SIGN(0))
                     END IF
                     IF(BETNEW.LE.BELAST) THEN
c                       trouble - takeoff angle should always
c                       increase; go back to shooting
                        CALL SHOOT(X,Z,NOCONV,BETA,PI,
     :                  TRUGEO,XREC(1),XREC(NREC),RECDPT,
     :                  .FALSE.,0,0.,0.,0.)
c                       try to get to nearest receiver to right
                        IREC = ( X(N+1) - XREC(1) ) / TRUGEO + 2
                        GEOINC = XREC(IREC) - X(N+1)
                        GEOZ = ZREC(IREC) - Z(N+1)
                        CALL RECCON(X,Z,N,GEOINC,GEOZ,PI,
     :                  BETNEW,NOCONV,SIGN(0))
                     END IF

                  END IF

                  DX1 = ABS(X1LAST-X(1))
                  IF((BETNEW-BETA).GE.DBMAX.AND.DX1.GE.DX1MAX) THEN
                     JUMP = .TRUE.
                  ELSE
                     JUMP = .FALSE.
                  END IF
                  IF(BETNEW.LE.BELAST.OR.JUMP) THEN
                     NOCONV = .TRUE.
                  END IF
                  IF(NOCONV) THEN
c                    all efforts failed
c                    go back to shooting
                     BETA = BETA + DELTAB
                  ELSE
c                    prepare to return to continuation
                     BETA = BETNEW
                     GEOINC = TRUGEO
                     CAUSTC = .FALSE.
                     IPEN = ICOLOR(5)
                  END IF

               END IF

               IF(NOCONV) THEN
               ELSE
                  CALL GAP(IREC,NTR1B,NTRNB,NTR1F,INGAP,NTRABS)
c                 plot or output this ray
                  IF(.NOT.INGAP.AND.PLTRAY) THEN
                     CALL RAYPLT(X,Z,N,IPEN)
                  ENDIF
                  IF(.NOT.INGAP.AND.TINFO) THEN
                     CALL TTIME(N,D,V,TIME1)
                  END IF
                  IF(SHTREC) THEN
                     BETA2 = BETA
                     FIRST = .TRUE.
                     IF(.NOT.INGAP) THEN
                     CALL RAYDAT(X(N+1),VREF,IREFL,IEVENT,
     :               AMP1,PHASE1,TCOEFF,MAXEVT,MAXREF)
                     END IF
                  END IF
                  IF(.NOT.INGAP.AND.LIST) THEN
                     WRITE(RAYOUT,*)N+2,TIME1
                     CALL XZOUT(X,Z,N+1,RAYOUT)
                     CALL XZOUT(X,Z,N+1,RAYLST)
                     WRITE(RAYLST,'(3X,A,F10.6/)') 't = ',TIME1
                  END IF
               END IF

            END IF

         ELSE

c           Use continuation methods.
c           update the receiver location
            IRECP = IREC
            X1LAST = X(1)
            JUMP = .FALSE.

            IF(XREC(IREC).GT.XREC(1).AND.XREC(IREC).LT.XREC(NREC)) THEN
c              receiver inside end points of line
               IF(GEOINC.GT.0.) THEN
                  GEOZ = ZREC(IREC+1) - Z(N+1)
               ELSE
                  GEOZ = ZREC(IREC-1) - Z(N+1)
               END IF
               CALL RECCON(X,Z,N,GEOINC,GEOZ,PI,
     :         BETNEW,NOCONV,SIGN(0))

            ELSE IF(XREC(IREC).EQ.XREC(NREC).AND.GEOINC.LT.0.) THEN
c              at the end of the line and moving to the left
               GEOZ = ZREC(IREC-1) - Z(N+1)
               CALL RECCON(X,Z,N,GEOINC,GEOZ,PI,
     :         BETNEW,NOCONV,SIGN(0))

            ELSE IF(XREC(IREC).EQ.XREC(1).AND.GEOINC.GT.0.) THEN
c              at the start of the line and moving to the right
               GEOZ = ZREC(IREC+1) - Z(N+1)
               CALL RECCON(X,Z,N,GEOINC,GEOZ,PI,
     :         BETNEW,NOCONV,SIGN(0))

            ELSE
c              reached the end of the line
c              search for more solutions by shooting
               NOCONV = .TRUE.

            END IF

c           make sure the takeoff angle of the new ray found
c           by the continuation procedure is greater than the
c           takeoff angle for the previous ray
c           also check to see that the change in takeoff angle
c           is not too big - if it is, suspect some missing solutions

            DX1 = ABS(X1LAST-X(1))
            IF((BETNEW-BETA).GE.DBMAX.AND.DX1.GE.DX1MAX) THEN
               JUMP = .TRUE.
            END IF
            IF(BETA.GE.BETNEW.OR.JUMP) THEN
               NOCONV = .TRUE.
               XLAST = XREC(IREC)
               BELAST = BETA
               BETA = BETA + DELTAB
            ELSE
c              update beta
               BETA = BETNEW
               IF(NOCONV) THEN
c                 set values from last good ray
c                 ray might not end at a receiver location
                  XLAST = X(N+1)
                  BELAST = BETA
                  BETA = BETA + DELTAB
                  X1LAST = X(1)
               ELSE
                  IF(GEOINC.GT.0.) THEN
                     IREC = IREC + 1
                  ELSE
                     IREC = IREC - 1
                  END IF
               END IF
            END IF

            IF(.NOT.NOCONV) THEN
               CALL GAP(IREC,NTR1B,NTRNB,NTR1F,INGAP,NTRABS)
               IF(.NOT.INGAP.AND.PLTRAY) THEN
                  CALL RAYPLT(X,Z,N,IPEN)
               END IF
               IF(.NOT.INGAP.AND.TINFO) THEN
                  CALL TTIME(N,D,V,TIME2)
               END IF
               IF(.NOT.INGAP.AND.LIST) THEN
                  WRITE(RAYOUT,*) N+2,TIME2
                  CALL XZOUT(X,Z,N+1,RAYOUT)
                  CALL XZOUT(X,Z,N+1,RAYLST)
                  WRITE(RAYLST,'(3X,A,F10.6/)') 't = ',TIME2
               END IF
            END IF

            IF(SHTREC) THEN
c              Do the amplitude calculation.
c              Ray tube consists of three rays, except at
c              start and end of branch.
c              Add the wavelet into the trace at the correct traveltime.

               IF(NOCONV) THEN
                  IF(FIRST) THEN
c                    only one ray in tube
                     DBETA = 0.
                  ELSE
c                    last ray in branch
c                    two rays in tube
                     DBETA = BETA2 - BETA1
                     SPACIN = 1.
                  END IF
               ELSE
c                 do some amplitude calculations for this ray
c                 these will be used next time
                  IF(.NOT.INGAP) THEN
                     CALL RAYDAT(X(N+1),VREF,IREFL,IEVENT,
     :               AMP2,PHASE2,TCOEFF,MAXEVT,MAXREF)
                  END IF

                  BETA3 = BETA

                  IF(FIRST) THEN
c                    first ray of branch
c                    two rays in tube
                     DBETA = BETA3 - BETA2
                     SPACIN = 1.
                  ELSE
c                    three rays in tube
                     DBETA = BETA3 - BETA1
                     SPACIN = 2.
                  END IF
               END IF

               IF(DBETA.EQ.0.) THEN
c                 can't find amplitude ( spreading )
               ELSE
c                 finish the amplitude calculation for the previous ray
c                 if it is not in the gap
                  CALL GAP(IRECP,NTR1B,NTRNB,NTR1F,INGAP,NTRABS)
                  IF(.NOT.INGAP) THEN
                     AMP = CONST * AMP1 *
     :               SQRT( ( V(1) * DBETA * PI / 180. ) / SPACIN )
                     IF(NTRABS.LE.NTRNB) THEN
                        ITRACE = IRECP
                     ELSE
                        ITRACE = IRECP - NGAP
                     END IF
                     WRITE(SHTOUT)IRECD,ITRACE,NTRABS+MU,TIME1,
     :               XREC(IRECP)-X(0),IEVENT,ZS(IRECD),
     :               AMP,PHASE1,CAUSTC,HEAD
                  END IF
               END IF

               IF(NOCONV) THEN
c                 will have to start again ( shooting )
               ELSE
c                 drop first ray, prepare to pick up new ray
                  TIME1 = TIME2
                  AMP1 = AMP2
                  PHASE1 = PHASE2
                  BETA1 = BETA2
                  BETA2 = BETA3
                  FIRST = .FALSE.
               END IF

            END IF

         END IF

         GO TO 900

      END IF

999   CONTINUE
c     Identify end of event.
      IF(LIST) THEN
         WRITE(RAYLST,'(2X,A)') 'End of event'
      END IF


1000  CONTINUE


      IF(LIST) THEN
         WRITE(RAYLST,'(/2X,A)') 'End of Shot'
      END IF


2000  CONTINUE

      IF(LIST) THEN
         WRITE(RAYLST,'(2(/),2X,A)') 'End of listing'
      END IF


c     close files


      IF(BEGPLT) THEN
         CALL PLOTE
      END IF

      IF(SHTREC) THEN
         CLOSE(UNIT=SHTOUT,STATUS='keep')
      END IF

      IF(LIST) THEN
         CLOSE(UNIT=RAYOUT,STATUS='keep')
         CLOSE(UNIT=RAYLST,STATUS='keep')
      END IF

5005  FORMAT(2F10.2)

      STOP
      END

*--------------- end of main program -------------------------------
