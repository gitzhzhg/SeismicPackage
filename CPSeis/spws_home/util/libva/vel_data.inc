


ccccccccccccc   vel_data.inc   ccccccccccccccc

cccc   the defined parameters must exactly match corresponding constants
cccc   in the C header file vel_data.h

cccc   common block vd must exactly match structure VdStruct in vel_data.h
cccc   common block vp must exactly match structure VpStruct in vel_data.h

cccc   common block vp is a template for dynamically allocated memory for
cccc   storing picks.
cccc
cccc   Two buffers for a velocity function are stored in dynamically
cccc   allocated memory (picks only), and in locations nfunmax+1 and
cccc   nfunmax+2.

cccc   Default values for velocity function headers are stored in location
cccc   nfunmax+3 (there is no associated dynamic memory).


c------------defined parameters for common block sizes.

      integer nfunmax   ! maximum number of velocity functions in a file.
      integer nmax      ! maximum number of picks in a velocity function.
      integer nmutemax  ! maximum number of mute indices.
      integer nraymax   ! maximum number of rays to trace.
      integer numtypes  ! number of velocity function types.

      parameter (nfunmax=5000, nmax=200, nmutemax=20, nraymax=200)
      parameter (numtypes=11)


c------------constants corresponding to velocity function types.

      integer firsttype,lasttype
      integer VTNM,VTRM,VZRM,VLRM
      integer      VTAV,VZAV,VLAV
      integer      VTIN,VZIN,VLIN,VTDP

      parameter (firsttype=0)
      parameter (VTNM= 0)  ! veltype(1) = "VTNM" (NMO vel      vs 2-way time) 
      parameter (VTRM= 1)  ! veltype(2) = "VTRM" (RMS vel      vs 2-way time) 
      parameter (VZRM= 2)  ! veltype(3) = "VZRM" (RMS vel      vs depth)      
      parameter (VLRM= 3)  ! veltype(4) = "VLRM" (RMS vel      vs thickness)  
      parameter (VTAV= 4)  ! veltype(5) = "VTAV" (average vel  vs 2-way time) 
      parameter (VZAV= 5)  ! veltype(6) = "VZAV" (average vel  vs depth)      
      parameter (VLAV= 6)  ! veltype(7) = "VLAV" (average vel  vs thickness)  
      parameter (VTIN= 7)  ! veltype(8) = "VTIN" (interval vel vs 2-way time) 
      parameter (VZIN= 8)  ! veltype(9) = "VZIN" (interval vel vs depth)      
      parameter (VLIN= 9)  ! veltype(10)= "VLIN" (interval vel vs thickness)  
      parameter (VTDP=10)  ! veltype(11)= "VTDP" (depth        vs 2-way time) 
      parameter (lasttype=numtypes-1)

 

c------------common block vd.

      integer nfun       ! number of velocity functions in the file.
      integer ifun       ! current active velocity function in the file.
      integer nhx        ! header word for X-bin location.
      integer nhy        ! header word for Y-bin location.
      integer order      ! order     of moveout (2 or 4).
      real    nhosign    ! sign      of moveout (normally 1.0 or -1.0).
      real    nhoexp     ! exponent  of moveout (normally 2.0 or  4.0).
      integer inil       ! integer value interpreted as a nil.
      real    fnil       ! floating point value interpreted as a nil.
      integer changeflag ! flag set to non-zero when change made since saving.

      real    offmin            ! minimum offset.
      real    offmax            ! maximum offset.
      integer muteflag          ! mute flag (1 or 2 or 3).
      integer nmute             ! number of offset/time mute pairs.
      real    omute (nmutemax)  ! mute offsets.
      real    tmute (nmutemax)  ! mute times.

      real    startvel          ! default starting velocity.
      real    bottomtime        ! default ending time.
      real    timetol           ! time  tolerance for assuming equality.
      real    depthtol          ! depth tolerance for assuming equality.
      real    xcenter,xwidth    ! any xbin center, xbin width.
      real    ycenter,ywidth    ! any ybin center, ybin width.
      real    xtol              ! xbin  tolerance for assuming equality.
      real    ytol              ! ybin  tolerance for assuming equality.
      real    xtol_request      ! user-requested xbin tolerance.
      real    ytol_request      ! user-requested ybin tolerance.
      integer auto_xtol         ! 1 = auto calc xtol; 0 = use xtol_request.
      integer auto_ytol         ! 1 = auto calc ytol; 0 = use ytol_request.
      real    ybin_request      ! user-requested ybin value to use.
      integer xdirwant          ! xbin sorting preference.
      integer ydirwant          ! ybin sorting preference.

      integer nray              ! number of traced rays.
      real    oray (nraymax)    ! raytraced offsets.
      real    tray (nraymax)    ! raytraced times.

      real         xbin   (nfunmax+3)   ! X-bin location.
      real         ybin   (nfunmax+3)   ! Y-bin location.
      integer      n      (nfunmax+3)   ! number of velocity function picks.
      integer      point  (nfunmax+3)   ! pointer to copy of common block vp.

      character*8  vfid   (nfunmax+3)   ! velocity function name.
      character*6  select (nfunmax+3)   ! codes indicating selected functions.
      character*4  type   (nfunmax+3)   ! velocity function type (see below).
      character*1  errmsg (nfunmax+3)   ! code indicating conversion error.
      character*1  raymsg (nfunmax+3)   ! code indicating whether raytraced.

      character*10 project(nfunmax+3)   ! project name.
      character*10 line   (nfunmax+3)   ! line name.
      character*5  rdate  (nfunmax+3)   ! recording date.
      character*5  pdate  (nfunmax+3)   ! processing date.
      character*3  userid (nfunmax+3)   ! user identification.
      character*15 comment(nfunmax+3)   ! comment string.

      character*4  veltype(numtypes)    ! velocity function types.

      common/vd/
c    $   nfun,ifun,nhx,nhy,inil,fnil,changeflag,
     $   nfun,ifun,nhx,nhy,order,nhosign,nhoexp,inil,fnil,changeflag,
     $   offmin,offmax,muteflag,nmute,omute,tmute,
     $   startvel,bottomtime,timetol,depthtol,
     $   xcenter,xwidth,ycenter,ywidth,
     $   xtol,ytol,xtol_request,ytol_request,
     $   auto_xtol,auto_ytol,ybin_request,xdirwant,ydirwant,
     $   nray,oray,tray,
     $   xbin,ybin,n,point,
     $   vfid,select,type,errmsg,raymsg,
     $   project,line,rdate,pdate,userid,comment,
     $   veltype



c------------common block vp (velocity function picks).

      real depths (nmax)      ! depth picks.                   VZxx
      real times  (nmax)      ! two-way time picks.            VTxx
      real thick  (nmax)      ! thickness picks.               VLxx
      real vrms   (nmax)      ! RMS velocity picks.            xxRM
      real vav    (nmax)      ! average velocity picks.        xxAV
      real vint   (nmax)      ! interval velocity picks.       xxIN
      real vnmo   (nmax)      ! NMO velocity picks.            xxNM
      real offpick(nmax)      ! maximum raytraced offset

      common/vp/depths,times,thick,vrms,vav,vint,vnmo,offpick



c------------------------end.

