c       velfun_futil.f
C<license>
C-------------------------------------------------------------------------------
C Copyright (c) 2007 ConocoPhillips Company
C
C Permission is hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to deal
C in the Software without restriction, including without limitation the rights
C to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
C copies of the Software, and to permit persons to whom the Software is
C furnished to do so, subject to the following conditions:
C
C The above copyright notice and this permission notice shall be included in all
C copies or substantial portions of the Software.
C
C THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
C IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
C AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
C LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
C OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
C SOFTWARE.
C-------------------------------------------------------------------------------
C</license>
C***************************** COPYRIGHT NOTICE ********************************
C*                                                                             *
C*                 CONFIDENTIAL AND PROPRIETARY INFORMATION                    *
C*                              OF CONOCO INC.                                 *
C*                      PROTECTED BY THE COPYRIGHT LAW                         *
C*                          AS AN UNPUBLISHED WORK                             *
C*                                                                             *
C***************************** COPYRIGHT NOTICE ********************************
c        1         2         3         4         5         6         7  |
c23456789012345678901234567890123456789012345678901234567890123456789012|
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                   EXPLORATION RESEARCH AND SERVICES
C                              CONOCO, INC.
C
C                             U T I L I T Y 
C       written in fortran -- designed to be called from fortran
C
C     Utility Name:  velfun_util       (velocity function utilities)
C          Written:  93/01/29  by:  Tom Stoeckley
C     Last revised:  93/03/10  by:  Tom Stoeckley
C
C  Purpose:   This is a set of routines for converting velocity
C             functions from one type to another, retrieving and
C             storing velocity function picks, ray-tracing a
C             velocity function, etc.  Used by applications VEL
C             and VA.
C
C  Related Documentation:  VELFUN is an older routine which does
C                          some velocity function conversions.
C-----------------------------------------------------------------------
C                          LOCATION OF CODE
C
C  machine     source code directory     source files        library
C  -------     ---------------------     ------------        -------
C  ultrix      ~spws/util/trslib         velfun_futil.f      trslib.a
C                                        velfun_futil.h
C----------------------------------------------------------------------
C             DIFFERENCES BETWEEN CODE ON DIFFERENT MACHINES
C                                 n/a 
C-----------------------------------------------------------------------
C            FORTRAN ROUTINES ON SOURCE FILE velfun_futil.f
C
C  Subroutines:        velfun_type_cc2ii         velfun_put_picks
C                      velfun_fixup_first_pick   velfun_get_picks
C                      velfun_insert_pick        velfun_put_pick
C                      velfun_remove_pick        velfun_get_pick
C                      velfun_generate
C                      velfun_update             velfun_inv
C                      velfun_curve              velfun_nmo
C                      velfun_fixup_x            velfun_fixup_v
C                      velfun_fixup_xx
C                      velfun_nmo_prep           velfun_do_nmo
C                      nonhyp_nmo_prep           nonhyp_do_nmo
C                      eta_nmo_prep              eta_do_nmo
C
C  Subroutine entries: velfun_type_ii2cc
C
C  Functions:          none
C  Function entries:   none
C  Common blocks:      none
C-----------------------------------------------------------------------
C          LIBRARIES AND HEADER FILES REQUIRED BY THIS UTILITY
C
C  Libraries:     trslib.a  cpsprim.a
C  Header files:  none
C-----------------------------------------------------------------------
C                 EXTERNALS REFERENCED BY THIS UTILITY
C
C              curve    polya    least   dyncc
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author     Description
C     ----      ------     -----------
C  2. 93/03/10  Stoeckley  add nmo conversion.
C  1. 93/01/29  Stoeckley  initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
c                    TYPES OF VELOCITY FUNCTIONS
c
c  type  itype  descrip        (ntypes = 11)           defined by arrays
c  ----  -----  -------                                -----------------
c  VTNM    1    VTNM: NMO velocity versus 2-way time       times  vnmo
c  VTRM    2    VTRM: RMS velocity versus 2-way time       times  vrms
c  VZRM    3    VZRM: RMS velocity versus depth            depths vrms
c  VLRM    4    VLRM: RMS velocity versus thickness        thick  vrms
c  VTAV    5    VTAV: average velocity versus 2-way time   times  vav 
c  VZAV    6    VZAV: average velocity versus depth        depths vav 
c  VLAV    7    VLAV: average velocity versus thickness    thick  vav 
c  VTIN    8    VTIN: interval velocity versus 2-way time  times  vint
c  VZIN    9    VZIN: interval velocity versus depth       depths vint
c  VLIN   10    VLIN: interval velocity versus thickness   thick  vint
c  VTDP   11    VTDP: depth versus 2-way time              times  depths
c
C-----------------------------------------------------------------------
C       THE VARIOUS ROUTINES IN THIS UTILITY ARE LISTED BELOW.
C
C  For each of the following routines, each parameter is flagged as
C  follows:   i = value required upon INPUT
C             o = value set by the routine upon OUTPUT
C             b = value BOTH required upon input and changed upon output
c             s = scratch array
C-----------------------------------------------------------------------
c                 SUMMARY OF CALLS AND ARGUMENTS
c
c                            i          o      o      o
c  velfun_type_cc2ii       (type,     itype,descrip,ntypes)
c  velfun_type_ii2cc       (itype,     type,descrip,ntypes)
c
c                            i   i    i  i       o           o   o
c  velfun_put_pick         (type,j,   xj,vj,   XXXX7,       msg,ierr)
c  velfun_put_picks        (type,n,   x ,v ,   XXXX7,       msg,ierr)
c
c                            i   i    o  o       i           o   o
c  velfun_get_pick         (type,j,   xj,vj,   XXXX7,       msg,ierr)
c  velfun_get_picks        (type,n,   x ,v ,   XXXX7,       msg,ierr)
c
c                              i   b  i    i       b
c  velfun_insert_pick        ( j  ,n,nmax,       XXXX7                )
c  velfun_remove_pick        ( j  ,n,            XXXX7                )
c  velfun_fixup_first_pick   (type,n,nmax,SSSS,  XXXX7                )
c  velfun_fixup_first_pick_c (type,n,nmax,SSSS,  XXXX7                )
c
c                      i     i   i      i        b    o      o   o
c  velfun_generate  (       type,n,            XXXX6,       msg,ierr)
c  velfun_update    (invoke,type,n,    MMMM,   XXXX7,RRRR,  msg,ierr)
c  velfun_update_c  (invoke,type,n,    MMMM,   XXXX7,RRRR,  msg,ierr)
c  velfun_nmo       (            n,    MMMM,   XXXX5,RRRR,  msg,ierr)
c  velfun_inv       (            n,    MMMM,   XXXX7,RRRR,  msg,ierr)
c
c                i   i     i     i      i        o    o
c  velfun_curve (n,depths,vint,offmin,offmax,   vnmo,RRRR           )
c
c                    i    i   i  i    i   i      o   o   o  o   o    o
c  velfun_nmo_prep(times,vnmo,n,tmin,tmax,dt,   nnn,ttt,vvv,tb,cnst,ierr)
c
c                    i   i   i   i  i   i    i     i      s    i o  i    o
c  velfun_do_nmo   (dop,nnn,ttt,vvv,tb,cnst,idir,offset,  ta,  a,b,nval,bmute)
c
c                      i      i
c  nonhyp_nmo_prep(nhosign,nhoexp,
c                   times,vnmo,n,tmin,tmax,dt,   nnn,ttt,vvv,tb,cnst,ierr)
c                    i    i   i  i    i   i      o   o   o  o   o    o
c
c                      i      i
c  nonhyp_do_nmo   (nhosign,nhoexp,
c                   dop,nnn,ttt,vvv,tb,cnst,idir,offset,  ta,  a,b,nval,bmute)
c                    i   i   i   i  i   i    i     i      s    i o  i    o
c
c                    i    i    i  i  i    i   i      o   o   o   o   o   o
c  eta_nmo_prep   (times,vnmo,eta,n,tmin,tmax,dt,   nnn,ttt,vvv,eee,fff,ggg,
c                  tb,cnst,ierr)
c                   o  o    o
c
c                   i   i   i   i   i   i   i  i   i    i     i   
c  eta_do_nmo     (dop,nnn,ttt,vvv,eee,fff,ggg,tb,cnst,idir,offset,
c                  ta,  a,b,nval,bmute)
c                  s    i o  i    o
c
c     SSSS  = startvel,timetol,depthtol                 (always input)
c     MMMM  = muteflag,offmin,offmax,nmute,omute,tmute  (always input)
c     XXXX7 = depths,times,vnmo,vrms,vav,vint,thick
c     XXXX6 = depths,times,     vrms,vav,vint,thick
c     XXXX5 = depths,times,vnmo,vrms,    vint
c     RRRR  = offpick,nray,oray,tray                    (always output)
C-----------------------------------------------------------------------
c                    DEFINITIONS OF ARGUMENTS
c
c  integer itype         type of velocity function (1 thru ntypes).
c  character*(*) type    type of velocity function (4 characters).
c  character*(*) descrip description of velocity function type.
c  integer ntypes        number of velocity function types (constant).
c
c  integer n             number of velocity function picks.
c  integer nmax          max allowed number of velocity function picks.
c  integer invoke        = 0 means do not do ray tracing.
c
c  integer j             number of pick (1 thru n) to insert,remove,put,get.
c  real    xj            j-th time/depth/thickness pick   to put or get.
c  real    vj            j-th vnmo/vrms/vav/vint velocity to put or get.
c  real    x(n)          time/depth/thickness picks       to put or get.
c  real    v(n)          vnmo/vrms/vav/vint velocities    to put or get.
c
c  real    offmin        minimum offset to raytrace.
c  real    offmax        maximum offset to raytrace.
c
c  character*(*) msg     = blank means no error occurred.
c  integer ierr          = 0 means no error occurred.
c
c-----------------combined in shorthand SSSS above:
c
c  real    startvel      default starting velocity.
c  real    timetol       small tolerance (seconds) interpreted as zero.
c  real    depthtol      small tolerance (feet/meters) interpreted as zero.
c
c-----------------combined in shorthand MMMM above:
c
c  integer muteflag      = 1 means do not mute at all.
c                        = 2 means mute at maximum offset = depth.
c                        = 3 means use offset/time mutes after NMO.
c  real    offmin        minimum offset for ray tracing.
c  real    offmax        maximum offset for ray tracing.
c  integer nmute         number of mute offset/time pairs.
c  real    omute(nmute)  mute offsets (monotonically increasing).
c  real    tmute(nmute)  mute times (monotonically increasing).
c
c-----------------combined in shorthand XXXX above:
c
c  real    DEPTHS (n)    depth picks.
c  real    TIMES  (n)    time picks.
c  real    VNMO   (n)    NMO (stacking) velocities.
c  real    VRMS   (n)    RMS velocities.
c  real    VAV    (n)    average velocities.
c  real    VINT   (n)    interval velocities (in layer above each pick).
c  real    THICK  (n)    layer thicknesses (in layer above each pick).
c
c-----------------combined in shorthand RRRR above:
c
c  real    offpick(n)    maximum raytraced offset for each pick.
c                        = 0      if raytracing not attempted.
c                        = offmax if raytracing successfully completed.
c                        = -1     if no rays are successfully traced.
c                        = minus max offset reached if offmax not reached.
c  integer NRAY          (for last pick) number of rays traced (0 thru 200).
c  real    ORAY(200)     (for last pick) raytraced offsets.
c  real    TRAY(200)     (for last pick) raytraced times.
c
C-----------------------------------------------------------------------
c             INTEGER AND CHARACTER VELOCITY FUNCTION TYPES
c
c  To get  integer  velocity function type from character type:
c  To get character velocity function type from  integer  type:
c                             i        o      o      o
c         velfun_type_cc2ii (type,   itype,descrip,ntypes)
c         velfun_type_ii2cc (itype,   type,descrip,ntypes)
c
c  Also returns a description of the type, and the number of types.
c  If character type  is invalid, itype=0  is returned.
c  If integer   itype is invalid, type=' ' is returned.
C-----------------------------------------------------------------------
c                   GETTING AND PUTTING PICKS
c
c  To put one pick          of specified type into the velocity arrays:
c  To put an array of picks of specified type into the velocity arrays:
c  To get one pick          of specified type from the velocity arrays:
c  To get an array of picks of specified type from the velocity arrays:
c                           i   i    i  i       o      o   o
c        velfun_put_pick  (type,j,   xj,vj,   XXXX7,  msg,ierr)
c        velfun_put_picks (type,n,   x ,v ,   XXXX7,  msg,ierr)
c
c                           i   i    o  o       i      o   o
c        velfun_get_pick  (type,j,   xj,vj,   XXXX7,  msg,ierr)
c        velfun_get_picks (type,n,   x ,v ,   XXXX7,  msg,ierr)
c
c  When putting picks, other velocity types are set to nil.
c  If picks (x,v) or pick (xj,xj) contain any nils, ierr=1 is returned.
C-----------------------------------------------------------------------
c                   INSERTING AND REMOVING PICKS
c
c  To insert one pick into the velocity arrays:
c  To remove one pick from the velocity arrays:
c                                i   b  i        b
c            velfun_insert_pick (j  ,n,nmax,   XXXX7)
c            velfun_remove_pick (j  ,n,        XXXX7)
c
c  When inserting:
c     j must lie between 1 and n+1.
c     n must be less than nmax.
c     n is incremented by one if successful.
c     if j > 1, the inserted pick is all nils.
c     if j = 1, the inserted pick has unchanged velocities at zero time.
c  When removing:
c     j must lie between 1 and n.
c     n must be greater than 0.
c     n is decremented by one if successful.
c  If j or n is out of range, nothing is done.
C-----------------------------------------------------------------------
c                     FIXING UP FIRST PICK
c
c  To make sure the first pick is valid and at zero time:
c                                      i   b  i    i       b
c          velfun_fixup_first_pick   (type,n,nmax,SSSS,  XXXX7)
c          velfun_fixup_first_pick_c (type,n,nmax,SSSS,  XXXX7)
c
c  If first pick is within tolerance of zero, or negative,
c      it is reset to zero; otherwise a new pick is inserted at zero.
c  If the first velocity is nil or <=0, or a new pick has been inserted,
c      the velocity is reset.
c  If n <= 0, it is assumed to be 1, and is reset to 1.
c  No insertions are done if n >= nmax; the first pick may be changed.
c  Upon return, the following are always true:
c      depths(1) = times(1) = thick(1) = 0.
c      vnmo(1) = vrms(1) = vav(1) = vint(1) = valid number.
c  velfun_fixup_first_pick_c is a C-callable interface to
c      velfun_fixup_first_pick.
C-----------------------------------------------------------------------
c                 GENERATING ALL VELOCITY TYPES
c
c  Given the velocity function type, and the values in the corresponding
c  velocity arrays, fill out the rest of the arrays for all other types:
c                         i     i   i  i       b      o    o   o
c      velfun_generate (       type,n,       XXXX6,       msg,ierr)
c      velfun_update   (invoke,type,n,MMMM,  XXXX7,  RRRR,msg,ierr)
c      velfun_update_c (invoke,type,n,MMMM,  XXXX7,  RRRR,msg,ierr)
c
c  The velocity function does not have to start at zero time.
c  velfun_generate cannot use type VTNM (stacking velocity).
c  velfun_update will do ray-tracing to convert to or from type VTNM.
c  velfun_update calls velfun_generate, velfun_nmo, and velfun_inv.
c  If invoke = 0, types VTNM and VTRM will be identical upon output.
c  Sets values to nil if they cannot be calculated.
c  Sets ierr = 0 if all is OK.
c  Sets ierr = -1 if n is zero.
c  Sets ierr = -2 if the type is invalid.
c  Sets ierr = -3 if difficult raytracing is not completed.
c  Sets ierr = first bad index (1 thru n) if some values are set to nil.
c  velfun_update_c is a C-callable interface to velfun_update.
C-----------------------------------------------------------------------
c                 FORWARD AND INVERSE RAY TRACING
c
c  To get VTNM (stacking velocities) by ray tracing:
C  To get all other velocities from VTNM by inverse ray tracing:
c                        i  i        b       o    o   o
c            velfun_nmo (n,MMMM,   XXXX5,   RRRR,msg,ierr)
c            velfun_inv (n,MMMM,   XXXX7,   RRRR,msg,ierr)
c
c  velfun_nmo  input: depths,times,vrms,vint   output: vnmo
c  velfun_inv  input:        times,vnmo        output: all other arrays
c  velfun_inv calls velfun_generate and velfun_nmo.
c  velfun_nmo calls velfun_curve.
C-----------------------------------------------------------------------
c               GETTING LEAST SQUARES STACKING VELOCITY
c
c  Given depths and interval velocities down to interface n, and given
c  desired offset range, returns actual offsets and timings before NMO
c  correction, and least squares stacking velocity at interface n:
c                     i   i     i     i      i       o    o
c       velfun_curve (n,depths,vint,offmin,offmax,  vnmo,RRRR)
c
c  oray(1)    = first raytraced offset (>= offmin).
c  oray(nray) = last raytraced offset (>= offmax if successful).
c  vnmo is set to zero unless successfully raytraced and curve-fitted.
c  velfun_curve calls curve, polya, and least.
C-----------------------------------------------------------------------
c                  DOING NMO CORRECTION ON A TRACE
c
c  First get variables which will increase the efficiency:
c  Then do the NMO correction for a seismic trace:
c
c                    i    i   i   i   i   i      o   o   o  o   o    o
c  velfun_nmo_prep(times,vnmo,n,tmin,tmax,dt,   nnn,ttt,vvv,tb,cnst,ierr)
c
c  velfun_do_nmo  (dop,nnn,ttt,vvv,tb,cnst,idir,offset,  ta,  a,b,nval,bmute)
c                   i   i   i   i  i   i    i     i      s    i o  i    o
c
c                     i      i
c  nonhyp_nmo_prep(nhosign,nhoexp,
c                  times,vnmo,n,tmin,tmax,dt,   nnn,ttt,vvv,tb,cnst,ierr)
c                    i    i   i   i   i   i      o   o   o  o   o    o
c
c                     i      i
c  nonhyp_do_nmo  (nhosign,nhoexp,
c                  dop,nnn,ttt,vvv,tb,cnst,idir,offset,  ta,  a,b,nval,bmute)
c                   i   i   i   i  i   i    i     i      s    i o  i    o
c
c                    i    i    i  i  i    i   i      o   o   o   o   o   o
c  eta_nmo_prep   (times,vnmo,eta,n,tmin,tmax,dt,   nnn,ttt,vvv,eee,fff,ggg,
c                  tb,cnst,ierr)
c                   o  o    o
c
c                   i   i   i   i   i   i   i  i   i    i     i   
c  eta_do_nmo     (dop,nnn,ttt,vvv,eee,fff,ggg,tb,cnst,idir,offset,
c                  ta,  a,b,nval,bmute)
c                  s    i o  i    o
c
c  real nhosign  = 1.0 for normal NMO; typically -1.0 for non-hyperbolic NMO.
c  real nhoexp   = 2.0 for normal NMO; typically  4.0 for non-hyperbolic NMO.
c  real times(n) = zero-offset time picks.
c  real vnmo(n)  = stacking velocity picks.
c  real eta(n)   = eta picks for anisotropic NMO.
c  real tmin     = starting time in seconds (on first index of seismic trace).
c  real tmax     = ending time in seconds (on last index of seismic trace).
c  real dt       = sample rate (in seconds) on seismic trace.
c  integer nnn   = length of the following arrays ( = n or n+1).
c  real ttt(nnn) = normalized times squared.
c  real vvv(nnn) = normalized inverse velocities squared.
c  real eee(nnn) = additional normalized factors for anisotropic NMO.
c  real fff(nnn) = additional normalized factors for anisotropic NMO.
c  real ggg(nnn) = additional normalized factors for anisotropic NMO.
c  real ta(nnn)  = indices of pick times on trace before movout correction.
c  real tb(nnn)  = indices of pick times on nmo-corrected trace.
c  real cnst     = helpful constant.
c  integer ierr  =  =0 if no error; =1 if error (e.g. nil values).
c  real dop      = doppler stretch (>1) (maximum stretch factor allowed).
c  integer idir  =  >0 for forward nmo and <0 for reverse nmo.
c  real offset   = offset of seismic trace.
c  real a(nval)  = input trace.
c  real b(nval)  = output trace.
c  real bmute    = mute index.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C\END DOC



      subroutine velfun_type_cc2ii (type,  itype,descrip,ntypes)
c     converts between character and integer velocity function type,
c        and also returns a description of the type, and the number
c        of types.
c     entry velfun_type_cc2ii:  converts character*4 type to integer.
c     entry velfun_type_ii2cc:  converts integer type to character*4.
c     the integer type is a value from 1 thru ntypes.
c     if character type  is invalid, itype=0  is returned.
c     if integer   itype is invalid, type=' ' is returned.

      implicit none
      character*(*) type,descrip
      integer itype,ntypes,n,i
      parameter (n=11)
      character*60 typemes(n)
      save typemes
      data typemes/'VTNM: NMO velocity versus 2-way time',
     $             'VTRM: RMS velocity versus 2-way time',
     $             'VZRM: RMS velocity versus depth     ',
     $             'VLRM: RMS velocity versus thickness ',
     $             'VTAV: average velocity versus 2-way time',
     $             'VZAV: average velocity versus depth     ',
     $             'VLAV: average velocity versus thickness ',
     $             'VTIN: interval velocity versus 2-way time',
     $             'VZIN: interval velocity versus depth     ',
     $             'VLIN: interval velocity versus thickness ',
     $             'VTDP: depth versus 2-way time            '/

c     entry velfun_type_cc2ii (type,  itype,descrip,ntypes)
      ntypes=n
      do i=1,n
           if (type.eq.typemes(i)(1:4)) then
                descrip=typemes(i)
                itype=i
                return
           end if
      end do
      descrip='invalid velocity function type'
      itype=0
      return


      entry velfun_type_ii2cc (itype,  type,descrip,ntypes)
      ntypes=n
      if (itype.ge.1.and.itype.le.n) then
           type=typemes(itype)(1:4)
           descrip=typemes(itype)
      else
           type=' '
           descrip='invalid velocity function type'
      end if
      return
      end





      subroutine velfun_put_picks (type,n,x,v,
     $            depths,times,vnmo,vrms,vav,vint,thick,   msg,ierr)
c     put picks x.v into velocity function, and set the other velocity
C        types to nil.
c     returns ierr=1 if x,v contain any nils.

      implicit none
      character*(*) type
      integer n,ierr,i,ierr2
      real x(*),v(*)
      real depths(*),times(*),vnmo(*),vrms(*),vav(*),vint(*),thick(*)
      character*(*) msg
 
      msg=' '
      ierr=0
      do i=1,n
           call velfun_put_pick (type,i,x(i),v(i),
     $            depths,times,vnmo,vrms,vav,vint,thick,   msg,ierr2)
           if (ierr2.ne.0) ierr=ierr2
      end do
      if (ierr.ne.0) msg='some picks have nil values'
      return
      end



      subroutine velfun_get_picks (type,n,x,v,
     $             depths,times,vnmo,vrms,vav,vint,thick,   msg,ierr)
c     get picks x.v from velocity function.
c     returns ierr=1 if x,v contain any nils.

      implicit none
      character*(*) type
      integer n,ierr,i,ierr2
      real x(*),v(*)
      real depths(*),times(*),vnmo(*),vrms(*),vav(*),vint(*),thick(*)
      character*(*) msg
 
      msg=' '
      ierr=0
      do i=1,n
           call velfun_get_pick (type,i,x(i),v(i),
     $             depths,times,vnmo,vrms,vav,vint,thick,   msg,ierr2)
           if (ierr2.ne.0) ierr=ierr2
      end do
      if (ierr.ne.0) msg='some picks have nil values'
      return
      end





      subroutine velfun_fixup_first_pick_c (h_type,n,nmax,
     $            startvel,timetol,depthtol,
     $            depths,times,vnmo,vrms,vav,vint,thick)

      implicit none
      integer       h_type(*)
      character*8   type
      integer n,nmax
      real startvel,timetol,depthtol
      real depths(*),times(*),vnmo(*),vrms(*),vav(*),vint(*),thick(*)

      call convert_hh2cc (h_type,0,   type,8)
      call velfun_fixup_first_pick (type,n,nmax,
     $            startvel,timetol,depthtol,
     $            depths,times,vnmo,vrms,vav,vint,thick)
      return
      end





      subroutine velfun_fixup_first_pick (type,n,nmax,
     $            startvel,timetol,depthtol,
     $            depths,times,vnmo,vrms,vav,vint,thick)
c     makes sure first pick is valid and at zero time.
c     if first pick is within tolerance of zero, or negative,
c         it is reset to zero; otherwise a new pick is inserted at zero.
c     if the first velocity is nil or <=0, or a new pick has been inserted,
c         the velocity is set to startvel (or similar valid number).
c     if n <= 0, it is assumed to be 1, and is reset to 1.
c     no insertions are done if n >= nmax; the first pick may be changed. 
c     upon return, the following are always true:
c         depths(1) = times(1) = thick(1) = 0.
c         vnmo(1) = vrms(1) = vav(1) = vint(1) = valid number.

      implicit none
      character*(*) type
      integer n,nmax
      real startvel,timetol,depthtol,fnil
      real depths(*),times(*),vnmo(*),vrms(*),vav(*),vint(*),thick(*)
      logical insert,insert2

      fnil = -1.0e-30

      n=max(n,1)
      insert=.false.
      if (type.eq.'VTDP') then
           call velfun_fixup_x (n,nmax,times ,timetol ,insert)
           call velfun_fixup_x (n,nmax,depths,depthtol,insert)
           if(n.ge.2)call velfun_fixup_x(1,2,times(2) ,timetol ,insert2)
           if(n.ge.2)call velfun_fixup_x(1,2,depths(2),depthtol,insert2)
           call velfun_fixup_xx (n,times,depths,startvel)
      else if (type(1:2).eq.'VT') then
           call velfun_fixup_x (n,nmax,times ,timetol ,insert)
      else if (type(1:2).eq.'VZ') then
           call velfun_fixup_x (n,nmax,depths,depthtol,insert)
      else if (type(1:2).eq.'VL') then
           call velfun_fixup_x (n,nmax,thick ,depthtol,insert)
      end if
      if (type(3:4).eq.'NM') then
           call velfun_fixup_v (n,vnmo,startvel)
      else if (type(3:4).eq.'RM') then
           call velfun_fixup_v (n,vrms,startvel)
      else if (type(3:4).eq.'AV') then
           call velfun_fixup_v (n,vav ,startvel)
      else if (type(3:4).eq.'IN') then
           call velfun_fixup_v (n,vint,startvel)
      end if
      if (insert) then
           call velfun_insert_pick (1,n,nmax,
     $                     depths,times,vnmo,vrms,vav,vint,thick)
           if (startvel.ne.fnil.and.startvel.gt.0.) then
                vnmo(1)=startvel
                vrms(1)=startvel
                vav (1)=startvel
                vint(1)=startvel
           end if
      end if
      return
      end
 
      
      
      
      subroutine velfun_fixup_x (n,nmax,x,tolerance,insert)
c     if x(1) is invalid, or n >= nmax, reset x(1) to zero.
c     if x(1) > 0 when finished, reset insert=.true.
c     it is assumed that n >= 1.

      implicit none
      integer n,nmax
      real x(*),tolerance,fnil
      logical insert

      fnil = -1.0e-30
      if (x(1).eq.fnil.or.x(1).le.tolerance.or.n.ge.nmax) x(1)=0.
      if (x(1).gt.0.) insert=.true.
      return
      end
      
      
      
      
      subroutine velfun_fixup_v (n,v,startvel)
c     if v(1) is invalid, first reset to startvel, then reset to v(2) if
c                  n >= 2 and v(2) is valid and v(2) is less than startvel.
c     it is assumed that n >= 1.

      implicit none
      integer n
      real v(*),startvel,fnil

      fnil = -1.0e-30
      if (v(1).gt.0..and.v(1).ne.fnil) return
   !  if (startvel.ne.fnil.and.startvel.gt.0.) then   ! removed 1/29/01
      if (startvel.ne.fnil.and.startvel.ge.0.) then   ! added   1/29/01
                                                      ! so first geopress
!                                                     ! attribute can be 0.
           v(1)=startvel
      else if (n.ge.2) then
           if (v(2).gt.0..and.v(2).ne.fnil) v(1)=v(2)
      end if
      return
      end



      subroutine velfun_fixup_xx (n,times,depths,startvel)
c     if only one of times(1) and depths(1) is zero, then:
c          if startvel is valid, reset based on startvel.
c          else if if n >= 2 and times(2) is valid and depths(2) is valid,
c                     reset based on times(2) and depths(2).
c          else do nothing.
c     it is assumed that n >= 1.
c     is is assumed that times(1) and depths(1) were already processed
c          with subroutine velfun_fixup_x.
c     is is assumed that times(2) and depths(2) were already processed
c          with subroutine velfun_fixup_x.

      implicit none
      integer n
      real times(*),depths(*),startvel,fnil

      if (times(1).ne.0..and.depths(1).ne.0.) return
      fnil = -1.0e-30
      if (startvel.eq.fnil.or.startvel.le.0.) return
      if (times(1).eq.0..and.depths(1).ne.0.) then
           times(1)=2.0*depths(1)/startvel
           return
      else if (times(1).ne.0..and.depths(1).eq.0.) then
           depths(1)=0.5*times(1)*startvel
           return
      else if (n.le.1) then
           return
      end if
      if (times(2).eq.0..and.depths(2).ne.0.) then
           times(2)=2.0*depths(2)/startvel
      else if (times(2).ne.0..and.depths(2).eq.0.) then
           depths(2)=0.5*times(2)*startvel
      end if
      return
      end





      subroutine velfun_insert_pick (j,n,nmax,
     $            depths,times,vnmo,vrms,vav,vint,thick)
c     inserts pick into velocity function at location j.
c     j must lie between 1 and n+1.
c     n must be less than nmax.
c     if j or n is out of range, nothing is done.
c     n is incremented by one if successful.
c     if j > 1, the inserted pick is all nils.
c     if j = 1, the inserted pick has unchanged velocities at zero time.

      implicit none
      integer j,n,nmax,i
      real fnil
      real depths(*),times(*),vnmo(*),vrms(*),vav(*),vint(*),thick(*)
 
      if (n.ge.nmax.or.j.lt.1.or.j.gt.n+1) return
      n=max(n,0)
      if (j.le.n) then
           do i=n,j,-1
                depths(i+1)=depths(i)
                times (i+1)=times (i)
                thick (i+1)=thick (i)
                vnmo  (i+1)=vnmo  (i)
                vrms  (i+1)=vrms  (i)
                vav   (i+1)=vav   (i)
                vint  (i+1)=vint  (i)
           end do
      end if
      fnil = -1.0e-30
      if (j.eq.1) then
           depths(j)=0.
           times (j)=0.
           thick (j)=0.
           if (n.eq.0) then
                vnmo(j)=fnil
                vrms(j)=fnil
                vav (j)=fnil
                vint(j)=fnil
           end if
      else
           depths(j)=fnil
           times (j)=fnil
           thick (j)=fnil
           vnmo  (j)=fnil
           vrms  (j)=fnil
           vav   (j)=fnil
           vint  (j)=fnil
      end if
      n=n+1
      return
      end




      subroutine velfun_remove_pick (j,n,
     $            depths,times,vnmo,vrms,vav,vint,thick)
c     removes pick from velocity function at location j.
c     j must lie between 1 and n.
c     n must be greater than 0.
c     if j or n is out of range, nothing is done.
c     n is decremented by one if successful.

      implicit none
      integer j,n,i
      real depths(*),times(*),vnmo(*),vrms(*),vav(*),vint(*),thick(*)
 
      if (n.lt.1.or.j.lt.1.or.j.gt.n) return
      if (j.lt.n) then
           do i=j+1,n
                depths(i-1)=depths(i)
                times (i-1)=times (i)
                thick (i-1)=thick (i)
                vnmo  (i-1)=vnmo  (i)
                vrms  (i-1)=vrms  (i)
                vav   (i-1)=vav   (i)
                vint  (i-1)=vint  (i)
           end do
      end if
      n=n-1
      return
      end




      subroutine velfun_put_pick (type,j,x,v,
     $            depths,times,vnmo,vrms,vav,vint,thick,   msg,ierr)
c     put j-th pick x.v into velocity function, and set the other velocity
C        types to nil.
c     returns ierr=1 if x,v contain any nils.

      implicit none
      character*(*) type
      integer j,ierr
      real x,v,fnil
      real depths(*),times(*),vnmo(*),vrms(*),vav(*),vint(*),thick(*)
      character*(*) msg
 
      fnil = -1.0e-30
      depths(j)=fnil
      times (j)=fnil
      thick (j)=fnil
      vnmo  (j)=fnil
      vrms  (j)=fnil
      vav   (j)=fnil
      vint  (j)=fnil
      if (type(1:2).eq.'VZ') depths(j)=x
      if (type(1:2).eq.'VT') times (j)=x
      if (type(1:2).eq.'VL') thick (j)=x
      if (type(3:4).eq.'NM') vnmo  (j)=v
      if (type(3:4).eq.'RM') vrms  (j)=v
      if (type(3:4).eq.'AV') vav   (j)=v
      if (type(3:4).eq.'IN') vint  (j)=v
      if (type(3:4).eq.'DP') depths(j)=v
      if (x.eq.fnil.or.v.eq.fnil) then
           msg='this pick has nil values'
           ierr=1
      else
           msg=' '
           ierr=0
      end if
      return
      end





      subroutine velfun_get_pick (type,j,x,v,
     $             depths,times,vnmo,vrms,vav,vint,thick,   msg,ierr)
c     get j-th pick x.v from velocity function.
c     returns ierr=1 if x,v contain any nils.

      implicit none
      character*(*) type
      integer j,ierr
      real x,v,fnil
      real depths(*),times(*),vnmo(*),vrms(*),vav(*),vint(*),thick(*)
      character*(*) msg
 
      fnil = -1.0e-30
      x=fnil
      v=fnil
      if (type(1:2).eq.'VZ') x=depths(j)
      if (type(1:2).eq.'VT') x=times (j)
      if (type(1:2).eq.'VL') x=thick (j)
      if (type(3:4).eq.'NM') v=vnmo  (j)
      if (type(3:4).eq.'RM') v=vrms  (j)
      if (type(3:4).eq.'AV') v=vav   (j)
      if (type(3:4).eq.'IN') v=vint  (j)
      if (type(3:4).eq.'DP') v=depths(j)
      if (x.eq.fnil.or.v.eq.fnil) then
           msg='this pick has nil values'
           ierr=1
      else
           msg=' '
           ierr=0
      end if
      return
      end






      subroutine velfun_generate (type,n,
     $            depths,times,vrms,vav,vint,thick,   msg,ierr)

c     Given the type of velocity function, and the values in the
c        corresponding arrays, fills out the rest of the arrays
c        for all other corresponding types.
c     The function does not have to start at zero time.
c     Valid types = VTIN,VTAV,VTRM,VTDP,  VZIN,VZAV,VZRM,  VLIN,VLAV,VLRM.
c     Sets values to nil if they cannot be calculated.
c     Sets ierr = 0 if all is OK.
c     Sets ierr = -1 if n is zero.
c     Sets ierr = -2 if the type is invalid.
c     Sets ierr = first bad index (1 thru n) if some values are set to nil.

      implicit none
      character*(*) type                                 ! input
      integer       n                                    ! input
      real          depths(*),times(*)                ! input or output
      real          vrms(*),vav(*),vint(*),thick(*)   ! input or output
      character*(*) msg                                  ! output
      integer       ierr                                 ! output
      INTEGER       I
      REAL          fnil,delta,dddd,aaaa,bbbb,cccc,abcd

c---------------------------make initial checks and start do loop.

      if (type.ne.'VTRM'.and.type.ne.'VTAV'.and.type.ne.'VTIN'.and.
     $    type.ne.'VZRM'.and.type.ne.'VZAV'.and.type.ne.'VZIN'.and.
     $    type.ne.'VLRM'.and.type.ne.'VLAV'.and.type.ne.'VLIN'.and.
     $    type.ne.'VTDP') then
           msg='INVALID VELOCITY FUNCTION TYPE'
           ierr=-2
           return
      end if
      if (n.le.0) then
           msg='no picks in this velocity function'
           ierr=-1
           return
      end if
      msg=' '
      ierr=0
      fnil = -1.0e-30
      do i=1,n

c-------------------------------check for bad input.

      if (ierr.eq.0) then
           if (type(1:2).eq.'VT'.or.type.eq.'VTDP') then
                if (times (i).eq.fnil.or.times (i).lt.0.) ierr=i
                if (ierr.eq.0.and.i.ge.2) then
                     if (times(i).le.times(i-1)) ierr=i
                end if
           end if
           if (type(1:2).eq.'VZ'.or.type.eq.'VTDP') then
                if (depths(i).eq.fnil.or.depths(i).lt.0.) ierr=i
                if (ierr.eq.0.and.i.ge.2) then
                     if (depths(i).le.depths(i-1)) ierr=i
                end if
           end if
           if (type(1:2).eq.'VL') then
                if (thick (i).eq.fnil.or.thick (i).lt.0.) ierr=i
                if (ierr.eq.0.and.i.ge.2) then
                     if (thick(i).eq.0.) ierr=i
                end if
           end if
           if (type(3:4).eq.'IN') then
                if (vint(i).eq.fnil.or.vint(i).le.0.) ierr=i
           else if (type(3:4).eq.'AV') then
                if (vav (i).eq.fnil.or.vav (i).le.0.) ierr=i
           else if (type(3:4).eq.'RM') then
                if (vrms(i).eq.fnil.or.vrms(i).le.0.) ierr=i
           end if
      end if

c------------------------------set bad pick.

10    if (ierr.ne.0) then
           if (type(1:2).ne.'VT')                    times (i)=fnil
           if (type(1:2).ne.'VZ'.and.type.ne.'VTDP') depths(i)=fnil
           if (type(1:2).ne.'VL')                    thick (i)=fnil
           if (type(3:4).ne.'IN')                    vint  (i)=fnil
           if (type(3:4).ne.'AV')                    vav   (i)=fnil
           if (type(3:4).ne.'RM')                    vrms  (i)=fnil

c------------------------------set first good pick.

      else if (i.eq.1) then
           if (type.eq.'VTDP') then
                if (times(i).gt.0..and.depths(i).gt.0.) then
                     vav(i)=2.0*depths(i)/times(i)
                else if (n.ge.2) then
                     if (times (i+1).gt.0..and.times (i+1).ne.fnil.and.
     $                   depths(i+1).gt.0..and.depths(i+1).ne.fnil) then
                          vav(i)=2.0*depths(i+1)/times(i+1)
                     else
                          ierr=i
                          go to 10
                     end if
                else
                     ierr=i
                     go to 10
                end if
           else if (type(3:4).eq.'IN') then
                vav(i)=vint(i)
           else if (type(3:4).eq.'RM') then
                vav(i)=vrms(i)                          ! now we have vav
           end if
           if (vav(i).le.0.) then
                ierr=i
                go to 10
           end if
           if (type(1:2).eq.'VT'.and.type.ne.'VTDP') then
                depths(i)=0.5*times(i)*vav(i)
           else if (type(1:2).eq.'VL') then
                depths(i)=thick(i)                      ! now we have depth
           end if
           if (depths(i).lt.0.) then
                ierr=i
                go to 10
           end if
           times(i)=2.0*depths(i)/vav(i)
           thick(i)=depths(i)
           vrms (i)=vav   (i)
           vint (i)=vav   (i)

c------------------------------set subsequent good picks.

      else
           if (type(1:2).eq.'VL') then
                depths(i)=depths(i-1)+thick(i)
           end if
           if (type.eq.'VZIN'.or.type.eq.'VLIN') then
                times(i)=times(i-1)+2.*(depths(i)-depths(i-1))/vint(i)
           ELSE IF (TYPE.EQ.'VZAV'.or.type.eq.'VLAV') THEN
                times(i)=2.*depths(i)/vav(i)
           ELSE IF (TYPE.EQ.'VZRM'.or.type.eq.'VLRM') THEN
                DDDD=depths(i)-depths(i-1)
                AAAA=vrms(i)**2
                BBBB=-times(i-1)*(vrms(i)**2+vrms(i-1)**2)/2.
                CCCC=times(i-1)**2*vrms(i-1)**2/4.-DDDD**2
                ABCD=BBBB**2-4.*AAAA*CCCC
                IF (ABCD.GE.0.) THEN
                     times(i)=2.*(-BBBB+SQRT(ABCD))/(2.*AAAA)
                ELSE
                     ierr=i
                     go to 10
                END IF
           END IF                                    ! now we have time
           if (times(i).le.0.) then
                ierr=i
                go to 10
           end if
           if (type.eq.'VTDP') then
                vav(i)=2.*depths(i)/times(i)
           end if
           delta=times(i)-times(i-1)
           IF (TYPE(3:4).EQ.'AV'.or.type.eq.'VTDP') THEN
                VINT(i)=(vav(i)*times(i)-vav(i-1)*times(i-1))/DELTA
           ELSE IF (TYPE(3:4).EQ.'RM') THEN
                ABCD=vrms(i)**2*times(i)-vrms(i-1)**2*times(i-1)
                IF (ABCD.GT.0.) THEN
                     VINT(i)=SQRT(ABCD/DELTA)
                ELSE
                     ierr=i
                     go to 10
                END IF
           END IF                                   ! now we have vint
           if (vint(i).le.0.) then
                ierr=i
                go to 10
           end if
           VRMS(i)=SQRT((VRMS(i-1)**2*times(i-1)+VINT(i)**2*DELTA)
     $                                             /times(i)) 
           VAV(i)=(VAV(i-1)*times(i-1)+VINT(i)*DELTA)/times(i) 
           DEPTHs(i)=DEPTHs(i-1)+VINT(i)*DELTA/2.
           thick(i)=depths(i)-depths(i-1)
           if (vrms  (i).le.0..or.vav  (i).le.0..or.
     $         depths(i).le.0..or.thick(i).le.0.) then
                ierr=i
                go to 10
           end if
      end if

c-------------------------------finish do loop and return.

      end do
      if (ierr.ne.0) msg='VELOCITY CONVERSION ERROR'
      return
      end








      SUBROUTINE velfun_update_c (invoke,h_type,n,
     $            muteflag,offmin,offmax,nmute,omute,tmute,
     $            DEPTHS,TIMES,VNMO,VRMS,VAV,VINT,THICK,
     $            offpick,NRAY,ORAY,TRAY,   h_msg,ierr)

      implicit none
      integer       invoke,n,muteflag,nmute              ! input
      integer       h_type(*)                            ! input
      real          offmin,offmax,omute(*),tmute(*)      ! input
      real          depths(*),times(*),vnmo(*)        ! input or output
      real          vrms(*),vav(*),vint(*),thick(*)   ! input or output
      real          offpick(*),oray(200),tray(200)       ! output
      integer       h_msg(*)                             ! output
      integer       ierr,nray                            ! output
      character*8   type
      character*100 msg

      call convert_hh2cc (h_type,0,   type,8)
      call velfun_update (invoke,type,n,
     $            muteflag,offmin,offmax,nmute,omute,tmute,
     $            DEPTHS,TIMES,VNMO,VRMS,VAV,VINT,THICK,
     $            offpick,NRAY,ORAY,TRAY,   msg,ierr)
      call convert_cc2hh (msg,0,   h_msg,0)
      return
      end





      SUBROUTINE velfun_update (invoke,type,n,
     $            muteflag,offmin,offmax,nmute,omute,tmute,
     $            DEPTHS,TIMES,VNMO,VRMS,VAV,VINT,THICK,
     $            offpick,NRAY,ORAY,TRAY,   msg,ierr)

c     Given the type of velocity function, and the values in the
c        corresponding arrays, fills out the rest of the arrays
c        for all other corresponding types.
c     Valid types = VTIN,VTAV,VTRM,VTDP,
c                   VZIN,VZAV,VZRM,
c                   VLIN,VLAV,VLRM,VTNM.
c     Sets values to nil if they cannot be calculated.
c     Sets ierr = 0 if all is OK.
c     Sets ierr = -1 if n is zero.
c     Sets ierr = -2 if the type is invalid.
c     Sets ierr = -3 if difficult raytracing is not completed.
c     Sets ierr = first bad index (1 thru n) if some values are set to nil.

      implicit none
      integer       invoke,n,muteflag,nmute              ! input
      character*(*) type                                 ! input
      real          offmin,offmax,omute(*),tmute(*)      ! input
      real          depths(*),times(*),vnmo(*)        ! input or output
      real          vrms(*),vav(*),vint(*),thick(*)   ! input or output
      real          offpick(*),oray(200),tray(200)       ! output
      character*(*) msg                                  ! output
      integer       ierr,nray                            ! output
      INTEGER       i,nn,ierr2
      REAL          fnil
      character*80  msg2

c-----------------------------------get started.

      NRAY=0
      if (n.le.0) then
           msg='no picks in this velocity function'
           ierr=-1
           return
      end if
      fnil = -1.0e-30

c-----------------------------do not do ray tracing when type is vnmo.

      if (invoke.eq.0.and.type.eq.'VTNM') then
           do i=1,n    
                vrms(i)=vnmo(i)
                offpick(i)=fnil
           end do
           call velfun_generate ('VTRM',n,
     $            depths,times,vrms,vav,vint,thick,   msg,ierr)

c-----------------------------do not do ray tracing when type is not vnmo.

      else if (invoke.eq.0.and.type.ne.'VTNM') then
           call velfun_generate (type,n,
     $            depths,times,vrms,vav,vint,thick,   msg,ierr)
           do i=1,n    
                vnmo(i)=vrms(i)
                offpick(i)=fnil
           end do
           
c--------------------------do inverse ray tracing when type is vnmo.

      else if (invoke.ne.0.and.type.eq.'VTNM') then
           CALL velfun_inv (n,
     $         muteflag,offmin,offmax,nmute,omute,tmute,
     $         depths,times,vnmo,vrms,vav,vint,thick,
     $         offpick,NRAY,ORAY,TRAY,  msg,ierr)

c--------------------------do forward ray tracing when type is not vnmo.

      else if (invoke.ne.0.and.type.ne.'VTNM') then
           call velfun_generate (type,n,
     $            depths,times,vrms,vav,vint,thick,   msg,ierr)
           do i=1,n    
                vnmo(i)=fnil
                offpick(i)=fnil
           end do
           if (ierr.lt.0) return
           nn=n
           if (ierr.gt.0) nn=ierr-1
           if (nn.eq.0) return
           CALL velfun_nmo (nn,
     $              muteflag,offmin,offmax,nmute,omute,tmute,
     $              depths,times,vnmo,vrms,vint,
     $              offpick,NRAY,ORAY,TRAY,   msg2,ierr2)
           if (ierr2.ne.0.and.ierr.eq.0) then
                msg=msg2
                ierr=ierr2
           end if

c--------------------------cannot get to here.

      else
           msg='cannot get to here'
           ierr=-777
      end if
      return
      end





      SUBROUTINE velfun_inv (n,
     $            muteflag,offmin,offmax,nmute,omute,tmute,
     $            depths,times,vnmo,vrms,vav,vint,thick,
     $            offpick,NRAY,ORAY,TRAY,   msg,ierr)
C     GENERATE RMS VELOCITY FUNCTION FROM NMO VELOCITY FUNCTION.

      implicit none
      integer       n,muteflag,nmute                      ! input
      real          offmin,offmax,omute(*),tmute(*)       ! input
      real          times(n),vnmo(n)                      ! input
      real          depths(n),vrms(n),vav(n),vint(n),thick(n) ! output
      real          offpick(*),oray(200),tray(200)            ! output
      character*(*) msg                                       ! output
      integer       ierr,nray                                 ! output
      integer       i,igood,kount,maxkount,ierr2
      REAL          delta,deltalast,deltamax,factor,cutoff,fnil
      character*80  buffer,msg2
      save          cutoff,maxkount
      data          cutoff,maxkount/0.49,15/

C----------INITIALIZE THE RMS VELOCITY TO THE NMO VELOCITY.

      fnil = -1.0e-30
      msg=' '
      DO i=1,n
           vrms(i)=VNMO(i)
      END DO
      KOUNT=0
      deltamax=999.
      factor=1.
      igood=n

C----------GET ALL TYPES OF VELOCITIES FROM RMS VELOCITY.

5     KOUNT=KOUNT+1
c---------------new stuff follows:
      call velfun_generate ('VTRM',n,
     $            depths,times,vrms,vav,vint,thick,   msg2,ierr2)
c             print *, 'kount=',kount,'  deltamax=',deltamax
      IF (kount.gt.maxkount.or.ierr2.ne.0) go to 999
      IF (deltamax.le.cutoff) return

C----------GET NMO VELOCITY FROM RMS VELOCITY BY RAY TRACING.
ccc    VAV(n) is used temporarily for best-fit VNMO returned from velfun_nmo.

      CALL velfun_nmo (n,
     $              muteflag,offmin,offmax,nmute,omute,tmute,
     $              depths,times,VAV,vrms,vint,
     $              offpick,NRAY,ORAY,TRAY,   msg,ierr)

C----------CHANGE THE RMS VELOCITY.

      deltalast=deltamax
      deltamax=0.
      igood=0
      DO i=1,n
           delta=vnmo(i)-VAV(i)                ! VAV is really the new VNMO.
           vrms(i)=vrms(i)+factor*delta        
           deltamax=max(abs(delta),deltamax)
           if (deltamax.le.cutoff) igood=i
      END DO
      if (deltamax.gt.cutoff.and.deltamax.gt.deltalast) then
           if (factor.lt.0.2) go to 999
           factor=0.5*factor
      end if
      GO TO 5

C----------WE FAILED TO CONVERGE.

999   IF (igood.lt.n) then
           DO i=igood+1,n
                depths(i)=fnil
                vrms  (i)=fnil
                vav   (i)=fnil
                vint  (i)=fnil
                thick (i)=fnil
           END DO
      END IF
      buffer=' '
      write (buffer,1000) kount,nint(deltamax)
1000  format ('FAILED TO CONVERGE AFTER',I3,' ITERATIONS',
     $                                       ' with vel error',I4)
      msg=buffer
      ierr=1
      RETURN
      END




      SUBROUTINE velfun_nmo (N,
     $              muteflag,offmin,offmax,nmute,omute,tmute,
     $              depths,times,vnmo,vrms,vint,
     $              offpick,NRAY,ORAY,TRAY,   msg,ierr)
C     GET STACKING VELOCITY BY RAY TRACING.

      implicit none
      integer       n,muteflag,nmute                    ! input
      real          offmin,offmax,omute(*),tmute(*)     ! input
      real          depths(n),times(n),vrms(n),vint(n)  ! input
      real          vnmo(n)                                 ! output
      real          offpick(n),oray(200),tray(200)          ! output
      character*(*) msg                                     ! output
      integer       ierr,nray                               ! output
      integer       i
      real          omax,terp1

C----------LOOP THRU THE VARIOUS LEVELS.

      ierr=0
      msg=' '
      DO I=1,N

C----------GET MAXIMUM OFFSET FOR THIS LEVEL.

        IF (muteflag.EQ.2) THEN                 ! mute at max offset = depth.
           OMAX=AMIN1(depths(I),offMAX)
        ELSE IF (muteflag.EQ.1.or.nmute.eq.0) THEN      ! do not mute at all.
           OMAX=offMAX
        ELSE IF (times(i).gt.tmute(nmute)) then ! we are below far offset mute.
           omax=offmax
        ELSE IF (times(i).lt.tmute(1)) then     ! we are above near offset mute.
           omax=0.
        ELSE                                ! use offset/time mutes after NMO.
           OMAX=TERP1(times(I),tmute,nmute,omute)
           OMAX=AMIN1(OMAX,offMAX)
        END IF

C----------GET RESULTS FOR THIS LEVEL.

        CALL velfun_curve (I,depths,VINT,offMIN,OMAX,
     $                           vnmo(i),offpick(i),NRAY,ORAY,TRAY)
        if (vnmo(i).eq.0.) VNMO(I)=VRMS(I)
        if (offpick(i).lt.0.) ierr=-3

C----------FINISH UP AND RETURN.

      END DO
      if (ierr.eq.-3) msg='error - difficult raytracing not completed'
      RETURN
      END





      SUBROUTINE velfun_curve (N,DEPTHS,VINT,OFFMIN,OFFMAX,
     $                                vnmo,offpick,NRAY,ORAY,TRAY)
C     GIVEN DEPTHS AND INTERVAL VELOCITIES DOWN TO INTERFACE N,
C         AND GIVEN DESIRED OFFSET RANGE (offmin to offmax),
C     RETURNS ACTUAL OFFSETS oray(nray) AND TIMINGS tray(nray) BEFORE NMO,
C         AND RETURNS LEAST SQUARES STACKING VELOCITY AT INTERFACE N.
c     nray can be 0 thru 200 upon return.
c     oray(1)    = first raytraced offset (>= offmin).
c     oray(nray) = last raytraced offset (>= offmax if successful).
c     returns offpick = 0 if raytracing not attempted.
c     returns offpick = offmax if raytracing successfully completed.
c     returns offpick = -1 if no rays are successfully traced.
c     returns offpick = minus max offset reached if offmax not reached.
c     VNMO is set to zero unless successfully raytraced and curve-fitted.

      implicit none
      integer   nnn,i,k,ierr
      parameter (nnn=200)
      integer   n                                   ! input
      real      DEPTHS(n),VINT(n),offmin,offmax     ! input
      real      vnmo,offpick,ORAY(nnn),TRAY(nnn)        ! output
      integer   nray                                    ! output
      real      step,angle,COEF(2),ECOEF(2),sigma
C----------GET STARTED.
      NRAY=0
      VNMO=0.
      offpick=0.
      IF (n.EQ.1.AND.DEPTHS(1).EQ.0.) RETURN
      if (offmax.le.offmin) return
      step=3.14159/(2*nnn)
C----------GET TIME VERSUS OFFSET FOR THIS REFLECTOR.
      DO 30 i=1,nnn
      ANGLE=(i-1)*step
      K=NRAY+1
      CALL CURVE (DEPTHS,VINT,n,ANGLE,   ORAY(K),TRAY(K),IERR)
      IF (IERR.EQ.1) GO TO 40
      IF (ORAY(K).GE.OFFMIN) NRAY=K
      IF (ORAY(K).GE.OFFMAX) GO TO 40
30    CONTINUE
40    offpick=-1.
      IF (NRAY.EQ.0) RETURN
      offpick=AMAX1(1.,AMIN1(ORAY(NRAY),OFFMAX))
      if (offpick.lt.offmax) offpick=-offpick
      IF (NRAY.EQ.1) RETURN
C----------DO LEAST SQUARES VELOCITY ANALYSIS.
      DO 50 i=1,NRAY
      CALL POLYA (ORAY(i)**2,2,COEF)
50    CALL LEAST (2,COEF,TRAY(i)**2,1.)
      CALL LEAST (0,COEF,ECOEF,SIGMA)
ccc   IF (COEF(1).GT.0.) TNMO=SQRT(COEF(1))     ! best-fit zero-offset time.
      IF (COEF(2).GT.0.) VNMO=1./SQRT(COEF(2))  ! best-fit stacking velocity.
      RETURN
      END





      subroutine velfun_nmo_prep (times,vnmo,n,tmin,tmax,dt,
     $                                    nnn,ttt,vvv,tb,cnst,ierr)
      implicit none
      real times(*),vnmo(*),tmin,tmax,dt,ttt(*),vvv(*),tb(*),cnst
      integer n,nnn,ierr,i
      real dt2,dt22,fnil

      ierr=1
      if (dt.le.0.00001) return
      dt2=1.0/dt
      cnst=1.0-tmin*dt2
      dt22=dt2**2
      fnil = -1.0e-30
      do i=1,n
           if (times(i).eq.fnil) return
           if (vnmo (i).eq.fnil) return
           if (vnmo (i).le.0.01) return
           if (times(i).lt.0.00) return
           ttt(i)=dt22*times(i)**2
           vvv(i)=dt22/vnmo (i)**2
           tb(i)=times(i)*dt2+cnst
      end do
      if(times(n).ge.tmax) then
           nnn=n
      else
           nnn=n+1
           ttt(nnn)=dt22*tmax**2
           vvv(nnn)=dt22/vnmo(n)**2
           tb(nnn)=tmax*dt2+cnst
      end if
      ierr=0
      return
      end




      subroutine velfun_do_nmo (dop,nnn,ttt,vvv,tb,cnst,idir,offset,
     $                                          ta,  a,b,nval,bmute)
      implicit none
      real ttt(*),vvv(*),tb(*),cnst,offset,ta(*),a(*),b(*)
      integer nnn,idir,nval,i
      real dop,off2,bmute

      off2=offset**2
      do i=1,nnn
           ta(i)=sqrt(ttt(i)+off2*vvv(i))+cnst
      end do
      if (idir.gt.0) then
           call dyncc (dop,nnn,ta,tb,nval,a,   b,bmute)
      else
           call dyncc (dop,nnn,tb,ta,nval,a,   b,bmute)
      end if
      return
      end




      subroutine nonhyp_nmo_prep (nhosign,nhoexp,
     $                            times,vnmo,n,tmin,tmax,dt,
     $                                    nnn,ttt,vvv,tb,cnst,ierr)
      implicit none
      real nhosign,nhoexp,nhoexp2
      real times(*),vnmo(*),tmin,tmax,dt,ttt(*),vvv(*),tb(*),cnst
      integer n,nnn,ierr,i
      real dt2,dt22,fnil

      ierr=1
      if (dt.le.0.00001) return
      nhoexp2=max(nhoexp,0.1)
      nhoexp2=min(nhoexp,8.0)
      dt2=1.0/dt
      cnst=1.0-tmin*dt2
      dt22=dt2**2
      fnil = -1.0e-30
      do i=1,n
           if (times(i).eq.fnil) return
           if (vnmo (i).eq.fnil) return
           if (vnmo (i).le.0.01) return
           if (times(i).lt.0.00) return
           ttt(i)=dt22*times(i)**2
c          vvv(i)=dt22/vnmo (i)**2
           vvv(i)=dt22/vnmo (i)**nhoexp2
           tb(i)=times(i)*dt2+cnst
      end do
      if(times(n).ge.tmax) then
           nnn=n
      else
           nnn=n+1
           ttt(nnn)=dt22*tmax**2
c          vvv(nnn)=dt22/vnmo(n)**2
           vvv(nnn)=dt22/vnmo(n)**nhoexp2
           tb(nnn)=tmax*dt2+cnst
      end if
      ierr=0
      return
      end




      subroutine nonhyp_do_nmo (nhosign,nhoexp,
     $                          dop,nnn,ttt,vvv,tb,cnst,idir,offset,
     $                                          ta,  a,b,nval,bmute)
      implicit none
      real nhosign,nhoexp,nhoexp2,offset2
      real ttt(*),vvv(*),tb(*),cnst,offset,ta(*),a(*),b(*)
      integer nnn,idir,nval,i
      real dop,off2,bmute,arg

      nhoexp2=max(nhoexp,0.1)
      nhoexp2=min(nhoexp,8.0)
      offset2=abs(offset)
      if (offset2.lt.1.0) offset2=1.0
c     off2=offset**2
      off2=nhosign*offset2**nhoexp2
      do i=1,nnn
c          ta(i)=sqrt(ttt(i)+off2*vvv(i))+cnst
           arg=ttt(i)+off2*vvv(i)
           if (arg.lt.0.0) arg=0.0
           ta(i)=sqrt(arg)+cnst
      end do
      if (idir.gt.0) then
           call dyncc (dop,nnn,ta,tb,nval,a,   b,bmute)
      else
           call dyncc (dop,nnn,tb,ta,nval,a,   b,bmute)
      end if
      return
      end




      subroutine eta_nmo_prep (times,vnmo,eta,n,tmin,tmax,dt,
     $                                 nnn,ttt,vvv,eee,fff,ggg,
     $                                 tb,cnst,ierr)
      implicit none
      real times(*),vnmo(*),tmin,tmax,dt,ttt(*),vvv(*),tb(*),cnst
      real eta(*)
      real eee(*),fff(*),ggg(*)
      real,parameter :: econstant = 1.2
      integer n,nnn,ierr,i
      real dt2,dt22,fnil,arg,vnmo2,times2,vhor2

      ierr=1
      if (dt.le.0.00001) return
      dt2=1.0/dt
      cnst=1.0-tmin*dt2
      dt22=dt2**2
      fnil = -1.0e-30
      do i=1,n
           if (times(i).eq.fnil) return
           if (vnmo (i).eq.fnil) return
           if (vnmo (i).le.0.01) return
           if (times(i).lt.0.00) return
           times2=times(i)**2
           vnmo2 =vnmo (i)**2
           ttt(i)=dt22*times2
           vvv(i)=dt22/vnmo2
           tb(i)=times(i)*dt2+cnst
           vhor2 = vnmo2 * (1.0 + 2.0 * eta(i))
           eee(i) = dt22 * (vhor2 - vnmo2)
           fff(i) = vnmo2 * times2 * vnmo2 * vnmo2
           ggg(i) = vnmo2 * econstant * vhor2
      end do
      if(times(n).ge.tmax) then
           nnn=n
      else
           nnn=n+1
           times2=tmax   **2
           vnmo2 =vnmo(n)**2
           ttt(nnn)=dt22*times2
           vvv(nnn)=dt22/vnmo2
           tb(nnn)=tmax*dt2+cnst
           vhor2 = vnmo2 * (1.0 + 2.0 * eta(n))
           eee(nnn) = dt22 * (vhor2 - vnmo2)
           fff(nnn) = vnmo2 * times2 * vnmo2 * vnmo2
           ggg(nnn) = vnmo2 * econstant * vhor2
      end if
      ierr=0
      return
      end




      subroutine eta_do_nmo (dop,nnn,ttt,vvv,eee,fff,ggg,
     $                       tb,cnst,idir,offset,
     $                       ta,  a,b,nval,bmute)
      implicit none
      real ttt(*),vvv(*),tb(*),cnst,offset,ta(*),a(*),b(*)
      real eee(*),fff(*),ggg(*)
      integer nnn,idir,nval,i
      real dop,off2,bmute,arg,off4,eterm,eterm1,eterm2

      off2=offset**2
      off4=off2**2
      eterm = 0.0
      do i=1,nnn
           eterm1 = eee(i)*off4
           eterm2 = fff(i) + ggg(i)*off2
           eterm = 0.0
           if (eterm2 /= 0.0) eterm = eterm1 / eterm2
c          ta(i)=sqrt(ttt(i) + off2*vvv(i) - eterm1/eterm2) + cnst
           arg=ttt(i) + off2*vvv(i) - eterm
           if (arg.lt.0.0) arg=0.0
           ta(i)=sqrt(arg) + cnst
      end do
      if (idir.gt.0) then
           call dyncc (dop,nnn,ta,tb,nval,a,   b,bmute)
      else
           call dyncc (dop,nnn,tb,ta,nval,a,   b,bmute)
      end if
      return
      end




