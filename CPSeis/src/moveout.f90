!<CPS_v1 type="PRIMITIVE"/>


!<license>
!-------------------------------------------------------------------------------
! Copyright (c) 2007 ConocoPhillips Company
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.
!-------------------------------------------------------------------------------
!</license>


!<brief_doc>
!------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : MOVEOUT        (normal moveout correction)
! Category   : velocity
! Written    : 2003-10-02   by: Tom Stoeckley
! Revised    : 2007-11-27   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : To apply or remove NMO corrections to seismic data.
! Portability: No known limitations.
!
!------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!------------------------------------------------------------------------------
!                           GENERAL DESCRIPTION
!
! This primitive is used by the NMO process, the Motif program VA, and any
! other program which wants to perform normal moveout corrections on seismic
! data.
!
! This primitive was made from a subset of the NMO process, which was
! converted to the new system by Randy Selzler.  The purpose of splitting
! this primitive out of the NMO process was to call it from both NMO and VA
! (and potentially other locations such as SVA), thereby eliminating duplicate
! code in VA and guaranteeing that the moveout correction is performed
! identically in both places.  Unlike the NMO process, this primitive does
! not read velocity files, interpolate between velocity functions, do FFT
! trace resampling, or modify velocities for GVS or demult.
!
! MOVEOUT has several modes of operation: perform normal moveout correction
! on seismic data, restore normal moveout to seismic data, perform partial
! moveout, and put velocity values into output traces.
!
! Stretch muting is applied in both forward and reverse NMO.  A cosine taper
! (of length 15 samples) is also applied if the top of the trace is muted
! due to excessive stretch or time reversals.
!
! This primitive requires traces to start at zero time.
!
! This primitive uses the DYNSAMP primitive to dynamically resample traces
! using either a linear or a 4-point cubic interpolation.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                             SUBROUTINES
!
! Call to create or delete the object:
!
!                           o    i    i     i       i      o     o
!     call moveout_create (obj, ndpt, dt, action, order, error, msg,
!                          terpmode, doppler, tracemute)
!                             i         i         i   
!
!     call moveout_delete (obj)
!                           b
!
! Call each time the relevant velocity function is changed:
!
!     call moveout_velfun (obj, error, msg, vnmo, vfourth)
!                           b     o     o    i       i
!
! Call for each trace to process:
!
!     call moveout_apply  (obj, offset, offnew, tr, mtop, mbottom)
!                           b     i       i     b    b       b  
!
!-------------------------------------------------------------------------------
!                        SUBROUTINE ARGUMENTS
!
! type(moveout_struct)   obj = pointer to the MOVEOUT object.
! integer               ndpt = number of trace values and velocity picks.
! real                    dt = trace and velocity sample interval (seconds).
! integer             action = NMO action to perform.
! integer              order = normal moveout order parameter.
! logical              error = error flag (true if an error occurred).
! char(len=*)            msg = error message (non-blank if an error occurred).
! integer           terpmode = interpolation option.
! real               doppler = doppler stretch factor.
! logical          tracemute = whether to mute the output trace.
! real            vnmo(ndpt) = NMO velocity picks (seconds).
! real         vfourth(ndpt) = fourth order "velocity" or eta parameter.
! real                offset = offset of trace.
! real                offnew = new offset of trace (if doing partial NMO).
! integer               mtop = head mute index (used and changed).
! integer            mbottom = tail mute index (used and changed).
! real              tr(ndpt) = trace values.
!
!-------------------------------------------------------------------------------
!                    SUBROUTINE ARGUMENT DETAILS
!
! The sample interval for the following arguments is DT:
! The first sample in the following arguments is at time 0:
!
!       TR(NDPT)  VNMO(NDPT)  VFOURTH(NDPT)  ETA(NDPT)
!
! ACTION = MOVEOUT_FORWARD means apply forward NMO correction.
! ACTION = MOVEOUT_REVERSE means apply reverse NMO correction.
! ACTION = MOVEOUT_PARTIAL means apply partial NMO correction.
! ACTION = MOVEOUT_VNMO    means put NMO (stacking) velocities into TR.
! ACTION = MOVEOUT_VINT    means put Dix interval velocities into TR.
!
! OFFNEW is used only when doing partial NMO.
!
! ORDER = MOVEOUT_2           =  normal    2nd order   hyperbolic moveout.
! ORDER = MOVEOUT_4           = residual   4th order   non-hyperbolic moveout.
! ORDER = MOVEOUT_ETA4        = residual 4th order eta non-hyperbolic moveout.
! ORDER = MOVEOUT_ETA         = residual  "exact" eta  non-hyperbolic moveout.
! ORDER = MOVEOUT_2_PLUS_4    = 2nd order plus   4th order   moveout combined.
! ORDER = MOVEOUT_2_PLUS_ETA4 = 2nd order plus 4th order eta moveout combined.
! ORDER = MOVEOUT_2_PLUS_ETA  = 2nd order plus  "exact" eta  moveout combined.
!
! TERPMODE = MOVEOUT_LINEAR means 2-point linear interpolation.
! TERPMODE = MOVEOUT_CUBIC  means 4-point cubic interpolation.
! TERPMODE = MOVEOUT_FFT2   means FFT densification*2 plus linear interpolation.
! TERPMODE = MOVEOUT_FFT4   means FFT densification*4 plus linear interpolation.
! TERPMODE = MOVEOUT_FFT8   means FFT densification*8 plus linear interpolation.
!
! DOPPLER is the maximum stretch factor allowed.
! Larger values of DOPPLER correspond to less severe muting.
! DOPPLER = 1.7 is often used as a default value.
!
!                  stretch  crossing event  refraction  time reversal
!       DOPPLER    muting       muting        muting       muting
!       -------    -------  --------------  ----------  -------------
!        > 1.0       yes         yes           yes          yes
!     0.0 to 1.0     no          yes           yes          yes
!        < 0.0       no          no            no           yes
!
! MTOP and MBOTTOM will first be adjusted up or down by the same amount as
! the corresponding trace sample moves.  Then (for forward moveout correction
! only), MTOP might be further adjusted downward if one or more of the four
! above muting operations are performed.  If TRACEMUTE is true, the top of
! the trace will then be muted down to the doppler mute, and a mute taper
! will be applied.
!
!     ORDER                   VNMO          VFOURTH       ETA
!     ------------            --------      --------      --------
!     MOVEOUT_2               needed        not used      not used
!     MOVEOUT_4               not used      needed        not used
!     MOVEOUT_ETA4            needed        not used      needed      
!     MOVEOUT_ETA             needed        not used      needed      
!     MOVEOUT_2_PLUS_4        needed        needed        not used
!     MOVEOUT_2_PLUS_ETA4     needed        not used      needed      
!     MOVEOUT_2_PLUS_ETA      needed        not used      needed      
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                          REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
!  7. 2007-11-27 Stoeckley  Eliminate use of the memman primitive.
!006. 2006-10-16 D. Glover  Added NULLIFY statements for Intel compiler.
!  5. 2005-01-31 Stoeckley  Add several new fourth-order options including eta.
!  4. 2003-11-03 Stoeckley  Move all muting code from the DYNSAMP primitive
!                            to this primitive, and enhance this muting code;
!                            change arguments from optional to required.
!  3. 2003-10-27 Stoeckley  Add options for doing FFT resampling.
!  2. 2003-10-16 Stoeckley  Documentation changes regarding muting actions.
!  1. 2003-10-02 Stoeckley  Initial version, made from a subset of the NMO
!                            process.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  SECOND AND FOURTH ORDER NORMAL MOVEOUT
!
! ########## ORDER = MOVEOUT_2:
!
!   Second order normal moveout is defined as follows:
!
!         TIME(OFFSET)**2 = TIME(0)**2 + (OFFSET / VNMO)**2
!
! ########## ORDER = MOVEOUT_4:
!
!   Fourth order normal moveout is defined as follows:
!
!         TIME(OFFSET)**2 = TIME(0)**2 - (OFFSET / VFOURTH)**4
!
!   The fourth order residual moveout correction must be applied only after
!   the second order correction has been applied.
!
!   For fourth order moveout, VFOURTH is a parameter governing the size
!   of the fourth order term, analagous to VNMO for the second order term.
!
! ########## ORDER = MOVEOUT_2_PLUS_4:
!
!   Second and fourth order normal moveout combined is defined as follows:
!
!         TIME(OFFSET)**2 = TIME(0)**2 + (OFFSET / VNMO)**2
!                                      - (OFFSET / VFOURTH)**4
!
!-------------------------------------------------------------------------------
!               "EXACT" ANISOTROPIC NORMAL MOVEOUT USING ETA
!
! ########## ORDER = MOVEOUT_2_PLUS_ETA:
!
!   Anisotropic normal moveout based on non-elliptic anisotropy in a VTI
!   medium is defined as follows:
!
!     TIME(OFFSET)**2 = TIME(0)**2 + (OFFSET / VNMO)**2
!
!                         (VHOR**2 - VNMO**2) * OFFSET**4
!         -  ----------------------------------------------------------------
!             VNMO**2 * (TIME(0)**2 * VNMO**4 + CONST * VHOR**2 * OFFSET**2)
!
!   where VHOR**2 = VNMO**2 * (1.0 + 2 * ETA)
!   and ETA specifies the departure from elliptic anisotropy.
!
!   The first part of the above equation is the same as for traditional
!   second order normal moveout.  The last term is the residual anisotropic
!   normal moveout term.
!
!   The last term above can also be written as follows:
!
!                            2 * ETA * OFFSET**4
!      ----------------------------------------------------------------------
!       VNMO**2 * (TIME(0)**2 * VNMO**2 + CONST * (1 + 2 * ETA) * OFFSET**2)
!
! ########## ORDER = MOVEOUT_ETA:
!
!        TIME(OFFSET)**2 = TIME(0)**2 minus the above term.
!
!-------------------------------------------------------------------------------
!                  ANISOTROPIC NORMAL MOVEOUT USING ETA
!                       FOURTH ORDER APPROXIMATION
!
! The last term above can be expanded into a series whose first term is the
! following fourth order term:
!
!                            2 * ETA * OFFSET**4
!                           ----------------------
!                            TIME(0)**2 * VNMO**4
!
! This leads to the following moveout equations:
!
! ########## ORDER = MOVEOUT_ETA4:
!
!        TIME(OFFSET)**2 = TIME(0)**2 minus the above term.
!
! ########## ORDER = MOVEOUT_2_PLUS_ETA4:
!
!   TIME(OFFSET)**2 = TIME(0)**2 + (OFFSET / VNMO)**2 minus the above term.
!
! Therefore, after the second order moveout correction is applied, ETA
! can be determined from the residual fourth order term as follows:
!
!                   OFFSET**4       2 * ETA * OFFSET**4
!                   ----------  =  ----------------------
!                   VFOURTH**4      TIME(0)**2 * VNMO**4
!
! Or:
!                               TIME(0)**2 * VNMO**4
!                      ETA  =  ----------------------
!                                  2 * VFOURTH**4      
!
!-------------------------------------------------------------------------------
!            DEFINITION OF VARIABLES IN THE ABOVE EQUATIONS
!
! TIME(OFFSET) = trace time at specified offset.
! TIME(0)      = corresponding trace time at zero offset.
! OFFSET       = offset of the trace.
! VNMO         = stacking velocity at the corresponding zero offset time.
! VFOURTH      = fourth order parameter at the corresponding zero offset time.
! VHOR         = horizontal velocity at the corresponding zero offset time.
! ETA          = value of eta at the corresponding zero offset time.
! CONST        = a constant normally between 1.0 and 1.2.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                          PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module moveout_module
      use named_constants_module
      use dynsamp_module
      use mth_module
      implicit none
      public

      character(len=100),public :: moveout_ident = &
"$Id: moveout.f90,v 1.7 2007/11/28 14:56:18 Stoeckley beta sps $"


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: moveout_struct
        private

        integer                      :: ndpt     
        real                         :: dt2     
        integer                      :: action 
        integer                      :: order 
        logical                      :: tracemute
        real                         :: squeeze
        logical                      :: do_stretch_muting
        logical                      :: do_time_reversal_muting
        logical                      :: do_refraction_muting
        logical                      :: do_crossing_event_muting
        logical                      :: two
        integer                      :: four
        type(dynsamp_struct),pointer :: dynsamp
        real                ,pointer :: tzeroterm(:)
        real                ,pointer :: velterm  (:)
        real                ,pointer :: smallterm(:)
        real                ,pointer :: small2   (:)
        real                ,pointer :: small3   (:)

      end type moveout_struct


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      integer,parameter,public :: MOVEOUT_FORWARD = 1               ! action.
      integer,parameter,public :: MOVEOUT_REVERSE = 2               ! action.
      integer,parameter,public :: MOVEOUT_PARTIAL = 3               ! action.
      integer,parameter,public :: MOVEOUT_VNMO    = 4               ! action.
      integer,parameter,public :: MOVEOUT_VINT    = 5               ! action.

      integer,parameter,public :: MOVEOUT_2           = 1           ! order.
      integer,parameter,public :: MOVEOUT_4           = 2           ! order.
      integer,parameter,public :: MOVEOUT_ETA4        = 3           ! order.
      integer,parameter,public :: MOVEOUT_ETA         = 4           ! order.
      integer,parameter,public :: MOVEOUT_2_PLUS_4    = 5           ! order.
      integer,parameter,public :: MOVEOUT_2_PLUS_ETA4 = 6           ! order.
      integer,parameter,public :: MOVEOUT_2_PLUS_ETA  = 7           ! order.

      integer,parameter,public :: MOVEOUT_CUBIC   = DYNSAMP_CUBIC   ! terpmode.
      integer,parameter,public :: MOVEOUT_LINEAR  = DYNSAMP_LINEAR  ! terpmode.
      integer,parameter,public :: MOVEOUT_FFT2    = DYNSAMP_FFT2    ! terpmode.
      integer,parameter,public :: MOVEOUT_FFT4    = DYNSAMP_FFT4    ! terpmode.
      integer,parameter,public :: MOVEOUT_FFT8    = DYNSAMP_FFT8    ! terpmode.

      integer,parameter,private :: M_NONE = 1     ! four.
      integer,parameter,private :: M_4    = 2     ! four.
      integer,parameter,private :: M_ETA4 = 3     ! four.
      integer,parameter,private :: M_ETA  = 4     ! four.

      integer,parameter,private :: TAPER_LENGTH = 15
      real             ,private :: taper(TAPER_LENGTH)

      contains


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine moveout_delete (obj)

      type(moveout_struct),pointer :: obj       ! arguments

      if (.not.associated(obj)) return

      call dynsamp_delete (obj%dynsamp)

      if (associated(obj%tzeroterm)) deallocate (obj%tzeroterm)
      if (associated(obj%velterm  )) deallocate (obj%velterm)
      if (associated(obj%smallterm)) deallocate (obj%smallterm)
      if (associated(obj%small2   )) deallocate (obj%small2)
      if (associated(obj%small3   )) deallocate (obj%small3)

      deallocate(obj)

      end subroutine moveout_delete


!!----------------------------- create ----------------------------------!!
!!----------------------------- create ----------------------------------!!
!!----------------------------- create ----------------------------------!!


      subroutine moveout_create (obj,ndpt,dt,action,order,error,msg, &
                                 terpmode,doppler,tracemute)

      type(moveout_struct),pointer       :: obj                   ! arguments
      integer             ,intent(in)    :: ndpt                  ! arguments
      real                ,intent(in)    :: dt                    ! arguments
      integer             ,intent(in)    :: action                ! arguments
      integer             ,intent(in)    :: order                 ! arguments
      logical             ,intent(out)   :: error                 ! arguments
      character(len=*)    ,intent(out)   :: msg                   ! arguments
      integer             ,intent(in)    :: terpmode              ! arguments
      real                ,intent(in)    :: doppler               ! arguments
      logical             ,intent(in)    :: tracemute             ! arguments
      integer                            :: ier1,ier2,ier3,indx   ! local
      integer                            :: ier4,ier5             ! local
      real                               :: factor                ! local

      !************************************************
      !**** allocate and initialize data structure ****
      !************************************************

      allocate (obj)

      obj%ndpt                     = ndpt 
      obj%dt2                      = dt**2
      obj%action                   = action 
      obj%order                    = order
      obj%tracemute                = tracemute
      obj%squeeze                  = 0.0
      obj%do_stretch_muting        = (doppler >  1.0)
      obj%do_refraction_muting     = (doppler >= 0.0)
      obj%do_crossing_event_muting = (doppler >= 0.0)
      obj%do_time_reversal_muting  = .true.
      obj%two                      = .false.
      obj%four                     = M_NONE

      if (doppler > 1.0) obj%squeeze = 1.0 / doppler

      nullify (obj%dynsamp) ! jpa
      nullify (obj%tzeroterm) ! jpa
      nullify (obj%velterm) ! jpa
      nullify (obj%smallterm) ! jpa
      nullify (obj%small2) ! jpa
      nullify (obj%small3) ! jpa

      !*************************************
      !**** validate input parameters ******
      !*************************************

      select case (obj%action)
           case (MOVEOUT_FORWARD) ; continue
           case (MOVEOUT_REVERSE) ; continue
           case (MOVEOUT_VNMO   ) ; continue
           case (MOVEOUT_VINT   ) ; continue
           case (MOVEOUT_PARTIAL) ; continue
           case default
                           error = .true.
                           msg = 'illegal value for ACTION'
                           return
      end select

      select case (obj%order)
           case (MOVEOUT_2          ) ; obj%two = .true.  ; obj%four = M_NONE
           case (MOVEOUT_4          ) ; obj%two = .false. ; obj%four = M_4
           case (MOVEOUT_ETA4       ) ; obj%two = .false. ; obj%four = M_ETA4
           case (MOVEOUT_ETA        ) ; obj%two = .false. ; obj%four = M_ETA
           case (MOVEOUT_2_PLUS_4   ) ; obj%two = .true.  ; obj%four = M_4
           case (MOVEOUT_2_PLUS_ETA4) ; obj%two = .true.  ; obj%four = M_ETA4
           case (MOVEOUT_2_PLUS_ETA ) ; obj%two = .true.  ; obj%four = M_ETA
           case default
                           error = .true.
                           msg = 'illegal value for ORDER'
                           return
      end select

      if (obj%order /= MOVEOUT_2) then
           if (obj%action == MOVEOUT_PARTIAL) then
                error = .true.
                msg   = 'PARTIAL NON-HYPERBOLIC NMO NOT AVAILABLE'
                return
           else if(obj%action == MOVEOUT_VNMO .and. obj%order /= MOVEOUT_4) then
                error = .true.
                msg   = 'ORDER must be 2 or 4 when requesting ACTION = VNMO'
                return
           else if(obj%action == MOVEOUT_VINT) then
                error = .true.
                msg   = 'ORDER must be 2 when requesting ACTION = VINT'
                return
           end if
      end if

      !***************************
      !**** Allocate memory ******
      !***************************

      allocate (obj%tzeroterm (obj%ndpt), stat=ier1)
      allocate (obj%velterm   (obj%ndpt), stat=ier2)
      allocate (obj%smallterm (obj%ndpt), stat=ier3)
      allocate (obj%small2    (obj%ndpt), stat=ier4)
      allocate (obj%small3    (obj%ndpt), stat=ier5)

      if (ier1 /= 0 .or. ier2 /= 0 .or. ier3 /= 0 .or. &
          ier4 /= 0 .or. ier5 /= 0) then
           error = .true.
           msg   = 'error allocating MOVEOUT memory'
           return
      end if

      !*************************************
      !**** Make table of index values *****
      !**** TZEROTERM = TZERO**2       *****
      !*************************************

      do indx = 1,obj%ndpt
           obj%tzeroterm(indx) = (indx - 1)**2 * obj%dt2
           obj%velterm  (indx) = 0.0
           obj%smallterm(indx) = 0.0
           obj%small2   (indx) = 0.0
           obj%small3   (indx) = 0.0
      end do

      !********************************
      !**** Create dynsamp object *****
      !********************************

      call dynsamp_create (obj%dynsamp,ndpt,terpmode,error,msg)

      if (.not.error) msg = 'moveout object successfully created'

      !***************************************
      !**** Compute cosine taper for mute ****
      !***************************************

      factor = (0.5*PI)/TAPER_LENGTH

      do indx = 1,TAPER_LENGTH
        taper(indx) = sin((indx - 1)*factor)**2
      end do

      end subroutine moveout_create


!!---------------------------- velfun --------------------------------------!!
!!---------------------------- velfun --------------------------------------!!
!!---------------------------- velfun --------------------------------------!!


      subroutine moveout_velfun (obj,error,msg,vnmo,vfourth)

      type(moveout_struct),intent(inout) :: obj                  ! arguments
      logical             ,intent(out)   :: error                ! arguments
      character(len=*)    ,intent(out)   :: msg                  ! arguments
      real                ,intent(in)    :: vnmo(:)              ! arguments
      real                ,intent(in)    :: vfourth(:)           ! arguments
      integer                            :: indx                 ! local
      real                               :: vrms2,vprev2,vint2   ! local

      error = .false.
      msg   = ' '

      !***************************************************************
      !***    At this point, VNMO contains stacking velocities     ***
      !***          sampled at DT starting at time zero.           ***
      !***************************************************************
      !***      At this point, VFOURTH contains fourth order       ***
      !***                 "velocity" parameter                    ***
      !***          sampled at DT starting at time zero.           ***
      !***************************************************************
      !***       At this point, ETA contains ETA parameter         ***
      !***          sampled at DT starting at time zero.           ***
      !***************************************************************
      !***        Now we will calculate VELTERM from VNMO.         ***
      !***************************************************************

      if (obj%action == MOVEOUT_VNMO) then                     ! needs VNMO.

          !******************************************************
          !****    VNMO = stacking velocity or anything else ****
          !**** VELTERM = stacking velocity or anything else ****
          !******************************************************

          do indx = 1,obj%ndpt
            obj%velterm(indx) = vnmo(indx)
          end do
          return           ! do nothing more here.

      else if (obj%action == MOVEOUT_VINT) then                ! needs VNMO.

          !**************************************
          !****    VNMO = stacking velocity  ****
          !**** VELTERM = interval velocity  ****
          !**************************************

          vrms2          = vnmo(1)**2
          obj%velterm(1) = vnmo(1)

          do indx = 2, obj%ndpt
            vprev2            = vrms2
            vrms2             = vnmo(indx)**2
            vint2             = vrms2 * indx - vprev2 * (indx-1)
            if (vint2 < 0.0) vint2 = 0.0
            obj%velterm(indx) = sqrt(vint2)
          end do
          return           ! do nothing more here.

      else if (obj%two) then                                   ! needs VNMO.

          !*************************************
          !****    VNMO = stacking velocity ****
          !**** VELTERM = 1 / VNMO**2       ****
          !*************************************

          do indx = 1,obj%ndpt
            if (vnmo(indx) <= 0.0) then
                 error = .true.
                 msg   = 'zero or negative stacking velocity encountered'
                 return
            end if
            obj%velterm(indx) = 1.0 / vnmo(indx)**2
          end do
      end if

      !***************************************************************
      !***  Now we will calculate SMALLTERM from VFOURTH or ETA.   ***
      !***         Now we will calculate SMALL2 from ETA.          ***
      !***         Now we will calculate SMALL3 from ETA.          ***
      !***************************************************************

      if (obj%four == M_4) then                             ! needs VFOURTH.

          !********************************************************
          !****   VFOURTH = fourth order "velocity" parameter  ****
          !**** SMALLTERM = -1 / VFOURTH**4                    ****
          !********************************************************

          do indx = 1,obj%ndpt
            if (vfourth(indx) <= 0.0) then
                 error = .true.
                 msg   = 'zero or negative 4th order velocity encountered'
                 return
            end if
            obj%smallterm(indx) = -1.0 / vfourth(indx)**4
          end do

      else if (obj%four == M_ETA4) then            ! needs VNMO and VFOURTH.

          !*****************************************************
          !****      VNMO = stacking velocity               ****
          !****   VFOURTH = eta parameter = ETA             ****
          !**** TZEROTERM = TZERO**2                        ****
          !**** SMALLTERM = -2 * ETA / TZERO**2 / VNMO**4   ****
          !*****************************************************
          ! indices start at 2 since tzeroterm(1) == 0 in denominator.

          do indx = 2,obj%ndpt
            if (vnmo(indx) <= 0.0) then
                 error = .true.
                 msg   = 'zero or negative stacking velocity encountered'
                 return
            end if
            if (vfourth(indx) <= -0.5) then
                 error = .true.
                 msg   = 'too small ETA encountered - ETA must be > -0.5'
                 return
            end if
            obj%smallterm(indx) = -2.0 * vfourth(indx) / &
                             (obj%tzeroterm(indx) * vnmo(indx)**4)
          end do

      else if (obj%four == M_ETA) then             ! needs VNMO and VFOURTH.

          !**********************************************
          !****      VNMO = stacking velocity        ****
          !****   VFOURTH = eta parameter = ETA      ****
          !**** TZEROTERM = TZERO**2                 ****
          !**** SMALLTERM = -2 * ETA                 ****
          !****    SMALL2 = VNMO**4 * TZERO**2       ****
          !****    SMALL3 = VNMO**2 * (1 + 2 * ETA)  ****
          !**********************************************

          do indx = 1,obj%ndpt
            if (vnmo(indx) <= 0.0) then
                 error = .true.
                 msg   = 'zero or negative stacking velocity encountered'
                 return
            end if
            if (vfourth(indx) <= -0.5) then
                 error = .true.
                 msg   = 'too small ETA encountered - ETA must be > -0.5'
                 return
            end if
            obj%smallterm(indx) = -2.0 * vfourth(indx)
            obj%small2   (indx) = vnmo(indx)**4 * obj%tzeroterm(indx)
            obj%small3   (indx) = vnmo(indx)**2 * (1.0 + 2.0 * vfourth(indx))
          end do

      end if

      end subroutine moveout_velfun


!!--------------------------- apply ----------------------------------------!!
!!--------------------------- apply ----------------------------------------!!
!!--------------------------- apply ----------------------------------------!!


      subroutine moveout_apply (obj,offset,offnew,tr,mtop,mbottom)

      type(moveout_struct),intent(in)    :: obj                     ! arguments
      real                ,intent(in)    :: offset,offnew           ! arguments
      real                ,intent(inout) :: tr(:)                   ! arguments
      integer             ,intent(inout) :: mtop,mbottom            ! arguments
      logical                            :: forward                 ! local
      real                               :: exact   (obj%ndpt)      ! local
      real                               :: timeterm(obj%ndpt)      ! local
      real                               :: trkeep  (obj%ndpt)      ! local
      real                               :: offset2,offnew2,offset4 ! local
      real                               :: offterm,fracdiff        ! local
      integer                            :: indx,mtaper             ! local
      integer                            :: mdoppler,mtest,mkeep    ! local
      integer                            :: istart,indx_of_exact    ! local
      real                               :: starting,value_of_exact ! local

      !******************************************************
      !**** Store velocities in the trace                ****
      !**** ACTION is MOVEOUT_VNMO or MOVEOUT_VINT       ****
      !**** VELTERM = stacking velocity or anything else ****
      !**** VELTERM = interval velocity                  ****
      !******************************************************

      if (obj%action == MOVEOUT_VNMO .or. obj%action == MOVEOUT_VINT) then
           tr(1:obj%ndpt) = obj%velterm(1:obj%ndpt)
           mtop    = 1                                ! set to no mute.
           mbottom = obj%ndpt                         ! set to no mute.
           return                                     ! do nothing more here.
      end if

      !*************************************************************
      !**** Compute the moveout function                        ****
      !**** Calculate TIMETERM                                  ****
      !*************************************************************
      !****     TZEROTERM = TZERO**2                            ****
      !*************************************************************
      !****      TIMETERM = TIME**2                             ****
      !*************************************************************
      !****      TIMETERM = TZEROTERM   + OFFSET**2 * VELTERM   ****
      !****       TIME**2 = TZERO**2    + OFFSET**2 / VNMO**2   ****
      !*************************************************************

      timeterm(1:obj%ndpt) = obj%tzeroterm(1:obj%ndpt)

      if (obj%two) then

          !********************************
          !**** VELTERM = 1 / VNMO**2  ****
          !********************************

          offset2 = offset**2
          offnew2 = offnew**2

          if (obj%action /= MOVEOUT_PARTIAL) then      ! FORWARD or REVERSE.
               offterm = offset2
          else if (offset2 > offnew2) then             ! PARTIAL forward.
               offterm = offset2 - offnew2
          else                                         ! PARTIAL reverse.
               offterm = offnew2 - offset2
          end if

          do indx = 1, obj%ndpt
               timeterm(indx) = timeterm(indx) + offterm*obj%velterm(indx)
          end do

      end if

      if (obj%four == M_4 .or. obj%four == M_ETA4) then

          !****************************************************
          !**** SMALLTERM = -1 / VFOURTH**4                ****
          !**** SMALLTERM = -2 * ETA / TZERO**2 / VNMO**4  ****
          !****************************************************

          offset4 = offset**4
          do indx = 1, obj%ndpt
               timeterm(indx) = timeterm(indx) + offset4*obj%smallterm(indx)
          end do

      else if (obj%four == M_ETA) then

          !**********************************************
          !**** SMALLTERM = -2 * ETA                 ****
          !****    SMALL2 = VNMO**4 * TZERO**2       ****
          !****    SMALL3 = VNMO**2 * (1 + 2 * ETA)  ****
          !**********************************************
          ! index starts at 2 because small2(1) is zero in denominator.

          offset4 = offset**4
          offset2 = offset**2
          do indx = 2, obj%ndpt
               timeterm(indx) = timeterm(indx) + offset4*obj%smallterm(indx) &
                           / (obj%small2(indx) + offset2*obj%small3(indx))
          end do

      end if

      !****************************************
      !**** Move out the trace             ****
      !**** Time reversals are muted       ****
      !**** Calculate EXACT moveout index  ****
      !**** Uses TIMETERM (computed above) ****
      !**** Changes TR                     ****
      !**** Changes MTOP and MBOTTOM       ****
      !****************************************

      do indx = 1, obj%ndpt
           if (timeterm(indx) < 0.0) timeterm(indx) = 0.0
           exact(indx) = 1.0 + sqrt(timeterm(indx) / obj%dt2)
      end do

      mkeep = mtop                             ! for crossing event muting.
      call mth_constrain (mkeep,1,obj%ndpt)    ! for crossing event muting.

      forward = (obj%action == MOVEOUT_FORWARD .or. &
                   (obj%action == MOVEOUT_PARTIAL .and. offset > offnew))

      trkeep(:obj%ndpt) = tr(:obj%ndpt)

      if (forward) then
           call dynsamp_forward (obj%dynsamp, exact, trkeep, tr, mtop, mbottom)
      else
           call dynsamp_reverse (obj%dynsamp, exact, trkeep, tr, mtop, mbottom)
      end if

      !******************************************
      !**** Calculate the mute               ****
      !**** Uses EXACT      (computed above) ****
      !**** Uses TIMETERM   (computed above) ****
      !**** Uses VELTERM(1) (computed above) ****
      !**** Uses OFFTERM    (computed above) ****
      !**** Calculates MDOPPLER              ****
      !**** Changes MTOP                     ****
      !******************************************

      if (.not.forward) return

      mdoppler = 1

      if (obj%do_stretch_muting .or. obj%do_time_reversal_muting) then
           mtest = 1
           do indx = obj%ndpt, 2, -1
               if (exact(indx) - exact(indx-1) <= obj%squeeze) then
                    mtest = indx
                    exit
               end if
           end do
           mdoppler = max(mdoppler,mtest)
      end if

      if (obj%do_refraction_muting .and. obj%two) then
           mtest = 1
           do indx = obj%ndpt, 2, -1
               fracdiff = (timeterm(indx) - timeterm(indx-1)) / obj%dt2
               if (fracdiff <= indx - 1.5) then
                    mtest = indx
                    exit
               end if
           end do
           mdoppler = max(mdoppler,mtest)
      end if

      if (obj%do_crossing_event_muting .and. obj%two) then
           if (forward) then
                fracdiff = offterm * obj%velterm(1) / obj%dt2
                starting = max(0.0, (mkeep - 1.0)**2 - fracdiff)
                istart   = max(nint(1.0 + sqrt(starting)),1)
           else
                istart   = mkeep
           end if
           if (istart < mdoppler) then
             indx_of_exact  = mth_ismax &
                                (mdoppler-istart,exact(istart:mdoppler-1),1)
             value_of_exact = exact(istart + indx_of_exact-1)
             mtest = obj%ndpt
             do indx = mdoppler, obj%ndpt
                 if (exact(indx) > value_of_exact) then
                      mtest = indx
                      exit
                 end if
             end do
             mdoppler = max(mdoppler,mtest)
           end if
      end if

      mtop = max(mtop,mdoppler)

      !*****************************************
      !**** Apply the mute (with a taper)   ****
      !**** Uses MDOPPLER (computed above)  ****
      !**** Changes TR                      ****
      !*****************************************

      if (obj%tracemute .and. mdoppler > 1) then
           tr(1:mdoppler-1) = 0.0
           mtaper = min(mdoppler+TAPER_LENGTH-1, obj%ndpt)
           do indx = mdoppler,mtaper
                tr(indx) = tr(indx) * taper(indx-mdoppler+1)
           end do
      end if

      end subroutine moveout_apply


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module moveout_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

