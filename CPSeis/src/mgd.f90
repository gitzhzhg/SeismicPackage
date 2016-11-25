!<CPS_v1 type="PROCESS"/>
!!------------------------------- mgd.f90 ---------------------------------!!
!!------------------------------- mgd.f90 ---------------------------------!!
!!------------------------------- mgd.f90 ---------------------------------!!

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
!-------------------------------------------------------------------------------
!                         C P S   P R O C E S S
!
! Name       : MGD          (Marine Geometry Description)
! Category   : headers
! Written    : 1989-02-21   by: Chuck I. Burch
! Revised    : 2006-09-11   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Generate and apply headers for 2D marine lines with simple
!               acquisition geometry.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! MGD builds headers for 2D marine datafor the simple case in which the source
! increment is either the (receiver increment)/4.0 or a whole number multiple
! of (receiver increment)/2.0.  It is expected that MGD can be used for 90%
! or more of 2D marine data.
!
! MGD fills in skipped profiles with dead traces.  These dead traces have
! correct CPS headers.
!
!                           Coordinate Systems
!
! MGD sets header values according to the following conventions:
!
!     1. Grid coordinate is sequential CMP number starting with 1 at the
!        location of the first midpoint on the line.
!
!     2. Surveyed coordinate origin (zero value) is the location of the far
!        receiver of the first profile.
!
!     3. Sequential ground position starts with 1 at the location of the far
!        receiver of the first profile.
!
!
!                        Implied Grid Transformation
!
! MGD coordinate system conventions are independent of the grid transform set
! in Project Data Screen.  The grid transform implied by the action of MGD is:
!
!                          DX(1,1) = REC_INC/2.0
!                   ORIGIN_INL = (far offset - REC_INC)/2.0
!
! If it is necessary to use the grid transform in subsequent jobs, these values
! should be used.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! SP_INIT, SP_INC and ANT_CORR are used only to determine the shotpoint header
! value (header word 37).  (A shotpoint is a surveyed location or flag position.
! It is not necessarily a source location.)
!
! MGD requires single trace input.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
! This process requires traces to be input one at a time.
! Process requires traces to be input in shot profile sort order.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process only applies CPS header values.
! This process outputs the same traces as it receives.
! This process outputs one trace at a time.
! This is a trace-supplying process (filling in skipped profiles).
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NWIH     number of words in trace header       used but not changed
! NDPT     number of trace sample values         used but not changed
!
! (See note on grid transform in General Description.)
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description    Action taken
! ----    -----------    ------------
!                        All headers are set except user-defined headers
!                        (48-55), headers above 64, the LAV header (25),
!                        and the mute headers (2 and 64), which are passed
!                        through unchanged.  (The LAV header is reset to
!                        zero if the trace is killed.)
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date       Author     Description
!     ---------- ---------  -----------
! 27. 2006-09-11 Stoeckley  Add call to pc_register_array_names for SeisSpace.
! 26. 2001-08-01 Stoeckley  Set user defined headers to zero for dead fill
!                            traces.
! 25. 2001-06-04 Stoeckley  Add setting of header word 29 (receiver shotpoint).
! 24. 2001-02-13 Stoeckley  Pass through trace headers above 64, and the mute
!                            headers 2 and 64, unchanged.
! 23. 2000-12-08 Stoeckley  Change wrapup flag.
! 22. 2000-09-15 Stoeckley  Change several INT functions to NINT (including
!                            antanna correction calculation); eliminate call
!                            to STRING_TO_UPPER in a trap.
! 21. 2000-08-24 Stoeckley  Move the CPS_v1 tag to an acceptable location;
!                            add missing </execute_only> flag.
! 20. 2000-04-07 Stoeckley  Add GUI definition section and bring SPECIFIC
!                            CALLING documentation up to standards.
! 19. 2000-03-10 Stoeckley  Change the array traps for SP_DEPTH and DEPTHS
!                            to be called as arrayset traps; add additional
!                            checks in SRC_INC trap; change call to CHART
!                            to give it the correct number of grid points.
! 18. 2000-02-15 Stoeckley  Improve detection and error reporting of missing
!                            globals.
! 17. 2000-02-07 Stoeckley  Allow 0.0 for antenna correction; fix traps for
!                            ant_corr, sp_depths, and depths; do not set
!                            mute headers to NDPT+1 for dead traces; improve
!                            error messages.
! 16. 2000-02-04 Stoeckley  Improved use of wrapup flag to keep from executing
!                            wrapup code from front end.
! 15. 2000-01-24 Stoeckley  Finish conversion of this process.
! 14. 1999-10-22 Dorman     Convert process to conform to new CPS standards
!                            and Fortran 90.
! 13. 1998-11-10 Vunderink  Begin using the f90 compiler.
! 12. 1997-03-12 Vunderink  Added ISKP parameter and fixed so that SIN
!                            can be any whole number multiple of (RIN/2)
! 11. 1996-11-20 Cooper     Increase array sizes from 250 to 1000
!                            (SKIP,WDSP,WDNT)
! 10. 1993-03-16 Goodger    Make correction with 'L format to satisfy
!                            5.04 compiler.
! 9.  1992-03-20 Troutt     Set tail mute (hw64) to NDPT.
!                            Note that MGD, unlike FGD, does NOT pass input
!                            mute indices.
! 7.  1991-08-19 Burch      Added logic for SIN = RIN/4.
! 6.  1991-02-15 Burch      Modified so user defined headers (48-55)
!                            are passed through.
! 5.  1990-09-19 Howard     Set inverse of rotation matrix in GLOBALS.
! 4.  1989-12-12 Burch      LAV passed from input - not calculated.
! 3.  1989-10-09 Burch      Loop splitting logic added to allow skipped
!                            profiles to be filled with dead traces.
! 2.  1989-10-05 Burch      Changed scratch allocation in set-up.
! 1.  1989-09-21 Burch      Original Code
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS
!
! No known limitations.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                    SPECIFIC CALLING CHARACTERISTICS
! Control
! Parameter     Value
! Name          Reported   Description
! ----          --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST   true      whether this process ever needs to request traces.
! NEED_LABEL     true      whether this process needs a label.
! TWOSETS        true      whether this process needs two trace/header arrays.
! NSCRATCH        >0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more input traces.
!    NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<int_calling_doc>
!-------------------------------------------------------------------------------
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!
!-------------------------------------------------------------------------------
!</int_calling_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                  ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!  The following flags are used in the logic that controls alternate
!  return and filling skipped profiles with dead traces:
!
!        INUSED  =  .false.  Current input trace not yet used.
!                =  .true.   Current input trace has been used.
!        IFILL   =  .false.  Current profile is present.
!                =  .true.   Current profile is skipped.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!<NS MGD Process/NC=80>
!
!                Marine Geometry Description
!
!     NUM_CHANNELS=`III              OFF_NEAR=`FFFFFFFFFFF
!
!     REC_INC=~~~~~`FFFFFFFFFFF      SRC_INC= `FFFFFFFFFFF
!
!     ORDER=~~~~~~~`CCCC             NPTB=~~~~`IIIIIIIIII
!
!     SP_INIT=~~~~~`FFFFFFFFFFF      SP_INC=~~`FFFFFFFFFFF
!
!     SKIP_INIT=~~~`IIIIIIIII
!
!     ANT_CORR=~~~~`FFFFFFFFFFF      CHART=~~~`CCCCC
!
!     Shot Point                     Shot Point Water Depth
!     PROF_SKIP                      SP_DEPTH    DEPTHS
!     `FFFFFFFFFFF                   `FFFFFFFFFFF`FFFFFFFFFFF
!     `FFFFFFFFFFF                   `FFFFFFFFFFF`FFFFFFFFFFF
!     `FFFFFFFFFFF                   `FFFFFFFFFFF`FFFFFFFFFFF
!     `FFFFFFFFFFF                   `FFFFFFFFFFF`FFFFFFFFFFF
!
!<PARMS PROF_SKIP        [/XST/YST]>
!<PARMS SP_DEPTH_ARRAYSET[/XST/YST]>
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="NUM_CHANNELS">
!<Tip> Number of channels (receiver groups) in streamer. </Tip>
! Default = 320
! Allowed = 1 - 9999
!</Help>
!
!<Help KEYWORD="OFF_NEAR">
!<Tip> Near offset. </Tip>
! Default = 300
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="REC_INC">
!<Tip> Receiver increment (distance between receiver groups in streamer). </Tip>
! Default = 12.5
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="SRC_INC">
!<Tip> Source increment (distance between adjacent source locations). </Tip>
! Default = 12.5
! Allowed = real > 0.0
! SRC_INC must be either REC_INC/4.0 or a whole number multiple of REC_INC/2.0.
!</Help>
!
!<Help KEYWORD="ORDER">
!<Tip> First trace in shot profile is FAR or NEAR channel. </Tip>
! Default = FAR
! Allowed = FAR  (First trace in shot profile is FAR channel.)
! Allowed = NEAR (First trace in shot profile is NEAR channel.)
!</Help>
!
!<Help KEYWORD="NPTB">
!<Tip> Number of shot Profiles To Build (including skips). </Tip>
! Default = 1000
! Allowed = int > 0
! MGD will abort if it receives more traces than NPTB profiles.  There is no
! penalty for setting NPTB larger than the actual number of input profiles.
!</Help>
!
!<Help KEYWORD="SKIP_INIT">
!<Tip> Number of trace headers to skip initially. </Tip>
! Default = 0
! Allowed = int >= 0
! If SKIP_INIT > 0, then MGD will act as if it had read SKIP_INIT imaginary
! traces (and built headers for them) prior to reading the first actual trace.
! MGD will then treat the first actual trace as if it were the SKIP_INIT + 1st
! trace.
!
! If SKIP_INIT = 0, then MGD will treat the first trace read as the first trace
! in the line.
!</Help>
!
!<Help KEYWORD="SP_INIT">
!<Tip> Shotpoint annotation at antenna position of first source. </Tip>
! Default = 101
! Allowed = real
! SP_INIT, SP_INC and ANT_CORR are used only to determine the shotpoint header
! value (header word 37).
!</Help>
!
!<Help KEYWORD="SP_INC">
!<Tip> Shotpoint interval. </Tip>
! Default = 25
! Allowed = real /= 0.0
! Shotpoint interval (distance boat must travel to change shotpoint one unit).
! SP_INC should be negative if shotpoint decrements.
!</Help>
!
!<Help KEYWORD="ANT_CORR">
!<Tip> Antenna correction. </Tip>
! Default = 100
! Allowed = real >= 0.0
! ANT_CORR should be the distance from the source to the antenna (or the
! distance from the source to the location where shotpoint annotations are
! specified if that is not the antenna location).
!</Help>
!
!<Help KEYWORD="PROF_SKIP">
!<Tip> Array of shotpoints of skipped profiles. </Tip>
! Default = none
! Allowed = real array (any number of values)
! Array of shotpoints of skipped profiles.  MGD fills in skipped profiles with
! dead traces.
!</Help>
!
!<Help KEYWORD="SP_DEPTH">
!<Tip> Array of shotpoints associated with each water depth entry. </Tip>
! Default = none
! Allowed = real array (any number of values)
! Array of shotpoints associated with each water depth entry.
! SP_DEPTH values must not lie beyond ends of line.
!</Help>
!
!<Help KEYWORD="DEPTHS">
!<Tip> Array of water depths. </Tip>
! Default = none
! Allowed = real array (any number of values)
! Array of water depth entries.
! MGD expects water depths to be entered as positive numbers.
!</Help>
!
!<Help KEYWORD="CHART">
!<Tip> Stacking chart print flag. </Tip>
! Default = YES
! Allowed = YES or NO.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


    module mgd_module
      use pc_module
      use named_constants_module
      use grid_module 
      use chart_module 
      implicit none
      private
      public :: mgd_create     ! uses the parameter cache.
      public :: mgd_initialize
      public :: mgd_update     ! uses the parameter cache.
      public :: mgd_delete
!<execute_only>
      public :: mgd            ! main execution (trace processing) routine.
      public :: mgd_wrapup
!</execute_only>

      character(len=100),public :: MGD_IDENT = &
        "$Id: mgd.f90,v 1.27 2006/09/11 13:15:48 Stoeckley prod sps $"


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!


      type,public :: mgd_struct

        private
        logical                      :: skip_wrapup      ! wrapup flag.
        integer                      :: nwih             ! global.
        integer                      :: ndpt             ! global.
        integer                      :: num_channels     ! process parameter.
        real                         :: off_near         ! process parameter.
        real                         :: rec_inc          ! process parameter.
        real                         :: src_inc          ! process parameter.
        character(len=4)             :: order            ! process parameter.
        integer                      :: nptb             ! process parameter.
        real                         :: sp_init          ! process parameter.
        real                         :: sp_inc           ! process parameter.
        integer                      :: skip_init        ! process parameter.
        real                         :: ant_corr         ! process parameter.
        real              ,pointer   :: prof_skip(:)     ! process parameter.
        real              ,pointer   :: sp_depth (:)     ! process parameter.
        real              ,pointer   :: depths   (:)     ! process parameter.
        integer                      :: n_prof_skip      ! process parameter.
        integer                      :: n_sp_depth       ! process parameter.
        integer                      :: n_depths         ! process parameter.
        logical                      :: chart            ! process parameter.
        integer                      :: nprfl            ! dependent.
        integer                      :: ntr              ! dependent.
        integer                      :: k                ! dependent.
        real                         :: froff            ! dependent.
        real                         :: bin              ! dependent.
        real                         :: fpint            ! dependent.
        integer                      :: israt            ! dependent.
        real                         :: begsp            ! dependent.
        real                         :: spinc            ! dependent.
        real                         :: g1               ! dependent.
        real                         :: dlx              ! dependent.
        logical                      :: ifill            ! dependent.
        logical                      :: inused           ! dependent.
        integer                      :: ifirst           ! dependent.
        integer                      :: nout             ! dependent.
        integer                      :: numgrid          ! dependent.
        integer           ,pointer   :: nskip(:)         ! dependent.
        real              ,pointer   :: wd   (:)         ! dependent.
        real              ,pointer   :: head (:)         ! dependent.
        integer           ,pointer   :: igrid(:)         ! dependent.
        type(chart_struct),pointer   :: chartobj         ! dependent.

      end type mgd_struct


!     Definitions of mgd_struct variables related to process parameters:
!
!     num_channels - Number of channels (receiver groups) in streamer.
!     off_near     - Near offset.
!     rec_inc      - Receiver increment (distance between receiver groups
!                      in streamer).
!     src_inc      - Source increment (distance between adjacent source
!                      locations).
!     order        - First trace in shot profile is FAR or NEAR channel.
!     nptb         - Number of shot profiles to build (including skips).
!     sp_init      - Shotpoint annotation at antenna position of first
!                      source.
!     sp_inc       - Shotpoint interval.
!     skip_init    - Number of trace headers to skip initially.
!     ant_corr     - Antenna correction.
!     prof_skip    - Array of shotpoof skipped profiles.
!     n_prof_skip  - Number of elements in prof_skip.
!     sp_depth     - Array of shotpoints associated with each water depth
!                      entry.
!     n_sp_depth   - Number of elements in sp_depth (same as n_depths).
!     depths       - Array of water depths.
!     n_depths     - Number of elements in depths (same as n_sp_depth).
!     chart        - Stacking chart print flag.


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!


      type(mgd_struct),pointer,save :: object      ! needed for traps.

      integer,parameter     :: norder = 2
      integer,parameter     :: nchart = 2
      character(len=8),save :: order_options (norder)
      logical         ,save :: chart_options (nchart)

      data order_options  /'NEAR','FAR'/
      data chart_options  /.true.,.false./

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


    subroutine mgd_create (obj)
      implicit none
      type(mgd_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify  (obj%prof_skip)
      nullify  (obj%sp_depth)
      nullify  (obj%depths)
      nullify  (obj%nskip)
      nullify  (obj%wd)
      nullify  (obj%head)
      nullify  (obj%igrid)
      nullify  (obj%chartobj)

      call mgd_initialize (obj)
      return
    end subroutine mgd_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


    subroutine mgd_delete (obj)
      implicit none
      type(mgd_struct),pointer :: obj       ! arguments

!<execute_only>
      call mgd_wrapup (obj)
!</execute_only>

      if (associated(obj%prof_skip)) deallocate    (obj%prof_skip) ! was skip
      if (associated(obj%sp_depth))  deallocate    (obj%sp_depth)  ! was wdsp
      if (associated(obj%depths))    deallocate    (obj%depths)    ! was wdnt
      if (associated(obj%nskip))     deallocate    (obj%nskip)
      if (associated(obj%wd))        deallocate    (obj%wd)
      if (associated(obj%head))      deallocate    (obj%head)
      if (associated(obj%igrid))     deallocate    (obj%igrid)
      if (associated(obj%chartobj)) call chart_delete (obj%chartobj)

      deallocate(obj)
      return
    end subroutine mgd_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


    subroutine mgd_initialize (obj)
      implicit none
      type(mgd_struct),pointer :: obj       ! arguments

      obj%num_channels = 320             ! was numch
      obj%off_near     = 300.0           ! was offnr
      obj%rec_inc      = 12.5            ! was rin
      obj%src_inc      = 12.5            ! was sin
      obj%order        = 'FAR'           ! was ft
      obj%nptb         = 1000            ! was nptb
      obj%sp_init      = 101.0           ! was sp1
      obj%sp_inc       = 25.0            ! was spin
      obj%skip_init    = 0               ! was iskp
      obj%ant_corr     = 100.0           ! was ac
      obj%n_prof_skip  = 0               ! was numskip
      obj%n_sp_depth   = 0               ! was numwd
      obj%n_depths     = 0               ! was numwd2
      obj%chart        = .true.          ! was sco
      obj%nwih         = -1              ! was nwih
      obj%ndpt         = -1              ! was ndpt

      call mgd_update (obj)
      return
    end subroutine mgd_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


    subroutine mgd_update (obj)
      implicit none
      type(mgd_struct),target :: obj                           ! arguments
      integer                 :: i, j
      integer                 :: nscratch, nstore
      integer                 :: chart_nscratch, chart_nstore
!!!!  integer                 :: numfp
      integer                 :: israt2, ierr, l
      integer                 :: ispg1
      real                    :: dlsp, ratio
      type(grid_struct)       :: grid

      object => obj                ! needed for traps.
      obj%skip_wrapup = .true.     ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_register_array_names ("sp_depth_arrayset", (/  &
                                    "sp_depth",              &
                                    "depths  " /))

      call pc_get_global ('nwih'    , obj%nwih) 
      call pc_get_global ('ndpt'    , obj%ndpt) 

      call pc_get ('NUM_CHANNELS', obj%num_channels,  mgd_num_channels_trap)
      call pc_get ('OFF_NEAR',     obj%off_near,      mgd_off_near_trap)
      call pc_get ('REC_INC',      obj%rec_inc,       mgd_rec_inc_trap)
      call pc_get ('SRC_INC',      obj%src_inc,       mgd_src_inc_trap)
      call pc_get ('ORDER',        obj%order,         mgd_order_trap)
      call pc_get ('NPTB',         obj%nptb,          mgd_nptb_trap)
      call pc_get ('SP_INIT',      obj%sp_init,       mgd_sp_init_trap)
      call pc_get ('SP_INC',       obj%sp_inc,        mgd_sp_inc_trap)
      call pc_get ('SKIP_INIT',    obj%skip_init,     mgd_skip_init_trap)
      call pc_get ('ANT_CORR',     obj%ant_corr,      mgd_ant_corr_trap)
      call pc_get ('CHART',        obj%chart)
      call pc_alloc ('PROF_SKIP' , obj%prof_skip, obj%n_prof_skip)
      call pc_alloc ('SP_DEPTH'  , obj%sp_depth , obj%n_sp_depth)
      call pc_alloc ('DEPTHS'    , obj%depths   , obj%n_depths)

      call pc_call_array_trap    ('PROF_SKIP'        , mgd_prof_skip_array_trap)
      call pc_call_arrayset_trap ('SP_DEPTH_ARRAYSET', mgd_sp_depth_array_trap)
      call pc_call_arrayset_trap ('SP_DEPTH_ARRAYSET', mgd_depths_array_trap)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!


      if (obj%n_sp_depth /= obj%n_depths) then
        call pc_error ('#SP_DEPTH and #DEPTHS must be the same.')
        obj%n_depths   = min(obj%n_depths, obj%n_sp_depth)
        obj%n_sp_depth = min(obj%n_depths, obj%n_sp_depth)
      end if

      if (abs(obj%src_inc/obj%rec_inc - 0.25) <= 1.0e-3) then
        obj%bin = obj%src_inc
        obj%numgrid = obj%nptb + 2 * obj%num_channels - 2
        obj%fpint = obj%src_inc
!!!!    numfp = obj%numgrid
      else
        obj%bin = obj%rec_inc / 2.0
        obj%israt = nint(obj%src_inc / obj%bin)
        israt2 = obj%israt / 2
        if (2 * israt2 == obj%israt) then
          obj%fpint = obj%rec_inc
        else
          obj%fpint = obj%bin
        end if
        obj%numgrid = obj%num_channels + (obj%nptb - 1) * obj%israt
!!!!    numfp = obj%num_channels + (obj%nptb - 1) * nint(obj%src_inc/obj%fpint)
      endif

!!!!  numfp = numfp + obj%num_channels *  &
!!!!                   (obj%src_inc / obj%bin + obj%rec_inc / obj%bin)
!!!!               ! the above is simply to add enough CMP locations for the
!!!!               ! stacking chart without trying to figure out exactly
!!!!               ! how many are needed.

      obj%froff = obj%off_near + (obj%num_channels - 1) * obj%rec_inc
      obj%spinc = obj%bin / obj%sp_inc
      obj%g1    = obj%froff / 2.0


!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


      chart_nstore   = 0
      chart_nscratch = 0

      if (obj%chart) then

           call pc_clear

           call grid_initialize      (grid)
           call grid_set_xorigin     (grid, (obj%froff - obj%rec_inc)/2.0d0)
           call grid_set_xgrid_width (grid, obj%rec_inc/2.0d0)

           call pc_put_global  ('grid', grid)

           call pc_put_process ('mode'     , 'HEADERS')
           call pc_put_process ('spacing'  , 'SINGLE' )
           call pc_put_process ('device'   , 'PRINTER')
           call pc_put_process ('prof_init', 1        )
           call pc_put_process ('prof_last', obj%nptb )
           call pc_put_process ('hdr_len'  , 7        )
           call pc_put_process ('len_init' , 1.0      )
           call pc_put_process ('len_inc'  , 1.0      )
!!!!       call pc_put_process ('len_tot'  , numfp    )
           call pc_put_process ('len_tot'  , obj%numgrid)
           call pc_put_process ('hdr_wid'  , 0        )
           call pc_put_process ('wid_init' , 1.0      )
           call pc_put_process ('wid_inc'  , 1.0      )

           if (associated(obj%chartobj)) then
                call chart_update (obj%chartobj)
           else
                call chart_create (obj%chartobj)
           end if

           call pc_get_control ("nstore"  , chart_nstore)
           call pc_get_control ("nscratch", chart_nscratch)

           call pc_restore

      endif


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!


      nscratch = 0
      nstore   = obj%n_prof_skip + obj%n_sp_depth + obj%n_depths  &
            + obj%nwih + obj%numgrid + obj%n_prof_skip + obj%n_sp_depth

      call pc_put_options_field ('order', order_options, norder)
      call pc_put_options_field ('chart', chart_options, nchart)

      call pc_put ('NUM_CHANNELS', obj%num_channels    )
      call pc_put ('OFF_NEAR',     obj%off_near    , 12)
      call pc_put ('REC_INC',      obj%rec_inc     , 12)
      call pc_put ('SRC_INC',      obj%src_inc     , 12)
      call pc_put ('ORDER',        obj%order           )
      call pc_put ('NPTB',         obj%nptb            )
      call pc_put ('SP_INIT',      obj%sp_init     , 12)
      call pc_put ('SP_INC',       obj%sp_inc      , 12)
      call pc_put ('SKIP_INIT',    obj%skip_init       )
      call pc_put ('ANT_CORR',     obj%ant_corr    , 12)
      call pc_put ('CHART',        obj%chart           )

      call pc_put ('PROF_SKIP' , obj%prof_skip, obj%n_prof_skip, 12)
      call pc_put ('SP_DEPTH'  , obj%sp_depth , obj%n_sp_depth , 12)
      call pc_put ('DEPTHS'    , obj%depths   , obj%n_depths   , 12)

      call pc_put_control ('nscratch'     , nscratch + chart_nscratch)
      call pc_put_control ('nstore'       , nstore   + chart_nstore)
      call pc_put_control ('twosets'      , .true.)
      call pc_put_control ('need_label'   , .true.)
      call pc_put_control ('need_request' , .true.)


!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!


      if (associated(obj%nskip))     deallocate        (obj%nskip)
      if (associated(obj%wd))        deallocate        (obj%wd)
      if (associated(obj%head))      deallocate        (obj%head)
      if (associated(obj%igrid))     deallocate        (obj%igrid)

      obj%nprfl  = 0
      obj%ntr    = obj%num_channels 
                               ! Set NTR to cause profile increment cycle.
      obj%k      = 1
      obj%ifirst = 1
      obj%nout   = 0
      obj%ifill  = .false.
      obj%inused = .false.

!<execute_only>

      if (pc_do_not_process_traces()) return

      obj%skip_wrapup = .false.    ! to run wrapup code after processing.

!-------------------allocate arrays:

      allocate (obj%head(obj%nwih),stat=ierr)
      if (ierr /= 0) then
        call pc_error ('HEAD memory allocation error for NWIH = ',obj%nwih)
      else
        call pc_print ('HEAD memory allocation for NWIH = ',obj%nwih)
      end if

      allocate (obj%wd(obj%numgrid),stat=ierr)
      if (ierr /= 0) then
        call pc_error ('WD memory allocation error for NUMGRID = ',obj%numgrid)
      else
        call pc_print ('WD memory allocation for NUMGRID = ',obj%numgrid)
      end if

      allocate (obj%nskip(obj%n_prof_skip),stat=ierr)
      if (ierr /= 0) then
        call pc_error ('NSKIP memory allocation error for N_PROF_SKIP = ', &
                                                      obj%n_prof_skip)
      else
        call pc_print ('NSKIP memory allocation for N_PROF_SKIP = ', &
                                                obj%n_prof_skip)
      end if

      if (obj%n_sp_depth /= 0) then
        allocate (obj%igrid(obj%n_sp_depth),stat=ierr)
        if (ierr /= 0) then
          call pc_error ('IGRID memory allocation error for N_SP_DEPTH = ', &
                                                        obj%n_sp_depth)
      else
          call pc_print ('IGRID memory allocation for N_SP_DEPTH = ', &
                                                  obj%n_sp_depth)
        end if
      end if

!-------------------initialize trace header:

      obj%head(1:obj%nwih) = 0.0
      obj%head(2) = 1.0
      obj%head(5) = 1.0
      obj%head(64) = obj%ndpt

!------------------calculate shot point for first trace on line:

      ispg1 = nint((obj%ant_corr + obj%froff / 2.0) / obj%bin) + 1
!                                                     antenna grid for 1st
      obj%begsp = obj%sp_init - (ispg1 - 1) * obj%spinc

!------------------determine water depth for each grid position:

      if (obj%n_sp_depth /= 0) then
        do i = 1, obj%n_sp_depth                 !  calc grid for each wd entry
          dlsp = obj%sp_depth(i) - obj%begsp
          obj%igrid(i) = nint(dlsp/obj%spinc) + 1
        end do
                                                 !  beginning of line
        obj%wd(:obj%igrid(1)) = obj%depths(1)

        if (obj%n_sp_depth > 1) then
          l400: do j = 1, obj%n_sp_depth - 1          !  loop on wd entries
            do L = obj%igrid(j) + 1, obj%igrid(j+1)   !  loop on grids for each
              ratio = float(L - obj%igrid(j))/float(obj%igrid(j+1)-obj%igrid(j))
              if (L <= obj%numgrid) then
                obj%wd(L) = obj%depths(j)+(obj%depths(j+1)-obj%depths(j))*ratio
              else
                cycle  l400
              endif
            end do
          end do l400
        endif

        if (obj%igrid(obj%n_sp_depth) /= obj%numgrid) then
                                                 !  end of line
          obj%wd(obj%igrid(obj%n_sp_depth)+1:obj%numgrid) =  &
                                           obj%depths(obj%n_sp_depth)
        endif
      endif

!------------------calculate skipped profile numbers from skipped shot points:

      if (obj%n_prof_skip > 0) then
        do i = 1, obj%n_prof_skip
          obj%nskip(i) = nint((obj%prof_skip(i)-obj%sp_init) * &
                         (obj%sp_inc/obj%src_inc)) + 1
        end do
      endif

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!


      return
    end subroutine mgd_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


    subroutine mgd_num_channels_trap (keyword)
      implicit none
      character(len=*), intent(in) :: keyword

      if (object%num_channels <= 0 .or. object%num_channels > 9999) then
        call pc_warning ('NUM_CHANNELS has been set from', &
                     object%num_channels,'to default 320.')
        object%num_channels = 320
      end if
    end subroutine mgd_num_channels_trap



    subroutine mgd_off_near_trap (keyword)
      implicit none
      character(len=*), intent(in) :: keyword

      if (object%off_near <= 0.0) then
        call pc_warning ('OFF_NEAR has been set from',object%off_near,  &
                                     'to default 300.0.')
        object%off_near = 300.0
      end if
    end subroutine mgd_off_near_trap



    subroutine mgd_rec_inc_trap (keyword)
      implicit none
      character(len=*), intent(in) :: keyword

      if (object%rec_inc <= 0.0) then
        call pc_warning ('REC_INC has been set from',object%rec_inc,  &
                                     'to default 12.5')
        object%rec_inc = 12.5
      end if
    end subroutine mgd_rec_inc_trap



    subroutine mgd_src_inc_trap (keyword)
      implicit none
      character(len=*), intent(in) :: keyword
      integer                      :: israt

      if (object%src_inc <= 0.0) then
        call pc_warning ('SRC_INC has been set from',object%src_inc,  &
                                     'to default 12.5')
        object%src_inc = 12.5
      end if
      if (object%src_inc / object%rec_inc - 0.25 <= 0.05) then
        object%src_inc = 0.25 * object%rec_inc
      else
        israt = nint(object%src_inc / (object%rec_inc / 2.0))
        if (israt * object%rec_inc /= object%src_inc * 2.0) then
           call pc_error ('INVALID SRC_INC',object%src_inc,  &
                                  ' - should be multiple of REC_INC/2')
           return
        end if
        object%src_inc = israt * (object%rec_inc / 2.0)
      end if
    end subroutine mgd_src_inc_trap



    subroutine mgd_order_trap (keyword)
      implicit none
      character(len=*), intent(in) :: keyword
      character(len=1)             :: first

      first = object%order(1:1)
      if (first == 'N' .or. first == 'n') then
           object%order = 'NEAR'
      else if (first == 'F' .or. first == 'f') then
           object%order = 'FAR'
      else
           call pc_warning ('ORDER has been set from',trim(object%order),  &
                                     'to default FAR.')
           object%order = 'FAR'
      end if

  !   call string_to_upper (object%order)
  !   select case (object%order)
  !   case ('FAR', 'NEAR')
  !     return
  !   case default
  !     call pc_warning ('ORDER has been set from',trim(object%order),  &
  !                                  'to default FAR.')
  !     object%order = 'FAR'
  !     return
  !   end select
    end subroutine mgd_order_trap



    subroutine mgd_nptb_trap (keyword)
      implicit none
      character(len=*), intent(in) :: keyword

      if (object%nptb <= 0) then
        call pc_warning ('NPTB has been set from',object%nptb,  &
                                     'to default 1000.')
        object%nptb = 1000
      end if
    end subroutine mgd_nptb_trap



    subroutine mgd_skip_init_trap (keyword)
      implicit none
      character(len=*), intent(in) :: keyword

      if (object%skip_init < 0) then
        call pc_warning ('SKIP_INIT has been set from',object%skip_init,  &
                                     'to default 0.')
        object%skip_init = 0
      end if
    end subroutine mgd_skip_init_trap



    subroutine mgd_sp_init_trap (keyword)
      implicit none
      character(len=*), intent(in) :: keyword

!  parameter can be any real number; no action required

        return
    end subroutine mgd_sp_init_trap



    subroutine mgd_sp_inc_trap (keyword)
      implicit none
      character(len=*), intent(in) :: keyword

      if (object%sp_inc == 0.0) then
        call pc_warning ('SP_INC has been set from',object%sp_inc,  &
                                     'to default 25.0.')
        object%sp_inc = 25.0
      end if
      return
    end subroutine mgd_sp_inc_trap



    subroutine mgd_ant_corr_trap (keyword)
      implicit none
      character(len=*), intent(in) :: keyword

      if (object%ant_corr < 0.0) then
        call pc_warning ('ANT_CORR has been set from',object%ant_corr,  &
                                     'to default 100.0.')
        object%ant_corr = 100.0
      end if
      return
    end subroutine mgd_ant_corr_trap



    subroutine mgd_prof_skip_array_trap (keyword)     ! array trap
      implicit none
      character(len=*), intent(in) :: keyword
      integer                      :: i

      if (object%n_prof_skip > 1) then
        if (object%prof_skip(2) >= object%prof_skip(1)) then  ! increasing
          do i = 2, object%n_prof_skip
            if (object%prof_skip(i) <= object%prof_skip(i-1)) then
              call pc_error ('PROF_SKIP entries must increase continuously.')
              return
            end if
          end do
        else
          do i = 2, object%n_prof_skip
            if (object%prof_skip(i) >= object%prof_skip(i-1)) then
              call pc_error ('PROF_SKIP entries must decrease continuously.')
              return
            end if
          end do
        end if
      end if
      return
    end subroutine mgd_prof_skip_array_trap



    subroutine mgd_sp_depth_array_trap (keyword)
      implicit none
      character(len=*), intent(in) :: keyword
      integer                      :: i, nline, kount
      real                         :: endsp
      integer                      :: ispg1
! find range of allowed shotpoint values
      object%froff = object%off_near + (object%num_channels-1)*object%rec_inc
      if (abs(object%src_inc/object%rec_inc - 0.25).le.1e-3) then
        object%bin = object%src_inc
        object%numgrid = object%nptb + 2*object%num_channels - 2
      else
        object%bin = object%rec_inc/2.
        object%israt = nint(object%src_inc/object%bin)
        object%numgrid = object%num_channels + (object%nptb-1)*object%israt
      end if
      object%spinc = object%bin/object%sp_inc
      ispg1        = nint((object%ant_corr+(object%froff/2.))/object%bin) + 1
      object%begsp = object%sp_init - (ispg1 - 1)*object%spinc
      endsp = object%begsp + object%spinc*(object%numgrid-1)

! removed 2/7/00:      object%n_sp_depth = 0
! removed 2/7/00:      do i = 1, 1000, -1            ! locate extent of array
! removed 2/7/00:        if (object%sp_depth(i) == 0.0) cycle
! removed 2/7/00:        object%n_sp_depth = i
! removed 2/7/00:        exit
! removed 2/7/00:      end do
      kount = 0
      do nline=1,object%n_sp_depth
        if (object%sp_inc.gt.0.0) then  ! check for entries outside sp range
          if (object%sp_depth(nline).lt.object%begsp) then
            kount = kount + 1
            if (kount < 10)  &
              call pc_warning ('SP_DEPTH entry', object%sp_depth(nline), &
                     'beyond beginning of line - reset to', object%begsp)
            object%sp_depth(nline) = object%begsp
          else if (object%sp_depth(nline).gt.endsp) then
            kount = kount + 1
            if (kount < 10)  &
              call pc_warning ('SP_DEPTH entry', object%sp_depth(nline), &
                     'beyond end of line - reset to', endsp)
            object%sp_depth(nline) = endsp
          end if
        else                                   ! (line decrements.)
          if (object%sp_depth(nline).gt.object%begsp) then
            kount = kount + 1
            if (kount < 10)  &
              call pc_warning ('SP_DEPTH entry', object%sp_depth(nline), &
                     'beyond beginning of line - reset to', object%begsp)
            object%sp_depth(nline) = object%begsp
          else if (object%sp_depth(nline).lt.endsp) then
            kount = kount + 1
            if (kount < 10)  &
              call pc_warning ('SP_DEPTH entry', object%sp_depth(nline), &
                     'beyond end of line - reset to', endsp)
            object%sp_depth(nline) = endsp
          end if
        end if
      end do

      if (object%n_sp_depth.gt.1) then
        if (object%sp_depth(2).ge.object%sp_depth(1)) then ! increasing entries
          kount = 0
          do i=2,object%n_sp_depth
            if(object%sp_depth(i).le.object%sp_depth(i-1)) then
              kount = kount + 1
              if (kount < 10)  &
                call pc_error ('SP_DEPTH entries not all increasing.')
            end if
          end do
        else                             !  decreasing entries
          kount = 0
          do i=2,object%n_sp_depth
            if(object%sp_depth(i).ge.object%sp_depth(i-1)) then
              kount = kount + 1
              if (kount < 10)  &
                call pc_error ('SP_DEPTH entries not all decreasing.')
            end if
          end do
        end if
      end if
      return
    end subroutine mgd_sp_depth_array_trap



    subroutine mgd_depths_array_trap (keyword)        ! array trap
      implicit none
      character(len=*), intent(in) :: keyword
      integer                      :: i,kount
      character(len=80)            :: message

! removed 2/7/00:      object%n_depths = 0
! removed 2/7/00:      do i = 1, 1000, -1          ! locate extent of array
! removed 2/7/00:        if (object%depths(i) == 0.0) cycle
! removed 2/7/00:        object%n_depths = i
! removed 2/7/00:        exit
! removed 2/7/00:      end do
      kount = 0
      do i = 1, object%n_depths
        if (kount < 10 .and. object%depths(i) < 0) then
          kount = kount + 1
          write(message,*)'DEPTHS(',i,') = ',object%depths(i),' is not &
                          &a positive number.'
          call pc_error (message)
        end if
      end do
      return
    end subroutine mgd_depths_array_trap


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


!<execute_only>

    subroutine mgd (obj,ntr,hdi,tri,hdo,tro)
      implicit none
      type(mgd_struct)                :: obj                ! arguments
      integer         ,intent(inout)  :: ntr                ! arguments
      double precision,intent(in)     :: hdi(:,:)           ! arguments
      real            ,intent(in)     :: tri(:,:)           ! arguments
      double precision,intent(inout)  :: hdo(:,:)           ! arguments
      real            ,intent(inout)  :: tro(:,:)           ! arguments
      integer                         :: ntrf               ! local

!------------------initial call direction logic

      if (ntr == NEED_TRACES) then               ! this call is from below
        if (obj%inused) then                     ! get a new trace from above
          ntr = NEED_TRACES
          return
        end if
        ntr = 1
      else
        if (ntr > 1) then
          call pc_print ('MGD input must be single trace only.')
          call mgd_wrapup (obj)
          ntr = FATAL_ERROR
          return
        else if (ntr == NO_MORE_TRACES) then         ! too many traces
          call mgd_wrapup (obj)
          return
        end if
        obj%inused = .false.                         ! no more traces
      end if

  150 continue
      obj%ntr = obj%ntr + 1                          ! increment trace number

!------------------new profile processing starts here

      if (obj%ntr > obj%num_channels) then
        obj%nprfl = obj%nprfl + 1
        if (obj%nprfl > obj%nptb) then                   ! don't build too many
          call pc_print ('Too many traces; check value of NPTB.')
          call mgd_wrapup (obj)
          ntr = FATAL_ERROR
          return
        end if
        obj%ntr = 1
        obj%ifill = .false.
        if (obj%n_prof_skip /= 0) then
          if (obj%nskip(obj%k) == obj%nprfl) then    ! is this profile skipped?
            if (obj%k < obj%n_prof_skip) obj%k = obj%k + 1
            obj%ifill = .true.
          end if
        end if

!------------------calc headers depending on profile only

        obj%dlx = (obj%nprfl - 1) * obj%src_inc       ! find x displacement
        obj%head(3) = obj%nprfl
        obj%head(9) = obj%nprfl
        obj%head(11) = obj%froff + obj%dlx
        obj%head(29) = obj%sp_init+(obj%nprfl - 1)*(obj%src_inc/obj%sp_inc)
                                                      ! for stacking chart

        obj%head(33) = nint((obj%head(11) - obj%g1) / obj%bin) + 1
        obj%head(46) = nint(obj%head(11) / obj%fpint) + 1
      endif

      if (obj%ifirst == 1) then
        if (obj%skip_init == 0) then
          obj%ifirst = 0
        else if (obj%skip_init > 0) then
          obj%skip_init = obj%skip_init - 1
          go to 150
        end if
      end if

!------------------calc. headers depending on receiver number.

      if (obj%order == 'NEAR') then               ! make ntrf be trace number
        ntrf = obj%num_channels - obj%ntr + 1     ! counting from far end
      else
        ntrf = obj%ntr
      endif

      obj%nout     = obj%nout + 1
      obj%head( 1) = obj%nout
      obj%head( 4) = obj%ntr
      obj%head(10) = obj%ntr
      obj%head( 6) = obj%off_near + (obj%num_channels - ntrf)*obj%rec_inc
      obj%head(14) = (ntrf - 1)*obj%rec_inc + obj%dlx
      obj%head(17) = (obj%head(11)+obj%head(14))/2.
      obj%head( 7) = nint((obj%head(17)-obj%g1)/obj%bin) + 1
      obj%head(35) = nint((obj%head(14)-obj%g1)/obj%bin) + 1
      obj%head(37) = obj%begsp + obj%spinc*(obj%head(7)-1)
      obj%head(47) = nint(obj%head(14)/obj%fpint) + 1
      obj%head(19) = -1.0*obj%wd(nint(obj%head(7)))
      obj%head(25) = hdi(25,1)                      ! transfer lav unchanged.
      obj%head( 2) = hdi( 2,1)                      ! transfer mute unchanged.
      obj%head(64) = hdi(64,1)                      ! transfer mute unchanged.
      obj%head(28) = obj%head(29) - obj%head(6) / obj%sp_inc    ! receiver sp.

      hdo(:obj%nwih,1) = obj%head(:obj%nwih)        ! apply calculated headers.

      hdo(48:55,1) = hdi(48:55,1)   ! pass thru user defined headers unchanged.

      hdo(HDR_NOMINAL_SIZE+1:obj%nwih,1) = hdi(HDR_NOMINAL_SIZE+1:obj%nwih,1)
                                    ! pass thru headers above 64 unchanged.

!------------------fill output trace buffers.

      if (.not.obj%ifill) then                       ! transfer good trace.
        tro(:obj%ndpt,1) = tri(:obj%ndpt,1)
        obj%inused = .true.
      else                                          ! this profile is skipped.
        hdo(25,1) = 0.0
        hdo(48:55,1) = 0.0                           ! user defined headers.
        hdo(HDR_NOMINAL_SIZE+1:obj%nwih,1) = 0.0     ! user defined headers.
        tro(:obj%ndpt,1) = 0.0
      endif

!------------------add header to stacking chart.

      ntr = 1
      if (obj%chart) then
        call chart (obj%chartobj, ntr, hdo, tro)      ! never changes ntr.
      end if

!------------------return (down) to pass out processed trace.

      return
    end subroutine mgd

!</execute_only>


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine mgd_wrapup (obj)
      implicit none
      type(mgd_struct) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      if (associated(obj%chartobj)) call chart_wrapup (obj%chartobj)

!!!!  if (pc_get_update_state() /= PC_EXECUTE) return

      return
      end subroutine mgd_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module mgd_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!



