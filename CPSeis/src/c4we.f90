!<CPS_v1 type="PROCESS"/>
!!------------------------------- c4we.f90 ---------------------------------!!
!!------------------------------- c4we.f90 ---------------------------------!!
!!------------------------------- c4we.f90 ---------------------------------!!

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
! Name       : C4WE     (4th order Cumulant Wavelet Estimation)
! Category   : filters
! Written    : 1997-05-20   by: Greg Lazear
! Revised    : 2007-01-03   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Wavelet estimation by fourth order cumulant method.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! C4WE estimates a wavelet from seismic data by calculating the the fourth-
! order cumulant of the data.  It solves for the wavelet using a nonlinear
! least-squares optimization to fit the wavelet fourth-moment to the cumulant.
! The solution involves iteratively linearizing the problem, and solving for
! perturbations to the wavelet via conjugate gradient descent. The estimated
! cumulant is tapered to suppress the variance at large lags. This taper
! greatly improves the estimated wavelet.
!
! The wavelet is estimated such that its fourth-order moment matches selected
! lags of the fourth-order cumulant of the data in a least-squares sense.
! Cumulant terms that correspond to the autocorrelation of the squared trace
! have been dropped from the solution since they have no phase discrimination.
!
! The cumulant of the data is tapered with a three-dimensional triangular taper
! which results from the fourth-order correlation of a square wave of length
! LEN_OP. This reduces the variance noise of the cumulant at large lags where
! the fourth-order moment of the wavelet goes to zero, but the cumulant does
! not. This greatly improves the stability of the estimated wavelet, and
! reliable wavelets can be found from much smaller data sets.
!
!
! Fourth-Order Cumulant
! Unlike the autocorrelation, which carries no phase information, the fourth-
! order cumulant does carry information on the phase of the dataset it
! represents.  It is the simplest statistic to do so.
!
! REFERENCES
! Lazear, G., 1991, Mixed-Phase Wavelet Estimation Using Fourth-Order
! Cumulants:  Conoco Research Report #0611-Q01-001-1-91.
!
! Lazear, G., 1993, Mixed-phase wavelet estimation using fourth-order cumulants:
! Geophysics, 58, no. 07, 1042 - 1051.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                             ADVICE FOR USERS
!
! Polarity
! C4WE is indifferent as to polarity.  The wavelet estimated by C4WE and its
! polarity reversed version should be considered equally likely.
!
! Size of Input Dataset
! C4WE requires more input traces to make a reliable estimate when the traces
! are very similar.  Normally not fewer than 300 traces should be used and
! there is no advantage to using more than about 1200.
!
! Phase of Estimated Wavelet
! C4WE estimates the wavelet embedded in the input dataset under the assumption
! that the underlying reflectivity sequence is completely uncorrelated.  Since
! there can be no assurance that this is the case, it is probably more accurate
! to claim that C4WE estimates the "average event character" within the input
! dataset.  Only if the input dataset has a perfectly uncorrelated reflectivity
! sequence, does the "average event character" represent the true embedded
! wavelet.
!
! Wavelet Centering
! Just as C4WE is indifferent as to polarity, it is also indifferent as to
! arbitrary time shifting of the wavelet. The TIM_FIRST parameter allows
! APPROXIMATE centering of the wavelet at zero time, but you may still get a
! bulk shift when applying this wavelet to your data. To avoid this problem,
! you may analyze the estimated wavelet using SPCT with OPT_PHASE = FLATTEN,
! which will estimate a more accurate TIM_FIRST for your wavelet, based on the
! time shift that maximally flattens its phase spectrum. The improved TIM_FIRST
! is displayed in header word HDR_TIM_FIRST (default = 50) in the spectrum
! traces output by SPCT. You may then enter this improved TIM_FIRST manually
! in DSIG when applying your wavelet, thereby overriding the TIM_FIRST value
! that DSIG picks up automatically from your wavelet file.
!
! Input Subsets
! Subsets of the input dataset are defined by constant values of HDR_IN, or you
! may set HDR_IN = 0 to estimate only one wavelet from your entire dataset. If
! HDR_IN > 0, C4WE estimates a wavelet from each subset with common value of
! header word HDR_IN, and your input data must be sorted in order of HDR_IN.
!
! Cost and Time
! The run time for C4WE is approximately proportional to the number of input
! traces and the length of the estimation window.   Run time is approximately
! proportional to LEN_OP raised to the fourth power.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! This process requires traces to be input one at a time
!
! If HDR_IN > 0, input traces must be sorted by HDR_IN such that the value
! of header word HDR_IN can be used to designate data subsets for individual
! wavelet estimation.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process passes traces unaltered (changed in August 2003).
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       Used (must be 1).
! NWIH      number of words in trace header         Used but not changed.
! NDPT      number of sample values in trace        Used but not changed.
! DT        trace sample interval                   Used but not changed.
! TSTRT     starting time on trace                  Used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Header words used from input traces:
!
! Hwd#      Description                 Action taken
! ----      -----------                 ------------
! 2          HDR_TOP_MUTE               Used in limiting estimation window.
! 25         HDR_LAV                    Used to check for dead traces.
! 64         HDR_BOTTOM_MUTE            Used in limiting estimation window.
! HDR_IN     User-specified header      Used for defining trace groups.
!            designating groups for
!            wavelet estimation
! WIN_HDR_A  User-specified header      Used in specifying estimation window.
!            for coordinate A for
!            laterally varying window
! WIN_HDR_B  User-specified header      Used in specifying estimation window.
!            for coordinate B for
!            laterally varying window
!
! Header words set in output traces (estimated wavelets)
! (All OTHER header words, not listed below, are set to zero on output):
!
! Hwd#      Description                 Action taken
! ----      -----------                 ------------
! 1          HDR_SEQUENCE               Set to sequential wavelet number.
! 2          HDR_TOP_MUTE               Set to 1.
! 3          HDR_CURRENT_GROUP          Set to sequential wavelet number.
! 4          HDR_CURRENT_CHANNEL        Set to 1.
! 25         HDR_LAV                    Set to wavelet LAV.
! 64         HDR_BOTTOM_MUTE            Set to # samples in output wavelet.
! HDR_IN     User-specified header      Retained from input traces.
!            designating groups for
!            wavelet estimation
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date       Author    Description
!     ----       ------    -----------
! 18. 2007-01-03 Stoeckley Fixed cut-and-paste error in HelpSection which made
!                           this process misbehave in SeisSpace; added file
!                           selection box.
!017. 2006-06-22 D. Glover Added NULLIFY statements for Intel compiler.
!016. 2006-01-10 B. Menger Removed Unused Variables.
! 15. 2005-01-31 Ried      Start time of passed traces will not change
!                          The wavelet start time will change with TIM_FIRST
! 14. 2004-08-31 SMCook    Removed kind intrinsic function due to compiler
!                           warning.  Replaced with hard-coded value.
! 13. 2003-08-06 SMCook    Changed nature of trace flow.  C4WE is no longer a
!                           loop-splitting process.  It now outputs its
!                           results to a trace file, and passes traces through
!                           unaltered.  Formerly, C4WE consumed input traces
!                           and passed along its results (wavelet estimates).
!                          Fixed apparent long-standing potential bug caused by
!                           several arrays not being properly zeroed (on the
!                           Solaris platform causing a float point error).
!                          Added a WATERBOTTOM_DEFAULTS option that sets a
!                           number of gui parameters to values that make sense
!                           when trying to use C4WE as a waterbottom extractor.
!                          Changed EXCLUDE_SHALLOW guidef approach slightly.
! 12. 2003-06-30 SMCook    Added EXCLUDE_SHALLOW to easily exclude shallow
!                           analysis windows from the wavelet estimate.
! 11. 2001-05-14 Baumel    Change wrapped_up flag to skip_wrapup.
! 10. 2000-06-16 Baumel    Eliminate check of GATHERED global (process simply
!                          requires NUMTR = 1 but shouldn't care whether
!                          GATHERED is true or false). Also new documentation
!                          on wavelet centering.
!  9. 2000-06-06 Baumel    Full conversion to new system.
!  8. 2000-04-05 Baumel    Add RCS ident string (but have *not* yet gone thru
!                          this process to fix all of Sharp's errors. This
!                          process is not yet ready for testing. Maturity is
!                          still raw!).
!  7. 1999-11-15 Sharp     Convert code to new system.
!  6. 1998-11-10 Vunderink Begin using the f90 compiler.
!  5. 1997-11-10 Vunderink Added spatial wavelet estimation window
!  4. 1997-07-14 Lazear    Modified trace flow control.
!  3. 1997-07-08 Goodger   Set header word 4 to negative tim_first rather than
!                          tim_first per Chuck I. Burch.
!  2. 1997-06-30 Lazear    Dropped squared terms of cumulant.
!  1. 1997-05-20 Lazear    Created original version
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

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
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
! NEED_REQUEST   false     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH       varies    amount of temporary memory needed.
! NSTORE         varies    amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!    NTR == 1              means to process the input trace.
!    NTR == NO_MORE_TRACES means there are no more input traces.
!    NTR == NEED_TRACES    means that a later process needs more traces.
!
! Upon output, NTR will have one of these values:
!    NTR == 1              if this process is outputting a trace.
!    NTR == NO_MORE_TRACES if there are no more traces to output.
!    NTR == FATAL_ERROR    if this process has a fatal error.
!    NTR == NEED_TRACES    if this process needs more traces.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<int_calling_doc>
!-------------------------------------------------------------------------------
!                    ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
!
!-------------------------------------------------------------------------------
!</int_calling_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
! C4WE uses an iterative method to find the wavelet whose 4th order moment (M4)
! matches the normalized 4th order cumulant of the data (C4). The number of
! overall iterations is set by the user parameter ITER. However, each of these
! major iterations includes a conjugate gradient solution of the linearized
! problem using NUM_CONJGRAD iterations followed by a line search of the full
! nonlinear objective using NUM_LINE_SEARCH iterations. The values of
! NUM_CONJGRAD and NUM_LINE_SEARCH are hard-wired into the code but are easily
! accessible in the "data" section immediately following the definition of the
! data structure for this process. Currently (2000-06-06), these are set to
! NUM_CONJGRAD = 10 and NUM_LINE_SEARCH = 15, which are both reduced from Greg
! Lazear's previous version (In that older version, NUM_CONJGRAD was equal to
! the number of wavelet samples--which would produce an exact solution of the
! linearized problem given exact arithmetic but is certainly excessive for any
! numerical conjugate gradient algorithm--and NUM_LINE_SEARCH was set to 20).
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
! Portions of the calculation are performed in double precision in order to
! obtain reasonable results and avoid overflows (e.g., when multiplying four
! trace values together). As of 2000-06-06, more of the calculation has been
! set in double precision than in Lazear's previous version. This issue was
! irrelevant on the Cray, which effectively performed all arithmetic in
! double precision (64-bit) in any case. Lazear's code actually declared some
! intermediate variables double precision even though it wasn't needed on the
! Cray. In this current version, more of the intermediate results are kept in
! double precision.
!
!-------------------------------------------------------------------------------
!</programming_doc>

!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS C4WE Process>
!         4th order Cumulant Wavelet Estimation Process.
!
!  WATERBOTTOM_DEFAULTS~~=`CC
!
! `-------------------------------------------------------------------------
!   HDR_IN=`IIIIIII    LEN_OP=~~~`FFFFFFFFFFF    EXCLUDE_SHALLOW=`CC
!   ITER=~~`IIIIIII    TIM_FIRST=`FFFFFFFFFFF    EXCLUDE_TIM=~~~~`FFFFF
!
!  Select WAVELETS_OUT[WAVELETS_OUT]=`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!                 [wavelets_out_info]`XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
! `-------------------------------------------------------------------------
!
!         Specify time window(s) to derive wavelet from
!
!<include latwin.f90>
!
!</gui_def>

!<HelpSection>
!
!<Help KEYWORD="WATERBOTTOM_DEFAULTS">
!<Tip> Lock various parameters to their default values for deep water. </Tip>
! Default = NO
! Allowed = YES, NO
! If set to YES, sets
!   LEN_OP          =  .200
!   TIM_DFIRST      = -.100
!   WIN_TIM_LEN     =  .400
!   EXCLUDE_SHALLOW =  'YES'
!   EXCLUDE_TIM     =  .750
! and desensitizes these fields.
!</Help>
!
!<Help KEYWORD="HDR_IN">
!<Tip> Header word designating trace subsets for wavelet estimation. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! Each subset of input traces, with a common value of HDR_IN, will have a
! wavelet estimated by C4WE. Input traces should be sorted with HDR_IN as a
! sort header word.  Specify HDR_IN = 0 to estimate just one wavelet from
! the entire dataset.
!</Help>
!
!<Help KEYWORD="LEN_OP">
!<Tip> Length of the estimated wavelet, in seconds. </Tip>
! Default = 0.12
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="ITER">
!<Tip> Number of iterations in the least-squares optimization. </Tip>
! Default = 10
! Allowed = int > 0
!</Help>
!
!<Help KEYWORD="TIM_FIRST">
!<Tip> Time of first sample in estimated wavelet, in seconds. </Tip>
! Default = -0.5 * LEN_OP
! Allowed = 0.0 >= real >= -LEN_OP
! Time of first sample in the wavelet for approximate centering of wavelet
! (setting TIM_FIRST = -0.5 * LEN_OP places time zero at the center of the
! wavelet). Upon output, the TSTRT global is reset to your TIM_FIRST value;
! you may then use TROT to save your estimated wavelet in a file for use by
! DSIG, which will automatically pick up this TIM_FIRST value from the file.
! However, wavelet centering by C4WE is only approximate. To avoid a bulk
! shift, you may analyze your estimated wavelet using SPCT to estimated an
! improved TIM_FIRST value, which may then be used when applying your wavelet
! using DSIG. For more info, see "Wavelet Centering" in ADVICE FOR USERS.
!</Help>
!
!<Help KEYWORD="EXCLUDE_SHALLOW">
!<Tip> Exclude shallow windows based on time of top of window. </Tip>
! Default = NO
! Allowed = YES, NO
! If you are using C4WE essentially as a tool to extract the waterbottom in
! deep water post-stack data, this parameter allows you to throw out shallow
! picks that need to be excluded from a far field signature estimate.
!</Help>
!
!<Help KEYWORD="EXCLUDE_TIM">
!<Tip> Exclude windows having top of window shallower than this. </Tip>
! Default = .75
! Allowed = real
! A value of about .75 (seconds) is usually about right, assuming there is
! 100 msec of data between the mute pick and the waterbottom event.
!</Help>
!
!<Help KEYWORD="WAVELETS_OUT">
!<Tip> Trace file containing wavelet estimates. </Tip>
! Default =
! Allowed =
! Contains one or more output traces, depending on how HDR_IN is being used.
!</Help>
!
!<Help KEYWORD="WAVELETS_OUT_INFO" TYPE="DISPLAY_ONLY">
!<Tip> Status of WAVELETS_OUT. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_WAVELETS_OUT">
!<Tip> Choose WAVELETS_OUT using a file selection dialog box. </Tip>
!</Help>
!
!</HelpSection>

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module c4we_module
      use cio_module
      use conjgrad_module
      use latwin_module
      use lav_module
      use mem_module
      use mth_module
      use named_constants_module
      use pathcheck_module
      use pathchoose_module
      use pc_module
      use trcio_module
      use unix_module

      implicit none
      private
      public :: c4we_create
      public :: c4we_initialize
      public :: c4we_update
      public :: c4we_delete

!<execute_only>
      public :: c4we       ! main execution (trace processing) routine.
      public :: c4we_wrapup
!</execute_only>

      character(len=100),public,save :: C4WE_IDENT = &
'$Id: c4we.f90,v 1.18 2007/01/03 14:01:36 Stoeckley prod sps $'

!!------------------------ parameter structure ----------------------------!!
!!------------------------ parameter structure ----------------------------!!
!!------------------------ parameter structure ----------------------------!!

      type,public :: c4we_struct

      private
      logical                        :: skip_wrapup     ! wrapup flag.

      real                           :: tstrt  ! time of 1st trace sample(sec).
      integer                        :: hdr_in          ! process parameters.
      real                           :: len_op          ! process parameters.
      integer                        :: iter            ! process parameters.
      real                           :: tim_first       ! process parameters.
      logical                        :: waterbottom_defaults
      logical                        :: exclude_shallow !
      real                           :: exclude_tim     !
      character(len=FILENAME_LENGTH) :: wavelets_out    !

      integer                      :: nwih, ndpt      ! globals.
      real                         :: dt              ! globals.

      type(latwin_struct), pointer :: latwin          ! dependent variables.
      integer                      :: lun             ! dependent variables.
      logical                      :: idone           ! dependent variables.
      integer                      :: nw, it0, mlag   ! dependent variables.
      integer                      :: numwvlts        ! dependent variables.
      integer                      :: nlive           ! dependent variables.
      double precision             :: hdr_group_val   ! dependent variables.
      double precision, pointer    :: c2(:)           ! dependent variables.
      double precision, pointer    :: c4(:)           ! dependent variables.


      type(pathchoose_struct),pointer  :: dialog_out    !
      type(trcio_struct),pointer       :: trc_out       !

      end type c4we_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(c4we_struct),pointer,save :: object      ! needed for traps.

!!----Following constants used in iterative solutions for estimating wavelet
      integer, parameter           :: num_conjgrad = 10
      integer, parameter           :: num_line_search = 15

!!----Following constant used in converting variables to double precision
      !integer, parameter           :: kdble = kind(0.0D0)
      integer, parameter           :: kdble = 8

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine c4we_create (obj)
      implicit none
      type(c4we_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%c2)
      nullify (obj%c4)
      nullify (obj%latwin)
      nullify (obj%dialog_out) ! jpa
      nullify (obj%trc_out) ! jpa

      call latwin_create(obj%latwin)

      call pathchoose_create(obj%dialog_out, 'WAVELETS_OUT', 'trc')

      call c4we_initialize (obj)
      return
      end subroutine c4we_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine c4we_delete (obj)
      implicit none
      type(c4we_struct),pointer :: obj       ! arguments

!<execute_only>
      call c4we_wrapup (obj)
!</execute_only>

      call mem_free (obj%c2)
      call mem_free (obj%c4)

      call latwin_delete (obj%latwin)

      call pathchoose_delete (obj%dialog_out)

      deallocate(obj)
      return
      end subroutine c4we_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine c4we_initialize (obj)
      implicit none
      type(c4we_struct),intent(inout) :: obj          ! arguments

      integer                         :: nw2          ! local variables

      obj%ndpt = 1
      obj%dt   = 1.0
      call pc_get_global ('NDPT', obj%ndpt)
      call pc_get_global ('DT'  , obj%dt)

      obj%hdr_in    =  0
      obj%iter      =  10
      obj%len_op    =  0.12
      nw2 = nint(0.5 * min(0.5*obj%dt*(obj%ndpt-1), abs(obj%len_op)) / obj%dt)
      nw2 = max (nw2, 1)
      obj%nw = 2*nw2 + 1
      obj%len_op = (obj%nw-1) * obj%dt
      obj%it0 = 1 + nw2
      obj%tim_first = (1 - obj%it0) * obj%dt

      obj%waterbottom_defaults = .false.
      obj%exclude_shallow      = .false.
      obj%exclude_tim          = .75
      obj%wavelets_out         = ' '

      call latwin_initialize (obj%latwin)

      call c4we_update (obj)
      return
      end subroutine c4we_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine c4we_update (obj)
      implicit none
      type(c4we_struct),intent(inout),target :: obj            ! arguments

      integer               :: numtr, nscratch, nstore         ! local
      integer               :: nw2, nwm                        ! local
      integer               :: nw_last, it0_last               ! local
      integer               :: istat                           ! local


      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.
      obj%lun = pc_get_lun()      ! for printing

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      if(pathchoose_update(obj%dialog_out,obj%wavelets_out)) return

      numtr    = 0
      obj%nwih = 0
      obj%ndpt = 0
      obj%dt   = -1.0
      call pc_get_global ('NUMTR'    , numtr)
      call pc_get_global ('NWIH'     , obj%nwih)
      call pc_get_global ('NDPT'     , obj%ndpt)
      call pc_get_global ('TSTRT'    , obj%tstrt)
      call pc_get_global ('DT'       , obj%dt)
      if (numtr == 0) then
        call pc_error ("NUMTR global hasn't been set.")
      else if (numtr > 1) then
        call pc_error ("Traces input to C4WE must arrive one at a time. &
                       &Please insert an UNGATHER before C4WE.")
      end if
      if (obj%nwih == 0) call pc_error ("NWIH global hasn't been set.")
      if (obj%ndpt == 0) then
        call pc_error ("NDPT global hasn't been set.")
        obj%ndpt = 1
      end if
      if (obj%dt <= 0.0) then
        call pc_error ("DT global hasn't been set.")
        obj%dt = 1.0
      end if

      call pc_get ('WATERBOTTOM_DEFAULTS' , obj%waterbottom_defaults)
      call pc_get ('HDR_IN'               , obj%hdr_in)
      call pc_get ('LEN_OP'               , obj%len_op)
      call pc_get ('ITER'                 , obj%iter)
      call pc_get ('TIM_FIRST'            , obj%tim_first)
      call pc_get ('EXCLUDE_SHALLOW'      , obj%exclude_shallow)
      call pc_get ('EXCLUDE_TIM'          , obj%exclude_tim)
      call pc_get ('WAVELETS_OUT'         , obj%wavelets_out, &
                                               c4we_wavelets_out_trap)


!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      obj%hdr_in = min (max(obj%hdr_in,0), obj%nwih)
      obj%iter = max (abs(obj%iter), 1)

      nw_last  = obj%nw
      it0_last = obj%it0
      nw2 = nint(0.5 * min(0.5*obj%dt*(obj%ndpt-1), abs(obj%len_op)) / obj%dt)
      nw2 = max (nw2, 1)
      obj%nw = 2*nw2 + 1
      obj%len_op = (obj%nw-1) * obj%dt
      obj%it0 = nint(1.0 - obj%tim_first/obj%dt)
      obj%it0 = min (max(obj%it0,1), obj%nw)
      if (pc_get_update_state() == PC_GUI) then
        if (obj%nw/=nw_last .and. obj%it0==it0_last) then
          obj%it0 = 1 + nw2
          call pc_info ("TIM_FIRST set to -LEN_OP/2. You may reset &
                        &TIM_FIRST manually if necessary.")
          call pc_jump_field ('TIM_FIRST')
        end if
      end if
      obj%tim_first = (1 - obj%it0) * obj%dt


      call pathcheck('WAVELETS_OUT', object%wavelets_out,          &
             ext='trc', required=.true., show=PATHCHECK_INFO_OUTPUT)

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

!!----Determine memory usage
      nwm      = obj%nw - 1
      obj%mlag = (nwm**3 + 6*(nwm**2) + 11*nwm + 6) / 6
      nstore = 2*obj%nw + 2*obj%mlag
      nscratch = 7*obj%nw + 4*obj%mlag + obj%mlag*obj%nw

      if(obj%waterbottom_defaults) then
        obj%len_op             =  .20
        obj%tim_first          = -.10
        obj%exclude_shallow    = .true.
        obj%exclude_tim        = .75
        call pc_put ('WIN_TIM_LEN', .40)
      end if

!!----Write process parameters
      call pc_put ('WATERBOTTOM_DEFAULTS' , obj%waterbottom_defaults)
      call pc_put ('HDR_IN'               , obj%hdr_in)
      call pc_put ('LEN_OP'               , obj%len_op)
      call pc_put ('ITER'                 , obj%iter)
      call pc_put ('TIM_FIRST'            , obj%tim_first)
      call pc_put ('EXCLUDE_SHALLOW'      , obj%exclude_shallow)
      call pc_put ('EXCLUDE_TIM'          , obj%exclude_tim)
      call pc_put ('WAVELETS_OUT'         , obj%wavelets_out)

!!----Update the LATWIN primitive
      call latwin_update (obj%latwin)

!!----Write globals & control parameters
      call pc_put_global  ('TSTRT'        , obj%tstrt)

      call pc_put_control ('NSCRATCH'     , nscratch)
      call pc_put_control ('NSTORE'       , nstore)

      if(obj%waterbottom_defaults) then
        call pc_put_sensitive_field_flag ('LEN_OP'          , .false.)
        call pc_put_sensitive_field_flag ('TIM_FIRST'       , .false.)
        call pc_put_sensitive_field_flag ('WIN_TIM_LEN'     , .false.)
        call pc_put_sensitive_field_flag ('EXCLUDE_SHALLOW' , .false.)
        call pc_put_sensitive_field_flag ('EXCLUDE_TIM'     , .false.)
      else
        call pc_put_sensitive_field_flag ('LEN_OP'          , .true.)
        call pc_put_sensitive_field_flag ('TIM_FIRST'       , .true.)
        call pc_put_sensitive_field_flag ('WIN_TIM_LEN'     , .true.)
        call pc_put_sensitive_field_flag ('EXCLUDE_SHALLOW' , .true.)
        if(obj%exclude_shallow) then
          call pc_put_sensitive_field_flag ('EXCLUDE_TIM' , .true.)
        else
          call pc_put_sensitive_field_flag ('EXCLUDE_TIM' , .false.)
        end if
      end if

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

      call mem_free (obj%c2)
      call mem_free (obj%c4)

      obj%idone         = .false.
      obj%numwvlts      = 0
      obj%nlive         = 0
      obj%hdr_group_val = -1.0D30

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.          ! needed for the wrapup routine.

      call mem_alloc (obj%c2, obj%nw)
      call mem_alloc (obj%c4, obj%mlag)

      obj%c2 = 0.0D0
      obj%c4 = 0.0D0

!!----Open output trace file.
      call pc_print('C4WE: opening ' // trim(obj%wavelets_out) // ' ...')
      obj%trc_out => trcio_open(                                  &
        trim(obj%wavelets_out), 'w', srate=obj%dt,                &
        nwih=HDR_NOMINAL_SIZE, ndpt=obj%nw, nbits=32, nbitshd=64, &
        strt_val=obj%tim_first)
      call pc_print('C4WE: ... successful open ...')

      istat = cio_set_file_ext_size(250000000)
      if(istat /= cio_ok) then
        call pc_error('C4WE: Error setting file extent size.')
      endif
      call cio_set_file_space_commit(0)     ! no need to reserve disk space


!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine c4we_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

      subroutine c4we_wavelets_out_trap()
      implicit none

      call pathcheck('WAVELETS_OUT', object%wavelets_out,          &
             ext='trc', required=.true., show=PATHCHECK_INFO_OUTPUT)

      end subroutine c4we_wavelets_out_trap


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine c4we (obj,ntr,hd,tr)
      implicit none
      type(c4we_struct) ,intent(inout) :: obj              ! arguments
      integer           ,intent(inout) :: ntr              ! arguments
      double precision  ,intent(inout) :: hd(:,:)          ! arguments
      real              ,intent(inout) :: tr(:,:)          ! arguments

      logical           :: findwvlt, newgroup              ! local
      real              :: wvlt(obj%nw)                    ! local
      real              :: mintest, maxtest                ! local
      double precision  ::       header_newval ! local

      double precision  :: hd_local(HDR_NOMINAL_SIZE)      ! local
      integer           :: istat                           ! local


!---- proceed
      findwvlt = .false.
      newgroup = .false.
      if (ntr == NEED_TRACES) then
        if (obj%idone) then
          call c4we_wrapup (obj)
          ntr = NO_MORE_TRACES
        end if
        return
      else if (ntr == NO_MORE_TRACES) then
        obj%idone = .true.
        if (obj%nlive > 0) then
          findwvlt  = .true.
        else
          call c4we_wrapup (obj)
          return
        end if
      else if (ntr > 1) then
        call pc_print ('C4WE received', ntr, 'traces in one call. This &
                    &process requires traces to be input one at a time.')
        call c4we_wrapup (obj)
        ntr = FATAL_ERROR
        return
      else  ! If come here, must have ntr == 1
        if (obj%hdr_in > 0) then
          if (hd(obj%hdr_in,1) /= obj%hdr_group_val) then
            newgroup = .true.
            header_newval = hd(obj%hdr_in,1)
            if (obj%nlive > 0) findwvlt = .true.
          end if
        end if
      end if

!!----If we get here, then we have an input trace to process and/or we
!!----must compute an output wavelet.

      if (findwvlt) then
        call c4we_estimate_wavelet (obj, wvlt)   ! estimate

        mintest = minval(wvlt)
        maxtest = maxval(wvlt)
        if(abs(mintest) > abs(maxtest)) maxtest = -mintest

        if(maxtest /= 0) wvlt = wvlt / maxtest   ! normalize
      end if

      if (newgroup) then  ! Zero the cumulants if starting a new group
        obj%nlive = 0
        obj%c2 = 0.0D0
        obj%c4 = 0.0D0
      end if

      if (ntr == 1) call c4we_accumulate_sums (obj, hd(:,1), tr(:,1))

!-----Old trace flow logic.
!     if (findwvlt) then  ! set header and trace for output wavelet
!       hd(:obj%nwih,1) = 0.0D0
!       hd(HDR_SEQUENCE,1)        = obj%numwvlts
!       hd(HDR_TOP_MUTE,1)        = 1
!       hd(HDR_CURRENT_GROUP,1)   = obj%numwvlts
!       hd(HDR_CURRENT_CHANNEL,1) = 1
!       hd(HDR_BOTTOM_MUTE,1)     = obj%nw
!       if (obj%hdr_in > 0) hd(obj%hdr_in,1) = obj%hdr_group_val
!       tr(1:obj%nw,1) = wvlt
!       tr(obj%nw+1:obj%ndpt,1) = 0.0
!       call lav_set_hdr (hd(:,1), tr(:,1), obj%ndpt)
!       ntr = 1
!     else
!       ntr = NEED_TRACES
!     end if
!
!-----New trace flow logic (SMCook).
      if (findwvlt) then
        hd_local(:obj%nwih)           = 0.0D0
        hd_local(HDR_SEQUENCE)        = obj%numwvlts
        hd_local(HDR_TOP_MUTE)        = 1
        hd_local(HDR_CURRENT_GROUP)   = obj%numwvlts
        hd_local(HDR_CURRENT_CHANNEL) = 1
        hd_local(HDR_BOTTOM_MUTE)     = obj%nw
        if(obj%numwvlts == 1) hd_local(HDR_SCRATCH_30) = 1
        call lav_set_hdr (hd_local, wvlt, obj%nw)
        istat = trcio_write_trace(obj%trc_out, hd_local,       &
                        wvlt, tnum=nint(hd_local(HDR_SEQUENCE)))
        if(istat /= trcio_ok) then
          call pc_error('C4WE: trcio_write_trace istat = ', istat)
          return
        end if
      end if

      if (newgroup) obj%hdr_group_val = header_newval

      return
      end subroutine c4we

!!------------------------ c4we_accumulate_sums ---------------------------!!
!!------------------------ c4we_accumulate_sums ---------------------------!!
!!------------------------ c4we_accumulate_sums ---------------------------!!

      subroutine c4we_accumulate_sums (obj, hd, tr)
      implicit none
      type(c4we_struct),intent(inout) :: obj               ! arguments
      double precision ,intent(in)    :: hd(:)             ! arguments
      real             ,intent(in)    :: tr(:)             ! arguments

      double precision      :: aver, sum                   ! local
      integer               :: itwin, ibwin, i, j, k       ! local
      integer               :: idx, kk                     ! local
      real                  :: tr_loc(obj%ndpt)            ! local

      if (hd(HDR_LAV) <= 0D0) return

      tr_loc = tr

      call latwin_get_window ( &
        obj%latwin, hd, tr_loc, index1=itwin, index2=ibwin)
      ibwin = min (ibwin, obj%ndpt-obj%nw+1)
      if (ibwin < itwin) return

      if(obj%exclude_shallow) then
        if(itwin < obj%exclude_tim / obj%dt) return   ! SMCook
      end if

      aver = 0.0D0
      do i = itwin, ibwin+obj%nw-1
        aver = aver + real(tr_loc(i),kdble)
      end do
      aver = aver / (ibwin + obj%nw - itwin)
      do i = itwin, ibwin+obj%nw-1
        tr_loc(i) = tr_loc(i) - aver
      end do

      do i = 1, obj%nw
        sum = 0.0D0
        do j = itwin, ibwin
          if (i == 1) then
            if (tr_loc(j) /= 0.0) obj%nlive = obj%nlive + 1
          end if
          sum = sum  +  real(tr_loc(j),kdble) * real(tr_loc(j+i-1),kdble)
        end do
        obj%c2(i) = obj%c2(i) + sum
      end do

      idx = 0
      do k = 0, obj%nw-1
        do j = 0, k
          do i = 0, j
            if (i==0 .and. k>0 .and. k==j) cycle
            idx = idx + 1
            sum = 0.0D0
            do kk= itwin,ibwin
              sum = sum + &
                        real(tr_loc(kk),kdble) * real(tr_loc(kk+i),kdble) * &
                        real(tr_loc(kk+j),kdble) * real(tr_loc(kk+k),kdble)
            end do
            obj%c4(idx) = obj%c4(idx) + sum
          end do
        end do
      end do

      return
      end subroutine c4we_accumulate_sums

!!------------------------ c4we_estimate_wavelet --------------------------!!
!!------------------------ c4we_estimate_wavelet --------------------------!!
!!------------------------ c4we_estimate_wavelet --------------------------!!

      subroutine c4we_estimate_wavelet (obj, wvlt)
      implicit none
      type(c4we_struct),intent(inout) :: obj               ! arguments
      real             ,intent(out)   :: wvlt(obj%nw)      ! arguments

      integer             :: i, j, k, idx    , iter, l ! local
      double precision    :: scale2, scale4, pwr, gm4      ! local
      double precision    :: sums(obj%nw)                  ! local
      real                :: kurtosis, c4p, junk           ! local
      real                :: alpha, err                    ! local
      real                :: dw(obj%nw)                    ! local
      real                :: wtemp(obj%nw)                 ! local
      real                :: m4(obj%mlag)                  ! local
      real                :: rv(obj%mlag)                  ! local
      real                :: am(obj%mlag, obj%nw)          ! local

!!----Initialize (failure to do this can cause floating point errors)
      sums  = 0.0
      dw    = 0.0
      wtemp = 0.0
      m4    = 0.0
      rv    = 0.0
      am    = 0.0

!!----Normalize and taper the cumulant before estimating wavelet

      pwr = obj%c2(1) / obj%nlive
      scale2 = 1.0D0 / obj%c2(1)
      obj%c2 = obj%c2 * scale2
      write (obj%lun,*)
      if (obj%hdr_in > 0) then
        write (obj%lun,*) 'C4WE wavelet estimation for trace group', &
                          obj%hdr_group_val
      else
        write (obj%lun,*) 'C4WE wavelet estimation using all input traces'
      end if
      write (obj%lun,*) 'Number of live trace samples used =', obj%nlive
      write (obj%lun,*) 'Average trace power =', real(pwr)
      scale4 = 1.0D0 / (obj%nlive * pwr**2)
      kurtosis = scale4*obj%c4(1) - 3.0D0
      write (obj%lun,*) 'Kurtosis of trace group =', kurtosis
      write (obj%lun,*) 'Number of cumulant lags to match =', obj%mlag

      idx = 0
      do k = 0, obj%nw-1
        do j = 0, k
          do i = 0, j
            if (i==0 .and. k>0 .and. k==j) cycle
            idx = idx + 1
            gm4 = obj%c2(i+1) * obj%c2(k-j+1)  +  &
                  obj%c2(j+1) * obj%c2(k-i+1)  +  &
                  obj%c2(k+1) * obj%c2(j-i+1)
            obj%c4(idx)= (scale4*obj%c4(idx) - gm4) * c4we_laz(k,obj%nw)
          end do
        end do
      end do

!!----Compute the energy in the cumulant
      c4p = dot_product (obj%c4(2:obj%mlag), obj%c4(2:obj%mlag))
      c4p = c4p / (obj%mlag - 1)

!!-------------------------------------------------------------
!!    Solution for wavelet from the fourth order cumulant
!!-------------------------------------------------------------

!!----Initialize the wavelet to an impulse plus Gaussian random values
      do i = 1, obj%nw-2, 2
        call mth_gauss_ranf (0.1, wvlt(i), wvlt(i+1))
      end do
      call mth_gauss_ranf (0.1, wvlt(obj%nw), junk)
      wvlt(obj%it0) = 1.0

!!----Iterate over changes to the wavelet
      do iter = 1, obj%iter

!!------Compute the fourth moment of the wavelet
        call c4we_m4w (obj%nw, wvlt, m4)

!!------Fill the right side vector with c4-m4
        do k = 2, obj%mlag
          rv(k) = obj%c4(k) - m4(k)
        end do

!!------Fill left side matrix AM (Jacobean of m4 relative to w)
        idx = 0
        do k = 0, obj%nw-1
          do j = 0, k
            do i = 0, j
              if (i==0 .and. k>0 .and. k==j) cycle
              idx = idx + 1
              sums = 0.0D0
              do l = 1, obj%nw-k
                sums(l) = sums(l)  +  &
                                  real(wvlt(l+i),kdble) * &
                                  real(wvlt(l+j),kdble) * &
                                  real(wvlt(l+k),kdble)
              end do
              do l = i+1, obj%nw-k+i
                sums(l) = sums(l)  +  &
                                  real(wvlt(l-i),kdble) * &
                                  real(wvlt(l-i+j),kdble) * &
                                  real(wvlt(l-i+k),kdble)
              end do
              do l = j+1, obj%nw-k+j
                sums(l) = sums(l)  +  &
                                  real(wvlt(l-j),kdble) * &
                                  real(wvlt(l-j+i),kdble) * &
                                  real(wvlt(l-j+k),kdble)
              end do
              do l = k+1, obj%nw
                sums(l) = sums(l)  +  &
                                  real(wvlt(l-k),kdble) * &
                                  real(wvlt(l-k+i),kdble) * &
                                  real(wvlt(l-k+j),kdble)
              end do
              am(idx,:) = sums
            end do
          end do
        end do

!!------Initialize conjugate gradient solution to Gaussian random noise
        do i = 1, obj%nw-2, 2
          call mth_gauss_ranf (0.1, wtemp(i), wtemp(i+1))
        end do
        call mth_gauss_ranf (0.1, wtemp(obj%nw), junk)

!!------Solve for change in the wavelet (DW) using conjugate gradients
        call conjgrad (obj%nw, dw, obj%mlag-1, rv(2:obj%mlag), &
                       am(2:obj%mlag, :), nter=num_conjgrad,   &
                       prwh=1.0e-10, xstart=wtemp)

!!------Perform line search using full nonlinear objective
        call c4we_line_search (obj, wvlt, dw, wtemp, alpha, m4, err)

!!------Update the wavelet by amount DW
        write (obj%lun,*) 'Iteration =', iter, '   Error =',  &
                          sqrt(err/c4p), '   Alpha =', alpha
        wvlt = wvlt + alpha*dw

      end do        ! End iterations over wavelet changes

      obj%numwvlts = obj%numwvlts + 1

      return
      end subroutine c4we_estimate_wavelet

!!------------------------- subroutine c4we_m4w ----------------------------!!
!!------------------------- subroutine c4we_m4w ----------------------------!!
!!------------------------- subroutine c4we_m4w ----------------------------!!

      subroutine c4we_m4w (nw, w, m4)
!---------------------------------------------------------------------------
!     Fourth moment of a wavelet. The wavelet and 4th moment are stored in
!     single precision, but all arithmetic and intermediate results are in
!     double precision.
!---------------------------------------------------------------------------
      implicit none
      integer      , intent(in)  :: nw                ! arguments
      real         , intent(in)  :: w(nw)             ! arguments
      real         , intent(out) :: m4(:)             ! arguments
!
      integer          :: idx, i, j, k, kk            ! local variables
      double precision :: sum                         ! local variables
!
      m4 = 0.0
      idx = 0
      do k = 0, nw-1
        do j = 0, k
          do i = 0, j
            if (i==0 .and. k>0 .and. k==j) cycle
            idx = idx + 1
            sum = 0.0D0
            do kk = 1, nw-k
              sum = sum + &
                          real(w(kk),kdble) * real(w(kk+i),kdble) * &
                          real(w(kk+j),kdble) * real(w(kk+k),kdble)
            end do
            m4(idx) = sum
          end do
        end do
      end do
!
      return
      end subroutine c4we_m4w

!!-------------------------- function c4we_laz -----------------------------!!
!!-------------------------- function c4we_laz -----------------------------!!
!!-------------------------- function c4we_laz -----------------------------!!

      function c4we_laz (k, nw)  result (laz)
!----------------------------------------------------------------
!     Computes the taper for the cumulant.
!     Compute the number of overlapping samples in the fourth
!     moment of a wavelet of length nw at lags (i,j,k)
!     where lags are in the range:
!                  0  <=  i  <=  j  <=  k  <=  nw-1
!----------------------------------------------------------------
      implicit none
      integer , intent(in) :: k                         ! arguments
      integer , intent(in) :: nw                        ! arguments
      real                 :: laz                       ! result

      laz = real(nw-k) / real(nw)

      return
      end function c4we_laz

!!---------------------------- c4we_objective ------------------------------!!
!!---------------------------- c4we_objective ------------------------------!!
!!---------------------------- c4we_objective ------------------------------!!

      subroutine c4we_objective (obj, wvlt, dw, wtemp, alpha, m4, err)
!--------------------------------------------------------------------------
!     Computes the mean squared error between C4 and M4.  While M4 is of
!     type single precision, C4 is part of data structure OBJ and is
!     double precision. Thus, the calculation here is actually in double
!     precision.  Note: WTEMP and M4 are passed through the argument list
!     in order to use memory allocated elsewhere but are actually used
!     as scratch arrays. Also note that although c4we_line_search and
!     c4we_objective appear to have identical argument lists, ALPHA has
!     different intent in these two routines.
!--------------------------------------------------------------------------
      implicit none
      type(c4we_struct),intent(in)    :: obj               ! arguments
      real             ,intent(in)    :: wvlt(obj%nw)      ! arguments
      real             ,intent(in)    :: dw(obj%nw)        ! arguments
      real             ,intent(inout) :: wtemp(obj%nw)     ! arguments
      real             ,intent(in)    :: alpha             ! arguments
      real             ,intent(inout) :: m4(obj%mlag)      ! arguments
      real             ,intent(out)   :: err               ! arguments



!!----Form wavelet perturbation
      wtemp = wvlt + alpha*dw

!!----Compute the fourth moment
      call c4we_m4w (obj%nw, wtemp, m4)

!!----Compute the mean squared error
      err = sum((obj%c4(2:obj%mlag)-m4(2:obj%mlag))**2) / (obj%mlag - 1)

      return
      end subroutine c4we_objective

!!--------------------------- c4we_line_search -----------------------------!!
!!--------------------------- c4we_line_search -----------------------------!!
!!--------------------------- c4we_line_search -----------------------------!!

      subroutine c4we_line_search (obj, wvlt, dw, wtemp, alpha, m4, err)
!--------------------------------------------------------------------------
!     Does a bisecting line search for the minimum in an objective.
!     Note: WTEMP and M4 are passed through the argument list in order
!     to use memory allocated elsewhere but are actually used as scratch
!     arrays in c4we_objective. Also note that although c4we_line_search
!     and c4we_objective appear to have identical argument lists, ALPHA
!     has different intent in these two routines.
!--------------------------------------------------------------------------
      implicit none
      type(c4we_struct),intent(in)    :: obj               ! arguments
      real             ,intent(in)    :: wvlt(obj%nw)      ! arguments
      real             ,intent(in)    :: dw(obj%nw)        ! arguments
      real             ,intent(inout) :: wtemp(obj%nw)     ! arguments
      real             ,intent(out)   :: alpha             ! arguments
      real             ,intent(inout) :: m4(obj%mlag)      ! arguments
      real             ,intent(out)   :: err               ! arguments

      real   ,parameter :: step1 = 0.01                    ! local
      integer           :: imin, iter                      ! local
      real              :: alp(5), alpmin, emin            ! local
      real              :: alptest, etest                  ! local

      alp(1) = 0.0
      alpmin = 0.0
      call c4we_objective (obj, wvlt, dw, wtemp, alp(1), m4, emin)

      alptest = step1
      do
        call c4we_objective (obj, wvlt, dw, wtemp, alptest, m4, etest)
        if (etest > emin) exit
        alpmin = alptest
        emin = etest
        alptest = 2.0 * alptest
      end do

!!----Store the 2nd and 3rd points
      alp(3) = alpmin
      alp(5) = alptest
      imin = 3

!!----Bisect the three points and save the three about the minimum
      do iter = 1, num_line_search
        alp(2) = 0.5 * (alp(1) + alp(3))
        if (alp(2) > alp(1)) then
          call c4we_objective (obj, wvlt, dw, wtemp, alp(2), m4, etest)
          if (etest < emin) then
            emin = etest
            imin = 2
          end if
        endif
        alp(4) = 0.5 * (alp(3) + alp(5))
        if (imin == 3) then
          call c4we_objective (obj, wvlt, dw, wtemp, alp(4), m4, etest)
          if (etest < emin) then
            emin = etest
            imin = 4
          endif
        end if
!!------Store three points about the minimum
        alp( (/ 1, 3, 5 /) ) = alp(imin-1:imin+1)
        imin = 3
      end do

      alpha = alp(3)
      err = emin

      return
      end subroutine c4we_line_search

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine c4we_wrapup (obj)
      implicit none
      type(c4we_struct),intent(inout) :: obj       ! arguments

      integer          :: istat                    ! local


      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      call pc_print ('C4WE wrapup: estimated', obj%numwvlts, 'wavelets.')

      istat = trcio_close(obj%trc_out)
      if(istat /= trcio_ok) then
        call pc_error('C4WE: trcio_close istat = ', istat)
        return
      end if


      return
      end subroutine c4we_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module c4we_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
