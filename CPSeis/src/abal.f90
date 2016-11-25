!<CPS_v1 type="PROCESS"/>
!!-------------------------------- abal.f90 --------------------------------!!
!!-------------------------------- abal.f90 --------------------------------!!
!!-------------------------------- abal.f90 --------------------------------!!

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
! Name       : ABAL        (Amplitude BALance)
! Category   : amplitude_mod
! Written    : 1990-02-02   by: Bob Baumel
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Perform various trace- and gather-based amplitude balances.
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
! ABAL is a general trace scaling process typically used to prepare CMPs for 
! AVA analysis.  All operations are based on an amplitude statistic measurement
! for each trace within a specified time window using either the median 
! absolute amplitude or the average absolute amplitude.  (The term "amplitude" 
! in the balance of this documentation refers to this amplitude statistic.)
!
! If MODE = SINGLE, ABAL performs a simple trace equalization on single traces 
! individually.  Traces may be input in any order, gathered or as single traces.
!
! If MODE = CMP or SHOT, traces must be input in CMP or SHOT gathers, 
! respectively.  ABAL does a polynomial fit of the amplitude trend with offset
! and optionally scales traces within a gather to comply with the fitted trend.
! ABAL will also optionally scale gathers to each other to make their 
! amplitudes at a specified offset equal to unity.
!
! (In SHOT mode, ABAL fits the amplitude trend to offset, while in CMP mode it 
! fits the amplitude trend to offset squared.  This permits ABAL to handle 
! traces with signed offset in CMPs.)
! 
!
! Time Windows
!
! The offset-dependent windowing scheme used by ABAL is controlled by LATWIN.
! Please refer to the LATWIN documentation for details.
!
! ABAL (via LATWIN) does not currently support defining windows using normal
! moveout velocities like it used to on the VAX.
!
!
! Polynomial Fitting
!
! The polynomial fitting (MODE = CMP or SHOT) is done in two passes.  After
! pass 1, traces with amplitude deviating sufficiently from the fitted trend 
! are killed.  In pass 2, the remaining traces are fitted again, and are then 
! optionally scaled to match the polynomial obtained this time.  Also at this 
! point, traces are optionally scaled to balance gathers with each other.
!
!
! Diagnostic Table #1
!
! If MODE = CMP or SHOT, ABAL prints a set of summary statistics at wrapup time
! to the .rpt file attempting to quickly communicate to the user the overall
! amplitude trend in the input data.  The column values are offset (or offset
! squared) and average pass 1 predicted amplitude.  Only 95% full gathers, i.e.
! those having at least .95*NUMTR live traces, are included in these particular
! summary computations.  The offset range for this summary is taken as the
! minimum and maximum absolute offsets actually encountered in the input.
!
! Unlike Diagnostic Table #2, this summary table uses linear amplitude units,
! and it pertains exclusively to the input data.
!
!
! Diagnostic Table #2 (optional, based on OPT_PRINT)
!
! If MODE = CMP or SHOT, ABAL can also print a diagnostic table in the .rpt file
! using one line per input trace.  Each line contains 4 numbers:  trace number
! within gather, original trace amplitude (in selected window), fitted amplitude
! after pass 1, and final adjusted amplitude after pass 2.
!
! Tables are tab-delimited for ease of use with plotting software.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS                      
!
!
! Traces should have an amplitude balance applied prior to this process, 
! preferably a deterministic balance such as TPOW or GDIV, but MVXP may be 
! satisfactory in some cases.
!
! ABAL is NOT surface consistent.  For a surface consistent amplitude balance,
! use SCAB (or possibly SCDECON).
!
!
! Warning
!
! Applying a gain to a trace based on a measurement taken some distance from 
! where the gain will be applied can have unintended and undesirable results.
! For example, traces that are generally well balanced at the bottom that 
! encounter anomalously high amplitudes shallow due to shallow gas (or 
! encounter anomalously low amplitudes shallow due to a salt lens) will have 
! the balance at the bottom disrupted if the anomalous amplitudes fall within 
! a trace equalization window.
!
! Generally, if the amplitude within the window is not representative of the 
! trace as a whole, applying the balance may give undesirable results.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS                
!
! Process is a single-trace process for MODE = SINGLE.
!
! Process is a multiple-trace process for MODE = CMP or SHOT.
!
! If MODE = CMP or SHOT, traces must be input in CMP or SHOT gathers.
!
! If MODE = SINGLE, traces may be input in any order, either gathered or as 
! single traces.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS             
!
!
! This process alters input traces.
!
! This process outputs the same traces as it receives (possibly altered).
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED      
!
! Name      Description                               Action taken
! ----      -----------                               ------------
! NWIH      number of words in header                 used but not changed
! NUMTR     max number of traces input/output         used but not changed
! GATHERED  whether traces are a legitimate gather    used but not changed
! NDPT      number of sample values in trace          used but not changed
! TSTRT     starting time on trace                    used but not changed
! DT        trace sample interval                     used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED           
! 
! Hwd#    Description         Action taken
! ----    -----------         ------------
!
!  2      HDR_TOP_MUTE        Used (if windowing tied to mute)
! 64      HDR_BOTTOM_MUTE     Used 
! 25      LAV                 Changed
!         HDR_OFFSET          Used
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date        Author      Description
!     ----        ------      -----------
!015. 2006-09-18  D. Glover   Added NULLIFY statements for Intel compiler.
!014. 2006-01-10  B. Menger   Removed Unused Variables.
!13.  2001-12-10  SMCook      Changed subroutine arguments from (*) to (:).
!12.  2001-01-09  SMCook      Improved summary printouts.  Required correcting
!                              an integer division where a floating point one
!                              was needed.  Also required the addition of a temp
!                              file to prevent trace-by-trace information from
!                              being garbled up with everything else in the .rpt
!                              file.
!                             Changed wrapped_up to skip_wrapup.
!11.  2000-12-01  SMCook      Incorporates LATWIN primitive instead of carrying
!                              it's own windowing logic.
!                             Fixed bug where AVE mode was accidentally referred
!                              to as AVG mode, causing AVE mode to be identical
!                              to MED mode.
!10.  2000-09-05  SMCook      (MAJOR) Converted to new CPS system.
! 9.  1999-01-11  Vunderink   Begin using the f90 compiler.
! 8.  1995-05-07  Baumel      Documentation changes only.
!                             Program moved from newlib to conlib.
! 7.  1995-04-28  Baumel      GBAL allows balancing at specified OFFSET.
! 6.  1995-04-04  Baumel      Add GBAL parameter.
! 5.  1995-03-31  Baumel      Recognize tail mute header word.
! 4.  1993-02-26  Baumel      Indicate killed traces in TABLES.
! 3.  1990-04-19  Baumel      Fix POWER bug for MODE=FIXED.
! 2.  1990-02-06  Baumel      Fix inconsistency in TABLES output.
! 1.  1990-02-02  Baumel      Original version. 
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
! NSCRATCH         0       amount of temporary memory needed.       
! NSTORE           0       amount of permanent memory needed.      
! IFTD           false     whether this process frees tape drives. 
! NDISK            0       disk space needed (megabytes) if large. 
! SETUP_ONLY     false     whether this process is setup-only.    
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
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
! ABAL uses the POLYFIT primitive to accomplish the curvefitting (currently a
! Gaussian-Elimination brute force matrix solution).
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES                 
!
! HDR_LAV is used internally as a flag and recomputed at the end by the lav
! primitive.
!
! Curvefitting is done after values are converted to decibel units.  This may
! help prevent overflow/underflow/precision problems with polynomial fitting,
! although this is now down double-precision in POLYFIT anyway.
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<gui_def>
!<NS ABAL Process/NC=80>
! 
!               Amplitude BALance
!
! 
! MODE=`CCCCC
!
! STAT=`CC
!
! OPT_PRINT=`CC
!
!
! OPT_SCALE=`CCCCC
! 
! OFF_SCALE=`FFFFFFF
!
!
! ORDER=`IIIII
!
!
! DB_KILL=`FFFFFFF
!
!
!<NS Window Parameters/NC=80>
!<include latwin.f90>
!
!<PARMS Window Parameters [screen2]>
!
!</gui_def>
!-------------------------------------------------------------------------------


!<HelpSection>
!-------------------------------------------------------------------------------
!
!<Help KEYWORD="MODE">
!<Tip> Whether to use CMP, SHOT or SINGLE mode. </Tip>
! Default = CMP
! Allowed = CMP     (Balance traces within CMP gathers.)
! Allowed = SHOT    (Balance traces within shot profiles.)
! Allowed = SINGLE  (Perform trace equalization on single traces individually.)
! If MODE = CMP, traces must be input in CMP gathers.
!
! If MODE = SHOT, traces must be input in SHOT gathers.
!
! If MODE = SINGLE, traces may be input in any order, either gathered or as 
! single traces.
!</Help>
!
!<Help KEYWORD="STAT">
!<Tip> Statistic for measuring amplitude in trace windows. </Tip>
! Default = MED
! Allowed = MED   (Median of absolute value.)
! Allowed = AVE   (Average of absolute value.)
!</Help>
!
!<Help KEYWORD="OPT_PRINT">
!<Tip> Whether to print 'Diagnostic Table #2' in the .rpt file. </Tip>
! Default = NO
! Allowed = YES/NO
! Warning:  use only with small datasets since this option prints a line in the
! .rpt file for each input trace.
!</Help>
!
!----------------------parameters for MODE = CMP or SHOT------------------------
!
!<Help KEYWORD="OPT_SCALE">
!<Tip> Option on how to scale traces. </Tip>
! Default = TRACE
! Allowed = TRACE    (Scale traces in each gather to the fitted trend.)
! Allowed = GATHER   (Also scale gathers to each other.)
! Allowed = NONE     (Do not scale traces.)
! If OPT_SCALE = TRACE, individual traces in each gather are scaled to the 
! fitted trend for that gather.
!
! If OPT_SCALE = GATHER, individual traces in each gather are scaled to the 
! fitted trend for that gather and gathers are scaled to make their 
! amplitudes at offset OFF_SCALE equal to unity.
!
! If OPT_SCALE = NONE, then do no scaling.  Choose this option if you want only
! to kill traces or to print 'Diagnostic Table #2' in the .rpt file or both.
!</Help>
!
!<Help KEYWORD="OFF_SCALE">
!<Tip> Offset to use when scaling gathers to each other. </Tip>
! Default = 0.0
! Allowed = real 
!</Help>
!
!<Help KEYWORD="ORDER">
!<Tip> Order of polynomial fit of AVO. </Tip>
! Default = 3
! Allowed = int > 0
! (In SHOT mode, ABAL fits the amplitude trend to offset, while in CMP mode it 
! fits the amplitude trend to offset squared.  This permits ABAL to handle 
! traces with signed offset in CMPs.)
!</Help>
!
!<Help KEYWORD="DB_KILL">
!<Tip> Threshold for killing traces. </Tip>
! Default = 15.0
! Allowed = real >= 0.0
! Kill all traces whose amplitude, relative to the fitted trend, is either 
! greater than DB_KILL db or less than -DB_KILL db.
!
! If DB_KILL = 0.0, then do not kill traces.
!</Help>
!
!---------------------------time window parameters------------------------------
!
! HANDLED BY LATWIN.
!
!-------------------------------------------------------------------------------
!</HelpSection>


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module abal_module
      use cio_module
      use interp_module
      use lav_module
      use latwin_module
      use median_module
      use mem_module
      use mth_module
      use mutehw_module
      use named_constants_module
      use pc_module
      use polyfit_module
      use string_module
      use tempname_module

      implicit none
      private
      public :: abal_create
      public :: abal_initialize
      public :: abal_update
      public :: abal_delete
!<execute_only>
      public :: abal            ! main execution (trace processing) routine
      public :: abal_wrapup
!</execute_only>


      character(len=100),public,save :: ABAL_IDENT = &
       '$Id: abal.f90,v 1.15 2006/09/18 13:32:34 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      integer, parameter            :: NTEST = 30

      type,public :: abal_struct              
 
        private
        logical                            :: skip_wrapup  ! wrapup flag.

        logical                            :: gathered     ! global
        integer                            :: numtr        ! global
        integer                            :: nwih         ! global
        integer                            :: ndpt         ! global
        real                               :: dt           ! global
        real                               :: tstrt        ! global

        character(len=6)                   :: mode         ! process parameter
        character(len=3)                   :: stat         ! process parameter
        logical                            :: opt_print    ! process parameter

        character(len=6)                   :: opt_scale    ! process parameter
        real                               :: off_scale    ! process parameter

        type(latwin_struct), pointer       :: latwin       ! process parameter

        integer                            :: order        ! process parameter
        real                               :: db_kill      ! process parameter

        integer                            :: off_pts      ! dependent

        character(len=120)                 :: tempname
        integer                            :: templun

        integer                            :: ntrace, nkill, ngrp

        real               :: minoff, maxoff
        integer            :: pass1fail1, pass1fail2
        integer            :: pass2fail1, pass2fail2

        real               :: coef1(50)
        integer            :: ncoef1

      end type abal_struct


!!----------------------------- interfaces ---------------------------------!!
!!----------------------------- interfaces ---------------------------------!!
!!----------------------------- interfaces ---------------------------------!!


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
      type(abal_struct),pointer,save :: object      ! needed for traps.


      integer,parameter      :: mode_noptions = 3
      character(len=6),save  :: mode_options(mode_noptions) 
      data mode_options/'CMP', 'SHOT', 'SINGLE'/

      integer,parameter      :: stat_noptions = 2
      character(len=6),save  :: stat_options(stat_noptions) 
      data stat_options/'MED', 'AVE'/

      integer,parameter      :: opt_scale_noptions = 3
      character(len=6),save  :: opt_scale_options(opt_scale_noptions) 
      data opt_scale_options/'TRACE', 'GATHER', 'NONE'/

      contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine abal_create (obj)
      implicit none
      type(abal_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify (obj%latwin) ! jpa

      call latwin_create (obj%latwin, no_mute = .false.)

      call abal_initialize (obj)

      end subroutine abal_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


      subroutine abal_delete (obj)
      implicit none
      type(abal_struct),pointer :: obj       ! arguments

!<execute_only>
      call abal_wrapup (obj)
!</execute_only>

      call latwin_delete (obj%latwin)

      deallocate(obj)

      end subroutine abal_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


      subroutine abal_initialize (obj)
      implicit none
      type(abal_struct),intent(inout) :: obj       ! arguments


      obj%mode      = 'CMP'
      obj%stat      = 'MED'
      obj%opt_print = .false.

      obj%opt_scale = 'TRACE'
      obj%off_scale = 0.

      obj%order     = 3
      obj%db_kill   = 15.

      obj%ntrace    = 0
      obj%nkill     = 0
      obj%ngrp      = 0

      call latwin_initialize (obj%latwin)

      call abal_update (obj)

      end subroutine abal_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


      subroutine abal_update (obj)
      implicit none
      type(abal_struct),intent(inout),target :: obj             ! arguments

      character(len=120)   :: str
      integer              :: itmp, nwin


      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      nwin = 1
      call latwin_update (obj%latwin, nwin)

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
      call pc_get_global ('gathered', obj%gathered) ! whether properly gathered.
      call pc_get_global ('numtr'   , obj%numtr)    ! max number of traces.
      call pc_get_global ('nwih'    , obj%nwih)     ! number of header words.
      call pc_get_global ('ndpt'    , obj%ndpt)     ! number of trace samples.
      call pc_get_global ('dt'      , obj%dt)       ! start time
      call pc_get_global ('tstrt'   , obj%tstrt)    ! start time


      call pc_get ('mode'     , obj%mode)
      call pc_get ('stat'     , obj%stat)
      call pc_get ('opt_print', obj%opt_print)

      call pc_get ('opt_scale' ,obj%opt_scale)
      call pc_get ('off_scale' ,obj%off_scale)

      call pc_get ('order'    ,obj%order)
      call pc_get ('db_kill'  ,obj%db_kill)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
      if(pc_verify_screen('screen1')) then
        call abal_verify_parameters(obj)
      end if

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field ('mode'     , mode_options, mode_noptions)
      call pc_put_options_field ('stat'     , stat_options, stat_noptions)
      call pc_put_options_field ('opt_scale', &
                                    opt_scale_options, opt_scale_noptions)

      call pc_put ('mode'     , obj%mode)
      call pc_put ('stat'     , obj%stat)
      call pc_put ('opt_print', obj%opt_print)

      call pc_put ('opt_scale' ,obj%opt_scale)
      call pc_put ('off_scale' ,obj%off_scale)

      call pc_put ('order'    ,obj%order)
      call pc_put ('db_kill'  ,obj%db_kill)

!---- sensitivities: practically everything is turned off for mode SINGLE
      if(obj%mode == 'SINGLE') then
        call pc_put_sensitive_field_flag('opt_print',.false.)
        call pc_put_sensitive_field_flag('opt_scale',.false.)
        call pc_put_sensitive_field_flag('off_scale',.false.)
        call pc_put_sensitive_field_flag('order'    ,.false.)
        call pc_put_sensitive_field_flag('db_kill'  ,.false.)
      else
        call pc_put_sensitive_field_flag('opt_print',.true.)
        call pc_put_sensitive_field_flag('opt_scale',.true.)
        call pc_put_sensitive_field_flag('off_scale',.true.)
        call pc_put_sensitive_field_flag('order'    ,.true.)
        call pc_put_sensitive_field_flag('db_kill'  ,.true.)

        if(obj%opt_scale == 'GATHER') then
          call pc_put_sensitive_field_flag('off_scale',.true.)
        else
          call pc_put_sensitive_field_flag('off_scale',.false.)
        end if
      end if

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.


!---- precompute an array of offsets to be used for statistics in wrapup
!---- printout (if mode /= 'SINGLE')

      obj%ncoef1 = 0       ! counter

      obj%minoff = +1e20
      obj%maxoff = -1e20

      obj%pass1fail1 = 0   ! no points for first pass
      obj%pass2fail1 = 0   ! no points for second pass
      obj%pass1fail2 = 0   ! poly fit failure for first pass
      obj%pass2fail2 = 0   ! poly fit failure for second pass

      obj%coef1 = 0.0

!---- open write-only tempfile to hold the printout destined for the .rpt file
      obj%tempname = tempname('tmpfile_abal')

      obj%templun  = cio_fopen(obj%tempname,'w')

      itmp = cio_fputline('-------------------', 120, obj%templun)
      itmp = cio_fputline('DIAGNOSTIC TABLE #2', 120, obj%templun)
      itmp = cio_fputline('-------------------', 120, obj%templun)

      str = '         ' // char(9) // 'ORIG'  // char(9)
      str = trim(str)  // 'PASS1' // char(9)
      str = trim(str)  // 'PASS2'

      itmp = cio_fputline(str, 120, obj%templun)

!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      end subroutine abal_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine abal (obj,ntr,hd,tr)
      implicit none
      type(abal_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

      character(PC_LENGTH)          :: str1,str2
      integer                       :: i,j
      integer                       :: itop,ibot
      integer                       :: nval,nval2
      integer                       :: nonzero
      integer                       :: nord

      real                          :: DBFACT, XPFACT
      real                          :: guess,amplitude,fterm
      real                          :: xbal(1)
      real                          :: coeff(50)
      real                          :: x_orig(ntr), y_orig(ntr)
      real                          :: y_pred1(ntr), y_pred2(ntr)
      real                          :: xval(ntr),  yval(ntr)   ! pass 1
      real                          :: xval2(ntr), yval2(ntr)  ! pass 2
      real                          :: shift(1)
      real                          :: buf(obj%ndpt)  



!-------------------------------------------------------------------------------
! Bypass entirely if no more traces.
!-------------------------------------------------------------------------------
      if (ntr == NO_MORE_TRACES) go to 1000

!-------------------------------------------------------------------------------
! Initializations.
!-------------------------------------------------------------------------------
      nval = 0
      nval2 = 0
      DBFACT = 20./log(10.) 
      XPFACT = 1./DBFACT

      xval  = FNIL
      yval  = FNIL

!---- conditionally increment gather counter
      if(obj%gathered) obj%ngrp = obj%ngrp + 1

!---- set up some temp values
      if(obj%opt_scale == 'SHOT') then
        xbal(1) = obj%off_scale
      else if(obj%opt_scale == 'CMP') then
        xbal(1) = obj%off_scale * obj%off_scale
      end if

!-------------------------------------------------------------------------------
! First main loop.  For mode == 'SINGLE', this is the only loop.  For the other
! modes (SHOT and CMP), this loop gathers the first round of statistics.
!-------------------------------------------------------------------------------
      do i=1,ntr

!------ The logic in this module is predicated on there being no FNIL values
!------ in the HDR_LAV slot on input (it's used as a flag).  Although this seems
!------ unlikely to happen, let's check it anyway and abort if it occurs.
        if(hd(HDR_LAV,i) == FNIL) then
          call pc_error('Invalid input FNIL value for HDR_LAV.')
          ntr = FATAL_ERROR
          go to 999
        end if

!------ initializations
        nonzero = 0

        y_orig(i)  = FNIL
        x_orig(i)  = hd(HDR_OFFSET,i)

        obj%minoff = min(obj%minoff, abs(x_orig(i)))
        obj%maxoff = max(obj%maxoff, abs(x_orig(i)))

        if (obj%mode == 'CMP') x_orig(i) = x_orig(i)*x_orig(i)

!-------------------------------------------------------------------------------
! Figure out where the top and base of the window will be for this trace.
!-------------------------------------------------------------------------------
        call latwin_get_window ( &
          obj%latwin, hd(:,i), tr(:,i), index1=itop, index2=ibot)

        if(itop < 1 .or. ibot < 1 .or. ibot <= itop) then
          hd(HDR_LAV,i) = FNIL
          y_orig(i) = 0
          cycle
        end if

!-------------------------------------------------------------------------------
! Obtain a value for trace 'amplitude' using average or median statistic.
!-------------------------------------------------------------------------------
        if (obj%stat == 'AVE') then 

          nonzero = count(tr(itop:ibot,i) /= 0.) 
          if (nonzero > 0) then
            amplitude = 0          ! mth_amplitude accumulates to this value!!!
            call mth_amplitude(tr(itop:ibot,i),ibot-itop+1,amplitude)
            amplitude = amplitude/nonzero
          end if

        else

          nonzero = 0
          guess = 0. 
          do j = itop, ibot 
            if (tr(j,i) == 0.) cycle  
            nonzero = nonzero + 1 
            buf(nonzero) = abs(tr(j,i)) 
            guess = guess + abs(tr(j,i)) 
          end do 

          if (nonzero > 0) then 
            call median (buf, nonzero, amplitude, 0.845*guess/nonzero) 
          endif 

        endif 

!        write(*,*) 'hd(2),itop,ibot,nonzero=', &
!         hd(2,i),',',itop,',',ibot,',',nonzero

!-------------------------------------------------------------------------------
! If nonzero is zero, flag the trace.
!
! If SINGLE mode, simply scale traces based on the amplitude measure (and don't
! scale them again in the final loop).
!
! For gathered data, store information in xval(i) and yval(i).
!-------------------------------------------------------------------------------

        if(nonzero < 1) then

          hd(HDR_LAV,i) = FNIL
          y_orig(i) = 0

        else if (obj%mode == 'SINGLE') then 

          tr(:,i) = tr(:,i) / amplitude                ! simple trace balance

        else

          nval = nval + 1 
          xval(nval) = x_orig(i) 
          if (obj%mode == 'CMP') xval(nval) = xval(nval)*xval(nval)

          yval(nval) = DBFACT * log(amplitude)
          y_orig(i)  = DBFACT * log(amplitude)

        endif 

      end do 

!      pause

!-------------------------------------------------------------------------------
! End of first main loop.
!
! Skip a bunch of code under 2 circumstances:
!   -- mode is SINGLE mode, in which case the calculation is already finished
!   -- nval is zero, in which case no sensible calculation can be done below
!-------------------------------------------------------------------------------
      if(obj%mode == 'SINGLE') go to 160

      if(nval < 1) then
        obj%pass1fail1 = obj%pass1fail1 + 1
        go to 160
      end if

!-------------------------------------------------------------------------------
! Block of logic for modes 'CDP' and 'SHOT' (SINGLE mode is finished already).
! xval and yval arrays were prepared above (each have 'nval' valid values)
!-------------------------------------------------------------------------------
!---- initializations
      xval2 = FNIL
      yval2 = FNIL

!-------------------------------------------------------------------------------
! 1st pass polynomial fit.
!-------------------------------------------------------------------------------
!---- nord is set to obj%order unless too few samples for poly fit
      nord = min(obj%order,nval-1)
      call polyfit_calc (nval, xval, yval, nord, coeff) 

!---- test for failure of polynomial fit
!---- if no valid polynomial, kill all traces in gather
      if (coeff(1) == FNIL) then 
        hd(HDR_LAV,:ntr) = FNIL
        obj%pass1fail2 = obj%pass1fail2 + 1
        go to 160
      endif 

!---- conditionally add to coef1 totals for later averaging
      if(real(nval)/obj%numtr > .95) then
        obj%ncoef1 = obj%ncoef1 + 1
        obj%coef1 = obj%coef1 + coeff
      end if

!---- calculate predicted amp at each original x
      call abal_predict_y_from_x (obj%nwih, ntr, nord, coeff, x_orig, y_pred1) 

!---- flag for exclusion values that exceed DB_KILL
      do i = 1, ntr

        if(abs(y_orig(i) - y_pred1(i)) > obj%db_kill) then
          hd(HDR_LAV,i) = FNIL
          obj%nkill = obj%nkill + 1 
        end if

      end do 

!---- conditionally modify data (based on user request)
      if (obj%opt_scale /= 'NONE' .or. obj%opt_print) then 

        nval2 = 0 
        do i = 1, ntr
          if (hd(HDR_LAV,i) == FNIL) cycle  
          nval2 = nval2 + 1 
          xval2(nval2) = x_orig(i) 
          if (obj%mode == 'CMP') xval2(nval2) = xval2(nval2)*xval2(nval2)
          yval2(nval2) = y_orig(i) 
        end do 

!------ If the counter for pass 2 is same as counter for pass 1, there's no
!------ reason to do the curvefit again.
        if (nval2 == nval) then
          y_pred2 = y_pred1
          go to 150
        end if

!-------------------------------------------------------------------------------
! 2nd pass polynomial fit.
!-------------------------------------------------------------------------------
!------ nord is set equal to obj%order unless too few samples for poly fit
        if (nval2 < 1) then 
          hd(HDR_LAV,:ntr) = FNIL
          obj%pass2fail1 = obj%pass2fail1 + 1
          go to 160
        else
          nord = min(obj%order,nval2 - 1) 
          call polyfit_calc (nval2, xval2, yval2, nord, coeff) 
        end if

!------ test for failure of polynomial fit
!------ if no valid polynomial, kill all traces
        if (coeff(1) == FNIL) then 
          hd(HDR_LAV,:ntr) = FNIL
          obj%pass2fail2 = obj%pass2fail2 + 1
          go to 160
        end if

!---- calculate predicted amp at each original x
      call abal_predict_y_from_x (obj%nwih, ntr, nord, coeff, x_orig, y_pred2) 

      endif 


!-------------------------------------------------------------------------------
! Final logic.
!-------------------------------------------------------------------------------
!---- extra logic if GATHER scaling is requested
150   if (obj%opt_scale == 'GATHER') then 
        call abal_predict_y_from_x(obj%nwih, 1, nord, coeff, xbal, shift)
      else 
        shift = 0. 
      endif 

160   obj%ntrace = obj%ntrace + ntr

      do i = 1, ntr

!------ kill flagged traces and do final scaling
!------ remember that SINGLE-mode data has already had final scaling !!!

        if (hd(HDR_LAV,i) == FNIL) then 
          tr(:,i) = 0.
        else if (obj%mode /= 'SINGLE' .and. obj%opt_scale /= 'NONE') then

          fterm = exp(XPFACT*(y_pred2(i) - y_orig(i) - shift(1)))
          tr(:,i) = tr(:,i) * fterm

!          write(*,*) 'i,ypred2,y_orig,shift,fterm=', &
!            i,',',y_pred2(i),',',y_orig(i),',',shift(1),',',fterm

!          do j=1,obj%ndpt
!            write(*,*) tr(j,i)
!          end do

        end if

        if(obj%OPT_PRINT .and. obj%mode /= 'SINGLE') then

          call string_ii2cc(i         ,str1)
            str2 = 'ABAL: '   // trim(str1) // char(9)
          call string_ff2cc(y_orig (i),str1)
            str2 = trim(str2) // trim(str1) // char(9)
          call string_ff2cc(y_pred1(i),str1)
            str2 = trim(str2) // trim(str1) // char(9)
          call string_ff2cc(y_pred2(i),str1)
            str2 = trim(str2) // trim(str1)

          j = cio_fputline(str2, 120, obj%templun)

        end if

      end do 

!      pause

!-------------------------------------------------------------------------------
! Reset LAV, and return.
!-------------------------------------------------------------------------------
      call lav_set_hdr(hd, tr, obj%ndpt, ntr)

      return  

!---- conditional call to the WRAPUP routine
  999 ntr = FATAL_ERROR
 1000 call abal_wrapup (obj)
      return

      end subroutine abal

!</execute_only>


!-------------------------------------------------------------------------------
! Subroutine to verify the input parameters (called by screen trap).
!-------------------------------------------------------------------------------
      subroutine abal_verify_parameters(obj)
      implicit none
      type(abal_struct)      :: obj

!---- verify that the data is gathered if mode /= SINGLE
      if(obj%mode /= 'SINGLE') then
        if(.not. obj%gathered) then
          call pc_error ('ABAL: ', obj%mode, ' mode requires gathered data.')
        end if
      end if

      end subroutine abal_verify_parameters

!-------------------------------------------------------------------------------
! Subroutine to do the polynomial fitting.
!-------------------------------------------------------------------------------

      subroutine abal_predict_y_from_x(nwih, n, nord, coeff, xval, predict_y)
      implicit none
      integer , intent(in)          :: nwih
      integer , intent(in)          :: n
      integer , intent(in)          :: nord 
      real , intent(in)             :: coeff(:) 
      real , intent(in)             :: xval(n) 
      real , intent(inout)          :: predict_y(n) 

      integer :: k 

      predict_y = coeff(nord+1) 
      do k = nord, 1, -1 
        predict_y(:n) = predict_y(:n)*xval(:n) + coeff(k) 
      end do 

      end subroutine abal_predict_y_from_x


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine abal_wrapup (obj)
      implicit none
      type(abal_struct),intent(inout) :: obj       ! arguments

      character(len=120)   :: str
      integer              :: i, itmp, lun
      real                 :: x(1),y1(1)
      real                 :: DBFACT, XPFACT


      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.


      DBFACT = 20./log(10.) 
      XPFACT = 1./DBFACT


      call pc_print ( &
      '-----------------------------------------------------------------------')
      call pc_print ( &
      '-- ABAL wrapup --------------------------------------------------------')
      call pc_print ( &
      '-----------------------------------------------------------------------')

      call pc_print  ('MODE of operation                     : ',obj%mode)
      call pc_print  ('OPT_SCALE was                         : ',obj%opt_scale)
      call pc_print  ('Minimum abs(offset)                   : ',obj%minoff)
      call pc_print  ('Maximum abs(offset)                   : ',obj%maxoff)

      if(obj%mode == 'SINGLE') then
        call pc_print('Number of traces processed   : ',obj%ntrace)
      else
        call pc_print('Number of gathers processed            : ',obj%ngrp)
        call pc_print('Number of traces killed due to DB_KILL : ',obj%nkill)
        call pc_print( &
          'Number of times too few data points for pass 1 : ', obj%pass1fail1)
        call pc_print( &
          'Number of times too few data points for pass 2 : ', obj%pass2fail1)
        call pc_print( &
          'Number of times curve fitting failed for pass 1: ', obj%pass1fail2)
        call pc_print( &
          'Number of times curve fitting failed for pass 2: ', obj%pass1fail2)
      end if


      if(obj%mode /= 'SINGLE') then

        lun = pc_get_lun()

!------ convert sums to averages
        if(obj%ncoef1 == 0) then
          obj%coef1 = 0
          call pc_print('')
          call pc_print('!!!! WARNING: No valid pass 1 curvefit results. !!!!')
        else
          obj%coef1 = obj%coef1 / obj%ncoef1
          call pc_print('')
          call pc_print('Average Pass 1 Curvefit Coefficients:')
          do i=0, obj%order
            write(lun,4000) obj%coef1(i+1)
          end do
        end if

!------ table
        call pc_print('')
        call pc_print('-------------------')
        call pc_print('DIAGNOSTIC TABLE #1')
        call pc_print('-------------------')
        call pc_print('Curvefit predicted values***')
      call pc_print( &
        '(Due to the possible exclusion of incomplete gathers please note that')
      call pc_print('prediction is based on ', obj%ncoef1, ' gather(s) ).')

        call pc_print('    Offset or     Average Pass 1 ')
        call pc_print('    Offset**2    Ampl. Prediction')
        call pc_print('    ---------    ----------------')

        do i=1,NTEST

          x(1) = obj%minoff + (i-1) * (obj%maxoff - obj%minoff) / (NTEST-1)
          if(obj%mode == 'CMP') x(1) = x(1)*x(1)

          call polyfit_evaluate(obj%order,obj%coef1,1,x,y1)

          if(y1(1) /= 0) y1(1) = exp(XPFACT * y1(1))
          write(lun,5000) x(1), y1(1)

        end do

        call pc_print( &
          '***These are linear amplitudes, NOT dB values as with OPT_PRINT.')
        call pc_print('')

      end if

4000  format(E17.10)
5000  format(F13.2,E19.5)


!---- TABLE #2 information is in the temp file...
!---- close and reopen temp print file.  copy contents to pc_print.  remove.
      itmp = cio_fclose(obj%templun)

      obj%templun = cio_fopen(obj%tempname,'r')

      itmp = 1
      do while(itmp > 0)
        itmp = cio_fgetline(str,120,obj%templun)
        call pc_print(str)
      end do

      itmp = cio_fclose(obj%templun)
      itmp = cio_remove(obj%tempname)

!---- mark end of wrapup printout
      call pc_print ( &
      '-----------------------------------------------------------------------')
      call pc_print ( &
      '-- ABAL wrapup --------------------------------------------------------')
      call pc_print ( &
      '-----------------------------------------------------------------------')

      return
      end subroutine abal_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module abal_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
