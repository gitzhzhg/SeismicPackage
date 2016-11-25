!<CPS_v1 type="PROCESS"/>

!!------------------------------- ampdg.f90 ---------------------------------!!
!!------------------------------- ampdg.f90 ---------------------------------!!
!!------------------------------- ampdg.f90 ---------------------------------!!

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
! Name       : AMPDG  (Amplitude Diagnostic) [includes former FLAG and ASCALE]
! Category   : diagnostics
! Written    : 1990-10-16   by: Bill Harlan  (FLAG)
! Revised    : 2006-09-18   by: D. Glover
! Maturity   : production
! Purpose    : Various ways of analyzing amplitude behavior of seismic data.
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
! AMPDG is used to analyze amplitude behavior of seismic data.  Currently two
! modes are available, DECAY and STAT.
!
! If MODE = DECAY, AMPDG analyzes the decay of amplitude with time of the input
! traces and determines the time raised to a power (to apply with TPOW) and the
! scale factor (to apply with SCALE) to optimally balance the data to a median
! of 1.0 uniformly down the trace.  These values are printed in the .rpt file.
! (This is the former ASCALE function.)
!
! If MODE = STAT, within the time window for each input trace, AMPDG calculates
! the specified statistic and writes the value of the calculated statistic in
! the specified header word.  This information can be further analyzed with
! DIST or TSLC (to make produce color plots) or a graphing package.  (This is
! the former FLAG function.)
!
! In MODE = STAT the time window can never extend above the head mute time or
! below the tail mute time.
!
!
! Statistics Available in MODE = STAT
!
! (All statistics are based on the trace sample values within the specified
! time window.)
!
! IF STAT = MEDIAN, the median of the absolute values is calculated or
! 50th percentile of the absolute value.
!
! IF STAT = AVERAGE, the mean (average) of the absolute values is calculated.
! A.K.A. L1 Norm or median deviation or absolute deviation.
!                  n
!   AVERAGE = 1/n SUM  ABS( a )
!                 i=1        i
!
! IF STAT = RMS, the square root of the mean square is calculated.
! A.K.A. L2 Norm or root-mean-square or if the median is zero its also the
! standard deviation.
!                n        2  1/2
!   RMS = { 1/n SUM  ( a )  }
!               i=1     i
!
! IF STAT = LAV, the largest absolute value is calculated.
! A.K.A. 100 percentile of the absolute values.
!
!   LAV = max(|a |)
!               i
!
! IF STAT = MEAN_FREQ, the mean frequency is calculated.  The trace within the
! window is autocorrelated and the mean frequency is found as 0.25 divided by
! the time from the autocorrelation peak to the first zero-crossing.
!
! IF STAT = PCTL, the specified percentile of the absolute amplitudes is
! calculated.  (PCTL = 75 means that 75% of the samples have an absolute
! amplitude less than the calculated amplitude.)  Do not use this option for a
! median or LAV calculation (50th and 100th percentile respectively)
! since it will run slower than the STAT = MEDIAN and LAV option.
!
! IF STAT = KURT, the kurtosis is calculated.  Kurtosis (L4/((3^.25)*L2))
! is a measure of the spikiness of the distribution.  Gaussian distributions
! have a kurtosis of 3.0, most seismic data is leptokurtotic and
! has a kurtosis > 3.0.
!               n        2  1/2
!   L2 = { 1/n SUM  ( a )  }
!              i=1     i
!
!               n        4  1/4
!   L4 = { 1/n SUM  ( a )  }
!              i=1     i
!
!
! Histogram
!
! In MODE = STAT a histogram is printed in the .rpt file of the calculated
! statistic values.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! In DECAY mode, input traces should be raw data without any gain applied.
! Normally the time window should be as large as possible.  Sample values above
! the head mute will be ignored in this mode.
!
! In STAT mode, the usual task is to identify and characterize anomalous traces
! so they can be killed in SELECT or clipped in CLIP.  Usually a deterministic
! gain such as TPOW or GDIV applied to the input data works best, although some
! options may still be useful if XP or MVXP is used.  The time window should be
! chosen as appropriate for the situation being investigated.  In some cases
! windows entirely above the first break may be useful for analyzing noise. 
!
!
! Color Diagnostic Plots
!
! Color amplitude diagnostic plots can be produced for a 2D line or a sail line
! from a 3D survey with a job set up as follows.
!
!   DATA INPUT
!   AMPDG (mode = stat, stat = median, hdr_stat = 48)
!   TSLC  (hdr_x = 10, hdr_y = 7, headers = 48)
!   COLR  (set up as in TSLC .rpt file)
!
! Statistics other than the median may also be useful.
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a single-trace process.
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process does not alter input traces.
! This process outputs the same traces as it receives.
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       used but not changed
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!
!         HDR_STAT                   Header word(s) for calculated statistic
! 2       Head mute index            Used but not changed
! 64      Tail mute index            Used but not changed
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                        REVISION HISTORY FOR FLAG
!
!     Date       Author      Description
!     ----       ------      -----------
!015. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!014. 2006-01-10  B. Menger  Removed Unused Variables.
! 13. 2005-01-17 Menger      Fixed MUTE mode for OPT_WIN, added mode=DECAY
! 12.  2002-07-01 CI Burch   Documentation change only.
! 11.  2002-06-18 Goodger    Change trace windowing scheme to be the same
!                            as dist.  Add IGNORE_ZEROS parameter.  Change
!                            mode default to STAT.
! 10.  2001-04-30 Selzler    Corrected idx_beg and idx_end initialization.
!  9.  2000-12-11 Selzler    Changed wrapup logic to use skip_wrapup
!  8.  2000-11-14 Selzler    Added histogram descriptive to printout.
!  7.  2000-08-15 Selzler    Changed TSLC reference to coordinate based keyword
!  6.  2000-08-02 Selzler    Conversion to f90.
!  5.  1998-12-18 Vunderink  Begin using the f90 compiler.
!  4.  1994-02-10 Harlan     Histogram accepts small numbers in FLAGHSTN
!  3.  1991-08-28 Harlan     Avoid possible overflow in subroutine FLAGPCTL
!  2.  1991-04-17 Ball       Moved from NEWLIB to CONLIB  no prog. change
!  1.  1990-10-16 Harlan     Original Version
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
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!
!<gui_def>
!<NS ampdg Process/NC=80>
! Amplitude Diagnostic, statistical analysis of trace samples
!
! MODE=`CCCC        STAT=`CCCCCCCC           IGNORE_ZEROS=`CCC
!
!  `- Windowing Scheme -------------------------------------------------------
!    OPT_WIN=`CCCCC
!
!       Fixed Window~~~   TIM_BEG=`FFFFFFFFFFF     TIM_END=`FFFFFFFFFFF
!       Mute Window~~~~   TIM_ADD=`FFFFFFFFFFF     WIN_LEN=`FFFFFFFFFFF
!  `--------------------------------------------------------------------------
!
! HDR_STAT=`III     PCTL=`FFFFF
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Whether to use the DECAY or STAT mode. </Tip>
! Default = STAT
! Allowed = DECAY
! Allowed = STAT
! If MODE = DECAY, AMPDG analyzes the decay of amplitude with time of the input
! traces and determines the time raised to a power (to apply with TPOW) and the
! scale factor (to apply with SCALE) to optimally balance the data to a median
! of 1.0 uniformly down the trace.  (This is the former ASCALE function.)
!
! If MODE = STAT, within the time window for each input trace, AMPDG calculates
! the specified statistic and writes the value of the calculated statistic in
! the specified header word.  This information can be further analyzed with
! DIST or TSLC (to make produce color plots) or a graphing package.  (This is
! the former FLAG function.)
!</Help>
!
!<Help KEYWORD="STAT">
!<Tip> Statistic to calculate in STAT mode. </Tip>
! Default = MEDIAN
! Allowed = MEDIAN
! Allowed = AVERAGE
! Allowed = RMS
! Allowed = LAV
! Allowed = MEAN_FREQ
! Allowed = PCTL
! Allowed = KURT
! (All statistics are based on the trace sample values within the specified
! time window.)
!
! IF STAT = MEDIAN, the median of the absolute values is calculated.
!
! IF STAT = AVERAGE, the mean (average) of the absolute values is calculated.
! (This is the L1 norm.)
!
! IF STAT = RMS, the square root of the mean square is calculated.
! (This is the L2 norm.)
!
! IF STAT = LAV, the largest absolute value is calculated.
!
! IF STAT = MEAN_FREQ, the mean frequency is calculated.  The trace within the
! window is autocorrelated and the mean frequency is found as 0.25 divided by
! the time from the autocorrelation peak to the first zero-crossing.
!
! IF STAT = PCTL, the specified percentile of the absolute amplitudes is
! calculated.  (PCTL = 75 means that 75% of the samples have an absolute
! amplitude less than the calculated amplitude.)  Do not use this option for a
! median calculation (50th percentile) since it will run slower than the
! STAT = MEDIAN option.
!
! IF STAT = KURT, the kurtosis is calculated.  Kurtosis (L4/((3^.25)*L2)
! is a measure of the spikiness of the distribution.  Gaussian distributions
! have a kurtosis of 3.0, most seismic data is leptokurtotic and
! has a kurtosis > 3.0.
!</Help>
!
!<Help KEYWORD="IGNORE_ZEROS">
!<Tip> Whether to ignore zero input values. </Tip>
! Default = NO
! Allowed = YES/NO
!</Help>
!
!<Help KEYWORD="OPT_WIN">
!<Tip> Method to use for specifying trace time window. </Tip>
! Default = FIXED
! Allowed = MUTE     (Window defined by times measured from mute.)
! Allowed = FIXED    (Window is fixed.)
! AMPDG uses only the trace samples within the trace time window.
!
! If OPT_WIN = FIXED, then the window is specified by TIM_BEG and TIM_END.
!
! If OPT_WIN = MUTE, then the window is specified by TIM_ADD and WIN_LEN.
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Time for top of trace window if OPT_WIN = FIXED. </Tip>
! Default = TSTRT
! Allowed = real >= TSTRT
! The window top will be set at the larger of TIM_BEG, in seconds,
! or the mute time.
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> Time for bottom of trace window if OPT_WIN = FIXED. </Tip>
! Default =  end of trace
! Allowed = real >= TIM_BEG
! The window bottom will be set at the smaller of TIM_END, in seconds,
! or the tail mute time.
!</Help>
!
!<Help KEYWORD="TIM_ADD">
!<Tip> Time at top of window = mute time + TIM_ADD. </Tip>
! Default = 0.0
! Allowed = real > 0.0
! TIM_ADD in seconds is added to mute time to find the first sample.
!
! Active only if OPT_WIN = MUTE.
!</Help>
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Time at bottom of window = time at top of window + WIN_LEN. </Tip>
! Default = trace length 
! Allowed = real between zero and trace length
! WIN_LEN is the length of the trace window in seconds.
! Bottom of window cannot be below tail mute time.
!
! Active only if OPT_WIN = MUTE.
!</Help>
!
!<Help KEYWORD="HDR_STAT">
!<Tip> Header word(s) to use for calculated statistic in STAT mode. </Tip>
! Default = 48
! Allowed = 1 - NWIH
! In mode = DECAY, THREE header words are used.  HDR_STAT, HDR_STAT+1, and
!           HDR_STAT+2.  AMP goes in HDR_STAT, POW next, then ERR where
!           ERR is the standard error from doing a linear regression fit of
!           AMP and POW to the equation y = amp*x**pow
!</Help>
!
!<Help KEYWORD="PCTL">
!<Tip> Percentile to use for calculated statistic if STAT = PCTL. </Tip>
! Default = 95
! Allowed = real 0.0 - 100.0
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
! NOTES FOR CONVERSION PROGRAMMER
!
!
!1.  Use care with Harlan code.  It may be best to simply extract the needed
!algorithms and check them over throughly.
!
!2.  Normalization as in old FLAG is not being implemented.
!
!3.  Note operation of mode = decay regarding ignoring samples above the head
!mute index.
!
!------------------------------------------------------------------------
! Translation of important notes from old FLAG to new AMPDG process.
! old    old     new
! IMODE  IMODE2  STAT
!   1      1     MEAN_FREQ  Mean frequency
!   3      2     AVERAGE    Average of absolute values
!   5      3     RMS        Root Mean Square of values
!   7      4     LAV        Largest Absolute value
!   9      5     PCTL       Percentile
!  11      6      na        Deprecated
!  13      7     KURT       Kurtosis
!  na      na    MEDIAN     Meadian value
!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module ampdg_module
      use pc_module
      use median_module
      use lav_module
      use histogram_module
      use named_constants_module
      use mem_module
      implicit none
      private
      public :: ampdg_create
      public :: ampdg_initialize
      public :: ampdg_update
      public :: ampdg_delete
!<execute_only>
      public :: ampdg
      public :: ampdg_wrapup
!</execute_only>

      character(len=100),public,save :: ampdg_IDENT = &
       '$Id: ampdg.f90,v 1.15 2006/09/18 13:32:35 Glover prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      real, parameter :: EPSILON = 1.E-28

      ! Note: FOURTH_ROOT_OF_3 = sqrt(sqrt(3.0)) = 1.316074
      real, parameter :: FOURTH_ROOT_OF_3 = 1.316074


      type,public :: ampdg_struct
        private
        logical                    :: skip_wrapup      ! wrapup flag.

        character(len=5)           :: mode             ! process parameters.
        character(len=9)           :: stat             ! process parameters.
        character(len=4)           :: ignore_zeros
        character(len=8)           :: opt_win
        real                       :: tim_beg          ! process parameters.
        real                       :: tim_end          ! process parameters.
        real                       :: tim_add
        real                       :: win_len
        integer                    :: hdr_stat         ! process parameters.
        real                       :: pctl             ! process parameters.

        integer                    :: nwih             ! global parameter
        integer                    :: ndpt             ! global parameter
        real                       :: dt               ! global parameter
        real                       :: tstrt            ! global parameter

        integer                    :: idx_beg          ! process parameters.
        integer                    :: idx_end          ! process parameters.
        integer                    :: idx_count        ! process parameters.
        character(len=64)          :: title            ! process parameters.
        integer                    :: add_pts,firstpt,lastpt

        type(histogram_struct), pointer :: histogram   ! process parameters.
        type(histogram_struct), pointer :: histogram_pctl ! process parameters.
        type(histogram_struct), pointer :: histogram_decay ! for decay mode

        double precision           :: wgtavgpow        ! weighted average 
                                                       ! of power for decay mode
        double precision           :: wgtavgamp        ! ditto for amp
        double precision           :: sumerr           ! sum of 1/err
        integer                    :: lun              ! unit number
        integer                    :: tottrc           ! total number trcs done
      end type ampdg_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(ampdg_struct),pointer,save :: object      ! needed for traps.

      character(len=5),dimension(2),parameter :: MODE_OPTIONS = &
        (/'STAT ','DECAY'/)

      character(len=9),dimension(7),parameter :: STAT_OPTIONS = &
        (/'MEDIAN   ','AVERAGE  ','RMS      ','LAV      ','MEAN_FREQ', &
        'PCTL     ','KURT     '/)


      integer    ,parameter :: yesno_noptions = 2
      character(len=4)      :: yesno_options(yesno_noptions)            &
                               = (/ 'YES ', 'NO  ' /)

      integer    ,parameter :: noyes_noptions = 2
      character(len=4)      :: noyes_options(noyes_noptions)            &
                               = (/ 'NO  ', 'YES ' /)

      integer    ,parameter :: opt_win_noptions = 2
      character(len=8)      :: opt_win_options(opt_win_noptions)            &
                               = (/ 'FIXED   ', 'MUTE    ' /)

      

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine ampdg_create (obj)
      implicit none
      type(ampdg_struct),pointer :: obj       ! arguments

      allocate (obj)
      nullify (obj%histogram) ! jpa
      nullify (obj%histogram_pctl) ! jpa
      nullify (obj%histogram_decay) ! jpa

      call ampdg_initialize (obj)
      return
      end subroutine ampdg_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine ampdg_delete (obj)
      implicit none
      type(ampdg_struct),pointer :: obj       ! arguments

!<execute_only>
      call ampdg_wrapup (obj)

      if(associated(obj%histogram)) then
        call histogram_delete(obj%histogram)
      end if

      if(associated(obj%histogram_pctl)) then
        call histogram_delete(obj%histogram_pctl)
      end if

      if(associated(obj%histogram_decay)) then
        call histogram_delete(obj%histogram_decay)
      end if

!</execute_only>

      deallocate(obj)
      return
      end subroutine ampdg_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine ampdg_initialize (obj)
      implicit none
      type(ampdg_struct),intent(inout) :: obj       ! arguments

      call pc_get_global('nwih', obj%nwih)
      call pc_get_global('ndpt', obj%ndpt)
      call pc_get_global('dt', obj%dt)
      call pc_get_global('tstrt', obj%tstrt)

      obj%mode = 'STAT'
      obj%stat = 'MEDIAN'
      obj%ignore_zeros = 'NO'
      obj%opt_win='FIXED'  
      obj%tim_beg = obj%tstrt
      obj%tim_end = obj%tstrt + (obj%ndpt - 1) * obj%dt
      obj%tim_add = 0.0
      obj%win_len = obj%tstrt + (obj%ndpt - 1) * obj%dt
      obj%hdr_stat = 48
      obj%pctl = 95.0

      obj%idx_beg = 0
      obj%idx_end = 0
      obj%idx_count = 0
      obj%title = '<none>'
      obj%wgtavgpow = 0d0
      obj%wgtavgamp = 0d0
      obj%sumerr    = 0d0
      obj%tottrc    = 0
      obj%lun       = pc_get_lun()

      call ampdg_update (obj)

      return
      end subroutine ampdg_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine ampdg_update (obj)
      implicit none
      type(ampdg_struct),intent(inout),target :: obj             ! arguments

      real        :: end_time                                   ! local
      integer     :: state  
      integer     :: nscratch
      logical     :: verify

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

      state = pc_get_update_state()

      if(state == PC_FRONTEND .or. state == PC_BACKEND) then
        verify = .true.
      else
        verify = .false.
      end if

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get_global('nwih', obj%nwih)
      call pc_get_global('ndpt', obj%ndpt)
      call pc_get_global('dt', obj%dt)
      call pc_get_global('tstrt', obj%tstrt)

      end_time = obj%tstrt + (obj%ndpt - 1) * obj%dt

      call pc_get('mode', obj%mode)
      call pc_get('stat', obj%stat)
      call pc_get('ignore_zeros',obj%ignore_zeros)
      call pc_get('opt_win',obj%opt_win,ampdg_opt_win_trap)
      call pc_get('tim_beg', obj%tim_beg,ampdg_tim_beg_trap)
      call pc_get('tim_end', obj%tim_end,ampdg_tim_end_trap)
      call pc_get('tim_add',obj%tim_add,ampdg_tim_add_trap)
      call pc_get('win_len',obj%win_len,ampdg_win_len_trap)
      call pc_get('hdr_stat', obj%hdr_stat)
      call pc_get('pctl', obj%pctl)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if(obj%mode == 'DECAY') then
        if(obj%hdr_stat < 1 .or. obj%hdr_stat > obj%nwih-2) then
          call pc_error('invalid HDR_STAT, must be >= 1 and <= NWIH-2')
          obj%hdr_stat = min(obj%nwih,48)
        end if
      else if(obj%mode == 'STAT') then
        if(all(STAT_OPTIONS /= obj%stat)) then
          call pc_error('invalid STAT, must be ' // &
            'MEDIAN, AVERAGE, RMS, LAV, MEAN_FREQ, PCTL or KURT')
          obj%stat = 'MEDIAN'
        end if

        if(obj%stat == 'PCTL' .and. &
          obj%pctl < 0.0 .or. obj%pctl > 100.0) then
          call pc_error('invalid PCTL, must be >= 0.0 and <= 100.0')
          obj%pctl = 95.0
        end if

        if(obj%hdr_stat < 1 .or. obj%hdr_stat > obj%nwih) then
          call pc_error('invalid HDR_STAT, must be >= 1 and <= NWIH')
          obj%hdr_stat = min(obj%nwih,48)
        end if
      else
        call pc_error('invalid MODE, must be "DECAY" or "STAT"')
        obj%mode = 'STAT'
      end if

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field('mode', mode_options, 2)
      call pc_put_options_field('stat', stat_options, 7)
      call pc_put_options_field ('opt_win', opt_win_options, opt_win_noptions)
      call pc_put_options_field ('ignore_zeros', yesno_options, yesno_noptions)

      call pc_put('mode', obj%mode)
      call pc_put('stat', obj%stat)
      call pc_put('ignore_zeros', obj%ignore_zeros)
      call pc_put('opt_win',obj%opt_win)
      call pc_put('tim_beg', obj%tim_beg)
      call pc_put('tim_end', obj%tim_end)
      call pc_put('tim_add',obj%tim_add)
      call pc_put('win_len',obj%win_len)
      call pc_put('hdr_stat', obj%hdr_stat)
      call pc_put('pctl', obj%pctl)

      if(obj%mode == 'STAT') then
        call pc_put_sensitive_field_flag('stat', .true.)
        if(obj%stat == 'PCTL') then
          call pc_put_sensitive_field_flag('PCTL', .true.)
        else
          call pc_put_sensitive_field_flag('PCTL', .false.)
        end if
      else
        call pc_put_sensitive_field_flag('stat', .false.)
        call pc_put_sensitive_field_flag('PCTL', .false.)
      end if


      nscratch = 123

      call pc_put_control('nscratch', nscratch)

      call pc_call_end_trap(ampdg_end_trap)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      obj%firstpt = 1 + nint((max(obj%tim_beg, obj%tstrt)&
                                - obj%tstrt) / obj%dt)
      obj%lastpt = 1 + nint((min(obj%tim_end,  end_time)&
                                - obj%tstrt) / obj%dt)
      obj%add_pts = nint(obj%tim_add/obj%dt)

!!!      obj%idx_count = 1 + obj%idx_end - obj%idx_beg

      ! initialize statistics needed for histogram
      call histogram_create(obj%histogram)

      if(obj%mode == 'STAT') then
        if(obj%stat == 'PCTL') then
          ! initialize statistics needed for histogram percentile
          call histogram_create(obj%histogram_pctl)
        end if

        select case(obj%stat)
        case ('MEDIAN')
          obj%title = 'MEDIAN of absolute values (50th percentile of absolute)'
        case ('AVERAGE')
          obj%title = 'AVERAGE (mean) of absolute values (L1 Norm)'
        case ('RMS')
          obj%title = 'RMS (Root Mean Square) of samples (L2 Norm)'
        case ('LAV')
          obj%title = 'LAV (Largest Absolute Value) of samples'
        case ('MEAN_FREQ')
          obj%title = 'MEAN_FREQ (Mean Frequency) of samples'
        case ('PCTL')
          obj%title = 'PCTL PerCenTiLe of absolute values'
        case ('KURT')
          obj%title = 'KURT Kurtosis of samples (L4/((3^.25)*L2))'
        end select
      else
        !--- store power in this one.  (amp in "histogram")
        call histogram_create(obj%histogram_decay)
      end if

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine ampdg_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

subroutine ampdg_opt_win_trap(opt_win)
  character(len=*),intent(in) :: opt_win
  call string_to_upper(object%opt_win)
  select case (object%opt_win)
    case ('MUTE')
         call pc_put_sensitive_field_flag('TIM_ADD', .true.)
         call pc_put_sensitive_field_flag('WIN_LEN', .true.)
         call pc_put_sensitive_field_flag('TIM_BEG', .false.)
         call pc_put_sensitive_field_flag('TIM_END', .false.)
         call pc_jump_field('TIM_ADD')
    case ('FIXED')
         call pc_put_sensitive_field_flag('TIM_ADD', .false.)
         call pc_put_sensitive_field_flag('WIN_LEN', .false.)
         call pc_put_sensitive_field_flag('TIM_BEG', .true.)
         call pc_put_sensitive_field_flag('TIM_END', .true.)
         call pc_jump_field('TIM_BEG')
    case default
      call pc_error('AMPDG: opt_win must be FIXED or MUTE')
      call pc_jump_field(opt_win)
  end select
end subroutine ampdg_opt_win_trap


subroutine ampdg_tim_add_trap(tim_add)
  character(len=*),intent(in) :: tim_add
  if(object%opt_win == 'MUTE') then
    if(object%Tim_Add < 0.0) then
      call pc_error('TIM_ADD cannot be less than zero')
    else if (object%Tim_Add > object%dt*(object%ndpt-1) - object%tstrt) THEN
      call pc_error('TIM_ADD must be less than trace length')
    end if
  endif
end subroutine ampdg_tim_add_trap

subroutine ampdg_win_len_trap(win_len)
  character(len=*),intent(in) :: win_len
  if(object%opt_win == 'MUTE') then
    if(object%win_len <= 0.0) then
      call pc_error('win_len must be greater than zero')
    else if (object%win_len > object%dt*(object%ndpt-1) - object%tstrt) then
      call pc_error('win_len cannot be greater than trace length')
    end if
  endif
end subroutine ampdg_win_len_trap

subroutine ampdg_tim_beg_trap(tim_beg)
  character(len=*),intent(in) :: tim_beg
  real                        :: end_time
  end_time = object%tstrt + (object%ndpt - 1) * object%dt
  if(object%opt_win == 'FIXED') then
    if(object%tim_beg <object%tstrt .or. object%tim_beg >= end_time ) then
      call pc_error('invalid tim_beg: must be >= start time & < end time.')
      object%tim_beg = object%tstrt
    endif
  endif
end subroutine ampdg_tim_beg_trap

subroutine ampdg_tim_end_trap(tim_end)
  character(len=*),intent(in) :: tim_end
  real                        :: end_time
  end_time = object%tstrt + (object%ndpt - 1) * object%dt
  if(object%opt_win == 'FIXED') then
    if(object%tim_end <= object%tim_beg .or. object%tim_end > end_time ) then
      call pc_error('invalid tim_end: must be > tim_beg & <= end time.')
      object%tim_end = end_time
    endif
  endif
end subroutine ampdg_tim_end_trap

subroutine ampdg_end_trap

  return
  if(object%opt_win.eq.'FIXED')then
    object%tim_add=0.0
    object%win_len=0.0
  else
    object%tim_beg=0.0
    object%tim_end=0.0
  endif

end subroutine ampdg_end_trap

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine ampdg (obj,ntr,hd,tr)
      implicit none
      type(ampdg_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

      real :: value, L2_value, L4_value
      real :: work(obj%ndpt)
      double precision                :: amp,pow,err
      integer :: i,j  ,ntr_do 

      ntr_loop: &
      do ntr_do = 1, ntr
        if(hd(25,ntr_do) == 0.0) then
          ! dead trace
          hd(obj%hdr_stat,ntr_do) = 0d0
          if(obj%mode=='DECAY') then
            hd(obj%hdr_stat+1,ntr_do)=0d0
            hd(obj%hdr_stat+2,ntr_do)=1d5
          endif
          cycle ntr_loop
        end if

        if(obj%opt_win.eq.'FIXED')then
          obj%idx_beg = MAX(obj%firstpt, nint(hd( 2,ntr_do)))
          obj%idx_end = MIN(obj%lastpt,  nint(hd(64,ntr_do)))
        else                ! obj%opt_win == 'MUTE'
          obj%idx_beg =     nint(hd( 2,ntr_do)) + obj%add_pts
          obj%idx_end = MIN(nint(hd(64,ntr_do)), &
                            obj%idx_beg + nint(obj%win_len/obj%dt) - 1)
        endif
        obj%idx_count=obj%idx_end-obj%idx_beg + 1


        if(obj%ignore_zeros.eq.'YES')then
          j=1
          do i=obj%idx_beg,obj%idx_end
            if(tr(i,ntr_do) == 0.0)then
              obj%idx_count=obj%idx_count-1
              cycle
            endif
            work(j)=tr(i,ntr_do)
            j=j+1
          enddo
        else
          work(1:obj%idx_count)=tr(obj%idx_beg:obj%idx_end,ntr_do)
        endif

        mode_choice: &
        if(obj%mode == 'DECAY') then
          !??? implementation pending
          call ampdg_linregresspwr(work(1:obj%idx_count),obj%idx_count,&
              (obj%idx_beg-1)*obj%dt,obj%dt,amp,pow,err)
          !--- weight the power and amp and sum the errors
          obj%wgtavgpow = obj%wgtavgpow + pow/err
          obj%wgtavgamp = obj%wgtavgamp + amp/err
          obj%sumerr    = obj%sumerr    + 1d0/err
          obj%tottrc    = obj%tottrc    + 1

          hd(obj%hdr_stat,ntr_do)   = amp
          hd(obj%hdr_stat+1,ntr_do) = pow
          hd(obj%hdr_stat+2,ntr_do) = err
          !write(obj%lun,*)' ',&
          !'AMPDG: Tr# ',nint(hd(1,ntr_do)),&
          !' Amp = ',AMP, ' Pow = ',POW, ' Serr= ',ERR

          ! accumulate histogram of computed statistic
          call histogram_compute(obj%histogram, real(amp))
          call histogram_compute(obj%histogram_decay, real(pow))
        else if(obj%mode == 'STAT') then ! mode_choice
          stat_choice: &
          select case(obj%stat)
          case ('MEDIAN')
            call median(abs(work(1:obj%idx_count)),obj%idx_count, value)
              
          case ('AVERAGE')  ! L1 Norm
            value = sum(abs(work(1:obj%idx_count))) / obj%idx_count
              

          case ('RMS')
            value = sqrt(sum(work(1:obj%idx_count)**2) / obj%idx_count)

          case ('LAV')
            ! Note:  value == max(abs(tr(obj%idx_beg:obj%idx_end,ntr_do)))
            value = lav(work(1:obj%idx_count), obj%idx_count)

          case ('MEAN_FREQ')
            value = ampdg_mean_freq(work(1:obj%idx_count),obj%idx_count, obj%dt)

          case ('PCTL')
            call histogram_clear(obj%histogram_pctl)

            call histogram_compute(obj%histogram_pctl, work(1:obj%idx_count))

            value = histogram_quantile(obj%histogram_pctl, 0.01 * obj%pctl)

          case ('KURT')
            L2_value = sqrt(sum(work(1:obj%idx_count)**2) / obj%idx_count)
              

            if(L2_value > 0.0) then
              L4_value = sqrt(sqrt(sum(work(1:obj%idx_count)**4)/obj%idx_count))
                
              ! Note: FOURTH_ROOT_OF_3 = sqrt(sqrt(3.0)) = 1.316074
              value = L4_value / (FOURTH_ROOT_OF_3 * L2_value)
            else
              ! Note: undefined, 0.0 / 0.0
              value = 0.0
            end if

          end select stat_choice

          ! accumulate histogram of computed statistic
          call histogram_compute(obj%histogram, value)

          ! preserve statistic in trace header
          hd(obj%hdr_stat,ntr_do) = value
        end if mode_choice
      end do ntr_loop

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
        call ampdg_wrapup (obj)
      end if

      return
      end subroutine ampdg

!</execute_only>

!!--------------------------- ampdg_mean_freq -------------------------------!!
!!--------------------------- ampdg_mean_freq -------------------------------!!
!!--------------------------- ampdg_mean_freq -------------------------------!!


!<execute_only>

      function ampdg_mean_freq(buf,nbuf,dt)
      implicit none
      real :: ampdg_mean_freq                      ! return type
      integer :: nbuf                              ! argument
      real,dimension(nbuf),intent(in) :: buf       ! argument
      real :: dt                                   ! argument

      integer, parameter :: TMP_W_SIZE = 20
      real, dimension(TMP_W_SIZE) :: tmp_w
      integer :: i3, iw, k1b
      real :: rlag, r1

      tmp_w = 0.0

      ! Autocorrelate each trace with itself.
      ! Calculates the distance DT from from the peak of the
      ! autocorrelation function to the first zero-crossing.
      ! The output mean frequency is then calculated as 0.25/DT.

      do i3 = 1, nbuf
        k1b = min(TMP_W_SIZE, i3)
        tmp_w(:k1b) = tmp_w(:k1b) + buf(i3:i3 + 1 - k1b: -1) * buf(i3)
      end do

      rlag = TMP_W_SIZE + 1  ! default, if not changed by do-loop

      do iw = 2, TMP_W_SIZE
        if(tmp_w(iw) <= 0.0) then
          r1 = tmp_w(iw) - tmp_w(iw - 1)

          if(abs(r1) > EPSILON) then
            ! Linearly interpolate zero crossing between (IW - 1) and (IW).
            ! rlag = (iw - 1) + some fraction of one do-loop increment.
            rlag = (iw - 1) - tmp_w(iw - 1) / r1
          else
            ! Curve is nearly flat at the zero crossing.
            rlag = iw
          end if

          exit
        end if
      end do

      ! Note: (rlag - 1.0) accounts for do-loop starting from 1 instead of 0.
      ampdg_mean_freq = 1.0 / (4.0 * (rlag - 1.0) * dt)

      return
      end function ampdg_mean_freq

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine ampdg_wrapup (obj)
      implicit none
      type(ampdg_struct),intent(inout) :: obj       ! arguments


      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      if(obj%mode == 'STAT') then
        ! print results of histogram
        if(associated(obj%histogram)) then

          write(obj%lun, *) ' '
          write(obj%lun, *) &
          'AMPDG: The following is a histogram derived from trace samples.'
          write(obj%lun, *) &
          'AMPDG:   Each trace is reduced to one number using statistics,'
          write(obj%lun, *) &
          'AMPDG:   i.e. median sample value, average, RMS, LAV, etc.' 
          write(obj%lun, *) &
          'AMPDG:   These numbers are accumulated in a histogram and printed.'
          write(obj%lun, *) &
          'AMPDG:   The title describes the statistical measure that was used.'
          write(obj%lun, *) &
          'AMPDG:   The printout includes the observed min and max numbers,'
          write(obj%lun, *) &
          'AMPDG:   total point (trace) count, histogram center and edges,'
          write(obj%lun, *) &
          'AMPDG:   and population summary (properly binned and outliers).'

          call histogram_print(obj%histogram, obj%lun, &
            verbose=.false., &
            title=trim("AMPDG: " // obj%title))
        end if
      elseif(obj%mode == 'DECAY') then
          obj%wgtavgpow = obj%wgtavgpow/obj%sumerr
          obj%wgtavgamp = obj%wgtavgamp/obj%sumerr
          write(obj%lun, *) ' '
          write(obj%lun, *) &
          'AMPDG:  The following is the weighted average power and amplitude'
          write(obj%lun, *) &
          'AMPDG:  from using MODE=DECAY. to fit tr = amp * t ** pow '
          write(obj%lun, *) &
          'AMPDG:  TotalTrc=',obj%tottrc,' AMP=',&
          obj%wgtavgamp,' POW=',obj%wgtavgpow
          if(associated(obj%histogram) .and. associated(obj%histogram_decay))&
          then
            write(obj%lun, *) &
            'AMPDG:   First histogram is for AMPLITUDE '
            obj%title = 'Decay mode -- Amplitude histogram'
            call histogram_print(obj%histogram, obj%lun, &
              verbose=.false., &
              title=trim("AMPDG: "// obj%title))
            write(obj%lun, *) &
            'AMPDG:   Second histogram is for POWER '
            obj%title = 'Decay mode -- Exponent histogram'
            call histogram_print(obj%histogram_decay, obj%lun, &
              verbose=.false., &
              title=trim("AMPDG: " // obj%title))


          endif
      end if

      return
      end subroutine ampdg_wrapup


subroutine ampdg_linregresspwr(tr,n,tstrt,dt,amp,pow,err)
  !----------------------------------------------------------
  integer, intent(in)                      :: n
  real, intent(in), dimension(:)           :: tr(n)
  real, intent(in)                         :: dt,tstrt
  double precision, intent(out)            :: amp,pow,err
  !----------------------------------------------------------
  !-----------------------------------------------------------------------------
  !---------- PURPOSE ----------------------------------------------------------
  ! linregresspwr will find the least squares fit to two parameters for an input
  ! tr where |tr(t)| = amp* t**pow.  Uses standard linear regression techniques.
  ! wmm 6/24/04
  !-----------------------------------------------------------------------------
  
  integer                                  :: i,m
  double precision,dimension(:)            :: x(n),y(n)
  double precision                         :: sumx, sumy, T   


  ! X = AMP * Y**(POW)

  !--- linear regression of power curve.
  !--- preset the coefficients
  AMP=0.0
  POW=0.0
  ERR=1D5
  m=0 !--- M is the count of samples we will actually use
  !--- tr is supposed to be |tr| before calling here, but no chances taken.
  !--- step 1. take log of x and y (x = time axis, y = trace axis)
  do i = 1, n ! remove zero values since we will be taking logs
    T = tstrt + (i-1)*dt
    if(abs(tr(i)) == 0.0 .or. T <= 0.0 ) cycle
      m = m + 1
      x(m) = log(T)
      y(m) = log(abs(tr(i)))
  end do

  !--- zero the rest of the arrays
  x(m+1:n) = 0d0
  y(m+1:n) = 0d0
  !--- quit if not enough points
  if( m <= 2 ) return
  !--- test for dead interval not needed since removed zero points above
  !--- Do the linear regression
  sumx=sum(x)
  sumy=sum(y)
  POW=(m*dot_product(x,y)-sumx*sumy )/( m*dot_product(x,x)-sumx*sumx )
  AMP=(sumy - POW*sumx)/m
  !--- Calculate the standard error (not sdev)
  ERR=0.0
  do i = 1, m
    ERR=ERR + (y(i)- (AMP+POW*x(i)) )**2
  end do
  ERR = sqrt(ERR/(m-2))
  !--- since we did this in log space, must exp the amplitude
  AMP = exp(AMP)
  
end subroutine ampdg_linregresspwr

!</execute_only>

end module ampdg_module
