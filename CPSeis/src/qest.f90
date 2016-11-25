!<CPS_v1 type="PROCESS"/>
!!------------------------------- qest.f90 ---------------------------------!!
!!------------------------------- qest.f90 ---------------------------------!!
!!------------------------------- qest.f90 ---------------------------------!!

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
! Name       : QEST    (Estimates Q(t) from surface seismic data.)
! Category   : filters
! Written    : 1997-09-29   by: Greg Lazear
! Revised    : 2001-01-31   by: Tom Stoeckley
! Maturity   : production   2001-04-30
! Purpose    : Estimates Q(t) attenuation function from surface seismic data.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! QEST estimates a Q(t) attenuation function from surface seismic data using
! the spectral ratio method.  It writes the computed Q(t) function to the .rpt
! file and may optionally also output windowed amplitude spectra in a bytefile.
! QEST does not alter the input traces.
!
!
! Detailed QEST Operation
!
!  1. QEST divides each input trace into time windows of length WIN_LEN with
!     an increment of WIN_INC between adjacent windows. The windows start
!     from the top of trace or the head mute time, depending on the MDTFMT
!     parameter, and end at TIM_LAST. (These windows are similar to the time
!     windows in XP or MVXP except that QEST uses only fully populated windows
!     for its calculation.)
!
!  2. A Fourier transform of each time window is computed.  Only the
!     amplitude spectra are used; phase spectra are discarded.
!
!  3. For each time window, and each frequency from FREQ_BEG to FREQ_END,
!     the amplitude spectra of all input traces are averaged.
!
!  4. The trace averaged amplitudes vs. frequency are then used to
!     compute the spectral ratios:
!
!                   ln ( A(f,t(i)) / A(f,t(i-1)) ),
!
!     where f is an individual frequency; and t(i-1) and t(i) are the centers
!     of adjacent time windows.
!
!  5. A linear least squares fit of spectral ratio vs. frequency is then
!     used to compute the average interval Q between the centers of adjacent
!     time windows.
!
!  6. The shallowest interval Q found by the process is extrapolated to
!     the surface (or to mute time if MDTMFT = YES).  The deeper interval Q
!     values are then used to derive an average Q(t) function suitable for
!     use in the IQ process.  Note: QEST actually prints both the interval and
!     average Q values in the .rpt file, but only the AVERAGE Q function
!     (Qav) should be used in IQ.
!
!
! Input Subsets
!
! If HDR_IN > 0, each subset of input traces with a common value of header
! word HDR_IN will have a Q function estimated by QEST. In this case, input
! traces should be sorted with HDR_IN as a primary sort header word. You may
! set HDR_IN = 0 to estimate just one Q function from the whole dataset.
!
!
! Amplitude Spectra Bytefile
!
! If a pathname is entered in the parameter PATHNAME_BYT, then QEST writes the
! amplitude spectrum of each time window (averaged over the traces) to a
! bytefile.  The average amplitude spectrum of each time window is written to
! a separate trace.
!
! If HDR_IN > 0, then each input subset defined by a HDR_IN value causes a
! separate set of amplitude spectra to be written to the bytefile. Sets of
! spectra traces associated with each input subset are written to the bytefile
! in the same order as the subsets are encountered by the QEST process (i.e.,
! time windows vary more rapidly than HDR_IN values in the bytefile).
!
! In the spectrum traces, header word HDR_TIM_OUT carries the time at the
! window center associated with that spectrum (measured from either mute time
! or zero time according to MDTFMT parameter) and header word HDR_FREQ_OUT
! carries the average frequency of the spectrum.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! In general, it appears that approximately one second long time windows work
! well.  It is crucial to determine the optimal spectral bandwidth; i.e., the
! widest bandwidth which is relatively uncontaminated by noise, and where the
! spectral ratios display a linear decay with frequency.  Inclusion of noise
! contaminated frequencies will adversely affect the resulting Q estimation.
!
! The amplitude spectra bytefile of the input data should be examined to
! determine what time and frequency ranges are likely to be relatively
! noise-free so that sensible parameter values for QEST can be chosen.
!
!
! Goodness of Fit
!
! The spectral ratio method assumes that the underlying reflectivity sequence
! has an amplitude spectrum with a uniform average for all the time windows.
! Since there is no assurance that this will be true for any particular
! dataset, estimating Q from surface seismic data is likely to be unreliable.
!
! For this reason a goodness-of-fit measure for the least-squares linear fit,
! namely, a Correlation Coefficient (CorCoef) is included in the .rpt file.
! It may be necessary to gain experience using this process with different
! datasets to determine acceptable values for this correlation coefficient.
! For perfect fit, the correlation coefficient would be -1.0 because spectral
! ratios should be decreasing functions of frequency. Correlation coefficients
! refer to the estimated INTERVAL Q (Qint) values, which are then integrated
! to determine the average Q (Qav) values.
!
!
! Using IQ
!
! Attenuation can be corrected using the IQ process, entering the appropriate
! average Q information as estimated by QEST.  The bandwidth used in IQ should
! probably be wider than that used for QEST because IQ is a filter and will
! remove frequencies outside the specified bandwidth.
!
! After applying Q correction via IQ, verify by generating another Amplitude
! Spectra Bytefile using QEST that the correction was successful in equalizing
! the amplitude spectra for all time windows.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
!
! This process doesn't care whether input traces are gathered or ungathered.
!
! If HDR_IN > 0, input traces must be sorted by HDR_IN such that the value
! of header word HDR_IN designates data subsets for individual Q estimation.
! If HDR_IN = 0, a single Q function is estimated from all of the input traces.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process does not alter input traces.
! This process outputs the same traces as it receives.
! This process outputs traces with same gather status as the input traces.
!
! The REAL output of this process consists of estimated Q functions in the
! .rpt file and an optional generated bytefile containing spectrum traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        used but not changed
! TSTRT     starting time on trace                  used but not changed
! DT        trace sample interval                   used but not changed
!
! But note: NDPT, TSTRT and DT are reset in the generated bytefile traces
! because these are in the frequency domain instead of time domain.
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Header words used from input traces:
!
! Hwd#       Description               Action taken
! ----       -----------               ------------
! 2          HDR_TOP_MUTE              Used in limiting time windows.
! 25         HDR_LAV                   Used to check for dead traces.
! 64         HDR_BOTTOM_MUTE           Used in limiting time windows.
! HDR_IN     User-specified header     Used for defining trace groups.
!            designating groups for
!            Q estimation
!
! Header words set in optional generated bytefile (spectrum traces)
! (All OTHER header words, not listed below, are set to zero in the bytefile):
!
! Hwd#          Description            Action taken
! ----          -----------            ------------
! 1             HDR_SEQUENCE           Set to sequential spectrum number.
! 2             HDR_TOP_MUTE           Set to 1.
! 3             HDR_CURRENT_GROUP      Set to count of distinct HDR_IN values.
! 4             HDR_CURRENT_CHANNEL    Set to count of time windows.
! 5             HDR_FOLD               Set to # traces contributing to spectrum.
! 25            HDR_LAV                Set to spectrum LAV.
! 64            HDR_BOTTOM_MUTE        Set to # frequencies in spectrum trace.
! HDR_IN        User-specified header  Retained from input traces.
! HDR_TIM_OUT   User-specified header  Set to time at center of time window.
! HDR_FREQ_OUT  User-specified header  Set to average frequency of spectrum.
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date      Author        Description
!     ----      ------        -----------
! 7. 2001-04-30 Stoeckley     Replace BYTE with PERMTFILE (which uses TRCIO).
! 6. 2000-12-07 Bob Baumel    Change wrapped_up flag to skip_wrapup.
! 5. 2000-11-29 Bob Baumel    Bug fix - Don't set header HDR_IN in bytefile if
!                             HDR_IN = 0; also cosmetic/documentation changes.
! 4. 2000-06-29 Bob Baumel    Conversion to new system (combines former QEST
!                             and MWSPCT).
! 3. 1999-08-17 Baumel/Stacy  Begin using the spectral ratio method.
! 2. 1999-02-22 Goodger       Begin using the fortran90 compiler.
! 1. 1997-09-29 Lazear        Created original version.
!
! Revision history of previous MWSPCT process, now part of QEST:
!
! 4. 1999-02-03 CIB           Added average frequency calculation
! 3. 1999-01-26 Vunderink     Documentation change only.
! 2. 1999-01-25 Vunderink     Installed in newlib.
! 1. 1999-01-11 Vunderink     Original version.
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
!                   ALTERNATE INTERNAL CALLING METHODS
!
!  None provided.
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
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!
!<gui_def>
!<NS QEST Process/NC=80>
!
!       Estimate Q(t) attenuation function from surface seismic data
!
!                      Parameters for Q(t) estimation
! `---------------------------------------------------------------------------
!   HDR_IN=~~~`IIIIII         MDTFMT=~~`CC
!
!   WIN_LEN=~~`FFFFFFFFFFF    WIN_INC= `FFFFFFFFFFF    TIM_LAST=`FFFFFFFFFFF
!
!   LEN_TAPER=`FFFFFFFFFFF    FREQ_BEG=`FFFFFFFFFFF    FREQ_END=`FFFFFFFFFFF
! `---------------------------------------------------------------------------
!
!           Parameters for optional bytefile of amplitude spectra
! `----------------------------------------------------------------------------
!   PATHNAME_BYT=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!
!   FREQ_SCALE=~~`FFFFFFFFFFF     FREQ_MAX=~~~~`FFFFFFFFFFF
!
!   HDR_TIM_OUT= `IIIIII          HDR_FREQ_OUT=`IIIIII
! `----------------------------------------------------------------------------
!
!<PARMS PATHNAME_BYT[/ML=140/XST]>
!</gui_def>
!
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="HDR_IN">
!<Tip> Header word designating input trace subsets for Q estimation. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! If HDR_IN > 0, then each subset of input traces, with a common value of
! header word HDR_IN, will have a Q function estimated by QEST. In this case,
! input traces should be sorted with HDR_IN as a primary sort header word.
!
! Specify HDR_IN = 0 to estimate just one Q function from the entire dataset.
!</Help>
!
!<Help KEYWORD="MDTFMT">
!<Tip> Measure Delay Time (for attenuation calculation) From Mute Time? </Tip>
! Default = YES
! Allowed = YES (Measure delay time for attenuation calculation from mute time.)
! Allowed = NO  (Measure delay time for attenuation calculation from zero time.)
! When MDTFMT = YES, the time windows used by QEST begin at the head mute time.
! When MDTFMT = NO, the time windows used by QEST begin at the top of the trace.
!
! You must use matching settings of MDTFMT in the QEST and IQ processes.
!
! MDTFMT = YES is appropriate for deep water marine data since Q for sea water
! can, as a practical matter, be taken as infinite.
!</Help>
!
!<Help KEYWORD="WIN_LEN">
!<Tip> Length, in seconds, of the time windows. </Tip>
! Default = 1.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="WIN_INC">
!<tip> Time increment for window locations, in seconds. </tip>
!  Default = 1.0
!  Allowed = real >= DT
!</Help>
!
!<Help KEYWORD="TIM_LAST">
!<Tip> Last time to use, in seconds, for the time windows. </Tip>
! Default = end_of_trace
! Allowed = end_of_trace >= real > TSTRT
! Leaving this field blank is equivalent to selecting end_of_trace.
!</Help>
!
!<Help KEYWORD="LEN_TAPER">
!<Tip> Length of cosine taper, in seconds, for the time windows. </Tip>
! Default = 0.1
! Allowed = real >= 0.0
! Cosine taper is centered on nominal window edges (but if taper would run
! off beginning or end of the trace, taper is moved inward until it lies
! entirely on the trace).
!</Help>
!
!<Help KEYWORD="FREQ_BEG">
!<Tip> Lowest frequency to use in spectral ratio method, in Hz. </Tip>
! Default = 20.0
! Allowed = Nyquist > real >= 0.0
!</Help>
!
!<Help KEYWORD="FREQ_END">
!<Tip> Highest frequency to use in spectral ratio method, in Hz. </Tip>
! Default = 60.0
! Allowed = Nyquist >= real > FREQ_BEG
!</Help>
!
!<Help KEYWORD="PATHNAME_BYT">
!<Tip> Pathname for bytefile containing spectra for each time window. </Tip>
! Default = NONE
! Allowed = char
! If a pathname is entered in the parameter PATHNAME_BYT, then QEST writes the
! amplitude spectrum of each time window (averaged over the traces) to a
! bytefile.  The average amplitude spectrum of each time window is written to
! a separate trace.
!
! If HDR_IN > 0, then each input subset defined by a HDR_IN value causes a
! separate set of amplitude spectra to be written to the bytefile. Sets of
! spectra traces associated with each input subset are written to the bytefile
! in the same order as the subsets are encountered by the QEST process (i.e.,
! time windows vary more rapidly than HDR_IN values in the bytefile).
!
! In the spectrum traces, header word HDR_TIM_OUT carries the time at the
! window center associated with that spectrum (measured from either mute time
! or zero time according to MDTFMT parameter) and header word HDR_FREQ_OUT
! carries the average frequency of the spectrum.
!
! If PATHNAME_BYT = NONE, then no bytefile is written.
!</Help>
!
!<Help KEYWORD="FREQ_SCALE">
!<Tip> Frequency interval, in Hz, for each "second" in bytefile traces. </Tip>
! Default = 100.0
! Allowed = real > 0.0
! This is the frequency interval, in Hz, denoted by each "second" in the
! bytefile traces. The TSTRT value in the bytefile is always 0.0 since the
! spectra start at zero frequency.
!</Help>
!
!<Help KEYWORD="FREQ_MAX">
!<Tip> Maximum frequency, in Hz, for bytefile traces. </Tip>
! Default = Nyquist
! Allowed = Nyquist >= real >= FREQ_END
! The value of FREQ_MAX determines the length of traces in the bytefile. This
! should be >= FREQ_END so that the bytefile displays at least as much of the
! spectrum as is actually used for Q estimation.
!</Help>
!
!<Help KEYWORD="HDR_TIM_OUT">
!<Tip> Header word whose values are window center times. </Tip>
! Default = 48
! Allowed = 1 - NWIH
! The center of the time window corresponding to each spectrum trace is stored
! in header word HDR_TIM_OUT of the bytefile traces. Center times are measured
! from either mute time or zero time according to the MDTFMT parameter.
!</Help>
!
!<Help KEYWORD="HDR_FREQ_OUT">
!<Tip> Header word whose values are average frequency of the spectrum. </Tip>
! Default = 49
! Allowed = 1 - NWIH
! The average frequency of the spectrum will be entered in header word
! HDR_FREQ_OUT, where the average frequency is defined as the mean frequency
! weighted by the amplitude spectrum.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module qest_module
      use pc_module
      use named_constants_module
      use mem_module
      use fft_module
      use pathcheck_module
      use lav_module
      use permtfile_module
      implicit none
      private
      public :: qest_create
      public :: qest_initialize
      public :: qest_update
      public :: qest_delete
!<execute_only>
      public :: qest            ! main execution (trace processing) routine.
      public :: qest_wrapup
!</execute_only>

      character(len=100),public,save :: QEST_IDENT = &
'$Id: qest.f90,v 1.7 2001/04/26 17:26:42 sps prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: qest_struct

        private
        logical                    :: skip_wrapup      ! wrapup flag.
        integer                    :: hdr_in           ! process parameters.
        logical                    :: mdtfmt           ! process parameters.
        real                       :: win_len          ! process parameters.
        real                       :: win_inc          ! process parameters.
        real                       :: tim_last         ! process parameters.
        real                       :: len_taper        ! process parameters.
        real                       :: freq_beg         ! process parameters.
        real                       :: freq_end         ! process parameters.
        character(len=FILENAME_LENGTH) :: pathname_byt ! process parameters.
        real                       :: freq_scale       ! process parameters.
        real                       :: freq_max         ! process parameters.
        integer                    :: hdr_tim_out      ! process parameters.
        integer                    :: hdr_freq_out     ! process parameters.

        integer                    :: nwih, ndpt       ! globals.
        real                       :: tstrt, dt        ! globals.

        type(permtfile_struct),pointer :: byte         ! for bytefile.
        type(fft_struct)      ,pointer :: fft          ! for fft routine

        integer                    :: lun              ! dependent variables.
        integer                    :: win_len_samp     ! dependent variables.
        integer                    :: win_inc_samp     ! dependent variables.
        integer                    :: tim_last_samp    ! dependent variables.
        integer                    :: num_win_max      ! dependent variables.
        integer                    :: len_taper_samp   ! dependent variables.
        integer                    :: half_len_taper_samp   ! dependent vars.
        integer                    :: nfft, nnyq       ! dependent variables.
        real                       :: df               ! dependent variables.
        integer                    :: freq_beg_samp    ! dependent variables.
        integer                    :: freq_end_samp    ! dependent variables.
        integer                    :: bandwidth_samp   ! dependent variables.
        integer                    :: freq_max_samp    ! dependent variables.
        integer                    :: numgroups        ! dependent variables.
        integer                    :: numspectra       ! dependent variables.
        integer                    :: istrt1           ! dependent variables.
        logical                    :: firsttrace       ! dependent variables.
        double precision           :: hdr_group_val    ! dependent variables.
        real              ,pointer :: wincenter(:)     ! dependent variables.
        real              ,pointer :: taper(:)         ! dependent variables.
        integer           ,pointer :: numfold(:)       ! dependent variables.
        real              ,pointer :: ampspect(:,:)    ! dependent variables.

      end type qest_struct

!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(qest_struct),pointer,save :: object      ! needed for traps.

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine qest_create (obj)
      implicit none
      type(qest_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%wincenter)
      nullify (obj%taper)
      nullify (obj%numfold)
      nullify (obj%ampspect)
      nullify (obj%byte)
      nullify (obj%fft)

      call qest_initialize (obj)
      return
      end subroutine qest_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine qest_delete (obj)
      implicit none
      type(qest_struct),pointer :: obj       ! arguments

!<execute_only>
      call qest_wrapup (obj)
!</execute_only>

      call mem_free (obj%wincenter)
      call mem_free (obj%taper)
      call mem_free (obj%numfold)
      call mem_free (obj%ampspect)

      if (associated(obj%byte)) call permtfile_close (obj%byte)
      if (associated(obj%fft))  call fft_delete      (obj%fft)

      deallocate(obj)
      return
      end subroutine qest_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine qest_initialize (obj)
      implicit none
      type(qest_struct),intent(inout) :: obj       ! arguments

      call pc_get_global ('DT', obj%dt)

      obj%hdr_in       = 0
      obj%mdtfmt       = .true.
      obj%win_len      = 1.0
      obj%win_inc      = 1.0
      obj%tim_last     = fnil
      obj%len_taper    = 0.1
      obj%freq_beg     = 20.0
      obj%freq_end     = 60.0
      obj%pathname_byt = PATHCHECK_EMPTY
      obj%freq_scale   = 100.0
      obj%freq_max     = 0.5 / obj%dt
      obj%hdr_tim_out  = 48
      obj%hdr_freq_out = 49

      call qest_update (obj)
      return
      end subroutine qest_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine qest_update (obj)
      implicit none
      type(qest_struct),intent(inout),target :: obj             ! arguments

      integer     :: ntest, nscratch, nstore, iwin              ! local
      integer     :: i, i_err                                   ! local
      real        :: nyquist, wincent1, fact                    ! local

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.
      obj%lun = pc_get_lun()      ! logical unit for printing

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get_global ('NWIH'  , obj%nwih)
      call pc_get_global ('NDPT'  , obj%ndpt)
      call pc_get_global ('DT'    , obj%dt)
      call pc_get_global ('TSTRT' , obj%tstrt)

      nyquist = 0.5 / obj%dt

      call pc_get ('HDR_IN'      , obj%hdr_in)
      call pc_get ('MDTFMT'      , obj%mdtfmt)
      call pc_get ('WIN_LEN'     , obj%win_len)
      call pc_get ('WIN_INC'     , obj%win_inc)
      call pc_get ('TIM_LAST'    , obj%tim_last)
      call pc_get ('LEN_TAPER'   , obj%len_taper)
      call pc_get ('FREQ_BEG'    , obj%freq_beg)
      call pc_get ('FREQ_END'    , obj%freq_end)
      call pc_get ('PATHNAME_BYT', obj%pathname_byt)
      call pc_get ('FREQ_SCALE'  , obj%freq_scale)
      call pc_get ('FREQ_MAX'    , obj%freq_max)
      call pc_get ('HDR_TIM_OUT' , obj%hdr_tim_out)
      call pc_get ('HDR_FREQ_OUT', obj%hdr_freq_out)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

!-----Check HDR_IN
      obj%hdr_in = min (max(obj%hdr_in, 0), obj%nwih)

!-----Check WIN_LEN
      obj%win_len_samp = max (nint(obj%win_len / obj%dt), 1)  +  1
      obj%win_len_samp = min (obj%win_len_samp, obj%ndpt)
      obj%win_len = (obj%win_len_samp - 1) * obj%dt

!-----Check WIN_INC
      obj%win_inc_samp = max (nint(obj%win_inc / obj%dt), 1)
      obj%win_inc = obj%win_inc_samp * obj%dt

!-----Check TIM_LAST
      if (obj%tim_last == fnil) then
        obj%tim_last_samp = obj%ndpt
      else
        obj%tim_last_samp = nint((obj%tim_last - obj%tstrt)/obj%dt) + 1
        obj%tim_last_samp = min (max(obj%tim_last_samp, 1), obj%ndpt)
        obj%tim_last = obj%tstrt + (obj%tim_last_samp - 1) * obj%dt
      end if

!-----Check LEN_TAPER
      if (obj%len_taper < 0.0) then
        call pc_error ('LEN_TAPER must be non-negative.')
        obj%len_taper = 0.0
      end if
      obj%half_len_taper_samp = nint (0.5 * obj%len_taper / obj%dt)
      obj%len_taper_samp      = 2 * obj%half_len_taper_samp  +  1
      obj%len_taper           = (obj%len_taper_samp - 1) * obj%dt

!-----Find number of windows that can fit in trace
      if (.not.obj%mdtfmt .and. obj%tstrt<0.0) then
        obj%istrt1 = min (nint(1.0 - obj%tstrt/obj%dt), obj%ndpt + 1)
      else
        obj%istrt1 = 1
      end if
      obj%num_win_max = max ((obj%tim_last_samp - obj%win_len_samp + 1 &
                        - obj%istrt1 + obj%win_inc_samp) / obj%win_inc_samp, 0)
      if (obj%num_win_max < 2) then
        call pc_error ('Fewer than two time windows available for QEST. &
                       &Please adjust WIN_LEN, WIN_INC, and TIM_LAST to &
                       &provide at least two windows.')
      end if

!-----Check FREQ_BEG and FREQ_END
      obj%freq_beg = min (max(obj%freq_beg, 0.0), nyquist)
      obj%freq_end = min (max(obj%freq_end, 0.0), nyquist)
      if (obj%freq_end <= obj%freq_beg) then
        if (pc_verify_scalar('FREQ_BEG')) then
          obj%freq_end = obj%freq_beg
        else
          obj%freq_beg = obj%freq_end
        end if
        call pc_error ('FREQ_END must be greater than FREQ_BEG.')
      end if

!-----Check PATHNAME_BYT and bytefile-dependent parameters
      call pathcheck ('PATHNAME_BYT', obj%pathname_byt, '.byt')
      if (obj%pathname_byt /= PATHCHECK_EMPTY) then
        call pc_put_sensitive_field_flag ('FREQ_SCALE'  , .true.)
        call pc_put_sensitive_field_flag ('FREQ_MAX'    , .true.)
        call pc_put_sensitive_field_flag ('HDR_TIM_OUT' , .true.)
        call pc_put_sensitive_field_flag ('HDR_FREQ_OUT', .true.)
        if (obj%freq_scale <= 0.0) then
          call pc_error ('FREQ_SCALE must be positive.')
        end if
      else
        call pc_put_sensitive_field_flag ('FREQ_SCALE'  , .false.)
        call pc_put_sensitive_field_flag ('FREQ_MAX'    , .false.)
        call pc_put_sensitive_field_flag ('HDR_TIM_OUT' , .false.)
        call pc_put_sensitive_field_flag ('HDR_FREQ_OUT', .false.)
      end if
      obj%freq_max = min (max(obj%freq_max, obj%freq_end), nyquist)
      obj%hdr_tim_out  = min (max(obj%hdr_tim_out,  1), obj%nwih)
      obj%hdr_freq_out = min (max(obj%hdr_freq_out, 1), obj%nwih)

!-----Determine FFT radix and related quantities
      ntest = 4 * (obj%win_len_samp + obj%len_taper_samp - 1)
      obj%nfft = 8
      do while (obj%nfft < ntest)
        obj%nfft = 2 * obj%nfft
      end do
      obj%nnyq = obj%nfft / 2  +  1
      obj%df = 1.0 / (obj%nfft * obj%dt)
      obj%freq_beg_samp  = nint (obj%freq_beg / obj%df)  +  1
      obj%freq_end_samp  = nint (obj%freq_end / obj%df)  +  1
      obj%bandwidth_samp = obj%freq_end_samp - obj%freq_beg_samp + 1
      if (obj%pathname_byt /= PATHCHECK_EMPTY) then
        obj%freq_max_samp = nint (obj%freq_max / obj%df)  +  1
      else
        obj%freq_max_samp = obj%freq_end_samp
      end if

!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!
!!------------------------- call processes internally ----------------------!!




!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put ('HDR_IN'      , obj%hdr_in)
      call pc_put ('MDTFMT'      , obj%mdtfmt)
      call pc_put ('WIN_LEN'     , obj%win_len)
      call pc_put ('WIN_INC'     , obj%win_inc)
      call pc_put ('TIM_LAST'    , obj%tim_last)
      call pc_put ('LEN_TAPER'   , obj%len_taper)
      call pc_put ('FREQ_BEG'    , obj%freq_beg)
      call pc_put ('FREQ_END'    , obj%freq_end)
      call pc_put ('PATHNAME_BYT', obj%pathname_byt)
      call pc_put ('FREQ_SCALE'  , obj%freq_scale)
      call pc_put ('FREQ_MAX'    , obj%freq_max)
      call pc_put ('HDR_TIM_OUT' , obj%hdr_tim_out)
      call pc_put ('HDR_FREQ_OUT', obj%hdr_freq_out)

      nscratch = max ( 2 * obj%nnyq  +  obj%nfft,                   &
                       obj%num_win_max * (obj%bandwidth_samp + 3),  &
                       2 * obj%nwih * obj%num_win_max )
      nstore = (obj%freq_max_samp + 2) * obj%num_win_max &
                 + obj%len_taper_samp + fft_mem_usage (obj%nfft, 'rtoc')
      call pc_put_control ('NSCRATCH', nscratch)
      call pc_put_control ('NSTORE'  , nstore)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

      call mem_free (obj%wincenter)
      call mem_free (obj%taper)
      call mem_free (obj%numfold)
      call mem_free (obj%ampspect)

      if (associated(obj%byte)) call permtfile_close (obj%byte)
      if (associated(obj%fft))  call fft_delete      (obj%fft)

      obj%numspectra = 0
      obj%numgroups  = 0
      obj%hdr_group_val = -1.0D30
      obj%firsttrace = .true.

!<execute_only>

      if (pc_do_not_process_traces()) return

      write (obj%lun, *) 'Fourier transform size =', obj%nfft
      write (obj%lun, *) 'Frequency increment =', obj%df
      write (obj%lun, *) 'Number of frequencies in Spectral Bandwidth =', &
                          obj%bandwidth_samp

!-----Allocate memory for WINCENTER, TAPER, NUMFOLD, and AMPSPECT arrays
      call mem_alloc (obj%wincenter, obj%num_win_max)
      call mem_alloc (obj%taper    , obj%len_taper_samp)
      call mem_alloc (obj%numfold  , obj%num_win_max)
      call mem_alloc (obj%ampspect , obj%freq_max_samp, obj%num_win_max)

      if (pc_do_not_process_traces()) return   ! in case of allocation errors.

!-----Initialize the WINCENTER, NUMFOLD, and AMPSPECT arrays
      wincent1 = 0.5 * obj%win_len
      if (.not.obj%mdtfmt) then
        wincent1 = wincent1 + obj%tstrt + (obj%istrt1 - 1)*obj%dt
      end if
      do iwin = 1, obj%num_win_max
        obj%wincenter(iwin) = wincent1 + (iwin - 1)*obj%win_inc
      end do
      obj%numfold  = 0
      obj%ampspect = 0.0

!-----Initialize TAPER array with a cosine taper
      fact = 0.5 * PI / (obj%len_taper_samp + 1)
      if (obj%len_taper_samp > 1) then
        do i = 1, obj%len_taper_samp
          obj%taper(i) = sin (fact * i) ** 2
        end do
      else
        obj%taper(1) = 1.0
      end if

!-----Initialize FFT routine
!-----Note: Set opt_scale=2.0 to match SPCT and Cray scaling
      i_err = fft_create (obj%fft, -1, obj%nfft, 'rtoc', opt_scale=2.0)
      if (i_err /= 0) call pc_error ('Error initializing FFT object')

!-----Open bytefile if if needed
      if (obj%pathname_byt /= PATHCHECK_EMPTY) then
        call permtfile_open_write (obj%byte, obj%pathname_byt, obj%nwih, &
                                   obj%freq_max_samp, 0.0,               &
                                   obj%df / obj%freq_scale,              &
                                   obj%lun, i_err, pc_get_ipn(), 8)
        if (i_err /= PERMTFILE_OK) call pc_error ('Error opening bytefile')
      end if

      if (pc_do_not_process_traces()) return   ! in case of errors.

      obj%skip_wrapup = .false.    ! needed for the wrapup routine.

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine qest_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine qest (obj, ntr, hd, tr)
      implicit none
      type(qest_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(inout) :: ntr                    ! arguments
      double precision ,intent(inout) :: hd(:,:)                ! arguments
      real             ,intent(inout) :: tr(:,:)                ! arguments

      integer    :: itrace, i_err                               ! local

      if (ntr >= 1) then

        do itrace = 1, ntr
          if (obj%firsttrace) then
            if (obj%hdr_in > 0) obj%hdr_group_val = hd(obj%hdr_in, itrace)
            obj%firsttrace = .false.
          else if (obj%hdr_in > 0) then
            if (hd(obj%hdr_in, itrace) /= obj%hdr_group_val) then
              call qest_estimate_q_function (obj)
              if (obj%pathname_byt /= PATHCHECK_EMPTY) then
                call qest_write_to_bytefile (obj, i_err)
                if (i_err /= 0) then
                  ntr = FATAL_ERROR
                  exit
                end if
              end if
              obj%hdr_group_val = hd(obj%hdr_in, itrace)
              obj%numfold  = 0
              obj%ampspect = 0.0
            end if
          end if
          call qest_accumulate_spectra (obj, hd(:,itrace), tr(:,itrace))
        end do

      else if (ntr == NO_MORE_TRACES) then

        if (obj%firsttrace) then
          write (obj%lun, *) 'QEST: No input traces received - Nothing &
                             &to compute.'
        else
          call qest_estimate_q_function (obj)
          if (obj%pathname_byt /= PATHCHECK_EMPTY) then
            call qest_write_to_bytefile (obj, i_err)
            if (i_err /= 0) ntr = FATAL_ERROR
          end if
        end if

      elseif ( ntr == NEED_TRACES ) then
        return
      else
        call pc_print ('QEST: Received illegal value NTR =', ntr)
        ntr = FATAL_ERROR

      end if

      if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
        call qest_wrapup (obj)
      end if
      return
      end subroutine qest

!!----------------------- qest_accumulate_spectra --------------------------!!
!!----------------------- qest_accumulate_spectra --------------------------!!
!!----------------------- qest_accumulate_spectra --------------------------!!

      subroutine qest_accumulate_spectra (obj, hd, tr)
      implicit none
      type(qest_struct),intent(inout) :: obj                    ! arguments
      double precision ,intent(in)    :: hd(:)                  ! arguments
      real             ,intent(in)    :: tr(:)                  ! arguments

      integer   :: mute1, mute2                                 ! local
      integer   :: iwin, win_start, win_end                     ! local
      integer   :: start_1st_taper, end_last_taper              ! local
      integer   :: start_last_taper                             ! local
      integer   :: len_trace_used, len_taper_used               ! local
      real      :: padded_trace (obj%nfft)                      ! local
      complex   :: spectrum_trace (obj%nnyq)                    ! local

!-----Do nothing if dead trace
      if (hd(HDR_LAV) == 0.0D0) return

!-----Locate mute times of this trace
      mute1 = nint(hd(HDR_TOP_MUTE))
      mute2 = nint(hd(HDR_BOTTOM_MUTE))

!-----Loop over trace windows (Start from mute if MDTFMT = YES)
      if (obj%mdtfmt) obj%istrt1 = mute1
      iwin = 0
      do win_start = obj%istrt1, &
                     min(obj%tim_last_samp, mute2) - obj%win_len_samp + 1, &
                     obj%win_inc_samp
        iwin = iwin + 1
        if (win_start < mute1) cycle
        win_end = win_start + obj%win_len_samp - 1

!-------Taper the windowed trace and put in PADDED_TRACE array
        start_1st_taper = max (win_start - obj%half_len_taper_samp, 1)
        end_last_taper = min (win_end + obj%half_len_taper_samp, obj%ndpt)
        len_trace_used = end_last_taper - start_1st_taper + 1
        len_taper_used = min (obj%len_taper_samp, len_trace_used)

        padded_trace(:len_trace_used) = tr(start_1st_taper:end_last_taper)

        padded_trace(:len_taper_used)  = obj%taper(:len_taper_used) &
                                          * padded_trace(:len_taper_used)
        start_last_taper = len_trace_used - len_taper_used + 1
        padded_trace(start_last_taper:len_trace_used)  &
           = obj%taper(len_taper_used:1:(-1))          &
              * padded_trace(start_last_taper:len_trace_used)
        padded_trace(len_trace_used+1:) = 0.0

!-------Compute FFT of tapered, windowed trace
        call fft_rc_transform (obj%fft, padded_trace, spectrum_trace)

!-------Increment ampspect and numfold arrays
        obj%ampspect(:,iwin) = obj%ampspect(:,iwin) &
                               + abs(spectrum_trace(:obj%freq_max_samp))
        obj%numfold(iwin) = obj%numfold(iwin) + 1
      end do

      return
      end subroutine qest_accumulate_spectra

!!----------------------- qest_estimate_q_function -------------------------!!
!!----------------------- qest_estimate_q_function -------------------------!!
!!----------------------- qest_estimate_q_function -------------------------!!

      subroutine qest_estimate_q_function (obj)
!!------------------------------------------------------------------------!!
!!   Note: In addition to estimating and printing a Q(t) function, this   !!
!!   routine also normalizes the obj%ampspect array in the parameter      !!
!!   structure. Therefore, this routine should be called before calling   !!
!!   subroutine qest_write_to_bytefile.                                   !!
!!------------------------------------------------------------------------!!
      implicit none
      type(qest_struct),intent(inout) :: obj                    ! arguments

      real, parameter :: vnil_private = -1.e30, vbig_private = 1.e30
      integer  :: iwin, ifreq, foldmax, ncount                  ! local
      integer  :: firstwin, lastwin                             ! local
      real     :: fact, avgf, avgr, sumf2, sumr2, sumfr, slope  ! local
      real     :: corr_coeff (obj%num_win_max)                  ! local
      real     :: qint (obj%num_win_max)                        ! local
      real     :: qav (obj%num_win_max)                         ! local
      real     :: ratios (obj%bandwidth_samp, obj%num_win_max)  ! local

      write (obj%lun, *)
      if (obj%hdr_in > 0) then
        write (obj%lun, *) 'QEST: Q-function estimation for group with &
                           &header value', obj%hdr_group_val
      else
        write (obj%lun, *) 'QEST: Q-function estimation using all input &
                           &traces'
      end if

!-----Normalize the amplitude spectra
      foldmax = 0
      do iwin = 1, obj%num_win_max
        if (obj%numfold(iwin) > 0) then
          fact = 1.0 / obj%numfold(iwin)
          obj%ampspect(:,iwin) = obj%ampspect(:,iwin) * fact
          foldmax = max (foldmax, obj%numfold(iwin))
        end if
      end do
      if (foldmax == 0) then
        write (obj%lun, *) 'Nothing to compute - No live spectra to work &
                           &with!'
        return
      end if

!-----Find spectral ratios - First put logs of spectra in RATIOS array
      do iwin = 1, obj%num_win_max
        do ifreq = obj%freq_beg_samp, obj%freq_end_samp
          if (obj%ampspect(ifreq,iwin) > 0.0) then
            ratios(ifreq - obj%freq_beg_samp + 1, iwin) &
                      = log (obj%ampspect(ifreq,iwin))
          else
            ratios(ifreq - obj%freq_beg_samp + 1, iwin) = vnil_private
          end if
        end do
      end do

!-----Find spectral ratios - Now difference the logs in RATIOS array
      do iwin = obj%num_win_max, 2, -1
        do ifreq = 1, obj%bandwidth_samp
          if (ratios(ifreq,iwin) == vnil_private &
               .or. ratios(ifreq,iwin-1) == vnil_private) then
            ratios(ifreq,iwin) = vnil_private
          else
            ratios(ifreq,iwin) = ratios(ifreq,iwin) - ratios(ifreq,iwin-1)
          end if
        end do
      end do

!-----Perform least squares fitting of frequency vs. spectral ratio
!-----Note: Qint values computed here are RECIPROCALS of actual Qint values
      do iwin = 2, obj%num_win_max
        ncount = 0
        avgf   = 0.0
        avgr   = 0.0
        do ifreq = 1, obj%bandwidth_samp
          if (ratios(ifreq,iwin) /= vnil_private) then
            avgf = avgf + ifreq
            avgr = avgr + ratios(ifreq,iwin)
            ncount = ncount + 1
          end if
        end do
        if (ncount > 1) then
          avgf = avgf / ncount
          avgr = avgr / ncount
        else
          corr_coeff(iwin) = 0.0
          qint(iwin) = vnil_private
          cycle
        end if
        sumf2 = 0.0
        sumr2 = 0.0
        sumfr = 0.0
        do ifreq = 1, obj%bandwidth_samp
          if (ratios(ifreq,iwin) /= vnil_private) then
            sumf2 = sumf2 + (ifreq - avgf)**2
            sumr2 = sumr2 + (ratios(ifreq,iwin) - avgr)**2
            sumfr = sumfr + (ifreq - avgf)*(ratios(ifreq,iwin) - avgr)
          end if
        end do
        if (sumr2 > 0.0) then
          corr_coeff(iwin) = sumfr / sqrt(sumf2*sumr2)
        else
          corr_coeff(iwin) = 0.0
        end if
        slope = sumfr / (sumf2 * obj%df)
        if (slope < 0.0) then
          qint(iwin) = abs(slope) &
                        / (PI * (obj%wincenter(iwin) - obj%wincenter(iwin-1)))
        else
          qint(iwin) = 0.0
        end if
      end do

!-----Extrapolate Qint from shallowest good window to the surface
      firstwin = 0
      do iwin = 2, obj%num_win_max
        if (qint(iwin) > 0.0) then
          qav(1:iwin) = 1./qint(iwin)
          qint(iwin) = qav(iwin)
          firstwin = iwin
          exit
        end if
      end do
      if (firstwin == 0) then
        write (obj%lun, *) 'No Qint values could be calculated.'
        return
      end if

!-----Convert Qint to Qav, extending calculation as deeply as possible
      lastwin = firstwin
      do iwin = firstwin + 1, obj%num_win_max
        if (qint(iwin) == vnil_private) exit
        qav(iwin) = obj%wincenter(iwin)  /    &
           (obj%wincenter(iwin-1)/qav(iwin-1) &
                  + (obj%wincenter(iwin)-obj%wincenter(iwin-1))*qint(iwin))
        if (qint(iwin) > 0.0) then
          qint(iwin) = 1.0 / qint(iwin)
        else
          qint(iwin) = vbig_private
        end if
        lastwin = iwin
      end do

!-----Print report of results
      if (obj%mdtfmt) then
        write (obj%lun, *) &
          '     Note: All times are relative to the mute time'
      else
        write (obj%lun, *) &
          '     Note: All times are relative to time zero'
      end if
      write (obj%lun, *) &
          '     Note: Qint shown for information; use Qav in IQ'
      write (obj%lun, *) &
          '##########################################################'
      write (obj%lun, *) &
          '          Time     CorCoef       Qint         Qav'
      do iwin = 1, lastwin
        if (iwin < firstwin) then
          write (obj%lun,'(5x, F10.3, 26x, F10.0)') &
                obj%wincenter(iwin), qav(iwin)
        else
          write (obj%lun,'(5x, F10.3, 2x, F10.6, 2x, F10.0, 2x, F10.0)')  &
                obj%wincenter(iwin), corr_coeff(iwin), qint(iwin), qav(iwin)
        end if
      end do
      write (obj%lun, *) &
          '##########################################################'
      write (obj%lun, *)

      return
      end subroutine qest_estimate_q_function

!!----------------------- qest_write_to_bytefile ---------------------------!!
!!----------------------- qest_write_to_bytefile ---------------------------!!
!!----------------------- qest_write_to_bytefile ---------------------------!!

      subroutine qest_write_to_bytefile (obj, i_err)
      implicit none
      type(qest_struct),intent(inout) :: obj                    ! arguments
      integer          ,intent(out)   :: i_err                  ! arguments

      integer          :: itrace, ifreq, ntr                    ! local
      real             :: sumamp, freqavg                       ! local
      double precision :: hdtemp (obj%nwih, obj%num_win_max)    ! local

      i_err = 0
      obj%numgroups = obj%numgroups + 1

!-----Set hdtemp array (first initialize hdtemp to zero)
      hdtemp = 0.0D0

!-----This loop sets all but the LAV header word
      do itrace = 1, obj%num_win_max
        hdtemp (HDR_SEQUENCE       , itrace) = obj%numspectra + itrace
        hdtemp (HDR_TOP_MUTE       , itrace) = 1.0D0
        hdtemp (HDR_CURRENT_GROUP  , itrace) = obj%numgroups
        hdtemp (HDR_CURRENT_CHANNEL, itrace) = itrace
        hdtemp (HDR_FOLD           , itrace) = obj%numfold(itrace)
        hdtemp (HDR_BOTTOM_MUTE    , itrace) = obj%freq_max_samp
        if (obj%hdr_in > 0) then
          hdtemp (obj%hdr_in       , itrace) = obj%hdr_group_val
        end if
        hdtemp (obj%hdr_tim_out    , itrace) = obj%wincenter (itrace)
!-------Find average frequency for HDR_FREQ_OUT
        sumamp  = 0.0
        freqavg = 0.0
        do ifreq = 1, obj%freq_max_samp
          sumamp  = sumamp  + obj%ampspect(ifreq,itrace)
          freqavg = freqavg + (ifreq-1)*obj%ampspect(ifreq,itrace)
        end do
        if (sumamp /= 0.0) freqavg = freqavg * obj%df / sumamp
        hdtemp (obj%hdr_freq_out   , itrace) = freqavg
      end do
      obj%numspectra = obj%numspectra + obj%num_win_max

!-----Now set the LAV header word
      ntr = obj%num_win_max
      call lav_set_hdr (hdtemp, obj%ampspect, obj%freq_max_samp, ntr)

!-----Write to bytefile
      do itrace = 1,ntr
        call permtfile_write &
                 (obj%byte, hdtemp(:,itrace), obj%ampspect(:,itrace), i_err)
        if (i_err /= PERMTFILE_OK) then
          write (obj%lun, *) 'QEST: Error writing to bytefile'
          i_err = 1
          exit
        end if
      end do

      return
      end subroutine qest_write_to_bytefile

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine qest_wrapup (obj)
      implicit none
      type(qest_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      if (associated(obj%byte)) call permtfile_close (obj%byte)
      if (associated(obj%fft))  call fft_delete      (obj%fft)

      call mem_free (obj%wincenter)
      call mem_free (obj%taper)
      call mem_free (obj%numfold)
      call mem_free (obj%ampspect)

      return
      end subroutine qest_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module qest_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
