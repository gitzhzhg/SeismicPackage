!<CPS_v1 type="PROCESS"/>

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
! Name       : MTFUN (Multiple Transfer Function Process) [includes former TFUN]
! Category   : filters
! Written    : 1995-11-02   by: Donna Vunderink
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Compute transfer functions between standard traces and sets of
!              "apply" traces.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Within each bin, MTFUN calculates a transfer function operator between the
! set of standard traces and each set of apply traces and outputs the transfer
! functions as output traces from the process.
!
!
! Bins
!
! MTFUN uses a grid of bins defined by the bin parameters (HDR_X, HDR_Y, etc.)
! and calculates a transfer function operator between the set of standard
! traces and each set of apply traces.  This calculation is done independently
! on traces occupying each bin.
!
!
! Standard and Apply Traces
!
! MTFUN identifies traces as standard traces that have header word HDR_TYPE set
! to a value of VAL_STAND (typically 1.0).  Apply traces are identified by
! having header word HDR_TYPE set to a value NOT equal to VAL_STAND.  Within
! each bin, all standard traces are composited to a single trace and all apply
! traces, within each set, are composited to a single trace prior to the
! transfer function calculation.
!
! It is possible to define more than one set of apply traces - each with its
! own distinct value of HDR_TYPE.  Transfer functions are calculated between
! the standard traces and each set of apply traces and all transfer functions
! are output from the process.  IT IS USUALLY MOST CONVENIENT TO SPECIFY JUST
! ONE SET OF APPLY TRACES.
!
!
! Transfer Functions
!
! We wish to find a convolutional operator of specified length (LEN_OP) such
! that the apply trace convolved with the operator is as close as possible to
! the standard trace in the least squares sense.  (This is also known as a
! Wiener filter calculation.)  This operator is referred to informally as a
! transfer function.  The transfer functions that MTFUN calculates are passed
! out as output traces.
!
!
! Time Shift and Phase Shift Calculation
!
! For each transfer function calculated, MTFUN calculates an effective
! equivalent time shift and phase shift.  It takes the phase spectrum, unwraps
! it and does a least squares linear fit (between the values of FREQ_BEG and
! FREQ_END).  The slope of the linear fit is used to calculate the time shift
! and the phase intercept is the phase shift.  These values are put in the
! transfer function headers and the (optional) diagnostic file.
!
!
! Mute Header Words
!
! MTFUN only uses traces samples within the range from the head mute to the tail
! mute, inclusive.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! Input Trace Preparation
!
! Input traces must have the proper value set in HDR_TYPE (using SETWORD) to
! identify them as standard or apply traces.  Input traces must also be sorted
! so that all traces belonging to a particular bin are input consecutively.
! This can be done with TSORT as follows:
!
!   TSORT (HDR_PRI = HDR_X, HDR_SEC = HDR_Y, HDR_TERT = HDR_TYPE).
!
!
! Transfer Function Header Words
!
! Transfer function operators are passed out of MTFUN as output traces with a
! header word format as follows:
!
!        Header                 Definition
!         Word
!
!        1        Sequential trace number
!        2        Top mute (1)
!        3        Sample interval (DT)
!        4        Time of first sample (-LEN_OP/2); this places the operator
!                  center at zero time
!        5        Calculated time shift, in seconds
!        6        Calculated phase shift, in degrees
!        9        Maximum correlation coefficient between standard and apply
!                 traces BEFORE convolving with the operator
!        10       Maximum correlation coefficient between standard and apply
!                 traces AFTER convolving with the operator
!        11       Value of HDR_TYPE for the apply trace set
!        12       Value of HDR_X to identfy bin
!        13       Value of HDR_Y to identfy bin
!        25       LAV of transfer function
!        64       Bottom mute (LEN_OP)
!
! This information is also optionally written to the diagnostic file.
!
!
! Processing of Transfer Functions
!
! Apart from the obvious use of convolving with the apply traces, the output
! transfer functions can also be further processed.  Examples include:
!
!        1.  Transfer functions can be selected by quality as based on the
!        correlation coefficients in the headers.
!
!        2.  Transfer functions can be adjusted in time by using RTC.
!
!        3.  Transfer functions can be composited to reduce the effect of noise.
!
!
! Use of Diagnostic File
!
! Information in the transfer function headers is also optionally written to a
! diagnostic trcio file.  This file can be used to make various diagnostic 
! plots.
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! Process is a multiple-trace process.
!
! Process requires traces to be input in bin order with HDR_TYPE set to
! identify standard and apply traces.
!
! This process intputs one trace at a time.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process outputs transfer function traces only.
!
! This process alters input traces.
!
! This process outputs one trace at a time.
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
!  2      Head mute index            Used
!  64     Tail mute index            Used
!         HDR_TYPE                   Identifies standard or apply traces
!         HDR_X, HDR_Y               Identifies bins
!
! Transfer function operators are passed out of MTFUN as output traces with a
! header word format as follows:
!
!        Header                 Definition
!         Word
!
!        1        Sequential trace number
!        3        Sample interval (DT)
!        4        Time of first sample (-LEN_OP/2); 
!                        this places the operator
!                        center at zero time
!        5        Calculated time shift, in seconds
!        6        Calculated phase shift, in Hz
!        9        Maximum correlation coefficient between standard and apply
!                        traces BEFORE convolving with the operator
!        10        Maximum correlation coefficient between standard and apply
!                        traces AFTER convolving with the operator
!        11        Value of HDR_TYPE for the apply trace set
!        12        Value of HDR_X to identfy bin
!        13        Value of HDR_Y to identfy bin
!        25        LAV of transfer function
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 20. 2006-06-20  B. Menger    Removed Unused Variables.
! 19. 2004-06-08  Selzler      Fixed memory overrun bug, when input NWIH > 64
! 18. 2002-09-17  Goodger      Use mth module for binning.  mtfun was NOT
!                              adding 1 to the bin calculation as does the
!                              mth_module.
! 17. 2002-02-14  Selzler      Corrected GUI field length for PATHNAME_DIAG
!                              file selection dialogue.
! 16. 2001-11-16  Selzler      Increase GUI field length for PATHNAME_DIAG and
!                              add file selection dialogue.
! 15. 2001-06-27  Selzler      Bug Report #415, TSTRT validation. PRODUCTION.
! 14. 2001-01-15  Brad Kruse   Bug Report #237, First output trace is dead, 
!                              output traces are duplicated.
! 13. 2001-01-04  Brad Kruse   Bug report #220, no PATHNAME_DIAG file, first 
!                              trace is dead, and help on PATHNAME_DIAG is 
!                              misleading (file is text, not TRCIO).
!                              Bug report #225, HW10, After Correlation, is 
!                              incorrect; documentation incorrectly labels
!                              phase unit, HW6, as 'Hz' which should be 
!                              'degrees'
! 12. 2000-12-11  Brad Kruse   Change name for wrapup flag to SKIP_WRAPUP for
!                              clarity, and slightly change how it is used.
! 11. 2000-12-11  Brad Kruse   CPS Conversion
! 10. 1998-11-20 Vunderink  Begin using the f90 compiler.
!  9. 1996-03-12 Vunderink  Fixed an array reuse problem resulting in bad
!                           transfer functions
!  8. 1996-02-27 Vunderink  Changed to hardcode location for "apply" type,
!                           X bin value, and Y bin value header words in
!                           transfer function.
!  7. 1996-02-27 Vunderink  Fixed calculation for maximum STROT traces
!  6. 1996-02-27 Vunderink  Check minimum window length via standard-apply
!                           pair instead of entire bin
!  5. 1996-02-26 Vunderink  Made sure bin number is 0 when HWX or HWY are
!                           set to 0.
!  4. 1996-02-14 Vunderink  Added argument to STROTSI call to pass IPN of
!                           calling process for history-folder processing.
!  3. 1995-11-13 Vunderink  Total the number of "No standard trace",
!                           "No apply trace", and "Not enough data" occurences.
!  2. 1995-11-08 Vunderink  Fixed problem calculating new bins when XINC or
!                           YINC is not 1.  Changed to not output NULL
!                           transfer function traces.  Shorted length of
!                           transfer function traces to OPLEN.  Added
!                           calculation of before and after correlation
!                           coefficients.  Added option to use only one
!                           header word for binning.
!  1. 1995-11-02 Vunderink  Original version.
!
!-------------------------------------------------------------------------------
!</history_doc>

!<portability_doc>
!-------------------------------------------------------------------------------
!                        PORTABILITY LIMITATIONS
!
! No known limitations.
!
!
!
!
!-------------------------------------------------------------------------------
!</portability_doc>

!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
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
!
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<gui_def>
!<NS MTFUN Process/NC=84/NR=23>
!
!                  Multiple Transfer Function Process Process
! Compute transfer functions between standard traces and sets of "apply" traces.
!
!        HDR_TYPE `IIIIII    VAL_STAND `FFFFFFFFFFFF
!
!        NUM_SETS `IIIIIIII
!
!         HDR_X~~`IIIIII               HDR_Y~~`IIIIII
!         X_INIT `FFFFFFFFFFFF         Y_INIT `FFFFFFFFFFFF
!         X_INC~~`FFFFFFFFFFFF         Y_INC~~`FFFFFFFFFFFF
!
!        LEN_OP `FFFFFFFFFFFF
!
!        DIAG_LOAD `FFFFFFFFFFFF (Diagonal load)
!
!        TIM_BEG `FFFFFFFFFFFF    FREQ_BEG `FFFFFFFFFFFF
!        TIM_END `FFFFFFFFFFFF    FREQ_END `FFFFFFFFFFFF
!        WIN_MIN `FFFFFFFFFFFF
!
!Select PATHNAME_DIAG[PATHNAME_DIAG]=`QSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
!               [PATHNAME_DIAG_INFO] `XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!<PARMS PATHNAME_DIAG[/ML=128/XST]>
!<PARMS PATHNAME_DIAG_INFO[/ML=128/XST]>
!</gui_def>
!
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!
!<HelpSection>
!
! ---------------------trace identification parameters--------------------------
!
!
!<Help KEYWORD="HDR_TYPE">
!<Tip> Header word labeling trace type (standard or apply). </Tip>
! Default = 48
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="VAL_STAND">
!<Tip> Value of HDR_TYPE indicating a standard trace. </Tip>
! Default = 1.0
! Allowed = real
! Standard traces are labeled by a value of VAL_STAND in header word HDR_TYPE.
! Traces with a value of header word HDR_TYPE different from VAL_STAND will be
! used as apply traces.  Apply traces with different values of HDR_TYPE are
! used as distinct sets of apply traces.
!</Help>
!
!<Help KEYWORD="NUM_SETS">
!<Tip> Number of distinct sets of apply traces. </Tip>
! Default = 1
! Allowed = int > 0
! NUM_SETS is the number of distinct sets of apply traces.  Within each bin,
! MTFUN will generate transfer functions between the standard set of traces and
! each distinct set of apply traces.  Each distinct set of apply traces has a
! different value of HDR_TYPE that is NOT equal to VAL_STAND.
!
! IT IS USUALLY MOST CONVENIENT TO SPECIFY JUST ONE SET OF APPLY TRACES.
!</Help>
!
! -----------------------------bin parameters----------------------------------
!
!<Help KEYWORD="HDR_X">
!<Tip> Header word forfirst arbitrary bin coordinate. </Tip>
! Default = 8
! Allowed = 0 - NWIH
! HDR_X is the header word for the first arbitrary bin coordinate 
! (typically inline stack bin coordinate).
!
! If HDR_X = 0, then do not use this coordinate.  Either HDR_X or HDR_Y
! must be greater than zero.
!</Help>
!
!<Help KEYWORD="X_INIT">
!<Tip> Value of HDR_X at the center of the first bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="X_INC">
!<Tip> Increment of HDR_X values between occupied bins. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!
!<Help KEYWORD="HDR_Y">
!<Tip> Header word for second arbitrary bin coordinate. </Tip>
! Default = 0
! Allowed = 0 - NWIH
! HDR_Y is the header word for the second arbitrary bin coordinate (typically
! crossline stack bin coordinate).
!
! If HDR_Y = 0, then do not use this coordinate.  Either HDR_X or HDR_Y
! must be greater than zero.
!</Help>
!
!<Help KEYWORD="Y_INIT">
!<Tip> Value of HDR_Y at the center of the first bin. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
!<Help KEYWORD="Y_INC">
!<Tip> Increment of HDR_Y values between occupied bins. </Tip>
! Default = 1.0
! Allowed = real
!</Help>
!
! ---------------------transfer function parameters-----------------------------
!
!<Help KEYWORD="LEN_OP">
!<Tip> Length of operator, in seconds, for transfer function calculation. </Tip>
! Default = 0.4
! Allowed = real > 0.0
! The operator is centered on zero time (with times running from -LEN_OP/2 to
! +LEN_OP/2).
!</Help>
!
!<Help KEYWORD="DIAG_LOAD">
!<Tip> Diagonal load, in percent, for transfer function calculation. </Tip>
! Default = 2.0
! Allowed = real > 0.0
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Top of trace window, in seconds.  </Tip>
! Default = TSTRT
! Allowed = real
! Top of trace window, in seconds, for transfer function calculation.
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> Bottom of trace window, in seconds.  </Tip>
! Default = end of trace
! Allowed = real > TIM_BEG
! Bottom of trace window, in seconds, for transfer function calculation.
!</Help>
!
!<Help KEYWORD="WIN_MIN">
!<Tip> Minimum window length, in seconds.  </Tip>
! Default = 0.2
! Allowed = real > 0.0
! Minimum window length, in seconds, required for transfer function calculation.
! Within any bin the standard and apply traces must share a live trace interval
! of at least WIN_MIN seconds, within the specified trace window, or the
! transfer function calculation will not be made.
!</Help>
!
!<Help KEYWORD="FREQ_BEG">
!<Tip> Low frequency, in Hz, for time shift and phase shift calculation.  </Tip>
! Default = 8.0
! Allowed = real >= 0.0
! MTFUN calculates the effective time shift and phase shift associated with each
! transfer function by assuming the phase spectrum to be linear.  FREQ_BEG and
! FREQ_END are used for this calculation only; no frequency filtering is done.
!</Help>
!
!<Help KEYWORD="FREQ_END">
!<Tip> High frequency, in Hz, for time shift and phase shift calculation. </Tip>
! Default = 50.0
! Allowed = real > FREQ_BEG
! MTFUN calculates the effective time shift and phase shift associated with each
! transfer function by assuming the phase spectrum to be linear.  FREQ_BEG and
! FREQ_END are used for this calculation only; no frequency filtering is done.
!</Help>
!
!<Help KEYWORD="PATHNAME_DIAG_INFO" TYPE= "DISPLAY_ONLY">
!<Tip> Status of PATHNAME_DIAG. </Tip>
!</Help>
!
!<Help KEYWORD="SELECT_PATHNAME_DIAG">
!<Tip> Choose PATHNAME_DIAG using a file selection dialog box. </Tip>
!</Help>
!
!<Help KEYWORD="PATHNAME_DIAG">
!<Tip> Pathname for the text file containing diagnostic information. </Tip>
! Default = NONE
! Allowed = char
! MTFUN will optionally write the information carried in the transfer function
! trace headers into a diagnostic file whose pathname is PATHNAME_DIAG.
!
! If PATHNAME_DIAG = NONE, then no diagnostic file will be written.
!</Help>
!
!</HelpSection>
!
!------------------------------------------------------------------------------

!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!
!!---------------------------- start of module -----------------------------!!


module mtfun_module
  !
  ! - Module references
  !
  use fft_module
  use fltr_module
  use lav_module
  use mem_module     
  use mth_module
  use named_constants_module
  use opfilt_module
  use pathcheck_module
  use pathchoose_module
  use pc_module
  use getlun_module, only: getlun
  !
  implicit none
  !
  private
  public :: mtfun_create
  public :: mtfun_initialize
  public :: mtfun_update
  public :: mtfun_delete

!<execute_only>
  public :: mtfun            ! main execution (trace processing) routine.
  public :: mtfun_wrapup
!</execute_only>


  character(len=100),public,save :: MTFUN_IDENT = &
    '$Id: mtfun.f90,v 1.20 2006/06/20 13:12:00 Menger prod sps $'


  !!------------------------ parameter structure ---------------------------!!
  !!------------------------ parameter structure ---------------------------!!
  !!------------------------ parameter structure ---------------------------!!

  type trace_type
    double precision, pointer :: hd (:)
    real,             pointer :: tr (:)
    integer                   :: count
    double precision          :: id
  end type trace_type
  !
  integer, parameter, public :: job_name_len   = 15
  !
  type, public :: mtfun_struct
    !
    private
    !
    ! - Process parameters
    !      Available from the parameter cache
    !
    character (len = FILENAME_LENGTH) :: pathname_diag      ! pc - char
    character (len = job_name_len) :: job_name
    integer                        :: hdr_type           ! pc - 1 - NWIH
    integer                        :: hdr_x              ! pc - 0 - NWIH
    integer                        :: hdr_y              ! pc - 0 - NWIH
    integer                        :: num_sets           ! pc - int > 0
    real                           :: diag_load          ! pc - real > 0.0
    real                           :: freq_beg           ! pc - real >= 0.0
    real                           :: freq_end           ! pc - real > FREQ_BEG
    real                           :: len_op             ! pc - real > 0.0
    real                           :: tim_beg            ! pc - real
    real                           :: tim_end            ! pc - real > TIM_BEG
    integer                        :: val_stand          ! pc - real
    real                           :: win_min            ! pc - real > 0.0
    double precision               :: x_inc              ! pc - real
    double precision               :: x_init             ! pc - real
    double precision               :: y_inc              ! pc - real
    double precision               :: y_init             ! pc - real
    !
    integer                        :: blim_npow2
    real                           :: blim_ff
    integer                        :: len_tr_function
    integer                        :: len_half_tr_function
    integer                        :: dfile_lun
    integer                        :: num_sets_used
    integer                        :: next_function
    integer                        :: last_function
    logical                        :: end_state          ! Signal processing end
    integer                        :: err_count_no_std_trace
    integer                        :: err_count_insuff_data
    integer                        :: err_count_no_apply_traces
    integer                        :: nnyq
    integer                        :: nstart
    integer                        :: nstop
    integer                        :: prev_x_bin_num
    integer                        :: prev_y_bin_num
    integer                        :: win_min_samps
    integer                        :: bot_win_samp
    integer                        :: top_win_samp
    !
    logical                        :: skip_wrapup 
    integer                        :: ndpt               ! Common globals, ndpt
    integer                        :: nwih               ! Common globals, nwih
    real                           :: dt                 ! Common globals, dt
    real                           :: tstrt              ! Common globals, tstrt
    !
    ! - Common mtfunp1
    !
    integer                        :: num_std_trc     ! Common mtfunp1, nstd
    !
    ! - Non-common values
    !
    logical                          :: first_time_flag  ! Was first
    integer                          :: function_seq
    !
    type (trace_type), pointer       :: function (:)     ! Was hdfun
    type (trace_type), pointer       :: apply (:)        ! was hdapl
    type (trace_type)                :: standard         ! Was hdstd

    type (fft_struct), pointer       :: fun2_fwd_fft
    type (fft_struct), pointer       :: blim_rtoc_fft
    type (fft_struct), pointer       :: blim_ctor_fft
    type(pathchoose_struct),pointer :: pathchoose  ! dependent parameter.

  end type mtfun_struct

  !!----------------------------- interfaces -------------------------------!!
  !!----------------------------- interfaces -------------------------------!!
  !!----------------------------- interfaces -------------------------------!!


  !!-------------------------------- data ----------------------------------!!
  !!-------------------------------- data ----------------------------------!!
  !!-------------------------------- data ----------------------------------!!


  type (trace_type), save :: empty_trace

  real, parameter :: twopi = 2.0 * pi

contains


  !!------------------------------- create ---------------------------------!!
  !!------------------------------- create ---------------------------------!!
  !!------------------------------- create ---------------------------------!!


  subroutine mtfun_create (obj)
    !
    ! - Arguments
    !
    type(mtfun_struct),pointer :: obj       ! arguments
    !
    ! - Begin mtfun_create
    !
    allocate (obj)
    !
    obj%num_sets = 0
    !
    nullify  (obj%apply )
    nullify  (obj%function )
    nullify  (obj%standard%hd)
    nullify  (obj%standard%tr)
    nullify  (obj%fun2_fwd_fft)
    nullify  (obj%blim_rtoc_fft)
    nullify  (obj%blim_ctor_fft)
    nullify  (obj%pathchoose)

    call pathchoose_create(obj%pathchoose, 'pathname_diag', ' ')

    call mtfun_initialize (obj)
    !
  end subroutine mtfun_create


  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!

  subroutine mtfun_delete (obj)
    !
    ! - Arguments
    !
    type (mtfun_struct), pointer :: obj
    !
    ! - Local variables
    !
    integer :: s
    !
    ! - Begin mtfun_delete
    !

!<execute_only>
      call mtfun_wrapup (obj)
!</execute_only>

    !
    if (associated (obj%apply)) then
      do s = 1, size (obj%apply)
        call mem_free (obj%apply (s)%hd)
        call mem_free (obj%apply (s)%tr)
      end do
      deallocate (obj%apply)
    end if
    !
    if (associated (obj%function)) then
      do s = 1, size (obj%function)
        call mem_free (obj%function (s)%hd)
        call mem_free (obj%function (s)%tr)
      end do
      deallocate (obj%function)
    end if
    !
    call mem_free (obj%standard%hd)
    call mem_free (obj%standard%tr)
    !
    if (associated(obj%fun2_fwd_fft)) call fft_delete (obj%fun2_fwd_fft)
    if (associated(obj%blim_rtoc_fft)) call fft_delete (obj%blim_rtoc_fft)
    if (associated(obj%blim_ctor_fft)) call fft_delete (obj%blim_ctor_fft)
    if (associated(obj%pathchoose))   call pathchoose_delete(obj%pathchoose)
    !
    deallocate(obj)
    !
  end subroutine mtfun_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


  subroutine mtfun_initialize (obj)
    !
    ! - Arguments
    !
    type (mtfun_struct), intent (inout) :: obj       ! arguments
    !
    ! - Begin mtfun_initialize
    !
    nullify (empty_trace%hd)
    nullify (empty_trace%tr)
    empty_trace%count = 0
    empty_trace%id    = 0.0d0
    !
    call pc_get_global ('TSTRT', obj%tim_beg)
    call pc_get_global ('NDPT',  obj%ndpt)
    call pc_get_global ('DT',    obj%dt)
    !
    obj%tim_end = obj%tim_beg + (obj%ndpt - 1) * obj%dt

    obj%diag_load                 = 2.0
    obj%freq_beg                  = 8.0
    obj%freq_end                  = 50.0
    obj%hdr_type                  = 48
    obj%hdr_x                     = 8
    obj%hdr_y                     = 0
    obj%len_op                    = 0.4
    obj%num_sets                  = 1
    obj%val_stand                 = 1.0
    obj%win_min                   = 0.2
    obj%x_inc                     = 1.0
    obj%x_init                    = 1.0
    obj%y_inc                     = 1.0
    obj%y_init                    = 1.0
    obj%pathname_diag             = PATHCHECK_EMPTY
    obj%standard                  = empty_trace
    obj%num_sets_used             = 0
    obj%next_function             = 0
    obj%last_function             = -1
    obj%end_state                 = .false.
    obj%dfile_lun                 = 0
    obj%err_count_insuff_data     = 0
    obj%err_count_no_apply_traces = 0
    obj%err_count_no_std_trace    = 0
    obj%job_name                  = ' '
    obj%nnyq                      = 1
    obj%nstart                    = 1
    obj%nstop                     = 1
    obj%function_seq              = 0
    obj%first_time_flag           = .false.
    obj%skip_wrapup               = .true.
    !
    obj%len_tr_function           = 0
    obj%len_half_tr_function      = 0
    obj%prev_x_bin_num            = 0
    obj%prev_y_bin_num            = 0
    obj%win_min_samps             = 0
    !
    call mtfun_update (obj)
    !
  end subroutine mtfun_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


  subroutine mtfun_update (obj)
    !
    ! - Arguments
    !
    type (mtfun_struct), intent (inout), target :: obj  ! arguments
    !
    ! - Locla variables
    !

    integer :: i
    integer :: i_err
    integer :: nscratch
    integer :: nstore
    integer :: numtr
    integer :: path_status

    !
    ! - Begin mtfun_update
    !
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.

    if (pathchoose_update(obj%pathchoose, obj%pathname_diag)) return

    !!------------------------ read parameters -----------------------------!!
    !!------------------------ read parameters -----------------------------!!
    !!------------------------ read parameters -----------------------------!!


    call pc_get ('HDR_TYPE',      obj%hdr_type)
    call pc_get ('VAL_STAND',     obj%val_stand)
    call pc_get ('NUM_SETS',      obj%num_sets)
    call pc_get ('HDR_X',         obj%hdr_x)
    call pc_get ('X_INIT',        obj%x_init)
    call pc_get ('X_INC',         obj%x_inc)
    call pc_get ('HDR_Y',         obj%hdr_y)
    call pc_get ('Y_INIT',        obj%y_init)
    call pc_get ('Y_INC',         obj%y_inc)
    call pc_get ('LEN_OP',        obj%len_op)
    call pc_get ('DIAG_LOAD',     obj%diag_load)
    call pc_get ('TIM_BEG',       obj%tim_beg)
    call pc_get ('TIM_END',       obj%tim_end)
    call pc_get ('WIN_MIN',       obj%win_min)
    call pc_get ('FREQ_BEG',      obj%freq_beg)
    call pc_get ('FREQ_END',      obj%freq_end)
    call pc_get ('PATHNAME_DIAG', obj%pathname_diag)

    call pc_get_jdata (keyword = 'JOBNAME',    &
                       scalar  = obj%job_name)

    call pc_get_global ('numtr',   numtr)     ! maximum number of traces.
    call pc_get_global ('nwih',    obj%nwih)  ! number of header words.
    call pc_get_global ('ndpt',    obj%ndpt)  ! number of trace samples.
    call pc_get_global ('dt',      obj%dt)    ! trace sample interval (sec).
    call pc_get_global ('tstrt',   obj%tstrt) !time of 1st trace sample(sec).


    !!----------------------- verify parameters ----------------------------!!
    !!----------------------- verify parameters ----------------------------!!
    !!----------------------- verify parameters ----------------------------!!

    if (numtr > 1) then
      call pc_error ('MTFUN: Traces must be input one at a time.  '    &
                     // 'NUMTR is ', numtr, ', not one (1).')
    end if
    !
    if (obj%dt == 0.0) then
      call pc_error ("MTFUN: Input trace sample spacing DT "    &
                     // "is set to zero (0.0)")
    end if
    !
    ! - Check HDR_TYPE
    !
  check_hdr_type:   &
    if ((obj%hdr_type < 1) .or. (obj%hdr_type > obj%nwih)) then
      !
      call pc_error (msg1 = 'MTFUN:  Bad header word number for HDR_TYPE (',  &
                     var1 = obj%hdr_type,                                   &
                     msg2 = ').  Setting to default 48')
      !
      obj%hdr_type = 48
      !
    end if check_hdr_type

    !
    ! - Check VAL_STAND
    !
    ! --- VAL_STAND may be any real value, and is not checked.

    !
    ! - Check NUM_SETS
    !
  check_num_sets:   &
    if (obj%num_sets <= 0) then   ! int > 0
      !
      call pc_error (msg1 = 'MTFUN:  Value for NUM_SETS (',     &
                     var1 = obj%num_sets,                     &
                     msg2 = ') must be creater than zero.  '    &
                            // 'Setting to default 1.')
      obj%num_sets = 1
      !
    end if check_num_sets

    !
    ! - Check HDR_X
    !
  check_hdr_x:   &
    if ((obj%hdr_x < 0) .or. (obj%hdr_x > obj%nwih)) then   ! 0 - NWIH
      !
      call pc_error (msg1 = 'MTFUN:  Bad value for HDR_X (',   &
                     var1 = obj%hdr_x,                       &
                     msg2 = ').  Setting to "unused" 0.')
      obj%hdr_x = 0
      !
    end if check_hdr_x

    !
    ! - Check X_INIT
    !
    ! --- X_INC may be any real value, and is not checked.

    !
    ! - Check X_INIT
    !
    ! --- X_INC may be any real value, and is not checked.

    !
    ! - Check HDR_Y
    !
  check_hdr_y:   &
    if ((obj%hdr_y < 0) .or. (obj%hdr_y > obj%nwih)) then   ! 0 - NWIH
      !
      call pc_error (msg1 = 'MTFUN:  Bad value for HDR_Y (',   &
                     var1 = obj%hdr_y,                       &
                     msg2 = ').  Setting to "unused" 0.')
      obj%hdr_y = 0
      !
    else if ((obj%hdr_x == 0) .and. (obj%hdr_y == 0)) then check_hdr_y
      !
      call pc_error (msg1 = 'MTFUN:  HDR_X and HDR_Y cannot both be zero (0).')
      call pc_error (msg1 = 'Setting HDR_X to default 8.')
      obj%hdr_x = 8
      !
    end if check_hdr_y
    !
    ! - Check Y_INIT
    !
  check_y_init:   &
    if (obj%y_init < 0.0) then   ! real
      !
      call pc_error (msg1 = 'MTFUN:  Bad value for Y_INIT (',   &
                     var1 = obj%y_init,                       &
                     msg2 = ').  Setting to default 1.0.')
      obj%y_init = 1.0
      !
    end if check_y_init

    !
    ! - Check Y_INC
    !
    ! --  Y_INC is not checked, all real values

    !
    ! - Check LEN_OP
    !
  check_len_op:   &
    if (obj%len_op < 0.0) then   ! real > 0.0
      !
      call pc_error (msg1 = 'MTFUN:  Bad value for LEN_OP (',   &
                     var1 = obj%len_op,                       &
                     msg2 = ')')
      !
    else
      !
      ! - Align len_op with an integral number of samples
      !
      obj%len_half_tr_function     &
                   = max (a1 = nint (0.5 * obj%len_op / obj%dt),  &
                          a2 = 1)
      obj%len_op = real (2 * obj%len_half_tr_function) * obj%dt

      !
    end if check_len_op

    !
    ! - Check DIAG_LOAD
    !
  check_diag_load:   &
    if (obj%diag_load <= 0.0) then   ! real > 0.0
      !
      call pc_error (msg1 = 'MTFUN:  Bad value for DIAG_LOAD (',   &
                     var1 = obj%diag_load,   &
                     msg2 = ').  Setting to default 2.0.')
      obj%diag_load = 2.0
      !
    end if check_diag_load

    !
    ! - Check TIM_BEG
    !
  check_tim_beg:   &
    if(obj%tim_beg < obj%tstrt .or. &
      obj%tim_beg >= obj%tstrt + (obj%ndpt - 1)*obj%dt) then   ! real
      !
      call pc_error (msg1 = 'MTFUN:  Bad value for TIM_BEG (',   &
                     var1 = obj%tim_beg,   &
                     msg2 = ').  Setting to default TSTRT.')
      obj%tim_beg = obj%tstrt
      !
    end if check_tim_beg

    !
    ! - Check TIM_END
    !
  check_tim_end:   &
    if (obj%tim_end <= obj%tim_beg .or. & ! real > TIM_BEG
      obj%tim_end > obj%tstrt + (obj%ndpt - 1)*obj%dt) then
      !
      call pc_error (msg1 = 'MTFUN:  Bad value for TIM_END (',   &
                     var1 = obj%tim_end,   &
                     msg2 = ').  Setting to default end-of-trace.')
      obj%tim_end = (obj%ndpt - 1) * obj%dt + obj%tstrt
      !
    end if check_tim_end

    !
    ! - Check WIN_MIN
    !
  check_win_min:   &
    if (obj%win_min <= 0.0) then   ! real > 0.0
      !
      call pc_error (msg1 = 'MTFUN:  Bad value for WIN_MIN (',   &
                     var1 = obj%win_min,   &
                     msg2 = ').  Default is 0.2 sec; setting to 10% of trace.')
      obj%win_min = obj%ndpt * obj%dt / 10
      !
    end if check_win_min

    !
    ! - Check FREQ_BEG
    !
  check_freq_beg:   &
    if (obj%freq_beg < 0.0) then   ! real >= 0.0
      !
      call pc_error (msg1 = 'MTFUN: Bad value for FREQ_BEG (',   &
                     var1 = obj%freq_beg,   &
                     msg2 = ').  Must be >= 0.0.  Setting to default 8.0.')
      obj%freq_beg = 8.0
      !
    end if check_freq_beg

    !
    ! - Check FREQ_END
    !
  check_freq_end:   &
    if (obj%freq_end <= obj%freq_beg) then   ! real > FREQ_BEG
      !
      call pc_error (msg1 = 'MTFUN:  Bad value for FREQ_END (',   &
                     var1 = obj%freq_end,                       &
                     msg2 = ').  Must be > FREQ_BEG (',           &
                     var2 = obj%freq_beg,                       &
                     msg3 = ').  Setting to default 50.0.')
      obj%freq_end = 50.0
      !
    end if check_freq_end

    !
    ! - Check PATHNAME_DIAG
    !
    call pathcheck (keyword  = 'PATHNAME_DIAG',        &
                    pathname = obj%pathname_diag,    &
                    show     = PATHCHECK_INFO_OUTPUT,  &
                    status   = path_status)
    !

! 16 Nov 2001, Selzler: I think pathcheck takes care of this now.
! check_pathname_diag:   &
!   if (path_status == PATH_INVALID) then
!     !
!     call pc_error (msg1 = 'MTFUN:  Invalid pathname for PATHNAME_DIAG ('   &
!                           // trim (obj%pathname_diag) // ')')
!     !
!   else if (path_status == PATH_INCOMPLETE) then
!     !
!     call pc_error (msg1 = 'MTFUN:  Incomplete pathname for PATHNAME_DIAG (' &
!                           // trim (obj%pathname_diag) // ').')
!     call pc_error (msg1 = '        Path ends with a directory not a file.')
!     !
!   end if check_pathname_diag


    !!------------------- call processes internally ------------------------!!
    !!------------------- call processes internally ------------------------!!
    !!------------------- call processes internally ------------------------!!


    !!------------------------ write parameters ----------------------------!!
    !!------------------------ write parameters ----------------------------!!
    !!------------------------ write parameters ----------------------------!!

    nscratch = (2 * obj%nwih + obj%ndpt) * 2
    nstore   = (2 * obj%nwih + obj%ndpt) * (obj%num_sets * 3)
    !
    call pc_put_control ('ntapes',       0)             ! default 0
    call pc_put_control ('need_request', .true.)        ! default false
    call pc_put_control ('need_label',   .true.)        ! default false
    call pc_put_control ('twosets',      .true.)        ! default false
    call pc_put_control ('nscratch',     nscratch)      ! default 0
    call pc_put_control ('nstore',       nstore)        ! default 0
    call pc_put_control ('iftd',         .false.)       ! default false
    call pc_put_control ('ndisk',        0)             ! default 0
    call pc_put_control ('setup_only',   .false.)       ! default .false.
    !
    obj%len_half_tr_function                            &
      = max (a1 = nint (0.5 * obj%len_op / obj%dt),   &
             a2 = 1)
    !
    obj%len_tr_function = 2 * obj%len_half_tr_function + 1
    obj%top_win_samp                                               &
      = nint (max (a1 = (obj%tim_beg - obj%tstrt)/obj%dt + 1., &
                   a2 = 1.0))
    obj%bot_win_samp    &
      = nint (min (a1 = (obj%tim_end - obj%tstrt)/obj%dt + 1., &
               a2 = real (obj%ndpt)))
    if (obj%bot_win_samp - obj%top_win_samp + 1     &
        < obj%len_tr_function) then
      call pc_error ('=>MTFUNS:  Window length less than LEN_OP')
      return
    end if

    call pc_put_global ('numtr', 1)
    call pc_put_global ('ndpt',  obj%len_tr_function)
    call pc_put_global ('tstrt', -1.0 * real (obj%len_half_tr_function)    &
                                 * obj%dt)
    call pc_put_global ('nwih',  obj%nwih)
    call pc_put_global ('dt',    obj%dt)

    call pc_put ('HDR_TYPE',      obj%hdr_type)
    call pc_put ('VAL_STAND',     obj%val_stand)
    call pc_put ('NUM_SETS',      obj%num_sets)
    call pc_put ('HDR_X',         obj%hdr_x)
    call pc_put ('X_INIT',        obj%x_init)
    call pc_put ('X_INC',         obj%x_inc)
    call pc_put ('HDR_Y',         obj%hdr_y)
    call pc_put ('Y_INIT',        obj%y_init)
    call pc_put ('Y_INC',         obj%y_inc)
    call pc_put ('LEN_OP',        obj%len_op)
    call pc_put ('DIAG_LOAD',     obj%diag_load)
    call pc_put ('TIM_BEG',       obj%tim_beg)
    call pc_put ('TIM_END',       obj%tim_end)
    call pc_put ('WIN_MIN',       obj%win_min)
    call pc_put ('FREQ_BEG',      obj%freq_beg)
    call pc_put ('FREQ_END',      obj%freq_end)
    call pc_put ('PATHNAME_DIAG', obj%pathname_diag)


    !!--------------------- prepare for execution --------------------------!!
    !!--------------------- prepare for execution --------------------------!!
    !!--------------------- prepare for execution --------------------------!!

    if (pc_do_not_process_traces()) return
    !
    obj%skip_wrapup = .false.
    ! 
    ! - Release any already-allocated memory
    !
    if (associated (obj%apply)) then
      do i = 1, size (obj%apply)
        call mem_free (obj%apply (i)%hd)
        call mem_free (obj%apply (i)%tr)
      end do
      deallocate (obj%apply)
    end if
    !
    if (associated (obj%function)) then
      do i = 1, size (obj%function)
        call mem_free (obj%function (i)%hd)
        call mem_free (obj%function (i)%tr)
      end do
      deallocate (obj%function)
    end if
    !
    call mem_free (obj%standard%hd)
    call mem_free (obj%standard%tr)
    !
    obj%err_count_insuff_data = 0
    obj%err_count_no_apply_traces = 0
    obj%err_count_no_std_trace = 0
    obj%first_time_flag = .true.
    obj%function_seq = 0
    obj%prev_x_bin_num = 0
    obj%prev_y_bin_num = 0

!<execute_only>

    if (pc_do_not_process_traces()) return      ! Additional check for errors
    !
    allocate (obj%apply    (obj%num_sets))
    allocate (obj%function (obj%num_sets))
    !
    obj%apply    = empty_trace
    obj%function = empty_trace
    !
    do i = 1, obj%num_sets
      call mem_alloc (obj%apply (i)%hd, obj%nwih)
      call mem_alloc (obj%apply (i)%tr, obj%ndpt)
      obj%apply (i)%hd    = 0.0d0
      obj%apply (i)%tr    = 0.0
      obj%apply (i)%id    = 0.0d0
      obj%apply (i)%count = 0
      !
      call mem_alloc (obj%function (i)%hd, obj%nwih)
      call mem_alloc (obj%function (i)%tr, obj%len_tr_function)
      obj%function (i)%hd    = 0.0d0
      obj%function (i)%tr    = 0.0
      obj%function (i)%id    = 0.0d0
      obj%function (i)%count = 0
    end do
    !
    call mem_alloc (obj%standard%hd, obj%nwih)
    call mem_alloc (obj%standard%tr, obj%ndpt)
    obj%standard%hd    = 0.0d0
    obj%standard%tr    = 0.0
    obj%standard%id    = 0.0d0
    obj%standard%count = 0
    !
    if (pc_do_not_process_traces()) return   ! in case of allocation errors.
    !
    obj%diag_load     = 1.0 + obj%diag_load / 100.
    obj%win_min_samps = max (a1 = nint (obj%win_min / obj%dt) + 1,    &
                               a2 = 1)
    !
    obj%blim_npow2 = 2 * 4
    do while (obj%blim_npow2 < obj%ndpt)
      obj%blim_npow2 = 2*obj%blim_npow2
    end do
    !
    obj%blim_ff = 1.0 / (real (obj%blim_npow2) * obj%dt)
    !
    obj%standard%hd ( 2) = 1
    obj%standard%hd (64) = real (obj%ndpt)
    obj%num_std_trc = 0
    obj%num_sets_used = 0
    !
    do i = 1, obj%num_sets
      obj%apply (i)%hd ( :) = 0.0d0
      obj%apply (i)%hd ( 2) = 1.0d0
      obj%apply (i)%hd (64) = real (obj%ndpt)
      obj%apply (i)%tr ( :) = 0.0
      !
    end do
    !
    if (trim (obj%pathname_diag) == 'NONE') then
      !
      obj%dfile_lun = 0
      !
    else
      !
      call getlun (lun = obj%dfile_lun)
      !
      open (unit   = obj%dfile_lun,               &
            file   = trim (obj%pathname_diag),    &
            form   = 'FORMATTED',                   &
            status = 'UNKNOWN')
      !
    end if
    !
    i_err = fft_create (obj   = obj%fun2_fwd_fft,    &
                        sign  = -1,                    &
                        size  = obj%blim_npow2,      &
                        ctype = 'rtoc')
    !
    i_err = fft_create (obj   = obj%blim_rtoc_fft,    &
                        sign  = 1,                      &
                        size  = obj%blim_npow2,       &
                        ctype = 'rtoc')
    !
    i_err = fft_create (obj       = obj%blim_ctor_fft,    &
                        sign      = 1,                      &
                        size      = obj%blim_npow2,       &
                        ctype     = 'ctor',                 &
                        opt_scale = 0.5)
    !
    obj%nnyq   = obj%blim_npow2 / 2 + 1
    obj%nstart = max (a1 = nint (obj%freq_beg          &
                                   * obj%dt              &
                                   * obj%blim_npow2),    &
                        a2 = 1)
    obj%nstop = min (a1 = nint (obj%freq_end          &
                                  * obj%dt              &
                                  * obj%blim_npow2),    &
                       a2 = obj%nnyq)


!</execute_only>


    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!
    !!------------------------- finish update ------------------------------!!

    if (obj%num_sets <= 0) obj%num_sets = 1
    obj%last_function = obj%num_sets
    obj%next_function = obj%num_sets + 1
    obj%end_state     = .false.
    !
  end subroutine mtfun_update


  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!
  !!------------------------------- traps ----------------------------------!!



  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!
  !!--------------------------- main execution -----------------------------!!


!<execute_only>

  subroutine mtfun (obj,ntr,hdi,tri,hdo,tro)
    !
    ! - Arguments
    !
    type (mtfun_struct), intent (inout) :: obj
    integer,             intent (inout) :: ntr
    double precision,    intent (in)    :: hdi (:,:)
    real,                intent (in)    :: tri (:,:)
    double precision,    intent (inout) :: hdo (:,:)
    real,                intent (inout) :: tro (:,:)
    !
    ! - Local variables
    !
    character (len = 130) :: card
    character (len =   8) :: my_date
    character (len =  10) :: my_time
    !



    integer :: ihd2
    integer :: ihd64
    integer :: j

    integer :: k
    integer :: kk
    integer :: s
    integer :: set
    integer :: tr_type
    integer :: window_len
    integer :: x_bin_num
    integer :: y_bin_num
    !

    !
    ! - Begin mtfun
    !
  select_ntr_mode:    &
    if (ntr <= 0) then
      !
    check_for_end_state:    &
      if ((ntr == NO_MORE_TRACES) .and. (.not. obj%end_state)) then
        !
        ! - Wrap up the function
        !
        if (obj%num_sets_used > 0) then 
          !
          call mtfun_derive_function (obj = obj)
          !
        end if
        !
        obj%end_state = .true.
        !
        write (card, 995) ' '
        call pc_print (card)
        !
        if (obj%err_count_no_std_trace > 0) then
          write (card, 996) 'No "standard" trace occurences       - ',     &
                           obj%err_count_no_std_trace
          call pc_print (card)
        end if
        !
        if (obj%err_count_insuff_data > 0) then
          write (card, 996) 'Not enough data in window occurences - ',     &
                            obj%err_count_insuff_data
          call pc_print (card)
        end if
        !
        if (obj%err_count_no_apply_traces > 0) then
          write (card, 996) 'No "apply" trace occurences          - ',     &
                            obj%err_count_no_apply_traces
          call pc_print (card)
        end if
        !
        write (card, 995) ' '
        call pc_print (card)
        !
      end if check_for_end_state
      !
    end if select_ntr_mode
    !
    ! - Process an input trace
    !
  loop_thru_traces:    &
    do k = 1, ntr
      !
      if (obj%hdr_x > 0) then
        !
        x_bin_num=mth_bin_number(obj%x_init,obj%x_inc,hdi (obj%hdr_x, k))
!rev18        x_bin_num = nint ((hdi (obj%hdr_x, k)    &
!rev18                           - obj%x_init)         &
!rev18                           / obj%x_inc)
      else
        x_bin_num = 0
      end if
      !
      if (obj%hdr_y > 0) then
        y_bin_num=mth_bin_number(obj%y_init,obj%y_inc,hdi(obj%hdr_y, k))
!rev18        y_bin_num = nint ((hdi (obj%hdr_y, k)    &
!rev18                           - obj%y_init)         &
!rev18                          / obj%y_inc)
      else
        y_bin_num = 0
      end if
      !
      ! - first time
      !
      if (obj%first_time_flag) then
        !
        obj%first_time_flag = .false.
        !
        if (obj%dfile_lun > 0) then
          call date_and_time (date = my_date,    &
                              time = my_time)

          write (obj%dfile_lun, 994)     &
            obj%job_name,    &
            my_date (1:4), my_date (5:6), my_date (7:8),    &
            my_time (1:2), my_time (3:4), my_time (5:6),    &
            obj%pathname_diag
          write (obj%dfile_lun, 995) &
          '                                                        '    &
          // '                                           before      after   '
          write (obj%dfile_lun, 998)
          write (obj%dfile_lun, 997)
        end if
        !
        obj%prev_x_bin_num  = x_bin_num
        obj%prev_y_bin_num  = y_bin_num
      end if
      !
      ! - New bin
      !
    check_for_prev_bin_end:                     &
      if (x_bin_num == obj%prev_x_bin_num     &
                 .and. y_bin_num == obj%prev_y_bin_num) then
        !
        ntr = NEED_TRACES    ! Get the rest of the bin
        !
      else if (obj%num_sets_used > 0) then check_for_prev_bin_end
        !
        call mtfun_derive_function (obj = obj)
        !
        ! - Clear standard and apply sets for this new bin
        !
        obj%prev_x_bin_num   = x_bin_num
        obj%prev_y_bin_num   = y_bin_num
        obj%num_std_trc      = 0

        obj%standard%hd    = 0.0d0
        obj%standard%tr    = 0.0
        obj%standard%count = 0
        !
        do s = 1, obj%num_sets
          obj%apply (s)%hd    = 0.0d0
          obj%apply (s)%tr    = 0.0
          obj%apply (s)%id    = 0.0d0
          obj%apply (s)%count = 0
        end do
        !
        obj%num_sets_used = 0
        !
      end if check_for_prev_bin_end
      !
      tr_type = nint (hdi (obj%hdr_type, k))
      !
    validate_standard_trace:    &
      if (tr_type == obj%val_stand) then
        !
        ihd2       = max (a1 = obj%top_win_samp,    &
                          a2 = int (hdi ( 2, k)))
        ihd64      = min (a1 = obj%bot_win_samp,    &
                          a2 = int (hdi (64, k)))
        window_len = max (0, ihd64 - ihd2 + 1)
        !
        if (window_len > 0) then
          !
          obj%standard%hd  (1) = hdi ( 1, k)
          obj%standard%hd  (2) = max (a1 = obj%standard%hd ( 2),    &
                                          a2 = hdi ( 2, k))
          if (obj%standard%hd (64) > 0.0d0) then
            obj%standard%hd (64) = min (a1 = obj%standard%hd (64),    &
                                            a2 = hdi (64, k))
          else
            obj%standard%hd (64) = hdi (64, k)
          end if
          !
          obj%standard%hd (3:63) = obj%standard%hd (3:  63)     &
                                       + hdi (3:  63, k)
          obj%standard%tr ( :obj%ndpt)     &
            = obj%standard%tr ( :obj%ndpt) + tri ( :obj%ndpt, k)
          obj%num_std_trc           = obj%num_std_trc + 1
          obj%standard%count      = obj%standard%count + 1
          !
        endif
        !
      else validate_standard_trace
        !
        set = 0
        !
        ! - Check against existing 'apply' sets
        !
        do kk = 1, obj%num_sets_used
          !
          if (tr_type == obj%apply (kk)%id) then
            set = kk
          end if
          !
        end do
        !
        if (set == 0) then
          !
          if (obj%num_sets_used < obj%num_sets) then
            !
            obj%num_sets_used = obj%num_sets_used + 1
            set                 = obj%num_sets_used
            !
            obj%apply (set)%hd (3:63) = 0.0d0
            obj%apply (set)%tr        = 0.0
            obj%apply (set)%count     = 0
            obj%apply (set)%id        = hdi (obj%hdr_type, k)
            !
          else
            !
            call pc_error ('MTFUN: Active set ID ', j, ' does not match '     &
                           // ' any active set, and all ',  obj%num_sets,   &
                           ' sets are in use.')
            call pc_error ('MTFUN  Prev X is ', obj%prev_x_bin_num,     &
                            ', prev Y is ', obj%prev_y_bin_num)
            call pc_error ('MTFUN  X is ', x_bin_num,    &
                           ', Y is ', y_bin_num)
            !
            cycle loop_thru_traces
            !
          end if
          !
        end if
        !
        if (set > 0) then
          !
          ihd2  = max (a1 = obj%top_win_samp,    &
                       a2 = nint (hdi (HDR_TOP_MUTE, k)))
          if (obj%bot_win_samp > 0) then
            ihd64 = min (a1 = obj%bot_win_samp,    &
                         a2 = nint (hdi (HDR_BOTTOM_MUTE, k)))
          else
            ihd64 = nint (hdi (HDR_BOTTOM_MUTE, k))
          end if
          !
          window_len = max (a1 = 0,                   &
                            a2 = ihd64 - ihd2 + 1)
          !
          ! - Verify there are some un-muted samples
          !
          if (window_len > 0) then
            !
            obj%apply (set)%hd (1) = hdi ( 1, k)
            obj%apply (set)%hd (2)                    &
              = max (a1 = obj%apply (set)%hd ( 2),    &
                     a2 = hdi ( 2, k))
            !
            if (obj%apply (set)%hd (64) > 0.0d0) then
              obj%apply (set)%hd (64)                   &
                = min (a1 = obj%apply (set)%hd (64),    &
                       a2 = hdi (64, k))
            else
              obj%apply (set)%hd (64) = hdi (64, k)
            end if
            !
            obj%apply (set)%hd (3 : 63)        &
              = obj%apply (set)%hd (3 : 63)    &
                + hdi (3:  63, k)
            !
            obj%apply (set)%tr (:)    &
              = obj%apply (set)%tr ( :)    &
                + tri ( :, k)
            !
            obj%apply (set)%count = obj%apply (set)%count + 1
            !
          end if
          !
        end if
        !
      end if validate_standard_trace
      !
    end do loop_thru_traces
    !
    if (obj%next_function <= obj%last_function) then
      !
      tro (obj%len_tr_function + 1:, 1) = 0.0
      tro (:obj%len_tr_function, 1)     &
          = obj%function (obj%next_function)%tr
      hdo (:obj%nwih, 1) = obj%function (obj%next_function)%hd
      obj%next_function  = obj%next_function + 1
      ntr                  = 1
      !
    else
      !
      if ( obj%end_state) then
        ntr = NO_MORE_TRACES
      else
        ntr = NEED_TRACES
      end if
      !
    end if
    !
    if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
      call mtfun_wrapup (obj = obj)
    end if
    !
  994 format (51 (' '), 'MTFUN Diagnostic file'/    &
              51 (' '), 'Job name: ', a15//,    &
              '   Run date: ', a4, '-', a2, '-', a2/,    &
              '   Run time: ', a2, ':', a2, ':', a2/,    &
              '   File: ', a)

  995 format (1x,a)
  996 format (' ', a, i6)
  997 format (61 (' -'))
  998 format (' --trace#-- ---xbin--- ---ybin--- -#std- ',    &
              '--apl--- -#apl- --ampmin-- --ampmax-- ',   &
              '--tshift-- --phase-- -corr coef- -corr coef-')
    !
  end subroutine mtfun



  !!----------------------- mtfun_derive_function --------------------------!!
  !!----------------------- mtfun_derive_function --------------------------!!
  !!----------------------- mtfun_derive_function --------------------------!!

  !
  ! - mtfun_derive_function
  !
  subroutine mtfun_derive_function (obj)
    !
    ! - Arguments
    !
    type (mtfun_struct), intent (inout) :: obj
    !
    ! - Local variables
    !
    character (len = 10) :: hdxchr
    character (len = 10) :: hdychr
    !
    integer :: bot_win_samp
    integer :: i
    integer :: i_bot
    integer :: i_top
    integer :: ixbin
    integer :: iybin
    integer :: j
    integer :: j1
    integer :: j2
    integer :: k
    integer :: nnyq
    integer :: nstart
    integer :: nstop
    integer :: top_win_samp
    integer :: window_len
    !
    complex :: complex_tr  (obj%len_half_tr_function + 1)
    double precision :: cc_after
    double precision :: cc_before
    double precision :: denom
    real :: a
    real :: ab
    real :: ampmnn
    real :: ampmxx
    real :: b
    real :: base_sum
    real :: corr_funct       (obj%len_tr_function)
    real :: cross_corr_funct (obj%len_tr_function)
    real :: fact
    real :: phase
    real :: shift
    real :: slope
    real :: sumtt
    real :: sumx
    real :: sumy
    real :: temp_tr (obj%blim_npow2)

    real :: tmps


    real :: tshift
    real :: work_mtfun2  (3 * obj%blim_npow2 + 4) ! (2 
                                                    !  * obj%len_tr_function)
    real :: ti1u   (obj%nstart : obj%nstop)
    real :: tram1u (obj%nstart : obj%nstop)
    real :: trsq1u (obj%nstart : obj%nstop)
    !
    ! - Begin mtfun_derive_function
    !
    ixbin = obj%prev_x_bin_num
    iybin = obj%prev_y_bin_num
    i_top = obj%top_win_samp
    i_bot = obj%bot_win_samp
    !
    if (obj%num_std_trc <= 0) then                    ! no "standard" trace
      obj%err_count_no_std_trace = obj%err_count_no_std_trace + 1
      return
    end if
    !
    fact = 1./ obj%num_std_trc
    obj%standard%hd (3:  63) = fact * obj%standard%hd (3:  63)
    obj%standard%tr ( :obj%ndpt)     &
      = fact * obj%standard%tr ( :obj%ndpt)
    !
    top_win_samp = max (a1 = obj%standard%hd ( 2),    &
                        a2 = dble (i_top))
    bot_win_samp = min (a1 = obj%standard%hd (64),    &
                        a2 = dble (i_bot))
    !
    window_len = max (a1 = 0,    &
                      a2 = bot_win_samp - top_win_samp + 1)
    !
    if (window_len < obj%win_min_samps) then
      !
      ! - Not enough data in standard trace window
      !
      obj%err_count_insuff_data = obj%err_count_insuff_data     &
                                    + obj%num_sets
      call pc_warning ("mtfun_derive_function: Not enough data in standard "  &
                       // "trace window.  Std winLen ", window_len,           &
                       "    WinMinSamps ", obj%win_min_samps)
      return
      !
    end if
    !
    obj%last_function = 0
    obj%next_function = 1
    nnyq   = obj%nnyq
    nstart = obj%nstart
    nstop  = obj%nstop
    !
    ! - Resolve each set
    !
  loop_thru_sets:    &
    do j = 1, obj%num_sets_used
      !
      ! - Limit the apply window length to the standard window length
      !
      top_win_samp = max (a1 = obj%standard%hd ( 2),     &
                          a2 = obj%apply (j)%hd ( 2),    &
                          a3 = dble (i_top))
      bot_win_samp = min (a1 = obj%standard%hd (64),     &
                          a2 = obj%apply (j)%hd (64),    &
                          a3 = dble (i_bot))
      window_len   = max (a1 = 0,    &
                          a2 = bot_win_samp - top_win_samp + 1)
      !
      if (window_len < obj%win_min_samps) then
        !
        ! - Not enough data in apply trace window
        !
        obj%err_count_insuff_data = obj%err_count_insuff_data + 1
        cycle loop_thru_sets
      end if
      !
      ! - Verify apply traces are present for this set
      !
      if (obj%apply (j)%count <= 0) then
        obj%err_count_no_apply_traces = obj%err_count_no_apply_traces + 1
        cycle loop_thru_sets
      end if
      !
      ! - Factor the apply trace for multiple traces in the set
      !
      fact = 1.0 / real (obj%apply (j)%count)
      !
      obj%apply (j)%hd (3:  63) = dble (fact)    &
                                      * obj%apply (j)%hd (3:  63)
      obj%apply (j)%tr = fact * obj%apply (j)%tr
      corr_funct = 0.0
      !
      ! - Compute autocorrelation obj%apply%tr into r
      !
      call fltr_filtrgs (filter      = obj%apply (j)%tr (top_win_samp:),  &
                         m           = window_len,                            &
                         data        = obj%apply (j)%tr (top_win_samp:),  &
                         n           = window_len,                            &
                         correlation = corr_funct,                            &
                         l           = obj%len_tr_function,                 &
                         iflag       = 1,                                     &
                         ishift      = 0)
      !
      if (corr_funct (1) == 0.) then                     ! no data in window
        obj%err_count_insuff_data = obj%err_count_insuff_data + 1
        cycle loop_thru_sets
      end if
      !
      corr_funct (1) = corr_funct (1) * obj%diag_load
      !
      ! - compute crosscorrelation obj%apply%tr * obj%standard%tr into x
      !
      call fltr_filtrgs (filter      = obj%apply (j)%tr (top_win_samp:),  &
                         m           = window_len,                            &
                         data        = obj%standard%tr (top_win_samp:),   &
                         n           = window_len,                            &
                         correlation = cross_corr_funct,                      &
                         l           = obj%len_tr_function,                 &
                         iflag       = 1,                                     &
                         ishift      = (- obj%len_half_tr_function))
      !
      ! - get transfer function into trfun
      !
      obj%last_function = obj%last_function + 1
      k                   = obj%last_function
      !
      call opfilt (N = obj%len_tr_function,      &
                   A = obj%function (k)%tr,    &
                   B = cross_corr_funct,           &
                   C = work_mtfun2,                &
                   R = corr_funct)
      !
      ! - bandpass filter "apply" trace
      !
      temp_tr (:obj%ndpt) = obj%apply (j)%tr
      !
      if (obj%blim_npow2 > obj%ndpt) then
        temp_tr (obj%ndpt + 1 : obj%blim_npow2) = 0.0
      end if
      !
      call mtfun_blim (a   = temp_tr,    &
                       obj = obj)
      !
      obj%apply (j)%tr (:obj%ndpt) = temp_tr (:obj%ndpt)
      !
      ! - bandpass filter "standard" trace
      !
      temp_tr(:obj%ndpt) = obj%standard%tr (:obj%ndpt)
      !
      if (obj%blim_npow2 > obj%ndpt) then
        temp_tr (obj%ndpt + 1 : obj%blim_npow2) = 0.0
      end if
      !
      call mtfun_blim (a   = temp_tr,    &
                       obj = obj)
      !
      ! - compute correlation coefficient for tramp and trtmp
      !
      ab = sum (obj%apply (j)%tr   (top_win_samp : bot_win_samp)    &
                * temp_tr (top_win_samp : bot_win_samp))
      !
      a = sum (obj%apply (j)%tr (top_win_samp : bot_win_samp) ** 2)
      b = sum (temp_tr (top_win_samp : bot_win_samp) ** 2)
      !
      denom = sqrt (dble (a) * dble (b))
      cc_before = 0.0
      !
      if (denom /= 0.0d0) then
        cc_before = dble (ab) / denom
      end if
      !
      ! - compute convolution obj%function%tr * obj%apply%tr into trtmp
      !
      work_mtfun2 (:obj%len_tr_function)     &
        = obj%function (k)%tr (obj%len_tr_function:1:(-1))
      !
      call fltr_filtrgs (filter      = work_mtfun2,              &
                         m           = obj%len_tr_function,    &
                         data        = obj%apply (j)%tr,     &
                         n           = obj%ndpt,               &
                         correlation = temp_tr,                  &
                         l           = obj%ndpt,               &
                         iflag       = 1,                        &
                         ishift      = (-obj%len_half_tr_function))
      !
      corr_funct (1) = corr_funct (1) / obj%diag_load
      !
      ! - bandpass filter "filtered apply" traces
      !
      if (obj%blim_npow2 > obj%ndpt) temp_tr (obj%ndpt+1:) = 0.0
      !
      call mtfun_blim (a   = temp_tr,    &
                       obj = obj)
      !
      obj%apply (j)%tr = temp_tr (:obj%ndpt)
      !
      ! - bandpass filter "standard" trace
      !
      temp_tr (:obj%ndpt) = obj%standard%tr (:obj%ndpt)
      if (obj%blim_npow2 > obj%ndpt) then
        temp_tr(obj%ndpt+1:) = 0.0
      end if
      !
      call mtfun_blim (a   = temp_tr,    &
                       obj = obj)
      !
      ! - Compute correlation coefficient for obj%apply%tr and trtmp
      !
      ab = sum (obj%apply (j)%tr (top_win_samp : bot_win_samp)    &
                * temp_tr            (top_win_samp : bot_win_samp))
      !
      a = sum (obj%apply (j)%tr (top_win_samp : bot_win_samp) ** 2)
      b = sum (temp_tr              (top_win_samp : bot_win_samp) ** 2)
      !
      denom = sqrt (dble (a) * dble (b))
      !
      if (denom == 0.0d0) then
        cc_after = 0.0
      else
        cc_after = dble (ab)/denom
      end if
      !
      ! - convert to amplitude and phase
      !
      temp_tr (:obj%blim_npow2) = 0.0
      !
      temp_tr (: obj%len_half_tr_function + 1)                    &
       = obj%function (k)%tr (obj%len_half_tr_function + 1    &
                                  : 2 * obj%len_half_tr_function + 1)
      temp_tr (obj%blim_npow2                                               &
               : obj%blim_npow2 + 1 - obj%len_half_tr_function : (-1))    &
        = obj%function (k)%tr (obj%len_half_tr_function:1:(-1))
      !
      call fft_rc_transform (obj  = obj%fun2_fwd_fft,    &
                             bufi = temp_tr,               &
                             bufo = complex_tr)
      !
      ! - put amplitude spectrum into trapl
      !
      do i = 1, nnyq
        !
        obj%apply (j)%tr (i) = abs (complex_tr (i))
        !
      end do
      !
      obj%apply (j)%tr (nnyq+1:) = 0.0
      !
      ! - put phase spectrum into work
      !
      do i = 1, nnyq
        !
        if (obj%apply (j)%tr (i) > 0.) then
          work_mtfun2 (i) = atan2 (aimag (complex_tr (i)),    &
                                   real (complex_tr(i)))
        else
          work_mtfun2 (i) = 0.0
        endif
        !
      end do
      !
      work_mtfun2 (nnyq + 1:3 * obj%blim_npow2 + 4) = 0.0
      !
      ! - do bulk shift calculations:
      !
      ! -  step 1) unwrap the phase trace, currently in work_mtfun2,
      !            put answer in trtmp
      !
      shift = 0.0
      !
      temp_tr (nstart) = work_mtfun2(nstart)
      !
      do i = nstart + 1, nstop
        !
        if (work_mtfun2(i) < work_mtfun2(i-1) - pi) then
          shift = shift + twopi
        else if (work_mtfun2(i) > work_mtfun2(i-1) + pi) then
          shift = shift - twopi
        end if
        !
        temp_tr(i) = work_mtfun2(i) + shift
        !
      end do
      !
      ! -  step 2) compute least squares fit of the unwrapped phase
      !    obj%apply%tr = amplitude spectra,  temp_tr = phase spectra
      !
      sumtt   = 0.0
      slope   = 0.0
      tmps    = (obj%freq_end - obj%freq_beg) / (nstop - nstart + 1)
      ampmxx  = obj%apply (j)%tr (nstart)
      ampmnn  = obj%apply (j)%tr (nstart)
      !
      where (obj%apply (j)%tr (nstart:nstop) < 1.0e-10)
        tram1u = 1.0e-19
      elsewhere
        tram1u = 1.0 / (obj%apply (j)%tr (nstart : nstop)    &
                        * obj%apply (j)%tr (nstart : nstop))
      end where
      !
      ampmxx = max (a1 = maxval (obj%apply (j)%tr (nstart : nstop)),     &
                    a2 = ampmxx)
      ampmnn = min (a1 = minval (obj%apply (j)%tr (nstart : nstop)),     &
                    a2 = ampmnn)
      !
      trsq1u = 1.0 / (tram1u * tram1u)
      !
      base_sum = sum (trsq1u)
      sumx     = sum ((obj%freq_beg                                &
                       + tmps * (/(j1, j1 = 0, nstop - nstart)/))    &
                      * trsq1u)
      sumy     = sum (temp_tr (nstart : nstop) * trsq1u)
      !
      ti1u = (obj%freq_beg                               &
              + tmps * (/(j2, j2 = 0, nstop - nstart)/)    &
              - sumx / base_sum)                           &
             / tram1u
      !
      sumtt = sum (ti1u * ti1u)
      slope = sum (ti1u * temp_tr (nstart : nstop) / tram1u)
      !
      slope = slope / sumtt
      !
      ! -  step 3) compute phase and time shifts
      !
      tshift = -slope / twopi
      !
      phase = (sumy - sumx * slope) / base_sum
      phase = mod (phase * 180.0 / real (pi, kind = 4), 360.0)
      !
      if (phase <= (-180.)) then
        phase = phase + 360.
      else if (phase > 180.) then
        phase = phase - 360.
      end if
      !
      ! - Compute and save the transfer function and info for diagnostic file
      !
      obj%function_seq = obj%function_seq  + 1
      !
      obj%function (k)%hd = 0.0d0
      !
      obj%function (k)%hd ( 1) = dble (obj%function_seq)
      obj%function (k)%hd ( 2) = 1.0d0
      obj%function (k)%hd ( 3) = real (obj%dt)
      obj%function (k)%hd ( 4) = - obj%len_op / 2
      obj%function (k)%hd ( 5) = tshift
      obj%function (k)%hd ( 6) = phase
      obj%function (k)%hd ( 9) = cc_before
      obj%function (k)%hd (10) = cc_after
      obj%function (k)%hd (11) = obj%apply (j)%id
      !
      if (obj%hdr_x > 0) then
        obj%function(k)%hd(12)=mth_bin_center(obj%x_init,obj%x_inc,ixbin)
!rev18        obj%function(k)%hd(12) = ixbin * obj%x_inc + obj%x_init
        write (hdxchr, '(F10.2)') obj%function (k)%hd(12)
      else
        hdxchr = '          '
      endif
      !
      if (obj%hdr_y > 0) then
        obj%function(k)%hd(13)=mth_bin_center(obj%y_init,obj%y_inc,iybin)
!rev18        obj%function (k)%hd(13) = iybin * obj%y_inc + obj%y_init
        write (hdychr, '(F10.2)') obj%function (k)%hd(13)
      else
        hdychr = '          '
      end if
      !
      obj%function (k)%hd (64) = real (obj%len_tr_function)
      !
      call lav_set_hdr (hd   = obj%function (k)%hd,    &
                        tr   = obj%function (k)%tr,    &
                        ndpt = obj%len_tr_function)
      !
      if (obj%dfile_lun > 0) then
        write (obj%dfile_lun, 999) obj%function_seq, hdxchr, hdychr,    &
                                     obj%num_std_trc,                     &
                                     int (obj%apply (j)%id),            &
                                     obj%apply (j)%count,               &
                                     ampmnn, ampmxx, tshift, phase,         &
                                     cc_before, cc_after
      end if
      !
    end do loop_thru_sets
    !
  999 format(1x, i10, 2 (1x, a10), 3 (1x, i6),    &
             3 (1x, g10.4), 1x, f9.4, 2 (1x, f11.8))
    !
  end subroutine mtfun_derive_function


  !!----------------------------- mtfun_blim -------------------------------!!
  !!----------------------------- mtfun_blim -------------------------------!!
  !!----------------------------- mtfun_blim -------------------------------!!

  !
  ! - mtfun_blim -- bandpass filter transfer function
  !    a   = input array
  !    fl  = low frequency
  !    fh  = high frequency
  !    ff  =
  !    np2 = nearest power of 2 to hold array a
  !
  subroutine mtfun_blim (a, obj)
    !
    ! - Arguments
    !
    real,                intent (inout) :: a (:)
    type (mtfun_struct), intent (in)    :: obj
    !
    ! - Local variables
    !
    complex :: complex_tr (obj%blim_npow2 + 4)
    !
    integer :: it

    integer :: n1
    integer :: n2
    integer :: np2
    integer :: n3
    integer :: np2d2
    integer :: np3
    integer :: nt
    !
    real :: savea
    !
    ! - Begin mtfun_blim
    !
    np2 = obj%blim_npow2
    !
    np3 = obj%blim_npow2 + 4
    n1  = max (a1 = 0,     &
               a2 = int (obj%freq_beg / obj%blim_ff - 2.0) * 2)
    n2  = int (obj%freq_end / obj%blim_ff + 5.0) * 2
    n3  = n2 + 1
    !
    call fft_rc_transform (obj  = obj%blim_rtoc_fft,    &
                           bufi = a,                      &
                           bufo = complex_tr)
    !
    complex_tr = complex_tr * 0.5
    !
    if (n1 > 0) then
      !
      complex_tr (:n1) = 0.0
      !
      complex_tr (n1 + 1) = complex_tr (n1 + 1) * 0.1
      complex_tr (n1 + 2) = complex_tr (n1 + 2) * 0.1
      complex_tr (n1 + 3) = complex_tr (n1 + 3) * 0.3
      complex_tr (n1 + 4) = complex_tr (n1 + 4) * 0.3
      complex_tr (n1 + 5) = complex_tr (n1 + 5) * 0.7
      complex_tr (n1 + 6) = complex_tr (n1 + 6) * 0.7
      complex_tr (n1 + 7) = complex_tr (n1 + 7) * 0.9
      complex_tr (n1 + 8) = complex_tr (n1 + 8) * 0.9
      !
    end if
    !
    if (n2 <= np2 + 2) then
      !
      complex_tr(n3:np3) = 0.0
      !
      complex_tr (n2)     = complex_tr (n2)     * 0.1
      complex_tr (n2 - 1) = complex_tr (n2 - 1) * 0.1
      complex_tr (n2 - 2) = complex_tr (n2 - 2) * 0.3
      complex_tr (n2 - 3) = complex_tr (n2 - 3) * 0.3
      complex_tr (n2 - 4) = complex_tr (n2 - 4) * 0.7
      complex_tr (n2 - 5) = complex_tr (n2 - 5) * 0.7
      complex_tr (n2 - 6) = complex_tr (n2 - 6) * 0.9
      complex_tr (n2 - 7) = complex_tr (n2 - 7) * 0.9
      !
    end if
    !
    call fft_cr_transform (obj       = obj%blim_ctor_fft,    &
                           bufi      = complex_tr,             &
                           bufo      = a,                      &
                           opt_scale = 1.0 / real (np2))
    !
    ! - invert output array
    !
    np2d2 = np2 / 2
    !
    do it = 2, np2d2
      !
      nt     = np2 - it + 2
      savea  = a (it)
      a (it) = a (nt)
      a (nt) = savea
      !
    end do
    !
  end subroutine mtfun_blim

!</execute_only>


  !!------------------------------- wrapup ---------------------------------!!
  !!------------------------------- wrapup ---------------------------------!!
  !!------------------------------- wrapup ---------------------------------!!


!<execute_only>

  subroutine mtfun_wrapup (obj)
    !
    ! - Arguments
    !
    type (mtfun_struct), intent (inout) :: obj       ! arguments
    !
    ! - Local variables
    !
    integer :: i
    !
    ! - Begin
    !
    if (obj%skip_wrapup) return
    obj%skip_wrapup = .true.
    !
  verify_dfile:    &
    if (obj%dfile_lun > 0) then
      !
      write (obj%dfile_lun, 997)
      write (obj%dfile_lun, 995) ' '
      write (obj%dfile_lun, 995) '----- MTFUN -----'
      write (obj%dfile_lun, 995) &
      'Phase and shift values derived from weighted least squares.  The error o&
      &f each term is assumed'
      write (obj%dfile_lun, 995) &
      'assumed to be proportional to the inverse of the square of the amplitude&
      & of the amplitude spectra.'
      write (obj%dfile_lun, 995) ' '
      write (obj%dfile_lun, 995) 'TRACE# - the trace sequence number'
      write (obj%dfile_lun, 995) 'XBIN   - the X bin number of the trace'
      write (obj%dfile_lun, 995) 'YBIN   - the Y bin number of the trace'
      write (obj%dfile_lun, 995) &
          '#STD   - the number of "standard" traces found for trace composite'
      write (obj%dfile_lun, 995) 'APL    - the "apply" type'
      write (obj%dfile_lun, 995) &
          '#APL   - the number of "apply" traces found for trace composite'
      write (obj%dfile_lun, 995) &
          'AMPMIN - the minimum value of the amplitude spectra'
      write (obj%dfile_lun, 995) &
          'AMPMAX - the maximim  value of the amplitude spectra'
      write (obj%dfile_lun, 995) &
          'TSHIFT - the time shift needed to match the standard trace, ',    &
          'in seconds'
      write (obj%dfile_lun, 995) &
              'PHASE  - the phase shift required to match the standard',    &
              ' trace, in degrees'
      write (obj%dfile_lun, 995) &
      'BEFORE - the correlation coefficient between the "standard" and "apply" &
      &trace'
      write (obj%dfile_lun, 995) 'CORR COEF'
      write (obj%dfile_lun, 995) &
      'AFTER  - the correlation coefficient between the "standard" and filtered&
      & "apply" trace'
      write (obj%dfile_lun, 995) 'CORR COEF'
      write (obj%dfile_lun, 995) ' '
      write (obj%dfile_lun, 996) 'Minimum samples needed in window ',    &
                                   obj%win_min_samps
      write (obj%dfile_lun, 997)
      !
      write (obj%dfile_lun, 995) ' '
      !
      if (obj%err_count_no_std_trace > 0) then
        write (obj%dfile_lun, 996)                    &
          'No "standard" trace occurences       - ',    &
           obj%err_count_no_std_trace
      end if
      !
      if (obj%err_count_insuff_data > 0) then
        write (obj%dfile_lun, 996)                    &
          'Not enough data in window occurences - ',    &
           obj%err_count_insuff_data
      end if
      !
      if (obj%err_count_no_apply_traces > 0) then
        write (obj%dfile_lun, 996)                    &
          'No "apply" trace occurences          - ',    &
           obj%err_count_no_apply_traces
      end if
      !
      write (obj%dfile_lun, 995) ' '
      close (obj%dfile_lun)
      obj%dfile_lun = 0
      !
    end if verify_dfile
    !
    do i = 1, obj%num_sets
      if (associated (obj%apply (i)%hd)) then
        call mem_free (obj%apply (i)%hd)
      end if
      !
      if (associated (obj%apply (i)%tr)) then
        call mem_free (obj%apply (i)%tr)
      end if
      !
      if (associated (obj%function (i)%hd)) then
        call mem_free (obj%function (i)%hd)
      end if
      !
      if (associated (obj%function (i)%tr)) then
        call mem_free (obj%function (i)%tr)
      end if
      !
    end do

    if (associated (obj%apply))         deallocate (obj%apply)
    if (associated (obj%function))      deallocate (obj%function)
    if (associated (obj%standard%hd)) call mem_free (obj%standard%hd)
    if (associated (obj%standard%tr)) call mem_free (obj%standard%tr)
    !
  995 format (1x, a)
  996 format (1x, a, i6)
  997 format (61 (' -'))
    !
  end subroutine mtfun_wrapup

!</execute_only>


  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!
  !!--------------------------- end of module ------------------------------!!

end module mtfun_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

