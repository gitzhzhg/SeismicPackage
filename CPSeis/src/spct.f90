!<CPS_v1 type="PROCESS"/>
!!------------------------------- spct.f90 ---------------------------------!!
!!------------------------------- spct.f90 ---------------------------------!!
!!------------------------------- spct.f90 ---------------------------------!!

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
! Name       : SPCT         (calculate trace SPeCTra.)
! Category   : transforms
! Written    : 1987-10-02   by: Bob Baumel
! Revised    : 2006-12-04   by: D. Glover
! Maturity   : production
! Purpose    : Transform input traces to amplitude and phase spectrum traces.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION

!
! If OPT_TRANSFORM = TIME_TO_FREQ
!
! SPCT takes input traces in the time domain and produces output traces in the
! frequency domain.  For each input trace, it generates TWO output traces:
! first the amplitude spectrum, then the phase spectrum (in consecutive traces).
!
!
! Phase Unwrapping
! When OPT_PHASE = UNWRAP or OPT_PHASE = FLATTEN, the phase spectrum is
! unwrapped in the range between FREQ_UNWRAP_BEG to FREQ_UNWRAP_END.  For
! frequencies outside this range, the phase spectrum traces still contain the
! original non-unwrapped phase (as in OPT_PHASE = NONE).
!
! If OPT_TRANSFORM = FREQ_TO_TIME
!
! SPCT takes two sequential input traces in the frequency domain and produces 
! output traces in the time domain.  
! For each pair input traces, it generates ONE output traces:
! The first input trace in each pair is the amplitude spectrum, 
! The second input trace in each pair the phase spectrum in degrees.
! Each pair of input traces must be passed to SPCT at the same time.
! You can use process BUNCH to gather traces toegether.
! No consideration is made of the phase flattening option.
! Note that tapering in the time domain or filtering in the frequency domain
! during the time to frequency transform may alter trace characteristics.
! This means that a trace that had both a TIME_TO_FREQ transform followed 
! by a FREQ_TO_TIME transform may not be identical to the original trace.
!
! Flattened Phase Spectra
! For OPT_PHASE = FLATTEN, the unwrapped phase spectrum is least squares
! fitted with a straight line between frequencies FREQ_FIT_BEG and FREQ_FIT_END
! (like TFUN calculation), to determine the time shift that makes the unwrapped
! phase as constant as possible in this frequency range.  (This causes the
! the phase spectrum in this frequency range to be as horizontal as possible.)
!
! The calculated time-shift is stored in header HDR_TIM_SHFT.  The average
! phase of the time-shifted wavelet (in the frequency range between
! FREQ_FIT_BEG and FREQ_FIT_END) is stored in header HDR_AVE_PHASE.
!
!
! Phase Units
! All phase values (in the output phase spectrum traces, and values stored in
! header HDR_AVE_PHASE) are in DEGREES.  For OPT_PHASE = NONE, values in phase
! traces are in range [-180,180]. For OPT_PHASE = UNWRAP and
! OPT_PHASE = FLATTEN, the unwrapped phase values may extend well outside this
! range.
!
!
! Reference
! Brigham, E., The Fast Fourier Transform, 1974, Prentice-Hall, p. 46.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! Frequency Sampling
! Accuracy of phase unwrapping depends on computing the spectrum at a
! sufficiently dense set of frequencies which, in turn, depends on the trace
! length going into SPCT (the longer the time window, the finer the frequency
! sampling). Thus, even if the desired spectrum is for a short wavelet (e.g.,
! 0.2 seconds) DO NOT chop the trace to this short length using TSEL prior to
! SPCT; instead, keep the trace several seconds long, and use TIM_BEG, TIM_END
! and LEN_TAPER in SPCT to window the input traces.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                      TRACE INPUT REQUIREMENTS
!
! No special requirements.
!
! Input traces may be gathered or ungathered.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                     TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
!
! Process generates two output traces for each input trace.
!
! This process outputs one trace at a time.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       set to 1
! GATHERED  whether traces are a legitimate gather  set to false.
! NWIH      number of words in trace header         used but not changed
! NDPT      number of sample values in trace        changed
!                                                    (number of frequencies)
! TSTRT     starting time on trace                  set to 0.0
!                                                    (starting frequency)
! DT        trace sample interval                   changed
!                                                    (scaled freq. interval)
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 1       Sequential Trace Count     Renumbered
! 2       Head mute                  Set to 1
! 3       Current Group number       Used but not changed
! 4       Current Channel number     Renumbered
! 25      Largest Absolute Value     Set
! 64      Tail mute                  Set to number of frequencies
!         HDR_TIM_SHFT               Set
!         HDR_AVE_PHASE              Set
!         HDR_TIM_FIRST              Set
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 15. 2006-12-04  D. Glover    Added NULLIFY statements for Intel compiler.
! 14. 2005-10-10  Goodger      Change argument tr_amp in routine transform_ft
!                              from intent(out) to intent(inout).
! 13. 2005-06-14  D Hanson     Add OPT_TRANSFORM for inverse transform.
! 12. 2002-04-22  B Baumel     Fix bug involving confusion of input and output
!                              NDPT globals causing invalid memory references.
! 11. 2001-04-30  B Baumel     Change wrapped_up flag to skip_wrapup.
! 10. 2000-06-16  B Baumel     Add setting of HDR_TIM_FIRST header word (to
!                              display TIM_FIRST value usable by DSIG) when
!                              OPT_PHASE = FLATTEN; also fix trace flow logic
!                              since this is now an "ungathering" process
!                              (change to two sets of headers & traces).
!  9. 2000-06-13  B Baumel     Another tweak to time-domain tapers.
!  8. 2000-06-12  B Baumel     Additional fix to center tapers on window edges.
!  7. 2000-06-09  Brad Kruse   Comments from Bob Baumel.
!  6. 2000-04-28  Brad Kruse   Converted from old system.
!  5. 1998-02-18  B Baumel     Allow separate frequency ranges for phase
!                                unwrapping and for fitting (OPT=3).
!  4. 1997-10-27  B Baumel     Added lots of new options.
!  3. 1988-10-25  H Ball       NWIH and NWPT Conversion
!  2. 1988-06-10  B Baumel     Add CPSPRT calls.
!  1. 1987-10-02  B Baumel     Original version.
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
! NEED_REQUEST    true     whether this process ever needs to request traces.
! NEED_LABEL      true     whether this process needs a label.
! TWOSETS         true     whether this process needs two trace/header arrays.
! NSCRATCH       varies    amount of temporary memory needed.
! NSTORE         varies    amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR == 1              if this process is outputting a trace.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
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
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<gui_def>
!<NS SPCT Process/NC=80>
!                          calculate trace SPeCTra.
!         Transform input traces to amplitude and phase spectrum traces.
!         or the inverse.
!
!         OPT_TRANSFORM=`CCCCCCCCCCC
!
!                               Find Spectra
!      FREQ_SCALE=`FFFFFFFFFFFF           FREQ_MAX=`FFFFFFFFFFFF
!      TIM_NUM~~~=`IIIIIIIIIII            TIM_INC =`FFFFFFFFFFFF
!      TIM_BEG~~~=`FFFFFFFFFFFF           TIM_END =`FFFFFFFFFFFF
!      LEN_TAPER =`FFFFFFFFFFFF
!
!                        Phase Manipulation Options
!      OPT_PHASE =`CCCCCC
!
!                  Frequency Range for Unwrapping the Phase
!      FREQ_UNW_BEG=`FFFFFFFFFFFF         FREQ_UNW_END=`FFFFFFFFFFFF
!
!                Frequency Range for Flattening Phase Spectrum
!      FREQ_FIT_BEG=`FFFFFFFFFFFF         FREQ_FIT_END=`FFFFFFFFFFFF
!
!               Header Words for Storing the Flattening Results
!      HDR_TIM_SHFT =`IIIIII
!      HDR_AVE_PHASE=`IIIIII
!      HDR_TIM_FIRST=`IIIIII
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="FREQ_SCALE">
!<Tip> Frequency interval in Hz for each "second" in output traces. </Tip>
! Default = 100.0
! Allowed = real > 0.0
! Frequency interval in Hz denoted by each "second" in the output traces.
!</Help>
!
!<Help KEYWORD="FREQ_MAX">
!<Tip> Maximum frequency for calculations and spectra. </Tip>
! Default = Nyquist
! Allowed = real > 0.0
! Maximum frequency to include the output spectrum traces. FREQ_SCALE and
! FREQ_MAX together determine the length of output traces.
!</Help>
!
!
!<Help KEYWORD="TIM_NUM">
!<Tip> Number of time samples. </Tip>
! Default = NDPT
! Allowed = integer
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Start of input trace time window for spectrum calculation. </Tip>
! Default = TSTRT
! Allowed = end_of_trace > real >= TSTRT
! Start of input trace time window, in seconds, for spectrum calculation.
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> End of input trace time window for spectrum calculation. </Tip>
! Default = end_of_trace
! Allowed = end_of_trace >= real > TIM_BEG
! End of input trace time window, in seconds, for spectrum calculation.
!</Help>
!
!<Help KEYWORD="TIM_INC">
!<Tip> Trace time increment for freq to time transform. </Tip>
! Default = DT
! Allowed = real
!</Help>
!
!<Help KEYWORD="LEN_TAPER">
!<Tip> Length of cosine taper, in seconds, to reduce transform artifacts. </Tip>
! Default = 0.1
! Allowed = real >= 0.0
! Cosine taper is centered on nominal window edges (but if taper would run
! off beginning or end of the trace, taper is moved inward until it lies
! entirely on the trace).
!</Help>
!
!<Help KEYWORD="OPT_TRANSFORM">
!<Tip> Transform from time to frequency or frequency to time. </Tip>
! Default = TIME_TO_FREQ
! Allowed = TIME_TO_FREQ Transform from time to frequency.
! Allowed = FREQ_TO_TIME Transform from frequency to time.
!</Help>
!
!<Help KEYWORD="OPT_PHASE">
!<Tip> Whether to unwrap phase or unwrap and flatten the phase spectrum. </Tip>
! Default = NONE
! Allowed = NONE     (Do no phase unwrapping.)
! Allowed = UNWRAP   (Unwrap phase but do not time-shift to flatten.)
! Allowed = FLATTEN  (Unwrap phase and time-shift to flatten phase spectrum.)
! Phase unwrapping eliminates the constraint that phase must lie within the
! +/- 180 degree range.
!
! Time-shifting the trace uses the phase-shift time-shift relationship to make
! the phase spectrum as horizontal as possible in the selected frequency range.
!</Help>
!
!<Help KEYWORD="FREQ_UNW_BEG">
!<Tip> Low end of frequency range to use for phase unwrapping, in Hz. </Tip>
! Default = 8
! Allowed = FREQ_MAX > real >= 0.0
! Active when OPT_PHASE = UNWRAP or OPT_PHASE = FLATTEN.
!</Help>
!
!<Help KEYWORD="FREQ_UNW_END">
!<Tip> High end of frequency range to use for phase unwrapping, in Hz. </Tip>
! Default = 60
! Allowed = FREQ_MAX >= real > FREQ_UNW_BEG
! Active when OPT_PHASE = UNWRAP or OPT_PHASE = FLATTEN.
!</Help>
!
!<Help KEYWORD="FREQ_FIT_BEG">
!<Tip> Low end of frequency range to use for phase spectrum flattening. </Tip>
! Default = 8
! Allowed = FREQ_UNW_END > real >= FREQ_UNW_BEG
! Active when OPT_PHASE = FLATTEN.
!</Help>
!
!<Help KEYWORD="FREQ_FIT_END">
!<Tip> High end of frequency range to use for phase spectrum flattening. </Tip>
! Default = 60
! Allowed = FREQ_UNW_END >= real > FREQ_FIT_BEG
! Active when OPT_PHASE = FLATTEN.
!</Help>
!
!<Help KEYWORD="HDR_TIM_SHFT">
!<Tip> Header word to receive time-shift when OPT_PHASE = FLATTEN. </Tip>
! Default = 48
! Allowed = 1 - NWIH
! Time-shift calculated when OPT_PHASE = FLATTEN is stored in header word
! HDR_TIM_SHFT.
!</Help>
!
!<Help KEYWORD="HDR_AVE_PHASE">
!<Tip> Header word to receive average phase when OPT_PHASE = FLATTEN. </Tip>
! Default = 49
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="HDR_TIM_FIRST">
!<Tip> Header word to receive corrected TIM_FIRST if OPT_PHASE = FLATTEN. </Tip>
! Default = 50
! Allowed = 1 - NWIH
! The value stored in this header word is a corrected TIM_FIRST value which
! may be used in the DSIG process to avoid a bulk shift when using the wavelet
! for designaturing traces.
!
! The actual value stored in this header word is given by the equation
!       corrected_TIM_FIRST  =  current_TSTRT  -  TIM_SHFT
! where TIM_SHFT is the value stored in header word HDR_TIM_SHFT.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

module spct_module
  !
  ! - Module references
  !
  use fft_module
  use headsave_module
  use lav_module
  use mem_module
  use named_constants_module
  use pattern_module
  use pc_module
  use sequence_module
  !
  implicit none
  !
  private
  !
  public :: spct_create
  public :: spct_initialize
  public :: spct_update
  public :: spct_delete

!<execute_only>
  public :: spct            ! main execution (trace processing) routine.
  public :: spct_wrapup
!</execute_only>

  character (len=100), public, save :: SPCT_IDENT = &
'$Id: spct.f90,v 1.15 2006/12/04 13:29:57 Glover prod sps $'


!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

  type, public :: spct_struct

    private
    !
    logical                    :: skip_wrapup        ! wrapup flag.
    !
    ! - Process parameters
    !      Available from the parameter cache
    !
    character (len=12)         :: opt_transform      ! pc - TIME_TO_FREQ
    character (len=7)          :: opt_phase          ! pc - NONE,UNWRAP,FLATTEN
    integer                    :: hdr_tim_shft       ! pc - 1 - NWIH
    integer                    :: hdr_ave_phase      ! pc - 1 - NWIH
    integer                    :: hdr_tim_first      ! pc - 1 - NWIH
    real                       :: freq_fit_beg       ! pc - real > FREQ_UNW_BEG
    real                       :: freq_fit_end       ! FREQ_FIT_BEG<FREQ_UNW_END
    real                       :: freq_max           ! pc - real > 0.0
    real                       :: freq_scale         ! pc - real > 0.0
    real                       :: freq_unwrap_beg    ! pc - real > 0.0
    real                       :: freq_unwrap_end    ! pc - real > FREQ_UNW_BEG
    real                       :: len_taper          ! pc - real >= 0.0
    integer                    :: tim_num            ! pc - integer
    real                       :: tim_inc            ! pc - real 
    real                       :: tim_beg            ! pc - real
    real                       :: tim_end            ! pc - real > TIM_BEG
    !
    ! - Common globals
    !
    integer                    :: numtr              ! Common globals, nwih
    integer                    :: nwih               ! Common globals, nwih
    !
    integer                    :: nt_inp             ! Common globals, inp num
    real                       :: t0_inp             ! Common globals, inp min
    real                       :: t1_inp             ! Common globals, inp max
    real                       :: dt_inp             ! Common globals, inp inc
    !
    integer                    :: nt_out             ! Common globals, out num
    real                       :: t0_out             ! Common globals, out min
    real                       :: t1_out             ! Common globals, out max
    real                       :: dt_out             ! Common globals, out inc
    !
    integer                    :: nt_data           ! time num
    real                       :: t0_data           ! time min
    real                       :: t1_data           ! time max
    real                       :: dt_data           ! time inc
    !
    ! - Common spctc1
    !
    complex, pointer           :: filter (:)
    integer                    :: fft_len
    integer                    :: first_time_samp
    integer                    :: freq_fit_beg_samp
    integer                    :: freq_fit_end_samp
    integer                    :: freq_unwrap_beg_samp
    integer                    :: freq_unwrap_end_samp
    integer                    :: last_time_samp
    integer                    :: len_taper_samp
    integer                    :: half_len_taper_samp
    integer                    :: nout
    integer                    :: ntr_inp
    integer                    :: ntr_out
    integer                    :: ntr_count
    integer                    :: ntr_seq
    integer                    :: nf_nyquist
    real                       :: rf_nyquist      ! nyquist frequency in Hz
    integer                    :: nf_data         ! freq sample num 
    real                       :: f0_data         ! freq sample min (Hz.)  
    real                       :: f1_data         ! freq sample max (Hz.)  
    real                       :: df_data         ! freq sample inc (Hz.)  
    real                       :: dw_data         ! Phase (rad.) per freq sample
    real,    pointer           :: taper (:)
    !
    integer                    :: ipn
    logical                    :: phase_valid
    real,    pointer           :: tr (:)
    double precision, pointer  :: hd (:, :)
    !
    type (sequence_struct)     :: seq_obj
    type (fft_struct), pointer :: fft_rc   ! real to complex fft structure
    type (fft_struct), pointer :: fft_cr   ! complex to real fft structure
    type ( headsave_struct ),pointer :: h  ! headsave structure
    !
  end type spct_struct


!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!


!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

    type(spct_struct),pointer,save :: object      ! needed for traps.

    real, parameter :: TWOPI = 2.0 * PI

  contains

  !!------------------------------- create ---------------------------------!!
  !!------------------------------- create ---------------------------------!!
  !!------------------------------- create ---------------------------------!!

  subroutine spct_create (obj)
    implicit none
    !
    ! - Arguments
    !
    type (spct_struct), pointer :: obj       ! arguments
    !
    ! - Begin spct_create
    !
    allocate (obj)
    !
    nullify (obj%filter)
    nullify (obj%taper)
    nullify (obj%tr)
    nullify (obj%hd)
    nullify (obj%fft_rc) ! jpa
    nullify (obj%fft_cr) ! jpa
    nullify (obj%h) ! jpa
    !
    call spct_initialize (obj)
    !
  end subroutine spct_create


  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!
  !!------------------------------- delete ---------------------------------!!


  subroutine spct_delete (obj)
    implicit none
    !
    ! - Arguments
    !
    type(spct_struct),pointer :: obj       ! arguments
    !
    ! - Begin spct_delete
    !

!<execute_only>
      call spct_wrapup (obj)
!</execute_only>

    call mem_free (obj%filter)
    call mem_free (obj%taper)
    call mem_free (obj%tr)
    call mem_free (obj%hd)
    if (associated(obj%fft_rc))  call fft_delete (obj%fft_rc)
    if (associated(obj%fft_cr))  call fft_delete (obj%fft_cr)
    if (associated(obj%h))       call headsave_delete (obj%h)
    !
    deallocate (obj)
    !
  end subroutine spct_delete


  !!----------------------------- initialize -------------------------------!!
  !!----------------------------- initialize -------------------------------!!
  !!----------------------------- initialize -------------------------------!!

  subroutine spct_initialize (obj)
    implicit none
    !
    type (spct_struct), intent (inout) :: obj       ! arguments
    !
    ! - Begin spct_initialize
    !
    ! - Get a few globals
    !
    call pc_get_global ('NDPT',  obj%nt_inp)
    call pc_get_global ('TSTRT', obj%t0_inp)
    call pc_get_global ('DT',    obj%dt_inp)
    obj%t1_inp = ( obj%nt_inp - 1 ) * obj%dt_inp + obj%t0_inp
    !
    ! - Initialize parameters
    !
    obj%freq_scale          = 100.0
    obj%freq_max            = 0.5 / obj%dt_inp
    obj%tim_num             = obj%nt_inp
    obj%tim_inc             = obj%dt_inp
    obj%tim_beg             = obj%t0_inp
    obj%tim_end             = obj%t0_inp + (obj%nt_inp - 1)*obj%dt_inp
    obj%len_taper           = 0.1
    obj%opt_transform       = 'TIME_TO_FREQ'
    obj%opt_phase           = 'NONE'
    obj%freq_unwrap_beg     = 8
    obj%freq_unwrap_end     = 60
    obj%freq_fit_beg        = 8
    obj%freq_fit_end        = 60
    obj%hdr_tim_shft        = 48
    obj%hdr_ave_phase       = 49
    obj%hdr_tim_first       = 50
    !
    call spct_update (obj)
    !
  end subroutine spct_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


  subroutine spct_update (obj)
    implicit none
    !
    type(spct_struct),intent(inout),target :: obj        ! arguments
    !
    real    :: fact                         ! local variables
    integer :: i, i_err, nscratch, nstore                ! local variables
    integer :: i_stat
    !
    ! - Begin spct_update
    !
    object => obj                ! needed for traps.
    obj%skip_wrapup = .true.     ! needed for the wrapup routine.

    !!-------------------------- read parameters ---------------------------!!
    !!-------------------------- read parameters ---------------------------!!
    !!-------------------------- read parameters ---------------------------!!

    !
    ! - Get a few globals
    !
    call pc_put_global ('NUMTR', obj%numtr)
    call pc_get_global ('NWIH' , obj%nwih)  ! number of header words.
    call pc_get_global ('NDPT',  obj%nt_inp)
    call pc_get_global ('TSTRT', obj%t0_inp)
    call pc_get_global ('DT',    obj%dt_inp)
    obj%t1_inp = ( obj%nt_inp - 1 ) * obj%dt_inp + obj%t0_inp
    obj%ipn = pc_get_ipn()
    !
    ! - Get process parameters
    !
    call pc_get ('FREQ_SCALE',    obj%freq_scale)
    call pc_get ('FREQ_MAX',      obj%freq_max)
    call pc_get ('TIM_NUM',       obj%tim_num)
    call pc_get ('TIM_INC',       obj%tim_inc)
    call pc_get ('TIM_BEG',       obj%tim_beg)
    call pc_get ('TIM_END',       obj%tim_end)
    call pc_get ('LEN_TAPER',     obj%len_taper)
    call pc_get ('OPT_TRANSFORM', obj%opt_transform)
    call pc_get ('OPT_PHASE',     obj%opt_phase)
    call pc_get ('FREQ_UNW_BEG',  obj%freq_unwrap_beg)
    call pc_get ('FREQ_UNW_END',  obj%freq_unwrap_end)
    call pc_get ('FREQ_FIT_BEG',  obj%freq_fit_beg)
    call pc_get ('FREQ_FIT_END',  obj%freq_fit_end)
    call pc_get ('HDR_TIM_SHFT',  obj%hdr_tim_shft)
    call pc_get ('HDR_AVE_PHASE', obj%hdr_ave_phase)
    call pc_get ('HDR_TIM_FIRST', obj%hdr_tim_first)
    !
!print'(" aa1 inp spct_update p=",i2," t=",i8,1x,g12.6,1x,g12.6,1x,g12.6)', &
!obj%ipn, obj%nt_inp, obj%t0_inp, obj%t1_inp, obj%dt_inp
!print'(" aa1 tim spct_update p=",i2," t=",i8,1x,g12.6,1x,g12.6,1x,g12.6)', &
!obj%ipn, obj%tim_num, obj%tim_beg, obj%tim_end, obj%tim_inc

    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!
    !!------------------------- verify parameters --------------------------!!
    xxif_time_to_freq_1 : &
    if ( string_upper_compare ( obj%opt_transform(1:1), 't' ) ) then
      !
      obj%nt_data = obj%nt_inp
      obj%t0_data = obj%t0_inp
      obj%t1_data = obj%t1_inp
      obj%dt_data = obj%dt_inp
      !
    else xxif_time_to_freq_1 
      !
      i_stat = pattern_stop2('fspct:', .true., &
       obj%tim_beg, obj%tim_inc, obj%tim_end, obj%tim_num, &
          'tim_beg',   'tim_inc',   'tim_end',   'tim_num', &
       pc_verify_scalar('tim_beg' ), pc_verify_scalar('tim_inc' ), &
       pc_verify_scalar('tim_end' ), pc_verify_scalar('tim_num' ))   
      !
      !obj%tim_num = ( obj%tim_end - obj%tim_beg ) / obj%tim_inc + 1
      !
      obj%nt_data = obj%tim_num 
      obj%t0_data = obj%tim_beg 
      obj%t1_data = obj%tim_end 
      obj%dt_data = obj%tim_inc 
      !
!print'(" aa2 spct_update p=",i2," t=",i8,1x,g12.6,1x,g12.6,1x,g12.6)', &
!obj%ipn, obj%tim_num, obj%tim_beg, obj%tim_end, obj%tim_inc
      !
    end if xxif_time_to_freq_1 
    !
!print'(" aa2 inp spct_update p=",i2," t=",i8,1x,g12.6,1x,g12.6,1x,g12.6)', &
!obj%ipn, obj%nt_inp, obj%t0_inp, obj%t1_inp, obj%dt_inp
!print'(" aa2 tim spct_update p=",i2," t=",i8,1x,g12.6,1x,g12.6,1x,g12.6)', &
!obj%ipn, obj%tim_num, obj%tim_beg, obj%tim_end, obj%tim_inc
    !
    obj%rf_nyquist = 0.5 / obj%dt_data
    !
    ! - Check FREQ_SCALE
    !
    if (obj%freq_scale <= 0.0) call pc_error ('FREQ_SCALE must be positive.')
    !
    ! - Check FREQ_MAX  (Note: will adjust to multiple of DF later)
    !
    if (obj%freq_max > 0.0) then
      obj%freq_max = min (obj%freq_max, obj%rf_nyquist)
    else
      call pc_error ('FREQ_MAX must be positive.')
      obj%freq_max = obj%rf_nyquist
    end if
    !
    ! - Check TIM_BEG and TIM_END
    !
    obj%first_time_samp = nint ((obj%tim_beg - obj%t0_data) / obj%dt_data) + 1
    obj%first_time_samp = min (max(obj%first_time_samp, 1), obj%nt_data)
    obj%last_time_samp  = nint ((obj%tim_end - obj%t0_data) / obj%dt_data) + 1
    obj%last_time_samp  = min (max(obj%last_time_samp, 1), obj%nt_data)
    if (obj%last_time_samp <= obj%first_time_samp) then
      call pc_error ('TIM_END must be greater than TIM_BEG.')
      if (pc_verify_scalar('TIM_BEG')) then
        obj%last_time_samp = obj%first_time_samp
      else
        obj%first_time_samp = obj%last_time_samp
      end if
    end if
    !
!print'(" aa3 spct_update p=",i2," t=",i8,1x,g12.6,1x,g12.6,1x,g12.6)', &
!obj%ipn, obj%tim_num, obj%tim_beg, obj%tim_end, obj%tim_inc
    !
    obj%tim_beg = obj%t0_data + (obj%first_time_samp - 1)*obj%dt_data
    obj%tim_end = obj%t0_data + (obj%last_time_samp  - 1)*obj%dt_data
    obj%tim_num = ( obj%tim_end - obj%tim_beg ) / obj%tim_inc + 1
    !
!print'(" aa3 inp spct_update p=",i2," t=",i8,1x,g12.6,1x,g12.6,1x,g12.6)', &
!obj%ipn, obj%nt_inp, obj%t0_inp, obj%t1_inp, obj%dt_inp
!print'(" aa3 tim spct_update p=",i2," t=",i8,1x,g12.6,1x,g12.6,1x,g12.6)', &
!obj%ipn, obj%tim_num, obj%tim_beg, obj%tim_end, obj%tim_inc
!print'(" aa3 fir spct_update p=",i2," t=",i8,1x,i8)', &
!obj%ipn, obj%first_time_samp, obj%last_time_samp  
    !
    ! - Check LEN_TAPER
    !
    if (obj%len_taper < 0.0) then
      call pc_error ('LEN_TAPER must be non-negative.')
      obj%len_taper = 0.0
    end if
    obj%half_len_taper_samp = nint (0.5 * obj%len_taper / obj%dt_data)
    obj%len_taper_samp      = obj%half_len_taper_samp * 2 + 1
    obj%len_taper           = (obj%len_taper_samp - 1) * obj%dt_data
    !
    ! - Check OPT_TRANSFORM
    !
    call string_to_upper (obj%opt_transform)
    !
    xxif_time_to_freq_2 : &
    if ( string_upper_compare ( obj%opt_transform(1:1), 't' ) ) then
      !
      obj%opt_transform = 'TIME_TO_FREQ'
      !
    else if ( string_upper_compare ( obj%opt_transform(1:1), 'f' ) ) then
      !
      obj%opt_transform = 'FREQ_TO_TIME'
      !
    else xxif_time_to_freq_2 
      !
      call pc_error ('opt_transform must be TIME_TO_FREQ or FREQ_TO_TIME.')
      !
    end if xxif_time_to_freq_2 
    !
    ! - Check OPT_PHASE
    !
    call string_to_upper (obj%opt_phase)
    if (obj%opt_phase(1:1) == 'N') then
      obj%opt_phase = 'NONE'
    else if (obj%opt_phase(1:1) == 'U') then
      obj%opt_phase = 'UNWRAP'
    else if (obj%opt_phase(1:1) == 'F') then
      obj%opt_phase = 'FLATTEN'
    else
      call pc_error ('OPT_PHASE must be NONE, UNWRAP, or FLATTEN.')
    end if
    !
    ! - Check FREQ_UNW_BEG, FREQ_UNW_END, FREQ_FIT_BEG, FREQ_FIT_END
    !
    obj%freq_unwrap_beg = min (max(obj%freq_unwrap_beg, 0.0), obj%freq_max)
    obj%freq_unwrap_end = min (max(obj%freq_unwrap_end, 0.0), obj%freq_max)
    if (obj%freq_unwrap_end < obj%freq_unwrap_beg) then
      if (pc_verify_scalar('FREQ_UNW_BEG')) then
        obj%freq_unwrap_end = obj%freq_unwrap_beg
      else
        obj%freq_unwrap_beg = obj%freq_unwrap_end
      end if
    end if
    !
    if (pc_verify_scalar('FREQ_FIT_BEG')  &
                    .or. pc_verify_scalar('FREQ_FIT_END')) then
      obj%freq_fit_beg = min (max(obj%freq_fit_beg, 0.0), obj%freq_max)
      obj%freq_fit_end = min (max(obj%freq_fit_end, 0.0), obj%freq_max)
      if (obj%freq_fit_end < obj%freq_fit_beg) then
        if (pc_verify_scalar('FREQ_FIT_BEG')) then
          obj%freq_fit_end = obj%freq_fit_beg
        else
          obj%freq_fit_beg = obj%freq_fit_end
        end if
      end if
      obj%freq_unwrap_beg = min (obj%freq_unwrap_beg, obj%freq_fit_beg)
      obj%freq_unwrap_end = max (obj%freq_unwrap_end, obj%freq_fit_end)
    else
      obj%freq_fit_beg = min (max(obj%freq_fit_beg, obj%freq_unwrap_beg), &
                              obj%freq_unwrap_end)
      obj%freq_fit_end = min (max(obj%freq_fit_end, obj%freq_fit_beg),    &
                              obj%freq_unwrap_end)
    end if
    !
    !call pc_put_sensitive_field_flag ('TIM_NUM', .false. )
    call pc_put_sensitive_field_flag ('TIM_NUM', &
                string_upper_compare ( obj%opt_transform(1:1), 'f') ) 
    call pc_put_sensitive_field_flag ('TIM_INC', &
                string_upper_compare ( obj%opt_transform(1:1), 'f') ) 
    !
    if (obj%opt_phase=='UNWRAP' .or. obj%opt_phase=='FLATTEN') then
      !
      if (obj%freq_unwrap_end <= obj%freq_unwrap_beg) then
        call pc_error ('FREQ_UNW_END must be greater than FREQ_UNW_BEG.')
      end if
      !
      call pc_put_sensitive_field_flag ('FREQ_UNW_BEG', .true.)
      call pc_put_sensitive_field_flag ('FREQ_UNW_END', .true.)
      !
    else
      !
      call pc_put_sensitive_field_flag ('FREQ_UNW_BEG', .false.)
      call pc_put_sensitive_field_flag ('FREQ_UNW_END', .false.)
      !
    end if
    !
    if (obj%opt_phase == 'FLATTEN') then
      !
      if (obj%freq_fit_end <= obj%freq_fit_beg) then
        call pc_error ('FREQ_FIT_END must be greater than FREQ_FIT_BEG.')
      end if
      obj%hdr_tim_shft  = min (max(obj%hdr_tim_shft,  1), obj%nwih)
      obj%hdr_ave_phase = min (max(obj%hdr_ave_phase, 1), obj%nwih)
      obj%hdr_tim_first = min (max(obj%hdr_tim_first, 1), obj%nwih)
      !
      call pc_put_sensitive_field_flag ('FREQ_FIT_BEG',  .true.)
      call pc_put_sensitive_field_flag ('FREQ_FIT_END',  .true.)
      call pc_put_sensitive_field_flag ('HDR_TIM_SHFT',  .true.)
      call pc_put_sensitive_field_flag ('HDR_AVE_PHASE', .true.)
      call pc_put_sensitive_field_flag ('HDR_TIM_FIRST', .true.)
      !
    else
      !
      call pc_put_sensitive_field_flag ('FREQ_FIT_BEG',  .false.)
      call pc_put_sensitive_field_flag ('FREQ_FIT_END',  .false.)
      call pc_put_sensitive_field_flag ('HDR_TIM_SHFT',  .false.)
      call pc_put_sensitive_field_flag ('HDR_AVE_PHASE', .false.)
      call pc_put_sensitive_field_flag ('HDR_TIM_FIRST', .false.)
      !
    end if
    !
    ! - Determine the FFT radix and related variables
    !
    obj%fft_len = 8
    do while(obj%fft_len < obj%nt_data)
      obj%fft_len = 2*obj%fft_len
    end do
    !
    obj%nf_nyquist     = obj%fft_len / 2 + 1
    !
    obj%f0_data  = 0.
    obj%df_data  = 1.0 / (obj%fft_len * obj%dt_data)
    obj%dw_data  = TWOPI * obj%df_data
    obj%nf_data    = nint (obj%freq_max / obj%df_data) + 1
    obj%f1_data  = (obj%nf_data - 1) * obj%df_data + obj%f0_data
    !
    obj%freq_max = obj%f1_data 
    !
    xxif_time_to_freq_4 : &
    if ( string_upper_compare ( obj%opt_transform(1:1), 't' ) ) then
      !
      obj%nt_out = obj%nf_data
      obj%t0_out = 0.
      !
    if (obj%freq_scale > 0.0) then
      obj%dt_out = obj%df_data / obj%freq_scale
    else
      obj%dt_out = obj%df_data
    end if
      !
    else xxif_time_to_freq_4 
      !
      obj%nt_out = obj%nt_data
      obj%t0_out = obj%t0_data
      obj%dt_out = obj%dt_data
      !
    end if xxif_time_to_freq_4 
    !
    obj%t1_out = ( obj%nt_out - 1 ) * obj%dt_out + obj%t0_out
    !
!print'(" aa4 out spct_update p=",i2," t=",i8,1x,g12.6,1x,g12.6,1x,g12.6)', &
!obj%ipn, obj%nt_out, obj%t0_inp, obj%t1_inp, obj%dt_inp

    !!------------------- call processes internally ------------------------!!
    !!------------------- call processes internally ------------------------!!
    !!------------------- call processes internally ------------------------!!


    !!------------------------ write parameters ----------------------------!!
    !!------------------------ write parameters ----------------------------!!
    !!------------------------ write parameters ----------------------------!!

    call pc_put_global ('NUMTR',    1)
    call pc_put_global ('GATHERED', .false.)
    call pc_put_global ('NDPT',     obj%nt_out)
    call pc_put_global ('TSTRT',    obj%t0_out)
    call pc_put_global ('DT',       obj%dt_out)
    !
    call pc_put_options_field ('OPT_TRANSFORM',                         &
                               (/'TIME_TO_FREQ', 'FREQ_TO_TIME' /),     &
                               2)
    !
    call pc_put_options_field ('OPT_PHASE',                             &
                               (/'NONE   ', 'UNWRAP ', 'FLATTEN' /),    &
                               3)
    !
    call pc_put ('FREQ_SCALE',    obj%freq_scale)
    call pc_put ('FREQ_MAX',      obj%freq_max)
    call pc_put ('TIM_NUM',       obj%tim_num)
    call pc_put ('TIM_INC',       obj%tim_inc)
    call pc_put ('TIM_BEG',       obj%tim_beg)
    call pc_put ('TIM_END',       obj%tim_end)
    call pc_put ('LEN_TAPER',     obj%len_taper)
    call pc_put ('OPT_TRANSFORM', obj%opt_transform)
    call pc_put ('OPT_PHASE',     obj%opt_phase)
    call pc_put ('FREQ_UNW_BEG',  obj%freq_unwrap_beg)
    call pc_put ('FREQ_UNW_END',  obj%freq_unwrap_end)
    call pc_put ('FREQ_FIT_BEG',  obj%freq_fit_beg)
    call pc_put ('FREQ_FIT_END',  obj%freq_fit_end)
    call pc_put ('HDR_TIM_SHFT',  obj%hdr_tim_shft)
    call pc_put ('HDR_AVE_PHASE', obj%hdr_ave_phase)
    call pc_put ('HDR_TIM_FIRST', obj%hdr_tim_first)
    !
!print'(" aa5 spct_update p=",i2," t=",i8,1x,g12.6,1x,g12.6,1x,g12.6)', &
!obj%ipn, obj%tim_num, obj%tim_beg, obj%tim_end, obj%tim_inc
    !
    ! - Set the controls
    !
    nscratch = obj%fft_len + 2*obj%nf_nyquist + obj%nf_data
    nstore   = 2*obj%nwih + 3*obj%nf_data + obj%len_taper_samp  &
               + fft_mem_usage (obj%fft_len, 'rtoc')
    !
    call pc_put_control ('need_request', .true.)        ! default false
    call pc_put_control ('need_label',   .true.)        ! default false
    call pc_put_control ('twosets',      .true.)        ! default false
    call pc_put_control ('nscratch',     nscratch)      ! default 0
    call pc_put_control ('nstore',       nstore)        ! default 0

    !!--------------------- prepare for execution --------------------------!!
    !!--------------------- prepare for execution --------------------------!!
    !!--------------------- prepare for execution --------------------------!!

    call mem_free (obj%tr)
    call mem_free (obj%hd)
    !
    obj%ntr_inp   = 0
    obj%ntr_out   = 0
    obj%ntr_seq   = 0
    obj%ntr_count = 0
    obj%phase_valid = .false.

!<execute_only>

    if (pc_do_not_process_traces()) return

    call mem_alloc (obj%hd,     obj%nwih,   1)
    call mem_alloc (obj%tr,     obj%nf_data)
    call mem_alloc (obj%filter, obj%nf_data)
    call mem_alloc (obj%taper,  obj%len_taper_samp)

    if (pc_do_not_process_traces()) return   ! in case of allocation errors.
    !
    ! - Initialize header sequencing
    !
    call sequence_clear (obj%seq_obj)
    !
    ! - Adjust frequency limits onto frequency grid
    !
    obj%freq_unwrap_beg_samp = nint (obj%freq_unwrap_beg / obj%df_data) + 1
    obj%freq_unwrap_end_samp = nint (obj%freq_unwrap_end / obj%df_data) + 1
    !
    obj%freq_fit_beg_samp    = max (nint (obj%freq_fit_beg / obj%df_data) + 1, &
                                    obj%freq_unwrap_beg_samp)
    obj%freq_fit_end_samp    = min (nint (obj%freq_fit_end / obj%df_data) + 1, &
                                    obj%freq_unwrap_end_samp)
    !
    ! - calculate filter in frequency domain,
    ! - to adjust phase spectrum for non-zero TSTRT
    !
!print'(" aa3 tim spct_update p=",i2," t=",i8,1x,g12.6,1x,g12.6,1x,g12.6)', &
!obj%ipn, obj%tim_num, obj%tim_beg, obj%tim_end, obj%tim_inc
!print'(" aa3 t0d spct_update p=",i2," t0_data=",g12.6)', obj%ipn, -obj%t0_data 
    !
    if (obj%t0_data == 0.0) then
      obj%filter = cmplx (1.0, 0.0)
    else
      fact = -obj%t0_data * obj%dw_data
      do i = 1, obj%nf_data
        obj%filter (i) = exp (cmplx (0.0, fact * real (i - 1)))
      end do
    end if
    !
    ! - Compensate for FFT scaling (to match previous Cray amplitudes)
    !
    obj%filter = obj%filter * 2.0
    !
    ! - calculate cosine taper in time domain
    !
    fact = 0.5 * PI / (obj%len_taper_samp + 1)
    !
    if (obj%len_taper_samp > 1) then
      do i = 1, obj%len_taper_samp
        obj%taper (i) = sin (fact * i) ** 2         ! Cosine taper function
      end do
    else
      obj%taper (1) = 1.0
    end if
    !
    call headsave_create ( obj%h, 'spct', obj%nwih, i_err )
    !
    ! - set up work array with sine-cosine tables
    ! - for the real to complex fft
    !
    i_err = fft_create (obj       = obj%fft_rc,    &
                        sign      = -1,            &
                        size      = obj%fft_len,   &
                        ctype     = 'rtoc',        &
                        opt_scale = 1.0)

    !
    ! - set up work array with sine-cosine tables
    ! - for the complex to real fft
    ! - note the scaling should be 1 / fft_len
    ! - but earlier scaling on filter changes this
    !
    i_err = fft_create (obj       = obj%fft_cr,    &
                        sign      = +1,            &
                        size      = obj%fft_len,   &
                        ctype     = 'ctor',        &
                        opt_scale = 0.5/obj%fft_len)

    obj%skip_wrapup = .false.         ! needed for the wrapup routine.
!                       CALLING SEQUENCE
!
! To create and delete an 1-D FFT object:
!                    o         i     i     i      opt       opt
!    i_err =  fft_create (obj, sign, size, ctype, opt_scale,opt_stdo)
!            - OR for F77 access -
!     ui   =  fft_new( sign, size, ctype )
!
!    ui  .... is a unique integer returned by the fft_new call
!    obj .... is a pointer to type(fft_struct)
!    ctype .. is one of the following strings
!            'ctoc' = complex to complex fft
!            'rtoc' = real to complex fft
!            'ctor' = complex to real fft
!    sign ... is a negative or positive integer. This is the sign
!             used in the complex exponential for the fourier
!             transform.
!    size ... can be a mixed radix number (a factor of small primes)
!            size >= 2
!    opt_scale. Optional argument to apply a user defined scaling to
!             the output buffer when the fft is applied.
!    opt_stdo.Set unit number for messages to standard output
!                   i
!    call fft_delete (obj)
!            - OR for F77 access -
!    call fft_del (ui)


!</execute_only>


!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

    !
  end subroutine spct_update


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!


!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>
  !
  subroutine spct (obj, ntr, hdi, tri, hdo, tro)
    implicit none
    !
    ! - Arguments
    !
    type (spct_struct), intent (inout) :: obj              ! arguments
    integer           , intent (inout) :: ntr              ! arguments
    double precision  , intent (inout) :: hdi (:, :)       ! arguments
    real              , intent (inout) :: tri (:, :)       ! arguments
    double precision  , intent (inout) :: hdo (:, :)       ! arguments
    real              , intent (inout) :: tro (:, :)       ! arguments
    !
    integer                            :: jtr
    integer                            :: jtr_amp ! amplitude trace index
    integer                            :: jtr_phs ! phase     trace index
    integer, save                      :: i_call = 0
    i_call = i_call + 1
    !
!print'(" top spct i=",i8," c=",i8," n=",i8)', obj%ipn, i_call, ntr
!if ( i_call .gt. 2000 ) stop
    !
    ! - Begin spct
    !
    xxif_have_input_traces : if (ntr >= 1) then
      !
      ! save input trace headers
      !
      do_input_traces : do jtr = 1 , ntr
        !
        obj%ntr_inp = obj%ntr_inp + 1
        !
        call headsave_store ( obj%h, obj%ntr_inp, 1, hdi (:obj%nwih, jtr) )
        call headsave_store ( obj%h, obj%ntr_inp, 5, hdi (:obj%nwih, jtr) )
        !
      end do do_input_traces 
      !
      obj%ntr_count = ntr
      obj%ntr_seq   = 0
      obj%phase_valid = .false.
      ntr = NEED_TRACES
      !
    end if xxif_have_input_traces 
    !
    xxif_transform_traces : &
    if (ntr == NEED_TRACES) then
      !
      xxif_freq_to_time : &
      if ( string_upper_compare ( obj%opt_transform(1:1), 'f' ) ) then
      !
      ! traces must be gathered two at a time
      !
      xxif_ntr_not_multiple_of_2 : &
      if ( mod ( obj%ntr_count, 2 ) .ne. 0 ) then
        !
        print'(" error in spct during frequency to time transform ", &
        & /, " you must enter the amplitude and phase traces together ", &
        & /, " the number of input traces per call must be a multiple of two", &
        & /," ipn=",i8," ntr_count=",i8)', &
        obj%ipn, obj%ntr_count 
        !
        ntr = FATAL_ERROR
        !
      end if xxif_ntr_not_multiple_of_2 
      !
      xxif_return_above : if (obj%ntr_seq >= obj%ntr_count) then
        !
        ! do nothing we output traces on the previous entry to spct
        ! and will return above for some more input traces
        !
      else xxif_return_above 
        !
        obj%ntr_seq = obj%ntr_seq + 2
!print'(" aa1 spct i=",i8," c=",i8," n=",i8)', obj%ipn, i_call, obj%ntr_seq 
        !
        jtr_amp = obj%ntr_seq - 1
        !
        jtr_phs = obj%ntr_seq 
        !
        hdo (:obj%nwih, 1)  = hdi (:obj%nwih, jtr_amp)
        !
        call spct_transform_ft (obj    = obj,              &
                                hd     = hdo (:, 1), &
                                tr     = tro (:, 1), &
                                tr_amp = tri (:, jtr_amp), &
                                tr_phs = tri (:, jtr_phs)  )
        !
        ! - Set headers for the output traces
        !
        hdo (:obj%nwih,       1) = hdi (:obj%nwih, jtr_amp)
        hdo (HDR_TOP_MUTE,    1) = 1.
        hdo (HDR_BOTTOM_MUTE, 1) = obj%nt_data
        !
!print'(" aa2 spct i=",i8," c=",i8," jtr=",i8)', obj%ipn, i_call, obj%num_seq
        !
        call sequence (obj = obj%seq_obj,   &
                       ntr = 1,             &
                       hd  = hdo(:,1:))
        !
!print'(" aa3 spct i=",i8," c=",i8," jtr=",i8)', obj%ipn, i_call, obj%num_seq
        !
        call lav_set_hdr (hd   = hdo (:, 1),  &
                          tr   = tro (:, 1),  &
                          ndpt = obj%nt_out )
        !
        ntr = 1
        !
!print'(" aa4 spct i=",i8," c=",i8," jtr=",i8)', obj%ipn, i_call, obj%num_seq
        !
      end if xxif_return_above 
      !
    else xxif_freq_to_time 
      !
      ! time to frequency transform
      !
      xxif_phase_valid : &
      if (obj%phase_valid) then
        !
        hdo (:obj%nwih, 1)  = obj%hd (:, 1)
        tro (:obj%nf_data, 1) = obj%tr
        obj%phase_valid = .false.
        ntr = 1
        !
      else if (obj%ntr_seq < obj%ntr_count) then
        !
        obj%ntr_seq = obj%ntr_seq + 1
        !
        call spct_transform_tf (obj    = obj,                              &
                                hd     = hdi (:obj%nwih,     obj%ntr_seq), &
                                tr     = tri (:obj%nt_data, obj%ntr_seq), &
                                tr_amp = tro (:, 1),                       &
                                tr_phs = obj%tr)
        !
        ! - Set headers for the output traces
        !
        hdo (:obj%nwih,       1) = hdi (:obj%nwih, obj%ntr_seq)
        hdo (HDR_TOP_MUTE,    1) = 1.
        hdo (HDR_BOTTOM_MUTE, 1) = obj%nf_data
        !
        ntr    = 1
        !
        call sequence (obj = obj%seq_obj,   &
                       ntr = ntr,           &
                       hd  = hdo)
        !
        call lav_set_hdr (hd   = hdo (:, 1),  &
                          tr   = tro (:, 1),  &
                          ndpt = obj%nf_data)
        !
        obj%hd = hdo (:obj%nwih, 1:1)
        !
        call sequence (obj = obj%seq_obj,   &
                       ntr = ntr,           &
                       hd  = obj%hd)
        !
        call lav_set_hdr (hd   = obj%hd (:, 1),  &
                          tr   = obj%tr,         &
                          ndpt = obj%nf_data)
        !
        obj%phase_valid = .true.
        !
      end if xxif_phase_valid 
      !
    end if xxif_freq_to_time 
    !
    end if xxif_transform_traces 
    !
    xxif_output_traces : if ( ntr >= 1 ) then
      !
      do_output_traces : do jtr = 1 , ntr
        !
        obj%ntr_out = obj%ntr_out + 1
        !
        call headsave_store ( obj%h, obj%ntr_out, 9, hdo (:obj%nwih, jtr) )
        !
      end do do_output_traces 
      !
    end if xxif_output_traces 
    !
!print'(" end spct i=",i8," c=",i8," n=",i8)', obj%ipn, i_call, ntr
    !
    xxif_all_done : if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
      !
      call headsave_print ( obj%h, 'spct', 1 )
      !
      call headsave_print ( obj%h, 'spct', 9 )
      !
      call spct_wrapup (obj)
      !
    end if xxif_all_done 
    !
    return
    !
  end subroutine spct
  !
  subroutine spct_original (obj, ntr, hdi, tri, hdo, tro)
    implicit none
    !
    ! - Arguments
    !
    type (spct_struct), intent (inout) :: obj              ! arguments
    integer           , intent (inout) :: ntr              ! arguments
    double precision  , intent (inout) :: hdi (:, :)       ! arguments
    real              , intent (inout) :: tri (:, :)       ! arguments
    double precision  , intent (inout) :: hdo (:, :)       ! arguments
    real              , intent (inout) :: tro (:, :)       ! arguments
    !
    ! - Begin spct
    !
    if (ntr >= 1) then
      !
      obj%ntr_count = ntr
      obj%ntr_seq   = 0
      obj%phase_valid = .false.
      ntr = NEED_TRACES
      !
    end if
    !
    if (ntr == NEED_TRACES) then
      !
      if (obj%phase_valid) then
        !
        hdo (:obj%nwih, 1)  = obj%hd (:, 1)
        tro (:obj%nf_data, 1) = obj%tr
        obj%phase_valid = .false.
        ntr = 1
        !
      else if (obj%ntr_seq < obj%ntr_count) then
        !
        obj%ntr_seq = obj%ntr_seq + 1
        !
        call spct_transform_tf (obj    = obj,                             &
                                hd     = hdi (:obj%nwih,    obj%ntr_seq), &
                                tr     = tri (:obj%nt_data, obj%ntr_seq), &
                                tr_amp = tro (:, 1),                      &
                                tr_phs = obj%tr)
        !
        ! - Set headers for the output traces
        !
        hdo (:obj%nwih,       1) = hdi (:obj%nwih, obj%ntr_seq)
        hdo (HDR_TOP_MUTE,    1) = 1.
        hdo (HDR_BOTTOM_MUTE, 1) = obj%nf_data
        !
        ntr    = 1
        !
        call sequence (obj = obj%seq_obj,   &
                       ntr = ntr,           &
                       hd  = hdo)
        !
        call lav_set_hdr (hd   = hdo (:, 1),  &
                          tr   = tro (:, 1),  &
                          ndpt = obj%nf_data)
        !
        obj%hd = hdo (:obj%nwih, 1:1)
        !
        call sequence (obj = obj%seq_obj,   &
                       ntr = ntr,           &
                       hd  = obj%hd)
        !
        call lav_set_hdr (hd   = obj%hd (:, 1),  &
                          tr   = obj%tr,         &
                          ndpt = obj%nf_data)
        !
        obj%phase_valid = .true.
        !
      end if
      !
    else if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
      !
      call spct_wrapup (obj)
      !
    end if
    !
    return
  end subroutine spct_original


!!--------------------------- spct_transform_tf   --------------------------!!
!!--------------------------- spct_transform_tf   --------------------------!!
!!--------------------------- spct_transform_tf   --------------------------!!

  subroutine spct_transform_tf (obj, hd, tr, tr_amp, tr_phs)
    implicit none
    !
    ! - Arguments
    !
    type (spct_struct), intent (inout) :: obj
    double precision ,  intent (inout) :: hd (:)
    real,               intent (inout) :: tr (:)
    real,               intent (out)   :: tr_amp (:)
    real,               intent (out)   :: tr_phs (:)
    !
    ! - Local variables
    !
    complex :: freq_spectrum (obj%nf_nyquist)
    !
    integer :: i
    integer :: start_1st_taper,  end_1st_taper
    integer :: start_last_taper, end_last_taper
    !
    real    :: delta, shift
    real    :: padded_input_trace (obj%fft_len)
    real    :: tr_shift (size (tr_phs))
    !real    :: tr_tmp (obj%fft_len)
    !real    :: bef_amp, aft_amp
    integer, save                      :: i_call = 0
    i_call = i_call + 1
    !
    ! - Begin
    !
    ! - window the time domain trace
    !
    start_1st_taper = max (a1 = 1,         &
                           a2 = obj%first_time_samp - obj%half_len_taper_samp)
    end_1st_taper   = min (a1 = obj%nt_data,  &
                           a2 = start_1st_taper + obj%len_taper_samp - 1)
    tr (start_1st_taper:end_1st_taper)                     &
      = obj%taper (1:end_1st_taper - start_1st_taper + 1)  &
        * tr (start_1st_taper:end_1st_taper)
    tr (:start_1st_taper-1) = 0.0
    !
    end_last_taper   = min (a1 = obj%nt_data, &
                            a2 = obj%last_time_samp + obj%half_len_taper_samp)
    start_last_taper = max (a1 = 1,        &
                            a2 = end_last_taper - obj%len_taper_samp + 1)
    tr (start_last_taper:end_last_taper)                          &
      = obj%taper (end_last_taper - start_last_taper + 1:1:(-1))  &
        * tr (start_last_taper:end_last_taper)
    tr (end_last_taper+1:obj%nt_data) = 0.0
    !
    padded_input_trace (1:obj%nt_data)  = tr (1:obj%nt_data)
    padded_input_trace (obj%nt_data+1:) = 0.0
    !
    ! test the rc, cr amplitude scaling
    !
    !tr_tmp = padded_input_trace 
    !
    !bef_amp = maxval(abs(tr_tmp (1:obj%nt_data)  ))
    !
    !call fft_rc_transform (obj  = obj%fft_rc,          &
    !                       bufi = tr_tmp,   &
    !                       bufo = freq_spectrum)
    !
    !call fft_cr_transform (obj  = obj%fft_cr,          &
    !                       bufi = freq_spectrum,       &
    !                       bufo = tr_tmp)
    !
    !aft_amp = maxval(abs(tr_tmp (1:obj%nt_data)  ))
    !
!print'(" spct_transform_tf p=",i2," c=",i8," bef=",g12.6," aft=",g12.6)', &
!obj%ipn, i_call, bef_amp, aft_amp
    !
    call fft_rc_transform (obj  = obj%fft_rc,          &
                           bufi = padded_input_trace,   &
                           bufo = freq_spectrum)
    !
    ! - phase shift to correct for  non-zero tstrt
    !
    freq_spectrum (:obj%nf_data) = obj%filter (:obj%nf_data)   &
                                 * freq_spectrum(:obj%nf_data)
    !
    do i = 1, obj%nf_data
      !
      ! - values in amplitude spectrum trace
      !
      tr_amp (i) = abs( freq_spectrum (i))
      !
      ! - values in phase spectrum trace
      !
      if (tr_amp (i) /= 0.) then
        tr_phs (i) = atan2(aimag(freq_spectrum(i)), real(freq_spectrum(i)))
      else
        tr_phs (i) = 0.
      end if
    end do
    !
    ! - unwrap the phase
    !
    if ((obj%opt_phase == 'UNWRAP ')    &
        .or. (obj%opt_phase == 'FLATTEN')) then
      !
      tr_shift = 0.0
      shift    = 0.0
      !
      do i = obj%freq_unwrap_beg_samp + 1, obj%freq_unwrap_end_samp
        !
        delta = tr_phs (i) - tr_phs (i - 1)
        !
        if (delta < (-PI)) then
          shift = shift + TWOPI
        else if (delta > PI) then
          shift = shift - TWOPI
        end if
        !
        tr_shift (i) = shift
        !
      end do
      !
      tr_phs = tr_phs + tr_shift
      !
      if (obj%opt_phase == 'FLATTEN') then
        !
        call spct_flatten (obj    = obj,     &
                           hd     = hd,      &
                           tr_amp = tr_amp,  &
                           tr_phs = tr_phs)
        !
      end if
    end if
    !
    ! - convert phase to degrees
    !
    tr_phs (:obj%nf_data) = DEGREES_PER_RADIAN * tr_phs (:obj%nf_data)
    !
  end subroutine spct_transform_tf 

!!--------------------------- spct_transform_ft   --------------------------!!
!!--------------------------- spct_transform_ft   --------------------------!!
!!--------------------------- spct_transform_ft   --------------------------!!

  subroutine spct_transform_ft (obj, hd, tr, tr_amp, tr_phs)
    implicit none
    !
    ! - Arguments
    !
    type (spct_struct), intent (inout) :: obj
    double precision ,  intent (inout) :: hd (:)
    real,               intent (inout) :: tr (:)
    real,               intent (inout)   :: tr_amp (:)
    real,               intent (inout)   :: tr_phs (:)
    !
    ! - Local variables
    !
    complex :: freq_spectrum (obj%nf_nyquist)
    !
    integer :: i
    !
    real    :: padded_output_trace (obj%fft_len)
    !
    ! - Begin
    !
    ! - combine the amplitude and phase traces into freq_spectrum
    !
    freq_spectrum = cmplx ( 0., 0. )
    !
    do_amp_phs : do i = 1, obj%nf_data
      !
      ! - values in amplitude spectrum trace
      ! - convert phase to radians
      !
      freq_spectrum (i) = tr_amp (i) &
* cmplx ( cos ( RADIANS_PER_DEGREE * tr_phs (i) ), &
          sin ( RADIANS_PER_DEGREE * tr_phs (i) ) )
      !
    end do do_amp_phs 
    !
    ! - phase shift to correct for  non-zero tstrt
    !
    freq_spectrum (:obj%nf_data) = conjg ( obj%filter (:obj%nf_data) )  &
                                 * freq_spectrum(:obj%nf_data)
    !
    ! - complex to real fft of freq_spectrum into padded_output_trace
    !
    call fft_cr_transform (obj  = obj%fft_cr,          &
                           bufi = freq_spectrum,       &
                           bufo = padded_output_trace)
    !
    ! - copy the time trace from padded_output_trace to tr
    !
    tr = 0.
    !
    tr (1:obj%nt_data) = padded_output_trace (1:obj%nt_data)  
    !
  end subroutine spct_transform_ft 
!</execute_only>


  !
  ! - Linear regression of unwrapped phase
  !
  subroutine spct_flatten (obj, hd, tr_amp, tr_phs)
    implicit none
    !
    ! - Arguments
    !
    type (spct_struct), intent (in)    :: obj
    double precision  , intent (inout) :: hd (:)
    real              , intent (in)    :: tr_amp (:)
    real              , intent (inout) :: tr_phs (:)
    !
    ! - Local variables
    !
    integer :: i
    real    :: phase
    real    :: shift
    real    :: slope
    real    :: sumxx
    real    :: tshift
    real    :: weight
    real    :: weight_sum
    real    :: xavg
    real    :: yavg
    real    :: yintcpt
    !
    ! - Begin spct_flatten
    !
    ! - linear regression of unwrapped phase
    !
    xavg       = 0.0
    yavg       = 0.0
    weight_sum = 0.0
    !
    do i = obj%freq_fit_beg_samp, obj%freq_fit_end_samp
      weight     = tr_amp (i)
      xavg       = xavg + weight * real (i)
      yavg       = yavg + weight * tr_phs (i)
      weight_sum = weight_sum + weight
    end do
    !
    if (weight_sum > 0.0) then
      xavg = xavg/weight_sum
      yavg = yavg/weight_sum
    end if
    !
    sumxx = 0.0
    slope = 0.0
    !
    do i = obj%freq_fit_beg_samp, obj%freq_fit_end_samp
      weight = tr_amp (i)
      sumxx = sumxx + weight * (real (i) - xavg) ** 2
      slope = slope + weight * (real (i) - xavg) * (tr_phs (i) - yavg)
    end do
    !
    if (sumxx > 0.0) slope = slope/sumxx
    !
    ! - find bulk time shift & phase
    !
    tshift = -slope/obj%dw_data
    yintcpt = yavg + slope*(1. - xavg)
    phase = mod (yintcpt, TWOPI)
    !
    if (phase <= (-PI)) then
      phase = phase + TWOPI
    else if (phase > PI) then
      phase = phase - TWOPI
    endif
    !
    shift = phase - yintcpt
    !
    do i = obj%freq_unwrap_beg_samp, obj%freq_unwrap_end_samp
      tr_phs (i) = tr_phs (i) + shift - slope * (i-1)
    end do
    !
    hd (obj%hdr_tim_shft)  = tshift
    hd (obj%hdr_ave_phase) = DEGREES_PER_RADIAN * phase
    hd (obj%hdr_tim_first) = obj%t0_data - tshift
    !
  end subroutine spct_flatten


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

  subroutine spct_wrapup (obj)
    implicit none
    !
    ! - Arguments
    !
    type (spct_struct), intent (inout) :: obj       ! arguments
    !
    ! - Begin
    !
    if (obj%skip_wrapup) return
    !
    obj%skip_wrapup = .true.
    !
    if (associated(obj%fft_rc))  call fft_delete (obj%fft_rc)
    !
  end subroutine spct_wrapup

!</execute_only>


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


end module spct_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
