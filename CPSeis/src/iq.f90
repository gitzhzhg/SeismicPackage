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
! Name       : IQ    (Inverse Q correction filter)
! Category   : filters
! Written    : 1997-04-29   by: Bob Baumel
! Revised    : 2001-03-05   by: Bob Baumel
! Maturity   : production   2001-03-30
! Purpose    : Removes amplitude and phase effects of constant Q attenuation.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! IQ performs either inverse Q correction or forward Q modeling.  The
! algorithm is described in the Algoritm Description section.  IQ uses a model
! of Q that can vary in time but does not vary laterally.  Normally this is
! not a significant restriction since Q measurements are subject to large
! uncertainty and actual variation of Q in time or space may be of the same
! order of magnitude as the measurement uncertainty.
!
!
! Gain Limitation
!
! The GAIN_MAX parameter allows the gain applied to the data in inverse Q
! correction to be limited.  If there were no limitation, the inverse Q
! correction would produce lots of noise at high frequencies and long delay
! times.   Setting GAIN_MAX to a moderate value will prevent gaining up excess
! noise at large values of trace time and frequency.  This parameter is only
! active when MODE = INVERSE.  Setting GAIN_MAX to very large values will
! likely produce excessive high frequency noise deep in the section.
!
!
! Phase-Only Q Correction
!
! Setting GAIN_MAX = 0.0 results in a Q correction of the phase only and no
! amplitude correction. In this case, it is also reasonable to set the bandpass
! parameters (FREQ_LOW_NONE, FREQ_LOW_FULL, FREQ_HIGH_FULL, FREQ_HIGH_NONE) to
! the values Zero-Zero-Nyquist-Nyquist.
!
!
! Reference for Delay Time
!
! The delay time (for attenuation calculation) can be referenced to the mute
! time (MDTFMT = YES), or to zero time (MDTFMT = NO).  Normally MDTFMT is set to
! YES for marine processing since sea water can be assumed to have essentially
! no attenuation.
!
!
! Reference
!
! Sheriff, R. E., And Geldart, L. P., 1982, Exploration Seismology (2 Volumes):
! Cambridge University Press, p. 55.
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! Execution Time and Memory
!
! The parameters FREQ_LOW_NONE, FREQ_LOW_FULL, FREQ_HIGH_FULL, FREQ_HIGH_NONE
! constitute a bandpass filter of the input data.  Your setting of these
! parameters affects both execution time and memory usage, as it determines the
! range of the frequency domain integration and the number of K(t,f) values that
! must be saved.  Normally, FREQ_LOW_NONE and FREQ_LOW_FULL are both kept at
! their defaults of zero, but you may limit execution time and memory usage by
! setting FREQ_HIGH_FULL and FREQ_HIGH_NONE lower than Nyquist (just so they
! include the bandwidth of your data).
!
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
! This process alters input traces.
! This process outputs the same traces as it receives (altered).
!
! This process outputs traces with same gather status as the input traces.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
!
! Name     Description                           Action taken
! ----     -----------                           ------------
! NDPT     number of sample values in trace      used but not changed
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
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
!  2      Head mute                  Used but not changed
! 25      LAV                        Set
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
! 16. 2001-03-30  Baumel     Add control parameters for parallel operation.
! 15. 2000-12-07  Baumel     Change wrapped_up flag to skip_wrapup.
! 14. 2000-11-29  Baumel     Several bug fixes (scaling, frequency counting);
!                            Remove defaults of 0.0 for FREQ_HIGH_FULL and
!                            FREQ_HIGH_NONE; Remove hard limit of 25 for number
!                            of TIMES, Q_VALS pairs; Avoid killing trace values
!                            above mute time when MDTFMT = YES (This is changed
!                            from behavior of the Cray version).
! 13. 2000-07-14  Coleman    Replace confusing help documentation.
! 12. 2000-06-19  Coleman    Replace complex constant that caused PGI warning.
! 11. 2000-06-14  Coleman    Use PI from named_constant module; replaced
!                            bandpass primative with bandps; replaced parameters
!                            FREQ_BEG, FREQ_END, TAPER_LO, & TAPER_HI with
!                            FREQ_LOW_NONE, FREQ_LOW_FULL, FREQ_HIGH_FULL, &
!                            FREQ_HIGH_NONE; eliminated parameter OPT_TAPER;
!                            reworked the gui for the the parameter changes; and
!                            added calls the set the min and max sizes of the
!                            arrays and removed the dimension checks.
! 10. 2000-05-16  Coleman    Set header word 25 to LAV,
!  9. 2000-04-17  Coleman    Added RCS Ident string and fixed gui_def.
!  8. 2000-03-31  Coleman    Converted to new CPS system.
!  7. 1998-11-13  Vunderink  Begin using the f90 compiler.
!  6. 1998-02-05  Baumel     Moved from newlib to conlib.
!  5. 1997-10-15  Baumel     Allow depth-dependent Q (TIMES,QVALS).
!  4. 1997-05-27  Baumel     Add FRMUT option.
!  3. 1997-05-01  Baumel     Fix to work with non-zero TSTRT global.
!  2. 1997-04-30  Baumel     Minor change in algorithm to improve speed:
!                            Now does FFT time->freq, then slow transform
!                            freq->time; previously did slow transform
!                            time->freq, then FFT freq->time.
!  1. 1997-04-29  Baumel     Original working version.
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
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
!
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
! IQ first does an FFT to transform each trace to the frequency domain
! x(t) -> X(f); then it does a "slow" transform to produce the final time
! domain ouput trace y(t):
!                           /
!                    y(t) = | K(t,f)*X(f) df
!                           /
!    where the kernel K(t,f) is given by:
!
!    K(t,f) = exp[ pi*f*t/Q - (2*i*f*t/Q)*ln(f/FREF) + 2*pi*i*f*t]
!
! (This is shown for inverse Q filtering (MODE = INVERSE).  Forward Q modeling
! (MODE = FORWARD) corresponds to reversing the sign of Q.)
!
! To limit execution time, the entire K(t,f) array is stored in memory (in
! IQ's data structure), which makes this a fairly high-memory process.
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
!
!<gui_def>
!<NS IQ Process/NC=80>
!
!                     Inverse Q correction filter Process
!       Removes amplitude and phase effects of constant Q attenuation
!
!                                       TIMES          Q_VALS
!       MODE =~~~~~~~~~~~`CCCCCCC       `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!                                       `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!       MDTFMT =~~~~~~~~~`CCC           `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!                                       `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!       FREQ_REF =~~~~~~~`FFFFFFFFF     `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!                                       `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!       GAIN_MAX =~~~~~~~`FFFFFFFFF     `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!                                       `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!       FREQ_LOW_NONE =~~`FFFFFFFFF     `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!                                       `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!       FREQ_LOW_FULL =~~`FFFFFFFFF     `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!                                       `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!       FREQ_HIGH_FULL = `FFFFFFFFF     `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!                                       `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!       FREQ_HIGH_NONE = `FFFFFFFFF     `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!                                       `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!       FREQ_NYQUIST =~~~`XXXXXXXXX     `FFFFFFFFFFFFFF`FFFFFFFFFFFFFF
!<PARMS TIMES_ARRAYSET[/XST/YST]>
!<PARMS TIMES[/R]>
!</gui_def>
!
!-------------------------------------------------------------------------------
!
!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Whether to perform inverse Q correction or forward Q modeling. </Tip>
! Default = INVERSE
! Allowed = INVERSE  (Perform inverse Q correction.)
! Allowed = FORWARD  (Perform forward Q modeling.)
!</Help>
!
!<Help KEYWORD="MDTFMT">
!<Tip> Measure Delay Time (for attenuation calculation) From Mute Time? </Tip>
! Default = YES
! Allowed = YES (Measure delay time for attenuation calculation from mute time.)
! Allowed = NO  (Measure delay time for attenuation calculation from time zero.)
! MDTFMT = YES is appropriate for deep water marine data since Q for sea water
! can, as a practical matter, be taken as infinite.
!
! If MDTFMT = NO, delay times are measured from time zero, which isn't
! necessarily the top of the trace, as the TSTRT global may be non-zero.
!</Help>
!
!<Help KEYWORD="FREQ_REF">
!<Tip> Reference frequency, in Hz, for Q correction or Q modeling. </Tip>
! Default = 50
! Allowed = real > 0.0
! The reference frequency is the frequency at which propagation velocity
! produces travel times matching the trace sample times.
!
! Frequencies below FREQ_REF travel slower, so will be delayed in forward Q
! modeling or advanced in inverse Q correction. Frequencies above FREQ_REF are
! shifted in the opposite direction.
!</Help>
!
!<Help KEYWORD="GAIN_MAX">
!<Tip> Maximum gain, in dB, for the inverse Q filter. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
! For inverse Q correction, GAIN_MAX limits the magnitude of the Q correction
! gain. In the absence of such a limit, the gain would be proportional to
! exp(F*T/Q), where F is frequency and T is delay time. Too large a value for
! GAIN_MAX allows excess noise at large values of F*T.
!
! Setting GAIN_MAX = 0.0 produces the most severe limitation of the amplitude
! gain, namely, a *phase-only* Q correction with no amplitude gain at all.
! When choosing this option, the bandpass limits (FREQ_LOW_NONE, FREQ_LOW_FULL,
! FREQ_HIGH_FULL, FREQ_HIGH_NONE) are usually set to Zero-Zero-Nyquist-Nyquist.
!
! The GAIN_MAX parameter has no effect when MODE = FORWARD.
!</Help>
!
!<Help KEYWORD="FREQ_LOW_NONE">
!<Tip> Frequency (Hz) where low frequency taper passes nothing. </Tip>
! Default = 0.0
! Allowed = real >= 0.0
!</Help>
!
!<Help KEYWORD="FREQ_LOW_FULL">
!<Tip> Frequency (Hz) where low frequency taper passes full amplitude. </Tip>
! Default = 0.0
! Allowed = real >= FREQ_LOW_NONE
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_FULL">
!<Tip> Frequency (Hz) where high frequency taper passes full amplitude. </Tip>
! Default = -
! Allowed = real >= FREQ_LOW_FULL
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_NONE">
!<Tip> Frequency (Hz) where high frequency taper passes nothing. </Tip>
! Default = -
! Allowed = Nyquist >= real >= FREQ_HIGH_FULL
!</Help>
!
!<Help KEYWORD="TIMES">
!<Tip> Delay times, in increasing order, for specifying Q values. </Tip>
! Default =  -
! Allowed = real linked array
! Entries in TIMES array need not cover the whole trace, as constant Q values
! are extrapolated before first time and after last time; thus, to represent
! depth-independent Q, only a single (TIMES, Q_VALS) pair need be specified.
! When more than one time is specified, Q values are linearly interpolated
! between the specified times.
!
! Delay times are specified in seconds, and are measured from either the mute
! time (if MDTFMT = YES) or from time zero (if MDTFMT = NO).
!</Help>
!
!<Help KEYWORD="Q_VALS">
!<Tip> Array of Q values corresponding to TIMES values. </Tip>
! Default =  -
! Allowed = real linked array
! Q values at a given time denote average Q for propagating through this much
! travel-time, starting from the reference specified by MDTFMT. Thus, these
! are NOT "interval" Q values.
!</Help>
!
!<Help KEYWORD="FREQ_NYQUIST" TYPE="DISPLAY_ONLY">
!<Tip> Nyquist frequency (Hz) displayed to help you set bandpass limits. </Tip>
! The low frequency bandpass limits (FREQ_LOW_NONE, FREQ_LOW_FULL) are usually
! kept at their defaults of zero. The high frequency limits (FREQ_HIGH_FULL,
! FREQ_HIGH_NONE) are usually set to Nyquist when performing either forward Q
! modeling (MODE = FORWARD) or phase-only inverse Q correction (GAIN_MAX = 0).
! High frequency limits should be set lower than Nyquist when performing Q
! correction with amplitude compensation (GAIN_MAX > 0) in order to limit high
! frequency noise.
!
! Note: Execution time of IQ is proportional to the frequency range from
! FREQ_LOW_NONE to FREQ_HIGH_NONE. Therefore, for all options, you can reduce
! execution time by reducing this range. Just be sure that your specified
! bandpass includes the bandwidth of your data!
!</Help>
!
!</HelpSection>
!
!-------------------------------------------------------------------------------
! NOTES FOR CONVERSION PROGRAMMER
!
!  1.  Check for efficient operation when GAIN_MAX = 0.0 as this is probably the
! most frequent value.  May need special code for this case.
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

module iq_module
    use pc_module
    use named_constants_module
    use mem_module
    use fft_module
    use bandps_module
    use mutehw_module
    use interp_module
    use lav_module

    implicit none

    private
    public :: iq_create     ! uses the parameter cache.
    public :: iq_initialize
    public :: iq_update     ! uses the parameter cache.
    public :: iq_delete

!<execute_only>

    public :: iq            ! main execution (trace processing) routine.
    public :: iq_wrapup

!</execute_only>

    character(len=100),public,save :: IQ_IDENT = &
'$Id: iq.f90,v 1.16 2001/03/30 14:13:03 sps prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

    type, public :: iq_struct
        private
        logical                     :: skip_wrapup      ! wrapup flag.
        character(len=7)            :: mode             ! process parameter
        logical                     :: mdtfmt           ! process parameter
        real                        :: freq_ref         ! process parameter
        real                        :: gain_max         ! process parameter
        real                        :: f1, f2, f3, f4   ! process parameters
        real, pointer               :: times(:)         ! process parameter
        real, pointer               :: q_vals(:)        ! process parameter

        integer                     :: ndpt             ! globals
        real                        :: dt, tstrt        ! globals

        integer                     :: nqvals           ! dependent variables
        integer                     :: npow2, nnyq      ! dependent variables
        integer                     :: ifmin, ifmax     ! dependent variables
        integer                     :: nfreq, ifshft    ! dependent variables
        real                        :: df               ! dependent variables
        complex, pointer            :: kernel(:,:)      ! dependent variables
        real, pointer               :: filter(:)        ! dependent variables
        type(fft_struct), pointer   :: fftrc            ! dependent variables
        type(fft_struct), pointer   :: fftcr            ! dependent variables

    end type iq_struct
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

    type(iq_struct),pointer,save :: object       ! needed for traps

contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


subroutine iq_create (obj)
    implicit none
    type(iq_struct), pointer :: obj

    allocate (obj)

    nullify (obj%times)
    nullify (obj%q_vals)
    nullify (obj%filter)
    nullify (obj%kernel)
    nullify (obj%fftrc)
    nullify (obj%fftcr)

    call iq_initialize (obj)

    return
end subroutine iq_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


subroutine iq_delete (obj)
    implicit none
    type(iq_struct), pointer :: obj

!<execute_only>
    call iq_wrapup (obj)
!</execute_only>

    call mem_free (obj%times)
    call mem_free (obj%q_vals)
    call mem_free (obj%filter)
    call mem_free (obj%kernel)
    if (associated(obj%fftrc)) call fft_delete (obj%fftrc)
    if (associated(obj%fftcr)) call fft_delete (obj%fftcr)

    deallocate (obj)

    return
end subroutine iq_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

subroutine iq_initialize (obj)
    implicit none
    type(iq_struct), intent(inout) :: obj

    obj%mode      = 'INVERSE'
    obj%mdtfmt    = .true.
    obj%freq_ref  =  50.0
    obj%gain_max  =   0.0
    obj%f1        =   0.0
    obj%f2        =   0.0
    obj%f3        =  FNIL
    obj%f4        =  FNIL

    obj%nqvals    =   1
    call mem_alloc (obj%times,  obj%nqvals)
    call mem_alloc (obj%q_vals, obj%nqvals)
    obj%times(1)  =   0.0
    obj%q_vals(1) = 100.0

    call iq_update (obj)

    return
end subroutine iq_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


subroutine iq_update (obj)
    implicit none
    type(iq_struct),intent(inout),target :: obj         ! argument

    real              :: fnyquist                       ! local
    integer           :: n1, n2, ierr, result           ! local
    integer           :: nscratch, nstore               ! local
    character(len=10) :: fltr_type                      ! local
    character(len=80) :: message                        ! local

    object         => obj        ! needed for traps
    obj%skip_wrapup = .true.     ! needed for the wrapup routine.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

    call pc_register_array_names('TIMES_ARRAYSET', (/ 'TIMES ', 'Q_VALS' /), 2)
    call pc_put_minsize_arrayset('TIMES_ARRAYSET', 1)

    call pc_get_global ('NDPT' , obj%ndpt )
    call pc_get_global ('DT'   , obj%dt   )
    call pc_get_global ('TSTRT', obj%tstrt)

    call pc_get ('MODE'          , obj%mode    )
    call pc_get ('MDTFMT'        , obj%mdtfmt  )
    call pc_get ('FREQ_REF'      , obj%freq_ref)
    call pc_get ('GAIN_MAX'      , obj%gain_max)
    call pc_get ('FREQ_LOW_NONE' , obj%f1      )  ! NOTE: names are different
    call pc_get ('FREQ_LOW_FULL' , obj%f2      )  ! NOTE: names are different
    call pc_get ('FREQ_HIGH_FULL', obj%f3      )  ! NOTE: names are different
    call pc_get ('FREQ_HIGH_NONE', obj%f4      )  ! NOTE: names are different

    n1 = obj%nqvals
    n2 = obj%nqvals
    call pc_alloc ('TIMES' , obj%times , n1)
    call pc_alloc ('Q_VALS', obj%q_vals, n2)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

    call pc_put_sensitive_field_flag ('GAIN_MAX', .true.)
    if (obj%mode(1:1)=='I' .or. obj%mode(1:1)=='i') then
        obj%mode = 'INVERSE'
    else if (obj%mode(1:1)=='F' .or. obj%mode(1:1)=='f') then
        obj%mode = 'FORWARD'
        call pc_put_sensitive_field_flag ('GAIN_MAX', .false.)
    else
        call pc_error ('*** IQ: Invalid value for MODE: '//obj%mode)
    end if

    if (obj%freq_ref <= 0.0) then
        call pc_error ('*** IQ: Invalid value for FREQ_REF:', obj%freq_ref)
        obj%freq_ref = 50.0
    endif

    if (obj%mode == 'FORWARD') then
        obj%gain_max = max (obj%gain_max, 0.0)
        if (pc_get_update_state() /= PC_GUI) obj%gain_max = 0.0
    else if (obj%gain_max < 0.0) then
        call pc_error ('*** IQ: Invalid value for GAIN_MAX:', obj%gain_max)
        obj%gain_max = 0.0
    endif

    if (n2 /= n1) then
        call pc_error ('TIMES and Q_VALS arrays have different lengths.')
        obj%nqvals = min (n1, n2)
    else
        obj%nqvals = n1
    endif

    fltr_type = 'BANDPASS'
    fnyquist  = 0.5 / obj%dt
    call bandps_check( result, message, fnyquist, fltr_type, &
                       obj%f1, obj%f2, obj%f3, obj%f4 )
    select case (result)
    case (BANDPS_INFO)
        call pc_info (message)
    case (BANDPS_ERROR)
        call pc_error (message)
    case (BANDPS_ENDERROR)
        if (pc_get_update_state() /= PC_GUI) call pc_error (message)
    end select

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

    call pc_put_control ('PARALLEL_SAFE'         , .true.)
    call pc_put_control ('PCPS_SEND_MODE'        , 'PCPS_SEND_FIRST_AVAIL')
    call pc_put_control ('PCPS_RECEIVE_MODE'     , 'PCPS_RECEIVE_PASSTHRU')
    call pc_put_control ('PCPS_BUNCH_MODE'       , 'PCPS_BUNCH_TRACES')
    call pc_put_control ('PCPS_SEND_EOF_MODE'    , 'PCPS_SEND_ALL_EOF')
    call pc_put_control ('PCPS_ALT_SEND_MODE'    , 'PCPS_SEND_ALL')
    call pc_put_control ('PCPS_ALT_RECEIVE_MODE' , 'PCPS_RECEIVE_ALL_EOF')

    call pc_put_options_field ('MODE', (/ 'FORWARD', 'INVERSE' /), 2)

    call pc_put ('MODE'          , obj%mode    )
    call pc_put ('MDTFMT'        , obj%mdtfmt  )
    call pc_put ('FREQ_REF'      , obj%freq_ref)
    call pc_put ('GAIN_MAX'      , obj%gain_max)
    call pc_put ('FREQ_LOW_NONE' , obj%f1      )  ! NOTE: names are different
    call pc_put ('FREQ_LOW_FULL' , obj%f2      )  ! NOTE: names are different
    call pc_put ('FREQ_HIGH_FULL', obj%f3      )  ! NOTE: names are different
    call pc_put ('FREQ_HIGH_NONE', obj%f4      )  ! NOTE: names are different
    call pc_put ('TIMES'         , obj%times   , obj%nqvals )
    call pc_put ('Q_VALS'        , obj%q_vals  , obj%nqvals )

    call pc_put_gui_only ('FREQ_NYQUIST', fnyquist)

! Determine FFT size and memory usage
    obj%npow2 = 8
    do while (obj%npow2 < obj%ndpt)
        obj%npow2 = 2 * obj%npow2
    end do
    obj%nnyq  = obj%npow2 / 2 + 1
    obj%df = 1.0 / (obj%npow2 * obj%dt)
    if (obj%f1 /= FNIL) then
      obj%ifmin = int ( obj%f1 / obj%df + 1.98 )
    else
      obj%ifmin = 1
    end if
    if (obj%f4 /= FNIL) then
      obj%ifmax = int ( obj%f4 / obj%df + 1.02 )
    else
      obj%ifmax = obj%nnyq
    end if
    obj%nfreq  = obj%ifmax - obj%ifmin + 1
    obj%ifshft = obj%ifmin - 1

    nstore   = 2 * obj%ndpt * obj%nfreq  +  obj%nnyq
    nscratch = 2 * obj%nnyq  +  obj%npow2
    call pc_put_control ('NSTORE'  , nstore)
    call pc_put_control ('NSCRATCH', nscratch)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

    call mem_free (obj%kernel)
    call mem_free (obj%filter)
    if (associated(obj%fftrc)) call fft_delete (obj%fftrc)
    if (associated(obj%fftcr)) call fft_delete (obj%fftcr)

!<execute_only>
    if (pc_do_not_process_traces()) return

    obj%skip_wrapup = .false.     ! needed for the wrapup routine.
    obj%gain_max = log(10.0) * obj%gain_max / 20.0

    call mem_alloc (obj%kernel, obj%ndpt, obj%nfreq)
    call mem_alloc (obj%filter, obj%nnyq)

    ierr = fft_create (obj%fftrc, -1, obj%npow2, 'rtoc', &
                       opt_scale=2.0/real(obj%npow2))
    if (ierr /= 0) call pc_error ('*** IQ: fft_create rtoc failed')

    if (obj%mdtfmt) then
      ierr = fft_create (obj%fftcr, +1, obj%npow2, 'ctor', opt_scale=0.5)
      if (ierr /= 0) call pc_error ('*** IQ: fft_create ctor failed')
    end if

    if (pc_do_not_process_traces()) return

    call iq_set_kernel (obj)
!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

    return
end subroutine iq_update

!!----------------------------- traps --------------------------------------!!
!!----------------------------- traps --------------------------------------!!
!!----------------------------- traps --------------------------------------!!


!!------------------------- main execution ---------------------------------!!
!!------------------------- main execution ---------------------------------!!
!!------------------------- main execution ---------------------------------!!

!<execute_only>

subroutine iq (obj, ntr, hd, tr)
    implicit none

    type(iq_struct) , intent(inout)  :: obj        ! arguments
    integer         , intent(inout)  :: ntr        ! arguments
    double precision, intent(inout)  :: hd(:,:)    ! arguments
    real            , intent(inout)  :: tr(:,:)    ! arguments

    integer        :: izero, jtr, idpt             ! local
    real           :: rtrace(obj%npow2)            ! local
    complex        :: ctrace(obj%nnyq)             ! local

    if (ntr == NEED_TRACES) return
    izero = 1
    do jtr = 1, ntr

        call mutehw (hd(:,jtr), tr(:,jtr), obj%ndpt, 0.0, MUTEHW_SET)

        if (obj%mdtfmt) then
            izero = max (nint(hd(HDR_TOP_MUTE,jtr)) - 1, 1)
        endif

        rtrace(1:obj%ndpt-izero+1) = tr(izero:obj%ndpt,jtr)
        rtrace(obj%ndpt-izero+2:obj%npow2-izero+1) = 0.0
        rtrace(obj%npow2-izero+2:obj%npow2) = tr(1:izero-1,jtr)

        call fft_rc_transform (obj%fftrc, rtrace, ctrace)

        do idpt = izero, obj%ndpt
            tr(idpt,jtr) = sum (real(obj%kernel(idpt-izero+1,:)  &
                                      * ctrace(obj%ifmin:obj%ifmax)))
        end do

        if (izero > 1) then
            ctrace = obj%filter * ctrace
            call fft_cr_transform (obj%fftcr, ctrace, rtrace)
            tr(1:izero-1,jtr) = rtrace(obj%npow2-izero+2:obj%npow2)
        end if

    end do

    if (ntr == NO_MORE_TRACES .or. ntr == FATAL_ERROR) then
        call iq_wrapup (obj)
    elseif (ntr > 0 ) then
        call lav_set_hdr (hd, tr, obj%ndpt, ntr)
    end if

    return
end subroutine iq

!!--------------------- subroutine iq_set_kernel ---------------------------!!
!!--------------------- subroutine iq_set_kernel ---------------------------!!
!!--------------------- subroutine iq_set_kernel ---------------------------!!

subroutine iq_set_kernel (obj)
    implicit none
    type(iq_struct), intent(inout)  :: obj                      ! argument

    integer           :: idir, itzero, i, im1, j                ! local
    real              :: tzero, afact0, afact1, afact2, afact3  ! local
    real              :: afactr, afacti1, afacti2, afactq       ! local
    real              :: times_interp(obj%ndpt)                 ! local
    real              :: qvals_interp(obj%ndpt)                 ! local
    character(len=10) :: fltr_type                              ! local

    if (obj%mode == 'FORWARD') then
      idir = 1
    else
      idir = -1
    end if

    fltr_type = 'BANDPASS'
    call bandps (obj%filter, obj%nnyq, obj%df, fltr_type,  &
                 obj%f1, obj%f2, obj%f3, obj%f4)

    if (obj%mdtfmt) then
        tzero = 1.0
    else
        tzero = 1.0  -  obj%tstrt / obj%dt
    endif

    itzero = min ( max ( int(tzero+0.99) , 1 ) , obj%ndpt + 1 )

    if (itzero <= obj%ndpt) then
        call interp_1d_var_lin_real (obj%times, obj%q_vals, obj%nqvals,  &
                times_interp, qvals_interp, obj%ndpt-itzero+1,           &
                (itzero-tzero)*obj%dt, (obj%ndpt-tzero)*obj%dt )
    endif

    afact0 = (2.0 * PI) / obj%npow2
    afact1 = -idir * afact0 * 0.5
    afact2 = real(2*idir) / obj%npow2
    afact3 = obj%df / obj%freq_ref

    do i = max(2,obj%ifmin), obj%ifmax
       im1     = i - 1
       afactr  = afact1 * im1
       afacti1 = (afact2 * im1) * log(afact3*im1)
       afacti2 =  afact0 * im1
       do j = 1, itzero-1
           obj%kernel(j,i-obj%ifshft) = obj%filter(i) *            &
                 exp( cmplx( 0.0, afacti2*(j-1) ) )
       end do
       do j = itzero, obj%ndpt
           afactq = (j - tzero) / qvals_interp(j-itzero+1)
           obj%kernel(j,i-obj%ifshft) = obj%filter(i) *            &
                 exp( cmplx( min( obj%gain_max, afactr*afactq ) ,  &
                             afacti1*afactq + afacti2*(j-1) ) )
       end do
    end do

    if (obj%ifmin == 1) then
        obj%kernel(:,1) = 0.5 * obj%filter(1)
    endif

    if (obj%ifmax == obj%nnyq) then
        obj%kernel(:,obj%nfreq) = real(obj%kernel(:,obj%nfreq))
    endif

    if (.not. obj%mdtfmt) call mem_free (obj%filter)
    return
end subroutine iq_set_kernel

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

subroutine iq_wrapup( obj )
    implicit none
    type(iq_struct), intent(inout) :: obj

    if( obj%skip_wrapup ) return
    obj%skip_wrapup = .true.

    return
end subroutine iq_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

end module iq_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
