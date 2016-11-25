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
! Name       : GENFILT (GENeral FILTer) [includes former FILTR, WVLT]
! Category   : filters
! Written    : 1987-10-04   by: Bob Baumel
! Revised    : 2007-01-03   by: S. Chiu
! Maturity   : production
! Purpose    : General frequency domain filter and wavelet building process.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! GENFILT is a general frequency domain filtering process that can also apply
! a variety of simple wavelets.  All GENFILT operations are performed in the
! frequency domain.  Each option is applied independently of the others; there
! is no limitation on the number of options that can be used in each call to
! the process.
!
! Normally these options will be used one at a time, although certain
! combinations may be useful (e.g., see Include a Ricker Wavelet, below).
!
!
! Trapezoid Frequency Filtering
!
! Trapezoid frequency filtering is available with ALLPASS, BANDPASS, LOWPASS,
! HIGHPASS, and BANDREJECT options. Frequency filtering in GENFILT is not
! time varying.
!
!
! Time Shift
!
! TIM_SHIFT is a bulk time shift, in ms, to be applied to the data.
! (Positive values shift data downward, negative values shift data upwards.)
!
!
! Phase Shift
!
! The PHAS_PWR and PHAS_ADD parameters each contribute constant phase shifts.
! The overall phase of the filter, in degrees, is given by the formula:
!
!            TOTAL_PHASE  =  90 * PHAS_PWR  +  PHAS_ADD
!
! These parameters are set independently from the trapezoid filtering
! options. Thus, GENFILT may apply a non-zero phase shift even when
! FILTER_TYPE = BANDREJECT -- an option which is not usually available in
! other CPS filtering processes.
!
!
! Multiply Fourier Transform by ABS(omega)**AMP_PWR
!
! This option multiplies the Fourier transform of the input traces by
! ABS(omega)**AMP_PWR.  This allows high frequencies to be enhanced or
! attenuated relative to low frequencies.  AMP_PWR = 1.0 constitutes a rho
! filter.  AMP_PWR = 0.0 has no effect on the data.
!
!
! Multiply Fourier Transform by (i*omega)**PHAS_PWR
!
! This option multiplies the Fourier transform of the input traces by
! (i*omega)**PHAS_PWR.  PHAS_PWR = 1.0 constitutes differentiation;
! PHAS_PWR = -1.0 constitutes integration.  (Fractional differentiation and
! integration are also allowed.)  PHAS_PWR = 0.0 has no effect on the data.
!
!
! Include a Gaussian Wavelet
!
! This option includes a Gaussian wavelet in the filter, where the wavelet is
! given by
!          exp(-0.5*(SIGMA*omega)**2) in the frequency domain and by
!          exp(-0.5*(t/SIGMA)**2)     in the time domain.
!
! SIGMA, in ms, is the standard deviation of the Gaussian in the time domain.
! If SIGMA = 0.0, then the Gaussian wavelet is not applied.
!
! (If defaults are not taken on parameters other than SIGMA, the wavelet
! applied may not be Gaussian in shape and SIGMA may not be its exact standard
! deviation. See additional information below.)
!
!
! Include a Ricker Wavelet
!
! GENFILT applies a Ricker Wavelet if SIGMA > 0.0 and AMP_PWR = 2.0 and
! defaults are taken on all other parameters.
!
! A "Generalized Ricker Wavelet" is applied if SIGMA > 0.0 and AMP_PWR is set
! to a value greater than zero but not equal to 2.0 and defaults are taken on
! all other parameters.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!
! The operation of GENFILT may be thought of as a filter, like the former
! FILTR, or as applying a wavelet, like the former WVLT. Both ways of thinking
! are equally valid and useful.
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
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NDPT      number of sample values in trace        used but not changed
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
! 2       Head mute index            Shifted if TIM_SHIFT /= 0.0
! 25      Largest absolute value     Reset
! 64      Tail mute index            Shifted if TIM_SHIFT /= 0.0
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                    GENFILT REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
!  9. 2007-01-03 Chiu       Pad extra zeros before FFT to avoid FFT
!                           wrap around.
!  8. 2002-02-21 Baumel     Add HDR_FLAG parameter.
!  7. 2001-04-30 Baumel     Change wrapped_up flag to skip_wrapup.
!  6. 2000-12-04 Baumel     Add Help for display-only parameters; restore
!                           FILTER_TYPE = BANDREJECT option; eliminate
!                           re-muting after filtering; change PHASE parameter
!                           to PHAS_ADD (handled independently from trapezoid
!                           filter parameters); modify logic to allow
!                           bandps_check call in GUI updates.
!  5. 2000-09-19 O'Brien    Removed FILTER_TYPE == NONE and
!                             FILTER_TYPE == BANDREJECT options
!  4. 2000-08-31 O'Brien    Remove bandps_check call from GUI updates
!  3. 2000-08-10 O'Brien    Implement FFT scaling
!                           Changed FILTER_TYPE default to ALLPASS
!                           Changed NSTORE to represent REAL words
!  2. 2000-05-17 O'Brien    Allow overall phase of wavelet to be properly
!                           reported to the GUI
!  1. 2000-05-16 O'Brien    Combination of FILTR and WVLT with absence of
!                           FILTR's filter from file option.
!
!
!                      FILTR REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
! 13. 1998-11-13 Vunderink  Begin using the f90 compiler.
! 12. 1990-12-07 Baumel     Replace FILTGS call by FILTRGS for OPT=2
!                           to fix occasional bombs in FILTGS.
! 11. 1990-04-25 Baumel     Fix STRINI call (add 'LTR' arg) for OPT=2.
! 10. 1990-03-07 Peterson   Zero array where OPT=2 operator is read in.
!  9. 1989-05-03 Baumel     Some cleanup after M Howard's STROT mod.
!  8. 1989-01-19 Howard     New DTRIN & DTROT Mod
!  7. 1988-09-26 Ball       NWIH and NWPT Conversion
!  6. 1988-08-17 Baumel     Match new form of TVFBPS primitive.
!  5. 1988-08-02 Baumel     Write to history file with NCODE.
!  4. 1988-06-02 Baumel     New convention for mute header word.
!  3. 1988-04-23 Baumel     Add CPSPRT calls.
!  2. 1988-03-09 Baumel     Added NOTCH filter capability.
!  1. 1987-10-04 Baumel     Original working version.
!
!
!                      WVLT REVISION HISTORY
!
!    Date     Author        Description
!    ----     ------        -----------
! 9. 1998-11-24 Goodger     Begin using the fortran90 compiler.
! 8. 1994-01-26 Baumel      Improved internal callability (uses GETSCR);
!                           recognizes HW64 for tail mute (calls MUTEHW);
!                           some modernization of code (although still
!                           includes Pointers & Hollerith).
! 7. 1988-09-29 Peterson    NWIH AND NWPT CONVERSION.
! 6. 1988-08-17 Baumel      Match new form of TVFBPS primitive.
! 5. 1988-08-02 Baumel      Write to history file with NCODE.
! 4. 1988-06-02 Baumel      New conventions for mute header word.
! 3. 1988-03-09 Baumel      Add NOTCH filter capability.
! 2. 1987-10-04 Baumel      Add TSHIFT parameter, set ISAVE=1.
! 1. 1987-02-03 Baumel      Original working version.
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
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                            PROGRAMMING NOTES
!
! For clarity, to facilitate maintenance, and to maintain consistency with
! variable names in other filtering processes, the following user_parameter
! to program_variable_name translation is used:
!
!        USER_PARAMETER       PROGRAM_VARIABLE_NAME
!        ----------------     ---------------------
!        FILTER_TYPE          ftyp
!        FREQ_LOW_NONE        f1
!        FREQ_LOW_FULL        f2
!        FREQ_HIGH_NONE       f3
!        FREQ_HIGH_FULL       f4
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  XML code for the GUI goes in this section. />
!<gui_def>
!<NS GENFILT Process/NC=80>
!
!          GENeral FILTer (includes former FILTR, WVLT)
!  General frequency domain filter and wavelet building process.
!
!   HDR_FLAG=~~~`IIIIIIIII
!
!                  Trapezoid Filtering Option
! `------------------------------------------------------------
!   FILTER_TYPE=`CCCCCCCCC
!   FREQ_LOW_NONE= `FFFFFFFFFFF   FREQ_LOW_FULL= `FFFFFFFFFFF
!   FREQ_HIGH_FULL=`FFFFFFFFFFF   FREQ_HIGH_NONE=`FFFFFFFFFFF
! `------------------------------------------------------------
!
!                   Other Filtering Options
! `------------------------------------------------------------
!   AMP_PWR=~~~~~~~`FFFFFFFFFFF   PHAS_PWR=~~~~~~`FFFFFFFFFFF
!   PHAS_ADD=~~~~~~`FFFFFFFFFFF   TIM_SHIFT=~~~~~`FFFFFFFFFFF
!   SIGMA=~~~~~~~~~`FFFFFFFFFFF
! `------------------------------------------------------------
!
!   Peak Frequency=`FFFFFFFFFFF   Overall Phase= `FFFFFFFFFFF
!
!     <PARMS PEAKFREQUENCY[PEAK_FREQ/EN]>
!     <PARMS OVERALLPHASE[TOTAL_PHASE/EN]>
!</gui_def>
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!<--  Parameter help information goes in this section. />
!
!<HelpSection>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0 - NWIH
!
! If HDR_FLAG = 0, then all traces are filtered.  Otherwise, only traces with
! a flag set in header word HDR_FLAG are filtered.
!</Help>
!
!------------------------Trapezoid Frequency Filtering--------------------------
!
!<Help KEYWORD="FILTER_TYPE">
!<Tip> Type of trapezoid frequency filter to apply (if any). </Tip>
! Default = ALLPASS
! Allowed = ALLPASS     (Pass all frequencies from 0.0 to Nyquist)
! Allowed = BANDPASS    (Pass frequencies from low taper to high taper)
! Allowed = LOWPASS     (Pass frequencies from 0.0 to high taper)
! Allowed = HIGHPASS    (Pass frequencies from low taper to Nyquist)
! Allowed = BANDREJECT  (Reject frequencies from low taper to high taper)
!
! In the FILTER_TYPE descriptions below, frequency parameters are listed in
! the required order, from low frequency to high frequency.
!
! If FILTER_TYPE=ALLPASS, then pass all frequencies from 0.0 frequency to
! Nyquist (Specification of FREQ_LOW_NONE, FREQ_LOW_FULL, FREQ_HIGH_FULL, and
! FREQ_HIGH_NONE parameters irrelevant in this case).
!
! If FILTER_TYPE=BANDPASS, then reject between 0.0 frequency and FREQ_LOW_NONE,
! pass between FREQ_LOW_FULL and FREQ_HIGH_FULL, reject between FREQ_HIGH_NONE
! and Nyquist, with linear tapers between the pass and reject regions.
!
! If FILTER_TYPE=LOWPASS, then pass between 0.0 frequency and FREQ_HIGH_FULL,
! reject between FREQ_HIGH_NONE and Nyquist, with a linear taper between pass
! and reject regions (Specification of FREQ_LOW_NONE and FREQ_LOW_FULL
! parameters irrelevant in this case).
!
! If FILTER_TYPE=HIGHPASS, then reject between 0.0 frequency and FREQ_LOW_NONE,
! pass between FREQ_LOW_FULL and Nyquist, with a linear taper between pass and
! reject regions (Specification of FREQ_HIGH_FULL and FREQ_HIGH_NONE parameters
! irrelevant in this case).
!
! If FILTER_TYPE=BANDREJECT (also known as a "NOTCH" filter), then pass
! between 0.0 frequency and FREQ_LOW_NONE, reject between FREQ_LOW_FULL and
! FREQ_HIGH_FULL, pass between FREQ_HIGH_NONE and Nyquist, with linear tapers
! between the pass and reject regions.
!</Help>
!
!<Help KEYWORD="FREQ_LOW_NONE">
!<Tip> Frequency where low frequency taper passes nothing. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=0.0
! For more information, see help for the FILTER_TYPE parameter.
!</Help>
!
!<Help KEYWORD="FREQ_LOW_FULL">
!<Tip> Frequency where low frequency taper passes full amplitude. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=0.0
! For more information, see help for the FILTER_TYPE parameter.
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_FULL">
!<Tip> Frequency where high frequency taper passes full amplitude. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=0.0
! For more information, see help for the FILTER_TYPE parameter.
!</Help>
!
!<Help KEYWORD="FREQ_HIGH_NONE">
!<Tip> Frequency where high frequency taper passes nothing. </Tip>
! Default =  -
! Allowed = Nyquist>=real>=0.0
! For more information, see help for the FILTER_TYPE parameter.
!</Help>
!
!-------------------------Other Filtering Options---------------------------
!
!<Help KEYWORD="AMP_PWR">
!<Tip> Multiply the Fourier transform by ABS(omega)**AMP_PWR. </Tip>
! Default = 0.0
! Allowed = real
! This option multiplies the Fourier transform of the input traces by
! ABS(omega)**AMP_PWR.  This allows high frequencies to be enhanced or
! attenuated relative to low frequencies.  AMP_PWR = 1.0 constitutes a rho
! filter.  AMP_PWR = 0.0 has no effect on the data.
!
! A Ricker wavelet is applied if AMP_PWR = 2.0 and SIGMA > 0.0.
!</Help>
!
!<Help KEYWORD="PHAS_PWR">
!<Tip> Multiply the Fourier transform by (i*omega)**PHAS_PWR. </Tip>
! Default = 0.0
! Allowed = real
! This option multiplies the Fourier transform of the input traces by
! (i*omega)**PHAS_PWR.  PHAS_PWR = 1.0 constitutes differentiation;
! PHAS_PWR = -1.0 constitutes integration.  (Fractional differentiation and
! integration are also allowed.)  PHAS_PWR = 0.0 has no effect on the data.
!
! Setting PHAS_PWR /= 0.0 causes a phase shift of 90 * PHAS_PWR degrees. The
! overall phase of the filter, in degrees, is determined by both PHAS_PWR
! and PHAS_ADD according to the formula:
!            TOTAL_PHASE  =  90 * PHAS_PWR  +  PHAS_ADD
!</Help>
!
!<Help KEYWORD="PHAS_ADD">
!<Tip> Constant phase, in degrees, to be added to the phase spectrum. </Tip>
! Default = 0.0
! Allowed = real
! This constant phase, in degrees is added to the phase otherwise determined
! by the PHAS_PWR parameter. The overall phase of the filter, in degrees, is
! given by the formula:
!            TOTAL_PHASE  =  90 * PHAS_PWR  +  PHAS_ADD
!</Help>
!
!<Help KEYWORD="TIM_SHIFT">
!<Tip> Bulk time shift, in ms, to be applied to the data. </Tip>
! Default = 0.0
! Allowed = real
! Positive values of TIM_SHIFT shift the data downwards, negative values shift
! it upwards.
!</Help>
!
!<Help KEYWORD="SIGMA">
!<Tip> Include a Gaussian wavelet of standard deviation SIGMA (in ms). </Tip>
! Default = 0.0
! Allowed = real >= 0.0
! This option includes a Gaussian wavelet in the filter, where the wavelet is
! given by
!             exp(-0.5*(SIGMA*omega)**2) in the frequency domain and by
!             exp(-0.5*(t/SIGMA)**2)     in the time domain.
!
! SIGMA, in ms, is the standard deviation of the Gaussian in the time domain.
! If SIGMA = 0.0, then the Gaussian wavelet is not applied.
!
! A Ricker wavelet is applied if SIGMA > 0.0 and AMP_PWR = 2.0 while defaults
! are taken on the other parameters.
!
! A "Generalized Ricker Wavelet" is applied if SIGMA > 0.0 and AMP_PWR is set
! to a value greater than zero but not equal to 2.0.
!</Help>
!
!<Help KEYWORD="PEAK_FREQ" TYPE="DISPLAY_ONLY">
!<Tip> Peak frequency in Hz of Ricker (or Generalized Ricker) wavelet. </Tip>
! Display-only parameter showing the frequency, in Hz, at which maximum
! amplitude is attained by the operator defined by the SIGMA, AMP_PWR, and
! PHAS_PWR parameters. This field is left blank when the operator would take
! its maximum amplitude at infinite frequency.
!
! Note: The calculation of PEAK_FREQ does *not* account for the effects of
! any trapezoid filter that may be present. Thus (for example), the displayed
! PEAK_FREQ might even be greater than Nyquist.
!</Help>
!
!<Help KEYWORD="TOTAL_PHASE" TYPE="DISPLAY_ONLY">
!<Tip> Overall phase of the operator, in degrees. </Tip>
! Display-only parameter showing the overall phase of the operator, in
! degrees. This is determined by the PHAS_PWR and PHAS_ADD parameters
! according to the formula:
!            TOTAL_PHASE  =  90 * PHAS_PWR  +  PHAS_ADD
!</Help>
!
!</HelpSection>

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module genfilt_module

      use fft_module
      use bandps_module
      use pc_module
      use sizeof_module
      use named_constants_module
      use mutehw_module
      use lav_module
      use mem_module

      implicit none

      private
      public :: genfilt_create
      public :: genfilt_initialize
      public :: genfilt_update
      public :: genfilt_delete

!<execute_only>

      public :: genfilt
      public :: genfilt_wrapup

!</execute_only>

      character(len=100),public,save :: GENFILT_IDENT = &
'$Id: genfilt.f90,v 1.9 2007/01/03 14:01:39 Chiu prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: genfilt_struct

        private
        logical                  :: skip_wrapup ! for wrapup routine

        integer                  :: hdr_flag    ! process parameters
        character(len=10)        :: ftyp        ! process parameters
        real                     :: f1          ! process parameters
        real                     :: f2          ! process parameters
        real                     :: f3          ! process parameters
        real                     :: f4          ! process parameters
        real                     :: amp_pwr     ! process parameters
        real                     :: phas_pwr    ! process parameters
        real                     :: phas_add    ! process parameters
        real                     :: tim_shift   ! process parameters
        real                     :: sigma       ! process parameters

        integer                  :: ndpt, nwih  ! globals.
        real                     :: dt          ! globals

        integer                  :: npo2        ! Power of 2 used for ffts
        integer                  :: nw          ! NumSmp in freq domain
        real                     :: df          ! SmpInt in freq domain (Hz)
        real                     :: dw          ! SmpInt in freq domain (rad)
        real                     :: fnyq        ! Nyquist frequency
        real                     :: peak_freq   ! Wavelet's peak frequency
        real                     :: total_phase ! Wavelet's total phase
        integer                  :: smp_shift   ! tim_shift in samples
        complex         ,pointer :: filter(:)   ! Complex filter array
        type(fft_struct),pointer :: rcfft       ! Forward fft object
        type(fft_struct),pointer :: crfft       ! Inverse fft object

      end type genfilt_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(genfilt_struct),pointer,save :: object      ! needed for traps.

      integer,parameter      :: ftyp_nopt=5             ! used in setup
      character(len=10),save :: ftyp_options(ftyp_nopt) ! used in traps

      data ftyp_options /'ALLPASS', 'BANDPASS', 'LOWPASS', 'HIGHPASS', &
                         'BANDREJECT'/

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine genfilt_create (obj)
      implicit none
      type(genfilt_struct),pointer :: obj       ! arguments

! Allocate the structure
      allocate (obj)

! Nullify all pointers
      nullify (obj%filter)
      nullify (obj%rcfft)
      nullify (obj%crfft)

      call genfilt_initialize (obj)

      return
      end subroutine genfilt_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine genfilt_delete (obj)
      implicit none
      type(genfilt_struct),pointer :: obj       ! arguments

!<execute_only>
      call genfilt_wrapup (obj)
!</execute_only>

      call mem_free (obj%filter)
      if (associated(obj%rcfft))  call fft_delete (obj%rcfft)
      if (associated(obj%crfft))  call fft_delete (obj%crfft)

      deallocate(obj)

      return
      end subroutine genfilt_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine genfilt_initialize (obj)
      implicit none
      type(genfilt_struct),intent(inout) :: obj       ! arguments

      obj%hdr_flag = 0
      obj%ftyp = 'ALLPASS'
      obj%f1   = FNIL
      obj%f2   = FNIL
      obj%f3   = FNIL
      obj%f4   = FNIL
      obj%amp_pwr   = 0.0
      obj%phas_pwr  = 0.0
      obj%phas_add  = 0.0
      obj%tim_shift = 0.0
      obj%sigma     = 0.0

      call genfilt_update (obj)

      return
      end subroutine genfilt_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine genfilt_update (obj)
      implicit none
      type(genfilt_struct),intent(inout),target :: obj      ! Arguments

! Local variables
      integer            :: nstore, nscratch, ier1, ier2
      logical            :: f1_sens, f2_sens, f3_sens, f4_sens
      integer            :: result, iw
      character(len=120) :: message
      complex            :: exponent, phfact
      real               :: gaus_factor, time_factor, power, angle
      integer            :: SIZEOF_REAL, SIZEOF_DOUBLE, SIZEOF_COMPLEX

      object => obj                   ! needed for traps.

      obj%skip_wrapup = .true.
      SIZEOF_REAL     = sizeof(1.0)
      SIZEOF_DOUBLE   = sizeof(1.0d0)
      SIZEOF_COMPLEX  = sizeof(cmplx(1.0,1.0))

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

! First retrieve globals
      call pc_get_global ('NDPT', obj%ndpt)
      call pc_get_global ('NWIH', obj%nwih)
      call pc_get_global ('DT'  , obj%dt)
! Find Nyquist frequency
      obj%fnyq = 0.5 / obj%dt

! Now retrieve user paramerters
      call pc_get ('HDR_FLAG'      , obj%hdr_flag)
      call pc_get ('FILTER_TYPE'   , obj%ftyp)
      call pc_get ('FREQ_LOW_NONE' , obj%f1)
      call pc_get ('FREQ_LOW_FULL' , obj%f2)
      call pc_get ('FREQ_HIGH_FULL', obj%f3)
      call pc_get ('FREQ_HIGH_NONE', obj%f4)
      call pc_get ('AMP_PWR'       , obj%amp_pwr)
      call pc_get ('PHAS_PWR'      , obj%phas_pwr)
      call pc_get ('PHAS_ADD'      , obj%phas_add)
      call pc_get ('TIM_SHIFT'     , obj%tim_shift)
      call pc_get ('SIGMA'         , obj%sigma)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

! Check HDR_FLAG
      if (obj%hdr_flag < 0 .or. obj%hdr_flag > obj%nwih) then
        call pc_info('HDR_FLAG out of range; reset to default = 0')
        obj%hdr_flag = 0
      endif

! Check trapezoid filter parameters
      call bandps_check (result, message, obj%fnyq, obj%ftyp, &
                         obj%f1, obj%f2, obj%f3, obj%f4)
      if  (obj%ftyp == 'ALLPASS' .or. obj%ftyp == 'BANDPASS' .or. &
           obj%ftyp == 'LOWPASS' .or. obj%ftyp == 'HIGHPASS' .or. &
           obj%ftyp == 'BANDREJECT') then
        select case (result)
        case (BANDPS_INFO)
          call pc_info (message)
        case (BANDPS_ERROR)
          call pc_error (message)
        case (BANDPS_ENDERROR)
          if (pc_get_update_state() /= PC_GUI) call pc_error (message)
        end select
      else if (obj%ftyp == 'NONE') then
        obj%ftyp= 'ALLPASS'
        call pc_info ("FILTER_TYPE = NONE isn't allowed for this process; &
                      &changed to ALLPASS.")
        if (result == BANDPS_INFO) then
          call pc_info ("Frequency limits irrelevant when FILTER_TYPE = &
                        &ALLPASS; values cleared.")
        end if
      else
        call pc_error ("FILTER_TYPE must be ALLPASS, BANDPASS, LOWPASS, &
                       &HIGHPASS, or BANDREJECT.")
      end if

      call bandps_sensitive (obj%ftyp, f1_sens, f2_sens, f3_sens, f4_sens)
      call pc_put_sensitive_field_flag ('FREQ_LOW_NONE'  , f1_sens)
      call pc_put_sensitive_field_flag ('FREQ_LOW_FULL'  , f2_sens)
      call pc_put_sensitive_field_flag ('FREQ_HIGH_FULL' , f3_sens)
      call pc_put_sensitive_field_flag ('FREQ_HIGH_NONE' , f4_sens)

! Check SIGMA parameter
      if (obj%sigma < 0.0) then
        obj%sigma = 0.0
        call pc_error ("SIGMA must not be negative; resetting to 0.0.")
      end if

! Get the fft size
      obj%npo2 = 8
      do while ( obj%npo2 < nint(obj%ndpt*1.3) )
        obj%npo2 = obj%npo2 * 2
      enddo
! Set nw, df, dw for frequency domain filters.
      obj%nw = obj%npo2/2 + 1
      obj%df = 1.0 / (obj%npo2 * obj%dt)
      obj%dw = (obj%df * 2.0) * PI

! Determine peak frequency and overall phase
      power = obj%amp_pwr + obj%phas_pwr
      if ( obj%sigma > 0.0 ) then
        obj%peak_freq = 1000.0 * sqrt(max(power,0.0)) / (PI*2.0*obj%sigma)
      else if (power < 0.0) then
        obj%peak_freq = 0.0
      else
        obj%peak_freq = FNIL
      end if
      obj%total_phase = 90.0*obj%phas_pwr + obj%phas_add

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field &
             ('FILTER_TYPE', ftyp_options, ftyp_nopt)

      call pc_put ('HDR_FLAG'      , obj%hdr_flag )
      call pc_put ('FILTER_TYPE'   , obj%ftyp     )
      call pc_put ('FREQ_LOW_NONE' , obj%f1       )
      call pc_put ('FREQ_LOW_FULL' , obj%f2       )
      call pc_put ('FREQ_HIGH_FULL', obj%f3       )
      call pc_put ('FREQ_HIGH_NONE', obj%f4       )
      call pc_put ('AMP_PWR'       , obj%amp_pwr  )
      call pc_put ('PHAS_PWR'      , obj%phas_pwr )
      call pc_put ('PHAS_ADD'      , obj%phas_add )
      call pc_put ('TIM_SHIFT'     , obj%tim_shift)
      call pc_put ('SIGMA'         , obj%sigma    )

! These are display-only parameters
      call pc_put ('PEAK_FREQ'     , obj%peak_freq )
      call pc_put ('TOTAL_PHASE'   , obj%total_phase)

! Determine memory usage
      nstore = SIZEOF_REAL &
          * (fft_mem_usage(obj%npo2,'rtoc') + fft_mem_usage(obj%npo2,'ctor'))
      nstore = nstore + obj%nw*SIZEOF_COMPLEX
      nstore = nstore / SIZEOF_REAL
      call pc_put_control ('NSTORE', nstore)

      nscratch = obj%npo2*SIZEOF_REAL + obj%nw*SIZEOF_COMPLEX
      nscratch  = nscratch / SIZEOF_REAL
      call pc_put_control ('NSCRATCH', nscratch)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

      call mem_free (obj%filter)
      if (associated(obj%rcfft))  call fft_delete (obj%rcfft)
      if (associated(obj%crfft))  call fft_delete (obj%crfft)

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

! Create forward and inverse fft objects
      ier1 = fft_create (obj%rcfft, -1, obj%npo2, 'rtoc', &
                         opt_scale=1.0/real(obj%npo2))
      ier2 = fft_create (obj%crfft, 1, obj%npo2, 'ctor')
      if (ier1 /= 0 .or. ier2 /= 0) then
        call pc_error ("Error creating FFT object.")
      end if

! Allocate the filter array carried by genfilt_struct
      call mem_alloc (obj%filter, obj%nw)

      if (pc_do_not_process_traces()) return ! In case of allocation errors


! smp_shift is used for shifting the mute header words
      obj%smp_shift = nint(obj%tim_shift/(1000.0*obj%dt))

! Precompute the basic filter
      call bandps ( obj%filter, obj%nw, obj%df, obj%ftyp,  &
                    obj%f1, obj%f2, obj%f3, obj%f4 )

! Modify the filter according to user parameters

      if (obj%total_phase /= 0.0) then
        angle = RADIANS_PER_DEGREE * obj%total_phase
        phfact = cmplx (cos(angle), sin(angle))
        obj%filter = phfact * obj%filter
      end if

      gaus_factor = (-0.5) * (obj%sigma*obj%dw/1000.0)**2
      time_factor = -obj%tim_shift*obj%dw/1000.0
      if ( gaus_factor /= 0.0 .or. time_factor /= 0.0 ) then
        do iw=2,obj%nw
          exponent=cmplx(gaus_factor*(iw-1)**2,time_factor*(iw-1))
          obj%filter(iw) = obj%filter(iw) * exp(exponent)
        enddo
      end if

      if (power /= 0.0)  then
        do iw=2,obj%nw
          obj%filter(iw) = obj%filter(iw) * ((iw-1)*obj%dw)**power
        enddo
        obj%filter(1) = cmplx(0.0,0.0)
      end if

      obj%filter(1)      = cmplx (real(obj%filter(1))     , 0.0)
      obj%filter(obj%nw) = cmplx (real(obj%filter(obj%nw)), 0.0)

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine genfilt_update

!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine genfilt (obj,ntr,hd,tr)
      implicit none
      type(genfilt_struct),intent(inout) :: obj            ! arguments
      integer             ,intent(inout) :: ntr            ! arguments
      double precision    ,intent(inout) :: hd(:,:)        ! arguments
      real                ,intent(inout) :: tr(:,:)        ! arguments

      integer               :: itr                         ! local
      real                  :: rtrace(obj%npo2)            ! local
      complex               :: ctrace(obj%nw)              ! local

!----------------------------------------------------------------
! Loop over traces

      do itr = 1, ntr

        ! Check for flag and skip trace as required.
        if (obj%hdr_flag > 0) then
          if (hd(obj%hdr_flag,itr) == 0.0) cycle
        end if

        ! Certainly we should skip dead traces
        if ( hd(HDR_LAV,itr) == 0.0d0 ) cycle

        ! Fourier transform the trace to the frequency domain
        rtrace(:obj%ndpt) = tr(:obj%ndpt,itr)
        rtrace(obj%ndpt+1:) = 0.0
        call fft_rc_transform (obj%rcfft, rtrace, ctrace)

        ! Filter the trace and transform to time domain
        ctrace = ctrace * obj%filter
        call fft_cr_transform (obj%crfft, ctrace, rtrace)

        ! Copy the filtered trace back to the trace array
        tr(:obj%ndpt,itr) = rtrace(:obj%ndpt)

        ! Shift mute header words if necessary and set LAV header word
        call mutehw (hd(:,itr), tr(:,itr), obj%ndpt, &
                     real(obj%smp_shift), MUTEHW_SET)
        call lav_set_hdr (hd(:,itr), tr(:,itr), obj%ndpt)

      enddo

      return
      end subroutine genfilt

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine genfilt_wrapup (obj)
      implicit none
      type(genfilt_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine genfilt_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module genfilt_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
