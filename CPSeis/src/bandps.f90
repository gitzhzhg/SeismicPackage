!<CPS_v1 type="PRIMITIVE"/>
!!-------------------------------- bandps.f90 --------------------------------!!
!!-------------------------------- bandps.f90 --------------------------------!!
!!-------------------------------- bandps.f90 --------------------------------!!

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
!                        C P S   P R I M I T I V E
!
! Name       : BANDPS   (originally TVFBPS)
! Category   : filters
! Written    : 1986-07-05   by: Bob Baumel and Shein Wang
! Revised    : 2000-11-10   by: Bob Baumel
! Maturity   : production   2000-11-17
! Purpose    : Build a bandpass filter in the frequency domain.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!  This module includes a subroutine for creating a bandpass filter in the
!  frequency domain, as well as subroutines for checking the parameters that
!  specify the filter, thereby allowing simplification of front-end as well as
!  back-end code in each process that does bandpass filtering. The parameters
!  which specify a filter allow a choice of six "FILTER_TYPE" values, namely:
!  NONE, BANDPASS, HIGHPASS, LOWPASS, ALLPASS, and BANDREJECT.
!
!  The routine that creates the filter (the basic BANDPS call) does NOT perform
!  any FFTs. It is assumed that the calling program will apply the filter in
!  the frequency domain and/or do any required FFT to get to the time domain.
!  For the sake of uniformity in specifying bandpass filters, this primitive
!  should be called by every CPS process that applies a bandpass filter.
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
!                             CALLING SEQUENCES
!
! This module contains 3 routines: BANDPS, BANDPS_CHECK, and BANDPS_SENSITIVE
! (where BANDPS is actually an overloaded interface to allow its first argument
! to be either real or complex). The calling sequences are:
!
!
!                     o        i         i           i             i
!      call bandps (filter, num_freq, freq_inc, filter_type, freq_low_none,  &
!
!                                                          opt     opt
!                i              i               i           i       i
!          freq_low_full, freq_high_full, freq_high_none, phase, amp_fact)
!
!
! real OR complex   filter(num_freq) = Array of returned filter coefficients.
! integer           num_freq = Number of frequencies for computing filter
!                              (first coefficient is always at frequency=0.).
! real              freq_inc = Frequency increment for filter coefficients.
! character(len=*)  filter_type = Type of filter to return; legal values are:
!                        NONE - No change to amplitude or phase spectrum.
!                        BANDPASS - Pass frequencies between tapers.
!                        HIGHPASS - Pass frequencies from low taper to Nyquist.
!                        LOWPASS - Pass frequencies from 0.0 to high taper.
!                        ALLPASS - Pass all frequencies - may change phase.
!                        BANDREJECT - Reject frequencies between tapers.
!                   Note: Length of filter_type must be >= 10 characters.
! real              freq_low_none  = Frequency where low frequency taper
!                                    passes nothing.
! real              freq_low_full  = Frequency where low frequency taper
!                                    passes full amplitude.
! real              freq_high_full = Frequency where high frequency taper
!                                    passes full amplitude.
! real              freq_high_none = Frequency where high frequency taper
!                                    passes nothing.
! real,optional     phase          = Phase of filter in degrees (default 0.).
! real,optional     amp_fact       = amplitude factor to set scale of filter
!                                    (default = 1.).
!
!
!                                o        o        i           b
!           call bandps_check (result, message, fnyquist, filter_type,     &
!
!                                b              b              b
!                          freq_low_none, freq_low_full, freq_high_full,   &
!
!                                           opt
!                                b           b
!                          freq_high_none, phase)
!
!
! integer           result = Returned result code;  values (named constants):
!                      BANDPS_OK - Everything's cool - no action required.
!                      BANDPS_INFO - Need to call pc_info to display message.
!                      BANDPS_ERROR - Need to call pc_error to display message.
!                      BANDPS_ENDERROR - Must call pc_error in "end" condition.
! character(len=*)  message = message to display with pc_info or pc_error.
!                        Set length >= 80 characters in calling program.
! real              fnyquist = Nyquist frequency - needed for error checking.
! character(len=*)  filter_type    = filter_type value to be checked.
! real              freq_low_none  = freq_low_none value to be checked
! real              freq_low_full  = freq_low_full value to be checked.
! real              freq_high_full = freq_high_full value to be checked.
! real              freq_high_none = freq_high_none value to be checked.
! real,optional     phase          = phase value to be checked.
!
!
!                                      i               o
!          call bandps_sensitive (filter_type, freq_low_none_sens,     &
!
!                                 o                    o
!                         freq_low_full_sens, freq_high_full_sens,     &
!
!                                                 opt
!                                  o               o
!                         freq_high_none_sens, phase_sens)
!
!
! character(len=*) filter_type         = Type of filter - as in BANDPS call.
! logical          freq_low_none_sens  = Returned flag to set field sensitivity.
! logical          freq_low_full_sens  = Returned flag to set field sensitivity.
! logical          freq_high_full_sens = Returned flag to set field sensitivity.
! logical          freq_high_none_sens = Returned flag to set field sensitivity.
! logical,optional phase_sens          = Returned flag to set field sensitivity.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
!   BANDPS creates a trapezoid-shaped filter in the frequency domain, where
!   the 4 points of the trapezoid are specified by parameters FREQ_LOW_NONE,
!   FREQ_LOW_FULL, FREQ_HIGH_FULL, and FREQ_HIGH_NONE. You may also optionally
!   specify a non-zero phase using the PHASE parameter, and you may optionally
!   set the overall amplitude scale using the AMP_FACT parameter.
!
!   Intended activity of the frequency limit and PHASE parameters for each of
!   the 6 possible FILTER_TYPE values is given by the table:
!
!                   NONE   BANDPASS   HIGHPASS   LOWPASS   ALLPASS   BANDREJECT
!   FREQ_LOW_NONE     N        Y          Y         N         N           Y
!   FREQ_LOW_FULL     N        Y          Y         N         N           Y
!   FREQ_HIGH_FULL    N        Y          N         Y         N           Y
!   FREQ_HIGH_NONE    N        Y          N         Y         N           Y
!   PHASE            0.0       Y          Y         Y         Y          0.0
!
!   "Y" values in the table mean that the frequency limit (or PHASE if present)
!   parameter is used fully: BANDPS uses this value in generating the filter;
!   BANDPS_CHECK requires the frequency limit to be non-nil and reports an
!   error otherwise; and BANDPS_SENSITIVE sets the sensitivity flag to .true.
!
!   "N" values in the table mean that the frequency limit is inactive (e.g.,
!   FREQ_LOW_NONE and FREQ_LOW_FULL are inactive when generating a LOWPASS
!   filter). In these cases, BANDPS ignores any value set for that frequency
!   limit; BANDPS_CHECK resets the parameter to the real "nil" value (FNIL in
!   NAMED_CONSTANTS) which clears it in the GUI; and BANDPS_SENSITIVE sets
!   the sensitivity flag to .false.
!
!   The table displays 0.0 for PHASE when FILTER_TYPE is NONE or BANDREJECT.
!   In these cases, PHASE is inactive and is regarded as hard-wired to 0.0;
!   BANDPS ignores any value set for PHASE and treats it as 0.0; BANDPS_CHECK
!   resets the value of PHASE (if present) to 0.0; and BANDPS_SENSITIVE sets
!   the phase sensitivity flag (if present) to .false.
!
!   In your calling program, be sure to make the FILTER_TYPE string at least
!   10 characters long (because one possible value is "BANDREJECT"), and the
!   MESSAGE string (returned by BANDPS_CHECK) at least 80 characters long.
!
!   The BANDPS routine creates a filter in the frequency domain. In most
!   processes, this would only be called from back-end code; i.e., from within
!   execute_only sections, although nothing stops you from calling it on the
!   front-end side if needed. It is assumed that before calling BANDPS, you
!   have called BANDPS_CHECK, which has not returned an error code.
!
!   BANDPS_CHECK verifies consistency of the parameters specifying the filter,
!   allowing elimination of a great deal of redundant front-end code in
!   different processes. One required input parameter for BANDPS_CHECK is the
!   Nyquist frequency (fnyquist), so it can check that all frequency limits
!   are between zero and Nyquist.
!
!   In BANDPS_CHECK, the FILTER_TYPE argument, as well as frequency limits and
!   (optional) PHASE argument all have INTENT(INOUT); their values are checked
!   on input and may also be changed on output. BANDPS_CHECK will NOT reset
!   FILTER_TYPE from one legal value (BANDPASS, HIGHPASS, ALLPASS, etc.) to
!   another of those values; however, BANDPS_CHECK does convert the FILTER_TYPE
!   string to uppercase and resets it to match one of the six allowed values
!   exactly (NONE, BANDPASS, HIGHPASS, LOWPASS, ALLPASS, or BANDREJECT) if the
!   input FILTER_TYPE string matches an initial substring. (If BANDPS_CHECK
!   can't figure out which value was intended, it returns an error code.)
!
!   BANDPS_CHECK would normally be called from a trap or from the verification
!   section of your update routine. BANDPS_CHECK does NOT interact directly
!   with the parameter cache. Instead, BANDPS_CHECK returns a result code and
!   a message string; then, your process should call PC_ERROR or PC_INFO when
!   appropriate, passing it the message string returned by BANDPS_CHECK.
!
!   The result code returned by BANDPS_CHECK is an integer with 4 possible
!   values which can be referenced by named constants (BANDPS_OK, BANDPS_INFO,
!   BANDPS_ERROR, and BANDPS_ENDERROR) which are available to any process that
!   uses BANDPS_MODULE. Examples:
!        if (result == BANDPS_INFO)  call pc_info  (message)
!        if (result == BANDPS_ERROR) call pc_error (message)
!
!   Result code BANDPS_INFO is returned in cases where a frequency limit is
!   supposed to be inactive ("N" in above table) but is currently non-nil, or
!   if the PHASE should be hard-wired to zero but is currently non-zero. In
!   these cases, BANDPS_CHECK resets the frequency limit to FNIL, or resets
!   PHASE to zero; also, an informative message is returned to display with
!   PC_INFO.
!
!   Result code BANDPS_ERROR is returned if the FILTER_TYPE string cannot be
!   identified as one of its 6 legal values. In this case, the calling process
!   should immediately call PC_ERROR using the returned message.
!
!   Result code BANDPS_ENDERROR is returned if some frequency limits that need
!   to be set (based on current FILTER_TYPE) currently have nil values, or if
!   required frequency limits are out of order. In these cases, your process
!   needs to report the error by calling PC_ERROR if this is a "final" update,
!   but users might get very annoyed if this kind of error is reported every
!   time your update routine is called. For example, suppose FILTER_TYPE is
!   currently NONE (and all 4 frequency limits are nil), and the user then
!   changes FILTER_TYPE to BANDPASS. Now, all 4 frequency limits need to be
!   set. If your update routine calls PC_ERROR every time this condition is
!   detected, the user will see an error screen FOUR TIMES, until all the
!   values have been set (and the user may threaten to shoot the programmer!).
!   Therefore, BANDPS_ENDERROR means to report an error if and only if the
!   user thinks he/she has "finished" setting the filter parameters. If your
!   process creates only a single filter (as opposed to an array of filters),
!   you can simply check all filter parameters in the verification section of
!   your update routine as follows:
!
!      integer           :: result
!      character(len=80) :: message
!      real              :: fnyquist
!      - - - - - - - - - - - - - -
!      - - - - - - - - - - - - - -
!      fnyquist = ......
!      call bandps_check (result, message, fnyquist, ...... )
!      select case (result)
!      case (BANDPS_INFO)
!        call pc_info(message)
!      case (BANDPS_ERROR)
!        call pc_error(message)
!      case (BANDPS_ENDERROR)
!        if (pc_get_update_state() /= PC_GUI) call pc_error(message)
!      end select
!
!   If your process creates an array of filters, you should have both an
!   array element trap and an arrayset trap. The array element trap should
!   call BANDPS_CHECK and look for BANDPS_INFO or BANDPS_ERROR but ignore
!   any BANDPS_ENDERROR code. The arrayset trap should call BANDPS_CHECK for
!   each filter and then call PC_ERROR if BANDPS_ENDERROR is detected. It
!   may then also call PC_JUMP_ARRAYSET_ROW to send the user to the filter
!   that needs to be fixed.
!
!   The BANDPS_SENSITIVE routine is provided to help set field sensitivity in
!   processes that create only a SINGLE filter. The returned logical variables
!   are intended for use in calls to PC_PUT_SENSITIVE_FIELD_FLAG. For example,
!   this will make the FREQ_LOW_NONE and FREQ_LOW_FULL fields insensitive when
!   FILTER_TYPE = LOWPASS. If you call BANDPS_SENSITIVE, you should do it after
!   calling BANDPS_CHECK. This routine should NOT be called by processes that
!   create an array of filters because the GUI does not have any capability to
!   make individual array elements sensitive or insensitive.
!
!   The PHASE arguments are optional in all three routines to allow specifying
!   filters in contexts where altering the phase would not be appropriate. For
!   this same reason, the BANDPS routine is overloaded to allow its first
!   argument (array of returned filter coefficients) to be either real or
!   complex. If it's a real array, the returned filter is always zero-phase.
!   Even in this case, you may include the PHASE argument in the BANDPS call
!   if you wish, but its value will be ignored.
!
!   The AMP_FACT argument in routine BANDPS allows setting the overall scale
!   of the filter in order to recover correct amplitudes. Usually, the goal is
!   to compensate for the amplitude change that occurs after forward & inverse
!   FFTs. With most FFT routines, the required factor is 1.0/NFFT where NFFT
!   is the FFT length. But note: Many CPS processes on the Cray called routines
!   RCFFT2 and CRFFT2, for which the required factor was 0.5/NFFT. Therefore,
!   you may need to alter this value when converting processes from the Cray.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
! 12. 2000-11-17  Baumel        More robust grid rounding (e.g., at Nyquist);
!                               also more efficient for processes that create
!                               large numbers of filters.
! 11. 2000-11-02  Baumel        Reduce error messages from BANDPS_CHECK to
!                               improve user friendliness.
! 10. 2000-10-26  Baumel        Documentation changes only: Fix comment in
!                               advice_doc which had accidental XML syntax
!                               causing cpsfcr error. Update other statements
!                               in advice_doc to reflect change of 2000-10-20.
!  9. 2000-10-20  Baumel        Modify BANDPS_CHECK to force non-nil frequency
!                               limits to lie between zero and Nyquist.
!  8. 1999-12-21  Baumel        Update BANDPS_CHECK to recognize nils for all
!                               filter parameters in case of newly created row
!                               in array of filters: If FILTER_TYPE is nil,
!                               changed to NONE. If PHASE nil, changed to 0.
!                               Also minor cleanup (e.g., remove explicit PI).
!  7. 1999-11-30  Baumel        New parameter set for specifying bandpass
!                               filters; added subroutines BANDPS_CHECK and
!                               BANDPS_SENSITIVE for front-end use. Module
!                               renamed BANDPS to allow time for conversion of
!                               all processes that call this primitive.
!  6. 1999-09-30  Baumel        Added optional TYPE_FILTER argument.
!  5. 1999-08-24  Baumel        Converted from old system; renamed BANDPASS.
!  4. 1999-01-07  Goodger       Begin using the fortran90 compiler.
!  3. 1988-10-21  Baumel        Add CR switch for complex/real filter.
!  2. 1988-08-17  Baumel        New calling sequence, allow only two taper
!                               types: linear and cosine.
!  1. 1986-07-05  Baumel & Wang Initial version of TVFBPS primitive.
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

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!   Specified frequency limits (FREQ_LOW_NONE, FREQ_LOW_FULL, etc.) are NOT
!   rounded onto the grid of output frequencies (multiples of FREQ_INC).
!   Instead, the frequency limits are kept as continuous, real values, and
!   the filter is computed as if frequency were continuous. The returned
!   filter coefficients are simply the values of this continuous function
!   sampled at the specified discrete frequencies (multiples of FREQ_INC).
!
!   If FREQ_LOW_NONE = FREQ_LOW_FULL (or FREQ_HIGH_FULL = FREQ_HIGH_NONE),
!   BANDPS creates a filter with a sharp cutoff at the low (or high) end.
!   In this case, if the specified frequency limit exactly matches one of the
!   discrete output frequencies (i.e., multiple of FREQ_INC), BANDPS will try
!   to keep that frequency in the "FULL" band (as for a FREQ_LOW_FULL or
!   FREQ_HIGH_FULL parameter).
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

      module bandps_module

      use string_module
      use named_constants_module

      implicit none

      private
      public :: bandps
      public :: bandps_check
      public :: bandps_sensitive

      character(len=100),public,save :: BANDPS_IDENT = &
'$Id: bandps.f90,v 1.12 2000/11/16 22:53:09 sps prod sps $'

!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

      interface bandps
        module procedure bandps_real
        module procedure bandps_complex
      end interface

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      integer, public, parameter :: BANDPS_OK       = 0
      integer, public, parameter :: BANDPS_INFO     = 1
      integer, public, parameter :: BANDPS_ERROR    = 2
      integer, public, parameter :: BANDPS_ENDERROR = 3

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

!!----------------------------- bandps_real -------------------------------!!
!!----------------------------- bandps_real -------------------------------!!
!!----------------------------- bandps_real -------------------------------!!

      subroutine bandps_real  (filter, num_freq, freq_inc,  &
                 filter_type, freq_low_none, freq_low_full, &
                 freq_high_full, freq_high_none, phase, amp_fact)
!
      implicit none
!
      real, intent(out)                 :: filter(num_freq)
      integer, intent(in)               :: num_freq
      real,    intent(in)               :: freq_inc
      character(len=*), intent(in)      :: filter_type
      real,    intent(in)               :: freq_low_none
      real,    intent(in)               :: freq_low_full
      real,    intent(in)               :: freq_high_full
      real,    intent(in)               :: freq_high_none
      real,    intent(in), OPTIONAL     :: phase
      real,    intent(in), OPTIONAL     :: amp_fact
!
      integer :: n1, n2, n3, n4, i                     ! local variables
      real    :: ampmax, fmin, fmax, w1, w2            ! local variables
      character(len=10) :: filter_type_loc             ! local variables
!
      filter_type_loc = 'BANDPASS'
      if (filter_type(1:1)=='N' .or. filter_type(1:1)=='n') then
        filter_type_loc = 'NONE'
      else if (filter_type(1:1)=='L' .or. filter_type(1:1)=='l') then
        filter_type_loc = 'LOWPASS'
      else if (filter_type(1:1)=='H' .or. filter_type(1:1)=='h') then
        filter_type_loc = 'HIGHPASS'
      else if (filter_type(1:1)=='A' .or. filter_type(1:1)=='a') then
        filter_type_loc = 'ALLPASS'
      else if (filter_type(5:5)=='R' .or. filter_type(5:5)=='r') then
        filter_type_loc = 'BANDREJECT'
      end if
!
      if (present(amp_fact)) then
        ampmax = amp_fact
      else
        ampmax = 1.
      end if
!
      if (filter_type_loc=='NONE' .or. filter_type_loc=='ALLPASS') then
        filter = ampmax
        return
      else if (freq_inc <= 0.0) then
        filter = 0.0
        return
      end if
!
      if (filter_type_loc == 'LOWPASS') then
        n2 = 1
        n1 = 0
      else
        n2 = min (int(max(freq_low_full/freq_inc + 1.99, 1.01)), num_freq)
        n1 = min (int(max(freq_low_none/freq_inc + 1.01, 0.01)), n2 - 1)
        if (n1 < n2 - 1) then
          fmin = freq_low_none/freq_inc + 1.0
          w1 = ampmax * (freq_inc / (freq_low_full - freq_low_none))
        end if
      end if
!
      if (filter_type_loc == 'HIGHPASS') then
        n3 = num_freq
        n4 = num_freq + 1
      else
        n3 = int(min(freq_high_full/freq_inc + 1.01, real(num_freq) + 0.01))
        n3 = max (n3, n2)
        n4 = int(min(freq_high_none/freq_inc + 1.99, real(num_freq) + 1.01))
        n4 = max (n4, n3 + 1)
        if (n4 > n3 + 1) then
          fmax = freq_high_none/freq_inc + 1.0
          w2 = ampmax * (freq_inc / (freq_high_none - freq_high_full))
        end if
      end if
!
      filter(1:n1) = 0.0
      do i = n1+1, n2-1
        filter(i) = w1 * (real(i) - fmin)
      end do
      filter(n2:n3) = ampmax
      do i = n3+1, n4-1
        filter(i) = w2 * (fmax - real(i))
      end do
      filter(n4:num_freq) = 0.0
!
      if (filter_type_loc == 'BANDREJECT') filter = ampmax - filter
!
      return
      end subroutine bandps_real

!!--------------------------- bandps_complex ------------------------------!!
!!--------------------------- bandps_complex ------------------------------!!
!!--------------------------- bandps_complex ------------------------------!!

      subroutine bandps_complex (filter, num_freq, freq_inc, &
                 filter_type, freq_low_none, freq_low_full,  &
                 freq_high_full, freq_high_none, phase, amp_fact)
!
      implicit none
!
      complex, intent(out)              :: filter(num_freq)
      integer, intent(in)               :: num_freq
      real,    intent(in)               :: freq_inc
      character(len=*), intent(in)      :: filter_type
      real,    intent(in)               :: freq_low_none
      real,    intent(in)               :: freq_low_full
      real,    intent(in)               :: freq_high_full
      real,    intent(in)               :: freq_high_none
      real,    intent(in), OPTIONAL     :: phase
      real,    intent(in), OPTIONAL     :: amp_fact
!
      integer :: n1, n2, n3, n4, i                     ! local variables
      real    :: ampmax, angle, fmin, fmax             ! local variables
      complex :: valmax, w1, w2                        ! local variables
      character(len=10) :: filter_type_loc             ! local variables
!
      filter_type_loc = 'BANDPASS'
      if (filter_type(1:1)=='N' .or. filter_type(1:1)=='n') then
        filter_type_loc = 'NONE'
      else if (filter_type(1:1)=='L' .or. filter_type(1:1)=='l') then
        filter_type_loc = 'LOWPASS'
      else if (filter_type(1:1)=='H' .or. filter_type(1:1)=='h') then
        filter_type_loc = 'HIGHPASS'
      else if (filter_type(1:1)=='A' .or. filter_type(1:1)=='a') then
        filter_type_loc = 'ALLPASS'
      else if (filter_type(5:5)=='R' .or. filter_type(5:5)=='r') then
        filter_type_loc = 'BANDREJECT'
      end if
!
      if (present(amp_fact)) then
        ampmax = amp_fact
      else
        ampmax = 1.
      end if
!
      if (present(phase) .and. filter_type_loc /= 'NONE' &
                         .and. filter_type_loc /= 'BANDREJECT') then
        if (phase /= FNIL) then
          angle  = RADIANS_PER_DEGREE * phase
          valmax = cmplx (ampmax * cos(angle), ampmax * sin(angle))
        else
          valmax = ampmax
        end if
      else
        valmax = ampmax
      end if
!
      if (filter_type_loc=='NONE' .or. filter_type_loc=='ALLPASS') then
        filter = valmax
        return
      else if (freq_inc <= 0.0) then
        filter = (0.0, 0.0)
        return
      end if
!
      if (filter_type_loc == 'LOWPASS') then
        n2 = 1
        n1 = 0
      else
        n2 = min (int(max(freq_low_full/freq_inc + 1.99, 1.01)), num_freq)
        n1 = min (int(max(freq_low_none/freq_inc + 1.01, 0.01)), n2 - 1)
        if (n1 < n2 - 1) then
          fmin = freq_low_none/freq_inc + 1.0
          w1 = valmax * (freq_inc / (freq_low_full - freq_low_none))
        end if
      end if
!
      if (filter_type_loc == 'HIGHPASS') then
        n3 = num_freq
        n4 = num_freq + 1
      else
        n3 = int(min(freq_high_full/freq_inc + 1.01, real(num_freq) + 0.01))
        n3 = max (n3, n2)
        n4 = int(min(freq_high_none/freq_inc + 1.99, real(num_freq) + 1.01))
        n4 = max (n4, n3 + 1)
        if (n4 > n3 + 1) then
          fmax = freq_high_none/freq_inc + 1.0
          w2 = valmax * (freq_inc / (freq_high_none - freq_high_full))
        end if
      end if
!
      filter(1:n1) = (0.0, 0.0)
      do i = n1+1, n2-1
        filter(i) = w1 * (real(i) - fmin)
      end do
      filter(n2:n3) = valmax
      do i = n3+1, n4-1
        filter(i) = w2 * (fmax - real(i))
      end do
      filter(n4:num_freq) = (0.0, 0.0)
!
      if (filter_type_loc == 'BANDREJECT') filter = valmax - filter
!
      return
      end subroutine bandps_complex

!!---------------------------- bandps_check -------------------------------!!
!!---------------------------- bandps_check -------------------------------!!
!!---------------------------- bandps_check -------------------------------!!

      subroutine bandps_check (result, message, fnyquist, filter_type,    &
            freq_low_none, freq_low_full, freq_high_full, freq_high_none, &
            phase)
!
      implicit none
!
      integer,          intent(out)    :: result              ! arguments
      character(len=*), intent(out)    :: message             ! arguments
      real,             intent(in)     :: fnyquist            ! arguments
      character(len=*), intent(inout)  :: filter_type         ! arguments
      real,             intent(inout)  :: freq_low_none       ! arguments
      real,             intent(inout)  :: freq_low_full       ! arguments
      real,             intent(inout)  :: freq_high_full      ! arguments
      real,             intent(inout)  :: freq_high_none      ! arguments
      real, OPTIONAL,   intent(inout)  :: phase               ! arguments

      real            :: phase_loc                            ! local
!
      result = BANDPS_OK
      message = CNIL
!
      if (present(phase)) then
        if (phase == FNIL) then
          phase_loc = 0.
        else
          phase_loc = phase
        end if
      else
        phase_loc = 0.
      end if
!
      if (freq_low_none /= FNIL) &
           freq_low_none = min (max(freq_low_none, 0.0), fnyquist)
      if (freq_low_full /= FNIL) &
           freq_low_full = min (max(freq_low_full, 0.0), fnyquist)
      if (freq_high_full /= FNIL) &
           freq_high_full = min (max(freq_high_full, 0.0), fnyquist)
      if (freq_high_none /= FNIL) &
           freq_high_none = min (max(freq_high_none, 0.0), fnyquist)
!
      if (filter_type == CNIL) filter_type = 'NONE'
      call string_to_upper (filter_type)
      if (filter_type(1:1) == 'N') then
        filter_type = 'NONE'
        if (freq_low_none/=FNIL .or. freq_low_full/=FNIL .or.  &
          freq_high_full/=FNIL .or. freq_high_none/=FNIL .or.  &
          phase_loc/=0.)  then
            freq_low_none  = FNIL
            freq_low_full  = FNIL
            freq_high_full = FNIL
            freq_high_none = FNIL
            phase_loc = 0.
            result = BANDPS_INFO
            message = 'FREQ limits and PHASE irrelevant when FILTER_TYPE =&
                      & NONE; values cleared.'
        end if
      else if (filter_type(1:5) == 'BANDP') then
        filter_type = 'BANDPASS'
        if (freq_low_none==FNIL .or. freq_low_full==FNIL .or.  &
              freq_high_full==FNIL .or. freq_high_none==FNIL) then
            result = BANDPS_ENDERROR
            message = 'All frequency limits must be set when FILTER_TYPE =&
                      & BANDPASS.'
        else if (freq_low_full<freq_low_none  &
            .or. freq_high_full<freq_low_full &
            .or. freq_high_none<freq_high_full) then
            result = BANDPS_ENDERROR
            message = 'Frequency limits out of order.'
        end if
      else if (filter_type(1:1) == 'H') then
        filter_type = 'HIGHPASS'
        if (freq_high_full/=FNIL .or. freq_high_none/=FNIL) then
            freq_high_full = FNIL
            freq_high_none = FNIL
            result = BANDPS_INFO
            message = 'FREQ_HIGH limits irrelevant when FILTER_TYPE =&
                      & HIGHPASS; values cleared.'
        end if
        if (freq_low_none==FNIL .or. freq_low_full==FNIL) then
            result = BANDPS_ENDERROR
            message = 'FREQ_LOW limits must be set when FILTER_TYPE =&
                      & HIGHPASS.'
        else if (freq_low_full<freq_low_none) then
            result = BANDPS_ENDERROR
            message = 'FREQ_LOW limits out of order.'
        end if
      else if (filter_type(1:1) == 'L') then
        filter_type = 'LOWPASS'
        if (freq_low_none/=FNIL .or. freq_low_full/=FNIL) then
            freq_low_none = FNIL
            freq_low_full = FNIL
            result = BANDPS_INFO
            message = 'FREQ_LOW limits irrelevant when FILTER_TYPE =&
                      & LOWPASS; values cleared.'
        end if
        if (freq_high_full==FNIL .or. freq_high_none==FNIL) then
            result = BANDPS_ENDERROR
            message = 'FREQ_HIGH limits must be set when FILTER_TYPE =&
                      & LOWPASS.'
        else if (freq_high_none<freq_high_full) then
            result = BANDPS_ENDERROR
            message = 'FREQ_HIGH limits out of order.'
        end if
      else if (filter_type(1:1) == 'A') then
        filter_type = 'ALLPASS'
        if (freq_low_none/=FNIL .or. freq_low_full/=FNIL .or.  &
          freq_high_full/=FNIL .or. freq_high_none/=FNIL) then
            freq_low_none  = FNIL
            freq_low_full  = FNIL
            freq_high_full = FNIL
            freq_high_none = FNIL
            result = BANDPS_INFO
            message = 'Frequency limits irrelevant when FILTER_TYPE =&
                      & ALLPASS; values cleared.'
        end if
      else if (filter_type(1:5) == 'BANDR') then
        filter_type = 'BANDREJECT'
        if (phase_loc /= 0.) then
            phase_loc = 0.
            result = BANDPS_INFO
            message = 'PHASE must be zero when FILTER_TYPE = BANDREJECT;&
                      & value set to zero.'
        end if
        if (freq_low_none==FNIL .or. freq_low_full==FNIL .or.  &
              freq_high_full==FNIL .or. freq_high_none==FNIL) then
            result = BANDPS_ENDERROR
            message = 'All frequency limits must be set when FILTER_TYPE =&
                      & BANDREJECT.'
        else if (freq_low_full<freq_low_none  &
            .or. freq_high_full<freq_low_full &
            .or. freq_high_none<freq_high_full) then
            result = BANDPS_ENDERROR
            message = 'Frequency limits out of order.'
        end if
      else
        result = BANDPS_ERROR
        message = 'FILTER_TYPE must be NONE, BANDPASS, HIGHPASS, LOWPASS,&
                  & ALLPASS, or BANDREJECT.'
      end if
!
      if (present(phase)) phase = phase_loc
      return
      end subroutine bandps_check

!!-------------------------- bandps_sensitive -----------------------------!!
!!-------------------------- bandps_sensitive -----------------------------!!
!!-------------------------- bandps_sensitive -----------------------------!!

      subroutine bandps_sensitive (filter_type, freq_low_none_sens,     &
          freq_low_full_sens, freq_high_full_sens, freq_high_none_sens, &
          phase_sens)
!
      implicit none
!
      character(len=*),  intent(in)  :: filter_type
      logical,           intent(out) :: freq_low_none_sens
      logical,           intent(out) :: freq_low_full_sens
      logical,           intent(out) :: freq_high_full_sens
      logical,           intent(out) :: freq_high_none_sens
      logical, OPTIONAL, intent(out) :: phase_sens
      logical                        :: phase_sens_loc   ! Local variable
!
      freq_low_none_sens  = .true.
      freq_low_full_sens  = .true.
      freq_high_full_sens = .true.
      freq_high_none_sens = .true.
      phase_sens_loc      = .true.

      select case (filter_type)
      case ('NONE')
        freq_low_none_sens  = .false.
        freq_low_full_sens  = .false.
        freq_high_full_sens = .false.
        freq_high_none_sens = .false.
        phase_sens_loc      = .false.
      case ('HIGHPASS')
        freq_high_full_sens = .false.
        freq_high_none_sens = .false.
      case ('LOWPASS')
        freq_low_none_sens  = .false.
        freq_low_full_sens  = .false.
      case ('ALLPASS')
        freq_low_none_sens  = .false.
        freq_low_full_sens  = .false.
        freq_high_full_sens = .false.
        freq_high_none_sens = .false.
      case ('BANDREJECT')
        phase_sens_loc      = .false.
      end select
!
      if (present(phase_sens)) phase_sens = phase_sens_loc
      return
      end subroutine bandps_sensitive

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module bandps_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
