!<CPS_v1 type="PROCESS"/>
!!------------------------------- ctan.f90 ---------------------------------!!
!!------------------------------- ctan.f90 ---------------------------------!!
!!------------------------------- ctan.f90 ---------------------------------!!

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
! Name       : CTAN   (Complex Trace ANalysis)
! Category   : transforms
! Written    : 1989-02-20   by: Roger Parsons and Bill Troutt
! Revised    : 2000-12-07   by: Bob Baumel
! Maturity   : production   2001-04-30
! Purpose    : Perform complex trace (Hilbert Transform) analysis.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!
! CTAN calculates a user-specified (Hilbert Transform - derived) attribute
! version of the input traces.  Some non-Hilbert transform options are also
! available for comparison.
!
!
! Input trace transform options include:
!
!       COSIP   Cosine of the instantaneous phase
!
!       ENVL    Envelope or reflection strength
!
!       HILBERT Hilbert transform
!
!       HORT    Hilbert transform of the rectified trace
!
!       IFREQ   Instantaneous frequency
!
!       IPHASE  Instantaneous phase
!
!       PFREQ   Instantaneous frequency computed at envelope peaks - output
!               trace FILLED in with these values
!
!       POL     Input trace corresponding to envelope peaks are kept - other
!               samples set to zero
!
!       PORT    Phase of the rectified trace
!
!       PPHAS   Instantaneous phase computed at envelope peaks - output
!               trace FILLED in with these values
!
!                            ----- ***  ----
!
!       (The following options are not Hilbert Transform - derived and are
!       included for comparison.)
!
!       TRACE   Original input trace (Note: this is a do-nothing option).
!
!       ZFREQ   Zero-crossing frequency is calculated as the reciprocal of
!               twice the period between zero-crossing times (which are
!               determined by linear interpolation).  All output samples
!               between zero-crossings are filled in with the zero-crossing
!               frequency.
!
!
! Basic Definitions
!
! Hilbert Transform: the 90 degree phase advanced version of a time series.
! If the input trace is Tr = A * Cos( w * t ), where A is amplitude, w is
! angular frequency and t is time, then H(Tr) = -A * Sin( w * t ).
!
! Analytic Trace: the complex-valued "trace" whose real part is equal to an
! original seismic trace and whose imaginary part is the Hilbert transform of
! the original seismic trace. Also known as the complex trace.
!
! Envelope: the modulus of the analytic trace, or
! sqrt(real_part^2 + imaginary_part^2).  Also known as reflection strength.
!
! Instantaneous phase: -arctan(imaginary_part/real_part).
!
! Instantaneous frequency: (1/(2*pi)) * time derivative of instantaneous phase.
!
!
! Reference
!
! Taner, M. T., et. al.,1979, Complex Seismic Trace Analysis: Geophysics, 44,
! 1041 - 1063 (illustrated).
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! Complex trace attributes are normally displayed as color plots.
!
! Instantaneous frequency values may exceed the Nyquist frequency for sudden
! changes in traces sample values.
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
! Name     Description                           Action taken
! ----     -----------                           ------------
! NDPT     number of sample values in trace      used but not changed
! DT       trace sample interval                 used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                   TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                   Action taken
! ----    -----------                   ------------
! 2       Head mute index               Used
! 25      Largest Absolute Value        Reset (except when MODE = TRACE)
! 64      Tail mute index               Used
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date       Author     Description
!     ----       ------     -----------
! 15. 2001-04-30 Baumel     Change wrapped_up flag to skip_wrapup.
! 14. 2000-03-31 Baumel     Improve handling of hard zeros when MODE = ZFREQ.
! 13. 2000-03-30 Baumel     Clip using median NON-ZERO value when MODE = IFREQ;
!                           refine ctan_fill routine; also, include "true zero"
!                           logic (item 6 below) for ALL options that depend on
!                           the envelope (thus, affects all options except
!                           TRACE, ZFREQ, HILBERT, and HORT).
! 12. 2000-03-28 Baumel     Cleaned up code, fixed many errors, various little
!                           changes in algorithm.
! 11. 1999-12-14 D. Sharp   Corrected listing of the CTAN completion message.
! 10. 1999-12-09 D. Sharp   Converted from old system.
! 9.  1998-12-14 Vunderink  Begin using the f90 compiler.
! 8.  1991-09-06 Troutt     Add MODE = COSIP (cosine of the instantaneous
!                           phase).
! 7.  1991-04-24 Troutt     Add code to zero out dead traces. They were
!                           previously flagged in header words 2 and 25,
!                           but not zeroed.
! 6.  1991-01-21 Troutt     For MODE=ENVL added logic regarding true zero
!                           values. The output envelope trace is set to
!                           zero for all samples that were zero on input.
!                           (This is NOT done for SINGLE, ISOLATED zeros.)
! 5.  1990-01-16 Troutt     Fix logic for MODE=POL (compute Hilbert
!                           transform for intermediate envelope calc.).
! 4.  1989-08-14 Troutt     Fix calculation of Hilbert transform at zero
!                           and Nyquist frequencies.
! 3.  1989-08-01 Troutt     Almost total re-write to make program more
!                           like the CONSEIS modules it replaces.
! 2.  1989-07-21 Howard     Fix bug in PUTP call.
! 1.  1989-02-20 Parsons    Converted from CONSEIS.
!
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
!
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
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more imput traces.
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
! Most of the algorithm is pretty straightforward. The most obscure part is
! calculation of instantaneous frequency, now in subroutine ctan_ifreq, which
! requires estimation of derivatives of discretely sampled functions (DTR and
! DTI denote derivatives of the TR and TI traces). The previous (Cray) code
! included the following, really strange, asymmetric formula:
!
!   df(i)  =  [ 6 * f(i-2)  -  60 * f(i-1)  -  40 * f(i)
!                   +  120 * f(i+1)  -  30 * f(i+2)  +  4 * f(i+3) ]  /  120
!
! for estimating the derivative at sample i in terms of 6 samples ranging
! from i-2 to i+3. The above asymmetric formula appears to be accurate when
! applied to polynomials up to 5th order. This has been replaced (2000-03-28)
! by the simpler, symmetric formula:
!
!   df(i)  =  [ f(i-2)  -  8 * f(i-1)  +  8 * f(i+1)  -  f(i+2) ]  /  12
!
! which expresses the derivative at sample i in terms of 4 samples ranging
! from i-2 to i+2, and is accurate for polynomials up to 4th order.
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

!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS CTAN Process>
!
!         Complex Trace ANalysis Process
!Perform complex trace (Hilbert Transform) analysis.
!
! MODE=`CCCCCCC       CLIP=~~~~`FFFFFFFFFFF    RECT=`CCCCCCCC
!
! BIAS=`FFFFFFFFFFF   FREQ_MAX=`FFFFFFFFFFF
!</gui_def>

!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Type of attribute of input traces to calculate. </Tip>
! Default = ENVL
! Allowed = COSIP    (Cosine of instantaneous phase of trace)
! Allowed = ENVL     (Envelope or reflection strength of trace)
! Allowed = HILBERT  (Hilbert transform of trace)
! Allowed = HORT     (Hilbert transform of rectified trace)
! Allowed = IFREQ    (Instantaneous frequency of trace)
! Allowed = IPHASE   (Instantaneous phase of trace)
! Allowed = PFREQ    (Instantaneous frequency at envelope peaks of trace)
! Allowed = POL      (Input trace at envelope peaks - other samples zeroed)
! Allowed = PORT     (Instantaneous phase of rectified trace)
! Allowed = PPHASE   (Instantaneous phase at envelope peaks of trace)
! Allowed = TRACE    (Original input trace)
! Allowed = ZFREQ    (Zero-crossing frequency)
!
! For PFREQ and PPHASE, the output trace is filled in with the values
! calculated at the envelope peaks.
!
! For ZFREQ, zero-crossing frequency is calculated as the reciprocal of twice
! the period between zero-crossing times (which are determined by linear
! interpolation).  All output samples between zero-crossings are filled in
! with the zero-crossing frequency.
!</Help>
!
!<Help KEYWORD="CLIP">
!<Tip> Clip factor for MODE = IFREQ. </Tip>
! Default = 2.5
! Allowed = real > 0.0
! Clip absolute values that exceed CLIP * (median output trace absolute
! value).
!
! Active for MODE = IFREQ only.
!</Help>
!
!<Help KEYWORD="RECT">
!<Tip> Type of rectification prior to transform. </Tip>
! Default = FULL
! Allowed = FULL      (Rectify positive and negative input values.)
! Allowed = POSITIVE  (Rectify positive input values only.)
! Allowed = NEGATIVE  (Rectify negative input values only.)
!
! Active for MODE = PORT and HORT only.
!</Help>
!
!<Help KEYWORD="BIAS">
!<Tip> Minimum for the rectified trace is BIAS * input trace LAV. </Tip>
! Default = 0.01
! Allowed = 1.0 > real >= 0.0
! Fraction of the input trace LAV to be used as the minimum for the rectified
! trace.  Rectified values < (BIAS * input trace LAV) are replaced by
! (BIAS * input trace LAV).
!
! Active for MODE = PORT and HORT only.
!</Help>
!
!<Help KEYWORD="FREQ_MAX">
!<Tip> Maximum frequency allowed for MODE = ZFREQ. </Tip>
! Default = Nyquist/2.
! Allowed = Nyquist > real > 0.0
! Clip computed frequency values that exceed FREQ_MAX.
!
! Active for MODE = ZFREQ only.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module ctan_module

      use pc_module
      use named_constants_module
      use fft_module
      use median_module
      use string_module
      use mutehw_module
      use lav_module

      implicit none
      private
      public :: ctan_create
      public :: ctan_initialize
      public :: ctan_update
      public :: ctan_delete
!<execute_only>
      public :: ctan
      public :: ctan_wrapup
!</execute_only>

      character(len=100),public,save :: CTAN_IDENT = &
'$Id: ctan.f90,v 1.15 2001/04/26 18:58:46 sps prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: ctan_struct
        private
        logical          :: skip_wrapup      ! wrapup flag.

        character(len=7) :: mode             ! process parameters
        real             :: clip             ! process parameters
        character(len=8) :: rect             ! process parameters
        real             :: bias             ! process parameters
        real             :: freq_max         ! process parameters

        integer          :: ndpt             ! globals
        real             :: dt               ! globals

        integer          :: npow2, nf        ! dependent variables
        real             :: factor           ! dependent variables

        type(fft_struct),pointer :: fftrc    ! for FFTs
        type(fft_struct),pointer :: fftcr    ! for FFTs

      end type ctan_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(ctan_struct),pointer,save :: object      ! needed for traps.

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine ctan_create (obj)
      implicit none
      type(ctan_struct),pointer :: obj       ! arguments

      allocate (obj)

      nullify (obj%fftrc)
      nullify (obj%fftcr)

      call ctan_initialize (obj)
      return
      end subroutine ctan_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine ctan_delete (obj)
      implicit none
      type(ctan_struct),pointer :: obj       ! arguments

!<execute_only>
      call ctan_wrapup (obj)
!</execute_only>

      if (associated(obj%fftrc)) call fft_delete (obj%fftrc)
      if (associated(obj%fftcr)) call fft_delete (obj%fftcr)

      deallocate(obj)
      return
      end subroutine ctan_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine ctan_initialize (obj)
      implicit none
      type(ctan_struct),intent(inout) :: obj       ! arguments

      obj%dt = -1.
      call pc_get_global ('DT', obj%dt)
      if (obj%dt <= 0.) obj%dt = 1.

      obj%mode      = 'ENVL'
      obj%clip      =  2.5
      obj%rect      = 'FULL'
      obj%bias      =  0.01
      obj%freq_max  =  0.25 / obj%dt

      call ctan_update (obj)
      return
      end subroutine ctan_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine ctan_update (obj)
      implicit none
      type(ctan_struct),intent(inout),target :: obj         ! arguments

      logical :: clip_sens, rect_sens, bias_sens, freq_max_sens
      integer :: nscratch, nstore, i_err
      real    :: fnyq

      object => obj               ! needed for traps.
      obj%skip_wrapup = .true.    ! needed for the wrapup routine.

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      obj%ndpt = -1
      call pc_get_global ('NDPT', obj%ndpt)
      if (obj%ndpt < 1) then
        call pc_error ("NDPT global hasn't been set.")
        obj%ndpt = 1
      end if
      obj%dt = -1.
      call pc_get_global ('DT', obj%dt)
      if (obj%dt <= 0.0) then
        call pc_error ("DT global hasn't been set.")
        obj%dt = 1.0
      end if

      call pc_get ('MODE'    , obj%mode)
      call pc_get ('CLIP'    , obj%clip)
      call pc_get ('RECT'    , obj%rect)
      call pc_get ('BIAS'    , obj%bias)
      call pc_get ('FREQ_MAX', obj%freq_max)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      clip_sens     = .false.
      rect_sens     = .false.
      bias_sens     = .false.
      freq_max_sens = .false.

      call string_to_upper (obj%mode)

      if (obj%mode(1:1) == 'C') then
        obj%mode = 'COSIP'
      else if (obj%mode(1:1) == 'E') then
        obj%mode = 'ENVL'
      else if (obj%mode(1:2) == 'HI') then
        obj%mode = 'HILBERT'
      else if (obj%mode(1:2) == 'HO') then
        obj%mode = 'HORT'
        rect_sens = .true.
        bias_sens = .true.
      else if (obj%mode(1:2) == 'IF') then
        obj%mode = 'IFREQ'
        clip_sens = .true.
      else if (obj%mode(1:2) == 'IP') then
        obj%mode = 'IPHASE'
      else if (obj%mode(1:2) == 'PF') then
        obj%mode = 'PFREQ'
      else if (obj%mode(1:3) == 'POL') then
        obj%mode = 'POL'
      else if (obj%mode(1:3) == 'POR') then
        obj%mode = 'PORT'
        rect_sens = .true.
        bias_sens = .true.
      else if (obj%mode(1:2) == 'PP') then
        obj%mode = 'PPHASE'
      else if (obj%mode(1:1) == 'T') then
        obj%mode = 'TRACE'
      else if (obj%mode(1:1) == 'Z') then
        obj%mode = 'ZFREQ'
        freq_max_sens = .true.
      else
        call pc_error ('Invalid setting of MODE parameter.')
      end if

      if (obj%clip <= 0.0) then
        if (clip_sens) then
          call pc_error ('CLIP must be positive.')
        else
          obj%clip = 2.5
        end if
      end if

      call string_to_upper (obj%rect)
      if (obj%rect(1:1) == 'F') then
        obj%rect = 'FULL'
      else if (obj%rect(1:1) == 'P') then
        obj%rect = 'POSITIVE'
      else if (obj%rect(1:1) == 'N') then
        obj%rect = 'NEGATIVE'
      else
        if (rect_sens) then
          call pc_error ('RECT must be FULL, POSITIVE, or NEGATIVE.') 
        else
          obj%rect = 'FULL'
        end if
      end if

      if (obj%bias<0.0 .or. obj%bias>=1.0) then
        if (bias_sens) then
          call pc_error ('BIAS must be in range from 0 to 1.')
        else
          obj%bias = 0.01
        end if
      end if

      fnyq = 0.5 / obj%dt
      if (obj%freq_max<=0.0 .or. obj%freq_max>fnyq) then
        if (freq_max_sens) then
          call pc_error ('FREQ_MAX must be between 0 and Nyquist.')
        else
          obj%freq_max = 0.25 / obj%dt
        end if
      end if

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field ('MODE', (/ 'COSIP  ', 'ENVL   ', 'HILBERT', &
           'HORT   ', 'IFREQ  ', 'IPHASE ', 'PFREQ  ', 'POL    ', 'PORT   ', &
           'PPHASE ', 'TRACE  ', 'ZFREQ  ' /), 12)
      call pc_put_options_field ('RECT', (/ 'FULL    ', 'POSITIVE',  &
                                            'NEGATIVE' /), 3)
      call pc_put ('MODE'    , obj%mode)
      call pc_put ('CLIP'    , obj%clip)
      call pc_put ('RECT'    , obj%rect)
      call pc_put ('BIAS'    , obj%bias)
      call pc_put ('FREQ_MAX', obj%freq_max)

      call pc_put_sensitive_field_flag ('CLIP'    , clip_sens)
      call pc_put_sensitive_field_flag ('RECT'    , rect_sens)
      call pc_put_sensitive_field_flag ('BIAS'    , bias_sens)
      call pc_put_sensitive_field_flag ('FREQ_MAX', freq_max_sens)

      obj%npow2 = 8
      do while (obj%npow2 < obj%ndpt)
        obj%npow2 = 2 * obj%npow2
      end do
      obj%nf = obj%npow2/2 + 1

      nstore   = 0
      nscratch = 3 * obj%ndpt
      if (obj%mode/='TRACE' .and. obj%mode/='ZFREQ') then
        nstore = nstore + fft_mem_usage(obj%npow2,'rtoc')  &
                        + fft_mem_usage(obj%npow2,'ctor')
        nscratch = nscratch + obj%npow2 + 2*obj%nf
      end if
      call pc_put_control ('NSCRATCH', nscratch)
      call pc_put_control ('NSTORE'  , nstore)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

      if (associated(obj%fftrc)) call fft_delete (obj%fftrc)
      if (associated(obj%fftcr)) call fft_delete (obj%fftcr)

!<execute_only>

      if (pc_do_not_process_traces()) return

      if (obj%mode/='TRACE' .and. obj%mode/='ZFREQ') then
        i_err = fft_create (obj%fftrc, -1, obj%npow2, 'rtoc',   &
                            opt_scale = 1./real(obj%npow2))
        if (i_err /= 0) then
          call pc_error ('Error creating Real to Complex FFT object.')
        end if
        i_err = fft_create(obj%fftcr, 1, obj%npow2, 'ctor')
        if (i_err /= 0) then
          call pc_error ('Error creating Complex to Real FFT object.')
        end if
      end if

      obj%factor = real (24. * PI * obj%dt)

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine ctan_update

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine ctan (obj, ntr, hd, tr)
      implicit none
      type(ctan_struct),intent(inout) :: obj              ! arguments
      integer          ,intent(inout) :: ntr              ! arguments
      double precision ,intent(inout) :: hd(:,:)          ! arguments
      real             ,intent(inout) :: tr(:,:)          ! arguments

      real      :: ti(obj%ndpt)
      real      :: a (obj%ndpt)
      real      :: p (obj%ndpt)
      integer   :: L, mute1, mute2, nvals, it, nonzero
      real      :: amax, amin, fmed, fmax

      if (ntr == NO_MORE_TRACES) then
        call ctan_wrapup (obj)
        return
      end if

      if (obj%mode == 'TRACE') return    ! do nothing if MODE = TRACE

      do L = 1, ntr              !loop over input traces

        amax = hd(HDR_LAV,L)
        if (amax == 0.0) cycle           ! do nothing if dead trace

        call mutehw (hd(:,L), tr(:,L), obj%ndpt, 0.0, MUTEHW_SET)
        mute1 = nint(hd(HDR_TOP_MUTE,L))
        mute2 = nint(hd(HDR_BOTTOM_MUTE,L))
        nvals = mute2 - mute1 + 1

        if (obj%mode == 'ZFREQ') then
          call ctan_zfreq (obj, nvals, a(:nvals), tr(mute1:mute2,L))
          goto 100
        end if

!!      Rectify trace if MODE is PORT or HORT
        if (obj%mode=='PORT' .or. obj%mode=='HORT') then
          amin = obj%bias * amax
          select case (obj%rect)
          case ('FULL')
            do it = 1, obj%ndpt
              tr(it,L) = max(abs(tr(it,L)),amin)
            end do
          case ('POSITIVE')
            do it = 1, obj%ndpt
              tr(it,L) = max(tr(it,L),amin)
            end do
          case ('NEGATIVE')
            do it = 1, obj%ndpt
              tr(it,L) = min(tr(it,L),(-amin))
            end do
          end select
        end if

!!      Perform Hilbert transform now; put result in TI array
        call ctan_hilbert (obj, tr(:,L), ti)

!!      If MODE = HILBERT or MODE = HORT, we're finished
        if (obj%mode=='HILBERT' .or. obj%mode=='HORT') then
          tr(:obj%ndpt,L) = ti(:obj%ndpt)
          cycle
        end if

!!      Put power (envelope squared) in P array
        p = tr(:obj%ndpt,L)**2 + ti**2

!!      Force envelope to be zero wherever two or more consecutive
!!      trace values are hard zeros.
        if (tr(1,L) == 0.0)  p(1) = 0.0
        do it = 2, obj%ndpt-1
          if (tr(it,L) == 0.0) then
            if (tr(it-1,L) == 0.0) then
              p(it) = 0.0
            else if (tr(it+1,L) == 0.0) then
              p(it) = 0.0
            end if
          end if
        end do
        if (tr(obj%ndpt,L) == 0.0)  p(obj%ndpt) = 0.0

        if (obj%mode == 'ENVL') then

          tr(:obj%ndpt,L) = sqrt(p)
          cycle

        else if (obj%mode == 'IFREQ') then

          call ctan_ifreq (obj, nvals, tr(mute1:mute2,L), ti(mute1:mute2), &
                           p(mute1:mute2), a(mute1:mute2) )
          nonzero = 0
          do it = mute1, mute2
            if (a(it) /= 0.0) then
              nonzero = nonzero + 1  ! P is no longer needed for envelope**2,
              p(nonzero) = a(it)     ! so use it for non-zero freqs for median
            end if
          end do
          if (nonzero >= 10) then
            call median (p(:nonzero), nonzero, fmed)
            fmax = obj%clip * fmed
            do it = mute1, mute2
              tr(it,L) = min(a(it),fmax)
            end do
          else
            tr(mute1:mute2,L) = 0.0
          end if

        else if (obj%mode == 'PFREQ') then

          call ctan_ifreq (obj, nvals, tr(mute1:mute2,L), ti(mute1:mute2), &
                           p(mute1:mute2), a(mute1:mute2) )
          call ctan_fill (nvals, p(mute1:mute2), a(mute1:mute2),  &
                          fill=.true., tr=tr(mute1:mute2,L) )

        else if (obj%mode=='IPHASE' .or. obj%mode=='PORT') then

          do it = mute1, mute2
            if (p(it) > 0.) then
              tr(it,L) = -atan2 (ti(it), tr(it,L))
            else
              tr(it,L) = 0.0
            end if
          end do

        else if (obj%mode == 'PPHASE') then

          do it = mute1, mute2
            if (p(it) > 0.) then
              a(it) = -atan2 (ti(it), tr(it,L))
            else
              a(it) = 0.0
            end if
          end do

          call ctan_fill (nvals, p(mute1:mute2), a(mute1:mute2),  &
                          fill=.true., tr=tr(mute1:mute2,L) )

        else if (obj%mode=='COSIP') then

          do it = mute1, mute2
            if (p(it) > 0.) then
              tr(it,L) = tr(it,L) / sqrt(p(it))
            else
              tr(it,L) = 0.0
            end if
          end do

        else if (obj%mode == 'POL') then

          a(mute1:mute2) = tr(mute1:mute2,L)

          call ctan_fill (nvals, p(mute1:mute2), a(mute1:mute2),  &
                          fill=.false., tr=tr(mute1:mute2,L) )
        end if

 100    call mutehw (hd(:,L), tr(:,L), obj%ndpt, 0.0, MUTEHW_BOTH)

      end do

      call lav_set_hdr (hd, tr, obj%ndpt, ntr)

      return
      end subroutine ctan

!!----------------------------- ctan_zfreq ---------------------------------!!
!!----------------------------- ctan_zfreq ---------------------------------!!
!!----------------------------- ctan_zfreq ---------------------------------!!

      subroutine ctan_zfreq (obj, nv, temp, tr)
      implicit none
!
!     This subroutine implements MODE = ZFREQ
!     Note: TEMP is just a work array, but passed from calling routine
!     to avoid extra memory allocation.
!
      type(ctan_struct),intent(in)    :: obj            ! arguments
      integer          ,intent(in)    :: nv             ! arguments
      real             ,intent(inout) :: temp(nv)       ! arguments
      real             ,intent(inout) :: tr(nv)         ! arguments

      integer         :: it, i1
      real            :: ztim1, ztim2, freq
      real, parameter :: ztim_none = -1.0e+30

!     Move trace to temp array
      temp = tr

      i1 = 1
      if (temp(1) == 0.0) then
        ztim1 = 0.
      else
        ztim1 = ztim_none
      end if

      do it = 2, nv

        if (temp(it-1)*temp(it) > 0.0) cycle

        if (temp(it-1) == 0.0) then
          if (temp(it) == 0.0) then
            ztim1 = ztim_none
            cycle
          else if (ztim1 == ztim_none) then
            ztim2 = (it-2) * obj%dt
          else
            cycle
          end if
        else if (temp(it) == 0.0) then
          ztim2 = (it-1) * obj%dt
        else
          ztim2 = ((it-2) + temp(it-1)/(temp(it-1)-temp(it))) * obj%dt
        end if

        if (ztim1 == ztim_none) then
          tr(i1:it-1) = 0.0
        else
          if (ztim2 > ztim1) then
            freq = min (0.5/(ztim2-ztim1), obj%freq_max)
          else
            freq = obj%freq_max
          end if
          tr(i1:it-1) = freq
        end if

        ztim1 = ztim2
        i1 = it

      end do

      tr(i1:nv) = 0.0           ! zero remainder of trace

      return
      end subroutine ctan_zfreq

!!---------------------------- ctan_hilbert --------------------------------!!
!!---------------------------- ctan_hilbert --------------------------------!!
!!---------------------------- ctan_hilbert --------------------------------!!

      subroutine ctan_hilbert (obj, tr, ti)
      implicit none
!
!     Hilbert transform of input trace TR placed in output array TI
!
      type(ctan_struct),intent(in)  :: obj           ! arguments
      real             ,intent(in)  :: tr(:)         ! arguments
      real             ,intent(out) :: ti(:)         ! arguments

      real    :: rtemp (obj%npow2)
      complex :: ctemp (obj%nf)

      rtemp(:obj%ndpt) = tr(:obj%ndpt)
      rtemp(obj%ndpt+1:obj%npow2) = 0.0
      call fft_rc_transform (obj%fftrc, rtemp ,ctemp)
      ctemp = (0.,1.) * ctemp
      ctemp(1)      = (0.,0.)
      ctemp(obj%nf) = (0.,0.)
      call fft_cr_transform (obj%fftcr, ctemp, rtemp)
      ti(:obj%ndpt) = rtemp(:obj%ndpt)
      return
      end subroutine ctan_hilbert

!!----------------------------- ctan_ifreq ---------------------------------!!
!!----------------------------- ctan_ifreq ---------------------------------!!
!!----------------------------- ctan_ifreq ---------------------------------!!

      subroutine ctan_ifreq (obj, nv, tr, ti, p, freq)
      implicit none
      type(ctan_struct),intent(in)  :: obj           ! arguments
      integer          ,intent(in)  :: nv            ! arguments
      real             ,intent(in)  :: tr(nv)        ! arguments
      real             ,intent(in)  :: ti(nv)        ! arguments
      real             ,intent(in)  :: p(nv)         ! arguments
      real             ,intent(out) :: freq(nv)      ! arguments

      integer  :: it
      real     :: dtr, dti

      if (nv < 5) then
        freq = 0.0
        return
      end if

      freq(1:2)     = 0.0
      freq(nv-1:nv) = 0.0

      do it = 3, nv-2
        if (p(it) > 0.) then
          dtr = tr(it-2) - 8.*tr(it-1) + 8.*tr(it+1) - tr(it+2)
          dti = ti(it-2) - 8.*ti(it-1) + 8.*ti(it+1) - ti(it+2)
          freq(it) = abs(ti(it)*dtr - tr(it)*dti) / (obj%factor * p(it))
        else
          freq(it) = 0.0
        end if
      end do

      return
      end subroutine ctan_ifreq

!!------------------------------ ctan_fill ---------------------------------!!
!!------------------------------ ctan_fill ---------------------------------!!
!!------------------------------ ctan_fill ---------------------------------!!

      subroutine ctan_fill (nv, env, at, fill, tr)
      implicit none
!
!     Routine to fill trace array TR with values of attribute AT
!     corresponding to peaks of envelope ENV.  NV values in each array.
!     if FILL = .true., all TR values between successive troughs of ENV
!        are set to the value of AT at peak of ENV.
!     If FILL = .false., TR is set to value of AT at peaks of ENV but
!        zeroed everywhere else.
!
      integer, intent(in)  :: nv         ! arguments
      real   , intent(in)  :: env(nv)    ! arguments
      real   , intent(in)  :: at (nv)    ! arguments
      logical, intent(in)  :: fill       ! arguments
      real   , intent(out) :: tr (nv)    ! arguments

      integer :: nlast, j
      real    :: apk, dxp, dxm

      if (nv < 3) then
        tr = 0.0
        return
      end if

      if (fill) then

        apk = 0.0
        nlast = 1
        dxp = env(2) - env(1)
        do j = 2, nv-1
          dxm = dxp
          dxp = env(j+1) - env(j)
          if (dxm*dxp > 0.0) cycle
          if (dxm > 0.0) then                       ! peak in envelope
            apk = at(j)
            tr(nlast:j) = apk
            nlast = j
          else if (dxm < 0.0 .or. dxp > 0.0) then   ! trough in envelope
            tr(nlast:j) = apk
            nlast = j
            apk = 0.0
          end if
        end do
        tr(nlast:nv) = apk

      else

        nlast = 0
        do j = 2, nv-1             ! scan for only the peaks in envelope
          if (env(j) > env(j-1) .and. env(j) >= env(j+1)) then
            tr(nlast+1:j-1) = 0.0
            tr(j) = at(j)
            nlast = j
          end if
        end do
        tr(nlast+1:nv) = 0.0

      end if

      return
      end subroutine ctan_fill
!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!

!<execute_only>

      subroutine ctan_wrapup (obj)
      implicit none
      type(ctan_struct),intent(inout) :: obj       ! arguments

      if (obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine ctan_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module ctan_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
