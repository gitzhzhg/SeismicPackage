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
! Name       : ADNS   (Add Noise)
! Category   : synthetics
! Written    : 1986-08-13   by: Bob Baumel
! Revised    : 2002-04-03   by: Bob Baumel
! Maturity   : production   2002-04-15
! Purpose    : Adds white noise to traces.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Adds white noise to traces.  This is often useful with synthetic data,
! either to test the effect of noise on processes, or to mask numerical
! artifacts in synthetics.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! When MODE=SINGLE, the noise level added to each trace is determined
! individually from the sample amplitudes in that trace. When MODE=MULTI, the
! noise level is determined from the RMS amplitude of the whole current gather
! of traces; then, this noise level is added to every trace in the gather.
! When MODE=ABS, an absolute noise level is added, independent from the
! amplitudes of the traces.
!
! TIM_BEG and TIM_END specify the portion of the input traces used for
! determining input RMS trace amplitudes when MODE=SINGLE and MODE=MULTI.
! Once a noise level has been determined, added noise is applied to the
! ENTIRE trace.  TIM_BEG and TIM_END have no effect when MODE=ABS.
!
! The noise added by ADNS is all-band white noise, containing all frequencies
! from zero to Nyquist. Thus, you may wish to follow ADNS by a band-pass
! filter. Please note, however, that this will generally reduce the RMS noise
! level, which may then be a SMALLER fraction of the RMS signal level than
! indicated by your setting of FRAC (assuming that the original signal was
! already bandlimited before addition of noise).
!
! Setting TYPE_NOISE = DEXP yields double-exponentially distributed noise.
! This is a distinctly non-Gaussian noise which puts more events on the
! "tails" of the distribution than a Gaussian of the same standard deviation
! (Technically, it's a "leptokurtic" distribution). Generating synthetics
! with this type of noise can provide a more rigorous test of a process's
! robustness than simple Gaussian noise.
!
! If HDR_FLAG is non-zero, random noise is added to only the flagged traces.
! Also, when MODE=MULTI, the flagged traces do double duty: Only flagged traces
! are used in determining the RMS amplitude of the gather; then, noise is added
! only to the flagged traces.
!
! ADNS is fully re-enterable with respect to all parameters except SEED. If
! ADNS occurs more than once in a job, no attempt is made to ensure that each
! ADNS generates the correct sequence of random numbers according to its SEED
! value. Therefore, if you want fully reproducible pseudo-random noise
! controlled by SEED, use only one ADNS in the job. If reproducibility of the
! noise doesn't matter, feel free to put more than one ADNS in the job.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! Process is either a single-trace or multiple-trace process, depending on
! setting of the MODE parameter:
!
! Traces must be input in gathers when MODE = MULTI.
!
! Traces may be input either singly or in gathers when MODE = SINGLE or ABS.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process alters input traces.
! This process outputs the same traces as it receives (usually altered).
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
! TSTRT    starting time on trace                used but not changed
! DT       trace sample interval                 used but not changed
! NWIH     number of words in header             used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
! 2       head mute index            used but not changed
! 25      largest absolute value     recomputed
! 64      tail mute index            used but not changed
! *       hdr_flag                   used but not changed
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author    Description
!     ----        ------    -----------
! 19. 2002-04-15  Baumel    Improve GUI robustness for TIM_BEG and TIM_END.
! 18. 2001-08-02  Baumel    Add GUI check for gathered input when MODE = MULTI.
! 17. 2001-05-14  Selzler   Changed wrapup logic to use skip_wrapup
! 16. 2000-07-07  Selzler   Fixed problems found by CPS Fortran Code Review.
! 15. 2000-04-17  Selzler   Fixed bug in update and used new random mth routine
! 14. 2000-03-28  Selzler   Correct more bugs in GUI interaction
! 13. 2000-03-14  Selzler   Correct bugs in GUI interaction
! 12. 2000-02-02  Selzler   Added support for GUI and general cleanup
! 11. 1999-11-19  Selzler   Added RCS "Id" strings to tag executeable
! 10. 1999-09-13  Selzler   Updated skip_wrapup and print_lun usage
!  9. 1999-08-27  Selzler   Conversion to f90.
!  8. 1999-04-29  Baumel    Add parameter seed.
!  7. 1997-12-23  Goodger   Add parameter HF#.  Convert to fortran90
!                           free form.
!  6. 1995-09-26  Baumel    Add option for double-exponential noise (new
!                           type_noise parameter) to provide better test of
!                           robustness of processes in handling noise.
!  5. 1994-02-09  Baumel    Calculate noise level from RMS amplitude
!                           instead of LAV (no more call to TRSCAN); add
!                           mode, tim_beg, tim_end parameters.
!  4. 1988-10-20  Baumel    NWIH conversion.
!  3. 1988-06-23  Baumel    Add CPSPRT calls, use TRSCAN for finding LAV,
!                           add option for fract<0.
!  2. 1988-06-03  Baumel    New Convention for mute header word.
!  1. 1986-08-13  Baumel    Original version.
!
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
!
! This process uses a single set of trace and header arrays.
!
! Upon input, NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == NO_MORE_TRACES means there are no more input traces.
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
! 1. As of 1994-02-09, the action of the fract parameter has changed
!    significantly. Previously, it set the RMS noise level as a fraction
!    of trace LAV.  Now (for MODE=SINGLE and MODE=MULTI), it sets RMS
!    noise level as a fraction of RMS trace amplitude.
!
! 2. The option of setting an ABSOLUTE noise level (independent of
!    input trace amplitudes), previously available by setting FRACT
!    negative, is now (1994-02-09) available by explicitly setting
!    MODE=ABS.  FRACT is now always positive.
!
!-------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<gui_def>
!<NS ADNS Process/NC=80>
!
!               Add Noise.
!
!  MODE=~~~~~~`CCCCC       FRACT=~~`FFFFFFFFFFF
!
!  TIM_BEG=~~~`FFFFFFFFFFF TIM_END=`FFFFFFFFFFF
!
!  TYPE_NOISE=`CCCC        SEED=~~~`IIIIIIIIII  
!
!  HDR_FLAG=~~`IIIIIIIIII
!
!</gui_def>
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="MODE">
!<Tip> Method of determining noise amplitude. </Tip>
! Default = MULTI
! Allowed = MULTI   Set noise level for each gather of traces based on RMS
!                   amplitude of that gather.
! Allowed = SINGLE  Set noise level for each trace individually, based on
!                   the RMS amplitude of that trace.
! Allowed = ABS     Add a user-specified ABSOLUTE noise level to all traces
!                   independently of values originally in the traces.
!</Help>
!
!<Help KEYWORD="FRACT">
!<Tip> Scale parameter of added noise. </Tip>
! Default = 0.5
! Allowed = real > 0.0
!
! If mode = SINGLE or MULTI, this is the noise standard deviation as a
! fraction of the RMS amplitude of input trace (or input gather).
! If mode = ABS, then FRACT is EXACTLY the standard deviation of the added
! noise.
!</Help>
!
!<Help KEYWORD="TIM_BEG">
!<Tip> Start of time window for calculating RMS amplitude, in seconds. </Tip>
! Default = TSTRT
! Allowed = TIM_END > real >= TSTRT
!
! The RMS amplitude of input traces is determined within the window defined by
! TIM_BEG and TIM_END when MODE=SINGLE or MODE=MULTI (ignored when MODE=ABS).
!
! Generated noise is always added to the ENTIRE trace.
!</Help>
!
!<Help KEYWORD="TIM_END">
!<Tip> End of time window for calculating RMS amplitude, in seconds. </Tip>
! Default = End-of-trace.
! Allowed = End-of-trace >= real > TIM_BEG
!
! The RMS amplitude of input traces is determined within the window defined by
! TIM_BEG and TIM_END when mode=SINGLE or mode=MULTI (ignored when mode=ABS).
!
! Generated noise is always added to the ENTIRE trace.
!</Help>
!
!<Help KEYWORD="TYPE_NOISE">
!<Tip> Type of noise added to trace. </Tip>
! Default = GAUSS
! Allowed = GAUSS, Gaussian (removable by least-squares methods)
! Allowed = DEXP,  Double-Exponential (requires more robust methods to remove
! noise, e.g., L1 techniques)
!</Help>
!
!<Help KEYWORD="SEED">
!<Tip> Seed for initializing the random number generator. </Tip>
! Default = 0, equivalent to not explicitly setting the seed.
! Allowed = any integer
!</Help>
!
!<Help KEYWORD="HDR_FLAG">
!<Tip> Header word denoting flagged traces. </Tip>
! Default = 0
! Allowed = 0 - NWIH
!
! If HDR_FLAG = 0, then noise is added to all traces. Otherwise, only traces
! with a flag set in header word HDR_FLAG are changed.

! If MODE = MULTI, then flagged traces do double duty, as only the flagged
! traces are used in determining RMS signal amplitudes; then, once a noise
! level has been determined for the gather, only the flagged traces are
! modified by adding noise to them.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!

      module adns_module
      use pc_module
      use named_constants_module
      use lav_module
      use mth_module
      use snrm2_module
      use mutehw_module
      implicit none
      private
      public :: adns_create     ! uses the parameter cache.
      public :: adns_initialize
      public :: adns_update     ! uses the parameter cache.
      public :: adns_delete

!<execute_only>
      public :: adns            ! main execution (trace processing) routine.
      public :: adns_wrapup
!</execute_only>

      character(len=100),public,save :: ADNS_IDENT = &
'$Id: adns.f90,v 1.19 2002/04/12 14:27:00 Baumel prod sps $'

!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!

      type,public :: adns_struct
      private
        logical                      :: skip_wrapup      ! dependent parameter

        character(len=6)             :: mode             ! process parameter.
        real                         :: fract            ! process parameter.
        real                         :: tim_beg          ! process parameter.
        real                         :: tim_end          ! process parameter.
        character(len=5)             :: type_noise       ! process parameter.
        integer                      :: seed             ! process parameter.
        integer                      :: hdr_flag         ! process parameter.

        integer                      :: nwih             ! global parameter.
        integer                      :: ndpt             ! global parameter.
        real                         :: dt               ! global parameter.
        real                         :: tstrt            ! global parameter.

        integer                      :: tim_beg_idx      ! dependent parameter.
        integer                      :: tim_end_idx      ! dependent parameter.

        integer                      :: print_lun        ! dependent parameter.
      end type adns_struct

!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!
!!--------------------------------- data -----------------------------------!!

      type(adns_struct),pointer,save :: object      ! needed for traps.

      contains

!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!

      subroutine adns_create (obj)
      implicit none
      type(adns_struct),pointer :: obj       ! arguments

      allocate (obj)

      call adns_initialize (obj)

      return
      end subroutine adns_create

!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!

      subroutine adns_delete (obj)
      implicit none
      type(adns_struct),pointer :: obj       ! arguments

!<execute_only>
      call adns_wrapup (obj)
!</execute_only>

      deallocate(obj)

      return
      end subroutine adns_delete

!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!

      subroutine adns_initialize (obj)
      implicit none
      type(adns_struct),pointer :: obj       ! arguments

      call pc_get_global('ndpt', obj%ndpt)
      call pc_get_global('dt', obj%dt)
      call pc_get_global('tstrt', obj%tstrt)

      obj%mode = 'MULTI'
      obj%fract = 0.5
      obj%tim_beg = obj%tstrt
      obj%tim_end = obj%tstrt + (obj%ndpt-1)*obj%dt
      obj%type_noise = 'GAUSS'
      obj%seed = 0
      obj%hdr_flag = 0

      call adns_update (obj)

      return
      end subroutine adns_initialize

!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!

      subroutine adns_update (obj)
      implicit none
      type(adns_struct),target :: obj                           ! arguments
      integer  :: numtr                                         ! local
      logical  :: gathered                                      ! local

      object => obj         ! needed for traps.
      obj%skip_wrapup = .true.

      obj%print_lun = pc_get_lun()

!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!
!!------------------------- read parameters --------------------------------!!

      call pc_get_global('NWIH', obj%nwih)
      call pc_get_global('NDPT', obj%ndpt)
      call pc_get_global('DT', obj%dt)
      call pc_get_global('TSTRT', obj%tstrt)
      call pc_get_global('NUMTR', numtr)
      call pc_get_global('GATHERED', gathered)

      call pc_get('MODE', obj%mode)
      call string_to_upper(obj%mode)
      call pc_get('FRACT', obj%fract)
      call pc_get('TIM_BEG', obj%tim_beg)
      call pc_get('TIM_END', obj%tim_end)
      call pc_get('TYPE_NOISE', obj%type_noise)
      call string_to_upper(obj%type_noise)
      call pc_get('SEED', obj%seed)
      call pc_get('HDR_FLAG', obj%hdr_flag)

!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!
!!-------------------------- verify parameters -----------------------------!!

      if(obj%dt <= 0.0) then
        call pc_error('DT must be greater than zero')
        return
      end if

      if(obj%mode(1:1) == 'M') then
        obj%mode = 'MULTI'
      else if(obj%mode(1:1) == 'S') then
        obj%mode = 'SINGLE'
      else if(obj%mode(1:1) == 'A') then
        obj%mode = 'ABS'
      else
        call pc_error('MODE must be either MULTI, SINGLE or ABS')
      end if

      if (obj%mode == 'MULTI') then
        ! Make sure input is properly gathered
        if (numtr < 2) then
          call pc_error ('ADNS requires gathered input when MODE = MULTI. &
                         &Please insert GATHER before ADNS or use a &
                         &different MODE value.')
        else if (.not. gathered) then
          call pc_error ('ADNS requires gathered input when MODE = MULTI. &
                         &Input traces are currently in groups but not in &
                         &functional gathers.')
        end if
      end if

      if (obj%mode == 'ABS') then
        call pc_put_sensitive_field_flag ('TIM_BEG', .false.)
        call pc_put_sensitive_field_flag ('TIM_END', .false.)
        if (pc_get_update_state() /= PC_GUI) then
          obj%tim_beg = obj%tstrt
          obj%tim_end = obj%tstrt + (obj%ndpt-1)*obj%dt
        end if
      else
        call pc_put_sensitive_field_flag ('TIM_BEG', .true.)
        call pc_put_sensitive_field_flag ('TIM_END', .true.)
      end if

      obj%fract = abs(obj%fract)
      if(obj%fract <= 0.0) then
        call pc_error('FRACT must be greater than 0.0')
      end if

      if(obj%type_noise(1:1) == 'G') then
        obj%type_noise = 'GAUSS'
      else if(obj%type_noise(1:1) == 'D') then
        obj%type_noise = 'DEXP'
      else
        call pc_error('TYPE_NOISE must be either GAUSS or DEXP')
        obj%type_noise = 'GAUSS'
      end if

      obj%hdr_flag = min (max(obj%hdr_flag, 0), obj%nwih)

      obj%tim_beg_idx = nint((obj%tim_beg-obj%tstrt)/obj%dt) + 1
      if(obj%tim_beg_idx < 1) then
        if (obj%mode /= 'ABS') call pc_error('TIM_BEG must be greater &
                                 &than or equal to TSTRT.')
        obj%tim_beg_idx = 1
      else if(obj%tim_beg_idx >= obj%ndpt) then
        if (obj%mode /= 'ABS') call pc_error('TIM_BEG must be less than &
                                 &trace end.')
        obj%tim_beg_idx = 1
      end if
      obj%tim_beg_idx = min (max(obj%tim_beg_idx, 1), obj%ndpt)

      obj%tim_end_idx = nint((obj%tim_end-obj%tstrt)/obj%dt) + 1
      if(obj%tim_end_idx > obj%ndpt) then
        if (obj%mode /= 'ABS') call pc_error('TIM_END must be less than &
                                 &or equal to trace end.')
        obj%tim_end_idx = obj%ndpt
      else if(obj%tim_end_idx <= 1) then
        if (obj%mode /= 'ABS') call pc_error('TIM_END must be greater &
                                 &than TSTRT.')
        obj%tim_end_idx = obj%ndpt
      end if
      obj%tim_end_idx = min (max(obj%tim_end_idx, obj%tim_beg_idx), obj%ndpt)

      if(obj%tim_end_idx == obj%tim_beg_idx .and. obj%mode /= 'ABS') then
        call pc_error('TIM_END must be greater than TIM_BEG.')
      end if

      obj%tim_beg = obj%tstrt + (obj%tim_beg_idx-1)*obj%dt
      obj%tim_end = obj%tstrt + (obj%tim_end_idx-1)*obj%dt

!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!
!!----------------------- write parameters ---------------------------------!!

      call pc_put_options_field('MODE', (/ "MULTI ", "SINGLE", "ABS   " /), 3)
      call pc_put_options_field('TYPE_NOISE', (/ "GAUSS", "DEXP " /), 2)
      call pc_put('MODE', obj%mode)
      call pc_put('FRACT', obj%fract)
      call pc_put('TIM_BEG', obj%tim_beg)
      call pc_put('TIM_END', obj%tim_end)
      call pc_put('TYPE_NOISE', obj%type_noise)
      call pc_put('SEED', obj%seed)
      call pc_put('HDR_FLAG', obj%hdr_flag)

!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!
!!----------------------- prepare for execution ----------------------------!!

!<execute_only>

      if (pc_do_not_process_traces()) return
      obj%skip_wrapup = .false.

      obj%fract = ABS(obj%fract)

      IF (obj%seed /= 0) CALL mth_ranset(obj%seed)

!</execute_only>

!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!
!!------------------------- finish update ----------------------------------!!

      return
      end subroutine adns_update

!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!

!<execute_only>

      subroutine adns (obj,ntr,hd,tr)
      implicit none
      type(adns_struct)               :: obj                    ! arguments
      integer         ,intent(inout)  :: ntr                    ! arguments
      double precision,intent(inout)  :: hd(:,:)                ! arguments
      real            ,intent(inout)  :: tr(:,:)                ! arguments

      integer :: nlen  ! local
      integer :: jstart  ! local
      integer :: jstop  ! local
      integer :: nsamp  ! local
      integer :: ntr_do  ! local
      real :: sumsq  ! local
      real :: sdev  ! local
      double precision :: eps ! local

      eps = epsilon(1.0d0)
      SUMSQ = 0.
      NSAMP = 0

      ntr_loop: DO ntr_do = 1,ntr

        CALL MUTEHW (HD(1:,ntr_do), TR(1:,ntr_do), obj%ndpt, 0., MUTEHW_SET)

        IF (obj%hdr_flag /= 0) THEN
          IF (ABS(HD(obj%hdr_flag,ntr_do)) < eps) CYCLE
        END IF

        IF (obj%mode == 'ABS') THEN
           CALL ADNS_APPLY (obj, TR(:obj%ndpt,ntr_do), obj%fract)
        ELSE
           JSTART = MAX (NINT(HD( 2,ntr_do)), obj%tim_beg_idx)
           JSTOP  = MIN (NINT(HD(64,ntr_do)), obj%tim_end_idx)

           IF (JSTOP >= JSTART) THEN
              NLEN = JSTOP - JSTART + 1

              IF (obj%mode == 'SINGLE') THEN
                 SDEV = obj%fract * &
                   snrm2_real(NLEN, TR(JSTART:JSTOP,ntr_do), 1) &
                             /  SQRT(REAL(NLEN))

                 CALL ADNS_APPLY (obj, TR(:obj%ndpt,ntr_do), SDEV)
              ELSE
                 SUMSQ = SUMSQ + &
                   snrm2_real(NLEN, TR(JSTART:JSTOP,ntr_do), 1) ** 2
                 NSAMP = NSAMP + NLEN
              ENDIF
           ENDIF
        ENDIF
      END DO ntr_loop

      IF (obj%mode == 'MULTI' .AND. NSAMP > 0) THEN
        SDEV = obj%fract * SQRT(SUMSQ/NSAMP)

        DO ntr_do = 1,ntr
          IF (obj%hdr_flag /= 0) THEN
            IF (ABS(HD(obj%hdr_flag,ntr_do)) < eps) CYCLE
          END IF

          CALL ADNS_APPLY (obj, TR(:obj%ndpt,ntr_do), SDEV)
        END DO
      END IF

      call lav_set_hdr(hd, tr, obj%ndpt, ntr)

      return
      end subroutine adns

!!----------------------------- adns_apply -----------------------------------!!
!!----------------------------- adns_apply -----------------------------------!!
!!----------------------------- adns_apply -----------------------------------!!

      SUBROUTINE ADNS_APPLY (obj, TRACE, SDEV)
      implicit none

      type(adns_struct)               :: obj                    ! arguments
      INTEGER ndpt_do
      REAL TRACE(obj%ndpt), SDEV, ranx, rany

      IF (obj%type_noise(1:1) == 'G') THEN
         DO ndpt_do = 1, obj%ndpt-1, 2
           call mth_gauss_ranf(sdev, ranx, rany)
           TRACE(ndpt_do) = TRACE(ndpt_do) + ranx
           TRACE(ndpt_do+1) = TRACE(ndpt_do+1) + rany
         END DO
      ELSE
         DO ndpt_do = 1, obj%ndpt
           TRACE(ndpt_do) = TRACE(ndpt_do) + mth_dexp_ranf(sdev)
         END DO
      END IF

      RETURN
      END SUBROUTINE ADNS_APPLY

!</execute_only>

!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


!<execute_only>

      subroutine adns_wrapup (obj)
      implicit none
      type(adns_struct) :: obj       ! arguments

      if(obj%skip_wrapup) return
      obj%skip_wrapup = .true.

      return
      end subroutine adns_wrapup

!</execute_only>

!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!

      end module adns_module

!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
