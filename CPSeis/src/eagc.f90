!<CPS_v1 type="PROCESS"/>
!!------------------------------- eagc.f90 ---------------------------------!!
!!------------------------------- eagc.f90 ---------------------------------!!
!!------------------------------- eagc.f90 ---------------------------------!!


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
! Name       : EAGC
! Category   : amplitude_mod
! Written    : 2003-05-16   by: Bill Done
! Revised    : 2007-11-29   by: Tom Stoeckley
! Maturity   : beta
! Purpose    : Apply AGC to an gather based on analysis of gather amplitude.
! Portability: No known limitations.
! Parallel   : No.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Traditional AGC processes in seismic applications compute the amplitude
! level of a seismic trace over all of the samples within a specified window.
! That amplitude is usually the sum of squares of the samples within the
! window. The reciprocal of that amplitude is used to scale the seismic
! trace sample at the center of the window toward some user specified
! level. The window then slides down one sample on the trace and the
! process is repeated. EAGC operates in exactly this manner except that
! the window covers all of the traces within a gather. The amplitude level
! computed is thus based on the trace samples within the window over all
! traces in the gather. The scale factor computed for each position of
! the window is applied to the sample in the center of the window for all
! traces in the gather.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
! --> Insert advice to the user here.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! This process requires traces to be input in valid gathers.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>


!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! The amplitude of the trace samples in a gather is modified. A window of
! length AGC_WINDOW_LENGTH sec starts with the center of the window located
! at the first sample of the trace. The analysis window location moves down
! the trace, sample by sample. At each sample location for the center of the
! analysis window, the sum of squares of the trace samples within the window
! for all traces in the ensemble is computed. A scale factor based on the
! root-mean-square (RMS) of all of the live trace samples within the analysis
! window and the desired output RMS level (SCALED_RMS_LEVEL) is then used to
! scale the trace sample at the center of the analysis window for all traces
! in the gather.
!
! This process outputs trace gathers. The number of traces output per gather
! and the trace length are unchanged from the input.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!       PROJECT DATA, JOB DATA, AND GLOBAL PARAMETERS USED OR CHANGED
!
! The following parameters are used by this process:
!
! Name      Description                             Action taken
! ----      -----------                             ------------
! NUMTR     max number of traces input/output       Used
! GATHERED  whether traces are a legitimate gather  Used
! NDPT      number of sample values in trace        Used
! DT        trace sample interval                   Used
!
!-------------------------------------------------------------------------------
!</global_doc>


!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! The following trace header word is changed by this process:
!
! Hwd#    Description                Action taken
! ----    -----------                ------------
!   25    LAV                        Updated after scaling traces
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  4. 2007-11-29  Stoeckley  Eliminate the use of the memman primitive.
!003. 2006-09-18  D. Glover  Added NULLIFY statements for Intel compiler.
!002. 2006-01-10  B. Menger  Removed Unused Variables.
!  1. 2004-12-15  Bill Done  Initial version ported from version written to
!                            run on Omega.
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
! Control
! Parameter     Value
! Name          Reported   Description
! ---------     --------   -----------
! NTAPES           0       number of magnetic tapes needed.
! NEED_REQUEST    true     whether this process ever needs to request traces.
! NEED_LABEL     false     whether this process needs a label.
! TWOSETS        false     whether this process needs two trace/header arrays.
! NSCRATCH        >0       amount of temporary memory needed.
! NSTORE          >0       amount of permanent memory needed.
! IFTD           false     whether this process frees tape drives.
! NDISK            0       disk space needed (megabytes) if large.
! SETUP_ONLY     false     whether this process is setup-only.
! PARALLEL_SAFE    ?       whether this process can be in a parallelized loop.
!
!
! Upon input, NTR must have one of these values:
!  NTR >= 1              means to process the input traces.
!  NTR == NO_MORE_TRACES means there are no more input traces.
!  NTR == NEED_TRACES    means someone else needs more traces.
!
! Upon output, NTR will have one of these values:
!  NTR >= 1              if this process is outputting traces.
!  NTR == NO_MORE_TRACES if there are no more traces to output.
!  NTR == FATAL_ERROR    if this process has a fatal error.
!  NTR == NEED_TRACES    if this process needs more traces.
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
! See PROGRAMMING NOTES section.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
! Subroutine eagc(...) is called with each gather of traces to which
! EAGC is to be applied. Subroutine eagc(...) calls subroutine
! eagc_apply(...), which initializes the center sample of the agc
! window at the start of the trace and iterates the location of the
! center of the window to the end of the trace. Initially and at the
! last position on the trace, the window is N+1 points wide, where
! N=obj%window_num_samples_half. As the window "rolls in" to the trace
! it reaches a maximum width of 2*N + 1, where N=obj%window_num_samples_half.
!
! At each location of the window, subroutine eagc_apply(...) calls
! subroutine eagc_calculate_scalars(...). This subroutine computes the
! sum of squares of the data samples within the agc window over all
! traces in the current gather. Dead traces are ignored. That sum of
! squares is divided by the number of nonzero sample values found within
! the window across all traces in the gather. The square root of this
! mean value is taken and returned to subroutine eagc_apply(...).
!
! For each sample time, subroutine eagc_apply(...) takes the rms value
! obtained from eagc_calculate_scalars(...) and multiplies the reciprocal
! of the rms value by obj%scaled_rms_level to obtain the scale factor
! for all traces samples at this sample time. eagc_apply(...) then applies
! that scale factor to the trace samples.
!
! When all traces with in the gather have been scaled, subroutine eagc(...)
! calls lav_set_hdr(...) to update the largest amplitude value in the header
! for each trace in the gather.
!
!-------------------------------------------------------------------------------
!</programming_doc>


!-------------------------------------------------------------------------------
!<gui_def>
!
!<NS EAGC - Ensemble AGC/NC=80>
!
! Ensemble AGC
!
! AGC_WINDOW_LENGTH = `FFFFFFFFF
!
! SCALED_RMS_LEVEL~~= `FFFFFFFFF
!
!
!
! RESTORE_DEFAULTS`P
!
!</gui_def>
!-------------------------------------------------------------------------------


!-------------------------------------------------------------------------------
!<HelpSection>
!
!<Help KEYWORD="AGC_WINDOW_LENGTH">
!<Tip> AGC analysis window length in sec </Tip>
! Default = 1.0
! Allowed = Value greater than three times the sample interval in sec
! This parameter determines the length of the AGC analysis window in sec.
! At each time value, the analysis is determined over all traces in the
! gather. The scale factor needed to achieve the desired SCALED_RMS_LEVEL
! (below) is computed and applied to the trace sample at the current time
! for all traces in the gather.
!</Help>
!
!<Help KEYWORD="RESTORE_DEFAULTS">
!<Tip> Click to restore default values to parameter entry fields </Tip>
! Default = 
! Allowed = 
! Clicking this button enters the default values for each parameter into
! its corresponding entry field.
!</Help>
!
!<Help KEYWORD="SCALED_RMS_LEVEL">
!<Tip> Desired RMS level in agc window after scaling </Tip>
! Default = 3000.
! Allowed = Value greater than zero
! Used to determine the scale factor at each time index after the AGC
! analysis is performed over the specified window. The resulting scale
! factor is applied to the trace amplitudes at the current time across
! all traces in the gather.
!</Help>
!
!
!</HelpSection>
!-------------------------------------------------------------------------------



!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


  module eagc_module
    use pc_module
    use named_constants_module

    use lav_module             ! for updating lav after trace scaling
    use cio_module             ! for debug

    implicit none
  private
    public :: eagc_create
    public :: eagc_initialize
    public :: eagc_update
    public :: eagc_delete
    public :: eagc            ! main trace processing routine.
    public :: eagc_wrapup

    character(len=100),public,save :: eagc_ident = &
      '$Id: eagc.f90,v 1.4 2007/11/30 13:55:17 Stoeckley beta sps $'


!!---------------------- parameter structure -------------------------------!!


    type,public :: eagc_struct

      private
      logical                    :: skip_wrapup      ! wrapup flag.

      logical                  :: gathered ! whether properly gathered.
      integer                  :: ndpt     ! number of trace samples.
      real                     :: dt       ! trace sample interval (sec).

      real, pointer            :: eagc_scalars(:)
      real                     :: agc_window_length
      real                     :: scaled_rms_level
      integer                  :: window_num_samples
      integer                  :: window_num_samples_half
      integer                  :: gather_hdr_word_num

    end type eagc_struct


!!---------------------------- interfaces ---------------------------------!!



!!--------------------------------- data -----------------------------------!!

    integer                  ,save :: lunprint  ! unit number for printing.
    type(eagc_struct),pointer,save :: object    ! needed for traps.

  contains


!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!
!!----------------------------- create -------------------------------------!!


  subroutine eagc_create(obj)

    type(eagc_struct),pointer :: obj       ! arguments
    integer                   :: ierr      ! for error checking
    !call pc_info('Enter eagc_create()')    !debug

    lunprint = pc_get_lun()
    allocate (obj, stat=ierr)
    if (ierr /= 0) call pc_error ("Unable to allocate obj in eagc_create")

    nullify (obj%eagc_scalars) ! jpa

    call eagc_initialize(obj)

  end subroutine eagc_create


!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!
!!------------------------------- delete -----------------------------------!!


  subroutine eagc_delete(obj)
    type(eagc_struct),pointer :: obj       ! arguments

    call eagc_wrapup(obj)

    if (associated(obj%eagc_scalars)) deallocate(obj%eagc_scalars)

  end subroutine eagc_delete


!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!
!!----------------------------- initialize ---------------------------------!!


  subroutine eagc_initialize(obj)

    type(eagc_struct),intent(inout) :: obj       ! arguments
    !call pc_info('Enter eagc_initialize()')    !debug

    ! set defaults

    obj%agc_window_length   = 1.0
    obj%scaled_rms_level    = 3000.0
    obj%gather_hdr_word_num = 3

    call eagc_update(obj)

  end subroutine eagc_initialize


!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!
!!------------------------- start of update --------------------------------!!


  subroutine eagc_update(obj)
    type(eagc_struct),intent(inout),target :: obj             ! arguments

    ! declare all required local variables.
    integer                    :: errorStatus

    !call pc_info('Enter eagc_update()')    !debug

    object => obj               ! needed for traps.
    obj%skip_wrapup = .true.    ! needed for the wrapup routine.


!!------------------------- read parameters --------------------------------!!


    call pc_get_global('gathered', obj%gathered)
    call pc_get_global('ndpt'    , obj%ndpt)
    call pc_get_global('dt'      , obj%dt)

    call pc_get('AGC_WINDOW_LENGTH', obj%agc_window_length,    &
                eagc_agc_window_length_trap)
    call pc_get('SCALED_RMS_LEVEL ', obj%scaled_rms_level ,    &
                eagc_scaled_rms_level_trap)

    ! check that input data is gathered
    if (.not. obj%gathered) then
      call eagc_gathered_trap
      return
    end if


!!------------------------- button handling --------------------------------!!

    if (pc_pressed('restore_defaults')) then
      call eagc_restore_defaults_trap('restore_defaults')
    endif

!!------------------------- screen traps -----------------------------------!!

    call pc_call_screen_trap('EAGCENSEMBLEAGC', eagc_eagcensembleagc_trap)

!!------------------------- end trap ---------------------------------------!!

    call pc_call_end_trap(eagc_end_trap)


!!-------------------------- verify parameters -----------------------------!!



!!------------------------- call processes internally ----------------------!!


!!----------------------- write parameters ---------------------------------!!


    call pc_put('AGC_WINDOW_LENGTH', obj%agc_window_length)
    call pc_put('SCALED_RMS_LEVEL ', obj%scaled_rms_level)

    call pc_put_control('ntapes'       , 0)
    call pc_put_control('need_request' , .false.)
    call pc_put_control('need_label'   , .false.)
    call pc_put_control('twosets'      , .false.)
    call pc_put_control('nscratch'     , 0)
    call pc_put_control('nstore'       , 0)
    call pc_put_control('iftd'         , .false.)
    call pc_put_control('ndisk'        , 0)
    call pc_put_control('setup_only'   , .false.)
    call pc_put_control('parallel_safe', .false.)


!!----------------------- prepare for execution ----------------------------!!


    if (associated(obj%eagc_scalars)) deallocate(obj%eagc_scalars)

    if (pc_do_not_process_traces()) return

    obj%skip_wrapup = .false.     ! to run wrapup code after processing.

    allocate (obj%eagc_scalars(obj%ndpt), stat=errorStatus)

    if (errorStatus /= 0) then
      call pc_error('Error allocating array of scalars.')
    end if

    if (pc_do_not_process_traces()) return   ! in case of allocation errors.

    ! do setup for backend processing
    if (pc_get_update_state() == PC_BACKEND) then
      call eagc_update_backend(obj)
    end if


!!------------------------- finish update ----------------------------------!!


  end subroutine eagc_update


  subroutine eagc_update_backend(obj)
    type(eagc_struct),intent(inout),target :: obj             ! arguments

    integer            :: numSampsInWindow

    ! compute number of samples in window, adding small number to
    ! avoid rounding down during division
    numSampsInWindow = (obj%agc_window_length + .000001)/obj%dt

    ! ensure that number of samples in window is odd
    numSampsInWindow = (numSampsInWindow/2)*2 + 1

    ! set values in data struct
    obj%window_num_samples = numSampsInWindow
    obj%window_num_samples_half = numSampsInWindow/2

    return
  end subroutine eagc_update_backend


!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!
!!------------------------------- traps ------------------------------------!!



  subroutine eagc_gathered_trap
    ! *** Trap for global variable 'gathered' ***
    call pc_error('Input data to EAGC module must be gathered.&
                  & Use gather module prior to eagc')
    return
  end subroutine eagc_gathered_trap


  subroutine eagc_agc_window_length_trap(keyword)
    ! *** Trap for variable AGC_WINDOW_LENGTH ***
    character(len=*), intent(in) :: keyword    !argument

    character(len=80)            :: msg

    !call pc_info('Enter eagc_agc_window_length_trap()')    !debug

    ! check value of window length, adding small number to
    ! avoid rounding down during implied division
    call pc_get(keyword, object%agc_window_length)
    if (object%agc_window_length + 0.000001 < 3*object%dt) then
      write (msg,'(a,a,f6.4,a)') keyword, ' must be >= ', 3*object%dt, '.'
      call pc_error (msg)
      call pc_jump_field(keyword)
    else
      call pc_put(keyword, object%agc_window_length)
    endif
    return
  end subroutine eagc_agc_window_length_trap


  subroutine eagc_restore_defaults_trap(keyword)
    ! *** Trap for variable RESTORE_DEFAULTS ***
    character(len=*), intent(in) :: keyword    !argument
    !call pc_info('Enter eagc_restore_defaults_trap()')    !debug

    ! reset object parms to default values
    object%agc_window_length = 1.0
    object%scaled_rms_level  = 3000.0

    ! put values in gui
    call pc_put('AGC_WINDOW_LENGTH', object%agc_window_length)
    call pc_put('SCALED_RMS_LEVEL ', object%scaled_rms_level)

    return
  end subroutine eagc_restore_defaults_trap


  subroutine eagc_scaled_rms_level_trap(keyword)
    ! *** Trap for variable SCALED_RMS_LEVEL ***
    character(len=*), intent(in) :: keyword    !argument
    !call pc_info('Enter eagc_scaled_rms_level_trap()')    !debug

    ! check for positive target rms level
    call pc_get(keyword, object%scaled_rms_level)
    if (object%scaled_rms_level <= 0.0) then
      call pc_error ('SCALED_RMS_LEVEL must be positive. Default = 3000.')
      call pc_jump_field(keyword)
    else
      call pc_put(keyword, object%scaled_rms_level)
    endif
    return
  end subroutine eagc_scaled_rms_level_trap


  subroutine eagc_eagcensembleagc_trap(keyword)
    ! *** Screen trap for  EAGCENSEMBLEAGC ***
    character(len=*), intent(in) :: keyword    !argument
    !call pc_info('Enter eagc_eagcensembleagc_trap()')    !debug

    return
  end subroutine eagc_eagcensembleagc_trap


  subroutine eagc_end_trap
    ! *** End trap for  EAGCENSEMBLEAGC ***
    !call pc_info('Enter eagc_end_trap()')    !debug

    return
  end subroutine eagc_end_trap



!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!
!!--------------------------- main execution -------------------------------!!


  subroutine eagc(obj,ntr,hd,tr)
    type(eagc_struct),intent(inout) :: obj                    ! arguments
    integer          ,intent(inout) :: ntr                    ! arguments
    double precision ,intent(inout) :: hd(:,:)                ! arguments
    real             ,intent(inout) :: tr(:,:)                ! arguments

    ! local declarations
    logical                         :: okStatus

    if (ntr > 0) then

      ! check gather number for these traces. currently assuming that
      ! header word 3 contains gather number info.
      call eagc_check_gather(hd, ntr, obj%gather_hdr_word_num, okStatus)
      if (.not. okStatus) then

        ! gather is improperly formed, fatal error
        ntr = FATAL_ERROR
        call eagc_wrapup(obj)
        return
      end if

      ! process this gather of traces and output
      call eagc_apply(hd, tr, ntr, obj%window_num_samples_half,   &
                      obj%scaled_rms_level, obj%ndpt,             &
                      obj%eagc_scalars, okStatus)

      if (okStatus) then

        ! this gather processed successfully

        ! update the largest absolute amplitude
        call lav_set_hdr(hd, tr, obj%ndpt, ntr)

        ! ntr remains unchanged, indicating traces are ready for output
        return
      else

        ! problem occurred processing this gather. indicate fatal error
        ntr = FATAL_ERROR
        call eagc_wrapup(obj)
        return
      endif

    else if (ntr == NO_MORE_TRACES) then

      ! monitor app indicates no more traces available, so pass on message
      call eagc_wrapup(obj)
      return

    else if (ntr == NEED_TRACES) then

      ! monitor app indicates another module needs trace. so does this
      ! one, so pass on message.
      return

    end if

  end subroutine eagc

  subroutine eagc_check_gather(hdr, ntraces, gatherWordNum, statusOK)
    double precision, intent(in)              :: hdr(:,:)
    integer         , intent(in)              :: ntraces
    integer         , intent(in)              :: gatherWordNum
    logical         , intent(out)             :: statusOK

    integer          :: k
    double precision :: prevHdrVal

    statusOK = .true.
    prevHdrVal = hdr(gatherWordNum, 1)
    do k = 2, ntraces
      if (hdr(gatherWordNum, k) /= prevHdrVal) then
        ! gather number for this trace does not match previous.
        ! declare a problem and return.
        statusOK = .false.
        return
      end if
    end do
    
    ! gather properly numbered if execution gets here
    return

  end subroutine eagc_check_gather

  subroutine eagc_apply(hdr, traces, ntraces, winlength, rms_level, &
                        numsamp, eagc, returnStatusOK)

    integer         , intent(in)              :: ntraces
    integer         , intent(in)              :: winlength
    integer         , intent(in)              :: numsamp
    real            , intent(in)              :: rms_level
    double precision, intent(in)              :: hdr(:,:)
    real            , intent(inout)           :: traces(:,:)
    real            , intent(out)             :: eagc(:)
    logical         , intent(out)             :: returnStatusOK

    ! local declarations
    integer                       :: i, j
    integer                       :: wincenter, winstart, winstop

    ! determine RMS values
    do i = 1, numsamp
       wincenter = i
       winstart  = wincenter - winlength
       if ( winstart < 1 ) winstart = 1

       winstop   = wincenter + winlength
       if ( winstop > numsamp ) winstop = numsamp

       ! calculate scalar for this time index (i)
       call eagc_calculate_scalars(hdr, traces, ntraces, numsamp, &
                                   winstart, winstop, eagc(i),    &
                                   returnStatusOK)
       if (.not. returnStatusOK) then
         return
       end if

    end do

    ! Take inverse of RMS values
    do i = 1, numsamp
       if ( eagc(i) /= 0.0 ) eagc(i) = rms_level/eagc(i)
    end do

    ! now apply inverse of RMS to all traces
    do i = 1, ntraces
      ! check for dead trace and skip if so
      if (hdr(25,i) == 0.0) cycle
      do j = 1, numsamp
        traces(j,i) = traces(j,i) * eagc(j)
      end do
    end do

    ! return to calling routine
    returnStatusOK = .true.
    return
  end subroutine eagc_apply

  subroutine eagc_calculate_scalars(hdr, trace, ntraces, numsamp,  &
                                    winstart, winstop, eagc,       &
                                    status_ok)

    integer         , intent(in)        :: ntraces
    integer         , intent(in)        :: numsamp
    integer         , intent(in)        :: winstart
    integer         , intent(in)        :: winstop
    double precision, intent(in)        :: hdr(:,:)
    real            , intent(in)        :: trace(:,:)
    real            , intent(out)       :: eagc
    logical         , intent(out)       :: status_ok

    ! local declarations
    integer                 :: nlive
    integer                 :: ws
    integer                 :: we
    integer                 :: wl
    integer                 :: i, j
    real                    :: sum
    real                    :: sumsqs
    real                    :: rms

    ! initialize
    eagc   = 0.0
    nlive  = 0
    sumsqs = 0.0

    ! loop over all traces

    ! set start, end, and length of window
    ws = winstart
    we = winstop
    wl = we - ws + 1

    do i = 1, ntraces

      ! check for dead trace and skip if so
      if (hdr(25,i) == 0.0) cycle

      ! check for 0 or 1 one sample in the window, skip if so
      if ( wl < 2 ) cycle

      ! find the number of live values in the window
      do j = ws, we
        if ( trace(j,i) /= 0.0 ) nlive = nlive + 1
      end do

      ! find the sum of squares for scaling
      sum = 0.0
      do j = ws, we
        sum = sum + trace(j,i) * trace(j,i)
      end do
      sumsqs = sumsqs + sum
    end do

    ! find the root mean square for scaling
    if ( nlive <= 0 ) then
      eagc = 0.0
    else
      rms  = sumsqs / nlive
      eagc = sqrt ( rms )
    end if

    ! return to the calling routine
    status_ok = .true.
    return
  end subroutine eagc_calculate_scalars


!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!
!!------------------------------- wrapup -----------------------------------!!


  subroutine eagc_wrapup(obj)

    type(eagc_struct),intent(inout) :: obj       ! arguments

    if (obj%skip_wrapup) return
    obj%skip_wrapup = .true.

  end subroutine eagc_wrapup


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


  end module eagc_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

