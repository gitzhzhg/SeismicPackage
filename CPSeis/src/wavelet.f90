!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- wavelet.f90 -------------------------------!!
!!------------------------------- wavelet.f90 -------------------------------!!
!!------------------------------- wavelet.f90 -------------------------------!!
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
!------------------------------------------------------------------------------ 
!                        C P S   P R I M I T I V E
!
! Name       : WAVELET    (construct WAVELET)
! Category   : synthetics
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2007-04-12   by: Douglas Hanson Add wavelet_get_process.
! Maturity   : beta
! Purpose    : Construct a seismic wavelet in the time domain.
! Portability: No known limitations.
!
!------------------------------------------------------------------------------ 
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------ 
!                         GENERAL DESCRIPTION
!
!  This routine creates a seismic wavelet in the time domain,
!  and convolves it with a seismic trace.
!
!  Currently a spike, a ricker a gaussian or its first two derivitives 
!  is supported.
!
!  TRACE WAVELETS AND FILTERING
!
!  Output traces are constructed by convolving a series of spikes
!  with a wavelet defined by parameters WAVELET_TYPE and WAVELET_LENGTH.
!
!  Both the wavelet and convolved output trace can be filtered by a bandpass
!  filter defined by the parameters:
!  WAVELET_FREQ_LOW_NONE, WAVELET_FREQ_LOW_FULL, 
!  WAVELET_FREQ_HIGH_FULL, WAVELET_FREQ_HIGH_NONE and WAVELET_PHASE.
!
!  Parameter WAVELET_FILTER_LEVEL controls how the bandpass filter is applied 
!  to the wavelet and or the trace.  
!  It can be applied to either on, both or neiher.
!
!  If WAVELET_FILTER_LEVEL = WAVELET the filtered wavelet is convolved 
!  with spikes to form the output trace.  
!  The trace is not filtered after convolution.
!
!  If WAVELET_FILTER_LEVEL = TRACE the unfiltered wavelet is convolved 
!  with spikes to form the output trace.  
!  The trace is filtered after convolution.
!
!  If WAVELET_FILTER_LEVEL = BOTH the filtered wavelet is convolved with spikes
!  to form the output trace.  The trace is filtered after convolution.
!
!  If WAVELET_FILTER_LEVEL = NONE the unfiltered wavelet is convolved 
!  with spikes to form the output trace.  
!  The trace is not filtered after convolution.
!
!  Filtering the traces tend to give larger side lobes than filtering
!  the wavelet only.
!
!  Parameter WAVELET_TYPE controls the type of wavelet convolved with the
!  output traces.  Currently a spike, a ricker, a gaussian or its first two
!  derivitives is supported.
!
!  For a ricker wavelet the expression is:
!  Encylopedic Dictionary of Applied Geophysics, Sheriff:
!
!  WAVELET(TIME) = ( 1 - 2 * ( PI * WAVELET_FREQ_RICKER * TIME ) **2 ) 
!                   * EXP (- ( PI * WAVELET_FREQ_RICKER * TIME ) **2 )
!
!  Where WAVELET_FREQ_RICKER is the central frequeuncy of the filter and is
!  (WAVELET_FREQ_LOW_NONE+WAVELET_FREQ_HIGH_NONE) * .5
!  Make sure WAVELET_LENGTH is large enough to capture the ricker wavelet.
!
!------------------------------------------------------------------------------ 
!</descript_doc>
 
!<calling_doc>
!------------------------------------------------------------------------------ 
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      wlt = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!------------------------------------------------------------------------------ 
!                          CALLING SEQUENCE
!
! create the wavelet object, o
!                           i
!      call wavelet_create (c_title,                                   &
!                           o
!                           wlt,                                       &
!                           i               i
!                           wavelet_type, wavelet_length, &
!                           i               i
!                           wavelet_fft, wavelet_inc, &
!                           i               i
!                           wavelet_freq_low_none,  wavelet_freq_low_full,  &
!                           i               i
!                           wavelet_freq_high_full, wavelet_freq_high_none, &
!                           i 
!                           wavelet_phase,                                &
!                           i 
!                           apply_filter,                              &
!                           o
!                           i_err)
!
! delete the wavelet object, o
!
!                          b
!      call wavelet_delete(o)
!
! convolve the wavelet with a trace, x_inp, length nx_inp to produce x_out
!
!                         i         i       b      b
!      call wavelet_apply(o, nx_inp, x_inp, x_out)
!
!  lu_out,         integer    = output unit for printing 
!                              useing lu_out <0 disables printout
!
!  c_title,        character  = character string added to the print out
!
!  wavelet_type,   character  = type of wavelet flag
!
!  wavelet_type = 'SPIKE'     = constant spike length 1 amplitude 1.
!  wavelet_type = 'RICKER'    = ricker wavelet
! ( 1 - 2 * ( pi * wavelet_freq_ricker * time ) **2 ) 
!   * exp (-( pi * wavelet_freq_ricker * time ) **2 )
! wavelet_freq_ricker is the central frequeuncy of the filter and is 
! (wavelet_freq_low_none+wavelet_freq_high_none) * .5
!  wavelet_type = 'GAUSSIAN0' = gaussian wavelet
!  wavelet_type = 'GAUSSIAN1' = first derivitive of a gaussian wavelet
!  wavelet_type = 'GAUSSIAN2' = second derivitive of a gaussian wavelet
!
!  wavelet_length, real       = wavelet length in time units
!  wavelet_inc,    real       = wavelet time increment in time units
!  wavelet_fft,    integer    = wavelet length in fft
!
!  The wavelet is filtered with a bandpass fitler defined by the following:
!
!  wavelet_freq_low_none,  real = frequency, in hertz, at start of lower taper
!  wavelet_freq_low_full,  real = frequency, in hertz, at end   of lower taper
!  wavelet_freq_high_full, real = frequency, in hertz, at start of upper taper
!  wavelet_freq_high_none, real = frequency, in hertz, at end   of upper taper
!  wavelet_phase,          real = additional wavelet_phase, in degrees, 
!                                 added to wavelet
!  apply_filter    logical    = flag to apply frtequency filter to wavelet
!
!  nx_inp,         integer    = number of time samples in x_inp and x_out
!  x_inp,          real array = input trace to be convolved with the wavelet
!  x_out,          real array = output trace - may bve the same as x_inp
!
!  i_err,          integer    = error lfag 0 = o.k. < 0 = error.
!
!------------------------------------------------------------------------------ 
!</calling_doc>

!<advice_doc>
!------------------------------------------------------------------------------ 
!                            ADVICE FOR USERS
!
!------------------------------------------------------------------------------ 
!</advice_doc>

!<history_doc>
!------------------------------------------------------------------------------ 
!                           REVISION HISTORY
!
!     Date        Author        Description
!     ----        ------        -----------
! 12  2007-04-12  Douglas Hanson Add wavelet_get_process.
!     2007-01-03  Douglas Hanson Correct apply_filter.
! 11  2006-10-17  Douglas Hanson Modify prints.
! 10  2006-10-03  Douglas Hanson Remove print.
!  9  2006-09-21  Douglas Hanson Set fxmig freq sensitivity.
!  8  2006-06-15  Douglas Hanson Use wavelet prefix.
!  7  2006-04-20  Douglas Hanson Add wavelet gui.
!  6  2006-01-10  B. Menger      Removed Unused Variables.
!  5  2001-01-10  Douglas Hanson Correct ricker definition.
!  4  2000-10-31  Douglas Hanson fix spike and ricker position
!  3  2000-08-25  Douglas Hanson cpsfcr
!  2  2000-05-26  Brad Kruse     Update XML prologue.
!  1  1999-12-01  Douglas Hanson Initial version.
!
!------------------------------------------------------------------------------ 
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
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
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
!<gui_def>
!
!`- wavelet definition ---------------------------------------------------------
!
! WAVELET_TYPE~~=`CCCCCCCC      
! WAVELET_SHIFT =`FFFFFFFFFFF
! WAVELET_LENGTH=`FFFFFFFFFFF
! WAVELET_INC~~~=`FFFFFFFFFFF
! WAVELET_FFT~~~=`IIIIIIIIIII
! WAVELET_NUM~~~=`IIIIIIIIIII
! WAVELET_FREQ_LOW_NONE =`FFFFFFFFFFF  WAVELET_FREQ_LOW_FULL =`FFFFFFFFFFF   
! WAVELET_FREQ_HIGH_FULL=`FFFFFFFFFFF  WAVELET_FREQ_HIGH_NONE=`FFFFFFFFFFF   
! WAVELET_PHASE~~~~~~~~=`FFFFFFFFFFFF  WAVELET_FILTER_LEVEL~~=`CCCCCCC
! WAVELET_FREQ_POWER~~~=`FFFFFFFFFFFF  WAVELET_FREQ_RICKER~~~=`FFFFFFFFFFFF  
!
!`------------------------------------------------------------------------------
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="WAVELET_TYPE">
!<Tip> Wavelet type. </Tip>
! Default = GAUSSIAN2
! Allowed = GAUSSIAN0
! Allowed = GAUSSIAN1
! Allowed = GAUSSIAN2
! Allowed = SPIKE
! Allowed = RICKER
!  Parameter WAVELET_TYPE controls the type of wavelet convolved with the
!  output traces.  Currently a spike, a ricker, a gaussian or its first two
!  derivitives is supported.
!
!  For a ricker wavelet the expression is:
!
!  WAVELET(TIME) = ( 1 - 2 * ( PI * WAVELET_FREQ_RICKER * TIME ) **2 ) 
!                  * EXP ( - ( PI * WAVELET_FREQ_RICKER * TIME ) **2 )
!
!  Where WAVELET_FREQ_RICKER is the central frequeuncy of the filter and is
!  (WAVELET_FREQ_LOW_NONE+WAVELET_FREQ_HIGH_NONE) * .5
!  Make sure WAVELET_LENGTH is large enough to capture the ricker wavelet.
!</Help>
!
!<Help KEYWORD="WAVELET_SHIFT">
!<Tip> Wavelet shift in time units. </Tip>
! Default = 0.
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="WAVELET_LENGTH">
!<Tip> Wavelet length in time units. </Tip>
! Default = 0.08
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="WAVELET_INC">
!<Tip> Wavelet increment in time units. </Tip>
! Default = 0.08
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="WAVELET_FFT">
!<Tip> Wavelet fft length in time samples. </Tip>
! Default = -1
! Allowed = integer scalar
! Using WAVELET_FFT = -1 means use the first power of 2 larger than WAVELET_NUM
! multiplied by 16.
! WAVELET_FFT defines the length of the fft used during filtering the wavelet
! in the frequency domain.  The frequency sampling during filtering is
! proportional to WAVELET_FFT.  Making WAVELET_FFT short may result in 
! sampling aritifacts.  Make WAVELET_FFT the next power of two larger than 
! the trace length that the wavelet will be used with.
!</Help>
!
!<Help KEYWORD="WAVELET_NUM">
!<Tip> Wavelet length in time samples. </Tip>
! Default = -1
! Allowed = integer scalar
!</Help>
!
!<Help KEYWORD="WAVELET_FREQ_LOW_NONE">
!<Tip> Low frequency limit where amp spectrum diminishes to 0.0, in Hz. </Tip>
! Default =  4
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="WAVELET_FREQ_LOW_FULL">
!<Tip> Low frequency point where amp spectrum is full pass, in Hz. </Tip>
! Default =  8
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="WAVELET_FREQ_HIGH_FULL">
!<Tip> High frequency point where amp spectrum is full pass, in Hz. </Tip>
! Default =  50
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="WAVELET_FREQ_HIGH_NONE">
!<Tip> High frequency limit where amp spectrum diminishes to 0.0, in Hz. </Tip>
! Default =  60.
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="WAVELET_PHASE">
!<Tip> Filter band wavelet_phase, in degrees. </Tip>
! Default =  0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="WAVELET_FILTER_LEVEL">
!<Tip> Apply the filter to the wavelet, trace both or neither. </Tip>
! Default = WAVELET
! Allowed = TRACE
! Allowed = BOTH
! Allowed = NONE
!  Parameter WAVELET_FILTER_LEVEL controls how the bandpass filter is applied 
!  to the wavelet and or the trace.  
!  It can be applied to either on, both or neiher.
!
!  If WAVELET_FILTER_LEVEL = WAVELET 
!  the filtered wavelet is convolved with spikes
!  to form the output trace.  The trace is not filtered after convolution.
!
!  If WAVELET_FILTER_LEVEL = TRACE 
!  the unfiltered wavelet is convolved with spikes
!  to form the output trace.  The trace is filtered after convolution.
!
!  If WAVELET_FILTER_LEVEL = BOTH 
!  the filtered wavelet is convolved with spikes
!  to form the output trace.  The trace is filtered after convolution.
!
!  If WAVELET_FILTER_LEVEL = NONE 
!  the unfiltered wavelet is convolved with spikes
!  to form the output trace.  The trace is not filtered after convolution.
!
!  Filtering the traces tend to give larger side lobes than filtering
!  the wavelet only.
!</Help>
!
!<Help KEYWORD="WAVELET_FREQ_POWER">
!<Tip> Frequency power to scale wavelet by. </Tip>
! Default =  0.
! Allowed = real scalar
! Modify wavelet by
! OUTPUT = INPUT * FRREQUENCY ** WAVEET_FREQ_POWER
!</Help>
!
!<Help KEYWORD="WAVELET_FREQ_RICKER">
!<Tip> Central frequency for Ricker wavelet. </Tip>
! Default =  -1.
! Allowed = real scalar
! Using WAVELET_FREQ_RICKER - -1 sets the central wavelet frequency
! to .5 * ( WAVELET_FREQ_LOW_NONE + WAVELET_FREQ_HIGH_NONE )
!</Help>
!
!</HelpSection>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

module wavelet_module
  !
  ! Module references
  !
  use amod_module
  use getsys_module
  use matfun_module
  use memfun_module
  use named_constants_module
  use pc_module
  use pcpsx_module
  use pp_module
  use string_module
  use timeglob_module
  use trcio_module
  !
  implicit  none
  !
  public
  !
  public :: wavelet_create               ! create     the wavelet structure
  public :: wavelet_delete               ! delete     the wavelet structure
  public :: wavelet_initialize           ! initialize the wavelet structure
  public :: wavelet_update               ! update     the wavelet structure
  public :: wavelet_get                  ! get        the wavelet structure
  public :: wavelet_put                  ! put        the wavelet structure
  public :: wavelet_get_process          ! 
  public :: wavelet_put_process          ! 
  public :: wavelet_verify               ! verify     the wavelet structure
  public :: wavelet_sensitive            ! sensitive  the wavelet structure
  public :: wavelet_all                  !   allocate wavelet memory
  public :: wavelet_del                  ! deallocate wavelet memory
  public :: wavelet_nul                  ! nullify    wavelet memory
  public :: wavelet_print                ! print wavelet info
  public :: wavelet_compute              ! compute a wavelet 
  public :: wavelet_apply                ! apply a wavelet to a trace
  public :: wavelet_spike                ! compute a spike wavelet
  public :: wavelet_gaussian             ! compute a gaussian wavelet
  !
  ! interfaces
  !
  character (len=100), public, save :: WAVELET_IDENT = &
    '$Id: wavelet.f90,v 1.12 2007/04/13 14:10:14 Hanson beta sps $'
  !
  integer,          parameter :: n_wavelet_filter_level = 4
  character(len=8), save      :: c_wavelet_filter_level &
                               ( n_wavelet_filter_level ) &
  = (/'WAVELET ' , 'TRACE   ' , 'BOTH    ' , 'NONE    '/)
  !
  integer,          parameter :: n_wavelet_type = 5
  character(len=9), save      :: c_wavelet_type ( n_wavelet_type ) &
  = (/'GAUSSIAN0' , 'GAUSSIAN1' , 'GAUSSIAN2' , &
  &    'SPIKE    ' , 'RICKER   '/)
  !
  type, public :: wavelet_struct
    !
    character(len=9)                   :: wavelet_type
    real                               :: wavelet_shift
    real                               :: wavelet_length
    real                               :: wavelet_inc
    integer                            :: wavelet_fft
    integer                            :: wavelet_num
    real                               :: wavelet_freq_low_none
    real                               :: wavelet_freq_low_full
    real                               :: wavelet_freq_high_full
    real                               :: wavelet_freq_high_none
    real                               :: wavelet_phase
    real                               :: wavelet_freq_power
    real                               :: wavelet_freq_ricker
    real                               :: wavelet_min
    character(len=8)                   :: wavelet_filter_level
    !
    character(len=32)                  :: c_title
    logical                            :: wavelet_filter_level_w
    logical                            :: wavelet_filter_level_t
    logical                            :: apply_filter
    !
    integer                            :: nt_glb ! glb t num
    real                               :: t0_glb ! glb t min
    real                               :: t1_glb ! glb t max
    real                               :: dt_glb ! glb t inc
    !
    integer                            :: nt_wlt ! wlt t num
    real                               :: t0_wlt ! wlt t min
    real                               :: t1_wlt ! wlt t maxnum
    real                               :: dt_wlt ! wlt t inc
    !
    integer                            :: nf_wlt ! wlt f num
    real                               :: f0_wlt ! wlt f min
    real                               :: f1_wlt ! wlt f maxnum
    real                               :: df_wlt ! wlt f inc
    !
    integer                            :: nw_wlt ! wlt w num
    real                               :: w0_wlt ! wlt w min
    real                               :: w1_wlt ! wlt w maxnum
    real                               :: dw_wlt ! wlt w inc
    !
    real,                      pointer :: wavelet_values(:)
    real,                      pointer :: wavelet_times(:)
    real,                      pointer :: rt_wlt(:)
    real,                      pointer :: ra_wlt(:)
    real,                      pointer :: rf_wlt(:)
    real,                      pointer :: rw_wlt(:)
    complex,                   pointer :: ca_wlt(:)
    !
  end type wavelet_struct
  !
  contains
  !
  subroutine wavelet_create ( wlt, c_title, i_err )
    !
    ! create the wavelet velocity structue
    !
    type ( wavelet_struct ),   pointer :: wlt      ! wavelet structure
    character(len=*),    intent(in   ) :: c_title  ! title
    integer,             intent(inout) :: i_err    ! error 0 O.K. -1 err
    !
    ! Local variables
    !
    !
    i_err = 0
    !
    ! allocate the structure
    !
    if ( .not. associated ( wlt ) ) &
                 allocate ( wlt )
    !
    ! initalize structure coefficients
    !
    call wavelet_initialize ( wlt )
    !
    wlt%c_title = c_title
    !
    call wavelet_all ( wlt, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call wavelet_update ( wlt )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " wavelet_create " , /, " REVISION: ", &
    &" 12  2007-04-12  Douglas Hanson Add wavelet_get_process. " &
    & )')
    !
    ! print the velocity structure info without the coefficients themselves
    !
    !if ( .not. pc_do_not_process_traces() ) &
    !call wavelet_print ( wlt, pc_get_lun(), ' wavelet_create start ' )
    !
    return
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in wavelet_create ", &
    & /, " during wavelet_all " &
    & )')
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in wavelet_create " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine wavelet_create
  !
  subroutine wavelet_delete ( wlt )
    !
    ! delete the wavelet structure
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    !
    ! Local variables
    !
    call wavelet_del ( wlt )
    !
    ! delalocate the velocity structure
    !
    if ( associated ( wlt ) ) &
         deallocate ( wlt )
    !
    return
    !
  end subroutine wavelet_delete
  !
  subroutine wavelet_initialize ( wlt )
    !
    ! verify _parameters
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    !
    integer                            :: i_err   ! error 0 O.K. -1 err
    !
    i_err = 0
    !
    call wavelet_initialize_0 ( wlt )
    !
    ! get the current globals
    !
    call timeglob_get ( wlt%nt_glb, wlt%t0_glb, wlt%t1_glb, wlt%dt_glb )
    !
    wlt%wavelet_type   = 'GAUSSIAN2' ! second derivitive of a gaussian
    wlt%wavelet_shift  = 0.          ! wavelet shift in seconds
    wlt%wavelet_length = .08         ! wavelet length in seconds
    wlt%wavelet_min    = 0.          ! wavelet min in seconds
    wlt%wavelet_inc    = wlt%dt_glb  ! wavelet inc in seconds
    wlt%wavelet_fft    = -1
    wlt%wavelet_num    = max ( 1 , nint ( &
    wlt%wavelet_length / wlt%wavelet_inc ) ) ! wavelet len in samples
    wlt%wavelet_freq_low_none  =  5.
    wlt%wavelet_freq_low_full  =  8.
    wlt%wavelet_freq_high_full = 50.
    wlt%wavelet_freq_high_none = 60.
    wlt%wavelet_phase          =  0.
    wlt%wavelet_filter_level   = 'WAVELET'
    wlt%wavelet_freq_power     =  0.
    wlt%wavelet_freq_ricker    = -1.
    !
    ! verify parameters
    !
    call wavelet_verify ( wlt )
    !
    return
    !
  end subroutine wavelet_initialize
  !
  subroutine wavelet_initialize_0 ( wlt )
    !
    ! verify _parameters
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    !
    integer                            :: i_err   ! error 0 O.K. -1 err
    !
    i_err = 0
    !
    call memfun_init ( wlt%wavelet_type )
    call memfun_init ( wlt%wavelet_shift )
    call memfun_init ( wlt%wavelet_length )
    call memfun_init ( wlt%wavelet_min )
    call memfun_init ( wlt%wavelet_inc )
    call memfun_init ( wlt%wavelet_fft )
    call memfun_init ( wlt%wavelet_num )
    call memfun_init ( wlt%wavelet_freq_low_none )
    call memfun_init ( wlt%wavelet_freq_low_full )
    call memfun_init ( wlt%wavelet_freq_high_full )
    call memfun_init ( wlt%wavelet_freq_high_none )
    call memfun_init ( wlt%wavelet_phase )
    call memfun_init ( wlt%wavelet_filter_level )
    call memfun_init ( wlt%wavelet_freq_power )
    call memfun_init ( wlt%wavelet_freq_ricker )
    call memfun_init ( wlt%c_title )
    call memfun_init ( wlt%wavelet_filter_level_w )
    call memfun_init ( wlt%wavelet_filter_level_t )
    call memfun_init ( wlt%apply_filter )
    call memfun_init ( wlt%nt_glb )
    call memfun_init ( wlt%t0_glb )
    call memfun_init ( wlt%t1_glb )
    call memfun_init ( wlt%dt_glb )
    !
    call memfun_init ( wlt%nt_wlt ) ! wlt t num
    call memfun_init ( wlt%t0_wlt ) ! wlt t min
    call memfun_init ( wlt%t1_wlt ) ! wlt t maxnum
    call memfun_init ( wlt%dt_wlt ) ! wlt t inc
    !
    call memfun_init ( wlt%nf_wlt ) ! wlt f num
    call memfun_init ( wlt%f0_wlt ) ! wlt f min
    call memfun_init ( wlt%f1_wlt ) ! wlt f maxnum
    call memfun_init ( wlt%df_wlt ) ! wlt f inc
    !
    call memfun_init ( wlt%nw_wlt ) ! wlt w num
    call memfun_init ( wlt%w0_wlt ) ! wlt w min
    call memfun_init ( wlt%w1_wlt ) ! wlt w maxnum
    call memfun_init ( wlt%dw_wlt ) ! wlt w inc
    !
    call wavelet_nul ( wlt )
    !
    return
    !
  end subroutine wavelet_initialize_0 
  !
  subroutine wavelet_update ( wlt )
    !
    ! update parameters
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    !
    call wavelet_get ( wlt )
    !
    call wavelet_verify ( wlt )
    !
    call wavelet_put ( wlt )
    !
    return
    !
  end subroutine wavelet_update 
  !
  subroutine wavelet_get ( wlt )
    !
    ! get parameters
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    !
    !
    call pc_get ( 'wavelet_type',           wlt%wavelet_type             )
    call pc_get ( 'wavelet_shift',          wlt%wavelet_shift            )
    call pc_get ( 'wavelet_length',         wlt%wavelet_length           )
    call pc_get ( 'wavelet_inc',            wlt%wavelet_inc              )
    call pc_get ( 'wavelet_fft',            wlt%wavelet_fft              )
    call pc_get ( 'wavelet_num',            wlt%wavelet_num              )
    call pc_get ( 'wavelet_freq_low_none',  wlt%wavelet_freq_low_none    )
    call pc_get ( 'wavelet_freq_low_full',  wlt%wavelet_freq_low_full    )
    call pc_get ( 'wavelet_freq_high_full', wlt%wavelet_freq_high_full   )
    call pc_get ( 'wavelet_freq_high_none', wlt%wavelet_freq_high_none   )
    call pc_get ( 'wavelet_phase',          wlt%wavelet_phase            )
    call pc_get ( 'wavelet_filter_level',   wlt%wavelet_filter_level     )
    call pc_get ( 'wavelet_freq_power',     wlt%wavelet_freq_power       )
    call pc_get ( 'wavelet_freq_ricker',    wlt%wavelet_freq_ricker      )
    !
    return
    !
  end subroutine wavelet_get
  !
  subroutine wavelet_put ( wlt )
    !
    ! put parameters
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    !
    call amod_line_feed ( 'wavelet_parameters' )
    !
    call pc_put ( 'wavelet_type',           wlt%wavelet_type             )
    call pc_put ( 'wavelet_shift',          wlt%wavelet_shift            )
    call pc_put ( 'wavelet_length',         wlt%wavelet_length           )
    call pc_put ( 'wavelet_inc',            wlt%wavelet_inc              )
    call pc_put ( 'wavelet_fft',            wlt%wavelet_fft              )
    call pc_put ( 'wavelet_num',            wlt%wavelet_num              )
    !
    call amod_line_feed ( 'wavelet_filter' )
    !
    call pc_put ( 'wavelet_freq_low_none',  wlt%wavelet_freq_low_none    )
    call pc_put ( 'wavelet_freq_low_full',  wlt%wavelet_freq_low_full    )
    call pc_put ( 'wavelet_freq_high_full', wlt%wavelet_freq_high_full   )
    call pc_put ( 'wavelet_freq_high_none', wlt%wavelet_freq_high_none   )
    call pc_put ( 'wavelet_phase',          wlt%wavelet_phase            )
    call pc_put ( 'wavelet_filter_level',   wlt%wavelet_filter_level     )
    call pc_put ( 'wavelet_freq_power',     wlt%wavelet_freq_power       )
    call pc_put ( 'wavelet_freq_ricker',    wlt%wavelet_freq_ricker      )
    !
    return
    !
  end subroutine wavelet_put
  !
  subroutine wavelet_get_process ( wlt )
    !
    ! get process parameters
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    !
call pc_get_process ( 'wavelet_type',           wlt%wavelet_type             )
call pc_get_process ( 'wavelet_shift',          wlt%wavelet_shift            )
call pc_get_process ( 'wavelet_length',         wlt%wavelet_length           )
call pc_get_process ( 'wavelet_inc',            wlt%wavelet_inc              )
call pc_get_process ( 'wavelet_fft',            wlt%wavelet_fft              )
call pc_get_process ( 'wavelet_num',            wlt%wavelet_num              )
    !
call pc_get_process ( 'wavelet_freq_low_none',  wlt%wavelet_freq_low_none    )
call pc_get_process ( 'wavelet_freq_low_full',  wlt%wavelet_freq_low_full    )
call pc_get_process ( 'wavelet_freq_high_full', wlt%wavelet_freq_high_full   )
call pc_get_process ( 'wavelet_freq_high_none', wlt%wavelet_freq_high_none   )
call pc_get_process ( 'wavelet_phase',          wlt%wavelet_phase            )
call pc_get_process ( 'wavelet_filter_level',   wlt%wavelet_filter_level     )
call pc_get_process ( 'wavelet_freq_power',     wlt%wavelet_freq_power       )
call pc_get_process ( 'wavelet_freq_ricker',    wlt%wavelet_freq_ricker      )
    !
    return
    !
  end subroutine wavelet_get_process 
  !
  subroutine wavelet_put_process ( wlt )
    !
    ! put process parameters
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    !
call pc_put_process ( 'wavelet_type',           wlt%wavelet_type             )
call pc_put_process ( 'wavelet_shift',          wlt%wavelet_shift            )
call pc_put_process ( 'wavelet_length',         wlt%wavelet_length           )
call pc_put_process ( 'wavelet_inc',            wlt%wavelet_inc              )
call pc_put_process ( 'wavelet_fft',            wlt%wavelet_fft              )
call pc_put_process ( 'wavelet_num',            wlt%wavelet_num              )
    !
call pc_put_process ( 'wavelet_freq_low_none',  wlt%wavelet_freq_low_none    )
call pc_put_process ( 'wavelet_freq_low_full',  wlt%wavelet_freq_low_full    )
call pc_put_process ( 'wavelet_freq_high_full', wlt%wavelet_freq_high_full   )
call pc_put_process ( 'wavelet_freq_high_none', wlt%wavelet_freq_high_none   )
call pc_put_process ( 'wavelet_phase',          wlt%wavelet_phase            )
call pc_put_process ( 'wavelet_filter_level',   wlt%wavelet_filter_level     )
call pc_put_process ( 'wavelet_freq_power',     wlt%wavelet_freq_power       )
call pc_put_process ( 'wavelet_freq_ricker',    wlt%wavelet_freq_ricker      )
    !
    return
    !
  end subroutine wavelet_put_process
  !
  subroutine wavelet_verify ( wlt )
    !
    ! verify parameters
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    !
    integer                            :: i_err   ! error 0 O.K. -1 err
    !
    i_err = 0
    !
    wlt%wavelet_num    = max ( 1, nint ( wlt%wavelet_length &
                                       / wlt%wavelet_inc  + 1 ) )
    !
    if ( wlt%wavelet_fft .lt. 0 ) &
    wlt%wavelet_fft = - 16 * matfun_pow2 ( wlt%wavelet_num )
    !
    if ( wlt%wavelet_freq_ricker .lt. 0. ) &
    wlt%wavelet_freq_ricker = &
    - .5 * ( wlt%wavelet_freq_low_none + wlt%wavelet_freq_high_none )
    !
    call pc_put_options_field ( &
    'wavelet_filter_level', c_wavelet_filter_level, n_wavelet_filter_level )
    !
    call pc_put_options_field ( &
    'wavelet_type', c_wavelet_type, n_wavelet_type )
    !
    ! set the wavelet_filter_level flags for the wavelet and the trace
    !
    call wavelet_set_filter_level ( wlt )
    !
    call wavelet_sensitive ( wlt )
    !
    return
    !
  end subroutine wavelet_verify
  !
  subroutine wavelet_sensitive ( wlt )
    !
    ! turn anisotropic coefficients sensitive
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    logical                            :: l_wavelet_inc
    logical                            :: l_wavelet_shift
    logical                            :: l_wavelet_ricker 
    !
    l_wavelet_inc = string_upper_compare ( wlt%c_title, 'kmig' )
    !
    l_wavelet_shift = string_upper_compare ( wlt%c_title, 'fxmig' ) &
                 .or. string_upper_compare ( wlt%c_title, 'kmig' )
    !
    l_wavelet_ricker = string_upper_compare ( wlt%wavelet_type(1:6), 'RICKER' ) 
    !
    call pc_put_sensitive_field_flag ( 'wavelet_inc', l_wavelet_inc )
    !
    call pc_put_sensitive_field_flag ( 'wavelet_num',  .false. )
    !
    call pc_put_visible_flag ( 'wavelet_shift', l_wavelet_shift )
    !
    call pc_put_sensitive_field_flag ( 'wavelet_freq_ricker', l_wavelet_ricker )
    !
    !xxif_fxmig : if ( string_upper_compare ( wlt%c_title, 'fxmig' ) ) then
      !
      ! for fxmig turn off the wavelet freq controls
      !
      !call pc_put_sensitive_field_flag ( 'wavelet_freq_low_none',  .false. )
      !call pc_put_sensitive_field_flag ( 'wavelet_freq_low_full',  .false. )
      !call pc_put_sensitive_field_flag ( 'wavelet_freq_high_full', .false. )
      !call pc_put_sensitive_field_flag ( 'wavelet_freq_high_none', .false. )
      !
    !end if xxif_fxmig 
    !
    return
    !
  end subroutine wavelet_sensitive
  !
  subroutine wavelet_all ( wlt, i_err )
    !
    ! allocate parameters
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    integer,             intent(inout) :: i_err   ! error 0 O.K. -1 err
    !
    i_err = 0
    !
    call memfun_all ( &
    wlt%wavelet_values, wlt%wavelet_num, 'wavelet_values', i_err )
    call memfun_all ( &
    wlt%wavelet_times, wlt%wavelet_num, 'wavelet_times', i_err )
    call memfun_all ( wlt%rt_wlt, wlt%nt_wlt, 'ra_wlt', i_err )
    call memfun_all ( wlt%ra_wlt, wlt%nt_wlt, 'ra_wlt', i_err )
    call memfun_all ( wlt%rf_wlt, wlt%nf_wlt, 'rf_wlt', i_err )
    call memfun_all ( wlt%rw_wlt, wlt%nw_wlt, 'rw_wlt', i_err )
    call memfun_all ( wlt%ca_wlt, wlt%nf_wlt, 'ca_wlt', i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    return
    !
998 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in wavelet_all ", &
    & /, " during memory allocation " &
    & )')
    go to 999
    !
999 continue
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), ' ( &
    & /, " error in wavelet_all " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine wavelet_all
  !
  subroutine wavelet_del ( wlt )
    !
    ! deallocate parameters
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    !
    call memfun_del ( wlt%wavelet_values ) 
    call memfun_del ( wlt%wavelet_times ) 
    call memfun_del ( wlt%rt_wlt )
    call memfun_del ( wlt%ra_wlt )
    call memfun_del ( wlt%rf_wlt )
    call memfun_del ( wlt%rw_wlt )
    call memfun_del ( wlt%ca_wlt )
    !
    return
    !
  end subroutine wavelet_del
  !
  subroutine wavelet_nul ( wlt )
    !
    ! nullify parameters
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    !
    call memfun_nul ( wlt%wavelet_values ) 
    call memfun_nul ( wlt%wavelet_times ) 
    call memfun_nul ( wlt%rt_wlt )
    call memfun_nul ( wlt%ra_wlt )
    call memfun_nul ( wlt%rf_wlt )
    call memfun_nul ( wlt%rw_wlt )
    call memfun_nul ( wlt%ca_wlt )
    !
    return
    !
  end subroutine wavelet_nul
  !
  subroutine wavelet_prep ( wlt, i_err )
    !
    ! prep  spike parameters
    !
    ! Arguments
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    integer,             intent(  out) :: i_err
    !
    i_err = 0
    !
    if ( pc_do_not_process_traces() ) return
    !
    call wavelet_verify ( wlt )
    !
    ! compute the applied wavelet
    !
    call wavelet_compute ( wlt, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in wavelet_prep  ", &
    & /," during wavelet_compute " &
    & )')
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in wavelet_prep  " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine wavelet_prep
  !
  subroutine wavelet_compute ( wlt, i_err )
    !
    ! create a wavelet object
    !
    ! c_title        = character string for printing
    ! wavelet_type   = type of wavelet flag
    !      SPIKE     = constant spike length 1 amplitude 1.
    !      GAUSSIAN0 = gaussian wavelet
    !      GAUSSIAN1 = first derivitive of a gaussian wavelet
    !      GAUSSIAN2 = second derivitive of a gaussian wavelet
    ! wavelet_length = wavelet length in time units
    ! wavelet_inc    = wavelet time increment in time units
    ! wavelet_values = wavelet array
    !
    ! The wavelet is filtered with a bandpass fitler defined by the following:
    ! wavelet_freq_low_none  = frequency, in hertz, at start of lower taper
    ! wavelet_freq_low_full  = frequency, in hertz, at end   of lower taper
    ! wavelet_freq_high_full = frequency, in hertz, at start of upper taper
    ! wavelet_freq_high_none = frequency, in hertz, at end   of upper taper
    ! wavelet_phase  = additional wavelet_phase, in degrees, added to wavelet
    ! apply_filter   = flag to apply frtequency filter to wavelet
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    integer,             intent(  out) :: i_err
    !
    !character(len=filename_length)    :: path_root     ! file root
    !character(len=filename_length)    :: path_out      ! file name
    !character(len=filename_length)    :: job_name      ! job_name
    !integer                           :: nh_out 
    !double precision                  :: hd_out ( 64,    2 )
    !real                              :: tr_out ( 10000, 3 )
    real                               :: tr_fft ( abs(wlt%wavelet_fft) )
    integer                            :: jf_wlt
    !
    ! Local variables
    !
    i_err = 0
    !
    ! cap the wavelet type
    !
    call string_to_upper ( wlt%wavelet_type )
    !
    wlt%wavelet_num    = max ( 1, nint ( wlt%wavelet_length &
                                       / wlt%wavelet_inc  + 1 ) )
    !
    wlt%wavelet_length = ( wlt%wavelet_num - 1 ) * wlt%wavelet_inc
    !
    if ( wlt%wavelet_freq_ricker .lt. 0. ) &
    wlt%wavelet_freq_ricker = &
    - .5 * ( wlt%wavelet_freq_low_none + wlt%wavelet_freq_high_none )
    !
    if ( wlt%wavelet_fft .lt. 0 ) &
    wlt%wavelet_fft = - 16 * matfun_pow2 ( wlt%wavelet_num )
    !
    wlt%nt_wlt = abs ( wlt%wavelet_fft )
    !
    wlt%nt_wlt = max ( wlt%nt_wlt, matfun_pow2 ( wlt%wavelet_num ) )
    !
    wlt%t0_wlt = 0.
    !
    wlt%dt_wlt = wlt%wavelet_inc
    !
    wlt%t1_wlt = ( wlt%nt_wlt - 1 ) * wlt%dt_wlt + wlt%t0_wlt 
    !
    wlt%nf_wlt = wlt%nt_wlt / 2 + 1
    !
    wlt%f0_wlt = 0.
    !
    wlt%f1_wlt = .5 / wlt%dt_wlt
    !
    wlt%df_wlt = ( wlt%f1_wlt - wlt%f0_wlt ) / ( wlt%nf_wlt - 1 ) 
    !
    wlt%nw_wlt = wlt%nf_wlt 
    !
    wlt%w0_wlt = matfun_htz_to_rad ( wlt%f0_wlt ) 
    !
    wlt%dw_wlt = matfun_htz_to_rad ( wlt%df_wlt )
    !
    wlt%w1_wlt = matfun_htz_to_rad ( wlt%f1_wlt )
    !
    ! allocate memory for the wavelet wavelet_values
    !
    call wavelet_all ( wlt, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call matfun_line ( wlt%wavelet_num, wlt%wavelet_times, &
    wlt%wavelet_min, wlt%wavelet_inc )
    call matfun_line ( wlt%nt_wlt, wlt%rt_wlt, wlt%t0_wlt, wlt%dt_wlt )
    call matfun_line ( wlt%nf_wlt, wlt%rf_wlt, wlt%f0_wlt, wlt%df_wlt )
    call matfun_line ( wlt%nw_wlt, wlt%rw_wlt, wlt%w0_wlt, wlt%dw_wlt )
    !
    !print'(" wavelet_compute wavelet_type=",a)', &
    !trim(wlt%wavelet_type)
    !
    xxif_wavelet_type : &
    if ( string_upper_compare ( wlt%wavelet_type(1:5), 'SPIKE' ) ) then 
      !
      !print'(" wavelet_compute bef wavelet_spike ")'
      !
      ! construct a spike wavelet length 1, amplitude 1.
      !
      call wavelet_spike ( wlt%wavelet_type,    &
                           wlt%wavelet_num,     &
                           wlt%wavelet_inc,     &
                           wlt%wavelet_values )
      !
    else if ( string_upper_compare ( wlt%wavelet_type(1:6), 'RICKER' ) ) then 
      !
      ! construct a ricker wavelet 
      !
      !print'(" wavelet_compute bef wavelet_ricker  ")'
      !
      call wavelet_ricker ( wlt%wavelet_type,       &
                            wlt%wavelet_num,        &
                            wlt%wavelet_inc,        &
                            wlt%wavelet_values,     &
                            wlt%wavelet_freq_ricker )
      !
    else if ( string_upper_compare ( wlt%wavelet_type(1:8), 'GAUSSIAN' ) ) then
      !
      ! construct a gaussian wavelet or its first two derivitves
      !
      !print'(" wavelet_compute bef wavelet_gaussian  ")'
      !
      call wavelet_gaussian ( wlt%wavelet_type,    &
                              wlt%wavelet_num,     &
                              wlt%wavelet_inc,     &
                              wlt%wavelet_values)
      !
    else xxif_wavelet_type 
      !
      ! this wavelet type is not supported
      !
      go to 997
      !
    end if xxif_wavelet_type 
    !
    ! normalize the wavelet to 1.
    !
    call matfun_normalize_r ( nx = wlt%wavelet_num, &
                              x  = wlt%wavelet_values, &
                              x0 = 1. )
    !
    tr_fft = 0.
    !
    tr_fft ( 1:wlt%wavelet_num ) = wlt%wavelet_values ( 1:wlt%wavelet_num )
    !
    !hd_out = 0.
    !
    !tr_out = 0.
    !
    !tr_out ( 1:wlt%wavelet_num, 1 ) = wlt%wavelet_values ( 1:wlt%wavelet_num )
    !
    ! filter the wavelet
    !
    if ( wlt%apply_filter ) &
    call matfun_filter ( freq_low_none  = wlt%wavelet_freq_low_none,     &
                         freq_low_full  = wlt%wavelet_freq_low_full,     &
                         freq_high_full = wlt%wavelet_freq_high_full,    &
                         freq_high_none = wlt%wavelet_freq_high_none,    &
                         freq_phase     = wlt%wavelet_phase,             &
                        !nt_inp         = wlt%wavelet_num,               &
                         nt_inp         = wlt%nt_wlt,                    &
                         t0_inp         = 0.,                            &
                         dt_inp         = wlt%wavelet_inc,               &
                         tr_inp         = tr_fft,                        &
                        !tr_inp         = wlt%wavelet_values,            &
                         i_err          = i_err                          &
                       )
    !
    if ( i_err .ne. 0 ) go to 996
    !
    wlt%wavelet_values ( 1:wlt%wavelet_num ) = tr_fft ( 1:wlt%wavelet_num ) 
    !
    !tr_out ( 1:wlt%wavelet_num, 2 ) = wlt%wavelet_values ( 1:wlt%wavelet_num )
    !
    wlt%ra_wlt = 0.
    !
    wlt%ra_wlt (1:wlt%wavelet_num) = wlt%wavelet_values (1:wlt%wavelet_num) 
    !
    ! construct the wavelet in the frequency domain from the time domain 
    !
    call matfun_fft ( +1, wlt%nt_wlt, wlt%ra_wlt, wlt%ca_wlt )
    !
    ! scale by wavelet_freq_power
    !
    !print'(" aa1_wavelet_compute wavelet_freq_power=",g12.6, &
    !& " nt_wlt=",i8," nf_wlt=",i8)', &
    !wlt%wavelet_freq_power, wlt%nt_wlt, wlt%nf_wlt 
    !
    do_jf_wlt : do jf_wlt = 1 , wlt%nf_wlt 
      !
    !print'(1x,i8,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6,1x,g12.6, &
    !& " aa1_wavelet_compute " )', &
    !jf_wlt, wlt%rf_wlt(jf_wlt), &
    !wlt%rf_wlt(jf_wlt) ** wlt%wavelet_freq_power / float ( wlt%nt_wlt ), &
    !wlt%ca_wlt(jf_wlt), &
    !wlt%ca_wlt(jf_wlt) &
    !* wlt%rf_wlt(jf_wlt) ** wlt%wavelet_freq_power / float ( wlt%nt_wlt )
      !
      wlt%ca_wlt(jf_wlt) = &
      wlt%ca_wlt(jf_wlt) &
    * wlt%rf_wlt(jf_wlt) ** wlt%wavelet_freq_power / float ( wlt%nt_wlt )
      !
    end do do_jf_wlt 
    !
    !print'(" aa1 wavelet_compute ra_wlt=",g12.6)', maxval(abs(wlt%ra_wlt))
    !
    ! construct the wavelet in the time domain from the frequency domain
    !
    call matfun_fft ( -1, wlt%nt_wlt, wlt%ca_wlt, wlt%ra_wlt )
    !
    wlt%wavelet_values (1:wlt%wavelet_num) = wlt%ra_wlt (1:wlt%wavelet_num) 
    !
    !tr_out ( 1:wlt%wavelet_num, 3 ) = wlt%wavelet_values ( 1:wlt%wavelet_num )
    !
    !print'(" aa2 wavelet_compute ra_wlt=",g12.6)', maxval(abs(wlt%ra_wlt))
    !
    call wavelet_print ( wlt, pc_get_lun(), wlt%c_title )
    !
    !call pc_get_jdata ( 'jobname', job_name )
    !
    !call pc_get_global ( 'nwih', nh_out )
    !
    !path_root = '/home/hansodw/jbs/'
    !
    !path_out = trim ( path_root ) // trim ( job_name ) // '_wavelet.trc'
    !
    !call wavelet_trace ( &
    !                     path_out, &
    !                     3, nh_out, wlt%wavelet_num, 0., wlt%wavelet_inc, &
    !                     hd_out, tr_out, &
    !                     i_err &
    !                   )
    !
    !if ( i_err .ne. 0 ) go to 995
    !
    return
    !
995 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in wavelet_compute " &
    & /, " during wavelet_trace " &
    & )')
    !
    go to 999
    !
996 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in wavelet_compute " &
    & /, " during matfun_filter " &
    & )')
    !
    go to 999
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in wavelet_compute " &
    & /, " in wavelet_type=", a &
    & )') &
    trim ( wlt%wavelet_type )
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in wavelet_compute " &
    & /, " during memory allocate -- wavelet_values " &
    & )')
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in wavelet_compute " &
    & )')
    !
    wlt%wavelet_num = 0
    !
    call wavelet_print ( wlt, pc_get_lun(), wlt%c_title )
    !
    i_err = -1
    !
    return
    !
  end subroutine wavelet_compute 
  !
  subroutine wavelet_apply ( wlt, nx_inp, x_inp, x_out )
    !
    ! apply a wavelet object by convolving the wavelet with x_inp
    ! x_out = wavelet convolved with x_inp
    ! may be inplace
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    !
    integer,             intent(in   ) :: nx_inp
    real,                intent(inout) :: x_inp(:)
    real,                intent(inout) :: x_out(:)
    !integer                            :: jt_wlt
    !real                               :: x_tmp(nx_inp)
    !
    !x_tmp(1:nx_inp) = x_inp(1:nx_inp)
    !
    call matfun_convolve ( n1 = wlt%wavelet_num, &
                          !x1 = wlt%wavelet_values, &
                           x1 = wlt%ra_wlt, &
                           n2 = nx_inp, &
                           x2 = x_inp, &
                           x3 = x_out )
    !
    !print '( 1x, i8, 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, g12.6, &
    !& " aa1_wavelet_apply" )', &
    !( jt_wlt, (jt_wlt-1)*wlt%dt_wlt, x_tmp(jt_wlt), x_out(jt_wlt), &
    !wlt%ra_wlt(min(jt_wlt,wlt%nt_wlt)), jt_wlt = 1, nx_inp )
    !
    return
    !
  end subroutine wavelet_apply
  !
  subroutine wavelet_spike ( wavelet_type, wavelet_num, &
                             wavelet_inc, wavelet_values )
    !
    ! compute a spike wavelet
    ! wavelet_type   = type of wavelet
    ! wavelet_num    = number of elements in wavelet
    ! wavelet_inc    = time increment
    ! wavelet_values = wavelet array
    !
    character(len=*),    intent(in   ) :: wavelet_type*(*)
    integer,             intent(in   ) :: wavelet_num
    real,                intent(in   ) :: wavelet_inc
    real,                intent(  out) :: wavelet_values  ( wavelet_num)
    !
    ! Local variables
    !
    integer                            :: wavelet_center
    !
    ! compute the center of the wavelet, wavelet_center
    !
    wavelet_center = wavelet_num / 2 + 1
    !
    ! set the wavelet to a spike at time sample wavelet center, amplitude 1.
    !
    wavelet_values(:) = 0.
    wavelet_values ( wavelet_center) = 1.
    !
    return
    !
  end subroutine wavelet_spike
  !
  subroutine wavelet_ricker  ( wavelet_type, wavelet_num,    &
                               wavelet_inc, wavelet_values, &
                               wavelet_freq_ricker )
    !
    ! compute a ricker wavelet
    ! wavelet_type   = type of wavelet
    ! wavelet_num    = number of elements in wavelet
    ! wavelet_inc    = time increment
    ! wavelet_values = wavelet array
    !
    character(len=*),    intent(in   ) :: wavelet_type*(*)

    integer,             intent(in   ) :: wavelet_num
    real,                intent(in   ) :: wavelet_inc
    real,                intent(  out) :: wavelet_values  ( wavelet_num)
    real,                intent(in   ) :: wavelet_freq_ricker
    !
    ! Local variables
    !
    integer                            :: wavelet_center
    integer                            :: wavelet_left
    integer                            :: wavelet_right
    real                               :: time
    !
    ! initialize to zero
    !
    wavelet_values(:) = 0.
    !
    ! compute the center of the wavelet, wavelet_center
    !
    wavelet_center = wavelet_num / 2 + 1
    !
    wavelet_left = wavelet_center
    !
    ! compute the right side of the wavelet and copy it to the left side
    !
    loop_wavelet_right : do wavelet_right = wavelet_center , wavelet_num
      !
      time =  ( wavelet_right - wavelet_center ) * wavelet_inc
      !
      wavelet_values ( wavelet_right ) = &
 ( 1 - 2 * ( pi * wavelet_freq_ricker * time ) **2 ) &
 * exp ( - ( pi * wavelet_freq_ricker * time ) **2 )
      !
      ! set the left side to the right side
      !
      if ( wavelet_left .ge. 1 ) wavelet_values ( wavelet_left  ) = &
                                 wavelet_values ( wavelet_right )
      !
      wavelet_left = wavelet_left - 1
      !
    end do loop_wavelet_right 
    !
    return
    !
  end subroutine wavelet_ricker
  !
  subroutine wavelet_gaussian  ( wavelet_type, wavelet_num,    &
                                 wavelet_inc, wavelet_values )
    !
    ! compute a gaussian wavelet or its derivitives
    !   wavelet_type   = type of wavelet
    !   wavelet_num    = number of elements in wavelet
    !   wavelet_inc    = time increment
    !   wavelet_values = wavelet array
    !
    character(len=*),    intent(in   ) :: wavelet_type*(*)
    integer,             intent(in   ) :: wavelet_num
    real,                intent(in   ) :: wavelet_inc
    real,                intent(  out) :: wavelet_values  ( wavelet_num)
    !
    ! Local variables
    !
    integer                            :: i_wave
    integer                            :: wavelet_idx
    real                               :: t
    real                               :: t0
    real                               :: t1
    real                               :: tmax
    real                               :: t_wave
    real                               :: y0
    real                               :: y1
    real                               :: y2
    !
    ! determine the gaussian derivitive level
    !
    xxif_wavelet : &
    if ( string_upper_compare ( wavelet_type(1:9), 'GAUSSIAN0' ) ) then
      !
      i_wave = 0 ! zero derivitive of gaussian
      !
    else if ( string_upper_compare ( wavelet_type(1:9), 'GAUSSIAN1' ) ) then
      !
      i_wave = 1 ! first derivitive of gaussian
      !
    else if ( string_upper_compare ( wavelet_type(1:9), 'GAUSSIAN2' ) ) then
      !
      i_wave = 2 ! second derivitive of gaussian
      !
    else xxif_wavelet 
      !
      i_wave = 0 ! zero derivitive of gaussian
      !
    end if xxif_wavelet 
    !
    tmax =  ( wavelet_num - 1) * wavelet_inc
    t0   = tmax / 2.0
    t1   = tmax / 6.
    y0   = -1.0 / t1 ** 2
    y1   = 2.0 * y0
    !
    loop_wavelet_idx : do wavelet_idx = 1 , wavelet_num
      !
      t =  ( wavelet_idx - 1) * wavelet_inc
      t_wave = t - t0
      !
      ! gaussian
      !
      if (i_wave .eq. 0) then
        !
        y2 = 1

        !
        ! first derivitive
        !
      else if (i_wave .eq. 1) then    ! if (i_wave .eq. 0) then
        !
        y2 = - y1 * t_wave

        !
        ! second derivitive
        !
      else
        !
        y2 = - y1 * (1.0 + y1 * t_wave * t_wave)
        !
      end if    ! if (i_wave .eq. 0) then
      !
      wavelet_values ( wavelet_idx) = y2 * exp (y0 * t_wave ** 2)
      !
    end do loop_wavelet_idx ! do wavelet_idx = 1 , wavelet_num
    !
    return
    !
  end subroutine wavelet_gaussian
  !
  subroutine wavelet_print ( wlt, lu_out, c_title )
    !
    ! print a wavelet to unit lu_out with title c_title
    ! wavelet_type = type of wavelet flag
    ! wavelet_num = number of time points in wavelet
    ! wavelet_inc = wavelet time increment
    ! wavelet_values = wavelet array
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    integer,             intent(in   ) :: lu_out
    character(len=*),    intent(in   ) :: c_title*(*)
    !
    ! Local variables
    !
    character(len=32)                  :: a_title
    character(len=32)                  :: t_title
    character(len=32)                  :: f_title
    integer                            :: wavelet_idx
    integer                            :: jt_wlt
    integer                            :: jf_wlt
    integer, save                      :: i_call = 0
    !
    i_call = i_call + 1
    !
    a_title = trim( c_title ) // '_a'
    !
    t_title = trim( c_title ) // '_t'
    !
    f_title = trim( c_title ) // '_f'
    !
    call pp_create_file_name ( &
                               base_name = a_title, &
                               file_name = a_title, &
                               i_pn      = i_call, &
                               i_worker  = pcpsx_i_pel() &
                             )
    !
    call pp_create_file_name ( &
                               base_name = t_title, &
                               file_name = t_title, &
                               i_pn      = i_call, &
                               i_worker  = pcpsx_i_pel() &
                             )
    !
    call pp_create_file_name ( &
                               base_name = f_title, &
                               file_name = f_title, &
                               i_pn      = i_call, &
                               i_worker  = pcpsx_i_pel() &
                             )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( lu_out, '( &
    & /, " wavelet_print         c=", i8, &
    & /, " wavelet_print c_title  =", a, &
    & /, " wavelet_print a_title  =", a, &
    & /, " wavelet_print t_title  =", a, &
    & /, " wavelet_print f_title  =", a, &
    & /, " wavelet_type           =", a, &
    & /, " wavelet_num            =", i8, &
    & /, " wavelet_length         =", f12.4, &
    & /, " wavelet_inc            =", f12.4, &
    & /, " wavelet_freq_low_none  =", g12.6, &
    & /, " wavelet_freq_low_full  =", g12.6, &
    & /, " wavelet_freq_high_full =", g12.6, &
    & /, " wavelet_freq_high_none =", g12.6, &
    & /, " wavelet_phase          =", g12.6 &
    & /, " apply_filter           =", l2, &
    & /, " wavelet_filter_level_w =",l2, &
    & /, " wavelet_filter_level_t =",l2, &
    & /, " wavelet_filter_level   =",a &
    & )') &
    i_call, &
    trim ( c_title ), &
    trim ( a_title ), &
    trim ( t_title ), &
    trim ( f_title ), &
    trim ( wlt%wavelet_type ), &
    wlt%wavelet_num, &
    wlt%wavelet_length, &
    wlt%wavelet_inc, &
    wlt%wavelet_freq_low_none, &
    wlt%wavelet_freq_low_full, &
    wlt%wavelet_freq_high_full, &
    wlt%wavelet_freq_high_none, &
    wlt%wavelet_phase, &
    wlt%apply_filter, &
    wlt%wavelet_filter_level_w, &
    wlt%wavelet_filter_level_t, &
    trim ( wlt%wavelet_filter_level)
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( lu_out, '( 1x, i8, 1x, g12.6, 1x, g12.6, 1x, a )') &
    ( wavelet_idx, &
      wlt%wavelet_times ( wavelet_idx ), &
      wlt%wavelet_values ( wavelet_idx ), &
      trim(a_title), &
      wavelet_idx = 1, wlt%wavelet_num )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( lu_out, '( &
    & /, " wavelet_print", &
    & " nt_glb=",i8," t0_wlt=",g12.6," t1_wlt=",g12.6," dt_wlt=",g12.6, &
    & /, " wavelet_print", &
    & " nt_wlt=",i8," t0_wlt=",g12.6," t1_wlt=",g12.6," dt_wlt=",g12.6, &
    & /, " wavelet_print", &
    & " nf_wlt=",i8," f0_wlt=",g12.6," f1_wlt=",g12.6," df_wlt=",g12.6, &
    & /, " wavelet_print", &
    & " nw_wlt=",i8," w0_wlt=",g12.6," w1_wlt=",g12.6," dw_wlt=",g12.6 &
    & )') &
    wlt%nt_glb, wlt%t0_glb, wlt%t1_glb, wlt%dt_glb, &
    wlt%nt_wlt, wlt%t0_wlt, wlt%t1_wlt, wlt%dt_wlt, &
    wlt%nf_wlt, wlt%f0_wlt, wlt%f1_wlt, wlt%df_wlt, &
    wlt%nw_wlt, wlt%w0_wlt, wlt%w1_wlt, wlt%dw_wlt
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( lu_out, '( &
    & /, "   index  time         amplitude wavelet_print ")')
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( lu_out, '( 1x, i8, 1x, g12.6, 1x, g12.6, 1x, a )') &
    ( jt_wlt, wlt%rt_wlt(jt_wlt), wlt%ra_wlt(jt_wlt), trim(t_title), &
      jt_wlt = 1 , wlt%wavelet_num )
    !  jt_wlt = 1 , wlt%nt_wlt )
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( lu_out, '( &
    & /, "   index  freq(htz)    freq(rad)    amplitude wavelet_print ")')
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    write ( lu_out, '( 1x, i8, 1x, g12.6, 1x, g12.6, 1x, g12.6, 1x, a )') &
    ( jf_wlt, wlt%rf_wlt(jf_wlt), wlt%rw_wlt(jf_wlt), &
    cabs(wlt%ca_wlt(jf_wlt)), trim(f_title), &
      jf_wlt = 1 , wlt%nf_wlt )
    !
    return
    !
  end subroutine wavelet_print
  !
  subroutine wavelet_set_filter_level ( wlt )
    !
    ! set the wavelet_filter_level flags for the wavelet and the trace
    !
    type ( wavelet_struct ),   pointer :: wlt     ! wavelet structure
    !
    call string_to_upper ( wlt%wavelet_filter_level )
    !
    xxif_wavelet_filter_level_1 : &
    if ( string_upper_compare ( wlt%wavelet_filter_level ( 1:1 ), 'W' )  ) then
      !
      wlt%wavelet_filter_level = 'WAVELET'
      !
    else if ( &
    string_upper_compare ( wlt%wavelet_filter_level ( 1:1 ), 'T' )  ) then
      !
      wlt%wavelet_filter_level = 'TRACE'
      !
    else if ( &
    string_upper_compare ( wlt%wavelet_filter_level ( 1:1 ), 'N' )  ) then
      !
      wlt%wavelet_filter_level = 'NONE'
      !
    else xxif_wavelet_filter_level_1 
      !
      wlt%wavelet_filter_level = 'BOTH'
      !
    end if xxif_wavelet_filter_level_1 
    !
    xxif_wavelet_filter_level_2 : &
    if ( string_upper_compare ( wlt%wavelet_filter_level, 'WAVELET' ) &
    .or. string_upper_compare ( wlt%wavelet_filter_level, 'BOTH'    )  ) then
      !
      wlt%wavelet_filter_level_w = .true.
      !
    else xxif_wavelet_filter_level_2 
      !
      wlt%wavelet_filter_level_w = .false.
      !
    end if xxif_wavelet_filter_level_2 
    !
    xxif_wavelet_filter_level_3 : &
    if ( string_upper_compare ( wlt%wavelet_filter_level, 'TRACE' ) &
    .or. string_upper_compare ( wlt%wavelet_filter_level, 'BOTH'  )  ) then
      !
      wlt%wavelet_filter_level_t = .true.
      !
    else xxif_wavelet_filter_level_3 
      !
      wlt%wavelet_filter_level_t = .false.
      !
    end if xxif_wavelet_filter_level_3 
    !
    wlt%apply_filter = wlt%wavelet_filter_level_w 
    !
    !print'( &
    !& /, " wavelet_set_filter_level apply_filter          =",l2, &
    !& /, " wavelet_set_filter_level wavelet_filter_level_w=",l2, &
    !& /, " wavelet_set_filter_level wavelet_filter_level_t=",l2, &
    !& /, " wavelet_set_filter_level wavelet_filter_level  =",a &
    !& )', &
    !wlt%apply_filter, &
    !wlt%wavelet_filter_level_w, &
    !wlt%wavelet_filter_level_t, &
    !trim ( wlt%wavelet_filter_level)
    !
    return
    !
  end subroutine wavelet_set_filter_level
  !
  subroutine wavelet_trace ( &
                             path_out, &
                             n0_out, nh_out, nt_out, t0_out, dt_out, &
                             hd_out, tr_out, &
                             i_err &
                           )
    !
    character(len=*),         intent(in   ) :: path_out
    integer,                  intent(in   ) :: n0_out
    integer,                  intent(in   ) :: nh_out
    integer,                  intent(in   ) :: nt_out
    real,                     intent(in   ) :: t0_out
    real,                     intent(in   ) :: dt_out
    double precision,         intent(in   ) :: hd_out(:,:)
    real,                     intent(in   ) :: tr_out(:,:)
    integer,                  intent(  out) :: i_err
    !
    type ( trcio_struct ),          pointer :: trcio_obj  ! trcio output obj
    integer                                 :: j0_out
    !
    i_err = 0
    !
    !print'(" wavelet_trace path_out=",a)', trim(path_out)
    !print'(" wavelet_trace n0_out=",i8)', n0_out
    !print'(" wavelet_trace nh_out=",i8)', nh_out
    !print'(" wavelet_trace nt_out=",i8)', nt_out
    !print'(" wavelet_trace t0_out=",g12.6)', t0_out
    !print'(" wavelet_trace dt_out=",g12.6)', dt_out
    !
    ! nullify the output trace trcio structures
    !
    nullify ( trcio_obj )
    !
    ! Open the trcio disk file 
    !
    ! delete thies file if it already exists
    !
    i_err = cio_remove ( path_out )
    !
    if ( i_err .lt. 0 ) go to 998
    !
    trcio_obj => trcio_open ( &
                              filename = path_out, &
                              io_mode  = 'w', &
                              scratch  = .false., &
                              nwih     = nh_out, &
                              ndpt     = nt_out, &
                              strt_val = t0_out, &
                              srate    = dt_out &
                            )
    !
    if ( .not. associated ( trcio_obj ) ) i_err = -1
    !
    if ( i_err .lt. 0 ) go to 997
    !
    do_j0_out : do j0_out = 1 , n0_out
      !
      i_err = trcio_write_trace ( &
                                  file = trcio_obj, &
                                  hd   = hd_out(:,j0_out), &
                                  tr   = tr_out(:,j0_out), &
                                  tnum = j0_out &
                              )
      !
      if ( i_err .lt. 0 ) go to 996
      !
    end do do_j0_out 
    !
    ! close the output file
    !
    i_err = trcio_close ( file=trcio_obj, remove=.false. )
    !
    if ( i_err .ne. 0 ) go to 995
    !
    return
    !
995 continue
    !
    print'( &
    & /, "error in wavelet_trace ", &
    & /," during trcio_close " &
    & )'
    !
    go to 999
    !
996 continue
    !
    print'( &
    & /, "error in wavelet_trace ", &
    & /," during trcio_write_trace " &
    & )'
    !
    go to 999
    !
997 continue
    !
    print'( &
    & /, "error in wavelet_trace ", &
    & /," during trcio_open " &
    & )'
    !
    go to 999
    !
998 continue
    !
    print'( &
    & /, "error in wavelet_trace ", &
    & /," during cio_remove " &
    & )'
    !
    go to 999
    !
999 continue
    !
    print'( &
    & /, "error in wavelet_trace " &
    & )'
    !
    i_err = -1
    !
    return
    !
  end subroutine wavelet_trace 
  !
end module wavelet_module
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
