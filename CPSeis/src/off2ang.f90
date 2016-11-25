!<CPS_v1 type="PROCESS"/>
!!------------------------------ off2ang.f90 ---------------------------------!!
!!------------------------------ off2ang.f90 ---------------------------------!!
!!------------------------------ off2ang.f90 ---------------------------------!!
!
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
! Name       : off2ang
! Category   : Transforms
! Written    : 2001-08-22   by: Douglas Hanson
! Revised    : 2007-11-08   by: Douglas Hanson  Change prints.
! Maturity   : beta
! Purpose    : Transform from offset depth to offset angle or reverse.
! Portability: No known limitations.
! Parallel   : No
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
!  Transform a Common Image Point Gather (CIG) from offset to angle 
!  or from angle to offset in the depth wavenumber - offset wavenumber domain
!
!  Equation 20 page 7.
!  tan ( incidence angle ) = - offset wavenumber / vertical wavenumber
!
!  Amplitude-preserved wave-equation migration.
!  Paul Sava abd Biondo Biondi
!  Stanford Exploration Project Report SEP-108, April 2, 2001 pages 1-27
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!                        INPUT PARAMETERS
!
! Name  Default  Valid    Description
! ----  -------  -----    -----------
!
!-----------------------------------------------------------------------
! This process is re-enterable.
!-----------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!  Data must be input to off2ang in gathers.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                          Action taken
! ----      -----------                          ------------
! NUMTR     max number of traces input/output    Set to OFF_num
! GATHERED  whether traces are gathered          Set to .true.
! NWIH      number of words in trace header      used but not changed
! NDPT      number of sample values in trace     used and changed
! TSTRT     Trace starting time                  used and changed
! DP        Trace time smaple rate               used and changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! HDR     Description                Action taken
! ----    -----------                ------------
!  6      offset or angle value      
!                                    For an offset to angle transform
!                                    off2ang will output ANG_TOT traces for 
!                                    each input gather.
!                                    The offset value will be set to 
!                                    ( I_ANG - 1 ) * ANG_INC + ANG_INIT
!                                    where I_ANG is the trace index 
!                                    within the gather.
!
!                                    For an angle to offset transform
!                                    off2ang will output OFF_TOT traces for 
!                                    each input gather.
!                                    The offset value will be set to 
!                                    ( I_OFF - 1 ) * OFF_INC + OFF_INIT
!                                    where I_OFF is the trace index 
!                                    within the gather.
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author         Description
!     ----        ------         -----------
!  8  2007-11-08 Douglas Hanson  Change prints.
!  7  2006-10-16 D. Glover       Added NULLIFY statements for Intel compiler.
!  6  2006-01-10 B. Menger       Removed Unused Variables.
!  5  2005-08-18 Douglas Hanson  Fix negative offsets in fft.
!  4  2005-05-24 Douglas Hanson  Add sym_off
!  3  2001-10-26 Karen Goodger   Rename labels that begin with if to get around
!                                intel compiler bug.
!  2  2001-09-04 Douglas Hanson  Fix OPT_DIR error.
!  1  2001-08-22 Douglas Hanson  Original version.
!-----------------------------------------------------------------------
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
! Upon input ,NTR must have one of these values:
!    NTR >= 1              means to process the input traces.
!    NTR == no_more_traces means there are no more imput traces.
!
! Upon output ,NTR will have one of these values:
!    NTR >= 1              if this process is outputting traces.
!    NTR == no_more_traces if there are no more traces to output.
!    NTR == fatal_error    if this process has a fatal error.
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
!                           PROGRAMMING moreS
!
!
!-------------------------------------------------------------------------------
!</programming_doc>
!-------------------------------------------------------------------------------
!<--  XML code for the GUI goes in this section. />
!<gui_def>
!<NS off2ang/NC=80>
!  
!     Offset to angle or angle to offset transforms.
!  
! DEP_SCALE=`FFFFFFF   OPT_DIR~~=`CCCCCCCCCCCCCC SYM_OFF=`CC
! OFF_FFT~~=`IIIIIII   DEP_FFT~~=`IIIIIII
! OFF_TOT~~=`IIIIIII   ANG_TOT~~=`IIIIIII 
! OFF_INIT =`FFFFFFF   ANG_INIT =`FFFFFFF 
! OFF_LAST =`FFFFFFF   ANG_LAST =`FFFFFFF 
! OFF_INC~~=`FFFFFFF   ANG_INC~~=`FFFFFFF 
!
!<PARMS off2ang [screen1]>
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="OPT_DIR">
!<Tip> Type of transform. </Tip>
! Default = OFFSET_TO_ANGLE
! Allowed = OFFSET_TO_ANGLE - offset to angle  transform.
! Allowed = ANGLE_TO_OFFSET - angle  to offset transform.
!</Help>
!
!<Help KEYWORD="SYM_OFF">
!<Tip> Use symetric offset during the fft. </Tip>
! Default = YES
! Allowed = YES
! Allowed = NO
!</Help>
!
!<Help KEYWORD="DEP_SCALE">
!<Tip> Scale factor for intput vertical units. </Tip>
! Default = 1000.
! Allowed = real>0.
! The input time orign and increment are assumed to depth divided by DEP_SCALE.
!</Help>
!
!<Help KEYWORD="DEP_FFT">
!<Tip> Number of depth fft trace bins. </Tip>
! Default = 1
! Allowed = int>0
! The number of depth fft trace bins.
!</Help>
!
!<Help KEYWORD="OFF_FFT">
!<Tip> Number of offset fft trace bins. </Tip>
! Default = 1
! Allowed = int>0
! The number of offset fft trace bins.
!</Help>
!
!<Help KEYWORD="OFF_TOT">
!<Tip> Number of offset trace bins. </Tip>
! Default = 1
! Allowed = int>0
! The number of offset trace bins.
!</Help>
!
!<Help KEYWORD="OFF_INIT">
!<Tip> First offset image location to migrate data to.  </Tip>
! Default = 0
! Allowed = real
!</Help>
!
!<Help KEYWORD="OFF_LAST">
!<Tip> Last offset trace bin location. </Tip>
! Default = 100
! Allowed = real
!</Help>
!
!<Help KEYWORD="OFF_INC">
!<Tip> FFT trace bin spacing. </Tip>
! Default = 1
! Allowed = real
!</Help>
!
!<Help KEYWORD="ANG_TOT">
!<Tip> Number of angle trace bins. </Tip>
! Default = 1
! Allowed = int>0
! The number of angle trace bins.
!</Help>
!
!<Help KEYWORD="ANG_INIT">
!<Tip> First angle bin midpoint value in degrees. </Tip>
! Default = 0
! Allowed = real
!</Help>
!
!<Help KEYWORD="ANG_LAST">
!<Tip> Last angle bin midpoint in degrees. </Tip>
! Default = 80.
! Allowed = 0.<=real<=90.
!</Help>
!
!<Help KEYWORD="ANG_INC">
!<Tip> Angle bin spacing in degrees. </Tip>
! Default = 1
! Allowed = real>=0.
!</Help>
!
!</HelpSection>
!
!-------------------------------------------------------------------------------

!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


module off2ang_module
  !
  use pc_module
  use pattern_module
  use named_constants_module
  use getsys_module
  use string_module
  use fft_module
  !use interpolate_module
  use matfun_module
  use memfun_module
  use migfun_module
  use headsave_module
  use timeglob_module
  use pcpsx_module
  !
  implicit  none
  !
  private
  !
  public :: off2ang_create               ! create a off2ang object
  public :: off2ang_delete               ! delete a off2ang object
  public :: off2ang_initialize           ! initialize parameters
  public :: off2ang_update               ! update parameters
  public :: off2ang_input_trap           ! input trap
  public :: off2ang                      ! process data
  public :: off2ang_wrapup               ! wrap up after processing
  public :: off2ang_input                ! input data
  public :: off2ang_transform            ! take fft transform
  public :: off2ang_amp_phase            ! interpolate complex using amp, phase
  !
  ! functions
  !
  ! rcs identifier string
  character(len=100),public,save :: off2ang_ident = &
  '$Id: off2ang.f90,v 1.8 2007/11/09 15:09:07 Hanson beta sps $'
  !
  type,  public :: off2ang_struct
    !
    private
    !
    type ( grid_struct )      :: grid_obj  ! transformation grid
    logical                   :: wrapped_up       ! wrapup flag.
    !
    !  input parameters
    !
    character(len=15)         :: opt_dir    ! type of transform
    !
    logical                   :: sym_off    ! symetric offsets
    !
    real                      :: dep_scale
    integer                   :: dep_fft
    !
    integer                   :: off_fft
    integer                   :: off_tot
    real                      :: off_init
    real                      :: off_last
    real                      :: off_inc
    !
    integer                   :: ang_tot
    real                      :: ang_init
    real                      :: ang_last
    real                      :: ang_inc
    !
    ! saved parameters
    !
    integer                   :: ipn           ! process number
    !
    integer                   :: nh_inp        ! number of header words
    !
    integer                   :: nt_inp        ! input time grid
    real                      :: t0_inp
    real                      :: t1_inp
    real                      :: dt_inp
    !
    integer                   :: j0_gat         ! number of gathers 
    integer                   :: n0_gat         ! number of valid gathers
    !
    integer                   :: i_trin        ! number of input traces
    integer                   :: n_trin        ! number of valid input traces
    !
    integer                   :: i_trot        ! number of output traces
    integer                   :: n_trot        ! number of valid output traces
    !
    integer                   :: gz_fft
    real                      :: sz_fft
    integer                   :: nz_nyq
    integer                   :: nz_fft
    integer                   :: nz_dat
    real                      :: z0_dat
    real                      :: z1_dat
    real                      :: dz_dat
    !
    integer                   :: go_fft
    real                      :: so_fft
    integer                   :: no_nyq
    integer                   :: no_fft
    integer                   :: no_dat
    real                      :: o0_dat
    real                      :: o1_dat
    real                      :: do_dat
    !
    integer                   :: na_dat
    real                      :: a0_dat
    real                      :: a1_dat
    real                      :: da_dat
    !
    integer                   :: nx_fft
    real                      :: x0_fft
    real                      :: x1_fft
    real                      :: dx_fft
    !
    integer                   :: nx_inp
    real                      :: x0_inp
    real                      :: x1_inp
    real                      :: dx_inp
    !
    integer                   :: nx_out
    real                      :: x0_out
    real                      :: x1_out
    real                      :: dx_out
    !
    integer                   :: n0_sto                  ! storage usage
    integer                   :: n0_scr                  ! scratch usage
    !
    type(fft_struct), pointer :: ok_fft                 ! o-k fft object
    type(fft_struct), pointer :: ko_fft                 ! k-o fft object
    type(fft_struct), pointer :: zk_fft                 ! z-k fft object
    type(fft_struct), pointer :: kz_fft                 ! k-z fft object
    double precision, pointer :: hd_sav(:, :)           ! inp, out head info
    !
    type ( headsave_struct ), pointer :: h  ! headsave structure
    !
    real                             :: o0_fft     ! o fft node min
    real                             :: do_fft     ! o fft node increment
    real                             :: z0_fft     ! z fft node min
    real                             :: dz_fft     ! z fft node increment
    !
  end type off2ang_struct
  !
  type ( off2ang_struct ), pointer, save :: object      ! needed for traps.
  !
  integer, parameter :: opt_dir_n = 2
  character(len=15),save :: opt_dir_c(opt_dir_n) &
  = (/ 'OFFSET_TO_ANGLE', 'ANGLE_TO_OFFSET' /)
  !
  integer, parameter :: yes_no_n = 2
  character(len=3),save :: yes_no_c(yes_no_n) &
  = (/ 'YES', 'NO ' /)
  !
  contains
  !
  subroutine off2ang_create ( o )
    !
    type ( off2ang_struct ), pointer :: o             ! off2ang structure
    !
    allocate ( o )
    nullify (o%ok_fft) ! jpa
    nullify (o%ko_fft) ! jpa
    nullify (o%zk_fft) ! jpa
    nullify (o%kz_fft) ! jpa
    nullify (o%hd_sav) ! jpa
    nullify (o%h) ! jpa
    !
    !call memfun_nul ( o%hd_sav      ) ! saved headers (h, 12)
    !
    call off2ang_initialize ( o )
    !
    return
    !
  end subroutine off2ang_create
  !
  subroutine off2ang_delete ( o )
    !
    type ( off2ang_struct ), pointer :: o             ! off2ang structure
    !
    !call memfun_del ( o%hd_sav      ) ! saved headers (h, 12)
    !
    call headsave_delete ( o%h )
    !
    ! delete the fft objects
    !
    call fft_delete ( o%ok_fft )
    call fft_delete ( o%ko_fft )
    call fft_delete ( o%zk_fft )
    call fft_delete ( o%kz_fft )
    !
    deallocate ( o )
    !
    return
    !
  end subroutine off2ang_delete
  !
  subroutine off2ang_initialize ( o )
    !
    ! iniitlze input parameters
    !
    type ( off2ang_struct ), pointer :: o             ! off2ang structure
    !
    !  set the output migration time values
    !
    call timeglob_get ( o%nt_inp, o%t0_inp, o%dt_inp)
    o%t1_inp = ( o%nt_inp - 1) * o%dt_inp + o%t0_inp
    !
    o%opt_dir   = 'OFFSET_TO_ANGLE' ! transform type
    !
    o%sym_off   = .true.            ! symetric offsets
    !
    o%dep_fft   = -1                ! number of depth fft bins
    o%off_fft   = -1                ! number of offset fft bins
    !
    o%off_tot   = 1                 ! number of offset  bins
    o%off_inc   = 1                 ! offset bin spacing
    o%off_init  = 0                 ! first offset bin center
    o%off_last  = 0                 ! last offset bin center
    !
    o%ang_tot   = 1                 ! number of angle bins
    o%ang_inc   = 1                 ! angle bin spacing
    o%ang_init  = 0                 ! first angle bin center
    o%ang_last  = 0                 ! last angle bin center
    !
    call off2ang_update ( o )
    !
    return
    !
  end subroutine off2ang_initialize
  !
  subroutine off2ang_update ( o )
    !
    type ( off2ang_struct ), pointer :: o             ! off2ang structure
    !
    integer                          :: i_err         ! error flag
    !
    object => o               ! needed for traps.
    !
    o%wrapped_up = .false.    ! needed for the wrapup routine.
    !
    ! get the off2ang parameters
    !
    call off2ang_get ( o )
    !
    ! verify the off2ang parameters
    !
    call off2ang_verify ( o )
    !
    ! put the off2ang parameters
    !
    call off2ang_put ( o )
    !  
    ! prepare for execution 
    !  
    call off2ang_prep ( o, i_err )
    !  
    if ( i_err .ne. 0 ) go to 998
    !  
    return
    !  
998 continue
    !
    print'( &
    & /,  "error in off2ang_update pe=" ,i8, &
    & /, " during off2ang_prep " &
    & )', &
    pcpsx_i_pel()
    !
    call pc_error ( 'off2ang_update 998 error ' )
    !
    goto 999
    !
999 continue
    !
    print'( &
    & /,  "error in off2ang_update" &
    & )'
    !
    call pc_error ( 'off2ang_update error ' )
    !
  end subroutine off2ang_update
  !
  subroutine off2ang_get ( o )
    !
    ! get parameters
    !
    type ( off2ang_struct ), pointer :: o             ! off2ang structure
    !
    ! get the time globals
    !
    call timeglob_get ( o%nt_inp, o%t0_inp, o%dt_inp)
    o%t1_inp = ( o%nt_inp - 1) * o%dt_inp + o%t0_inp
    !
    ! get the rotation grid object
    !
    call pc_get_global ( 'GRID' , o%grid_obj )
    !
    ! get the number of words in the header
    !
    call pc_get_global ( 'NWIH' , o%nh_inp)
    !
    call pc_get ( 'opt_dir',   o%opt_dir   )
    call pc_get ( 'sym_off',   o%sym_off   )
    call pc_get ( 'dep_scale', o%dep_scale )
    call pc_get ( 'dep_fft',   o%dep_fft   )
    call pc_get ( 'off_fft',   o%off_fft   )
    call pc_get ( 'off_tot',   o%off_tot   )
    call pc_get ( 'off_init',  o%off_init  )
    call pc_get ( 'off_last',  o%off_last  )
    call pc_get ( 'off_inc',   o%off_inc   )
    call pc_get ( 'ang_tot',   o%ang_tot   )
    call pc_get ( 'ang_init',  o%ang_init  )
    call pc_get ( 'ang_last',  o%ang_last  )
    call pc_get ( 'ang_inc',   o%ang_inc   )
    !
    return
    !
  end subroutine off2ang_get 
  !
  subroutine off2ang_verify ( o )
    !
    ! verify parameters
    !
    type ( off2ang_struct ), pointer :: o             ! off2ang structure
    !
    integer    :: i_stat                      ! status flag
    !
    if ( o%off_tot <= 0) &
    call pc_error ('off2ang : off_tot must be > 0' )
    !
    if ( o%off_inc == 0.0) &   
    call pc_error ('off2ang : off_inc must not be 0' )
    !
    if ( o%dep_fft .lt. 0 ) o%dep_fft = - matfun_pown ( o%nt_inp )
    if ( o%off_fft .lt. 0 ) o%off_fft = - matfun_pown ( o%off_tot )
    !
    ! make sure grids are well behaved
    !
    i_stat = pattern_stop2('off2ang:', .true., &
    o%off_init, o%off_inc, o%off_last, o%off_tot, &
     'off_init',   'off_inc',   'off_last',   'off_tot', &
    pc_verify_scalar('off_init' ), pc_verify_scalar('off_inc' ), &
    pc_verify_scalar('off_last' ), pc_verify_scalar('off_tot' )) 
    !
    ! set opt_dir to allowed values
    !
    call off2ang_set_opt_dir ( o%opt_dir )
    !
    return
    !
  end subroutine off2ang_verify 
  !
  subroutine off2ang_put ( o )
    !
    ! put parameters
    !
    type ( off2ang_struct ), pointer :: o             ! off2ang structure
    !
    integer    :: update_state                ! update state
    !
    call pc_put_options_field ( 'opt_dir', opt_dir_c, opt_dir_n )
    call pc_put_options_field ( 'sym_off', yes_no_c,  yes_no_n  )
    !
    call pc_put ( 'opt_dir',   o%opt_dir   )
    call pc_put ( 'sym_off',   o%sym_off   )
    call pc_put ( 'dep_scale', o%dep_scale )
    call pc_put ( 'dep_fft',   o%dep_fft   )
    call pc_put ( 'off_fft',   o%off_fft   )
    call pc_put ( 'off_tot',   o%off_tot   )
    call pc_put ( 'off_init',  o%off_init  )
    call pc_put ( 'off_last',  o%off_last  )
    call pc_put ( 'off_inc',   o%off_inc   )
    call pc_put ( 'ang_tot',   o%ang_tot   )
    call pc_put ( 'ang_init',  o%ang_init  )
    call pc_put ( 'ang_last',  o%ang_last  )
    call pc_put ( 'ang_inc',   o%ang_inc   )
    !  
    ! set the number of output traces
    !  
    xxif_opt_dir_1 : &
    if ( string_upper_compare ( o%opt_dir, 'OFFSET_TO_ANGLE' ) ) then
      !
      ! for offset to angle transforms the output is in angle
      !
      o%nx_out = o%ang_tot
      !
    else xxif_opt_dir_1
      !
      ! for angle to offset transforms the output is in offset
      !
      o%nx_out = o%off_tot
      !
    end if xxif_opt_dir_1
    !
    update_state = pc_get_update_state()
    !
    xxif_update : if ( update_state .eq. PC_FRONTEND &
                  .or. update_state .eq. PC_BACKEND ) then
      !
      o%n0_scr = 1000000 ! scratch usage
      !
      o%n0_sto = 1000000 ! storage usage
      !
      !  put the current globals
      !
      call timeglob_put ( o%nt_inp, o%t0_inp, o%dt_inp )
      !
      call pc_put_global  ( 'gathered'    , .true.     )
      call pc_put_global  ( 'numtr'       , o%nx_out   )
      call pc_put_control ( 'need_request', .false.    )
      call pc_put_control ( 'need_label'  , .false.    )
      call pc_put_control ( 'nscratch'    , o%n0_scr   )
      call pc_put_control ( 'nstore'      , o%n0_sto   )
      !
    end if xxif_update 
    !
    return
    !
  end subroutine off2ang_put 
  !
  subroutine off2ang_prep ( o, i_err )
    !
    ! prepare for executione
    !
    type ( off2ang_struct ), pointer :: o             ! off2ang structure
    integer,          intent (inout) :: i_err          ! err flag 0=o.k. -1=err
    !
    i_err = 0
    !
    if ( pc_do_not_process_traces() ) return
    !
    print'( &
    & /, " off2ang ", /, " REVISION: ", &
    & " 8  2007-11-08 Douglas Hanson  Change prints. " &
    & )'
    !
    ! if the input fft length is < 0 set the fft length to the next power of 2
    !
    if ( o%dep_fft .lt. 0 ) o%dep_fft = matfun_pown ( o%nt_inp )
    if ( o%off_fft .lt. 0 ) o%off_fft = matfun_pown ( o%off_tot )
    !
    ! copy the input parameters to the computational parameters
    !
    o%nz_fft = o%dep_fft
    o%nz_nyq = o%nz_fft / 2 + 1
    o%nz_dat = o%nt_inp
    o%z0_dat = o%t0_inp * o%dep_scale
    o%z1_dat = o%t1_inp * o%dep_scale
    o%dz_dat = o%dt_inp * o%dep_scale
    o%gz_fft  = +1                               ! z - k fft sign
    o%sz_fft = 1. / sqrt ( float ( o%nz_fft ) )  ! z - k fft scale
    !
    o%no_fft = o%off_fft * 2   ! scale by 2 for negative offsets
    o%no_nyq = o%no_fft / 2 + 1
    o%no_dat = o%off_tot
    o%o0_dat = min ( o%off_init, o%off_last )
    o%o1_dat = max ( o%off_init, o%off_last )
    o%do_dat = abs ( o%off_inc )
    o%go_fft  = +1                               ! o - k fft sign
    o%so_fft = 1. / sqrt ( float ( o%no_fft ) )  ! o - k fft scale
    !
    print'(" nz_fft=",i8," sz_fft=",g12.6)', o%nz_fft, o%sz_fft 
    print'(" no_fft=",i8," so_fft=",g12.6)', o%no_fft, o%so_fft 
    !
    o%nx_fft = o%no_fft
    o%x0_fft = 0.
    o%dx_fft = abs ( o%do_dat ) 
    o%x1_fft = int ( o%nx_fft / 2 ) * o%do_fft ! nyquist offset
    !
    o%na_dat = o%ang_tot
    o%a0_dat = min ( o%ang_init, o%ang_last )
    o%a1_dat = max ( o%ang_init, o%ang_last )
    o%da_dat = o%ang_inc
    !  
    ! set the number of output traces
    !  
    xxif_opt_dir_2 : &
    if ( string_upper_compare ( o%opt_dir, 'OFFSET_TO_ANGLE' ) ) then
      !
      ! for offset to angle transforms the output is in angle
      !
      o%nx_inp = o%no_dat
      o%x0_inp = o%o0_dat
      o%x1_inp = o%o1_dat
      o%dx_inp = o%do_dat
      !
      o%nx_out = o%na_dat
      o%x0_out = o%a0_dat
      o%x1_out = o%a1_dat
      o%dx_out = o%da_dat
      !
    else xxif_opt_dir_2
      !
      ! for angle to offset transforms the output is in offset
      !
      o%nx_inp = o%na_dat
      o%x0_inp = o%a0_dat
      o%x1_inp = o%a1_dat
      o%dx_inp = o%da_dat
      !
      o%nx_out = o%no_dat
      o%x0_out = o%o0_dat
      o%x1_out = o%o1_dat
      o%dx_out = o%do_dat
      !
    end if xxif_opt_dir_2
    !
    o%ipn = pc_get_ipn()
    !
    ! put the memory sum counter
    !
    o%n0_sto = 0
    !
    call memfun_prn_on ( )
    !
    call memfun_sum_put ( o%n0_sto )
    !
    ! saved headers, first, last, min, max, input, saved, output
    !
    call headsave_create ( o%h, 'off2ang', o%nh_inp, i_err )
    !
    !call memfun_all ( o%i1_ins, 2000,     "i1_ins", i_err )
    !call memfun_all ( o%i2_ins, 2000,     "i2_ins", i_err )
    !call memfun_all ( o%i3_ins, 2000,     "i3_ins", i_err )
    !call memfun_all ( o%jo_neg, o%no_fft, "jo_neg", i_err )
    !call memfun_all ( o%zr_fft, o%nz_fft, "zr_fft", i_err )
    !call memfun_all ( o%co_fft, o%no_fft, "co_fft", i_err )
    !call memfun_all ( o%ca_dat, o%na_dat, "ca_dat", i_err )
    !call memfun_all ( o%zo_fft, o%nz_nyq, max(o%no_fft,o%na_dat), &
    !"zo_fft", i_err )
    !
    if ( i_err .ne. 0 ) goto 998
    !
    o%o0_fft = 0.                                ! o fft wavenumber min
    !
    o%do_fft = off2ang_dx_fft ( o%no_fft, o%do_dat ) ! o fft wavenumber inc
    !
    o%z0_fft = 0.                                    ! z fft wavenumber min
    !
    o%dz_fft = off2ang_dx_fft ( o%nz_fft, o%dz_dat ) ! z fft wavenumber inc
    !
    !o%hd_sav = 0.  ! initalize the saved header values
    !
    ! get the memory sum counter
    !
    call memfun_sum_get ( o%n0_sto )
    !
    xxif_pcpsx_i_pel_1 : if ( pcpsx_i_pel() .eq. 0 ) then
      !
      print'( &
      & /, " off2ang initialization results ipn=", i8, &
      & /, " off2ang memory utilization n0_sto =", i10 &
      & )', &
      o%ipn, o%n0_sto
      !
    end if xxif_pcpsx_i_pel_1 ! if ( pcpsx_i_pel() .eq. 0 ) then
    !
    ! create ok_fft, the offset to wavenumber fft object
    !
    i_err = fft_create ( o%ok_fft, +o%go_fft, o%no_fft, 'CTOC', o%so_fft )
    !
    xxif_ok_err : if ( i_err .ne. 0 ) then
      !
      call pc_error ( ' error in off2ang during ok fft create ' )
      !
      go to 997
      !
    end if xxif_ok_err
    !
    ! create ko_fft, the wavenumber to offset fft object
    !
    i_err = fft_create ( o%ko_fft, -o%go_fft, o%no_fft, 'CTOC', o%so_fft )
    !
    xxif_ko_err : if ( i_err .ne. 0 ) then
      !
      call pc_error ( ' error in off2ang during ko fft create ' )
      !
      go to 997
      !
    end if xxif_ko_err
    !
    ! create zk_fft, the offset to wavenumber fft object
    !
    i_err = fft_create ( o%zk_fft, +o%gz_fft, o%nz_fft, 'CTOC', o%sz_fft )
    !
    xxif_zk_err : if ( i_err .ne. 0 ) then
      !
      call pc_error ( ' error in off2ang during zk fft create ' )
      !
      go to 997
      !
    end if xxif_zk_err
    !
    ! create kz_fft, the wavenumber to depth fft object
    !
    i_err = fft_create ( o%kz_fft, -o%gz_fft, o%nz_fft, 'CTOC', o%sz_fft )
    !
    xxif_kz_err : if ( i_err .ne. 0 ) then
      !
      call pc_error ( ' error in off2ang during kz fft create ' )
      !
      go to 997
      !
    end if xxif_kz_err
    !
    ! initialize some counters
    !
    o%j0_gat    = 0    ! number of gathers
    o%n0_gat    = 0    ! number of valid gathers
    !
    o%i_trin   = 0    ! number of input traces 
    o%n_trin   = 0    ! number of valid input traces
    !
    o%i_trot   = 0    ! number of output traces
    o%n_trot   = 0    ! number of valid output traces
    !
    !  set the output time globals
    !
    call timeglob_put ( o%nt_inp, o%t0_inp, o%dt_inp )
    !
    print'( &
    & /, " normal return off2ang pe=", i8 &
    & /, " off2ang input information ", &
    & /, " ipn=", i8, &
    & /, " opt_dir =", a, &
    & /, " sym_off =", l2, &
& /, " nx_inp=", i8, " x0_inp=", g12.6, " x1_inp=", g12.6, " dx_inp=", g12.6, &
& /, " nx_out=", i8, " x0_out=", g12.6, " x1_out=", g12.6, " dx_out=", g12.6, &
& /, " nt_inp=", i8, " t0_inp=", g12.6, " t1_inp=", g12.6, " dt_inp=", g12.6, &
& /, " nz_dat=", i8, " z0_dat=", g12.6, " z1_dat=", g12.6, " dz_dat=", g12.6, &
& /, " no_dat=", i8, " o0_dat=", g12.6, " o1_dat=", g12.6, " do_dat=", g12.6, &
& /, " na_dat=", i8, " a0_dat=", g12.6, " a1_dat=", g12.6, " da_dat=", g12.6, &
& /, " nz_fft =", i12, " nz_nyq =", i12, " gz_fft =", i12, " sz_fft=", g12.6, &
& /, " no_fft =", i12, " no_nyq =", i12, " go_fft =", i12, " so_fft=", g12.6, &
    & / )', &
    pcpsx_i_pel(), &
    o%ipn, &
    trim( o%opt_dir), &
    o%sym_off, &
    o%nx_inp, o%x0_inp, o%x1_inp, o%dx_inp, &
    o%nx_out, o%x0_out, o%x1_out, o%dx_out, &
    o%nt_inp, o%t0_inp, o%t1_inp, o%dt_inp, &
    o%nz_dat, o%z0_dat, o%z1_dat, o%dz_dat, &
    o%no_dat, o%o0_dat, o%o1_dat, o%do_dat, &
    o%na_dat, o%a0_dat, o%a1_dat, o%da_dat, &
    o%nz_fft, o%nz_nyq, o%gz_fft, o%sz_fft, &
    o%no_fft, o%no_nyq, o%go_fft, o%so_fft
    !
    return
    !
997 continue
    !
    print'( &
    & /,  "error in off2ang_prep pe= " ,i8, &
    & /, " createing the fft object " &
    & )', &
    pcpsx_i_pel()
    !
    call pc_error ( 'off2ang_prep 997 error ' )
    !
    goto 999
    !
998 continue
    !
    print'( &
    & /,  "error in off2ang_prep pe=" ,i8, &
    & /, " during memory allocation " &
    & )', &
    pcpsx_i_pel()
    !
    call pc_error ( 'off2ang_prep 998 error ' )
    !
    goto 999
    !
999 continue
    !
    print'( &
    & /,  "error in off2ang_prep" &
    & )'
    !
    call pc_error ( 'off2ang_prep error ' )
    !
    return
    !
  end subroutine off2ang_prep 
  !
  subroutine off2ang_input_trap ( )
    !
    return
    !
  end subroutine off2ang_input_trap
  !
  subroutine off2ang ( o, n0_inp, hd_inp, tr_inp )
    !
    type ( off2ang_struct ), pointer :: o             ! off2ang structure
    integer,          intent (inout) :: n0_inp        ! number of traces
    double precision, intent (inout) :: hd_inp(:, :)  ! headers
    real,             intent (inout) :: tr_inp(:, :)  ! traces
    !
    integer                          :: mh_inp        ! header dimension
    integer                          :: mt_inp        ! trace dimension
    integer                          :: n0_out        ! number of output traces
    integer                          :: i_err         ! error flag
    !
    integer, save                    :: i_call = 0    ! call index
    !
    i_call = i_call + 1    ! call index
    !
    mh_inp = size(hd_inp, 1)
    mt_inp = size(tr_inp, 1)
    !
    i_err = 0
    !
    ! input n0_inp traces
    !
    call off2ang_input ( o, n0_inp, n0_out, hd_inp, tr_inp, i_err )
    !
    if ( i_err .ne. 0) goto 998
    !
1999 continue
    !
    !print'(" off2ang c=",i8, &
    !& " ipn=",i3," ni=",i8," no=",i8," ti=",i8," to=",i8)',&
    !i_call, o%ipn, n0_inp, n0_out, o%n_trin, o%n_trot
    !
    n0_inp = n0_out
    !
    if ( n0_inp .eq. no_more_traces .or. n0_inp .eq. fatal_error ) &
    call off2ang_wrapup ( o )
    !
    return
    !
998 continue
    !
    print'( &
    & /,  " error in off2ang pe=" ,i8, &
    & /,  " during off2ang_input " &
    & )', &
    pcpsx_i_pel()
    !
    call pc_error ( ' error in off2ang ' )
    !
    goto 999
    !
999 continue
    !
    print'( &
    & /,  " error in off2ang pe=" ,i8 &
    & )', &
    pcpsx_i_pel()
    !
    call pc_error ( ' error in off2ang ' )
    !
    n0_inp = fatal_error
    !
    goto 1999
    !
  end subroutine off2ang
  !
  subroutine off2ang_wrapup ( o )
    !
    type ( off2ang_struct ), pointer :: o             ! off2ang structure
    !
    if ( o%wrapped_up) return
    o%wrapped_up = .true.
    !
    return
    !
  end subroutine off2ang_wrapup
  !
  subroutine off2ang_input ( o, n0_inp, n0_out, hd_inp, tr_inp, i_err )
    !
    ! input the next batch of n0_inp input traces
    ! fft the time samples and replace them in the traces
    !
    type ( off2ang_struct ), pointer :: o             ! off2ang structure
    !
    !
    integer,          intent (in   ) :: n0_inp        ! number of input  traces
    integer,          intent (  out) :: n0_out        ! number of output traces
    double precision, intent (inout) :: hd_inp(:, :)   ! headers
    real,             intent (inout) :: tr_inp(:, :)   ! traces
    integer,          intent (inout) :: i_err          ! err flag 0=o.k. -1=err
    !
    ! local variables
    !
    integer                          :: j0_inp        ! input trace index
    !
    integer                          :: jz_fft       ! z fft node index
    !
    integer                          :: ix_inp       ! input trace index
    integer                          :: jx_inp       ! input node index
    real                             :: rx_inp       ! input node value
    integer                          :: jx_fft       ! fft loc for inp
    integer                          :: kx_fft(o%nx_inp)! fft loc for each inp
    !
    !the input traces will be stored in the fft array in fft order
    !that is from zero to nyquist offset then
    !from -nyquist+off_inc to -off_inc
    ! array kx_fft defines the fft offset orderin co_fft
    !
    integer                          :: jx_out       ! output node index
    real                             :: rx_out       ! output node value
    !
    integer                          :: jo_ins       ! inside fft range index
    integer                          :: n0_ins       ! number inside fft range 
    integer                          :: i1_ins(n0_inp)! inp trace pointer
    integer                          :: i2_ins(n0_inp)! out trace pointer
    integer                          :: i3_ins(n0_inp)! fft trace pointer
    !
    integer                          :: top_mute     ! top mute index
    integer                          :: bot_mute     ! bot mute index
    !
    complex                    :: cz_fft(o%nz_fft) ! real z-k trace
    complex                    :: co_fft(o%no_fft) ! complex o,ko trace
    complex                    :: zo_fft(o%nz_fft,max(o%no_fft,o%na_dat))
    !
    character(len=16)                :: c_time     ! current time
    !
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! initialize the number of output traces
    !
    n0_out = 0
    !
    ! if n0_inp .eq. no_more_trace print the current group of input header words
    !
    if ( i_call .eq. 1 ) &
    print'( &
    & /, " off2ang c=", i8," n=", i8, " p=", i6, &
    & " ig=", i6, " ng=", i6, &
    & " ii=", i6, " ni=", i6, &
    & " io=", i6, " no=", i6 &
    & )', &
    i_call, n0_inp, o%ipn, o%j0_gat, o%n0_gat, &
    o%i_trin, o%n_trin, o%i_trot, o%n_trot
    !
    ! if n0_inp .eq. no_more_trace print the current group of input header words
    !
    xxif_n0_inp : if ( n0_inp .eq. no_more_traces ) then
      !
      print'( &
      & /, " off2ang end of output process number = ", i12, &
      & /, " total number of input gathers       = ", i12, &
      & /, " total number of valid gathers       = ", i12, &
      & /, " total number of input traces        = ", i12, &
      & /, " total number of valid input traces  = ", i12, &
      & /, " total number of output traces       = ", i12, &
      & /, " total number of valid output traces = ", i12, &
      & / &
      & )', &
      o%ipn, o%j0_gat, o%n0_gat, o%i_trin, o%n_trin, o%i_trot, o%n_trot
      !
      !
      call headsave_print ( o%h, 'off2ang', 1 )
      !
      call headsave_print ( o%h, 'off2ang', 9 )
      !
    else xxif_n0_inp ! if ( n0_inp .eq. no_more_traces ) then
      !
      ! initialize the inside fft range counter
      !
      n0_ins = 0 
      !
      ! initialize the top and bottom mute indices
      !
      top_mute = o%nt_inp
      !
      bot_mute = 1
      !
      ! compute the fft location
      !
      call off2ang_fft_loc ( o%nx_inp, o%x0_inp, o%dx_inp, o%nx_fft, kx_fft )
      !
      ! determine which input traces are include in the fft range
      ! 
      do_j0_inp : do j0_inp = 1 , n0_inp
        !
        !
        ! increment the input trace counter
        !
        o%i_trin = o%i_trin + 1
        !
        !  save headers for input traces (locations 1 - 4)
        !
        call headsave_store ( o%h, o%i_trin, 1, hd_inp(:, j0_inp) )
        !
        !  get the trace x location
        !
        call migfun_trace_location ( &
        hd_inp ( hdr_offset, j0_inp ), o%x0_inp, o%dx_inp, 1., jx_inp, rx_inp )
        !
        !if ( i_call .eq. 1 ) &
        !print'(" inp i=",i8," ix=",i8," rx=",g12.6, &
        !& " o=",g12.6," tr=",g12.6)', &
        !o%i_trin, jx_inp, rx_inp, &
        !hd_inp ( hdr_offset, j0_inp ), matfun_amax(o%nt_inp,tr_inp(:,j0_inp))
        !
        ! if this input trace is inside the image volume save it
        !
        xxif_jx_inp : if ( jx_inp .ge. 1 .and. jx_inp .le. o%nx_inp ) then
          !
          ! increment the inside sample counter
          !
          n0_ins = n0_ins + 1
          !
          i1_ins ( n0_ins ) = j0_inp            ! input index
          !
          i2_ins ( n0_ins ) = jx_inp            ! output index
          !
          i3_ins ( n0_ins ) = kx_fft ( jx_inp ) ! fft    index
          !
          ! save the top and bottom mute indices
          !
top_mute = min ( top_mute, nint ( hd_inp ( hdr_top_mute,    j0_inp ) ))
bot_mute = max ( bot_mute, nint ( hd_inp ( hdr_bottom_mute, j0_inp ) ))
          !
          ! increment the saved trace counter
          !
          o%n_trin = o%n_trin + 1
          !
          !  save headers for saved traces (locations 5 - 8)
          !
          call headsave_store ( o%h, o%n_trin, 5, hd_inp(:, j0_inp) )
          !
        end if xxif_jx_inp
        !
      end do do_j0_inp ! do j0_inp = 1 , n0_inp
      !
      ! if there are no traces inside this fft range set the gather to 0.
      !
      xxif_n0_ins : if ( n0_ins .eq. 0 ) then
        !
        tr_inp = 0.
        !
      else xxif_n0_ins
        !
        ! otherwise:
        ! 1. copy input time trace values to rt_fft or ct_fft,
        ! 2. fft
        ! 1. copy rt_fft or ct_fft back to output time samples in tr_inp
        !
        ! initialize the 2d fft array to zero
        !
        zo_fft = cmplx ( 0., 0. )
        !
        ! copy each input trace from tr_inp into zo_fft
        !
        do_jo_ins : do jo_ins = 1 , n0_ins
          !
          ix_inp = i1_ins ( jo_ins ) 
          !
          jx_inp = i2_ins ( jo_ins ) 
          !
          jx_fft = i3_ins ( jo_ins ) 
          !
          !print'(" i0=",i8," jx_inp=",i8," ix_inp=",i8)',jo_ins,jx_inp,ix_inp
          !
          cz_fft ( 1:o%nz_fft ) = 0.
          !
          cz_fft ( 1:o%nt_inp ) = cmplx ( tr_inp ( 1:o%nt_inp, ix_inp ), 0. )
          !
          !call fft_cc_transform ( o%zk_fft, cz_fft, zo_fft ( :, jx_inp ) )
          call fft_cc_transform ( o%zk_fft, cz_fft, zo_fft ( :, jx_fft ) )
          !
          !if ( i_call .eq. 1 ) &
          !print'(" fft jo=",i8," ix_inp=",i8," jx_inp=",i8," jx_fft=",i8)', &
          !jo_ins, ix_inp, jx_inp, jx_fft
          !
        end do do_jo_ins
        !
        ! take o to ko transform
        !
        xxif_off_to_ang : &
        if ( string_upper_compare ( o%opt_dir, 'OFFSET_TO_ANGLE' ) ) then
          !
          do_off_to_ang : do jz_fft = 1 , o%nz_fft
              !
              co_fft ( : ) = cmplx ( 0., 0. )
              !
              co_fft ( 1:o%no_nyq   ) = zo_fft ( jz_fft, 1:o%no_nyq )
              !
              if ( o%sym_off ) &
              co_fft ( o%no_fft:o%no_nyq+1 ) = zo_fft ( jz_fft, 2:o%no_nyq-1 )
              !
              !co_fft ( : ) = cmplx ( 0., 0. )
              !
              !co_fft ( 1 ) = zo_fft ( jz_fft, 1 )
              !
              zo_fft ( jz_fft, : ) = cmplx ( 0., 0. )
              !
              call fft_cc_transform ( o%ok_fft, co_fft )
              !
              zo_fft ( jz_fft, 1:o%no_fft ) = co_fft ( 1:o%no_fft )
              !
          end do do_off_to_ang
          !
        end if xxif_off_to_ang 
        !
        ! transfrom from angle to offset or from offset to angle
        !
        !if ( o%nz_dat .eq. -999 ) &
        call off2ang_transform ( &
                                 o%opt_dir, &
                                 o%nz_dat, o%z0_dat, o%dz_dat, &
                                 o%no_dat, o%o0_dat, o%do_dat, &
                                 o%na_dat, o%a0_dat, o%da_dat, &
                                 o%nz_fft, o%no_fft, &
                                 o%nz_nyq, o%no_nyq, &
                                 zo_fft, &
                                 i_err &
                               )
        !
        ! take o to ko transform
        !
        xxif_ang_to_off  : &
        if ( string_upper_compare ( o%opt_dir, 'ANGLE_TO_OFFSET' ) ) then
          !
          do_ang_to_off  : do jz_fft = 1 , o%nz_fft
              !
              co_fft = cmplx ( 0., 0. )
              !
              co_fft ( 1:o%no_fft ) = zo_fft ( jz_fft, 1:o%no_fft )
              !
              zo_fft ( jz_fft, : ) = cmplx ( 0., 0. )
              !
              call fft_cc_transform ( o%ko_fft, co_fft )
              !
              zo_fft ( jz_fft, 1:o%no_fft ) = co_fft ( 1:o%no_fft )
              !
          end do do_ang_to_off 
          !
        end if xxif_ang_to_off 
        !
        ! fft each output trace from k to z from zo_fft to tr_inp
        !
        do_jx_out : do jx_out = 1 , o%nx_out
          !
          cz_fft ( 1:o%nt_inp ) = cmplx ( 0., 0. )
          !
          call fft_cc_transform ( o%kz_fft, zo_fft ( :, jx_out ), cz_fft )
          !
          tr_inp ( 1:o%nt_inp, jx_out ) = real ( cz_fft ( 1:o%nt_inp ) )
          !
        end do do_jx_out
        !
        o%n_trot = o%n_trot + o%nx_out
        !
        o%n0_gat = o%n0_gat + 1
        !
      end if xxif_n0_ins
      !
      ! set the number of output traces
      !
      n0_out = o%nx_out
      !
      o%j0_gat = o%j0_gat + 1
      !
      call string_time ( c_time ) ! current time
      !
      print'(1x,i8,1x,i8,1x,i8,1x,g12.6,1x,g12.6,1x,a16," off2ang_inp " )', &
      i_call, o%j0_gat, n0_inp, hd_inp(7,1), hd_inp(8,1), trim(c_time)
      ! 
      ! reapply mutes
      !
      tr_inp ( 1:top_mute-1,          1:o%nx_out ) = 0.       ! top mute
      tr_inp ( bot_mute+1:o%nt_inp,   1:o%nx_out ) = 0.       ! bottom mute
      !
      ! set the output header words
      !
      do_jx_out_2 : do jx_out = 1 , o%nx_out
        !
        rx_out = ( jx_out - 1 ) * o%dx_out + o%x0_out
        !
        o%i_trot = o%i_trot + 1
        !
        hd_inp ( hdr_current_group,   jx_out ) = o%j0_gat ! group number
        hd_inp ( hdr_top_mute,        jx_out ) = top_mute ! top mute
        hd_inp ( hdr_bottom_mute,     jx_out ) = bot_mute ! bottom mute
        hd_inp ( hdr_offset,          jx_out ) = rx_out   ! x location
        hd_inp ( hdr_sequence,        jx_out ) = o%i_trot ! output num
        hd_inp ( hdr_current_channel, jx_out ) = jx_out   ! current channel
        hd_inp ( hdr_lav,             jx_out ) = &
  lav ( tr_inp ( 1:o%nt_inp,          jx_out ), o%nt_inp )
        !
        !print'(" out i=",i8," ix=",i8," rx=",g12.6," tr=",g12.6)', &
        !i_trot,jx_out,rx_out,matfun_amax(nt_inp,tr_inp(:,jx_out))
        !
        ! save the output headers
        !
        call headsave_store ( o%h, o%i_trot, 9, hd_inp ( :, jx_out ) )
        !
      end do do_jx_out_2 ! do jx_out = 1 , nx_out
      !
    end if xxif_n0_inp ! if ( n0_inp .eq. no_more_traces ) then
    !
    return
    !
  end subroutine off2ang_input
  !
  subroutine off2ang_transform ( &
                                 opt_dir, &
                                 nz_dat, z0_dat, dz_dat, &
                                 no_dat, o0_dat, do_dat, &
                                 na_dat, a0_dat, da_dat, &
                                 nz_fft, no_fft, &
                                 nz_nyq, no_nyq, &
                                 zo_fft, &
                                 i_err &
                               )
    !
    ! transform the input data from offset to angle or from angle to offset
    ! tan ( incidence angle ) = - offset wavenumber / vertical wavenumber
    !
    character(len=*), intent (in   ) :: opt_dir  ! type of transform
    !
    integer,          intent (in   ) :: nz_dat   ! z node number
    real,             intent (in   ) :: z0_dat   ! z node min
    real,             intent (in   ) :: dz_dat   ! z node increment
    !
    integer,          intent (in   ) :: no_dat   ! o node number
    real,             intent (in   ) :: o0_dat   ! o node min
    real,             intent (in   ) :: do_dat   ! o node increment
    !
    integer,          intent (in   ) :: na_dat   ! a node number
    real,             intent (in   ) :: a0_dat   ! a node min
    real,             intent (in   ) :: da_dat   ! a node increment
    !
    integer,          intent (in   ) :: nz_fft   ! z fft number
    integer,          intent (in   ) :: no_fft   ! o fft number
    integer,          intent (in   ) :: nz_nyq   ! z nyquist number
    integer,          intent (in   ) :: no_nyq   ! o nyquist number
    !
    complex,          intent (inout) :: zo_fft(:,:)! complex kz,ko gather
    !
    integer,          intent (inout) :: i_err    ! error flag 0=o.k. -1 = error
    !
    ! local variables
    !
    integer                          :: jz_fft     ! z fft node index
    real                             :: rz_fft     ! z fft node value
    real                             :: z0_fft     ! z fft node min
    real                             :: dz_fft     ! z fft node increment
    !
    integer                          :: jo_fft_1   ! o fft node index 1
    integer                          :: jo_fft_2   ! o fft node index 2
    real                             :: ro_fft_1   ! o fft node value 1
    real                             :: ro_fft_2   ! o fft node value 2
    real                             :: fo_fft_1   ! o fft node factor 1
    real                             :: fo_fft_2   ! o fft node factor 2
    integer                          :: jo_fft     ! o fft node index
    real                             :: ro_fft     ! o fft node value
    real                             :: o0_fft     ! o fft node min
    real                             :: do_fft     ! o fft node increment
    !
    integer                          :: ja_dat_1   ! a fft node index 1
    integer                          :: ja_dat_2   ! a fft node index 2
    real                             :: ra_dat_1   ! a fft node value 1
    real                             :: ra_dat_2   ! a fft node value 2
    real                             :: fa_dat_1   ! a fft node factor 1
    real                             :: fa_dat_2   ! a fft node factor 2
    integer                          :: ja_dat     ! a fft node index
    real                             :: ra_dat     ! a fft node value
    real                             :: ta_dat     ! tangent of angle
    !
    complex                          :: co_fft(no_fft)
    complex                          :: ca_dat(na_dat)
    integer                          :: jo_neg(no_fft)
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    call off2ang_neg_fft ( no_fft, jo_neg )  ! negative o wavenumber indices
    !
    o0_fft = 0.                                ! o fft wavenumber min
    do_fft = off2ang_dx_fft ( no_fft, do_dat ) ! o fft wavenumber increment
    !
    z0_fft = 0.                                ! z fft wavenumber min
    dz_fft = off2ang_dx_fft ( nz_fft, dz_dat ) ! z fft wavenumber increment
    !
    !print'( &
    !& /, " off2ang_transform pe=", i8 &
    !& /, " opt_dir =", a, &
    !& /, " nz_dat=", i8, " z0_dat=", g12.6, " dz_dat=", g12.6, &
    !& /, " no_dat=", i8, " o0_dat=", g12.6, " do_dat=", g12.6, &
    !& /, " na_dat=", i8, " a0_dat=", g12.6, " da_dat=", g12.6, &
    !& /, " nz_fft =", i12, " nz_nyq =", i12, " dz_fft=",g12.6, &
    !& /, " no_fft =", i12, " no_nyq =", i12, " do_fft=",g12.6, &
    !& / )', &
    !pcpsx_i_pel(), &
    !trim( opt_dir), &
    !nz_dat, z0_dat, dz_dat, &
    !no_dat, o0_dat, do_dat, &
    !na_dat, a0_dat, da_dat, &
    !nz_fft, nz_nyq, dz_fft, &
    !no_fft, no_nyq, do_fft
    !
    xxif_off_to_ang : &
    if ( string_upper_compare ( opt_dir, 'OFFSET_TO_ANGLE' ) ) then
      !
      ! offset to angle map
      !
      ! cycle over vertical wavenumbers
      !
      do_off_to_ang_z : do jz_fft = 1 , nz_fft
        !
        ! compute the input vertical wavenumber value, rz_fft
        !
        rz_fft = ( jz_fft - 1 ) * dz_fft + z0_fft
        !
        if ( jz_fft .gt. nz_nyq ) &
        rz_fft = - ( ( nz_fft - jz_fft + 1 ) * dz_fft + z0_fft )
        !
        ! copy this slice of offset wavenumbers from zo_fft to co_fft
        !
        co_fft ( 1:no_fft ) = zo_fft ( jz_fft, 1:no_fft )
        !
        ! zero this slice of offset wavenumbers in zo_fft
        !
        zo_fft ( jz_fft, : ) = cmplx ( 0., 0. )
        !
        ! cycle over output angles
        !
        do_off_to_ang_a : do ja_dat = 1 , na_dat
          !
          ! compute the output angle value, ra_dat
          !
          ra_dat = ( ja_dat - 1 ) * da_dat + a0_dat
          !
          ! compute the tangent of the output angle
          !
          ta_dat = tan ( ra_dat * radians_per_degree )
          !
          ! compute the input offset wavenumber value, ro_fft
          ! tan ( incidence angle ) = - offset wavenumber / vertical wavenumber
          !
          ro_fft = abs ( ta_dat * rz_fft )
          !
          ! compute the input offset wavenumber index, jo_fft
          !
          !jo_fft = nint ( ( ro_fft - o0_fft ) / do_fft ) + 1
          !
          ! if the input offset wavenumber index is within the defined range
          ! map from co_fft to zo_fft
          !
          !if ( jo_fft .ge. 1 .and. jo_fft .le. no_nyq ) &
          !zo_fft ( jz_fft, ja_dat ) = co_fft ( jo_fft )
          !
          ! compute the input offset wavenumber index, jo_fft
          !
          jo_fft_1 = int ( ( ro_fft - o0_fft ) / do_fft ) + 1
          jo_fft_2 = min ( no_nyq , jo_fft_1 + 1 )
          !
          ro_fft_1 = ( jo_fft_1 - 1 ) * do_fft + o0_fft
          ro_fft_2 = ( jo_fft_2 - 1 ) * do_fft + o0_fft
          !
          fo_fft_2 = max ( 0. , min ( 1. , ( ro_fft - ro_fft_1 ) / do_fft ) )
          fo_fft_1 = 1. - fo_fft_2
          !
          ! if the input offset wavenumber index is within the defined range
          ! map from co_fft to zo_fft
          !
          if ( jo_fft_1 .ge. 1 .and. jo_fft_1 .le. no_nyq ) &
          call off2ang_amp_phase ( &
                                   fo_fft_1, co_fft ( jo_fft_1 ), &
                                   fo_fft_2, co_fft ( jo_fft_2 ), &
                                   zo_fft ( jz_fft, ja_dat ) &
                                 )
          !
          !if ( jo_fft_1 .ge. 1 .and. jo_fft_1 .le. no_nyq ) &
          !zo_fft ( jz_fft, ja_dat ) = &
          !                            fo_fft_1 * co_fft ( jo_fft_1 ) &
          !                          + fo_fft_2 * co_fft ( jo_fft_2 )
          !
        end do do_off_to_ang_a
        !
      end do do_off_to_ang_z
      !
    else xxif_off_to_ang 
      !
      ! angle to offset map
      !
      ! cycle over vertical wavenumbers
      !
      do_ang_to_off_z : do jz_fft = 1 , nz_fft
        !
        ! compute the input vertical wavenumber value, rz_fft
        !
        rz_fft = ( jz_fft - 1 ) * dz_fft + z0_fft
        !
        if ( jz_fft .gt. nz_nyq ) &
        rz_fft = - ( ( nz_fft - jz_fft + 1 ) * dz_fft + z0_fft )
        !
        ! copy this slice of input angles from zo_fft to ca_dat
        !
        ca_dat ( 1:na_dat ) = zo_fft ( jz_fft, 1:na_dat )
        !
        ! zero this slice of input angles in zo_fft
        !
        zo_fft ( jz_fft, : ) = cmplx ( 0., 0. )
        !
        ! cycle over output offset wavenumbers
        !
        do_ang_to_off_o : do jo_fft = 1 , no_nyq
          !
          ! compute the output offset wavenumber value, ro_fft
          !
          ro_fft = ( jo_fft - 1 ) * do_fft + o0_fft
          !
          ! compute the input angle value, ra_dat
          ! tan ( incidence angle ) = - offset wavenumber / vertical wavenumber
          !
          xxif_rz_fft : if ( rz_fft .eq. 0. ) then
            !
            ra_dat = 90.
            !
          else xxif_rz_fft
            !
            ra_dat = abs ( atan ( ro_fft / rz_fft ) * degrees_per_radian )
            !
          end if xxif_rz_fft
          !
          ! compute the input angle index, ja_dat
          !
          !ja_dat = nint ( ( ra_dat - a0_dat ) / da_dat ) + 1
          !
          ! if the input angle index is within the defined range
          ! map from ca_dat to zo_fft
          !
          !if ( ja_dat .ge. 1 .and. ja_dat .le. na_dat ) &
          !zo_fft ( jz_fft, jo_fft ) = ca_dat ( ja_dat )
          !
          ja_dat_1 = int ( ( ra_dat - a0_dat ) / da_dat ) + 1
          ja_dat_2 = min ( na_dat , ja_dat_1 + 1 )
          !
          ra_dat_1 = ( ja_dat_1 - 1 ) * da_dat + a0_dat
          ra_dat_2 = ( ja_dat_2 - 1 ) * da_dat + a0_dat
          !
          fa_dat_2 = max ( 0. , min ( 1. , ( ra_dat - ra_dat_1 ) / da_dat ) )
          fa_dat_1 = 1. - fa_dat_2
          !
          ! if the input angle index is within the defined range
          ! map from ca_dat to zo_fft
          !
          if ( ja_dat_1 .ge. 1 .and. ja_dat_1 .le. na_dat ) &
          call off2ang_amp_phase ( &
                                   fa_dat_1, ca_dat ( ja_dat_1 ), &
                                   fa_dat_2, ca_dat ( ja_dat_2 ), &
                                   zo_fft ( jz_fft, jo_fft ) &
                                 )
          !
          !if ( ja_dat_1 .ge. 1 .and. ja_dat_1 .le. na_dat ) &
          !zo_fft ( jz_fft, jo_fft ) = &
          !                            fa_dat_1 * ca_dat ( ja_dat_1 ) &
          !                          + fa_dat_2 * ca_dat ( ja_dat_2 ) 
          !
          ! set the negative offset wavenumber value
          !
          zo_fft ( jz_fft, jo_neg ( jo_fft ) ) = zo_fft ( jz_fft, jo_fft )
          !
        end do do_ang_to_off_o
        !
      end do do_ang_to_off_z
      !
    end if xxif_off_to_ang 
    !
    return
    !
  end subroutine off2ang_transform
  !
  subroutine off2ang_set_opt_dir ( opt_dir )
    !
    !  set opt_dir to OFFSET_TO_ANGLE or ANGLE_TO_OFFSET
    !
    character(len=*), intent (inout) :: opt_dir  ! type of transform
    !
    call string_to_upper ( opt_dir )
    !
    xxif_opt_dir : if ( string_upper_compare ( opt_dir(1:1), 'A' ) ) then
      !
      opt_dir = 'ANGLE_TO_OFFSET'
      !
    else xxif_opt_dir 
      !
      opt_dir = 'OFFSET_TO_ANGLE'
      !
    end if xxif_opt_dir 
    !
    return
    !
  end subroutine off2ang_set_opt_dir
  !
  real function off2ang_dx_fft ( nx_fft, dx_fft )
    !
    ! compute the wavenumber increment from the spatial characteristics
    !
    integer,    intent (in   ) :: nx_fft
    real,       intent (in   ) :: dx_fft
    !
    integer                    :: nk_nyq  ! nyquist wavenumber
    real                       :: rk_nyq  ! nyquist wavenumber
    !
    xxif_nx_fft : if ( nx_fft .gt. 1 ) then
      !
      nk_nyq = nx_fft / 2 + 1                        ! num wavenumber to nyquist
      rk_nyq = .5 / dx_fft                           ! nyquist wavenumber
      !
      off2ang_dx_fft = 2. * pi * rk_nyq / ( nk_nyq - 1 )! x wavenumber inc
      !
    else xxif_nx_fft ! if ( nx_fft .gt. 1 ) then
      !
      off2ang_dx_fft = 1.                                ! x wavenumber inc
      !
    end if xxif_nx_fft ! if ( nx_fft .gt. 1 ) then
    !
    return
    !
  end function off2ang_dx_fft
  !
  subroutine off2ang_kx_fft ( nx_fft, dx_fft, kx_fft )
    !
    !  compute wavenumber spacing from the spatial characteristics
    !
    integer,    intent (in   ) :: nx_fft     ! number of wavenumbers
    real,       intent (in   ) :: dx_fft     ! x fft wavenumber increment
    real,       intent (inout) :: kx_fft (:) ! fft wavenumber value
    !
    ! Local variables
    !
    integer                    :: jx_fft     ! x fft wavenumber index
    !
    kx_fft(         1) = 0.                             ! zero    wavenumber
    kx_fft(nx_fft/2+1) = ( nx_fft / 2 ) * dx_fft        ! nyquist wavenumber
    !
    do_jx_fft : do jx_fft = 2 , nx_fft/2
      !
      kx_fft(       jx_fft  ) = ( jx_fft - 1 ) * dx_fft ! positive wavenumber
      kx_fft(nx_fft-jx_fft+2) = - kx_fft(jx_fft)        ! negative wavenumber
      !
    end do do_jx_fft ! do jx_fft = 2 , nx_fft/2
    !
    return
    !
  end subroutine off2ang_kx_fft
  !
  subroutine off2ang_neg_fft ( nx_fft, jx_neg )
    !
    ! compute negative wavenumber indices, jx_neg
    !
    integer,          intent (in   ) :: nx_fft    ! x fft length
    integer,          intent (inout) :: jx_neg(:) ! neg x fft index
    !
    ! local variables
    !
    integer    :: jx_fft          ! + x fft wavenumber index
    !
    do_jx_fft : do jx_fft = 1 , nx_fft/2+1
      !
      jx_neg ( jx_fft ) = max ( 1, min ( nx_fft, nx_fft - jx_fft + 2 ) )
      !
    end do do_jx_fft ! do jx_fft = 1 , nx_fft/2+1
    !
    return
    !
  end subroutine off2ang_neg_fft 
  !
  integer function off2ang_zero_or_nyq ( jx_fft, nx_fft)
    !
    !  set off2ang_zero_or_nyq = 1 if jx_fft = 1 or nyquist
    !  set off2ang_zero_or_nyq = 0 otherwise
    !
    integer,    intent (in   ) :: jx_fft
    integer,    intent (in   ) :: nx_fft
    !
    xxif_jx_fft : if ( jx_fft .eq. 1 .or. jx_fft .eq. nx_fft/2+1 ) then
      !
      off2ang_zero_or_nyq = 1
      !
    else xxif_jx_fft ! if ( jx_fft .eq. 1 .or. jx_fft .eq. nx_fft/2+1 ) then
      !
      off2ang_zero_or_nyq = 0
      !
    end if xxif_jx_fft ! if ( jx_fft .eq. 1 .or. jx_fft .eq. nx_fft/2+1 ) then
    !
    return
    !
  end function off2ang_zero_or_nyq
  !
  subroutine off2ang_amp_phase ( f1_inp, c1_inp, f2_inp, c2_inp, c0_out )
    !
    ! interpolate between c1_inp and c2_inp using amplitude and phase
    !
    real,             intent (in   ) :: f1_inp         ! int coeff 1
    real,             intent (in   ) :: f2_inp         ! int coeff 2
    !
    complex,          intent (in   ) :: c1_inp         ! input  value 1
    complex,          intent (in   ) :: c2_inp         ! input  value 2
    complex,          intent (inout) :: c0_out         ! output value 
    !
    ! Local variables
    !
    real                             :: a0_out         ! output amplitude
    real                             :: p0_out         ! output phase
    !
    real                             :: a1_inp         ! input  amplitude 1
    real                             :: p1_inp         ! input  phase     1
    !
    real                             :: a2_inp         ! input  amplitude 2
    real                             :: p2_inp         ! input  phase     2
    !
    real                             :: d0_inp         ! no wrap phase diff
    real                             :: d1_inp         ! p1_inp wrapped diff
    real                             :: d2_inp         ! p2_inp wrapped diff
    !
    real                             :: s1_inp         ! p1_inp + two_pi
    real                             :: s2_inp         ! p2_inp + two_pi
    !
    real                             :: two_pi         ! 2. * pi
    !
    two_pi = 2. * pi
    !
    ! compute amplitude and phase 1
    !
    a1_inp = abs ( c1_inp )
    !
    xxif_a1_inp : if ( a1_inp .ne. 0 ) then
      !
      p1_inp = atan2 ( aimag ( c1_inp ) , real ( c1_inp ) )
      !
    else xxif_a1_inp ! if ( a1_inp .ne. 0 ) then
      !
      p1_inp = 0.
      !
    end if xxif_a1_inp ! if ( a1_inp .ne. 0 ) then
    !
    ! compute amplitude and phase 2
    !
    a2_inp = real ( c2_inp )
    a2_inp = abs ( c2_inp )
    !
    xxif_a2_inp : if ( a2_inp .ne. 0 ) then
      !
      p2_inp = atan2 ( aimag ( c2_inp ) , real ( c2_inp ) )
      !
    else xxif_a2_inp ! if ( a2_inp .ne. 0 ) then
      !
      p2_inp = 0.
      !
    end if xxif_a2_inp ! if ( a2_inp .ne. 0 ) then
    !
    ! it is possible that one or both of the phases, 
    ! p1_inp, p2_inp have wrapped around +- two_pi .  
    ! If neither or both have there is no problem.
    ! if only one has wrapped the linear interpolation will yield the 
    ! wrong result.
    ! So we look for the combination of the phase differences 
    ! +- two_pi which yield the smallest phase difference between 
    ! the two.  This assumes there should not be > two_pi difference.
    !
    s1_inp  = p1_inp + two_pi          ! p1_inp has wrapped
    s2_inp  = p2_inp + two_pi          ! p2_inp has wrapped
    !
    d0_inp = abs ( p2_inp - p1_inp ) ! neither or both wrapped
    d1_inp = abs ( p2_inp - s1_inp ) ! p1_inp has wrapped
    d2_inp = abs ( s2_inp - p1_inp ) ! p2_inp has wrapped 
    !
    xxif_dp_inp : if ( d1_inp .le. min ( d0_inp , d2_inp ) ) then
      !
      ! p1_inp has wrapped 
      !
      p1_inp = s1_inp
      !
    else if ( d2_inp .le. min ( d0_inp , d1_inp ) ) then
      !
      ! p2_inp has wrapped 
      !
      p2_inp = s2_inp
      !
    end if xxif_dp_inp
    !
    ! interpolate the amplitude and phase
    !
    a0_out = f1_inp * a1_inp + f2_inp * a2_inp
    !
    p0_out = f1_inp * p1_inp + f2_inp * p2_inp
    !
    ! reconstruct c0_out from the interpolated amplitude and phase
    !
    c0_out = cmplx ( a0_out * cos ( p0_out ), a0_out * sin ( p0_out ) ) 
    !
    return
    !
  end subroutine off2ang_amp_phase 
  !
  subroutine off2ang_fft_loc ( nx_inp, x0_inp, dx_inp, nx_fft, kx_fft )
    !
    integer,          intent (in   ) :: nx_inp       ! num x inp
    real,             intent (in   ) :: x0_inp       ! min x inp
    real,             intent (in   ) :: dx_inp       ! inc x inp
    integer,          intent (in   ) :: nx_fft       ! num x fft
    integer,          intent (inout) :: kx_fft ( : ) ! index x fft
    !
    integer                          :: nx_nyq ! nyquist index
    integer                          :: jx_inp
    real                             :: rx_inp
    integer                          :: jx_fft

    !
    nx_nyq = nx_fft / 2 + 1 ! nyquist index
    !
    do_jx_inp : do jx_inp = 1 , nx_inp
      !
      rx_inp = ( jx_inp - 1 ) * dx_inp + x0_inp
      !
      jx_fft = nint ( rx_inp / dx_inp ) + 1 
      !
      if ( rx_inp .lt. 0. ) jx_fft = nx_nyq - jx_fft + 1
      !
      xxif_jx_fft_err : &
      if ( jx_fft .lt. 1 .or. jx_fft .gt. nx_fft ) then
        !
        print'( " off2ang_fft_loc error " )'
        !
        stop
        !
      end if xxif_jx_fft_err 
      !
      kx_fft ( jx_inp ) = jx_fft
      !
    end do do_jx_inp 
    !
  end subroutine off2ang_fft_loc 
  !
end module off2ang_module
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
