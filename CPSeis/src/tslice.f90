!<CPS_v1 type="PROCESS"/>
!!------------------------------ tslice.f90 ---------------------------------!!
!!------------------------------ tslice.f90 ---------------------------------!!
!!------------------------------ tslice.f90 ---------------------------------!!

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
! Name       : tslice
! Category   : Sorts
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2007-01-03   by: Douglas Hanson Clean up code.
! Maturity   : production
! Purpose    : put time slices to the trace flow and/or a data file.
! Portability: No known limitations.
! Parallel   : Yes.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! Tslice puts time slices to the trace flow and/or a data file.
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!  tslice passes out its traces MINP at a time.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<trace_in_doc>
!-------------------------------------------------------------------------------
!                        TRACE INPUT REQUIREMENTS
!
! NO input traces -- tslice is a trace supplying process.
!
!-------------------------------------------------------------------------------
!</trace_in_doc>

!<trace_out_doc>
!-------------------------------------------------------------------------------
!                      TRACE OUTPUT CHARACTERISTICS
!
! This process outputs MINP traces at a time.
! This is a trace-supplying process.
!
!-------------------------------------------------------------------------------
!</trace_out_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                    GLOBAL PARAMETERS USED OR CHANGED
!
! Name      Description                          Action taken
! ----      -----------                          ------------
! NUMTR     max number of traces input/output    Set to 1
! GATHERED  whether traces are gathered          Set to .false.
! NWIH      number of words in trace header      used but not changed
! NDPT      number of sample values in trace     used but not changed
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                    TRACE HEADER WORDS USED OR CHANGED
!
! HDR     Description                Action taken
! ----    -----------                ------------
! 1       Sequential Trace Count     Set
! 2       Head mute                  Set
! 3       Current gather number      Set
! 4       No. within current gather  Set
! 64      Tail mute                  Set
!         HDR_LINE                   Set
!         HDR_CMP                    Set
!         HDR_OFF                    Set
!
!-------------------------------------------------------------------------------
!</header_word_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY                    
! 
!     Date       Author         Description
!     ----       ------         -----------
!  9  2007-01-03 Douglas Hanson Clean up code.
!     2003-11-07 Douglas Hanson Have tslice stack traces.
!     2003-04-11 Douglas Hanson Put into alphalib
!  8  2001-02-16 Douglas Hanson Allow SLICE_TOT=0
!  7  2000-12-15 Douglas Hanson Correct use of no_more_traces
!  6  2000-12-12 Douglas Hanson Add skip_wrapup 
!  5  2000-09-20 Douglas Hanson Fix slice_header ezgui error.
!  4  2000-09-19 Douglas Hanson No if test for control cards.
!  3  2000-09-06 Douglas Hanson fix headslice gui error
!  2  2000-08-25 Douglas Hanson cpsfcr
!  1  1999-12-01 Douglas Hanson Initial version.
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
!    NTR ignored on input because this is a trace supplying process.
!
! Upon output, NTR will have one of these values:
!    NTR = MINP          if this process is outputting traces.
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
!------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!
!------------------------------------------------------------------------------
!</programming_doc>

!-------------------------------------------------------------------------------
!<--  EZGUI code for the GUI goes in this section. />
!<gui_def>
!<NS TSLICE/NC=80>
!
!              TSLICE - Time SLICE output.
! Output slices to the trace flow and / or disk
! 
! OPTION~~=`CCCCCCC
! PATH_NAME=`SSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSSS
! 
! SLOW_HDR~~=`II          FAST_HDR~~=`II         
! SLOW_TOT~~=`IIIIIIIIII  FAST_TOT~~=`IIIIIIIIII  SLICE_TOT~~=`IIIIIIIIII 
! SLOW_MIN~~=`FFFFFFFFFF  FAST_MIN~~=`FFFFFFFFFF  SLICE_MIN~~=`FFFFFFFFFF 
! SLOW_MAX~~=`FFFFFFFFFF  FAST_MAX~~=`FFFFFFFFFF  SLICE_MAX~~=`FFFFFFFFFF 
! SLOW_INC~~=`FFFFFFFFFF  FAST_INC~~=`FFFFFFFFFF  SLICE_INC~~=`FFFFFFFFFF 
!                         FAST_SCALE=`FFFFFFFFFF 
! 
!   SLICE_HEADER
!   `IIIIIIIIIII
!   `IIIIIIIIIII
!   `IIIIIIIIIII
! 
!<PARMS SLICE_HEADER[/ML=128/XST/YST]>
!<PARMS TSLICE[screen1]>
! 
!</gui_def>
! 
!<HelpSection>
!
!<Help KEYWORD="SLICE_HEADER">
!<Tip> Header word slice array. </Tip>
! Default = none
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="SLOW_HDR">
!<Tip> Header word for the direction across output traces direction. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="SLOW_TOT">
!<Tip> Number of output bins in the across trace direction. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="SLOW_MIN">
!<Tip> Minimum output value for the across trace direction. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="SLOW_MAX">
!<Tip> Maximum output value for the across trace direction. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="SLOW_INC">
!<Tip> Output bin increment in the across trace direction. </Tip>
! Default = 1.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="FAST_HDR">
!<Tip> Header word for along the output traces direction. </Tip>
! Default = 7
! Allowed = 1 - NWIH
!</Help>
!
!<Help KEYWORD="FAST_TOT">
!<Tip> Number of output bins in the along trace direction. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="FAST_MIN">
!<Tip> Minimum output value for the along trace direction. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="FAST_MAX">
!<Tip> Maximum output value for the along trace direction. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="FAST_INC">
!<Tip> Output bin increment in the along trace direction. </Tip>
! Default = 1.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="FAST_SCALE">
!<Tip> Output time origin and sample rate scale factor. </Tip>
! Default = 100
! Allowed = real>0
! The output trace time sample rate will be set to FAST_INC / FAST_SCALE
! The output trace time origin      will be set to FAST_MIN / FAST_SCALE
!</Help>
!
!<Help KEYWORD="SLICE_TOT">
!<Tip> Number of output time slices. </Tip>
! Default = 1
! Allowed = int>0
!</Help>
!
!<Help KEYWORD="SLICE_MIN">
!<Tip> Minimum output time slice value. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="SLICE_MAX">
!<Tip> Maximum output time slice value. </Tip>
! Default = 0.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="SLICE_INC">
!<Tip> Output time slice increment. </Tip>
! Default = 1.0
! Allowed = real scalar
!</Help>
!
!<Help KEYWORD="OPTION">
!<Tip> Output option. </Tip>
! Default = EXTRENAL
! Allowed = EXTRENAL output slice traces extrnally after tslice.
! Allowed = INTRENAL output slice traces intrnally to a file.
! Allowed = BOTH  output both after tslice and to file.
!</Help>
!
!<Help KEYWORD="PATH_NAME">
!<Tip> Output file name. </Tip>
! Default = NONE
! Allowed = Character.
!</Help>
!
!</HelpSection>
!-------------------------------------------------------------------------------
!
module tslice_module
  !
  use trcio_module
  use cio_module
  use headsave_module
  use lav_module
  use matfun_module
  use memfun_module
  use named_constants_module
  use pathcheck_module
  use pattern_module
  use pc_module
  use pcpsx_module
  use prnfun_module
  use string_module
  use timeglob_module
  use trbuf_module
  use wavelet_module
  !
  implicit none
  !
  private
  !
  public :: tslice_create     ! uses the parameter cache.
  public :: tslice_initialize ! uses the parameter cache.
  public :: tslice_update     ! uses the parameter cache.
  public :: tslice_delete
  public :: tslice            ! main execution (trace processing) routine.
  public :: tslice_wrapup
  public :: tslice_set_option
  public :: tslice_input
  public :: tslice_write
  public :: tslice_output
  !
! rcs identifier string
  !
  character(len=100),public,save :: tslice_ident = &
  '$Id: tslice.f90,v 1.9 2007/01/03 14:01:49 Hanson prod sps $'
  !
  type, public :: tslice_struct
    !
    private
    !
    logical                             :: skip_wrapup ! wrapup flag.
    !
    character(len=8)                    :: option
    character(len=FILENAME_LENGTH)      :: path_name
    !
    integer                             :: fast_idx
    integer                             :: fast_tot
    integer                             :: fast_hdr
    integer                             :: fast_hdr_last
    real                                :: fast_min
    real                                :: fast_max
    real                                :: fast_inc
    real                                :: fast_scale
    !
    integer                             :: slow_idx
    integer                             :: slow_tot
    integer                             :: slow_hdr
    real                                :: slow_min
    real                                :: slow_max
    real                                :: slow_inc
    !
    integer                             :: slice_idx
    integer                             :: slice_tot
    real                                :: slice_min
    real                                :: slice_max
    real                                :: slice_inc
    !
    integer                             :: i_pass
    integer                             :: i0_inp
    integer                             :: j0_inp
    integer                             :: j0_sav
    integer                             :: j0_out
    integer                             :: i_loop
    logical                             :: l0_keep
    integer                             :: i_tr_max
    integer                             :: i_tr_glb
    real                                :: tr_max
    real                                :: tr_glb
    real                                :: tr_tmp
    !
    integer                             :: n_scr
    integer                             :: n_sto
    integer                             :: slice_cnt 
    integer                             :: n0_out 
    integer                             :: mt_inp
    integer                             :: mh_inp
    integer                             :: nh_inp
    integer                             :: nt_glb
    real                                :: dt_glb
    real                                :: t0_glb
    !
    integer                             :: slice_header_tot
    integer,                    pointer :: slice_header(:)
    real,                       pointer :: tr_slc(:, :, :)
    !
    type ( headsave_struct ),   pointer :: h  ! headsave structure
    type (trcio_struct),        pointer :: trcio_obj
    !
  end type tslice_struct
  !
  type(tslice_struct), pointer, save :: object      ! needed for traps.
  !
  integer,          parameter :: option_n = 3
  character(len=8), save      :: option_c(option_n) &
  = (/ 'EXTERNAL', 'INTERNAL', 'BOTH    ' /)
  !
  contains
  !
  subroutine tslice_create ( o )
    !
    type(tslice_struct), pointer :: o       ! arguments
    !
    allocate (o )
    !
    call tslice_initialize ( o )
    !
    return
    !
  end subroutine tslice_create
  !
  subroutine tslice_delete ( o )
    !
    type(tslice_struct), pointer :: o       ! arguments
    !
    call memfun_del (  o%tr_slc )
    !
    if ( associated      ( o%h ) ) &
    call headsave_delete ( o%h )
    !
    deallocate ( o )
    !
    return
    !
  end subroutine tslice_delete
  !
  subroutine tslice_initialize ( o )
    !
    type(tslice_struct), pointer :: o       ! arguments
    !
    integer                      :: i_err
    !
    call pc_get_global ( 'nwih' , o%nh_inp)
    call timeglob_get ( o%nt_glb, o%t0_glb, o%dt_glb)
    !
    ! initialize parameters
    !
    o%slice_tot = 1
    o%slice_min = ((o%nt_glb - 1) * o%dt_glb + o%t0_glb) * .5
    o%slice_inc = o%dt_glb
    o%slow_hdr = 7
    o%slow_tot = 1
    o%slow_min = 0.
    o%slow_inc = 1.
    o%fast_hdr = 8
    o%fast_tot = 1
    o%fast_min = 0.
    o%fast_inc = 1.
    o%fast_scale = 100.
    o%option = 'BOTH'
    o%path_name = 'tslice'
    o%slice_header_tot = 0
    !
    call headsave_create ( o%h, 'tslice', o%nh_inp, i_err )
    !
    !if ( i_err .ne. 0 ) go to 998
    !
    call tslice_update ( o )
    !
    return
    !
  end subroutine tslice_initialize
  !
  subroutine tslice_update ( o )
    !
    type(tslice_struct),        pointer :: o                ! tslice strucutre
    !
    type(tslice_struct), save,  pointer :: object      ! needed for traps.
    !
    object => o         ! needed for traps.
    !
    o%skip_wrapup = .true.    ! needed for the wrapup routine.
    !
    ! get tslice parameters
    !
    print'(" tslice_update bef tslice_get ")'
    !
    call tslice_get ( o )
    !
    ! verify tslice parameters
    !
    print'(" tslice_update bef tslice_verify ")'
    !
    call tslice_verify ( o )
    !
    ! put tslice parameters
    !
    print'(" tslice_update bef tslice_put ")'
    !
    call tslice_put ( o )
    !
    ! prep tslice parameters
    !
    print'(" tslice_update bef tslice_prep ")'
    !
    call tslice_prep ( o )
    !
    print'(" tslice_update end ")'
    !
    return
    !
999 continue
    !
    write(pc_get_lun(), '( &
    &  /, " error in tslice_update", &
    & / ," during xxx " &
    & )' )
    call pc_error ( 'tslice_update error ' )
    !
    return
    !
  end subroutine tslice_update
  !
  subroutine tslice_get ( o )
    !
    ! get tslice parameters
    !
    type(tslice_struct),        pointer :: o                ! tslice strucutre
    !
    call timeglob_get ( o%nt_glb, o%t0_glb, o%dt_glb)
    call pc_get_global ( 'nwih' , o%nh_inp)
    call pc_get ( 'fast_hdr',   o%fast_hdr )
    call pc_get ( 'fast_tot',   o%fast_tot )
    call pc_get ( 'fast_min',   o%fast_min )
    call pc_get ( 'fast_max',   o%fast_max )
    call pc_get ( 'fast_inc',   o%fast_inc )
    call pc_get ( 'fast_scale', o%fast_scale )
    call pc_get ( 'slow_hdr',   o%slow_hdr )
    call pc_get ( 'slow_tot',   o%slow_tot )
    call pc_get ( 'slow_min',   o%slow_min )
    call pc_get ( 'slow_max',   o%slow_max )
    call pc_get ( 'slow_inc',   o%slow_inc )
    call pc_get ( 'slice_tot',  o%slice_tot )
    call pc_get ( 'slice_min',  o%slice_min )
    call pc_get ( 'slice_max',  o%slice_max )
    call pc_get ( 'slice_inc',  o%slice_inc )
    call pc_get ( 'option',     o%option    )
    call pc_get ( 'pathname',   o%path_name )
    call pc_get ( 'path_name',  o%path_name )
    call pc_alloc ( 'slice_header', o%slice_header, o%slice_header_tot)
    !
    return
    !
  end subroutine tslice_get
  !
  subroutine tslice_put ( o )
    !
    ! put tslice parameters
    !
    type(tslice_struct),        pointer :: o                ! tslice strucutre
    !
    integer                             :: n0_inp           ! num output traces
    !
    !  put the current globals
    !
    n0_inp = 1
    !
    call pc_put_global  ( 'NUMTR'      ,n0_inp     )
    !
    call pc_put_options_field ( 'option', option_c, option_n )
    !
    call pc_put ( 'option',     o%option    )
    call pc_put ( 'path_name',  o%path_name )
    !
    call pc_put ( 'fast_hdr',   o%fast_hdr )
    call pc_put ( 'fast_tot',   o%fast_tot )
    call pc_put ( 'fast_min',   o%fast_min )
    call pc_put ( 'fast_max',   o%fast_max )
    call pc_put ( 'fast_inc',   o%fast_inc )
    call pc_put ( 'fast_scale', o%fast_scale )
    !
    call pc_put ( 'slow_hdr',   o%slow_hdr )
    call pc_put ( 'slow_tot',   o%slow_tot )
    call pc_put ( 'slow_min',   o%slow_min )
    call pc_put ( 'slow_max',   o%slow_max )
    call pc_put ( 'slow_inc',   o%slow_inc )
    !
    call pc_put ( 'slice_tot',  o%slice_tot )
    call pc_put ( 'slice_min',  o%slice_min )
    call pc_put ( 'slice_max',  o%slice_max )
    call pc_put ( 'slice_inc',  o%slice_inc )
    !
    call pc_put ( 'slice_header', o%slice_header, o%slice_header_tot)
    !
    ! put tslice time parameters
    !
    call tslice_put_time ( o ) 
    !
    call pc_put_global  ( 'gathered',     .false. )
    call pc_put_control ( 'need_request', .true.  )
    call pc_put_control ( 'need_label',   .true.  )
    call pc_put_control ( 'nscratch',     o%n_scr )
    call pc_put_control ( 'nstore',       o%n_sto )
    call pc_put_control ( 'parallel_safe' ,         .true.                )
    call pc_put_control ( 'pcps_send_mode' ,        'PCPS_BOSS_EXECS'     )
    call pc_put_control ( 'pcps_generator_mode' ,   'PCPS_TRACE_GEN'      )
    !
    return
    !
  end subroutine tslice_put
  !
  subroutine tslice_verify ( o )
    !
    ! verify tslice parameters
    !
    type(tslice_struct),        pointer :: o                ! tslice strucutre
    !
    integer                             :: i_stat
    !
    if ( o%slow_hdr <= 0) &
    call pc_error ('TSLICE : SLOW_HDR must be > 0 ' )
    !
    if ( o%slow_hdr > o%nh_inp) &
    call pc_error ('TSLICE : SLOW_HDR must be < NWIH ' )
    !
    if ( o%slow_tot <= 0) &
    call pc_error ('TSLICE : SLOW_TOT must be > 0' )
    !
    if ( o%slow_inc == 0.0) &
    call pc_error ('TSLICE : SLOW_INC must not be 0 ' )
    !
    if ( o%fast_scale == 0.0) &
    call pc_error ('TSLICE : fast_scale must not be 0 ' )
    !
    if ( o%fast_hdr <= 0) &
    call pc_error ('TSLICE : FAST_HDR must be > 0' )
    !
    if ( o%fast_hdr > o%nh_inp) &
    call pc_error ('TSLICE : FAST_HDR must be < NWIH' )
    !
    if ( o%fast_tot <= 0) &
    call pc_error ('TSLICE : FAST_TOT must be > 0' )
    !
    if ( o%fast_inc == 0.0) &
    call pc_error ('TSLICE : FAST_INC must not be 0' )
    !
     i_stat = pattern_stop2('tslice:', .true., &
     o%slow_min, o%slow_inc, o%slow_max, o%slow_tot, &
     'slow_min', 'slow_inc', 'slow_max', 'slow_tot', &
     pc_verify_scalar('slow_min' ), pc_verify_scalar('slow_inc' ), &
     pc_verify_scalar('slow_max' ), pc_verify_scalar('slow_tot' ))
     !
     i_stat = pattern_stop2('tslice:', .true., &
     o%fast_min, o%fast_inc, o%fast_max, o%fast_tot, &
     'fast_min', 'fast_inc', 'fast_max', 'fast_tot', &
     pc_verify_scalar('fast_min' ), pc_verify_scalar('fast_inc' ), &
     pc_verify_scalar('fast_max' ), pc_verify_scalar('fast_tot' ))
     !
     if ( o%slice_tot .gt. 0 ) &
     i_stat = pattern_stop2('tslice:', .true., &
     o%slice_min, o%slice_inc, o%slice_max, o%slice_tot, &
     'slice_min', 'slice_inc', 'slice_max', 'slice_tot', &
     pc_verify_scalar('slice_min' ), pc_verify_scalar('slice_inc' ), &
     pc_verify_scalar('slice_max' ), pc_verify_scalar('slice_tot' ))
     !
    !
    ! - Check PATH_NAME
    !
    call pathcheck (keyword  = 'pathname',    &
                  pathname = o%path_name, &
                  ext      = 'tslice',      &
                  status   = i_stat         )
                  !
  check_path_name:    &
    if ( i_stat == path_unspecified ) then   ! char
    !
    if ( (pc_verify_screen (keyword = "slice")       &
        .and. (pc_get_update_state () == pc_gui))    &
        .or. pc_verify_scalar (keyword = 'path_name') ) then
    call pc_error (msg1 = 'PATH_NAME (',          &
                   var1 = trim (o%path_name),    &
                   msg2 = ') was not specified.')
    end if
    !
    else if ( i_stat == path_invalid ) then check_path_name  ! char
    !
    call pc_error (msg1 = 'Bad value for PATH_NAME (',    &
                   var1 = trim (o%path_name),      &
                   msg2 = ')')
    !
    end if check_path_name
    !
    !  check input values
    call tslice_set_option ( o%option)
    !
    ! save header values
    !
    o%slice_header(1:o%slice_header_tot) = &
    max(1, min(o%nh_inp, o%slice_header(1:o%slice_header_tot)))
    !
    ! save trace values
    !
    o%slice_min = max(o%t0_glb, &
    min((o%nt_glb-1)*o%dt_glb+o%t0_glb, o%slice_min))
    !
    o%slice_tot = max (0, min(o%slice_tot, &
    nint(((o%nt_glb-1)*o%dt_glb+o%t0_glb-o%slice_min)/o%slice_inc+1)))
    !
    o%slice_inc = o%dt_glb * nint(o%slice_inc/o%dt_glb)
    o%slice_max = max(0,(o%slice_tot-1)) * o%slice_inc + o%slice_min
    !
    o%slow_hdr = max(1, min(o%nh_inp, o%slow_hdr))
    o%fast_hdr = max(1, min(o%nh_inp, o%fast_hdr))
    !
    o%j0_inp = 0
    !
    o%n_scr = 0
    !
    o%n_sto = 0
    !
    ! set some counters
    !
    o%slice_cnt = o%slice_header_tot + o%slice_tot
    o%n0_out    = o%slow_tot * o%slice_cnt 
    !
    o%tr_glb = 0.
    o%tr_max = 0.
    o%i_tr_glb = 0
    o%i_tr_max = 0
    !
    o%i_pass = 1
    o%j0_inp = 0
    o%j0_sav = 0
    o%j0_out = 0
    o%i_loop = 0
    !
    return
    !
  end subroutine tslice_verify
  !
  subroutine tslice_prep ( o )
    !
    ! prep tslice parameters
    !
    type(tslice_struct),        pointer :: o                ! tslice strucutre
    !
    integer                             :: i_err
    !
    if ( pc_do_not_process_traces() ) return
    !
    o%skip_wrapup = .false.
    !
    o%n_sto = 0
    !
    call memfun_prn_put ( .true. )
    call memfun_sum_put ( o%n_sto )
    call memfun_all (  &
    o%tr_slc, o%fast_tot, o%slow_tot, o%slice_cnt, 'tr_slc', i_err )
    call memfun_sum_get ( o%n_sto )
    call memfun_prn_put ( .false. )
    !
    o%tr_slc = 0.
    !
    i_err = 0
    !
    write(pc_get_lun(), '( &
    & /, " tslice_update ", /, " REVISION: ", &
    & " 3  2006-06-22 Douglas Hanson Clean up code. " &
    & )')
    !
    call tslice_print ( o, 'tslice_prep' )
    !
    return
    !
  end subroutine tslice_prep 
  !
  subroutine tslice_put_time ( o ) 
    !
    ! put tslice time parameters
    !
    type(tslice_struct),        pointer :: o                ! tslice strucutre
    !
    !  if not outputing the original traces set the output globals
    !
    xxif_internal : &
    if ( string_upper_compare ( o%option, 'INTERNAL' ) ) then
      !
      call timeglob_put ( o%nt_glb, o%t0_glb, o%dt_glb )
      !
    else xxif_internal 
      !
      call timeglob_put ( &
      o%fast_tot, o%fast_min/o%fast_scale, o%fast_inc/o%fast_scale )
      !
    end if xxif_internal 
    !
    return
    !
  end subroutine tslice_put_time 
  !
  subroutine tslice_print ( o, c_title )
    !
    type(tslice_struct),    pointer :: o                          ! arguments
    character(len=*), intent(in   ) :: c_title
    !
    print'( &
    & /, " tslice_print ", a, &
    & /, " option   =", a, &
    & /, " path_name=", a, &
    & /, " nt_glb=", i8, " t0_glb=", f10.4, " t1_glb=", f10.4, &
    & " dt_glb=", f10.4, &
    & /, " slice_tot=", i8, " slice_min=", f10.4, &
    & " slice_max=", f10.4, " slice_inc=", f10.4, &
    & /, " slow_hdr=", i8, " slow_tot=", i8, &
    & " slow_min=", f10.4, " slow_max=", f10.4, " slow_inc=", f10.4, &
    & /, " fast_hdr=", i8, " fast_tot=", i8, &
    & " fast_min=", f10.4, " fast_max=", f10.4, " fast_inc=", f10.4, &
    & " fast_scale=", g10.4, &
    & /, " slice_header_tot=", i8 &
    & )', &
    trim(c_title), &
    trim(o%option), &
    trim(o%path_name), &
    o%nt_glb, o%t0_glb, &
    (o%nt_glb-1)*o%dt_glb+o%t0_glb, o%dt_glb, &
    o%slice_tot, o%slice_min, o%slice_max, o%slice_inc, &
    o%slow_hdr, o%slow_tot, o%slow_min, o%slow_max, o%slow_inc, &
    o%fast_hdr, o%fast_tot, o%fast_min, o%fast_max, o%fast_inc, &
    o%fast_scale, &
    o%slice_header_tot
    !
    return
    !
  end subroutine tslice_print 
  !
  subroutine tslice_input_trap ( )
    !
    return
    !
  end subroutine tslice_input_trap
  !
  subroutine tslice ( o, n0_inp, hd_inp, tr_inp)
    !
    type(tslice_struct),        pointer :: o                ! tslice strucutre
    integer,          intent(inout)     :: n0_inp                  ! arguments
    double precision, intent(inout)     :: hd_inp(:, :)            ! arguments
    real,             intent(inout)     :: tr_inp(:, :)            ! arguments
    !
    integer                             :: i0_inp
    integer                             :: i_err
    !
    o%mh_inp = size ( hd_inp, 1 )
    !
    o%mt_inp = size ( tr_inp, 1 )
    !
    i_err = 0
    !
    o%i_loop = o%i_loop + 1
    !
    ! if the previous pass output original trace and we need to go back
    ! for more input traces
    !
    xxif_i_pass_m1 : if ( o%i_pass .eq. -1 ) then
      !
      o%i_pass = 1
      !
      go to 1
      !
    end if xxif_i_pass_m1
    !
    xxif_i_pass_p1 : if ( o%i_pass .eq. 1 ) then
      !
      !  cycle over input traces
      !
      do_input : do i0_inp = 1 , n0_inp
        !
        o%j0_inp = o%j0_inp + 1
        !
        call tslice_input ( o, hd_inp(:, i0_inp), tr_inp(:, i0_inp) )
        !
        !  get the max amplitude for this trace
        !
        o%tr_tmp = matfun_amax(o%nt_glb, tr_inp(:, i0_inp))
        !
        !  save headers for input traces (locations 1 - 4)
        !
        call headsave_store ( o%h, o%j0_inp, 1, hd_inp ( :, i0_inp ) )
        !
        !  save headers for output traces (locations 9 - 12)
        !
        if ( string_upper_compare ( o%option, 'INTERNAL' ) ) &
        call headsave_store ( o%h, o%j0_inp, 9, hd_inp ( :, i0_inp ) )
        !
        !  get the saved max amplitude
        !
        xxif_keep : if ( o%l0_keep ) then
          !
          o%j0_sav = o%j0_sav + 1
          !
          if ( o%tr_tmp .gt. o%tr_max) &
          o%i_tr_max = o%j0_inp
          !
          o%tr_max = max(o%tr_max, o%tr_tmp)
          !
          !  save headers for saved input traces (locations 4 - 9)
          !
          call headsave_store ( o%h, o%j0_sav, 5, hd_inp ( :, i0_inp ) )
          !
        end if xxif_keep 
        !
        !  get the global max amplitude
        !
        if ( o%tr_tmp .gt. o%tr_glb) &
        o%i_tr_glb = o%j0_inp
        !
        o%tr_glb = max(o%tr_glb, o%tr_tmp)
        !
      end do do_input 
      !
      !  if outputing the original traces return below
      !  and set the flag so the next time tslice is entered we return above
      !  for more input traces
      if ( string_upper_compare ( o%option, 'INTERNAL' ) ) o%i_pass = -1
      !
      !  if all done with input write the output traces
      !
      xxif_done_input : if ( n0_inp .eq. no_more_traces ) then
        !
        ! write the slices to a file
        !
        if ( string_upper_compare ( o%option, 'INTERNAL' ) &
        .or. string_upper_compare ( o%option, 'BOTH' ) ) &
        call tslice_write ( o, i_err )
        !
        if ( i_err .ne. 0 ) go to 998
        !
      write(pc_get_lun(), '( &
      & /, " tslice", &
      & /, " traces disk file =", a, &
      & /, " number of traces  input=", i8, " max amp=", g16.9, &
      & " at trace #", i8, &
      & /, " number of traces  saved=", i8, " max amp=", g16.9, &
      & " at trace #", i8 &
      & )' ) &
      trim(o%path_name), &
      o%j0_inp, o%tr_glb, o%i_tr_glb, &
      o%j0_sav, o%tr_max, o%i_tr_max
      !
      !  print input header words
      call headsave_print ( o%h, ' tslice ', 1 )
      call headsave_print ( o%h, ' tslice ', 9 )
      !
      !  if outputting disk only set the process flow to done i_pass = 3
      if ( string_upper_compare ( o%option, 'INTERNAL' ) ) then
          o%i_pass = 3
      else    ! if ( string_upper_compare ( o%option, 'INTERNAL' ) ) then
      !  if outputting slices as traces set the process flow to i_pass = 2
        o%i_pass = 2
      end if    ! if ( string_upper_compare ( o%option, 'INTERNAL' ) ) then
      !
    end if xxif_done_input 
    !
    end if xxif_i_pass_p1 
    !
    !  output slices
    !
    xxif_i_pass_p2 : if ( o%i_pass .eq. 2 ) then
      !
      xxif_internal : &
      if ( string_upper_compare ( o%option, 'INTERNAL' ) ) then
        !
        ! internal output only
        !
        if ( n0_inp .eq. no_more_traces) o%i_pass = 3
        !
      else xxif_internal 
        !
        ! external output
        !
        o%j0_out = o%j0_out + 1
        !
        xxif_output : if ( o%j0_out .le. o%n0_out ) then
          !
          call tslice_output ( o, hd_inp(:, 1), tr_inp(:, 1) )
          !
          n0_inp = 1
          !
          !  save headers for output traces (locations 9 - 12)
          !
          call headsave_store ( o%h, o%j0_out, 9, hd_inp ( :, i0_inp ) )
          !
        else xxif_output 
          !
          if ( .not. string_upper_compare ( o%option, 'INTERNAL' ) ) &
          call headsave_print ( o%h, ' tslice ', 9 )
          !
          n0_inp = no_more_traces
          o%i_pass = 3
          !
        end if xxif_output 
        !
      end if xxif_internal 
      !
    end if xxif_i_pass_p2 
    !
    !write(pc_get_lun(), &
    !& '(" n0_inp=", i5, " i_pass=",i8 )' ) &
    !n0_inp, o%i_pass
    !
    !  come to here to skip processing
    !
  1 continue
    !
    !  return below to ouput another trace
    ! fatal error
    !
    xxif_fatal_error : if ( i_err .ne. 0 ) then
      !
      n0_inp = fatal_error
      !
    else if ( o%i_pass .eq. 1 ) then    ! return above for input trace
      !
      n0_inp = need_traces
      !
    else if ( n0_inp .eq. no_more_traces ) then
      !
      ! all done
      !
      n0_inp = no_more_traces
      !
    else xxif_fatal_error 
      !
    end if xxif_fatal_error 
    !
    return
    !
998 continue
    !
    write(pc_get_lun(), '( &
    & /, " error in tslice", &
    & /, " during tslice_write " &
    & )' )
    !
    n0_inp = fatal_error
    !
    call pc_error ( 'tslice error ' )
    !
    return
    !
  end subroutine tslice
  !
  subroutine tslice_wrapup ( o )
    !
    type(tslice_struct),        pointer :: o                ! tslice strucutre
    !
    if ( o%skip_wrapup ) return
    !
         o%skip_wrapup = .true.
    !
    return
    !
  end subroutine tslice_wrapup
  !
  subroutine tslice_set_option ( option )
    !
    ! set option to TRACE, FILE or BOTH
    !
    character(len=*), intent(inout) :: option
    !
    ! cap the otpion string
    !
    call string_to_upper ( option )
    !
    xxif_option : &
    if ( string_upper_compare ( option(1:1), 'E' ) ) then
      !
      option = 'EXTERNAL'
      !
    else if ( string_upper_compare ( option(1:1), 'I' ) ) then
      !
      option = 'INTERNAL'
      !
    else xxif_option 
      !
      option = 'BOTH'
      !
    end if xxif_option 
    !
    return
  end subroutine tslice_set_option
  !
  subroutine tslice_input ( o, hd_inp, tr_inp )
    !
    ! put trace values into memory
    !
    type(tslice_struct),        pointer :: o                ! tslice strucutre
    double precision, intent(in   ) :: hd_inp(:)
    real,             intent(in   ) :: tr_inp(:)
    !
    integer                         :: head_idx
    integer                         :: slice_idx
    integer                         :: jt_inp
    real                            :: rt_inp
    !
    o%slow_idx = nint((hd_inp(o%slow_hdr)-o%slow_min)/o%slow_inc) + 1
    !
    o%fast_idx = nint((hd_inp(o%fast_hdr)-o%fast_min)/o%fast_inc) + 1
    !
    o%l0_keep = o%slow_idx .ge. 1 .and. o%slow_idx .le. o%slow_tot &
          .and. o%fast_idx .ge. 1 .and. o%fast_idx .le. o%fast_tot
    !
    xxif_keep : if ( o%l0_keep ) then
      !
      ! save header values
      !
      do_head : do head_idx = 1 , o%slice_header_tot
        !
        o%tr_slc ( o%fast_idx, o%slow_idx, head_idx  ) = &
          hd_inp (          o%slice_header(head_idx) )
        !
      end do do_head 
      !
      ! save trace values
      !
      do_slice : do slice_idx = 1 , o%slice_tot
        !
        rt_inp = (slice_idx - 1) * o%slice_inc + o%slice_min
        !
        jt_inp = max ( 1, min ( o%nt_glb, &
                   nint ( ( rt_inp-o%t0_glb)/o%dt_glb)+1))
        !
        o%tr_slc(o%fast_idx, o%slow_idx, o%slice_header_tot+slice_idx) = &
        o%tr_slc(o%fast_idx, o%slow_idx, o%slice_header_tot+slice_idx) + &
          tr_inp(jt_inp)
        !
      end do do_slice
      !
    end if xxif_keep 
    !
    return
    !
  end subroutine tslice_input
  !
  subroutine tslice_write ( o, i_err )
    !
    type(tslice_struct),        pointer :: o                ! tslice strucutre
    integer,              intent(inout) :: i_err
    !
    integer                             :: slow_idx
    integer                             :: slice_idx
    double precision                    :: hd_tmp(o%mh_inp) ! automaitc aray
    real                                :: tr_tmp(o%fast_tot) ! automaitc aray
    !
    i_err = 0
    !
    if ( string_upper_compare ( o%path_name, 'NONE' ) ) return
    !
    ! open and write the data file only on pe 0
    !
    call tslice_open ( &
                       o%trcio_obj, o%path_name, 'w', .true., &
                       o%fast_tot, o%fast_min, o%fast_inc, o%fast_scale, &
                       i_err &
                     )
    !
    if ( i_err .lt. 0 ) go to 998
    !
    !  write each trace
    !
    o%j0_out = 0
    !
    do_slice_idx : do slice_idx = 1 , o%slice_cnt
      !
      do_slow_idx : do slow_idx = 1 , o%slow_tot
        !
        hd_tmp = 0.
        !
        o%j0_out = o%j0_out + 1
        !
        !  put the output header and trace j0_out into hd_tmp and tr_tmp
        !
        call tslice_output ( o, hd_tmp, tr_tmp )
        !
        if ( pcpsx_i_pel() .eq. 0 ) &
        i_err = trcio_write_trace ( &
                                    file = o%trcio_obj, &
                                    hd   = hd_tmp,      &
                                    tr   = tr_tmp,      &
                                    tnum = o%j0_out     &
                                  )
        !
        call pcpsx_check_worker_errors ( i_err )
        !
        if ( i_err .lt. 0 ) go to 997
        !
      end do do_slow_idx
      !
    end do do_slice_idx
    !
    call tslice_close ( o%trcio_obj, .false., i_err )
    !
    if ( i_err .lt. 0 ) go to 996
    !
    i_err = 0    ! reset i_err to 0, may be > 0 from tfio
    !
1999 continue
    !
    call pcpsx_check_worker_errors ( i_err )
    !
    if ( i_err .ge. 0 ) i_err = 0
    !
    return
    !
996 continue
    !
    write(pc_get_lun(), '( &
    & /, " error in tslice_write ", &
    & /, " during tslice_close " &
    & )' )
    !
    go to 999
    !
997 continue
    !
    write(pc_get_lun(), '( &
    & /, " error in tslice_write ", &
    & /, " during trcio_write_trace " &
    & )' )
    !
    go to 999
    !
998 continue
    !
    write(pc_get_lun(), '( &
    & /, " error in tslice_write ", &
    & /, " during tslice_open " &
    & )' )
    !
    go to 999
    !
999 continue
    write(pc_get_lun(), '( &
    & /, " error in tslice_write " &
    & )' )
    i_err = -1
    !
    go to 1999
    !
  end subroutine tslice_write
  !
  subroutine tslice_output ( o, hd_tmp, tr_tmp )
    !
    !  output the j0_out trace
    !
    type(tslice_struct),        pointer :: o                ! tslice strucutre
    !
    double precision,     intent(inout) :: hd_tmp(:)
    real,                 intent(inout) :: tr_tmp(:) 
    !
    !  open the trace file
    !
    xxif_output : if ( o%j0_out .le. o%n0_out ) then
      !
      o%slice_idx = (o%j0_out-1) / o%slow_tot  + 1
      !
      o%slow_idx = mod(o%j0_out-1, o%slow_tot) + 1
      !
      tr_tmp(1:o%fast_tot) = &
      o%tr_slc(1:o%fast_tot, o%slow_idx, o%slice_idx)
      !
      hd_tmp = 0.
      !
      ! set the output header words
      !
      hd_tmp(hdr_sequence) = o%j0_out
      !
      hd_tmp(hdr_top_mute) = 1
      !
      hd_tmp(hdr_current_group) = o%slow_idx
      !
      hd_tmp(o%slow_hdr) = (o%slow_idx - 1) * o%slow_inc + o%slow_min
      !
      hd_tmp(o%fast_hdr) = (o%slice_idx - 1) * o%slice_inc + o%slice_min
      !
      hd_tmp(hdr_current_channel) = o%slice_idx
      !
      hd_tmp(hdr_bottom_mute) = o%fast_tot
      !
      hd_tmp ( hdr_lav ) = lav ( tr_tmp, o%fast_tot )  ! max amp
      !
      if ( o%slow_hdr .eq. hdr_midpoint_xgrid &
      .or. o%fast_hdr .eq. hdr_midpoint_xgrid ) &
      hd_tmp(hdr_midpoint_xloc) = hd_tmp(hdr_midpoint_xgrid)
      !
      if ( o%slow_hdr .eq. hdr_midpoint_xloc &
      .or. o%fast_hdr .eq. hdr_midpoint_xloc ) &
      hd_tmp(hdr_midpoint_xgrid) = hd_tmp(hdr_midpoint_xloc)
      !
      if ( o%slow_hdr .eq. hdr_midpoint_ygrid &
      .or. o%fast_hdr .eq. hdr_midpoint_ygrid ) &
      hd_tmp(hdr_midpoint_yloc) = hd_tmp(hdr_midpoint_ygrid)
      !
      if ( o%slow_hdr .eq. hdr_midpoint_yloc &
      .or. o%fast_hdr .eq. hdr_midpoint_yloc ) &
      hd_tmp(hdr_midpoint_ygrid) = hd_tmp(hdr_midpoint_yloc)
      !
      hd_tmp(hdr_source_xloc   ) = hd_tmp(hdr_midpoint_xloc )
      hd_tmp(hdr_source_yloc   ) = hd_tmp(hdr_midpoint_yloc )
      hd_tmp(hdr_source_xgrid  ) = hd_tmp(hdr_midpoint_xgrid)
      hd_tmp(hdr_source_ygrid  ) = hd_tmp(hdr_midpoint_ygrid)
      !
      hd_tmp(hdr_receiver_xloc ) = hd_tmp(hdr_midpoint_xloc )
      hd_tmp(hdr_receiver_yloc ) = hd_tmp(hdr_midpoint_yloc )
      hd_tmp(hdr_receiver_xgrid) = hd_tmp(hdr_midpoint_xgrid)
      hd_tmp(hdr_receiver_ygrid) = hd_tmp(hdr_midpoint_ygrid)
      !
    !print*,' j0_out=',j0_out,' slow_idx=',slow_idx,' slice_idx=',slice_idx
    !if(slow_idx.eq.1)&
    !write(pc_get_lun(),'(" q2 i=",,i8," t=",g12.6,)')&
    !(i,tr_tmp(i),i=1,fast_tot)
    !
    end if xxif_output 
    !
    return
    !
  end subroutine tslice_output
  !
  subroutine tslice_open ( &
                           trcio_obj, path_name, io_mode, remove_file, &
                           nz_out, z0_out, dz_out, rz_scl, &
                           i_err &
                         )
    !
    ! open a trace file 
    !
    type (trcio_struct),       pointer :: trcio_obj ! trcio structure
    character(len=*),    intent(in   ) :: path_name ! file name
    character(len=*),    intent(in   ) :: io_mode
    logical,             intent(in   ) :: remove_file
    !
    integer,             intent(in   ) :: nz_out    ! z num trc
    real,                intent(in   ) :: z0_out    ! z min trc
    real,                intent(in   ) :: dz_out    ! z inc trc
    real,                intent(in   ) :: rz_scl    ! rz_scale
    !
    integer,             intent(inout) :: i_err     ! error 0 O.K. -1 err
    !
    ! Local variables
    !
    integer,                 parameter :: mh_out = 64
    !
    integer                            :: j_err
    !
    integer                            :: nwih     
    integer                            :: ndpt     
    real                               :: strt_val 
    real                               :: srate    
    integer                            :: lbo_version 
    !
    integer, save                      :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! nullify the trcio structure
    !
    nullify ( trcio_obj )
    !
    if ( string_upper_compare ( path_name(1:4), 'NONE' ) ) return
    !
    ! open and write the data file only on pe 0
    !
    print'(" tslice_open bef cio_remove path_name=",a)', &
    trim(path_name)
    !
    if ( remove_file ) &
    j_err = cio_remove ( path_name )
    !
    print'(" tslice_open bef trcio_open path_name=",a)', &
    trim(path_name)
    !
    nwih     = mh_out
    ndpt     = nz_out
    strt_val = z0_out / rz_scl
    srate    = dz_out / rz_scl  
    lbo_version = 2
    !
    print'(" bef trcio_obj nwih=",i8," ndpt=",i8,&
    & " strt_val=",g12.6," srate=",g12.6," lbo_version=",i8)', &
    nwih, ndpt, strt_val, srate, lbo_version 
    !
    trcio_obj => trcio_open ( &
                              filename = path_name,       &
                              io_mode  = io_mode,         &
                              scratch  = .false.,         &
                              nwih     = nwih,            &
                              ndpt     = ndpt,            &
                              strt_val = strt_val,        &
                              srate    = srate            &
                              !srate    = srate,           &
                              !lbo_version = lbo_version   &
                            )
    !
    print'(" aft trcio_obj nwih=",i8," ndpt=",i8,&
    & " strt_val=",g12.6," srate=",g12.6," lbo_version=",i8)', &
    nwih, ndpt, strt_val, srate, lbo_version 
    !
    if ( .not. associated ( trcio_obj ) ) i_err = -1
    !
    print'(" tslice_open aft trcio_open err=",i8)', &
    i_err 
    !
    if ( i_err .lt. 0 ) go to 998
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in tslice_open ", &
    & /, " during trcio_open " &
    & )')
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in tslice_open " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine tslice_open 
  !
  subroutine tslice_close ( trcio_obj, remove_file, i_err )
    !
    ! write a trace file for an array
    !
    type (trcio_struct),       pointer :: trcio_obj ! trcio structure
    logical,             intent(in   ) :: remove_file
    integer,             intent(inout) :: i_err     ! error 0 O.K. -1 err
    !
    ! Local variables
    !
    integer, save                      :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    print'(" tslice_close bef trcio_close ")'
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    i_err = trcio_close ( file=trcio_obj, remove=remove_file )
    !
    print'(" tslice_close aft trcio_close i_err=",i8)', i_err
    !
    call pcpsx_check_worker_errors ( i_err )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in tslice_close ", &
    & /, " during trcio_close " &
    & )')
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in tslice_close " &
    & )')
    !
    i_err = -1
    !
    return
    !
  end subroutine tslice_close 
  !
end module tslice_module
!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!
