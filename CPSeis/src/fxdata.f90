!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- fxdata.f90 -------------------------------!!
!!------------------------------- fxdata.f90 -------------------------------!!
!!------------------------------- fxdata.f90 -------------------------------!!

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
! Name       : fxdata    ( FX IO routines )
! Category   : migrations
! Written    : 2002-09-04   by: Douglas Hanson
! Revised    : 2007-10-23   by: Douglas Hanson Initialize j_err to 0 in close.
! Maturity   : beta
! Purpose    : Card IO routines for FX migrations.
! Portability: No known limitations.
! Parallel   : Yes
!
!------------------------------------------------------------------------------ 
!</brief_doc>

!<descript_doc>
!------------------------------------------------------------------------------ 
!                         GENERAL DESCRIPTION
!
!  This routine handles IO processes for FX migrations.
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
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!------------------------------------------------------------------------------ 
!                          CALLING SEQUENCE
!
!
! create the fxdata structure, f_s
!  call fxcard_create_1 ( &
!                      o
!                     i_err &
!                    )
!
! delete the fxdata structure, f_s
!                         b      o
!  call fxcard_delete ( f_s, i_err )
!
!
! open the fxdata files
!                         b      o
!  call fxdata_open ( f_s, i_err )
!
!
!                              i
! i_err = fxdata_gather_count(path_info)
! determine the number of gathers in the file path_info
!
!                                i         o    o     o
! i_err = fxdata_info_to_fxcard(path_info,cobj,card1,ncards)
! simple read of cards in path_info and store in cobj
!
! i_err = fxdata_rd_info(path_info, stdo, &
!    ngat,n_path_trin, path_trins, hx_gat,hy_gat,gatx,gaty,gatn,&
!    gxmin,gxmax,gymin,gymax)
!  get information about gathers
!
! i_err           integer         error code flag 0 = o.k. < 0 = error.
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
!     Date       Author         Description
!     ----       ------         -----------
! 37  2007-10-23 Douglas Hanson Initialize j_err to 0 in close.
! 36  2007-08-02 Douglas Hanson Enable migration slice files.
! 35  2007-06-14 Douglas Hanson Add fxdata_path_suffix.
! 34  2007-05-01 Douglas Hanson Fix trcio info trot order.
! 33  2007-01-16 Goodger        Copy format variable to a temporary name in
!                               routine hist_to_info_1 to make the absoft
!                               64 bit compiler happy.
! 32  2006-08-10 D. Glover      Added NULLIFY statements for Intel compiler.
! 31  2006-08-01 R.S.Day        Set nh_inp = max(64,trcio_obj%nwih) in 
!                               fxdata_info_bld to correct SEGY behavior.
! 30  2006-06-22 Douglas Hanson Change f_par to p.
! 29  2005-08-25 Ioan Vlad      Remove unused variables.
! 29  2005-07-12 Douglas Hanson Group test INFO initialize.
! 28  2005-04-21 Douglas Hanson PW hcanges.
! 27  2005-03-24 Douglas Hanson PSHOT format changes.
! 26  2005-01-31 R.S.Day        Fixed file locking logic in fxdata_trc_create.
! 25  2004-09-14 R.S.Day        Promoted to beta.
! 24  2004-08-26 Douglas Hanson Fix r4_inf - r5_inf.
! 23  2004-08-19 Douglas Hanson Add fxdata_hist_copy
! 22  2004-08-12 R.S.Day        Added fxdata_gather_count, fxdata_rd_info,
!                               fxdata_info_to_fxcard, fxdata_gather_count_c,
!                               fxdata_get_info_data_c,fxdata_get_info_type
!                               More checks for file locking.
! 21  2004-08-10 Douglas Hanson add fxdata_info_add.
! 20  2004-08-05 Faqi Liu       Fix a bug in fxdata_info_init used in 
!                               fxstack
! 19  2004-07-27 R.S.Day        Added debugging print in fxdata_trc_open.
! 18  2004-06-22 Hanson - Liu   Improve card io and fix a get global bug.
! 17  2004-06-17 Faqi Liu       
! 16  2004-04-22 R.S.Day        Save info_type settings in fxdata_info_clear.
!                               info_type increased to 12 characters.
! 15  2004-03-26 Douglas Hanson Add path_trot to info file.
! 14  2004-03-25 Douglas Hanson Fix info_type usage.
! 13  2004-03-23 Douglas Hanson g16.8 in fxdata_info_format_0 
! 12  2004-01-21 Douglas Hanson Change fxpar calls to pcpsx calls.
! 11  2003-11-06 R.S.Day        fxdata_card_to_gath_1 converted to a list
!                               directed read so old fxinfo files are read
!                               correctly
! 10  2003-11-05 R.S.Day        Altered fxdata_info_format_0 to allow trace
!                               counts over 1 million.
!  9  2003-10-23 R.S.Day        Added fxdata_get_trace_count. Added check
!                               for null p pointer.
!  8  2003-09-17 Faqi Liu       Add _gath_info_pw subroutine
!  7  2003-09-15 Faqi Liu       Add subroutines for plane wave migration 
!                               using internal IO
!  6  2003-06-10 R.S.Day        Set start time on output traces. trcio_open 
!                               call modified in in fxdata_trc_open
!  5  2002-10-21 Douglas Hanson Enhance duplicate testing.
!  4  2002-10-03 Douglas Hanson Add intent in fxdata_info_create.
!  3  2002-10-01 Douglas Hanson Add # to fxdata_info_title_0.
!  2  2002-09-26 Douglas Hanson Preserve card_to_head path_info.
!  1  2002-09-23 Douglas Hanson Initial version.
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
!                         PROGRAMMING NOTES
!
!
!-------------------------------------------------------------------------------
!</portability_doc>
!
module fxdata_module
  !
  use cardset_module
  use cio_module
  use fxcard_module
  use fxpar_module
  use headsave_module
  use mem_module
  use memfun_module
  use pathcheck_module
  use string_module
  use pc_module
  use pcpsx_module
  use timeglob_module
  use trcio_module
  !
  implicit  none
  !
  private
  !
  public :: fxdata_info_build          ! build info file for set of trace files
  public :: fxdata_info_build_1        ! build info file for a trace of inp
  public :: fxdata_info_create         ! create a fxinfo structure
  public :: fxdata_info_delete         ! delete a fxinfo structure
  public :: fxdata_info_nullify        ! nullify a fxinfo structure
  public :: fxdata_info_delete_n       ! delete  n fxinfo structures
  public :: fxdata_info_nullify_n      ! nullify n fxinfo structures
  public :: fxdata_info_clear          ! initialize parameters with    allocate
  public :: fxdata_info_clear_1        ! initialize parameters not hdr_x
  public :: fxdata_info_init           ! initialize list file
  public :: fxdata_info_add            ! add info card
  public :: fxdata_info_condition      ! condiiton info set min, max
  public :: fxdata_info_allocate       ! allocate info memory
  public :: fxdata_head_allocate       ! allocate head memory
  public :: fxdata_gath_allocate       ! allocate gath memory
  public :: fxdata_gath_reallocate     ! reallocate info memory
  public :: fxdata_info_compare        ! compare fxdatas for duplicate shots
  public :: fxdata_info_print          ! print info
  public :: fxdata_info_to_file        ! write info to   a file
  public :: fxdata_file_to_info        ! read  info from a file
  public :: fxdata_info_to_card        ! write info to   a info set
  public :: fxdata_card_to_info        ! read  info from a info set
  public :: fxdata_info_to_info        ! write head and gath to info
  public :: fxdata_hdrw_to_info        ! write head to   a info set
  public :: fxdata_gath_to_info        ! write gath to   a info set
  public :: fxdata_head_to_card        ! write head to   a card set
  public :: fxdata_card_to_head        ! read  head from a card set
  public :: fxdata_gath_to_card        ! write n gather info to   a card set
  public :: fxdata_gath_to_card_0      ! write 1 gather info to   a card set
  public :: fxdata_gath_to_card_1      ! write 1 gather info to   a card set
  public :: fxdata_card_to_gath        ! read  n gather info from a card set
  public :: fxdata_card_to_gath_0      ! read  1 gather info from a card set
  public :: fxdata_card_to_gath_1      ! read  1 gather info from a card set
  public :: fxdata_card_to_card        ! add a card to an array of cards 
  public :: fxdata_hist_to_info        ! add history card
  public :: fxdata_hist_copy           ! copy hist info
  public :: fxdata_bin_gather          ! compute gather bin
  public :: fxdata_gath_in_file        ! determine if a gather is in a file
  public :: fxdata_gath_in_info        ! determine if a gather is in a info
  public :: fxdata_gath_delete         ! delete a gather entry
  public :: fxdata_gath_copy           ! copy   a gather entry
  public :: fxdata_c4_to_path_trin     ! set path_trin array from c4_inf
  public :: fxdata_get_info_type       ! get info type
  !
  public :: fxdata_trc_create          ! create  a trcio strucutre
  public :: fxdata_trc_delete          ! delete  a trcio strucutre
  public :: fxdata_trc_nullify         ! nullify a trcio strucutre
  public :: fxdata_trc_reset           ! reset   a trcio strucutre
  public :: fxdata_trc_init            ! zero  trcio file with group
  public :: fxdata_trc_open            ! open  trcio file with group
  public :: fxdata_trc_close           ! close trcio file with group
  public :: fxdata_trc_read_b          ! read binary trcio file with group
  public :: fxdata_trc_write_b         ! write binary trcio file with group
  public :: fxdata_trc_read            ! read trcio file with group
  public :: fxdata_trc_write           ! write trcio file with group
  public :: fxdata_trc_init_b          ! initalize trot and info files
  public :: fxdata_info_build_pw
  public :: fxdata_trc_open_pw
  public :: fxdata_trc_read_pw
  public :: fxdata_trc_close_pw
  public :: fxdata_read_list_pw
  !
  ! functions
  !
  public :: fxdata_first_gather_card   ! index of first gather card
  public :: fxdata_number_of_gathers   ! number of gather cards
  public :: fxdata_gather_count        ! number of gather cards from file
  public :: fxdata_rd_info             !get data from fxinfo files
  !
  interface fxdata_info_clear
    !
    module procedure fxdata_info_clear
    module procedure fxdata_info_clear_0
    !
  end interface 
  !
  interface fxdata_hist_to_info
    !
    module procedure fxdata_hist_to_info_1
    module procedure fxdata_hist_to_info_n
    !
  end interface 
  !
  interface fxdata_file_to_info
    !
    module procedure fxdata_file_to_info_0
    module procedure fxdata_file_to_info_1
    module procedure fxdata_file_to_info_n
    !
  end interface 
  !
  interface fxdata_info_compare
    !
    module procedure fxdata_info_compare
    module procedure fxdata_info_compare_i
    module procedure fxdata_info_compare_0
    module procedure fxdata_info_compare_1
    !
  end interface 
  !
  interface fxdata_info_to_file
    !
    module procedure fxdata_info_to_file
    module procedure fxdata_info_to_file_n
    !
  end interface 
  !
  interface fxdata_gath_to_info
    !
    module procedure fxdata_gath_to_info
    module procedure fxdata_gath_to_info_1
    !
  end interface 
  !
  interface fxdata_card_to_card
    !
    module procedure fxdata_card_to_card
    module procedure fxdata_card_to_card_0
    !
  end interface 
  !
  ! rcs identifier string
  character(len=100),public,save :: fxdata_ident = &
  '$Id: fxdata.f90,v 1.37 2007/10/24 13:56:43 Hanson beta sps $'
  !
  character(len=fxcard_card_len), public, save :: fxdata_info_title_0 = &
  & '#     date      time         job_name&
  & index1 index2 index3 index4 index5    first     last   number&
  & gather_x     gather_y    tig_x_min    tig_x_max    tig_y_min    tig_y_max&
  &    path_name'
  !
  character(len=fxcard_card_len), public, save :: fxdata_info_title_1 = &
  & '      date      time         job_name&
  & index1 index2 index3 index4 index5    first     last   number&
  & gather_x     gather_y    tig_x_min    tig_x_max    tig_y_min    tig_y_max&
  &    path_name'
  !
  character(len=fxcard_card_len), public, save :: fxdata_info_format_0 = &
  & '( 1x, a10, 1x, a8, 1x, a16, &
  &1x, i6, 1x, i6, 1x, i6, 1x, i6, &
  &1x, i8, 1x, i8, 1x, i8, 1x, i8, &
  &1x, g16.8, 1x, g16.8, 1x, g16.8, 1x, g16.8, 1x, g16.8, 1x, g16.8, &
  &a )'
  !
  character(len=fxcard_card_len), public, save :: fxdata_info_history_format = &
  & '( 1x, a10, 1x, a8, 1x, a16, 1x, a )'
  !
  character(len=fxcard_card_len), public, save :: fxdata_pw_format_0 = &
  & '(" hdr_x=", i8, " hdr_y=", i8 )'
  !
  character(len=fxcard_card_len), public, save :: fxdata_pw_format_1 = &
  & '(" num_fil_tot=", i8, " num_fil_sec=", i8, " p0_scale=",g12.6 )'
  !
  character(len=fxcard_card_len), public, save :: fxdata_pw_format_2 = &
  & '(" pw_domain=", a15 )'
  !
  character(len=fxcard_card_len), public, save :: fxdata_pw_format_3 = &
  & '(" fft_length=", i8, &
  &" trace_length=", i8, " trace_increment=", f10.3 )'
  !
  character(len=fxcard_card_len), public, save :: fxdata_pw_format_4 = &
  & '(" freq_min_value=", f10.3, " freq_min_index=", i8,&
    & " freq_max_value=", f10.3, " freq_max_index=", i8 )'
  !
  character(len=fxcard_card_len), public, save :: fxdata_pw_format_5 = &
  & '(1x, i12, 1x, a)'
  !
  character(len=fxcard_card_len), public, save :: fxdata_pw_format_6 = &
  & '(" n_gat=", i6, " x_org=", f20.3, " y_org=", f20.3)'
  !
  character(len=fxcard_card_len), public, save :: fxdata_pw_format_7 = &
  & '(" i_gat=", i6, " x_gat=", f20.3, " y_gat=", f20.3)'
  !
  integer, public, save     :: fxdata_pw_j0_ray = hdr_user_50 
  integer, public, save     :: fxdata_pw_jx_ray = hdr_user_51
  integer, public, save     :: fxdata_pw_jy_ray = hdr_user_52
  integer, public, save     :: fxdata_pw_rx_ray = hdr_user_53
  integer, public, save     :: fxdata_pw_ry_ray = hdr_user_54
  integer, public, save     :: fxdata_pw_d0_ray = hdr_user_55
  !
  type, public :: fxdata_info_n
    !
    !private
    !
    type ( fxdata_info_p ),    pointer :: p(:)            ! fxinfo structure
    integer                          :: n               ! num p
    !
  end type fxdata_info_n
  !
  type, public :: fxdata_info_p
    !
    !private
    !
    type ( fxdata_info ),      pointer :: p              ! fxinfo data structure
    !
  end type fxdata_info_p
  !
  type, public :: fxdata_filestr
    !
    ! private
    !
    type (trcio_struct ), pointer :: trcio_obj
    integer                       :: num_trc
    !
  end type fxdata_filestr
  !
  type, public :: fxdata_info
    !
    !private
    !
    type ( grid_struct )                    :: grid_obj   ! transformation grid
    logical                                 :: wrapped_up ! wrapup flag.
    !
    integer                                 :: nh_inp     ! num trace header 
    integer                                 :: nt_inp     ! num trace time samp
    real                                    :: t0_inp     ! min inp time sample
    real                                    :: t1_inp     ! max inp time sample
    real                                    :: dt_inp     ! inc inp time sample
    !
    character(len=filename_length)          :: job_name   ! job_name
    !
    character(len=16)                       :: c_date     ! current date
    !
    character(len=16)                       :: c_time     ! current time
    !
    character(len=filename_length)          :: path_info  ! info file
    character(len=filename_length)          :: path_trot  ! trot file
    character(len=filename_length)          :: path_trot_0! trot file original
    !       
    integer                                 :: n_path_trin ! num trin files
    !
    character(len=filename_length), pointer :: path_trin(:)! trin file
    !
    character(len=filename_length)          :: line_title ! line title
    !
    integer                                 :: line_index ! line index number
    !
    logical                                 :: source_gather  ! source   gat
    !
    logical                                 :: receiver_gather! receiver gat
    !
    integer                                 :: hdr_x      ! x hdr
    integer                                 :: hx_gat     ! x src hdr
    integer                                 :: hx_tig     ! x rec hdr
    real                                    :: bin_x      ! x gat bin size
    !
    integer                                 :: hdr_y      ! y hdr
    integer                                 :: hy_gat     ! y src hdr
    integer                                 :: hy_tig     ! y rec hdr
    real                                    :: bin_y      ! y gat bin size
    !
    integer                                 :: m0_gat     ! number of gathers
    integer                                 :: n0_gat     ! input group number
    integer                                 :: n0_tot     ! total traces
    !
    integer                                 :: i1_inf_0   ! loc index 1 
    integer                                 :: i2_inf_0   ! loc index 2
    integer                                 :: i3_inf_0   ! loc index 3
    integer                                 :: i4_inf_0   ! loc index 4
    integer                                 :: i5_inf_0   ! loc index 5
    integer                                 :: i6_inf_0   ! loc first trace 
    integer                                 :: i7_inf_0   ! loc last  trace 
    integer                                 :: i8_inf_0   ! loc num traces grp
    !
    real                                    :: r1_inf_0   ! loc x src inp
    real                                    :: r2_inf_0   ! loc y src inp
    real                                    :: r3_inf_0   ! loc x rec inp
    real                                    :: r4_inf_0   ! loc y rec inp
    real                                    :: r5_inf_0   ! loc y rec inp
    real                                    :: r6_inf_0   ! loc y rec inp
    !
    character(len=16)                       :: c1_inf_0   ! loc date 
    character(len=16)                       :: c2_inf_0   ! loc time 
    character(len=16)                       :: c3_inf_0   ! loc job name 
    character(len=filename_length)          :: c4_inf_0   ! loc trin file 
    !
    integer                                 :: i1_inf_1   ! min index 1 
    integer                                 :: i2_inf_1   ! min index 2 
    integer                                 :: i3_inf_1   ! min index 3 
    integer                                 :: i4_inf_1   ! min index 4 
    integer                                 :: i5_inf_1   ! min index 5 
    integer                                 :: i6_inf_1   ! min first trace 
    integer                                 :: i7_inf_1   ! min last  trace 
    integer                                 :: i8_inf_1   ! min num traces grp
    !
    integer                                 :: i1_inf_2   ! max index 1 
    integer                                 :: i2_inf_2   ! max index 2 
    integer                                 :: i3_inf_2   ! max index 3 
    integer                                 :: i4_inf_2   ! max index 4 
    integer                                 :: i5_inf_2   ! max index 5 
    integer                                 :: i6_inf_2   ! max first trace 
    integer                                 :: i7_inf_2   ! max last  trace 
    integer                                 :: i8_inf_2   ! max num traces grp
    !
    real                                    :: r1_inf_1   ! r1_inf min 
    real                                    :: r2_inf_1   ! r2_inf min 
    real                                    :: r3_inf_1   ! r3_inf min 
    real                                    :: r4_inf_1   ! r4_inf min 
    real                                    :: r5_inf_1   ! r5_inf min 
    real                                    :: r6_inf_1   ! r6_inf min 
    !
    real                                    :: r1_inf_2   ! r1_inf max 
    real                                    :: r2_inf_2   ! r2_inf max 
    real                                    :: r3_inf_2   ! r3_inf max 
    real                                    :: r4_inf_2   ! r4_inf max 
    real                                    :: r5_inf_2   ! r5_inf max 
    real                                    :: r6_inf_2   ! r6_inf max 
    !       
    integer,                        pointer :: i1_inf(:)  ! index 1
    integer,                        pointer :: i2_inf(:)  ! index 2
    integer,                        pointer :: i3_inf(:)  ! index 3
    integer,                        pointer :: i4_inf(:)  ! index 4
    integer,                        pointer :: i5_inf(:)  ! index 5
    integer,                        pointer :: i6_inf(:)  ! first trace count
    integer,                        pointer :: i7_inf(:)  ! last  trace count
    integer,                        pointer :: i8_inf(:)  ! num traces in grp
    !
    real,                           pointer :: r1_inf(:)  ! x src loc
    real,                           pointer :: r2_inf(:)  ! y src loc
    real,                           pointer :: r3_inf(:)  ! x rec min
    real,                           pointer :: r4_inf(:)  ! x rec max
    real,                           pointer :: r5_inf(:)  ! y rec min
    real,                           pointer :: r6_inf(:)  ! y rec max
    !
    character(len=16),              pointer :: c1_inf(:)  ! date  
    character(len=16),              pointer :: c2_inf(:)  ! time  
    character(len=16),              pointer :: c3_inf(:)  ! job name  
    character(len=filename_length), pointer :: c4_inf(:)  ! trin file  
    !
    integer                                 :: n_history  ! num history cards
    character(len=fxcard_card_len), pointer :: c_history(:)! history cards
    !
    character(len=16)                       :: sort_select! sort type
    character(len=16)                       :: keep_select! keep type
    real                                    :: keep_min   ! min keep
    real                                    :: keep_max   ! max keep
    integer                                 :: keep_skip  ! num to skip 
    !
    character(len=12)                       :: info_type  ! shot/pshot  
    !
    type ( fxpar_struct ),          pointer :: p      ! fxpar divide
    !
  end type fxdata_info
  !
  type,  public :: fxdata_trc
    !
    logical                                 :: info_open   ! leave info open
    logical                                 :: trot_open   ! leave trot open
    integer                                 :: time_lock   ! file lock time sec
    integer                                 :: nr_max      ! max trace in file
    integer                                 :: nr_inp      ! num trace in file
    integer                                 :: nh_inp      ! num trace head
    integer                                 :: nt_inp      ! num trace time
    real                                    :: t0_inp      ! min trace time
    real                                    :: dt_inp      ! inc trace time
    !
    character(len=filename_length)          :: path_info   ! list  file name
    character(len=filename_length)          :: path_trot   ! trcio file name
    character(len=filename_length)          :: path_trot_0! trot file original
    character(len=2)                        :: file_stat   ! trcio file name
    type ( trcio_struct ),          pointer :: trcio       ! trcio structure 
    type ( fxdata_info ),           pointer :: f_inf       ! fxdata info struct
    type ( fxpar_struct ),          pointer :: p       ! fxpar divide 
    !
  end type fxdata_trc
  !
  type ( fxcard_struct ),   pointer, save :: structure    ! needed for traps.
  !
  contains
  !
  subroutine fxdata_info_build ( &
                                 p, &
                                 path_info, n_path_trin, path_trin, &
                                 line_title, line_index, &
                                 source_gather, receiver_gather, &
                                 hdr_x, hdr_y, &
                                 bin_x, bin_y, &
                                 l0_inp, i1_inp, i2_inp, &
                                 i_err &
                               )
    !
    ! build the info file for a trace file
    !
    type ( fxpar_struct ),   pointer :: p             ! fxpar structure
    character(len=*),  intent(in   ) :: path_info
    integer,           intent(in   ) :: n_path_trin
    character(len=*),  intent(in   ) :: path_trin(:)
    character(len=*),  intent(in   ) :: line_title ! line title
    integer,           intent(in   ) :: line_index ! line index number
    logical,           intent(in   ) :: source_gather  ! source   gather flag
    logical,           intent(in   ) :: receiver_gather! receiver gather flag
    integer,           intent(in   ) :: hdr_x      ! x hdr
    real,              intent(in   ) :: bin_x      ! x bin size
    integer,           intent(in   ) :: hdr_y      ! y hdr
    real,              intent(in   ) :: bin_y      ! y bin size
    !
    logical,           intent(in   ) :: l0_inp(:)        ! info every trace
    integer,           intent(in   ) :: i1_inp(:)        ! inp min
    integer,           intent(in   ) :: i2_inp(:)        ! inp max
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !
    integer                          :: i_trin_1         ! inp min
    integer                          :: i_trin_2         ! inp max
    integer                          :: nh_inp           ! header dimension
    integer                          :: nt_inp           ! trace dimension
    integer                          :: nh_glb           ! header dimension
    integer                          :: nt_glb           ! trace dimension
    !
    integer                          :: i_path_trin      ! input file index
    integer                          :: i_trin           ! input trace index
    integer                          :: n_trin           ! input trace number
    type ( fxdata_info ),    pointer :: f_inf
    type ( trcio_struct),    pointer :: trcio_obj        ! trcio structure
    double precision,        pointer :: hd_sav (:,:) ! h save buffer (h ,12)
    double precision,        pointer :: hd_inp (:,:) ! headers
    real,                    pointer :: tr_inp (:,:) ! traces
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    nullify ( f_inf     )
    !
    nullify ( trcio_obj )
    nullify (hd_sav) ! jpa
    nullify (hd_inp) ! jpa
    nullify (tr_inp) ! jpa
    !
    call memfun_nul ( hd_sav )
    !
    call memfun_nul ( hd_inp )
    !
    call memfun_nul ( tr_inp )
    !
    ! create the info structure
    !
    call fxdata_info_create ( &
                              f_inf, p, &
                              path_info, n_path_trin, path_trin, &
                              line_title, line_index, &
                              source_gather, receiver_gather, &
                              hdr_x, hdr_y, &
                              bin_x, bin_y, &
                              'SHOT', i_err &
                            )
    
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! only build the data base on the root processor
    !
    xxif_root_1 : if ( p%group_root ) then
      !
      do_path_trin : do i_path_trin = 1 , n_path_trin
        !
        ! open the data file
        !                    i        i     i(opt) i(opt) i(opt) i(opt) i(opt)
        ! file => trcio_open(filename,mode,scratch, nwih, ndpt,  nbits, nbitshd)
        !
        trcio_obj => trcio_open ( &
                                  filename = path_trin(i_path_trin), &
                                  io_mode  = 'r', &
                                  scratch  = .false. &
                                )
        !

        if ( .not. associated ( trcio_obj ) ) i_err = -1
        !
        if ( i_err .ne. 0 ) go to 996
        !
        nh_inp = trcio_obj%nwih
        nh_inp = max(64,trcio_obj%nwih)
        !
        nt_inp = trcio_obj%num_values
        !
        xxif_first_trin : if ( i_path_trin .eq. 1 ) then
          !
          nh_glb = max ( nh_inp, 64 )
          !
          nt_glb = nt_inp
          !
          call memfun_all ( hd_inp, nh_inp, 1, 'hd_inp', i_err )
          !
          if ( i_err .ne. 0 ) go to 995
          !
          call memfun_all ( tr_inp, nt_inp, 1, 'tr_inp', i_err )
          !
          if ( i_err .ne. 0 ) go to 995
          !
          call memfun_all ( hd_sav, nh_inp, 12, 'hd_sav', i_err )
          !
          if ( i_err .ne. 0 ) go to 995
          !
          hd_sav = 0.
          !
        else xxif_first_trin 
          !
          if ( nh_glb .ne. nh_inp .or. nt_glb .ne. nt_inp ) go to 994
          !
        end if xxif_first_trin 
        !
        ! input each trace in the trin file
        !
        n_trin = fxdata_get_trace_count ( trcio_obj )
        !
        !print'(" fxinfo i=",i8," n=",i8," f=",a)',&
        !i_path_trin, n_trin, trim ( path_trin(i_path_trin))
        !
        i_trin_1 = max ( i1_inp ( i_path_trin ) , 1      )
        !
        i_trin_2 = min ( i2_inp ( i_path_trin ) , n_trin ) 
        !
        if ( i2_inp ( i_path_trin ) .le. 0 ) &
        i_trin_2 = n_trin

        !
        do_i_trin : do i_trin = i_trin_1, i_trin_2
          !
          i_err = trcio_read_trace ( file = trcio_obj,      &
                                     hd   = hd_inp ( :,1 ), &
                                     tr   = tr_inp ( :,1 ), &
                                     tnum = i_trin          &
                                   )
          !
          if ( i_err .ne. 0 ) go to 993
          !
          ! pass this trace to fxdata_info_build_1
          !
          !call fxdata_info_build_2 ( &
          call fxdata_info_build_1 ( &
                                     f_inf, &
                                     l0_inp, i_trin, &
                                     i_trin_1, i_trin_2, &
                                     i_path_trin, n_path_trin, &
                                     path_trin, path_info, &
                                     nh_glb, hd_sav, &
                                     hd_inp, tr_inp, &
                                     i_err &
                                   )
          !
          if ( i_err .ne. 0 ) go to 1999
          !
        end do do_i_trin 
        !
        !  close the trace file
        !                        i     i(opt)
        !  status = trcio_close(file,remove)
        !         Purpose: Close the trace file. e-allocate file structure.
        !         if remove == true, the file is deleted upo
        !
        i_err = trcio_close ( file=trcio_obj, remove=.false. )
        !
        if ( i_err .lt. 0 ) go to 991
        !
      end do do_path_trin 
      !
    end if xxif_root_1
    !
1999 continue
    !
    call memfun_del ( hd_sav )
    !
    call memfun_del ( hd_inp )
    !
    call memfun_del ( tr_inp )
    !
    call fxpar_check_worker_errors ( p, i_err )
    !
    if (         associated ( f_inf ) ) &
    call fxdata_info_delete ( f_inf )
    !
    return
    !
991 continue
    !
    call pc_info (' error in fxdata_info_build during trcio_close ' )
    !
    go to 999
    !
992 continue
    !
    call pc_info (' error in fxdata_info_build during fxdata_info_build_1 ' )
    !
    go to 999
    !
993 continue
    !
    call pc_info (' error in fxdata_info_build during trcio_read_trace ' )
    !
    go to 999
    !
994 continue
    !
call pc_info ( ' error in fxdata_info_build traces should be the same length' )
    call pc_info ( ' file index=', i_path_trin )
    call pc_info ( ' first header length=', nh_glb, ' current=', nh_inp )
    call pc_info ( ' first trace  length=', nt_glb, ' current=', nt_inp )
    !
    go to 999
    !
995 continue
    !
    call pc_info ( ' error in fxdata_info_build during memory allocation ' )
    !
    go to 999
    !
996 continue 
    !
    call pc_info ( ' error in fxdata_info_build during trcio_open ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( ' error in fxdata_info_build during fxdata_info_create ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_info_build ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_info_build
  !
  subroutine fxdata_info_build_1 ( &
                                   f_inf, &
                                   l0_inp, i_trin, &
                                   i_trin_1, i_trin_2, &
                                   i_path_trin, n_path_trin, &
                                   path_trin, path_info, &
                                   nh_inp, hd_sav, &
                                   hd_inp, tr_inp, &
                                   i_err &
                                 )
    !
    ! input the next input trace
    !
    !
    type ( fxdata_info ),    pointer :: f_inf
    logical,           intent(in   ) :: l0_inp(:)        ! info every trace
    integer,           intent(in   ) :: i_trin           ! trace index in file
    integer,           intent(in   ) :: i_trin_1         ! first input trace
    integer,           intent(in   ) :: i_trin_2         ! last  input trace
    integer,           intent(in   ) :: i_path_trin      ! file index
    integer,           intent(in   ) :: n_path_trin
    character(len=*),  intent(in   ) :: path_trin(:)
    character(len=*),  intent(in   ) :: path_info
    integer,           intent(in   ) :: nh_inp
    double precision,  intent(inout) :: hd_sav (:,:) ! h save buffer (h ,12)
    double precision,  intent(in   ) :: hd_inp (:, :)    ! headers
    real,              intent(in   ) :: tr_inp (:, :)    ! traces
    !
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !
    integer                          :: i_gather         ! input gather index
    logical                          :: first_trace      ! first trace  flag
    logical                          :: last_trace       ! last  trace  flag
    logical                          :: new_trin         ! new   gather flag
    logical                          :: new_gather       ! new   gather flag
    !
    character(len=fxcard_card_len)   :: c_card           ! card image
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! save headers for input traces (locations 1 - 4)
    !
    i_gather = hd_inp ( hdr_current_group, 1 )  ! curent gather index
    !
    ! set the first_trace and last_trace and new_gather flags 
    !
    first_trace = i_trin .le. i_trin_1 .and. i_path_trin .eq. 1 
    !
    last_trace  = i_trin .ge. i_trin_2 .and. i_path_trin .ge. n_path_trin 
    !
    new_trin = i_trin .le. i_trin_1 
    !
    new_gather = first_trace &
            .or. i_gather .ne. f_inf%i3_inf_0 &
            .or. l0_inp(i_path_trin) 
    !
    !print'(" a1 i=",i4,1x,i8," f=",l2,1x,l2,1x,l2)',&
    !i_path_trin,i_trin,first_trin,first_gather,new_gather
    !
    ! save input header words
    !
    call headsave_store ( f_inf%n0_tot, 9, &
                          f_inf%nh_inp, hd_inp(:,1), hd_sav )
    !
    ! set the input trace x and y trace in gather positions
    !
    call fxdata_info_add ( &
                           f_inf, &
                           i_gather, i_path_trin, path_trin(i_path_trin), &
                           first_trace, last_trace, &
                           new_trin, new_gather, &
                           hd_inp(:,1) &
                         )
    !
    !
    ! if this is the lat input trace wrap up
    !
    xxif_last_trace : if ( last_trace ) then
      !
      write ( c_card,'( &
      & " fxdata_info_build_1 fxinfo number_of_traces=",i8, & 
      & " number_of_gathers=",i8 &
      & )') &
      f_inf%n0_tot, f_inf%n0_gat
      !
     !print'(" fxdata_info_build_1 bef fxdata_hist_to_info n=",i8," c=",a )', &
     !f_inf%n_history, trim(c_card)
      !
      call fxdata_hist_to_info ( f_inf, c_card, i_err )
      !
      if ( i_err .ne. 0 ) go to 997
      !
      !print'(" aft fxdata_hist_to_info n=",i8)', f_inf%n_history
      !
      ! close the info file the will replace the header with the current values
      !
      call fxdata_info_to_file ( path_info, f_inf, i_err )
      !
      if ( i_err .ne. 0 ) go to 996
      !
      ! print the input header word info
      !
      call headsave_print ( 'fxinfo', pc_get_lun(), 9, nh_inp, hd_sav )
      !
    end if xxif_last_trace 
    !
    return
    !
996 continue
    !
    call pc_error ( &
    ' error in fxdata_info_build_1 during fxdata_info_to_file ' )
    !
    go to 999
    !
997 continue
    !
    call pc_error ( &
    ' error in fxdata_info_build_1 during fxdata_hist_to_info ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( &
    ' error in fxdata_info_build_1 during fxdata_line_to_card ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_info_build_1 ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_info_build_1
  !
  subroutine fxdata_info_add ( &
                               f_inf, &
                               i_gather, i_path_trin, path_trin, &
                               first_trace, last_trace, &
                               new_trin, new_gather, &
                               hd_inp &
                             )
    !
    ! input the next input trace
    !
    !
    type ( fxdata_info ),    pointer :: f_inf
    integer,           intent(in   ) :: i_gather         ! gather index
    integer,           intent(in   ) :: i_path_trin      ! file index   
    character(len=*),  intent(in   ) :: path_trin        ! file name    
    logical,           intent(in   ) :: first_trace      ! first trace  flag
    logical,           intent(in   ) :: last_trace       ! last  trace  flag
    logical,           intent(in   ) :: new_trin         ! new   trin   flag
    logical,           intent(in   ) :: new_gather       ! new   gather flag
    double precision,  intent(in   ) :: hd_inp (:)       ! headers
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! local variables
    !
    !if ( i_call .le. 2000 ) &
    !print'(" fxdata_info_add c=",i8," n0_tot=",i8," n0_gat=",i8, &
    !& " f=",l2," l=",l2," t=",l2," g=",l2)', &
    !i_call, f_inf%n0_tot, f_inf%n0_gat, &
    !first_trace, last_trace, new_trin, new_gather 
    !
    !if ( first_trace ) &
    !print'(" fxdata_info_add first_trace n0_tot=",i8," n0_gat=",i8)', &
    !f_inf%n0_tot, f_inf%n0_gat
    !
    f_inf%r3_inf_0 = hd_inp ( f_inf%hx_tig ) ! tig x loc
    !
    f_inf%r4_inf_0 = hd_inp ( f_inf%hx_tig ) ! tig x loc
    !
    f_inf%r5_inf_0 = hd_inp ( f_inf%hy_tig ) ! tig y loc
    !
    f_inf%r6_inf_0 = hd_inp ( f_inf%hy_tig ) ! tig y loc
    !
    ! if this is a new gather 
    ! and not the first trace and not the last trace
    ! write the previous gather info
    !
    !if ( new_gather .and. .not. first_trace .and. .not. last_trace ) &
    !print'(" a fxdata_info_add new_gather n0_tot=",i8," n0_gat=",i8)', &
    !f_inf%n0_tot, f_inf%n0_gat
    !
    !if ( new_gather .and. .not. first_trace .and. .not. last_trace ) &
    !print'(" a0 c=",i4," i=",i8," x=",g12.6," y=",g12.6)', &
    !i_call, f_inf%i3_inf_0, f_inf%r1_inf_0, f_inf%r2_inf_0 
    !
    if ( new_gather .and. .not. first_trace ) &
    call fxdata_gath_to_info ( &
                               f_inf, &
              f_inf%i1_inf_0, f_inf%i2_inf_0, f_inf%i3_inf_0, f_inf%i4_inf_0, &
              f_inf%i5_inf_0, f_inf%i6_inf_0, f_inf%i7_inf_0, f_inf%i8_inf_0, &
              f_inf%r1_inf_0, f_inf%r2_inf_0, f_inf%r3_inf_1, f_inf%r4_inf_2, &
              f_inf%r5_inf_1, f_inf%r6_inf_2, &
              f_inf%c1_inf_0, f_inf%c2_inf_0, f_inf%c3_inf_0, f_inf%c4_inf_0 &
                           )
    !
    !if ( new_gather .and. .not. first_trace .and. .not. last_trace ) &
    !print'(" b fxdata_info_add new_gather n0_tot=",i8," n0_gat=",i8)', &
    !f_inf%n0_tot, f_inf%n0_gat
    !
    ! if this is a new gather reset info
    !
    xxif_new_gather : if ( new_gather ) then
      !
      call string_date  (            f_inf%c1_inf_0 ) ! current date
      !
      call string_time  (            f_inf%c2_inf_0 ) ! current time
      !
      call pc_get_jdata ( 'jobname', f_inf%c3_inf_0 ) ! job name
      !
      f_inf%c4_inf_0 = path_trin
      !
      f_inf%i1_inf_0 = i_path_trin                ! input file  count
      !
      f_inf%i2_inf_0 = f_inf%i2_inf_0 + 1         ! input gather count
      !
      f_inf%i3_inf_0 = i_gather                   ! input gather index
      !
      f_inf%i4_inf_0 = f_inf%i4_inf_0 + 1         ! input gather count
      !
      if ( new_trin ) &
      f_inf%i4_inf_0 = 1
      !
      f_inf%i5_inf_0 = f_inf%n0_tot + 1           ! first total trace
      !
      if ( new_trin ) &
      f_inf%i7_inf_0 = 0
      !
      f_inf%i6_inf_0 = f_inf%i7_inf_0 + 1         ! first trace in gather 
      !
      ! set the input trace x and y source positions
      !
      f_inf%r1_inf_0 = hd_inp ( f_inf%hx_gat ) ! src x loc
      f_inf%r2_inf_0 = hd_inp ( f_inf%hy_gat ) ! src y loc
      !
      f_inf%r3_inf_1 = f_inf%r3_inf_0 
      f_inf%r3_inf_2 = f_inf%r3_inf_0
      !
      f_inf%r4_inf_1 = f_inf%r4_inf_0 
      f_inf%r4_inf_2 = f_inf%r4_inf_0 
      !
      f_inf%r5_inf_1 = f_inf%r5_inf_0 
      f_inf%r5_inf_2 = f_inf%r5_inf_0 
      !
      f_inf%r6_inf_1 = f_inf%r6_inf_0 
      f_inf%r6_inf_2 = f_inf%r6_inf_0 
      !
      !if ( new_gather .and. .not. first_trace .and. .not. last_trace ) &
      !print'(" a1 c=",i4," i=",i8," x=",g12.6," y=",g12.6)', &
      !i_call, f_inf%i3_inf_0, f_inf%r1_inf_0, f_inf%r2_inf_0 
      !
    end if xxif_new_gather 
    !
    f_inf%n0_tot = f_inf%n0_tot + 1     ! total input trace count
    !
    f_inf%i7_inf_0 = f_inf%i7_inf_0 + 1 ! last  trace in gather count
    !
    f_inf%i8_inf_0 = f_inf%i7_inf_0 - f_inf%i6_inf_0 + 1 ! traces in gather
    !
    f_inf%r3_inf_1 = min ( f_inf%r3_inf_1 , f_inf%r3_inf_0 ) ! min x tig
    !
    f_inf%r3_inf_2 = max ( f_inf%r3_inf_2 , f_inf%r3_inf_0 ) ! max x tig
    !
    f_inf%r4_inf_1 = min ( f_inf%r4_inf_1 , f_inf%r4_inf_0 ) ! min x tig
    !
    f_inf%r4_inf_2 = max ( f_inf%r4_inf_2 , f_inf%r4_inf_0 ) ! max x tig
    !
    f_inf%r5_inf_1 = min ( f_inf%r5_inf_1 , f_inf%r5_inf_0 ) ! min y tig
    !
    f_inf%r5_inf_2 = max ( f_inf%r5_inf_2 , f_inf%r5_inf_0 ) ! max y tig
    !
    f_inf%r6_inf_1 = min ( f_inf%r6_inf_1 , f_inf%r6_inf_0 ) ! min y tig
    !
    f_inf%r6_inf_2 = max ( f_inf%r6_inf_2 , f_inf%r6_inf_0 ) ! max y tig
    !
    !print'(" r=",i4," ig=",i4," ng=",i4," n1=",i4," n2=",i4)',&
    !f_inf%n0_tot, f_inf%i3_inf_0, &
    !f_inf%n0_gat, f_inf%i6_inf_0, f_inf%i7_inf_0
    !
    ! if this is the last input trace write the last gather info
    !
    !if ( last_trace ) &
    !print'(" a2 c=",i4," i=",i8," x=",g12.6," y=",g12.6)', &
    !i_call, f_inf%i3_inf_0, f_inf%r1_inf_0, f_inf%r2_inf_0 
    !
    if ( last_trace ) &
    call fxdata_gath_to_info ( &
                               f_inf, &
              f_inf%i1_inf_0, f_inf%i2_inf_0, f_inf%i3_inf_0, f_inf%i4_inf_0, &
              f_inf%i5_inf_0, f_inf%i6_inf_0, f_inf%i7_inf_0, f_inf%i8_inf_0, &
              f_inf%r1_inf_0, f_inf%r2_inf_0, f_inf%r3_inf_1, f_inf%r4_inf_2, &
              f_inf%r5_inf_1, f_inf%r6_inf_2, &
              f_inf%c1_inf_0, f_inf%c2_inf_0, f_inf%c3_inf_0, f_inf%c4_inf_0 &
                           )
    !
    !if ( last_trace ) &
    !print'(" fxdata_info_add last_trace n0_tot=",i8," n0_gat=",i8)', &
    !f_inf%n0_tot, f_inf%n0_gat
    !
    return
    !
  end subroutine fxdata_info_add 
  !
  subroutine fxdata_info_build_2 ( &
                                   f_inf, &
                                   l0_inp, i_trin, &
                                   i_trin_1, i_trin_2, &
                                   i_path_trin, n_path_trin, &
                                   path_trin, path_info, &
                                   nh_inp, hd_sav, &
                                   hd_inp, tr_inp, &
                                   i_err &
                                 )
    !
    ! input the next input trace
    !
    !
    type ( fxdata_info ),    pointer :: f_inf
    logical,           intent(in   ) :: l0_inp(:)        ! info every trace
    integer,           intent(in   ) :: i_trin           ! trace index in file
    integer,           intent(in   ) :: i_trin_1         ! first input trace
    integer,           intent(in   ) :: i_trin_2         ! last  input trace
    integer,           intent(in   ) :: i_path_trin      ! file index
    integer,           intent(in   ) :: n_path_trin
    character(len=*),  intent(in   ) :: path_trin(:)
    character(len=*),  intent(in   ) :: path_info
    integer,           intent(in   ) :: nh_inp
    double precision,  intent(inout) :: hd_sav (:,:) ! h save buffer (h ,12)
    double precision,  intent(in   ) :: hd_inp (:, :)    ! headers
    real,              intent(in   ) :: tr_inp (:, :)    ! traces
    !
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !
    integer                          :: i_gather         ! input gather index
    logical                          :: new_gather       ! new   gather flag
    logical                          :: first_gather     ! first gather flag
    logical                          :: first_trin       ! first gather flag
    logical                          :: last_trace       ! last  trace  flag
    !

    !
    ! initialize the error flag
    !
    i_err = 0
    !
    !  save headers for input traces (locations 1 - 4)
    !
    i_gather = hd_inp ( hdr_current_group, 1 )  ! curent gather index
    !
    ! set the new_gather and first_gather flags
    !
    first_trin   = i_path_trin .eq. 1 
    !
    first_gather = .false.
    !
    if ( i_trin .eq. i_trin_1 ) &
    first_gather = .true.
    !
    new_gather = .false.
    !
    if ( first_gather .or. i_gather .ne. f_inf%i3_inf_0 &
    .or. l0_inp(i_path_trin) ) &
    new_gather = .true.
    !
    last_trace = .false.
    !
    if ( i_path_trin .ge. n_path_trin .and. i_trin .ge. i_trin_2 ) &
    last_trace = .true.
    !
    !print'(" a1 i=",i4,1x,i8," f=",l2,1x,l2,1x,l2)',&
    !i_path_trin,i_trin,first_trin,first_gather,new_gather
    !
    ! save input header words
    !
    call headsave_store ( f_inf%n0_tot, 9, &
                          f_inf%nh_inp, hd_inp (:, 1), hd_sav )
    !
    ! set the input trace x and y trace in gather positions
    !
    f_inf%r3_inf_0 = hd_inp ( f_inf%hx_tig, 1 ) ! tig x loc
    !
    f_inf%r4_inf_0 = hd_inp ( f_inf%hx_tig, 1 ) ! tig x loc
    !
    f_inf%r5_inf_0 = hd_inp ( f_inf%hy_tig, 1 ) ! tig y loc
    !
    f_inf%r6_inf_0 = hd_inp ( f_inf%hy_tig, 1 ) ! tig y loc
    !
    xxif_new_gather : if ( new_gather ) then
      !
      ! if this is not the first gather write the card
      !
!if ( .not. ( first_gather .and. first_trin ) ) &
!print'(" a2 i=",i4,1x,i4," f=",l2,1x,l2,1x,l2," c4=",a)',&
!i_path_trin,i_trin,first_trin,first_gather,new_gather,trim(f_inf%c4_inf_0 )
      !
      if ( .not. ( first_gather .and. first_trin ) ) &
      call fxdata_gath_to_info ( &
                                 f_inf, &
              f_inf%i1_inf_0, f_inf%i2_inf_0, f_inf%i3_inf_0, f_inf%i4_inf_0, &
              f_inf%i5_inf_0, f_inf%i6_inf_0, f_inf%i7_inf_0, f_inf%i8_inf_0, &
              f_inf%r1_inf_0, f_inf%r2_inf_0, f_inf%r3_inf_1, f_inf%r4_inf_2, &
              f_inf%r5_inf_1, f_inf%r6_inf_2, &
              f_inf%c1_inf_0, f_inf%c2_inf_0, f_inf%c3_inf_0, f_inf%c4_inf_0 &
                           )
      !
      !if ( .not. ( first_gather .and. first_trin ) ) &
      !print'(" a3 i=",i4,1x,i4," f=",l2,1x,l2,1x,l2)',&
      !i_path_trin,i_trin,first_trin,first_gather,new_gather
      !
      call string_date  (            f_inf%c1_inf_0 ) ! current date
      !
      call string_time  (            f_inf%c2_inf_0 ) ! current time
      !
      call pc_get_jdata ( 'jobname', f_inf%c3_inf_0 ) ! job name
      !
      f_inf%c4_inf_0 = path_trin(i_path_trin)
      !
      f_inf%i1_inf_0 = i_path_trin                ! input file  count
      !
      f_inf%i2_inf_0 = f_inf%i2_inf_0 + 1         ! input gather count
      !
      f_inf%i3_inf_0 = i_gather                   ! input gather index
      !
      f_inf%i4_inf_0 = f_inf%i4_inf_0 + 1         ! input gather count
      !
      if ( first_gather ) &
      f_inf%i4_inf_0 = 1
      !
      f_inf%i5_inf_0 = f_inf%n0_tot + 1           ! first total trace
      !
      if ( first_gather ) &
      f_inf%i7_inf_0 = 0
      !
      f_inf%i6_inf_0 = f_inf%i7_inf_0 + 1         ! first trace in gather 
      !
      ! set the input trace x and y source positions
      !
      f_inf%r1_inf_0 = hd_inp ( f_inf%hx_gat, 1 ) ! src x loc
      f_inf%r2_inf_0 = hd_inp ( f_inf%hy_gat, 1 ) ! src y loc
      !
      f_inf%r3_inf_1 = f_inf%r3_inf_0 
      f_inf%r3_inf_2 = f_inf%r3_inf_0
      !
      f_inf%r4_inf_1 = f_inf%r4_inf_0 
      f_inf%r4_inf_2 = f_inf%r4_inf_0 
      !
      f_inf%r5_inf_1 = f_inf%r5_inf_0 
      f_inf%r5_inf_2 = f_inf%r5_inf_0 
      !
      f_inf%r6_inf_1 = f_inf%r6_inf_0 
      f_inf%r6_inf_2 = f_inf%r6_inf_0 
      !
    end if xxif_new_gather 
    !
    f_inf%n0_tot = f_inf%n0_tot + 1     ! total input trace count
    !
    f_inf%i7_inf_0 = f_inf%i7_inf_0 + 1 ! last  trace in gather count
    !
    f_inf%i8_inf_0 = f_inf%i7_inf_0 - f_inf%i6_inf_0 + 1 ! traces in gather
    !
    f_inf%r3_inf_1 = min ( f_inf%r3_inf_1 , f_inf%r3_inf_0 ) ! min x tig
    !
    f_inf%r3_inf_2 = max ( f_inf%r3_inf_2 , f_inf%r3_inf_0 ) ! max x tig
    !
    f_inf%r4_inf_1 = min ( f_inf%r4_inf_1 , f_inf%r4_inf_0 ) ! min x tig
    !
    f_inf%r4_inf_2 = max ( f_inf%r4_inf_2 , f_inf%r4_inf_0 ) ! max x tig
    !
    f_inf%r5_inf_1 = min ( f_inf%r5_inf_1 , f_inf%r5_inf_0 ) ! min y tig
    !
    f_inf%r5_inf_2 = max ( f_inf%r5_inf_2 , f_inf%r5_inf_0 ) ! max y tig
    !
    f_inf%r6_inf_1 = min ( f_inf%r6_inf_1 , f_inf%r6_inf_0 ) ! min y tig
    !
    f_inf%r6_inf_2 = max ( f_inf%r6_inf_2 , f_inf%r6_inf_0 ) ! max y tig
    !
    !print'(" r=",i4," ig=",i4," ng=",i4," n1=",i4," n2=",i4)',&
    !f_inf%n0_tot, f_inf%i3_inf_0, &
    !f_inf%n0_gat, f_inf%i6_inf_0, f_inf%i7_inf_0
    !
    ! if this is the lat input trace wrap up
    !
    xxif_last_trace : if ( last_trace ) then
      !
      ! put the last card
      !
      !print'(" fxdata_info_build_2 ", &
      !& " bef fxdata_gath_to_info fxinfo number_of_traces=",i8, &
      !&" number_of_gathers=",i8)', f_inf%n0_tot, f_inf%n0_gat
      !print'(" a3 i=",i4,1x,i4," f=",l2,1x,l2,1x,l2," c4=",a)', &
      !i_path_trin,i_trin,first_trin,first_gather,new_gather, &
      !trim(f_inf%c4_inf_0 )
      !
      call fxdata_gath_to_info ( &
                                 f_inf, &
              f_inf%i1_inf_0, f_inf%i2_inf_0, f_inf%i3_inf_0, f_inf%i4_inf_0, &
              f_inf%i5_inf_0, f_inf%i6_inf_0, f_inf%i7_inf_0, f_inf%i8_inf_0, &
              f_inf%r1_inf_0, f_inf%r2_inf_0, f_inf%r3_inf_1, f_inf%r4_inf_2, &
              f_inf%r5_inf_1, f_inf%r6_inf_2, &
              f_inf%c1_inf_0, f_inf%c2_inf_0, f_inf%c3_inf_0, f_inf%c4_inf_0 &
                           )
      !
      !print'(" fxdata_info_build_2 ", &
      !& " aft fxdata_gath_to_info fxinfo number_of_traces=",i8,&
      !&" number_of_gathers=",i8)', f_inf%n0_tot, f_inf%n0_gat
      !
      !write ( c_card,'( &
      !& " fxdata_info_build_2 fxinfo number_of_traces=",i8, & 
      !& " number_of_gathers=",i8 &
      !& )') &
      !f_inf%n0_tot, f_inf%n0_gat
      !
      !print'(" fxdata_info_build_2 bef fxdata_hist_to_info n=",i8," c=",a )', &
      !f_inf%n_history, trim(c_card)
      !
      !call fxdata_hist_to_info ( f_inf, c_card, i_err )
      !
      if ( i_err .ne. 0 ) go to 997
      !
      !print'(" aft fxdata_hist_to_info n=",i8)', f_inf%n_history
      !
      ! close the info file the will replace the header with the current values
      !
      call fxdata_info_to_file ( path_info, f_inf, i_err )
      !
      if ( i_err .ne. 0 ) go to 996
      !
      ! print the input header word info
      !
      call headsave_print ( 'fxinfo', pc_get_lun(), 9, nh_inp, hd_sav )
      !
    end if xxif_last_trace 
    !
    return
    !
996 continue
    !
    call pc_error ( &
    ' error in fxdata_info_build_2 during fxdata_info_to_file ' )
    !
    go to 999
    !
997 continue
    !
    call pc_error ( &
    ' error in fxdata_info_build_2 during fxdata_hist_to_info ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( &
    ' error in fxdata_info_build_2 during fxdata_line_to_card ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_info_build_2 ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_info_build_2
  !
  subroutine fxdata_info_create ( &
                                  f_inf, p, &
                                  path_info, n_path_trin, path_trin, &
                                  line_title, line_index, &
                                  source_gather, receiver_gather, &
                                  hdr_x, hdr_y, &
                                  bin_x, bin_y, &
                                  info_type, &
                                  i_err &
                                )
    !
    type ( fxdata_info ),    pointer :: f_inf          ! input structure
    type ( fxpar_struct ),   pointer :: p          ! fxpar divide 
    character(len=*),  intent(in   ) :: path_info      ! info file
    integer,           intent(in   ) :: n_path_trin    ! num trin files
    character(len=*),  intent(in   ) :: path_trin(:)   ! trin file
    character(len=*),  intent(in   ) :: line_title     ! line title
    integer,           intent(in   ) :: line_index     ! line index number
    logical,           intent(in   ) :: source_gather  ! source   gather flag
    logical,           intent(in   ) :: receiver_gather! receiver gather flag
    integer,           intent(in   ) :: hdr_x          ! x hdr
    integer,           intent(in   ) :: hdr_y          ! y hdr
    real,              intent(in   ) :: bin_x          ! x bin size
    real,              intent(in   ) :: bin_y          ! y bin size
    character(len=*),  intent(in   ) :: info_type      ! shot/pshot
    integer,           intent(inout) :: i_err          ! err 0=o.k. -1=error
    !

    !
    i_err = 0
    !
    !print'(" top fxdata_info_create path_info=",a)', trim(path_info)
    !
    xxif_not_associated : if ( .not. associated ( f_inf ) ) then
      !
      allocate ( f_inf, stat=i_err )
      !
      if ( i_err .ne. 0 ) go to 997
      !
      nullify (f_inf%path_trin) ! jpa
      nullify (f_inf%i1_inf) ! jpa
      nullify (f_inf%i2_inf) ! jpa
      nullify (f_inf%i3_inf) ! jpa
      nullify (f_inf%i4_inf) ! jpa
      nullify (f_inf%i5_inf) ! jpa
      nullify (f_inf%i6_inf) ! jpa
      nullify (f_inf%i7_inf) ! jpa
      nullify (f_inf%i8_inf) ! jpa
      nullify (f_inf%r1_inf) ! jpa
      nullify (f_inf%r2_inf) ! jpa
      nullify (f_inf%r3_inf) ! jpa
      nullify (f_inf%r4_inf) ! jpa
      nullify (f_inf%r5_inf) ! jpa
      nullify (f_inf%r6_inf) ! jpa
      nullify (f_inf%c1_inf) ! jpa
      nullify (f_inf%c2_inf) ! jpa
      nullify (f_inf%c3_inf) ! jpa
      nullify (f_inf%c4_inf) ! jpa
      nullify (f_inf%c_history) ! jpa
      nullify (f_inf%p) ! jpa
      !
      call fxdata_info_nullify ( f_inf )
      !
    end if xxif_not_associated 
    !
    call fxdata_info_clear ( f_inf, p, info_type, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    f_inf%n_path_trin     = n_path_trin
    !
    call memfun_all ( f_inf%path_trin, f_inf%n_path_trin, 'path_trin', i_err )
    !
    f_inf%path_trin(1:n_path_trin) = path_trin(1:n_path_trin)
    !
    f_inf%path_info       = path_info 
    !
    call pc_get_jdata ( 'jobname', f_inf%job_name )
    !
    f_inf%source_gather   = source_gather   
    !
    f_inf%receiver_gather = receiver_gather 
    !
    f_inf%hdr_x           = hdr_x      
    !
    f_inf%hdr_y           = hdr_y      
    !
    f_inf%bin_x           = bin_x
    !
    f_inf%bin_y           = bin_y
    !
    f_inf%line_title      = line_title 
    !
    f_inf%line_index      = line_index 
    !
    f_inf%info_type       = info_type
    !
    call fxcard_h_gat_tig ( &
    f_inf%source_gather, f_inf%hdr_x, f_inf%hx_gat, f_inf%hx_tig )
    !
    call fxcard_h_gat_tig ( &
    f_inf%source_gather, f_inf%hdr_y, f_inf%hy_gat, f_inf%hy_tig )
    !
    return
    !
997 continue
    !
    call pc_info('fxdata_info_create: allocate f_inf error')
    !
    go to 999
    !
998 continue
    !
    call pc_info ( &
    ' error in fxdata_info_create during fxdata_info_clear ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxdata_info_create ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_info_create
  !
  subroutine fxdata_info_delete ( f_inf )
    !
    type ( fxdata_info ),  pointer :: f_inf              ! input structure
    !
    ! Local variables
    !
    if ( .not. associated ( f_inf ) ) return
    !
    call memfun_del ( f_inf%i1_inf )
    call memfun_del ( f_inf%i2_inf )
    call memfun_del ( f_inf%i3_inf )
    call memfun_del ( f_inf%i4_inf )
    call memfun_del ( f_inf%i5_inf )
    call memfun_del ( f_inf%i6_inf )
    call memfun_del ( f_inf%i7_inf )
    call memfun_del ( f_inf%i8_inf )
    call memfun_del ( f_inf%r1_inf )
    call memfun_del ( f_inf%r2_inf )
    call memfun_del ( f_inf%r3_inf )
    call memfun_del ( f_inf%r5_inf )
    call memfun_del ( f_inf%r4_inf )
    call memfun_del ( f_inf%r6_inf )
    call memfun_del ( f_inf%c1_inf )
    call memfun_del ( f_inf%c2_inf )
    call memfun_del ( f_inf%c3_inf )
    call memfun_del ( f_inf%c4_inf )
    call memfun_del ( f_inf%c_history )
    call memfun_del ( f_inf%path_trin )
    !
    call fxpar_delete ( f_inf%p )
    !
    if ( associated ( f_inf ) ) &
         deallocate ( f_inf )
    !
    !write(pc_get_lun(),*)' end of fxdata_info_delete'
    !
    return
    !
!999 continue
    !
    call pc_error ( ' error in fxdata_info_delete ' )
    !
    return
    !
  end subroutine fxdata_info_delete
  !
  subroutine fxdata_info_nullify ( f_inf )
    !
    type ( fxdata_info ),  pointer :: f_inf              ! input structure
    !
    ! Local variables
    !
    if ( .not. associated ( f_inf ) ) return
    !
    nullify ( f_inf%p )
    call memfun_nul ( f_inf%i1_inf )
    call memfun_nul ( f_inf%i2_inf )
    call memfun_nul ( f_inf%i3_inf )
    call memfun_nul ( f_inf%i4_inf )
    call memfun_nul ( f_inf%i5_inf )
    call memfun_nul ( f_inf%i6_inf )
    call memfun_nul ( f_inf%i7_inf )
    call memfun_nul ( f_inf%i8_inf )
    call memfun_nul ( f_inf%r1_inf )
    call memfun_nul ( f_inf%r2_inf )
    call memfun_nul ( f_inf%r3_inf )
    call memfun_nul ( f_inf%r4_inf )
    call memfun_nul ( f_inf%r5_inf )
    call memfun_nul ( f_inf%r6_inf )
    call memfun_nul ( f_inf%c1_inf )
    call memfun_nul ( f_inf%c2_inf )
    call memfun_nul ( f_inf%c3_inf )
    call memfun_nul ( f_inf%c4_inf )
    call memfun_nul ( f_inf%c_history )
    call memfun_nul ( f_inf%path_trin )
    !
  end subroutine fxdata_info_nullify
  !
  subroutine fxdata_info_delete_n ( n_inf )
    !
    type ( fxdata_info_n ),  pointer :: n_inf            ! fxinfo n structure
    !
    integer                          :: i_info           ! info file index
    !
    if ( .not. associated ( n_inf ) ) return
    !
    xxif_p_exists : if ( associated ( n_inf%p ) ) then
      !
      do_info : do i_info = 1 , n_inf%n
        !
        ! create this structure
        !
        call fxdata_info_delete ( n_inf%p(i_info)%p )
        !
      end do do_info 
      !
      deallocate ( n_inf%p )
      !
    end if xxif_p_exists 
    !
    deallocate ( n_inf )
    !
    return
    !
  end subroutine fxdata_info_delete_n 
  !
  subroutine fxdata_info_nullify_n ( n_inf )
    !
    type ( fxdata_info_n ),  pointer :: n_inf            ! fxinfo n structure
    !
    integer                          :: i_info           ! info file index
    !
    if ( .not. associated ( n_inf ) ) return
    !
    xxif_p_exists : if ( associated ( n_inf%p ) ) then
      !
      do_info : do i_info = 1 , n_inf%n
        !
        ! create this structure
        !
        call fxdata_info_nullify ( n_inf%p(i_info)%p )
        !
      end do do_info 
      !
      nullify ( n_inf%p )
      !
    end if xxif_p_exists 
    !
    nullify ( n_inf )
    !
    return
    !
  end subroutine fxdata_info_nullify_n 
  !
  subroutine fxdata_info_clear ( f_inf, p, info_type, i_err )
    !
    ! initialize input parameters without update
    !
    !
    type ( fxpar_struct ), pointer :: p        ! fxpar divide 
    type ( fxdata_info ),  pointer :: f_inf        ! info structure
    integer,         intent(inout) :: i_err        ! err 0=o.k. -1=error
    character(len=*), intent(in  ) :: info_type      ! shot/pshot
    !
    ! Local variables
    !
    i_err = 0
    !
    ! initialize the error flag
    !
    !print'(" top fxdata_info_clear p=",i4," ass=",l2)', &
    !fxpar_i_pel(), associated ( f_inf ) 
    !
    xxif_not_associated :if ( .not. associated ( f_inf ) ) then
      !
      allocate ( f_inf , stat=i_err ) 
      !
      if ( i_err .ne. 0 ) go to 997
      !
      nullify (f_inf%path_trin) ! jpa
      nullify (f_inf%i1_inf) ! jpa
      nullify (f_inf%i2_inf) ! jpa
      nullify (f_inf%i3_inf) ! jpa
      nullify (f_inf%i4_inf) ! jpa
      nullify (f_inf%i5_inf) ! jpa
      nullify (f_inf%i6_inf) ! jpa
      nullify (f_inf%i7_inf) ! jpa
      nullify (f_inf%i8_inf) ! jpa
      nullify (f_inf%r1_inf) ! jpa
      nullify (f_inf%r2_inf) ! jpa
      nullify (f_inf%r3_inf) ! jpa
      nullify (f_inf%r4_inf) ! jpa
      nullify (f_inf%r5_inf) ! jpa
      nullify (f_inf%r6_inf) ! jpa
      nullify (f_inf%c1_inf) ! jpa
      nullify (f_inf%c2_inf) ! jpa
      nullify (f_inf%c3_inf) ! jpa
      nullify (f_inf%c4_inf) ! jpa
      nullify (f_inf%c_history) ! jpa
      nullify (f_inf%p) ! jpa
      !
      call  fxdata_info_nullify ( f_inf )
      !
    end if xxif_not_associated 
    !
    f_inf%info_type=info_type
    !
    call fxpar_copy ( p, f_inf%p, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    !call pc_get_global ( 'NWIH' , f_inf%nh_inp )
    !
    !call timeglob_get ( f_inf%nt_inp, f_inf%t0_inp, f_inf%dt_inp )
    !
    ! allocate and initialize
    !
    call fxdata_info_clear_1 ( f_inf, i_err )
    !
    return
    !
 997 continue
    !
    call pc_info ( &
    ' error in fxdata_info_clear during allocate ' )
    !
998 continue
    !
    call pc_info ( &
    ' error in fxdata_info_clear during fxpar_copy ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxdata_info_clear ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_info_clear
  !
  subroutine fxdata_info_clear_0 ( f_inf, i_err )
    !
    ! initialize input parameters without update
    !
    type ( fxdata_info ),  pointer :: f_inf        ! input structure
    integer,         intent(inout) :: i_err        ! err 0=o.k. -1=error
    !
    ! Local variables
    !
    character(len=12) :: info_type
    i_err = 0 
    !
    if ( .not. associated ( f_inf ) ) go to 998
    !
    ! get the job name
    !
    call string_date  (            f_inf%c_date   ) ! current date
    call string_time  (            f_inf%c_time   ) ! current time
    call pc_get_jdata ( 'jobname', f_inf%job_name ) ! job name
    info_type = f_inf%info_type                     !save input type
    !
    f_inf%n0_gat     = 0
    !
    f_inf%n_history  = 0
    !
    f_inf%nh_inp = 0
    f_inf%nt_inp = 0
    f_inf%dt_inp = .004
    f_inf%t0_inp = 0.
    f_inf%t1_inp = max ( 0, f_inf%nt_inp - 1 ) * f_inf%dt_inp + f_inf%t0_inp 
    !
    f_inf%n_path_trin = 0
    !
    call fxdata_info_allocate ( f_inf, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    f_inf%path_info   = pathcheck_empty ! info file
    f_inf%path_trot   = pathcheck_empty ! trot file
    f_inf%path_trot_0 = f_inf%path_trot ! trot file original
    !
    f_inf%path_trin(1:f_inf%n_path_trin) = pathcheck_empty ! trin file
    f_inf%line_title  = pathcheck_empty ! line title
    f_inf%line_index  = 0               ! line index number
    !
    f_inf%c_history   = ' '
    !
    !f_inf%source_gather   = .true.      ! source   gather flag
    !f_inf%receiver_gather = .false.     ! receiver gather flag
    !f_inf%hdr_x       = 7               ! x hdr
    !f_inf%hx_gat      = 0               ! x src hdr
    !f_inf%hx_tig      = 0               ! x rec hdr
    !f_inf%hdr_y       = 8               ! y hdr
    !f_inf%hy_gat      = 0               ! y src hdr
    !f_inf%hy_tig      = 0               ! y rec hdr
    f_inf%sort_select = 'NONE'          ! sort type
    f_inf%keep_select = 'NONE'          ! keep type
    f_inf%keep_min    = 1               ! min keep
    f_inf%keep_max    = 99999999        ! max keep
    f_inf%keep_skip   = 0               ! num to skip 
    !
    f_inf%n0_tot      = 0               ! total traces
    !
    f_inf%i1_inf_0    = 0               ! index 1
    f_inf%i2_inf_0    = 0               ! index 2
    f_inf%i3_inf_0    = 0               ! index 3
    f_inf%i4_inf_0    = 0               ! index 4
    f_inf%i5_inf_0    = 0               ! index 5
    f_inf%i6_inf_0    = 0               ! first trace count
    f_inf%i7_inf_0    = 0               ! last trace count
    f_inf%i8_inf_0    = 0               ! num traces in gather
    !
    f_inf%r1_inf_0    = 0               ! x gat loc
    f_inf%r2_inf_0    = 0               ! y gat loc
    f_inf%r3_inf_0    = 0               ! x tig loc min
    f_inf%r4_inf_0    = 0               ! x tig loc min
    f_inf%r5_inf_0    = 0               ! y tig loc max
    f_inf%r6_inf_0    = 0               ! y tig loc max
    !
    f_inf%i1_inf_1    = 0               ! index 1 min
    f_inf%i2_inf_1    = 0               ! index 2 min
    f_inf%i3_inf_1    = 0               ! index 3 min
    f_inf%i4_inf_1    = 0               ! index 4 min
    f_inf%i5_inf_1    = 0               ! index 5 min
    f_inf%i6_inf_1    = 0               ! first trace count min
    f_inf%i7_inf_1    = 0               ! last trace count min
    f_inf%i8_inf_1    = 0               ! num traces in grp min
    !
    f_inf%r1_inf_1    = 0               ! x gat loc min
    f_inf%r2_inf_1    = 0               ! y gat loc min
    f_inf%r3_inf_1    = 0               ! x tig loc min min
    f_inf%r4_inf_1    = 0               ! y tig loc min min
    f_inf%r5_inf_1    = 0               ! x tig loc max min
    f_inf%r6_inf_1    = 0               ! y tig loc max min
    !
    f_inf%i1_inf_2    = 0               ! index 1 max
    f_inf%i2_inf_2    = 0               ! index 2 max
    f_inf%i3_inf_2    = 0               ! index 3 max
    f_inf%i4_inf_2    = 0               ! index 4 max
    f_inf%i5_inf_2    = 0               ! index 5 max
    f_inf%i6_inf_2    = 0               ! first trace count max
    f_inf%i7_inf_2    = 0               ! last trace count max
    f_inf%i8_inf_2    = 0               ! num traces in grp max
    !
    f_inf%r1_inf_2    = 0               ! x gat loc max
    f_inf%r2_inf_2    = 0               ! y gat loc max
    f_inf%r3_inf_2    = 0               ! x tig loc min max
    f_inf%r4_inf_2    = 0               ! y tig loc min max
    f_inf%r5_inf_2    = 0               ! x tig loc max max
    f_inf%r6_inf_2    = 0               ! y tig loc max max
    !
    f_inf%i1_inf(:)   = 0               ! index 1
    f_inf%i2_inf(:)   = 0               ! index 2
    f_inf%i3_inf(:)   = 0               ! index 3
    f_inf%i4_inf(:)   = 0               ! index 4
    f_inf%i5_inf(:)   = 0               ! index 5
    f_inf%i6_inf(:)   = 0               ! first trace count
    f_inf%i7_inf(:)   = 0               ! last trace count
    f_inf%i8_inf(:)   = 0               ! num traces in grp
    !
    f_inf%r1_inf(:)   = 0               ! x gat loc
    f_inf%r2_inf(:)   = 0               ! y gat loc
    f_inf%r3_inf(:)   = 0               ! x tig min
    f_inf%r5_inf(:)   = 0               ! x tig max
    f_inf%r4_inf(:)   = 0               ! y tig min
    f_inf%r6_inf(:)   = 0               ! y tig max
    !
    f_inf%c1_inf(:)   = f_inf%c_date
    f_inf%c2_inf(:)   = f_inf%c_time
    f_inf%c3_inf(:)   = f_inf%job_name
    f_inf%c4_inf(:)   = pathcheck_empty
    !
    f_inf%info_type = info_type
    !
    call fxcard_h_gat_tig ( &
    f_inf%source_gather, f_inf%hdr_x, f_inf%hx_gat, f_inf%hx_tig )
    !
    call fxcard_h_gat_tig ( &
    f_inf%source_gather, f_inf%hdr_y, f_inf%hy_gat, f_inf%hy_tig )
    !
1999 continue
    !
    call fxpar_check_worker_errors ( f_inf%p, i_err )
    !
    return
    !
997 continue
    !
    call pc_info ( &
    ' error in fxdata_info_clear_0 during fxdata_info_allocate ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( &
    ' error in fxdata_info_clear_0 info not associated ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxdata_info_clear_0 ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_info_clear_0
  !
  subroutine fxdata_info_clear_1 ( f_inf, i_err )
    !
    ! initialize input parameters without update
    !
    type ( fxdata_info ),  pointer :: f_inf        ! input structure
    integer,         intent(inout) :: i_err        ! err 0=o.k. -1=error
    !
    ! Local variables
    !
    character(len=12) :: info_type
    i_err = 0
    !
    if ( .not. associated ( f_inf ) ) go to 998
    !
    ! get the job name
    !
    call string_date  (            f_inf%c_date   ) ! current date
    call string_time  (            f_inf%c_time   ) ! current time
    call pc_get_jdata ( 'jobname', f_inf%job_name ) ! job name
    info_type = f_inf%info_type                     !save input type
    !
    f_inf%n0_gat     = 0
    !
    f_inf%n_history  = 0
    !
    f_inf%nh_inp = 0
    f_inf%nt_inp = 0
    f_inf%dt_inp = .004
    f_inf%t0_inp = 0.
   
    f_inf%t1_inp = max ( 0, f_inf%nt_inp - 1 ) * f_inf%dt_inp + f_inf%t0_inp 
    !
    f_inf%n_path_trin = 0
    !
    call fxdata_info_allocate ( f_inf, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    f_inf%path_info   = pathcheck_empty ! info file
    f_inf%path_trot   = pathcheck_empty ! trot file
    f_inf%path_trot_0 = f_inf%path_trot ! trot file original
    f_inf%path_trin(1:f_inf%n_path_trin) = pathcheck_empty ! trin file
    f_inf%line_title  = pathcheck_empty ! line title
    f_inf%line_index  = 0               ! line index number
    !
    f_inf%c_history   = ' '
    !
    f_inf%source_gather   = .true.      ! source   gather flag
    f_inf%receiver_gather = .false.     ! receiver gather flag
    f_inf%hdr_x       = 7               ! x hdr
    f_inf%hx_gat      = 0               ! x src hdr
    f_inf%hx_tig      = 0               ! x rec hdr
    f_inf%bin_x       = 1.              ! x gat bin size
    f_inf%hdr_y       = 8               ! y hdr
    f_inf%hy_gat      = 0               ! y src hdr
    f_inf%hy_tig      = 0               ! y rec hdr
    f_inf%bin_y       = 1.              ! y gat bin size
    f_inf%sort_select = 'NONE'          ! sort type
    f_inf%keep_select = 'NONE'          ! keep type
    f_inf%keep_min    = 1               ! min keep
    f_inf%keep_max    = 99999999        ! max keep
    f_inf%keep_skip   = 0               ! num to skip 
    !
    f_inf%n0_tot      = 0               ! total traces
    !
    f_inf%i1_inf_0    = 0               ! index 1
    f_inf%i2_inf_0    = 0               ! index 2
    f_inf%i3_inf_0    = 0               ! index 3
    f_inf%i4_inf_0    = 0               ! index 4
    f_inf%i5_inf_0    = 0               ! index 5
    f_inf%i6_inf_0    = 0               ! first trace count
    f_inf%i7_inf_0    = 0               ! last trace count
    f_inf%i8_inf_0    = 0               ! num traces in gather
    !
    f_inf%r1_inf_0    = 0               ! x gat loc
    f_inf%r2_inf_0    = 0               ! y gat loc
    f_inf%r3_inf_0    = 0               ! x tig loc min
    f_inf%r4_inf_0    = 0               ! x tig loc min
    f_inf%r5_inf_0    = 0               ! y tig loc max
    f_inf%r6_inf_0    = 0               ! y tig loc max
    !
    f_inf%i1_inf_1    = 0               ! index 1 min
    f_inf%i2_inf_1    = 0               ! index 2 min
    f_inf%i3_inf_1    = 0               ! index 3 min
    f_inf%i4_inf_1    = 0               ! index 4 min
    f_inf%i5_inf_1    = 0               ! index 5 min
    f_inf%i6_inf_1    = 0               ! first trace count min
    f_inf%i7_inf_1    = 0               ! last trace count min
    f_inf%i8_inf_1    = 0               ! num traces in grp min
    !
    f_inf%r1_inf_1    = 0               ! x gat loc min
    f_inf%r2_inf_1    = 0               ! y gat loc min
    f_inf%r3_inf_1    = 0               ! x tig loc min min
    f_inf%r4_inf_1    = 0               ! y tig loc min min
    f_inf%r5_inf_1    = 0               ! x tig loc max min
    f_inf%r6_inf_1    = 0               ! y tig loc max min
    !
    f_inf%i1_inf_2    = 0               ! index 1 max
    f_inf%i2_inf_2    = 0               ! index 2 max
    f_inf%i3_inf_2    = 0               ! index 3 max
    f_inf%i4_inf_2    = 0               ! index 4 max
    f_inf%i5_inf_2    = 0               ! index 5 max
    f_inf%i6_inf_2    = 0               ! first trace count max
    f_inf%i7_inf_2    = 0               ! last trace count max
    f_inf%i8_inf_2    = 0               ! num traces in grp max
    !
    f_inf%r1_inf_2    = 0               ! x gat loc max
    f_inf%r2_inf_2    = 0               ! y gat loc max
    f_inf%r3_inf_2    = 0               ! x tig loc min max
    f_inf%r4_inf_2    = 0               ! y tig loc min max
    f_inf%r5_inf_2    = 0               ! x tig loc max max
    f_inf%r6_inf_2    = 0               ! y tig loc max max
    !
    f_inf%i1_inf(:)   = 0               ! index 1
    f_inf%i2_inf(:)   = 0               ! index 2
    f_inf%i3_inf(:)   = 0               ! index 3
    f_inf%i4_inf(:)   = 0               ! index 4
    f_inf%i5_inf(:)   = 0               ! index 5
    f_inf%i6_inf(:)   = 0               ! first trace count
    f_inf%i7_inf(:)   = 0               ! last trace count
    f_inf%i8_inf(:)   = 0               ! num traces in grp
    !
    f_inf%r1_inf(:)   = 0               ! x gat loc
    f_inf%r2_inf(:)   = 0               ! y gat loc
    f_inf%r3_inf(:)   = 0               ! x tig min
    f_inf%r5_inf(:)   = 0               ! x tig max
    f_inf%r4_inf(:)   = 0               ! y tig min
    f_inf%r6_inf(:)   = 0               ! y tig max
    !
    f_inf%c1_inf(:)   = f_inf%c_date
    f_inf%c2_inf(:)   = f_inf%c_time
    f_inf%c3_inf(:)   = f_inf%job_name
    f_inf%c4_inf(:)   = pathcheck_empty
    !
    f_inf%info_type = info_type
    !
    call fxcard_h_gat_tig ( &
    f_inf%source_gather, f_inf%hdr_x, f_inf%hx_gat, f_inf%hx_tig )
    !
    call fxcard_h_gat_tig ( &
    f_inf%source_gather, f_inf%hdr_y, f_inf%hy_gat, f_inf%hy_tig )
    !
1999 continue
    !
    call fxpar_check_worker_errors ( f_inf%p, i_err )
    !
    return
    !
997 continue
    !
    call pc_info ( &
    ' error in fxdata_info_clear_1 during fxdata_info_allocate ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( &
    ' error in fxdata_info_clear_1 info not associated ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxdata_info_clear_1 ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_info_clear_1
  !
  subroutine fxdata_info_init ( p, path_info, path_trot, info_type,  i_err )
    !
    ! initalize an info file
    ! 
    character(len=*),  intent(in   ) :: path_info        ! trot list file name
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    character(len=*),  intent(in   ) :: info_type      ! shot/pshot
    character(len=*),  intent(in   ) :: path_trot
    !
    ! local variables
    !
    type ( fxdata_info ),    pointer :: f_inf         ! input strucut
    !
    ! initialize the error flag
    !
    !print'(" top fxdata_info_init p=",i5," c=",g12.6," path=",a)',&
    !fxpar_i_pel(), getsys_seconds(), trim(path_info)
    !
    i_err = 0
    !
    ! create shot list fxdata containing only the header 
    !
    call fxdata_info_clear ( f_inf, p, info_type, i_err )
    !
    !print'(" aa1 fxdata_info_init p=",i5," c=",g12.6," path=",a)',&
    !fxpar_i_pel(), getsys_seconds(), trim(path_info)
    !
    ! replace the path_trot parameter
    !
    f_inf%path_trot = path_trot  
    !
    f_inf%path_trot_0 = f_inf%path_trot ! trot file original
    !
    ! write this fxdata to the output file
    !
    !print'(" aa2 fxdata_info_init p=",i5," c=",g12.6," path=",a)',&
    !fxpar_i_pel(), getsys_seconds(), trim(path_info)
    !
    call fxdata_info_to_file ( path_info, f_inf, i_err )
    !
    !print'(" end fxdata_info_init p=",i5," c=",g12.6," path=",a)',&
    !fxpar_i_pel(), getsys_seconds(), trim(path_info)
    !
    if ( i_err .ne. 0 ) go to 998
    !
1999 continue
    !
    ! broadcast the error flag from the root pe to others in a group
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !
    ! delete the fxdata
    !
    call fxdata_info_delete ( f_inf )
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_info_init pe= ", i8, &
    & /,  " during fxcard_card_to_file " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_info_init pe= ", i8 &
    & )' ) &
    fxpar_i_pel()
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_info_init
  !
  subroutine fxdata_info_condition ( f_inf )
    !
    ! set the min and max values for each of the columns
    !
    type ( fxdata_info ),  pointer :: f_inf              ! fxdata_info struct
    !
    integer                        :: j0_gat             ! gather index
    integer                        :: i_err              ! error flag
    !
    !print'(" top fxdata_info_condition n0_gat=",i8," n0_tot=",i8)',&
    !f_inf%n0_gat,f_inf%n0_tot
    !
    xxif_no_data : if ( f_inf%n0_gat .le. 0 ) then
      !
      f_inf%i1_inf_1 = 0 
      f_inf%i1_inf_2 = 0 
      !
      f_inf%i2_inf_1 = 0 
      f_inf%i2_inf_2 = 0 
      !
      f_inf%i3_inf_1 = 0 
      f_inf%i3_inf_2 = 0 
      !
      f_inf%i4_inf_1 = 0 
      f_inf%i4_inf_2 = 0 
      !
      f_inf%i5_inf_1 = 0 
      f_inf%i5_inf_2 = 0 
      !
      f_inf%i6_inf_1 = 0 
      f_inf%i6_inf_2 = 0 
      !
      f_inf%i7_inf_1 = 0 
      f_inf%i7_inf_2 = 0 
      !
      f_inf%i8_inf_1 = 0 
      f_inf%i8_inf_2 = 0 
      !
      f_inf%r1_inf_1 = 0 
      f_inf%r1_inf_2 = 0 
      !
      f_inf%r2_inf_1 = 0 
      f_inf%r2_inf_2 = 0 
      !
      f_inf%r3_inf_1 = 0 
      f_inf%r3_inf_2 = 0 
      !
      f_inf%r4_inf_1 = 0 
      f_inf%r4_inf_2 = 0 
      !
      f_inf%r5_inf_1 = 0 
      f_inf%r5_inf_2 = 0 
      !
      f_inf%r6_inf_1 = 0 
      f_inf%r6_inf_2 = 0 
      !
      f_inf%n0_tot = 0
      !
    else xxif_no_data 
      !
      f_inf%i1_inf_1 = minval ( f_inf%i1_inf(1:f_inf%n0_gat) )
      f_inf%i1_inf_2 = maxval ( f_inf%i1_inf(1:f_inf%n0_gat) )
      !
      f_inf%i2_inf_1 = minval ( f_inf%i2_inf(1:f_inf%n0_gat) )
      f_inf%i2_inf_2 = maxval ( f_inf%i2_inf(1:f_inf%n0_gat) )
      !
      f_inf%i3_inf_1 = minval ( f_inf%i3_inf(1:f_inf%n0_gat) )
      f_inf%i3_inf_2 = maxval ( f_inf%i3_inf(1:f_inf%n0_gat) )
      !
      f_inf%i4_inf_1 = minval ( f_inf%i4_inf(1:f_inf%n0_gat) )
      f_inf%i4_inf_2 = maxval ( f_inf%i4_inf(1:f_inf%n0_gat) )
      !
      f_inf%i5_inf_1 = minval ( f_inf%i5_inf(1:f_inf%n0_gat) )
      f_inf%i5_inf_2 = maxval ( f_inf%i5_inf(1:f_inf%n0_gat) )
      !
      f_inf%i6_inf_1 = minval ( f_inf%i6_inf(1:f_inf%n0_gat) )
      f_inf%i6_inf_2 = maxval ( f_inf%i6_inf(1:f_inf%n0_gat) )
      !
      f_inf%i7_inf_1 = minval ( f_inf%i7_inf(1:f_inf%n0_gat) )
      f_inf%i7_inf_2 = maxval ( f_inf%i7_inf(1:f_inf%n0_gat) )
      !
      f_inf%i8_inf_1 = minval ( f_inf%i8_inf(1:f_inf%n0_gat) )
      f_inf%i8_inf_2 = maxval ( f_inf%i8_inf(1:f_inf%n0_gat) )
      !
      f_inf%r1_inf_1 = minval ( f_inf%r1_inf(1:f_inf%n0_gat) )
      f_inf%r1_inf_2 = maxval ( f_inf%r1_inf(1:f_inf%n0_gat) )
      !
      f_inf%r2_inf_1 = minval ( f_inf%r2_inf(1:f_inf%n0_gat) )
      f_inf%r2_inf_2 = maxval ( f_inf%r2_inf(1:f_inf%n0_gat) )
      !
      f_inf%r3_inf_1 = minval ( f_inf%r3_inf(1:f_inf%n0_gat) )
      f_inf%r3_inf_2 = maxval ( f_inf%r3_inf(1:f_inf%n0_gat) )
      !
      f_inf%r4_inf_1 = minval ( f_inf%r4_inf(1:f_inf%n0_gat) )
      f_inf%r4_inf_2 = maxval ( f_inf%r4_inf(1:f_inf%n0_gat) )
      !
      f_inf%r5_inf_1 = minval ( f_inf%r5_inf(1:f_inf%n0_gat) )
      f_inf%r5_inf_2 = maxval ( f_inf%r5_inf(1:f_inf%n0_gat) )
      !
      f_inf%r6_inf_1 = minval ( f_inf%r6_inf(1:f_inf%n0_gat) )
      f_inf%r6_inf_2 = maxval ( f_inf%r6_inf(1:f_inf%n0_gat) )
      !
      do_j0_gat : do j0_gat = 1 , f_inf%n0_gat
        !
        f_inf%i8_inf(j0_gat) = &
        f_inf%i7_inf(j0_gat) - &
        f_inf%i6_inf(j0_gat) + 1
        !
      end do do_j0_gat 
      !
      f_inf%n0_tot = sum ( f_inf%i8_inf(1:f_inf%n0_gat) )
      !
      !print'(" n0_gat=",i8)',f_inf%n0_gat
      !print'(" i=",i8," i6=",i8," i7=",i8," i8=",i8)',&
      !(j0_gat, f_inf%i6_inf(j0_gat), f_inf%i7_inf(j0_gat), &
      !         f_inf%i8_inf(j0_gat), j0_gat=1,f_inf%n0_gat )
      !
      !
    end if xxif_no_data 
    !
    ! set the path_trin array from c4_inf
    !
    call fxdata_c4_to_path_trin ( f_inf, i_err )
    !
    if ( string_upper_compare ( f_inf%path_trot, pathcheck_empty ) &
    .and. f_inf%n_path_trin .ge. 1 ) &
    f_inf%path_trot = f_inf%path_trin(1)
    !
    f_inf%path_trot_0 = f_inf%path_trot ! trot file original
    !
    !print'(" end fxdata_info_condition n0_gat=",i8," n0_tot=",i8)',&
    !f_inf%n0_gat,f_inf%n0_tot
    !
    return
    !
  end subroutine fxdata_info_condition 
  !
  subroutine fxdata_info_allocate ( f_inf, i_err )
    !
    ! allocate info memory, both head and gath
    !
    type ( fxdata_info ),  pointer :: f_inf        ! input structure
    integer,         intent(inout) :: i_err        ! err 0=o.k. -1=error
    !
    ! Local variables
    !
    i_err = 0
    !
    if ( .not. associated ( f_inf ) ) go to 998
    !
    call fxdata_head_allocate ( f_inf, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    call fxdata_gath_allocate ( f_inf, i_err )
    !
    if ( i_err .ne. 0 ) go to 996
    !
1999 continue
    !
    call fxpar_check_worker_errors ( f_inf%p, i_err )
    !
    return
    !
996 continue
    !
    call pc_info ( &
    ' error in fxdata_info_allocate during fxdata_gath_allocate ' )
    !
    go to 999
    !
997 continue
    !
    call pc_info ( &
    ' error in fxdata_info_allocate during fxdata_head_allocate ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( &
    ' error in fxdata_info_allocate info not associated ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxdata_info_allocate ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_info_allocate
  !
  subroutine fxdata_head_allocate ( f_inf, i_err )
    !
    ! allocate the fxinfo data structure arrays
    !
    type ( fxdata_info ),  pointer :: f_inf            ! fxinfo data struct
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    call memfun_prn_put ( .false. )
    !
    call memfun_all ( f_inf%c_history, f_inf%n_history, 'c_history', i_err )
    call memfun_all ( f_inf%path_trin, f_inf%n_path_trin, 'path_trin', i_err )
    !
    call memfun_prn_res ( )
    !
    if ( i_err .ne. 0 ) go to 998
    !
1999 continue
    !
    call fxpar_check_worker_errors ( f_inf%p, i_err )
    !
    return
    !
998 continue
    !
    call pc_info ( ' error in fxdata_head_allocate during memfun_all ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxdata_head_allocate ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_head_allocate
  !
  subroutine fxdata_gath_allocate ( f_inf, i_err )
    !
    ! allocate the fxinfo data structure arrays
    !
    type ( fxdata_info ),  pointer :: f_inf            ! fxinfo data struct
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    call memfun_prn_put ( .false. )
    !
    call memfun_all ( f_inf%i1_inf, f_inf%n0_gat, 'i1_inf', i_err )
    call memfun_all ( f_inf%i2_inf, f_inf%n0_gat, 'i2_inf', i_err )
    call memfun_all ( f_inf%i3_inf, f_inf%n0_gat, 'i3_inf', i_err )
    call memfun_all ( f_inf%i4_inf, f_inf%n0_gat, 'i4_inf', i_err )
    call memfun_all ( f_inf%i5_inf, f_inf%n0_gat, 'i5_inf', i_err )
    call memfun_all ( f_inf%i6_inf, f_inf%n0_gat, 'i6_inf', i_err )
    call memfun_all ( f_inf%i7_inf, f_inf%n0_gat, 'i7_inf', i_err )
    call memfun_all ( f_inf%i8_inf, f_inf%n0_gat, 'i8_inf', i_err )
    call memfun_all ( f_inf%r1_inf, f_inf%n0_gat, 'r1_inf', i_err )
    call memfun_all ( f_inf%r2_inf, f_inf%n0_gat, 'r2_inf', i_err )
    call memfun_all ( f_inf%r3_inf, f_inf%n0_gat, 'r3_inf', i_err )
    call memfun_all ( f_inf%r5_inf, f_inf%n0_gat, 'r5_inf', i_err )
    call memfun_all ( f_inf%r4_inf, f_inf%n0_gat, 'r4_inf', i_err )
    call memfun_all ( f_inf%r6_inf, f_inf%n0_gat, 'r6_inf', i_err )
    call memfun_all ( f_inf%c1_inf, f_inf%n0_gat, 'c1_inf', i_err )
    call memfun_all ( f_inf%c2_inf, f_inf%n0_gat, 'c2_inf', i_err )
    call memfun_all ( f_inf%c3_inf, f_inf%n0_gat, 'c3_inf', i_err )
    call memfun_all ( f_inf%c4_inf, f_inf%n0_gat, 'c4_inf', i_err )
    !
    f_inf%m0_gat = size ( f_inf%i1_inf, 1 )
    !
    call memfun_prn_res ( )
    !
    if ( i_err .ne. 0 ) go to 998
    !
1999 continue
    !
    call fxpar_check_worker_errors ( f_inf%p, i_err )
    !
    return
    !
998 continue
    !
    call pc_info ( ' error in fxdata_gath_allocate during memfun_all ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxdata_gath_allocate ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_gath_allocate
  !
  subroutine fxdata_gath_reallocate ( f_inf, n0_add, i_err )
    !
    ! reallocate the fxinfo data structure arrays
    !
    type ( fxdata_info ),    pointer :: f_inf            ! fxinfo data struct
    integer,           intent(in   ) :: n0_add           ! number to add
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    integer                          :: m0_gat            ! number of gathers
    !
    m0_gat = size(f_inf%i1_inf,1) + max ( 0, n0_add )
    !
    !print'(" a1 m=",i8, " n=",i8," g=",i8," a=",i8," s=",i8)', &
    !f_inf%m0_gat, f_inf%n0_gat, m0_gat, n0_add, &
    !size ( f_inf%i1_inf, 1 )
    !
    !print'(" a1 m=",i8, " n=",i8," new=",i8" n=",i8," s=",18(1x,i3))',&
    !f_inf%m0_gat, f_inf%n0_gat, m0_gat, n0_add, &
    !size ( f_inf%i1_inf, 1 ), &
    !size ( f_inf%i1_inf, 1 ), &
    !size ( f_inf%i2_inf, 1 ), &
    !size ( f_inf%i3_inf, 1 ), &
    !size ( f_inf%i4_inf, 1 ), &
    !size ( f_inf%i5_inf, 1 ), &
    !size ( f_inf%i6_inf, 1 ), &
    !size ( f_inf%i7_inf, 1 ), &
    !size ( f_inf%i8_inf, 1 ), &
    !size ( f_inf%r1_inf, 1 ), &
    !size ( f_inf%r2_inf, 1 ), &
    !size ( f_inf%r3_inf, 1 ), &
    !size ( f_inf%r4_inf, 1 ), &
    !size ( f_inf%r5_inf, 1 ), &
    !size ( f_inf%r6_inf, 1 ), &
    !size ( f_inf%c1_inf, 1 ), &
    !size ( f_inf%c2_inf, 1 ), &
    !size ( f_inf%c3_inf, 1 ), &
    !size ( f_inf%c4_inf, 1 )
    !
    i_err = 0
    !
    call mem_realloc ( f_inf%i1_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%i2_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%i3_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%i4_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%i5_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%i6_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%i7_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%i8_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%r1_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%r2_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%r3_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%r4_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%r5_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%r6_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%c1_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%c2_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%c3_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    call mem_realloc ( f_inf%c4_inf, m0_gat, i_err )
    if ( i_err .ne. 0 ) go to 998
    !
    f_inf%m0_gat = size ( f_inf%i1_inf, 1 )
    !
1999 continue
    !
    call fxpar_check_worker_errors ( f_inf%p, i_err )
    !
    return
    !
998 continue
    !
    call pc_info ( ' error in fxdata_gath_reallocate during mem_realloc' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxdata_gath_reallocate ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_gath_reallocate
  !
  subroutine fxdata_info_compare ( &
                                   f_inf_1, f_inf_2, &
                                   add_info, clear_info, n_duplicate_gather &
                                 )
    !
    ! make sure there are no duplicate shots between f_inf_1 and f_inf_2
    ! compare the shot list in f_inf_1 with f_inf_2
    ! if everything is o.k. modify f_inf_1 to include f_inf_2
    ! clear the current list of shots, f_info_1
    !
    type ( fxdata_info ),    pointer :: f_inf_1          ! info 1
    type ( fxdata_info ),    pointer :: f_inf_2          ! info 2
    logical,           intent(in   ) :: add_info         ! add info together
    logical,           intent(in   ) :: clear_info       ! clear info
    integer,           intent(inout) :: n_duplicate_gather! num duplicate gat
    !
    ! local variables
    !
    logical                          :: l_duplicate
    integer                          :: i_duplicate
    integer                          :: i1_gat
    integer                          :: i_err
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the duplicate shot counter
    !
    n_duplicate_gather = 0 ! num duplicate shots
    !
    ! check each term in f_inf_1 against each term in f_inf_2
    !
    do_info_1 : do i1_gat = 1 , f_inf_1%n0_gat
      !
      call fxdata_info_compare_i ( &
                                   l_duplicate, i_duplicate, &
                                   i1_gat, f_inf_1, f_inf_2 &
                                 )
      !
      !write ( pc_get_lun(), ' ( &
      !& " fxdata_info_compare i1=",i8," n=", i8," index=", i8, " l=", l2 &
      !& )') &
      !i1_gat, n_duplicate_gather, i_duplicate, l_duplicate
      !
      xxif_l_duplicate : if ( l_duplicate ) then
        !
        n_duplicate_gather = n_duplicate_gather + 1 ! num duplicate gat
        !
        write ( pc_get_lun(), ' ( &
        & " fxdata_info_compare duplicate gathers n=", i8, &
        & " index=", i8, " i1_gat=", i8 &
        & )') &
        n_duplicate_gather, i_duplicate, i1_gat
        !
      end if xxif_l_duplicate 
      !
    end do do_info_1 
    !
    ! add f_inf_2 to f_inf_1
    !
    if ( add_info .and. n_duplicate_gather .le. 0 ) &
    call fxdata_gath_to_info ( 1, f_inf_2%n0_gat, f_inf_2, f_inf_1 )
    !
    ! clear f_inf_2
    !
    if ( clear_info ) &
    call fxdata_info_clear_1 ( f_inf_2, i_err )
    !
    return
    !
  end subroutine fxdata_info_compare  
  !
  subroutine fxdata_info_compare_i ( &
                                     l_duplicate, i_duplicate, &
                                     i1_gat, f_inf_1, f_inf_2 &
                                   )
    !
    ! compare gather i1_gat of info f_inf_1 ot all gathers in f_inf_2
    !
    logical,           intent(inout) :: l_duplicate     ! duplicate flag
    integer,           intent(inout) :: i_duplicate     ! duplicate index
    integer,           intent(in   ) :: i1_gat          ! gather index
    type ( fxdata_info ),    pointer :: f_inf_1         ! info 
    type ( fxdata_info ),    pointer :: f_inf_2         ! info 
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! local variables
    !
    ! initialize the duplicate shot counter
    !
    l_duplicate = .false.
    !
    i_duplicate = 0
    !
    ! check gather i1_gat of f_inf_1 against each gather of f_inf_2
    !
    call fxdata_info_compare_0 ( &
                                 l_duplicate, i_duplicate, &
                                 f_inf_2, &
                                 f_inf_1%i1_inf(i1_gat), &
                                 f_inf_1%i2_inf(i1_gat), &
                                 f_inf_1%i3_inf(i1_gat), &
                                 f_inf_1%i4_inf(i1_gat), &
                                 f_inf_1%i5_inf(i1_gat), &
                                 f_inf_1%i6_inf(i1_gat), &
                                 f_inf_1%i7_inf(i1_gat), &
                                 f_inf_1%i8_inf(i1_gat), &
                                 f_inf_1%r1_inf(i1_gat), &
                                 f_inf_1%r2_inf(i1_gat), &
                                 f_inf_1%r3_inf(i1_gat), &
                                 f_inf_1%r4_inf(i1_gat), &
                                 f_inf_1%r5_inf(i1_gat), &
                                 f_inf_1%r6_inf(i1_gat), &
                                 f_inf_1%c1_inf(i1_gat), &
                                 f_inf_1%c2_inf(i1_gat), &
                                 f_inf_1%c3_inf(i1_gat), &
                                 f_inf_1%c4_inf(i1_gat) &
                               )
    !
    return
    !
  end subroutine fxdata_info_compare_i
  !
  subroutine fxdata_info_compare_0 ( &
                                     l_duplicate, i_duplicate, &
                                     f_inf, &
                                     i1_inf, i2_inf, i3_inf, i4_inf, &
                                     i5_inf, i6_inf, i7_inf, i8_inf, &
                                     r1_inf, r2_inf, r3_inf, &
                                     r4_inf, r5_inf, r6_inf, &
                                     c1_inf, c2_inf, c3_inf, c4_inf &
                                   )
    !
    ! make sure there are no duplicate shots between f_inf_1 and f_inf_2
    ! compare the shot list in f_inf_1 with f_inf_2
    ! if everything is o.k. modify f_inf_1 to include f_inf_2
    ! clear the current list of shots, f_info_1
    !
    logical,           intent(inout) :: l_duplicate     ! duplicate flag
    integer,           intent(inout) :: i_duplicate     ! duplicate index
    type ( fxdata_info ),    pointer :: f_inf           ! info 
    integer,           intent(in   ) :: i1_inf          ! pe index 
    integer,           intent(in   ) :: i2_inf          ! shot index 
    integer,           intent(in   ) :: i3_inf          ! shot index 
    integer,           intent(in   ) :: i4_inf          ! shot index 
    integer,           intent(in   ) :: i5_inf          ! shot index 
    integer,           intent(in   ) :: i6_inf          ! first trace 
    integer,           intent(in   ) :: i7_inf          ! last trace 
    integer,           intent(in   ) :: i8_inf          ! shot index 
    real,              intent(in   ) :: r1_inf          ! shot x loc 
    real,              intent(in   ) :: r2_inf          ! shot y loc 
    real,              intent(in   ) :: r3_inf          ! shot y loc 
    real,              intent(in   ) :: r4_inf          ! shot y loc 
    real,              intent(in   ) :: r5_inf          ! shot y loc 
    real,              intent(in   ) :: r6_inf          ! shot y loc 
    character(len=*),  intent(in   ) :: c1_inf          ! date 
    character(len=*),  intent(in   ) :: c2_inf          ! time 
    character(len=*),  intent(in   ) :: c3_inf          ! job name 
    character(len=*),  intent(in   ) :: c4_inf          ! trin file 
    !
    ! local variables
    !
    integer                          :: i0_gat

    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the duplicate shot counter
    !
    l_duplicate = .false.
    !
    i_duplicate = 0
    !
    ! check each term in f_inf against input term
    !
    do_info : do i0_gat = 1 , f_inf%n0_gat
      !
      call fxdata_info_compare_1 ( &
                                   l_duplicate, &
                                   i1_inf,                     i2_inf, &
                                   i3_inf,                     i4_inf, &
                                   i5_inf,                     i6_inf, &
                                   i7_inf,                     i8_inf, &
                                   r1_inf,                     r2_inf, &
                                   r3_inf,                     r4_inf, &
                                   r5_inf,                     r6_inf, &
                                   c1_inf,                     c2_inf, &
                                   c3_inf,                     c4_inf, &
                                   f_inf%i1_inf(i0_gat), f_inf%i2_inf(i0_gat), &
                                   f_inf%i3_inf(i0_gat), f_inf%i4_inf(i0_gat), &
                                   f_inf%i5_inf(i0_gat), f_inf%i6_inf(i0_gat), &
                                   f_inf%i7_inf(i0_gat), f_inf%i8_inf(i0_gat), &
                                   f_inf%r1_inf(i0_gat), f_inf%r2_inf(i0_gat), &
                                   f_inf%r3_inf(i0_gat), f_inf%r4_inf(i0_gat), &
                                   f_inf%r5_inf(i0_gat), f_inf%r6_inf(i0_gat), &
                                   f_inf%c1_inf(i0_gat), f_inf%c2_inf(i0_gat), &
                                   f_inf%c3_inf(i0_gat), f_inf%c4_inf(i0_gat), &
                                   f_inf%info_type &
                                 )
      !
      xxif_duplicate : if ( l_duplicate ) then
        !
        i_duplicate = i0_gat
        !
        go to 1
        !
      end if xxif_duplicate 
      !
    end do do_info
    !
  1 continue
    !
    return
    !
  end subroutine fxdata_info_compare_0
  !
  subroutine fxdata_info_compare_1 ( &
                                     l_duplicate, &
                                     i1_inf_1, i2_inf_1, i3_inf_1, i4_inf_1, &
                                     i5_inf_1, i6_inf_1, i7_inf_1, i8_inf_1, &
                                     r1_inf_1, r2_inf_1, r3_inf_1, &
                                     r4_inf_1, r5_inf_1, r6_inf_1, &
                                     c1_inf_1, c2_inf_1, c3_inf_1, c4_inf_1, &
                                     i1_inf_2, i2_inf_2, i3_inf_2, i4_inf_2, &
                                     i5_inf_2, i6_inf_2, i7_inf_2, i8_inf_2, &
                                     r1_inf_2, r2_inf_2, r3_inf_2, &
                                     r4_inf_2, r5_inf_2, r6_inf_2, &
                                     c1_inf_2, c2_inf_2, c3_inf_2, c4_inf_2, &
                                     info_type &
                                   )
    !
    ! make sure there are no duplicate shots between f_inf_1 and f_inf_2
    ! compare the shot list in f_inf_1 with f_inf_2
    ! if everything is o.k. modify f_inf_1 to include f_inf_2
    ! clear the current list of shots, f_info_1
    !
    logical,           intent(inout) :: l_duplicate     ! duplicate flag
    integer,           intent(in   ) :: i1_inf_1        ! pe index 
    integer,           intent(in   ) :: i2_inf_1        ! shot index 
    integer,           intent(in   ) :: i3_inf_1        ! shot index 
    integer,           intent(in   ) :: i4_inf_1        ! shot index 
    integer,           intent(in   ) :: i5_inf_1        ! shot index 
    integer,           intent(in   ) :: i6_inf_1        ! first trace 
    integer,           intent(in   ) :: i7_inf_1        ! last trace 
    integer,           intent(in   ) :: i8_inf_1        ! shot index 
    real,              intent(in   ) :: r1_inf_1        ! shot x loc 
    real,              intent(in   ) :: r2_inf_1        ! shot y loc 
    real,              intent(in   ) :: r3_inf_1        ! shot y loc 
    real,              intent(in   ) :: r4_inf_1        ! shot y loc 
    real,              intent(in   ) :: r5_inf_1        ! shot y loc 
    real,              intent(in   ) :: r6_inf_1        ! shot y loc 
    character(len=*),  intent(in   ) :: c1_inf_1        ! date 
    character(len=*),  intent(in   ) :: c2_inf_1        ! time 
    character(len=*),  intent(in   ) :: c3_inf_1        ! job name 
    character(len=*),  intent(in   ) :: c4_inf_1        ! trin file 
    integer,           intent(in   ) :: i1_inf_2        ! pe index 
    integer,           intent(in   ) :: i2_inf_2        ! shot index 
    integer,           intent(in   ) :: i3_inf_2        ! shot index 
    integer,           intent(in   ) :: i4_inf_2        ! shot index 
    integer,           intent(in   ) :: i5_inf_2        ! shot index 
    integer,           intent(in   ) :: i6_inf_2        ! first trace 
    integer,           intent(in   ) :: i7_inf_2        ! last trace 
    integer,           intent(in   ) :: i8_inf_2        ! shot index 
    real,              intent(in   ) :: r1_inf_2        ! shot x loc 
    real,              intent(in   ) :: r2_inf_2        ! shot y loc 
    real,              intent(in   ) :: r3_inf_2        ! shot y loc 
    real,              intent(in   ) :: r4_inf_2        ! shot y loc 
    real,              intent(in   ) :: r5_inf_2        ! shot y loc 
    real,              intent(in   ) :: r6_inf_2        ! shot y loc 
    character(len=*),  intent(in   ) :: c1_inf_2        ! date 
    character(len=*),  intent(in   ) :: c2_inf_2        ! time 
    character(len=*),  intent(in   ) :: c3_inf_2        ! job name 
    character(len=*),  intent(in   ) :: c4_inf_2        ! trin file 
    character(len=*),  intent(in   ) :: info_type       ! shot / pshot
    !
    ! local variables
    !
    real                             :: r0_eps
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    r0_eps = 0.1
    !
    l_duplicate = .false.
    !
    if ( string_upper_compare (info_type, 'SHOT' ) &
    .and. string_upper_compare ( c4_inf_1,     c4_inf_2 ) &
    .and.                        i6_inf_1 .eq. i6_inf_2 ) &
    l_duplicate = .true.
    !
    if ( ( string_upper_compare (info_type, 'PLANE_WAVE' ) &
      .or. string_upper_compare (info_type, 'CYLINDRICAL' ) ) &
    .and. abs(r1_inf_1 - r1_inf_2) .lt. r0_eps &
    .and. abs(r2_inf_1 - r2_inf_2) .lt. r0_eps ) &
    l_duplicate = .true.
    !
    !print'(" fxdata_info_compare_1 c=",i8," l=",l2,1x,l2,1x,l2,&
    !& " i=",i8,1x,i8,&
    !& " c=",a40,1x,a40)', &
    !i_call, l_duplicate, string_upper_compare ( c4_inf_1,     c4_inf_2 ), &
    !i6_inf_1 .eq. i6_inf_2, i6_inf_1, i6_inf_2, & 
    !trim(c4_inf_1), trim(c4_inf_2)
    !
    return
    !
  end subroutine fxdata_info_compare_1
  !
  subroutine fxdata_info_build_pw ( &
                                    p, &
                                    path_info, &
                                    n_path_trin, path_trin, &
                                    line_title, line_index, &
                                    source_gather, receiver_gather, &
                                    hdr_x, hdr_y, &
                                    bin_x, bin_y, &
                                    l0_inp, i1_inp, i2_inp, &
                                    i_trin_1, i_trin_2, &
                                    hdr_x_min, hdr_x_max, &
                                    hdr_y_min, hdr_y_max, &
                                    num_ray, px_ray, py_ray, &
                                    i_err &
                                  )
    !
    ! build the info file for a trace file
    !
    type ( fxpar_struct ),   pointer :: p             ! fxpar structure
    character(len=*),  intent(in   ) :: path_info
    integer,           intent(in   ) :: n_path_trin
    character(len=*),  intent(in   ) :: path_trin(:)
    character(len=*),  intent(in   ) :: line_title ! line title
    integer,           intent(in   ) :: line_index ! line index number
    logical,           intent(in   ) :: source_gather  ! source   gather flag
    logical,           intent(in   ) :: receiver_gather! receiver gather flag
    integer,           intent(in   ) :: hdr_x      ! x hdr
    real,              intent(in   ) :: bin_x      ! x bin size
    integer,           intent(in   ) :: hdr_y      ! y hdr
    real,              intent(in   ) :: bin_y      ! y bin size
    !
    logical,           intent(in   ) :: l0_inp(:)        ! info every trace
    integer,           intent(in   ) :: i1_inp(:)        ! inp min
    integer,           intent(in   ) :: i2_inp(:)        ! inp max
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    real,              intent(in   ) :: hdr_x_min, hdr_x_max
    real,              intent(in   ) :: hdr_y_min, hdr_y_max
    integer,           intent(in   ) :: num_ray
    real,              intent(in   ) :: px_ray(:)
    real,              intent(in   ) :: py_ray(:)
    !
    ! local variables
    !
    integer                          :: i_trin_1         ! inp min
    integer                          :: i_trin_2         ! inp max
    integer                          :: nh_inp           ! header dimension
    integer                          :: nt_inp           ! trace dimension
    integer                          :: nh_glb           ! header dimension
    integer                          :: nt_glb           ! trace dimension
    !
    integer                          :: i_path_trin      ! input file index
    integer                          :: i_trin           ! input trace index

    type ( fxdata_info ),    pointer :: f_inf
    !

    !
    ! initialize the error flag
    !
    i_err = 0
    !
    nullify ( f_inf     )
    !
    ! create the info structure
    !
    call fxdata_info_create ( &
                              f_inf, p, &
                              path_info, n_path_trin, path_trin, &
                              line_title, line_index, &
                              source_gather, receiver_gather, &
                              hdr_x, hdr_y, &
                              bin_x, bin_y, &
                              'plane_wave', &
                              i_err &
                            )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! only build the data base on the root processor
    !
    call fxdata_info_build_pw_1 ( &
                                  f_inf, &
                                  l0_inp, i_trin, &
                                  i_trin_1, i_trin_2, &
                                  n_path_trin, &
                                  path_trin, path_info, &
                                  hdr_x_min, hdr_x_max, &
                                  hdr_y_min, hdr_y_max, &
                                  num_ray, px_ray, py_ray,  &
                                  i_err &
                                )
    !
1999 continue
    !
    if (         associated ( f_inf ) ) &
    call fxdata_info_delete ( f_inf )
    !
    return
    !
    go to 999
    !
992 continue
    !
    call pc_info (' error in fxdata_info_build during fxdata_info_build_1 ' )
    !
    go to 999

994 continue
    !
    call pc_info ( ' error in fxdata_info_build traces should be ', &
                                                     'the same length' )
    call pc_info ( ' file index=', i_path_trin )
    call pc_info ( ' first header length=', nh_glb, ' current=', nh_inp )
    call pc_info ( ' first trace  length=', nt_glb, ' current=', nt_inp )
    !
    go to 999
    !
995 continue
    !
    call pc_info ( ' error in fxdata_info_build during memory allocation ' )
    !
    go to 999
    !
996 continue 
    !
    go to 999
    !
998 continue
    !
    call pc_info ( ' error in fxdata_info_build during fxdata_info_create ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_info_build ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_info_build_pw
  !
  subroutine fxdata_info_build_pw_1 ( &
                                         f_inf, &
                                         l0_inp, i_trin, &
                                         i_trin_1, i_trin_2, &
                                         n_path_trin, &
                                         path_trin, path_info, &
                                         hdr_x_min, hdr_x_max, &
                                         hdr_y_min, hdr_y_max, &
                                         num_ray,  px_ray, py_ray,  &
                                         i_err &
                                       )
    !
    ! input the next input trace
    !
    !
    type ( fxdata_info ),    pointer :: f_inf
    logical,           intent(in   ) :: l0_inp(:)        ! info every trace
    integer,           intent(in   ) :: i_trin           ! trace index in file
    integer,           intent(in   ) :: i_trin_1         ! first input trace
    integer,           intent(in   ) :: i_trin_2         ! last  input trace
   
    integer,           intent(in   ) :: n_path_trin
    character(len=*),  intent(in   ) :: path_trin(:)
    character(len=*),  intent(in   ) :: path_info
    real,              intent(in   ) :: hdr_x_min, hdr_x_max
    real,              intent(in   ) :: hdr_y_min, hdr_y_max
    integer,           intent(in   ) :: num_ray
    real,              intent(in   ) :: px_ray(:)
    real,              intent(in   ) :: py_ray(:)
    !
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !
    character(len=fxcard_card_len)   :: c_card           ! card image
    integer                          :: i_gat
    !
    ! initialize the error flag
    !
    i_err = 0
    ! 
    f_inf%i2_inf_0 = 0         ! input gather count
    f_inf%i4_inf_0 = 0         ! input gather count
    f_inf%i5_inf_0 = 1 - i_trin_2
    !
    xxdo_i_gat: do i_gat = 1, num_ray
      !   
      f_inf%i1_inf_0 = 1  !i_path_trin                ! input file  count
      !
      f_inf%i2_inf_0 = f_inf%i2_inf_0 + 1         ! input gather count
      !
      f_inf%i3_inf_0 = i_gat                      ! input gather index
      !
      f_inf%i4_inf_0 = f_inf%i4_inf_0 + 1         ! input gather count
      !
      f_inf%i5_inf_0 = f_inf%i5_inf_0 + i_trin_2  ! first total trace
      !
      f_inf%i6_inf_0 = 1
      !
      f_inf%i7_inf_0 = i_trin_2
      !
      f_inf%i8_inf_0 = i_trin_2
      !
      f_inf%r1_inf_0 = px_ray(i_gat)*1000000.0
      !
      f_inf%r2_inf_0 = py_ray(i_gat)*1000000.0
      !
      f_inf%r3_inf_1 = hdr_x_min
      !
      f_inf%r4_inf_2 = hdr_x_max
      !
      f_inf%r5_inf_1 = hdr_y_min
      !
      f_inf%r6_inf_2 = hdr_y_max
      !
      call string_date  (            f_inf%c1_inf_0 ) ! current date
      !
      call string_time  (            f_inf%c2_inf_0 ) ! current time
      !
      call pc_get_jdata ( 'jobname', f_inf%c3_inf_0 ) ! job name
      !
      f_inf%c4_inf_0 = path_trin(1)
      !
      call fxdata_gath_to_info ( &
                                 f_inf, &
              f_inf%i1_inf_0, f_inf%i2_inf_0, f_inf%i3_inf_0, f_inf%i4_inf_0, &
              f_inf%i5_inf_0, f_inf%i6_inf_0, f_inf%i7_inf_0, f_inf%i8_inf_0, &
              f_inf%r1_inf_0, f_inf%r2_inf_0, f_inf%r3_inf_1, f_inf%r4_inf_2, &
              f_inf%r5_inf_1, f_inf%r6_inf_2, &
              f_inf%c1_inf_0, f_inf%c2_inf_0, f_inf%c3_inf_0, f_inf%c4_inf_0 &
                           )
      !
     end do xxdo_i_gat
      !
      write ( c_card,'( &
      & " fxdata_info_build_1 fxinfo number_of_traces=",i8, & 
      & " number_of_gathers=",i8 &
      & )') &
      n_path_trin*i_trin_2, n_path_trin
      !
      !call fxdata_hist_to_info ( f_inf, c_card, i_err )
      !
      if ( i_err .ne. 0 ) go to 997
      !
      !print'(" aft fxdata_hist_to_info n=",i8)', f_inf%n_history
      !
      ! close the info file the will replace the header with the current values
      !
      call fxdata_info_to_file ( path_info, f_inf, i_err )
      !
      if ( i_err .ne. 0 ) go to 996
      !
    return
    !
996 continue
    !
    call pc_error ( &
    ' error in fxdata_info_build_pw_1 during fxdata_info_to_file ' )
    !
    go to 999
    !
997 continue
    !
    call pc_error ( &
    ' error in fxdata_info_build_pw_1 during fxdata_hist_to_info ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( &
    ' error in fxdata_info_build_1 during fxdata_line_to_card ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_info_build_1 ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_info_build_pw_1
  !
  subroutine fxdata_info_print ( f_inf, c_title )
    !
    ! read the fxinfo structure from c_inf
    ! this assume both f_inf and c_inf have already been created
    !
    type ( fxdata_info ),    pointer :: f_inf            ! fxinfo structure
    character(len=*),  intent(in   ) :: c_title
    !
    integer                          :: j0_gat           ! gather index
    !
    if ( .not. associated ( f_inf ) ) return
    !
    if ( f_inf%p%group_root ) &
    write ( pc_get_lun(), '( &
    & /, " number of gathers =", i8, 1x, a,  &
    & /, "    index input_index first     last ", &
    & "x_gather     y_gather     file " &
    & )') &
    f_inf%n0_gat, trim(c_title)
    !
    if ( f_inf%p%group_root ) &
    write ( pc_get_lun(), '( &
    & " ", &
    & i8, 1x, i8, 1x, i8, 1x, i8, 1x, g12.6, 1x, g12.6, 1x, a &
    & )') &
    ( j0_gat, &
      f_inf%i2_inf ( j0_gat ), &
      f_inf%i6_inf ( j0_gat ), &
      f_inf%i7_inf ( j0_gat ), &
      f_inf%r1_inf ( j0_gat ), &
      f_inf%r2_inf ( j0_gat ), &
      trim(f_inf%c4_inf ( j0_gat )), &
      j0_gat = 1 , f_inf%n0_gat )
    !
    return
    !
  end subroutine fxdata_info_print
  !
  subroutine fxdata_info_to_file ( path_info, f_inf, i_err )
    !
    character(len=*),  intent(in   ) :: path_info        ! info file
    type ( fxdata_info ),    pointer :: f_inf         ! input strucut
    integer,           intent(inout) :: i_err         ! err 0=o.k. -1=error
    !
    type ( fxcard_struct ),    pointer :: c_inf         ! fxcard structure
    !
    nullify (c_inf) ! jpa
    !
    i_err = 0
    !
    ! update the path_info file name
    !
!print'(" top fxdata_info_to_file path_info=",a)',trim(path_info)
    !
    f_inf%path_info = path_info
    !
    ! update the min max values
    !
    call fxdata_info_condition ( f_inf )
    !
    ! write the info to a card set
    !
!print'(" aa1 fxdata_info_to_file path_info=",a)',trim(path_info)
    !
    call fxdata_info_to_card ( f_inf, c_inf, i_err )
    !
!print'(" aa2 fxdata_info_to_file path_info=",a)',trim(path_info)
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! write the cardset to a file
    !
    call fxcard_card_to_file ( f_inf%p, f_inf%path_info, c_inf, i_err )
!print'(" end fxdata_info_to_file path_info=",a)',trim(path_info)
    !
    if ( i_err .ne. 0 ) go to 997
    !
    return
    !
997 continue
    !
    call pc_info ( ' error in fxdata_info_to_file during fxcard_card_to_file ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( ' error in fxdata_info_to_file during fxdata_card_to_file ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxdata_info_to_file ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_info_to_file
  !
  subroutine fxdata_info_to_file_n ( path_num, path_info, n_inf, i_err )
    !
    ! write each of the fxinfo files to disk
    !
    type ( fxdata_info_n ),    pointer :: n_inf            ! fxinfo n structure
    integer,           intent(inout) :: path_num         ! number of files
    character(len=*),  intent(in   ) :: path_info(:)     ! info file
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    integer                          :: i_info           ! info file index
    !
    i_err = 0
    !
    ! - allocate the fxdata_info_n structure
    !
    if ( .not. associated ( n_inf ) ) go to 999
    !
    ! write each structure
    !
    do_info : do i_info = 1 , n_inf%n 
      !
      ! write this info file
      !
      call fxdata_info_to_file ( path_info(i_info), n_inf%p(i_info)%p, i_err )
      !
      if ( i_err .ne. 0 ) go to 998
      !
    end do do_info 
    !
    return
    !
998 continue
    !
    call pc_info ( &
    ' error in fxdata_info_to_file_n during fxdata_file_to_info i=', i_info, &
    path_info(i_info) )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_info_to_file_n ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_info_to_file_n
  !
  subroutine fxdata_file_to_info_0 ( path_info, f_inf, p, i_err )
    !
    character(len=*),  intent(in   ) :: path_info     ! info file
    type ( fxdata_info ),    pointer :: f_inf         ! input strucut
    type ( fxpar_struct ),   pointer :: p         ! fxpar divide
    integer,           intent(inout) :: i_err         ! err 0=o.k. -1=error
    !
    !print'(" top fxdata_file_to_info_0 p=",i4," f=",a)', &
    !fxpar_i_pel(), trim ( path_info )
    !
    i_err = 0
    !
    call fxdata_info_clear ( f_inf, p, 'UNKNOWN', i_err)
    !
    !print'(" aa1 fxdata_file_to_info_0 p=",i4," f=",a)', &
    !fxpar_i_pel(), trim ( path_info )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    call fxdata_file_to_info_1 ( path_info, f_inf, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
1999 continue
    !
    !if ( pc_do_not_process_traces() ) &
    !print'(" end fxdata_file_to_info_0 pe=",i8," err=",i8," f=",a)',&
    !fxpar_i_pel(), i_err, trim ( path_info )
    !
    return
    !
997 continue
    !
    call pc_info ( &
    ' error in fxdata_file_to_info_0 during fxdata_file_to_info_1 ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( &
    ' error in fxdata_file_to_info_0 during fxdata_info_clear ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxdata_file_to_info_0 ' )
    !
    i_err = -1
    !
    go to 1999
    !
    !return
    !
  end subroutine fxdata_file_to_info_0
  !
  subroutine fxdata_file_to_info_1 ( path_info, f_inf, i_err )
    !
    character(len=*),  intent(in   ) :: path_info     ! info file
    type ( fxdata_info ),    pointer :: f_inf         ! input strucut
    integer,           intent(inout) :: i_err         ! err 0=o.k. -1=error
    !
    type ( fxcard_struct ),    pointer :: c_inf         ! fxcard structure
    !
    nullify (c_inf) ! jpa
    !
    i_err = 0
    !
    !print'(" top fxdata_file_to_info_1 p=",i4," f=",a)', &
    !fxpar_i_pel(), trim ( path_info )
    !
    f_inf%path_info = path_info
    !
    ! read the cardset from the file
    !
    !print'(" bb2 fxdata_file_to_info_1 p=",i4," f=",a)', &
    !fxpar_i_pel(), trim ( f_inf%path_info )
    !
    call fxcard_file_to_card ( f_inf%p, f_inf%path_info, c_inf, i_err )
    !
    !print'(" bb3 fxdata_file_to_info_1 p=",i4," e=",i8," f=",a)', &
    !fxpar_i_pel(), i_err, trim ( f_inf%path_info )
    !
    !if ( pc_do_not_process_traces() ) &
    !print'(" aa1 fxdata_file_to_info_1 f=",a)', trim ( path_info )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! read the info from the card set
    !
    call fxdata_card_to_info ( f_inf, c_inf, i_err )
    !
    !print'(" bb4 fxdata_file_to_info_1 p=",i4," f=",a)', &
    !fxpar_i_pel(), trim ( f_inf%path_info )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    f_inf%path_info = path_info
    !
    ! update the min max values
    !
    !print'(" bb5 fxdata_file_to_info_1 p=",i4," f=",a)', &
    !fxpar_i_pel(), trim ( f_inf%path_info )
    !
    call fxdata_info_condition ( f_inf )
    !
    !print'(" bb6 fxdata_file_to_info_1 p=",i4," f=",a)', &
    !fxpar_i_pel(), trim ( f_inf%path_info )
    !print'(" fxdata_file_to_info_1 aa2 path_trot=",a)', trim(f_inf%path_trot) 
    !
    !if ( pc_do_not_process_traces() ) &
    !
    !print'(" end fxdata_file_to_info_1 f=",a)', trim ( path_info )
    !
    return
    !
997 continue
    !
    call pc_info ( &
    ' error in fxdata_file_to_info_1 during fxdata_card_to_info ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( &
    ' error in fxdata_file_to_info_1 during fxcard_file_to_card ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxdata_file_to_info_1 ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_file_to_info_1
  !
  subroutine fxdata_file_to_info_n ( path_num, path_info, n_inf, p, i_err )
    !
    ! read each of the fxinfo files from disk
    !
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    type ( fxdata_info_n ),    pointer :: n_inf            ! fxinfo n structure
    integer,           intent(in   ) :: path_num         ! number of files
    character(len=*),  intent(in   ) :: path_info(:)     ! info file
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    integer                          :: i_info           ! info file index
    !
    i_err = 0
    !
    ! - allocate the fxdata_info_n structure
    !
    !if ( pc_do_not_process_traces() ) &
    !print'(" top fxdata_file_to_info_n n=",i8)', path_num
    !
!print*,' path_info=', trim ( path_info(1) )
!print*,' path_info=', trim ( path_info(path_num) )
    !
    if (           associated ( n_inf ) ) &
    call fxdata_info_delete_n ( n_inf )
!print*,' bb1 fxdata_file_to_info_n ', path_num
    !
    allocate ( n_inf , stat=i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
!print*,' bb2 fxdata_file_to_info_n ', path_num
    !
    nullify (n_inf%p) ! jpa
    !
    n_inf%n = path_num
    !
    ! allocate the fxdata_info_p structure
    !
    allocate ( n_inf%p(n_inf%n), stat=i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
!print*,' bb3 fxdata_file_to_info_n ', path_num
    !
    do i_info = 1, n_inf%n
        nullify (n_inf%p(i_info)%p) ! jpa
    end do
    !
    ! nullify the memory pointers
    !
    !call fxdata_info_nullify_n ( n_inf )
    !
    do_info : do i_info = 1 , n_inf%n 
      !
      ! read this info file
      !
      !if ( pc_do_not_process_traces() ) &
      !print'(" a1 fxdata_file_to_info_n i=",i8," f=",a)', &
      !i_info, trim(path_info(i_info))
      !
      call fxdata_file_to_info ( &
      path_info(i_info), n_inf%p(i_info)%p, p, i_err )
      !
      !if ( pc_do_not_process_traces() ) &
      !print'(" a2 fxdata_file_to_info_n i=",i8," n=",i8," f=",a)', &
      !i_info, n_inf%p(i_info)%p%n0_gat, trim(path_info(i_info))
      !
      if ( i_err .ne. 0 ) &
      call pc_info ( &
      ' error in fxdata_file_to_info_n during fxdata_file_to_info i=', &
      i_info, path_info(i_info) )
      !
      if ( i_err .ne. 0 ) i_err = 0
      !
      if ( i_err .ne. 0 ) go to 996
      !
    end do do_info 
    !
    !if ( pc_do_not_process_traces() ) &
    !print'(" end fxdata_file_to_info_n n=",i8)', path_num
    !
    return
    !
996 continue
    !
    call pc_info ( &
    ' error in fxdata_file_to_info_n during fxdata_file_to_info i=', i_info, &
    path_info(i_info) )
    !
    go to 999
    !
997 continue
    !
    call pc_info ( ' error in fxdata_file_to_info_n during allocate 2 ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( ' error in fxdata_file_to_info_n during allocate 1 ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_file_to_info_n ' )
    !
    if (           associated ( n_inf ) ) &
    call fxdata_info_delete_n ( n_inf )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_file_to_info_n
  !
  subroutine fxdata_card_to_info ( f_inf, c_inf, i_err )
    !
    ! read the fxinfo structure from c_inf
    ! this assume both f_inf and c_inf have already been created
    !
    type ( fxdata_info ),    pointer :: f_inf            ! fxinfo structure
    type ( fxcard_struct ),    pointer :: c_inf            ! fxcard structure
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    i_err = 0
    !
    if ( .not. associated ( c_inf ) ) go to 998
    !
    ! create an info structure
    !
    !if ( pc_do_not_process_traces() ) &
    !print'(" fxdata_card_to_info bef fxdata_info_clear_1 " )'
    !
    call fxdata_info_clear_1 ( f_inf, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! read head info from c_inf
    !
    !if ( pc_do_not_process_traces() ) &
    !print'(" fxdata_card_to_info bef fxdata_card_to_head " )'
    !
    call fxdata_card_to_head ( f_inf, c_inf, i_err )
    !
    if ( i_err .ne. 0 ) go to 996
    !
    ! write gather info to c_inf
    !
    !if ( pc_do_not_process_traces() ) &
    !print'(" fxdata_card_to_info bef fxdata_card_to_gath " )'
    !
    call fxdata_card_to_gath ( f_inf, c_inf, i_err )
    !
    !print'(" fxdata_card_to_info aft fxdata_card_to_gath " )'
    !
    if ( i_err .ne. 0 ) go to 995
    !
1999 continue
    !
    call fxpar_check_worker_errors ( f_inf%p, i_err )
    !
    if ( i_err .ne. 0 ) &
    write(pc_get_lun(),*) 'fxdata_card_to_info: par_groups=',&
    f_inf%p%par_groups
    !
    !if ( pc_do_not_process_traces() ) &
    !print'(" end fxdata_card_to_info " )'
    !
!print*,' fxdata_card_to_info end '
    !
    return
    !
995 continue
    !
    call pc_info ( ' error in fxdata_card_to_info during fxdata_card_to_gath ' )
    !
    go to 999
    !
996 continue
    !
    call pc_info ( ' error in fxdata_card_to_info during fxdata_card_to_head ' )
    !
    go to 999
    !
997 continue
    !
    call pc_info ( ' error in fxdata_card_to_info during fxdata_info_clear_1 ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( ' error in fxdata_card_to_info during xxx ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_card_to_info ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_card_to_info 
  !
  subroutine fxdata_info_to_card ( f_inf, c_inf, i_err )
    !
    ! write the fxinfo structure from path_info
    !
    type ( fxdata_info ),    pointer :: f_inf            ! fxinfo structure
    type ( fxcard_struct ),    pointer :: c_inf            ! fxcard structure
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    i_err = 0
    ! 
    if ( .not. associated ( f_inf ) ) go to 998
    !
    ! create a cardlist structure
    ! 
    nullify ( c_inf )
    !
  !print*,' fxdata_info_to_card bef fxcard_create '
    !
    call fxcard_create ( 'fxdata_info_to_card', c_inf )
    !
  !print*,' fxdata_info_to_card aft fxcard_create '
    !
    ! write head info to c_inf
    !
    call fxdata_head_to_card ( f_inf, c_inf, i_err )
    !
  !print*,' fxdata_info_to_card aft fxdata_head_to_card '
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! increase the cardset length to hold n0_gat lines
    !
  !print'(" fxdata_info_to_card bef fxcard_len_add ", &
  !& " m_card=",i8," n_card=",i8," n0_gat=",i8)', &
  !c_inf%m_card, c_inf%n_card, f_inf%n0_gat
    !
    if ( f_inf%n0_gat .gt. c_inf%m_card-c_inf%n_card ) &
    call fxcard_len_add ( c_inf, f_inf%n0_gat, i_err )
    !
  !print*,' fxdata_info_to_card aft fxcard_len_add '
    !
    if ( i_err .ne. 0 ) go to 996
    !
    ! write gather info to c_inf
    !
    call fxdata_gath_to_card ( f_inf, c_inf, i_err )
    !
  !print*,' fxdata_info_to_card aft fxdata_gath_to_card '
    !
    if ( i_err .ne. 0 ) go to 995
    !
1999 continue
    !
    call fxpar_check_worker_errors ( f_inf%p, i_err )
    !
    return
    !
995 continue
    !
    call pc_info ( ' error in fxdata_info_to_card during fxdata_gath_to_card ' )
    !
    go to 999
    !
996 continue
    !
    call pc_info ( ' error in fxdata_info_to_card during fxcard_len_add ' )
    !
    go to 999
    !
997 continue
    !
    call pc_info ( ' error in fxdata_info_to_card during fxdata_head_to_card ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( ' error in fxdata_info_to_card info not associated ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_info_to_card ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_info_to_card
  !
  subroutine fxdata_info_to_info ( i_inp, n_inp, f_inp, f_out )
    !
    ! add a header info and n_inp from f_inp to f_out
    !
    integer,           intent(in   ) :: i_inp           ! first to add
    integer,           intent(in   ) :: n_inp           ! num to add
    type ( fxdata_info ),    pointer :: f_inp           ! input strucut
    type ( fxdata_info ),    pointer :: f_out           ! input strucut
    !
    !  add head info
    !
    call fxdata_hdrw_to_info ( f_inp, f_out )
    !
    !  add gath info
    !
    call fxdata_gath_to_info ( i_inp, n_inp, f_inp, f_out )
    !
    return
    !
  end subroutine fxdata_info_to_info 
  !
  subroutine fxdata_hdrw_to_info ( f_inp, f_out )
    !
    ! copy the header word info
    !
    type ( fxdata_info ),    pointer :: f_inp           ! input strucut
    type ( fxdata_info ),    pointer :: f_out           ! input strucut
    !
    f_out%hdr_x           = f_inp%hdr_x
    f_out%hx_gat          = f_inp%hx_gat 
    f_out%hx_tig          = f_inp%hx_tig 
    f_out%bin_x           = f_inp%bin_x
    f_out%hdr_y           = f_inp%hdr_y
    f_out%hy_gat          = f_inp%hy_gat 
    f_out%hy_tig          = f_inp%hy_tig 
    f_out%bin_y           = f_inp%bin_y
    f_out%source_gather   = f_inp%source_gather 
    f_out%receiver_gather = f_inp%receiver_gather 
    f_out%sort_select     = f_inp%sort_select 
    f_out%keep_select     = f_inp%keep_select 
    f_out%keep_min        = f_inp%keep_min 
    f_out%keep_max        = f_inp%keep_max 
    f_out%keep_skip       = f_inp%keep_skip 
    !
    return
    !
  end subroutine fxdata_hdrw_to_info
  !
  subroutine fxdata_gath_to_info ( i_inp, n_inp, f_inp, f_out )
    !
    ! add a gather from f_inp to f_out
    !
    integer,           intent(in   ) :: i_inp           ! first to add
    integer,           intent(in   ) :: n_inp           ! num to add
    type ( fxdata_info ),    pointer :: f_inp           ! input strucut
    type ( fxdata_info ),    pointer :: f_out           ! input strucut
    !
    integer                          :: j_inp
    integer                          :: j_inp_1         ! first index to add
    integer                          :: j_inp_2         ! last  index to add
    !
    j_inp_1 = i_inp
    !
    j_inp_2 = min ( f_inp%n0_gat, i_inp+n_inp-1 )
    !
    !print'(" fxdata_gath_to_info i=",i8," n=",i8,/," ni=",i8," no=",i8,&
    !& /," j1=",i8," j2=",i8)',&
    !i_inp,n_inp,f_inp%n0_gat,f_out%n0_gat,j_inp_1,j_inp_2
    !
    do_inp : do j_inp = j_inp_1 , j_inp_2
      !
      !print'(" a j=",i8," n=",i8," i6=",i8," c4=",a)',&
      !j_inp,f_out%n0_gat,f_inp%i6_inf(j_inp),trim(f_inp%c4_inf(j_inp))
      !
      call fxdata_gath_to_info_1 ( &
                                   f_out, &
                                   f_inp%i1_inf(j_inp), &
                                   f_inp%i2_inf(j_inp), &
                                   f_inp%i3_inf(j_inp), &
                                   f_inp%i4_inf(j_inp), &
                                   f_inp%i5_inf(j_inp), &
                                   f_inp%i6_inf(j_inp), &
                                   f_inp%i7_inf(j_inp), &
                                   f_inp%i8_inf(j_inp), &
                                   f_inp%r1_inf(j_inp), &
                                   f_inp%r2_inf(j_inp), &
                                   f_inp%r3_inf(j_inp), &
                                   f_inp%r4_inf(j_inp), &
                                   f_inp%r5_inf(j_inp), &
                                   f_inp%r6_inf(j_inp), &
                                   f_inp%c1_inf(j_inp), &
                                   f_inp%c2_inf(j_inp), &
                                   f_inp%c3_inf(j_inp), &
                                   f_inp%c4_inf(j_inp) &
                                 )
      !
      !print'(" b j=",i8," n=",i8," i6=",i8," c4=",a)',&
      !j_inp,f_out%n0_gat,f_inp%i6_inf(j_inp),trim(f_inp%c4_inf(j_inp))
      !
    end do do_inp 
    !
    return
    !
  end subroutine fxdata_gath_to_info 
  !
  subroutine fxdata_gath_to_info_1 ( &
                                     f_inf, &
                                     i1_inf, i2_inf, i3_inf, i4_inf, &
                                     i5_inf, i6_inf, i7_inf, i8_inf, &
                                     r1_inf, r2_inf, r3_inf, &
                                     r4_inf, r5_inf, r6_inf, &
                                     c1_inf, c2_inf, c3_inf, c4_inf &
                                   )
    !
    ! add a single line of info to f_inf
    !
    type ( fxdata_info ),    pointer :: f_inf         ! input strucut
    integer,           intent(in   ) :: i1_inf          ! pe index 
    integer,           intent(in   ) :: i2_inf          ! shot index 
    integer,           intent(in   ) :: i3_inf          ! shot index 
    integer,           intent(in   ) :: i4_inf          ! shot index 
    integer,           intent(in   ) :: i5_inf          ! shot index 
    integer,           intent(in   ) :: i6_inf          ! first trace 
    integer,           intent(in   ) :: i7_inf          ! last trace 
    integer,           intent(in   ) :: i8_inf          ! shot index 
    real,              intent(in   ) :: r1_inf          ! shot x loc 
    real,              intent(in   ) :: r2_inf          ! shot y loc 
    real,              intent(in   ) :: r3_inf          ! shot y loc 
    real,              intent(in   ) :: r4_inf          ! shot y loc 
    real,              intent(in   ) :: r5_inf          ! shot y loc 
    real,              intent(in   ) :: r6_inf          ! shot y loc 
    character(len=*),  intent(in   ) :: c1_inf          ! date 
    character(len=*),  intent(in   ) :: c2_inf          ! time 
    character(len=*),  intent(in   ) :: c3_inf          ! job name 
    character(len=*),  intent(in   ) :: c4_inf          ! trin file 
    !
    logical                          :: l_duplicate   ! duplicate flag
    integer                          :: i_duplicate   ! duplicate index
    integer                          :: i_err
    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! allocate one more line o fmemory
    !
    i_err = 0
    !
    !print'(" aa1 fxdata_gath_to_info_1 c=",i8," n0_tot=",i8," n0_gat=",i8)', &
    !i_call, f_inf%n0_gat, f_inf%m0_gat
    !
    ! check to see if this is a duplicate
    !
    call fxdata_info_compare ( &
                               l_duplicate, i_duplicate, &
                               f_inf, &
                               i1_inf, i2_inf, i3_inf, i4_inf, &
                               i5_inf, i6_inf, i7_inf, i8_inf, &
                               r1_inf, r2_inf, r3_inf, &
                               r4_inf, r5_inf, r6_inf, &
                               c1_inf, c2_inf, c3_inf, c4_inf &
                             )
    !
    !print'(" aa2 fxdata_gath_to_info_1 c=",i8," n0_tot=",i8," n0_gat=",i8)', &
    !i_call, f_inf%n0_gat, f_inf%m0_gat
    !
    if ( l_duplicate ) return
    !
    ! if we need more space reallocate the arrays
    !
    !print'(" bef realloc n0_gat=",i8," size=",i8)', &
    !f_inf%n0_gat, size(f_inf%i1_inf,1) 
    !
    !print'(" aa3 fxdata_gath_to_info_1 c=",i8," n0_tot=",i8," n0_gat=",i8, & 
    !& " add=",i8)', &
    !i_call, f_inf%n0_gat, f_inf%m0_gat, fxcard_len_add_0
    !
    if ( size(f_inf%i1_inf,1) .lt. f_inf%n0_gat + 1 ) & 
    call fxdata_gath_reallocate ( f_inf, fxcard_len_add_0, i_err )
    !
    !print'(" aa4 fxdata_gath_to_info_1 c=",i8," n0_tot=",i8," n0_gat=",i8, &
    !& " err=", i8)', &
    !i_call, f_inf%n0_gat, f_inf%m0_gat, i_err
    !
    xxif_fxdata_gath_reallocate_err : if ( i_err .ne. 0 ) then
      !
      call pc_info( 'fxdata_gath_to_info_1: fxdata_gath_realo error')
      !
      return
      !
    end if xxif_fxdata_gath_reallocate_err 
    !
    ! increment the number of gathers counter
    !
    f_inf%n0_gat = f_inf%n0_gat + 1
    !
    !print'(" aa5 fxdata_gath_to_info_1 c=",i8," n0_tot=",i8," n0_gat=",i8)', &
    !i_call, f_inf%n0_gat, f_inf%m0_gat
    !
    !print'(" aft realloc n0_gat=",i8," size=",i8)', &
    !f_inf%n0_gat, size(f_inf%i1_inf,1) 
    !print'(" fxdata_gath_to_info_1 i6=",i8," c4=",a)',&
    !i6_inf,trim(c4_inf)
    !
    ! copy info
    !   
    f_inf%i1_inf(f_inf%n0_gat) = i1_inf
    f_inf%i2_inf(f_inf%n0_gat) = i2_inf
    f_inf%i3_inf(f_inf%n0_gat) = i3_inf
    f_inf%i4_inf(f_inf%n0_gat) = i4_inf
    f_inf%i5_inf(f_inf%n0_gat) = i5_inf
    f_inf%i6_inf(f_inf%n0_gat) = i6_inf
    f_inf%i7_inf(f_inf%n0_gat) = i7_inf
    f_inf%i8_inf(f_inf%n0_gat) = i8_inf
    !
    f_inf%r1_inf(f_inf%n0_gat) = r1_inf
    f_inf%r2_inf(f_inf%n0_gat) = r2_inf
    f_inf%r3_inf(f_inf%n0_gat) = r3_inf
    f_inf%r4_inf(f_inf%n0_gat) = r4_inf
    f_inf%r5_inf(f_inf%n0_gat) = r5_inf
    f_inf%r6_inf(f_inf%n0_gat) = r6_inf
    !
    f_inf%c1_inf(f_inf%n0_gat) = trim(adjustl(c1_inf))
    f_inf%c2_inf(f_inf%n0_gat) = trim(adjustl(c2_inf))
    f_inf%c3_inf(f_inf%n0_gat) = trim(adjustl(c3_inf))
    f_inf%c4_inf(f_inf%n0_gat) = trim(adjustl(c4_inf))
    !
    !print'(" fxdata_gath_to_info_1 n=",i8," i6=",i8," c4=",a)',&
    !f_inf%n0_gat, f_inf%i6_inf(f_inf%n0_gat), trim(f_inf%c4_inf(f_inf%n0_gat))
    !
    !print'(" aa6 fxdata_gath_to_info_1 c=",i8," n0_tot=",i8," n0_gat=",i8)', &
    !i_call, f_inf%n0_gat, f_inf%m0_gat
    !
    return
    !
  end subroutine fxdata_gath_to_info_1
  !
  subroutine fxdata_gath_delete ( f_inf, i0_gat )
    !
    ! delete a single gather from f_inf
    !
    type ( fxdata_info ),    pointer :: f_inf         ! input strucut
    integer,           intent(in   ) :: i0_gat       ! gather to delete
    !
    integer                          :: i1_gat
    integer                          :: i2_gat
    !
    if ( i0_gat .le. 0 .or. i0_gat .gt. f_inf%n0_gat ) return
    !
    ! shift gather i0_gat+1:n0_gat to i0_gat:n0_gat-1
    !
    i1_gat = i0_gat
    !
    do_gathers : do i2_gat = i0_gat+1 , f_inf%n0_gat
      !
      ! shift gather j2_gath to j1_gath
      !
      call fxdata_gath_copy ( i2_gat, f_inf, i1_gat, f_inf )
      !
      i1_gat = i2_gat
      !
    end do do_gathers 
    !
    ! zero the last gather
    !
    i1_gat = f_inf%n0_gat
    !
    f_inf%i1_inf(i1_gat) = 0
    f_inf%i2_inf(i1_gat) = 0
    f_inf%i3_inf(i1_gat) = 0
    f_inf%i4_inf(i1_gat) = 0
    f_inf%i5_inf(i1_gat) = 0
    f_inf%i6_inf(i1_gat) = 0
    f_inf%i7_inf(i1_gat) = 0
    f_inf%i8_inf(i1_gat) = 0
    !
    f_inf%r1_inf(i1_gat) = 0
    f_inf%r2_inf(i1_gat) = 0
    f_inf%r3_inf(i1_gat) = 0
    f_inf%r4_inf(i1_gat) = 0
    f_inf%r5_inf(i1_gat) = 0
    f_inf%r6_inf(i1_gat) = 0
    !
    f_inf%c1_inf(i1_gat) = ' '
    f_inf%c2_inf(i1_gat) = ' '
    f_inf%c3_inf(i1_gat) = ' '
    f_inf%c4_inf(i1_gat) = ' '
    !
    ! decrement the number of gathers
    !
    f_inf%n0_gat = f_inf%n0_gat - 1
    !
    return
    !
  end subroutine fxdata_gath_delete
  !
  subroutine fxdata_gath_copy ( i_inp, f_inp, i_out, f_out )
    !
    ! shift gather i_inp o ff_inp to gather i_out of f_out
    !
    integer,           intent(in   ) :: i_inp ! input  gather 
    type ( fxdata_info ),    pointer :: f_inp ! input  info structure
    integer,           intent(in   ) :: i_out ! output gather
    type ( fxdata_info ),    pointer :: f_out ! output info structure 
    !
    if ( i_inp .le. 0 .or. i_inp .gt. f_inp%n0_gat &
    .or. i_out .le. 0 .or. i_out .gt. f_out%n0_gat ) return
    !
    ! copy gather i_inp to f_inp to i_out of f_out
    !
    f_out%i1_inf(i_out) = f_inp%i1_inf(i_inp) 
    f_out%i2_inf(i_out) = f_inp%i2_inf(i_inp) 
    f_out%i3_inf(i_out) = f_inp%i3_inf(i_inp) 
    f_out%i4_inf(i_out) = f_inp%i4_inf(i_inp) 
    f_out%i5_inf(i_out) = f_inp%i5_inf(i_inp) 
    f_out%i6_inf(i_out) = f_inp%i6_inf(i_inp) 
    f_out%i7_inf(i_out) = f_inp%i7_inf(i_inp) 
    f_out%i8_inf(i_out) = f_inp%i8_inf(i_inp) 
    !
    f_out%r1_inf(i_out) = f_inp%r1_inf(i_inp) 
    f_out%r2_inf(i_out) = f_inp%r2_inf(i_inp) 
    f_out%r3_inf(i_out) = f_inp%r3_inf(i_inp) 
    f_out%r4_inf(i_out) = f_inp%r4_inf(i_inp) 
    f_out%r5_inf(i_out) = f_inp%r5_inf(i_inp) 
    f_out%r6_inf(i_out) = f_inp%r6_inf(i_inp) 
    !
    f_out%c1_inf(i_out) = f_inp%c1_inf(i_inp) 
    f_out%c2_inf(i_out) = f_inp%c2_inf(i_inp) 
    f_out%c3_inf(i_out) = f_inp%c3_inf(i_inp) 
    f_out%c4_inf(i_out) = f_inp%c4_inf(i_inp) 
    !
    return
    !
  end subroutine fxdata_gath_copy
  !
  subroutine fxdata_c4_to_path_trin ( f_inf, i_err )
    !
    ! set the path_trin array from the files in c4_inf
    !
    type ( fxdata_info ),    pointer :: f_inf           ! input strucut
    integer,           intent(inout) :: i_err          ! err 0=o.k. -1=error
    !
    integer                          :: j0_gat          ! gather index
    !
    i_err = 0
    !
    if ( .not. associated ( f_inf ) ) go to 998
    !
    ! set the path_trin array to zero
    !
    f_inf%n_path_trin = 0
    !
    ! add each file from c4_inf to path_trin
    !
    do_gathers : do j0_gat = 1 , f_inf%n0_gat
      !
      call fxdata_card_to_card ( &
      .true., f_inf%c4_inf(j0_gat), f_inf%n_path_trin, f_inf%path_trin, i_err )
      !
      if ( i_err .ne. 0 ) go to 997
      !
    end do do_gathers 
    !
1999 continue
    !
    if ( associated ( f_inf%p ) ) &
    call fxpar_check_worker_errors ( f_inf%p, i_err )
    !
    return
    !
997 continue
    !
    call pc_info ( &
    ' error in fxdata_c4_to_path_trin during fxdata_card_to_card ' )
    !
    go to 1999
    !
998 continue
    !
    call pc_info ( ' error in fxdata_c4_to_path_trin fxdata not allocated ' )
    !
    go to 1999
    !
999 continue
    !
    call pc_info ( ' error in fxdata_c4_to_path_trin fxdata not allocated ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_c4_to_path_trin 
  !
  subroutine fxdata_card_to_card  ( check_card, c_inp, n_out, c_out, i_err )
    !
    ! add string c_inp to array c_out
    !
    logical,           intent(in   ) :: check_card! keep dupl cards
    character(len=*),  intent(in   ) :: c_inp          ! input card
    integer,           intent(inout) :: n_out          ! num cards in c_out
    character(len=*),        pointer :: c_out(:)! input card
    integer,           intent(inout) :: i_err          ! err 0=o.k. -1=error
    !
    integer                          :: c_len          ! card lengtrh to test
    integer                          :: i_out          ! out card index
    logical                          :: add_inp        ! add c_inp flag
    !
    i_err = 0
    !
    if ( .not. associated ( c_out ) ) &
    n_out = 0
    !
    if ( .not. associated ( c_out ) ) &
    call memfun_all ( c_out, n_out, 'c_out', i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! check each component of c_out against c_inp
    !
    add_inp = .false.
    !
    c_len = min ( len(c_inp), len(c_out) )
    !
    xxif_check_card : if ( check_card ) then
      !
      do_out : do i_out = 1 , n_out
        !
        if ( string_upper_compare ( c_inp       (1:c_len), &
                                    c_out(i_out)(1:c_len) ) ) go to 1
        !
      end do do_out 
      !
    end if xxif_check_card 
    !
    add_inp = .true.
    !
  1 continue
    !
    ! add c_inp to c_out with realloc if needed
    !
    if ( add_inp ) &
    call fxdata_card_to_card_0 ( c_inp, n_out, c_out, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    return
    !
997 continue
    !
    call pc_info ( &
    ' error in fxdata_card_to_card during fxdata_card_to_card ' )
    !
    go to 999
    !
998 continue
    !
    call pc_info ( ' error in fxdata_card_to_card during memfun_all ' )
    !
    go to 999
    !
999 continue
    !
    call pc_info ( ' error in fxdata_card_to_card ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_card_to_card 
  !
  subroutine fxdata_card_to_card_0 ( c_inp, n_out, c_out, i_err )
    !
    ! add a card to c_out with realloc
    !
    character(len=*),  intent(in   ) :: c_inp ! input card
    integer,           intent(inout) :: n_out ! num cards in c_out
    character(len=*),        pointer :: c_out(:)! input card
    integer,           intent(inout) :: i_err          ! err 0=o.k. -1=error
    !
    i_err = 0
    !
    ! allocate more meory if needed
    !
    if ( n_out+1 .gt. size(c_out,1) ) &
    call mem_realloc ( c_out, n_out+10, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    n_out = n_out + 1
    !
    c_out ( n_out ) = c_inp
    !
    return
    !
998 continue
    !
    call pc_info ( &
    ' error in fxdata_card_to_card_0 during memory allocation ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_card_to_card_0 card=', c_inp )
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_card_to_card_0
  !
  subroutine fxdata_head_to_card ( f_inf, c_inf, i_err )
    !
    type ( fxdata_info ),    pointer :: f_inf          ! fxdata info structure
    type ( fxcard_struct ),  pointer :: c_inf          ! fxcard structure
    integer,           intent(inout) :: i_err          ! err 0=o.k. -1=error

    !
    type ( cardset_struct ), pointer :: f_cset         ! cardset 

    !
    i_err = 0
    !
    nullify ( f_cset )
    !
    call fxcard_cardset_create ( f_cset )
    !
    ! put the header parameters
    !
    if ( .not. associated ( c_inf ) ) go to 999
    !
    ! clear the fxcard structure
    !
    call fxcard_clear  ( c_inf )
    !
    f_inf%receiver_gather = .not. f_inf%source_gather 
    !
    call fxdata_info_condition ( f_inf )
    !
    call cardset_put_scalar ( f_cset, 'path_info',           f_inf%path_info )
    call cardset_put_scalar ( f_cset, 'path_trot',           f_inf%path_trot )
    call cardset_put_scalar ( f_cset, 'line_title',          f_inf%line_title)
    call cardset_put_scalar ( f_cset, 'line_index',          f_inf%line_index)
    call cardset_put_scalar ( f_cset, 'number_of_gathers',   f_inf%n0_gat    )
    call cardset_put_scalar ( f_cset, 'number_of_traces',    f_inf%n0_tot    )
    call cardset_put_scalar ( f_cset, 'min_traces_in_gather',f_inf%i8_inf_1  )
    call cardset_put_scalar ( f_cset, 'max_traces_in_gather',f_inf%i8_inf_2  )
    call cardset_put_scalar ( f_cset, 'source_gather',  f_inf%source_gather)
    call cardset_put_scalar ( f_cset, 'receiver_gather',f_inf%receiver_gather)
    call cardset_put_scalar ( f_cset, 'hdr_x',               f_inf%hdr_x    )
    call cardset_put_scalar ( f_cset, 'hdr_y',               f_inf%hdr_y    )
    call cardset_put_scalar ( f_cset, 'hdr_gat_x',           f_inf%hx_gat   )
    call cardset_put_scalar ( f_cset, 'hdr_gat_y',           f_inf%hy_gat   )
    call cardset_put_scalar ( f_cset, 'hdr_tig_x',           f_inf%hx_tig   )
    call cardset_put_scalar ( f_cset, 'hdr_tig_y',           f_inf%hy_tig   )
    call cardset_put_scalar ( f_cset, 'bin_x',               f_inf%bin_x    )
    call cardset_put_scalar ( f_cset, 'bin_y',               f_inf%bin_y    )
    call cardset_put_scalar ( f_cset, 'min_gat_x',           f_inf%r1_inf_1 )
    call cardset_put_scalar ( f_cset, 'max_gat_x',           f_inf%r1_inf_2 )
    call cardset_put_scalar ( f_cset, 'min_gat_y',           f_inf%r2_inf_1 )
    call cardset_put_scalar ( f_cset, 'max_gat_y',           f_inf%r2_inf_2 )
    call cardset_put_scalar ( f_cset, 'min_tig_x',           f_inf%r3_inf_1 )
    call cardset_put_scalar ( f_cset, 'max_tig_x',           f_inf%r4_inf_2 )
    call cardset_put_scalar ( f_cset, 'min_tig_y',           f_inf%r5_inf_1 )
    call cardset_put_scalar ( f_cset, 'max_tig_y',           f_inf%r6_inf_2 )
    call cardset_put_scalar ( f_cset, 'sort_select',         f_inf%sort_select )
    call cardset_put_scalar ( f_cset, 'keep_select',         f_inf%keep_select )
    call cardset_put_scalar ( f_cset, 'keep_min',            f_inf%keep_min    )
    call cardset_put_scalar ( f_cset, 'keep_max',            f_inf%keep_max    )
    call cardset_put_scalar ( f_cset, 'keep_skip',           f_inf%keep_skip   )
    call cardset_put_scalar ( f_cset, 'info_type',           f_inf%info_type   )
    !
    !print*,' bef cardset_put_array  n=',cardset_num_cards(f_cset)
    !
    call cardset_put_array  ( f_cset, 'path_trin', f_inf%path_trin, &
                                                   f_inf%n_path_trin )
    !
    !print'(" aft put_array n=",i8," p=",a)',&
    !f_inf%n_path_trin,trim(f_inf%path_trin(1))
    !
call cardset_put_array  ( f_cset, 'history', f_inf%c_history, f_inf%n_history )
    !
    !print*,' bef fxcard_cset_to_card n=',cardset_num_cards(f_cset)
    !
    call fxcard_cset_to_card ( c_inf, f_cset ) ! copy cardset cards 
    !
    ! add the data title card
    !
    call fxcard_add_card ( c_inf, fxdata_info_title_0 )
    !
    !print'(" b fxdata_head_to_card n_history=",i8)', f_inf%n_history
    !print'(" b fxdata_head_to_card i=",i8," c=",a)', &
    !( j_history, f_inf%c_history(j_history), j_history=1,f_inf%n_history )
    !
1999 continue
    !
    call fxpar_check_worker_errors ( f_inf%p, i_err )
    !
    return
    !
999 continue
    !
    call pc_info ( ' error in fxdata_head_to_card fxdata not allocated ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_head_to_card
  !
  subroutine fxdata_card_to_head ( f_inf, c_inf, i_err )
    !
    type ( fxdata_info ),    pointer :: f_inf            ! fxdata_info struct
    type ( fxcard_struct ),  pointer :: c_inf            ! fxcard structure
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    type ( fxcard_struct ),  pointer :: c_tmp            ! fxcard structure
    type ( cardset_struct ), pointer :: f_cset           ! cardset 
    character(len= 80)               :: crd_80           ! card image
    integer                          :: first_gather_card! first gat card index

    !
    i_err = 0
    !
    nullify ( f_cset )
    !
    nullify ( c_tmp )
    !
!print*,' top fxdata_card_to_head ' 
    if ( .not. associated ( c_inf ) ) go to 999
!print*,' aa1 fxdata_card_to_head ' 
    !
    ! copy the header cards from c_inf to c_tmp
    !
    first_gather_card = fxdata_first_gather_card ( c_inf ) 
    !
    !call fxcard_create ( 'fxdata_card_to_head', c_tmp )
    !
    call fxcard_card_to_card ( &
    .false., 1, first_gather_card-1, c_inf, c_tmp, i_err )
    !
    ! get the header parameters
    !
    call fxcard_card_to_cset ( c_tmp, f_cset ) ! copy cardset cards 
    !
if ( string_upper_compare ( f_inf%path_info, pathcheck_empty ) ) &
call cardset_get_scalar ( f_cset, 'path_info',     f_inf%path_info,    crd_80 )
call cardset_get_scalar ( f_cset, 'path_trot',     f_inf%path_trot,    crd_80 )
call cardset_get_scalar ( f_cset, 'line_title',    f_inf%line_title,   crd_80 )
call cardset_get_scalar ( f_cset, 'line_index',    f_inf%line_index,   crd_80 )
call cardset_get_scalar ( f_cset, 'number_of_gathers', f_inf%n0_gat,   crd_80 )
call cardset_get_scalar ( f_cset, 'number_of_traces',  f_inf%n0_tot,   crd_80 )
call cardset_get_scalar ( f_cset, 'min_traces_in_gather',f_inf%i8_inf_1,crd_80 )
call cardset_get_scalar ( f_cset, 'max_traces_in_gather',f_inf%i8_inf_2,crd_80 )
call cardset_get_scalar ( f_cset, 'source_gather',  f_inf%source_gather,crd_80 )
call cardset_get_scalar ( f_cset, 'receiver_gather',f_inf%receiver_gather, &
                                                                       crd_80 )
call cardset_get_scalar ( f_cset, 'hdr_x',          f_inf%hdr_x,       crd_80 )
call cardset_get_scalar ( f_cset, 'hdr_y',          f_inf%hdr_y,       crd_80 )
call cardset_get_scalar ( f_cset, 'hdr_gat_x',      f_inf%hx_gat,      crd_80 )
call cardset_get_scalar ( f_cset, 'hdr_gat_y',      f_inf%hy_gat,      crd_80 )
call cardset_get_scalar ( f_cset, 'hdr_tig_x',      f_inf%hx_tig,      crd_80 )
call cardset_get_scalar ( f_cset, 'hdr_tig_y',      f_inf%hy_tig,      crd_80 )
call cardset_get_scalar ( f_cset, 'bin_x',          f_inf%bin_x,       crd_80 )
call cardset_get_scalar ( f_cset, 'bin_y',          f_inf%bin_y,       crd_80 )
call cardset_get_scalar ( f_cset, 'min_gat_x',      f_inf%r1_inf_1,    crd_80 )
call cardset_get_scalar ( f_cset, 'max_gat_x',      f_inf%r1_inf_2,    crd_80 )
call cardset_get_scalar ( f_cset, 'min_gat_y',      f_inf%r2_inf_1,    crd_80 )
call cardset_get_scalar ( f_cset, 'max_gat_y',      f_inf%r2_inf_2,    crd_80 )
call cardset_get_scalar ( f_cset, 'min_tig_x',      f_inf%r3_inf_1,    crd_80 )
call cardset_get_scalar ( f_cset, 'max_tig_x',      f_inf%r4_inf_2,    crd_80 )
call cardset_get_scalar ( f_cset, 'min_tig_y',      f_inf%r5_inf_1,    crd_80 )
call cardset_get_scalar ( f_cset, 'max_tig_y',      f_inf%r6_inf_2,    crd_80 )
call cardset_get_scalar ( f_cset, 'min_tig_x',      f_inf%r3_inf_1,    crd_80 )
call cardset_get_scalar ( f_cset, 'max_tig_x',      f_inf%r4_inf_2,    crd_80 )
call cardset_get_scalar ( f_cset, 'min_tig_y',      f_inf%r5_inf_1,    crd_80 )
call cardset_get_scalar ( f_cset, 'max_tig_y',      f_inf%r6_inf_2,    crd_80 )
call cardset_get_scalar ( f_cset, 'sort_select',    f_inf%sort_select, crd_80 )
call cardset_get_scalar ( f_cset, 'keep_select',    f_inf%keep_select, crd_80 )
call cardset_get_scalar ( f_cset, 'keep_min',       f_inf%keep_min,    crd_80 )
call cardset_get_scalar ( f_cset, 'keep_max',       f_inf%keep_max,    crd_80 )
call cardset_get_scalar ( f_cset, 'keep_skip',      f_inf%keep_skip,   crd_80 )
call cardset_get_scalar ( f_cset, 'info_type',      f_inf%info_type,   crd_80 )
    !
    call cardset_alloc_array ( f_cset, &
    'path_trin', f_inf%path_trin, f_inf%n_path_trin, crd_80 )
    !
    call cardset_alloc_array ( f_cset, &
    'history', f_inf%c_history, f_inf%n_history, crd_80 )
    !
    !print'(" a fxdata_card_to_head n_history=",i8)', f_inf%n_history
    !print'(" a fxdata_card_to_head i=",i8," c=",a)', &
    !( j_history, f_inf%c_history(j_history), j_history=1,f_inf%n_history )
    !
    call cardset_delete ( f_cset )
    !
    f_inf%receiver_gather = .not. f_inf%source_gather 
    !
    if ( string_upper_compare ( f_inf%path_trot, pathcheck_empty ) &
    .and. f_inf%n_path_trin .ge. 1 ) &
    f_inf%path_trot = f_inf%path_trin(1)
    !
    f_inf%path_trot_0 = f_inf%path_trot ! trot file original
    !
!print*,' fxdata_card_to_head bef fxdata_info_condition '
!    call fxdata_info_condition ( f_inf )
!print*,' fxdata_card_to_head aft fxdata_info_condition '
    !
    !print'(" b fxdata_card_to_head n_history=",i8)', f_inf%n_history
    !print'(" b fxdata_card_to_head i=",i8," c=",a)', &
    !( j_history, f_inf%c_history(j_history), j_history=1,f_inf%n_history )
    !
1999 continue
    !
    call fxpar_check_worker_errors ( f_inf%p, i_err )
    !
    if (    associated ( c_tmp ) ) &
    call fxcard_delete ( c_tmp )
    !
    return
    !
999 continue
    !
    call pc_error ( ' error in fxdata_card_to_head fxdata not allocated ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_card_to_head
  !
  subroutine fxdata_gath_to_card ( f_inf, c_inf, i_err )
    !
    ! write the fxinfo structure from path_info
    !
    type ( fxdata_info ),    pointer :: f_inf            ! fxinfo structure
    type ( fxcard_struct ),  pointer :: c_inf            ! fxcard structure
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    integer                          :: i_card           ! card index
    !
    character(len=fxcard_card_len)   :: c_card          ! card image
    !
    i_err = 0
    !
    !print*,' fxdata_gath_to_card n=',f_inf%n0_gat
    !
    do_data_cards : do i_card = 1 , f_inf%n0_gat
      !
      !if ( fxpar_i_pel() .eq. 0 ) &
      !print'(" fxdata_gath_to_card i=",i8," i6=",i8," c4=",a)',&
      !i_card, f_inf%i6_inf(i_card), trim(f_inf%c4_inf(i_card))
      !
      call fxdata_gath_to_card_0 ( &
                                   c_inf, c_card, &
                                   f_inf%i1_inf(i_card), f_inf%i2_inf(i_card), &
                                   f_inf%i3_inf(i_card), f_inf%i4_inf(i_card), &
                                   f_inf%i5_inf(i_card), f_inf%i6_inf(i_card), &
                                   f_inf%i7_inf(i_card), f_inf%i8_inf(i_card), &
                                   f_inf%r1_inf(i_card), f_inf%r2_inf(i_card), &
                                   f_inf%r3_inf(i_card), f_inf%r4_inf(i_card), &
                                   f_inf%r5_inf(i_card), f_inf%r6_inf(i_card), &
                                   f_inf%c1_inf(i_card), f_inf%c2_inf(i_card), &
                                   f_inf%c3_inf(i_card), f_inf%c4_inf(i_card) &
                                 )
      !
      !if ( fxpar_i_pel() .eq. 0 ) &
      !print'(" fxdata_gath_to_card i=",i8," c=",a)',&
      !i_card, trim(c_card)
      !
    end do do_data_cards 
    !
    if ( i_err .ne. 0 ) go to 998
    !
1999 continue
    !
    call fxpar_check_worker_errors ( f_inf%p, i_err )
    !
    return
    !
998 continue
    !
    call pc_info ( ' error in fxdata_gath_to_card during xxx ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_gath_to_card ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_gath_to_card
  !
  subroutine fxdata_gath_to_card_0 ( &
                                     card_info, c_card, &
                                     i1_inf, i2_inf, i3_inf, i4_inf, &
                                     i5_inf, i6_inf, i7_inf, i8_inf, &
                                     r1_inf, r2_inf, r3_inf, r4_inf, &
                                     r5_inf, r6_inf, &
                                     c1_inf, c2_inf, c3_inf, c4_inf &
                                   )
    !
    ! read a single line of info
    !
    type ( fxcard_struct ),  pointer :: card_info       ! fxdata list 
    character(len=*),  intent(  out) :: c_card          ! card image
    integer,           intent(in   ) :: i1_inf          ! pe index 
    integer,           intent(in   ) :: i2_inf          ! shot index 
    integer,           intent(in   ) :: i3_inf          ! shot index 
    integer,           intent(in   ) :: i4_inf          ! shot index 
    integer,           intent(in   ) :: i5_inf          ! shot index 
    integer,           intent(in   ) :: i6_inf          ! first trace 
    integer,           intent(in   ) :: i7_inf          ! last trace 
    integer,           intent(in   ) :: i8_inf          ! shot index 
    real,              intent(in   ) :: r1_inf          ! shot x loc 
    real,              intent(in   ) :: r2_inf          ! shot y loc 
    real,              intent(in   ) :: r3_inf          ! shot y loc 
    real,              intent(in   ) :: r4_inf          ! shot y loc 
    real,              intent(in   ) :: r5_inf          ! shot y loc 
    real,              intent(in   ) :: r6_inf          ! shot y loc 
    character(len=*),  intent(in   ) :: c1_inf          ! date 
    character(len=*),  intent(in   ) :: c2_inf          ! time 
    character(len=*),  intent(in   ) :: c3_inf          ! job name 
    character(len=*),  intent(in   ) :: c4_inf          ! trin file 
    !
  !print'(" fxdata_gath_to_card_0 i6=",i8," c4=",a)',&
  !i6_inf, trim(c4_inf)
    !
    call fxdata_gath_to_card_1 ( &
                                 c_card, &
                                 i1_inf, i2_inf, i3_inf, i4_inf, &
                                 i5_inf, i6_inf, i7_inf, i8_inf, &
                                 r1_inf, r2_inf, r3_inf, r4_inf, &
                                 r5_inf, r6_inf, &
                                 c1_inf, c2_inf, c3_inf, c4_inf &
                               )
    !
  !print'(" fxdata_gath_to_card_0 c=",a)',trim(c_card)
    !
    call fxcard_add_card ( card_info, c_card )
  !print'(" fxdata_gath_to_card_0 n=",i8)',card_info%n_card
    !
    return
    !
  end subroutine fxdata_gath_to_card_0
  !
  subroutine fxdata_gath_to_card_1 ( &
                                     c_card, &
                                     i1_inf, i2_inf, i3_inf, i4_inf, &
                                     i5_inf, i6_inf, i7_inf, i8_inf, &
                                     r1_inf, r2_inf, r3_inf, r4_inf, &
                                     r5_inf, r6_inf, &
                                     c1_inf, c2_inf, c3_inf, c4_inf &
                                   )
    !
    ! read a single line of info
    !
    character(len=*),  intent(  out) :: c_card          ! card image
    integer,           intent(in   ) :: i1_inf          ! pe index 
    integer,           intent(in   ) :: i2_inf          ! shot index 
    integer,           intent(in   ) :: i3_inf          ! shot index 
    integer,           intent(in   ) :: i4_inf          ! shot index 
    integer,           intent(in   ) :: i5_inf          ! shot index 
    integer,           intent(in   ) :: i6_inf          ! first trace 
    integer,           intent(in   ) :: i7_inf          ! last trace 
    integer,           intent(in   ) :: i8_inf          ! shot index 
    real,              intent(in   ) :: r1_inf          ! shot x loc 
    real,              intent(in   ) :: r2_inf          ! shot y loc 
    real,              intent(in   ) :: r3_inf          ! shot y loc 
    real,              intent(in   ) :: r4_inf          ! shot y loc 
    real,              intent(in   ) :: r5_inf          ! shot y loc 
    real,              intent(in   ) :: r6_inf          ! shot y loc 
    character(len=*),  intent(in   ) :: c1_inf          ! date 
    character(len=*),  intent(in   ) :: c2_inf          ! time 
    character(len=*),  intent(in   ) :: c3_inf          ! job name 
    character(len=*),  intent(in   ) :: c4_inf          ! trin file 
    !
    c_card = ' '
    !
    !print'(" fxdata_gath_to_card_1 i6=",i8," c4=",a)',&
    !i6_inf, trim(c4_inf)
    !
    write ( c_card, fxdata_info_format_0 ) &
    trim(adjustl(c1_inf)), trim(adjustl(c2_inf)), trim(adjustl(c3_inf)), &
    i1_inf, i2_inf, i3_inf, i4_inf, &
    i5_inf, i6_inf, i7_inf, i8_inf, &
    r1_inf, r2_inf, r3_inf, r4_inf, &
    r5_inf, r6_inf, &
    trim(adjustl(c4_inf))
    !
    !print'(" fxdata_gath_to_card_1 c=",a)',trim(c_card)
    !
    return
    !
  end subroutine fxdata_gath_to_card_1
  !
  subroutine fxdata_card_to_gath ( f_inf, c_inf, i_err )
    !
    ! read the fxinfo structure from c_inf
    !
    type ( fxdata_info ),    pointer :: f_inf            ! fxinfo structure
    type ( fxcard_struct ),    pointer :: c_inf            ! fxcard structure
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! Local variables
    !

    character(len=fxcard_card_len)   :: c_card           ! card image
    integer                          :: i_card           ! card index
    integer                          :: j_card           ! card index
    integer                          :: first_gather_card! first gat card index
    !
    i_err = 0
    !
    ! allocate data memory
    !
    first_gather_card = fxdata_first_gather_card ( c_inf ) 
    !
    f_inf%n0_gat = fxcard_num_cards ( c_inf ) - first_gather_card + 1
    !
    !if ( pc_do_not_process_traces() ) &
    !print'(" top fxdata_card_to_gath n0_gat =", i8, &
    !& " fxcard_num_cards =", i8, " first_gather_card =", i8 )', &
    !f_inf%n0_gat, f_inf%n0_gat + first_gather_card - 1, first_gather_card 
    !
    !print*,' first_gather_card=',first_gather_card
    !print*,' n0_gat=',f_inf%n0_gat
    !
    call fxdata_gath_allocate ( f_inf, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! read data cards from file
    !
    !if ( pc_do_not_process_traces() ) &
    !print'(" qq3 n0_gat=",i8," i_err=",i8)',f_inf%n0_gat,i_err
    !
    do_data_cards : do i_card = 1 , f_inf%n0_gat
      !
      j_card = first_gather_card + i_card - 1
      !
      call fxdata_card_to_gath_0 ( &
                                   c_inf, c_card, j_card, &
                                   f_inf%i1_inf(i_card), f_inf%i2_inf(i_card), &
                                   f_inf%i3_inf(i_card), f_inf%i4_inf(i_card), &
                                   f_inf%i5_inf(i_card), f_inf%i6_inf(i_card), &
                                   f_inf%i7_inf(i_card), f_inf%i8_inf(i_card), &
                                   f_inf%r1_inf(i_card), f_inf%r2_inf(i_card), &
                                   f_inf%r3_inf(i_card), f_inf%r4_inf(i_card), &
                                   f_inf%r5_inf(i_card), f_inf%r6_inf(i_card), &
                                   f_inf%c1_inf(i_card), f_inf%c2_inf(i_card), &
                                   f_inf%c3_inf(i_card), f_inf%c4_inf(i_card) &
                                 )
      !
      !if ( pc_do_not_process_traces() .and. mod ( j_card, 100 ) .eq. 1 ) &
      !print'(" qq4 i=",i5," j=",i5," c=",a)',&
      !i_card,j_card,trim(c_card)
      !
      !if ( fxpar_i_pel() .eq. 0 ) &
      !write ( pc_get_lun(), '( &
      !& )' ) &
      !write ( pc_get_lun(), fxdata_info_format_0 ) &
      !f_inf%c1_inf(i_card), f_inf%c2_inf(i_card), f_inf%c3_inf(i_card), &
      !f_inf%i1_inf(i_card), f_inf%i2_inf(i_card), &
      !f_inf%i3_inf(i_card), f_inf%i4_inf(i_card), &
      !f_inf%i5_inf(i_card), f_inf%i6_inf(i_card), &
      !f_inf%i7_inf(i_card), f_inf%i8_inf(i_card), &
      !f_inf%r1_inf(i_card), f_inf%r2_inf(i_card), &
      !f_inf%r3_inf(i_card), f_inf%r4_inf(i_card), &
      !f_inf%r5_inf(i_card), f_inf%r6_inf(i_card), &
      !f_inf%c4_inf(i_card)
      !
    end do do_data_cards 
    !
    !if ( pc_do_not_process_traces() ) &
    !print'(" aa1 fxdata_card_to_gath n0_gat =", i8, &
    !& " fxcard_num_cards =", i8, " first_gather_card =", i8 )', &
    !f_inf%n0_gat, f_inf%n0_gat + first_gather_card - 1, first_gather_card 
    !
    ! set the min and max values for each column
    !
    call fxdata_info_condition ( f_inf )
    !
    !if ( pc_do_not_process_traces() ) &
    !print'(" end fxdata_card_to_gath n0_gat =", i8, &
    !& " fxcard_num_cards =", i8, " first_gather_card =", i8 )', &
    !f_inf%n0_gat, f_inf%n0_gat + first_gather_card - 1, first_gather_card 
    !
1999 continue
    !
    return
    !
997 continue
    !
    call pc_error ( ' error in fxdata_card_to_gath during read ' )
    !
    go to 999
    !
998 continue
    !
    call pc_error ( &
    ' error in fxdata_card_to_gath during fxdata_gath_allocate ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_card_to_gath ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_card_to_gath 
  !
  subroutine fxdata_card_to_gath_0 ( &
                                     card_info, c_card, i_card, &
                                     i1_inf, i2_inf, i3_inf, i4_inf, &
                                     i5_inf, i6_inf, i7_inf, i8_inf, &
                                     r1_inf, r2_inf, r3_inf, r4_inf, &
                                     r5_inf, r6_inf, &
                                     c1_inf, c2_inf, c3_inf, c4_inf &
                                   )
    !
    ! read a single line of info
    !
    type ( fxcard_struct ),  pointer :: card_info       ! fxdata list 
    character(len=*),  intent(inout) :: c_card          ! card image
    integer,           intent(in   ) :: i_card          ! pe index 
    integer,           intent(  out) :: i1_inf          ! pe index 
    integer,           intent(  out) :: i2_inf          ! shot index 
    integer,           intent(  out) :: i3_inf          ! shot index 
    integer,           intent(  out) :: i4_inf          ! shot index 
    integer,           intent(  out) :: i5_inf          ! shot index 
    integer,           intent(  out) :: i6_inf          ! first trace 
    integer,           intent(  out) :: i7_inf          ! last trace 
    integer,           intent(  out) :: i8_inf          ! shot index 
    real,              intent(  out) :: r1_inf          ! shot x loc 
    real,              intent(  out) :: r2_inf          ! shot y loc 
    real,              intent(  out) :: r3_inf          ! shot y loc 
    real,              intent(  out) :: r4_inf          ! shot y loc 
    real,              intent(  out) :: r5_inf          ! shot y loc 
    real,              intent(  out) :: r6_inf          ! shot y loc 
    character(len=*),  intent(  out) :: c1_inf          ! date 
    character(len=*),  intent(  out) :: c2_inf          ! time 
    character(len=*),  intent(  out) :: c3_inf          ! job name 
    character(len=*),  intent(  out) :: c4_inf          ! trin file 
    !
    character(len= 80)               :: crd_80            ! card image
    !
    call fxcard_get_card ( card_info, i_card, c_card, crd_80 )
    !
    call fxdata_card_to_gath_1 ( &
                                 c_card, &
                                 i1_inf, i2_inf, i3_inf, i4_inf, &
                                 i5_inf, i6_inf, i7_inf, i8_inf, &
                                 r1_inf, r2_inf, r3_inf, r4_inf, &
                                 r5_inf, r6_inf, &
                                 c1_inf, c2_inf, c3_inf, c4_inf &
                               )
    !
    return
    !
  end subroutine fxdata_card_to_gath_0
  !
  subroutine fxdata_card_to_gath_1 ( &
                                     c_card, &
                                     i1_inf, i2_inf, i3_inf, i4_inf, &
                                     i5_inf, i6_inf, i7_inf, i8_inf, &
                                     r1_inf, r2_inf, r3_inf, r4_inf, &
                                     r5_inf, r6_inf, &
                                     c1_inf, c2_inf, c3_inf, c4_inf &
                                   )
    !
    ! read a single line of info
    !
    character(len=*),  intent(in   ) :: c_card          ! card image
    integer,           intent(  out) :: i1_inf          ! pe index 
    integer,           intent(  out) :: i2_inf          ! shot index 
    integer,           intent(  out) :: i3_inf          ! shot index 
    integer,           intent(  out) :: i4_inf          ! shot index 
    integer,           intent(  out) :: i5_inf          ! shot index 
    integer,           intent(  out) :: i6_inf          ! first trace 
    integer,           intent(  out) :: i7_inf          ! last trace 
    integer,           intent(  out) :: i8_inf          ! shot index 
    real,              intent(  out) :: r1_inf          ! shot x loc 
    real,              intent(  out) :: r2_inf          ! shot y loc 
    real,              intent(  out) :: r3_inf          ! shot y loc 
    real,              intent(  out) :: r4_inf          ! shot y loc 
    real,              intent(  out) :: r5_inf          ! shot y loc 
    real,              intent(  out) :: r6_inf          ! shot y loc 
    character(len=*),  intent(  out) :: c1_inf          ! date 
    character(len=*),  intent(  out) :: c2_inf          ! time 
    character(len=*),  intent(  out) :: c3_inf          ! job name 
    character(len=*),  intent(  out) :: c4_inf          ! trin file 
    !
    integer                          :: i
    !
    read ( c_card, * ) &
    c1_inf, c2_inf, c3_inf, &
    i1_inf, i2_inf, i3_inf, i4_inf, &
    i5_inf, i6_inf, i7_inf, i8_inf, &
    r1_inf, r2_inf, r3_inf, r4_inf, &
    r5_inf, r6_inf
    !
    ! parse c4_inf with special care since the / character
    ! in unix file names acts as a field delimiter for
    ! list directed reads.
    i = index(trim(c_card),' ',.true.)
    !
    c4_inf = 'UNKNOWN'
    !
    if ( i .gt. 0 ) read(c_card(i+1:),'(a)') c4_inf
    !
    c1_inf = adjustl ( c1_inf )
    c2_inf = adjustl ( c2_inf )
    c3_inf = adjustl ( c3_inf )
    c4_inf = adjustl ( c4_inf )
    !
    return
    !
  end subroutine fxdata_card_to_gath_1
  !
  subroutine fxdata_hist_to_info_1 ( f_inf, h_card, i_err )
    !
    ! add history card, h_card, to the history array in a fxinfo structure
    !
    type ( fxdata_info ),    pointer :: f_inf            ! fxinfo structure
    character(len=*),  intent(in   ) :: h_card           ! history card
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    character(len=16)                :: job_name         ! job name
    character(len=16)                :: c_date           ! current date
    character(len=16)                :: c_time           ! current time
    character(len=fxcard_card_len)   :: crd_xxx          ! card image
    character(len=80)                :: tmpfmt        
    !
    i_err = 0
    !
    !print*,' fxdata_hist_to_info associated=',associated ( f_inf ) 
    !
    if ( .not. associated ( f_inf ) ) go to 998
    !
    !print'(" top fxdata_hist_to_info path_info=",a)',trim(f_inf%path_info)
    !print'(" top fxdata_hist_to_info n=",i8," c=",a)',&
    !f_inf%n_history, trim(h_card)
    !
    ! allocate more meory if needed
    !
    call string_date ( c_date ) ! current date
    !
    call string_time ( c_time ) ! current time
    !
    call pc_get_jdata ( 'jobname', job_name )
    !
    ! add a card for this shot to card_info_inp
    ! include the dat, time, original shot index, jobname and info file name
    !
    tmpfmt=fxdata_info_history_format
    write(crd_xxx,tmpfmt)trim(c_date),trim(c_time),trim(job_name),trim(h_card) 
    !
    call fxdata_card_to_card ( &
    .true., crd_xxx, f_inf%n_history, f_inf%c_history, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    !print*,' aa4 fxdata_hist_to_info '
    !
1999 continue
    !
    !print'(" end fxdata_hist_to_info n=",i8)',f_inf%n_history 
    !
    return
    !
997 continue
    !
    call pc_info ( &
    ' error in fxdata_hist_to_info_1 during fxdata_card_to_card ' )
    !
    go to 999
    !
    !
998 continue
    !
    call pc_info ( ' error in fxdata_hist_to_info_1 f_inf does not exist ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_hist_to_info_1 card=', h_card )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_hist_to_info_1
  !
  subroutine fxdata_hist_to_info_n ( f_inf, n_card, h_card, i_err )
    !
    ! add history card, h_card, to the history array in a fxinfo structure
    !
    type ( fxdata_info ),    pointer :: f_inf            ! fxinfo structure
    integer,           intent(in   ) :: n_card           ! num history card
    character(len=*),  intent(in   ) :: h_card(:)        ! history card
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    integer                          :: i_card           ! card index
    !
    i_err = 0
    !
    if ( .not. associated ( f_inf ) ) go to 998
    !
    ! add a card for this shot to card_info_inp
    ! include the dat, time, original shot index, jobname and info file name
    !
    do_i_card : do i_card = 1 , n_card
      !
      call fxdata_hist_to_info_1 ( f_inf, h_card(i_card), i_err )
      !
      if ( i_err .ne. 0 ) go to 997
      !
    end do do_i_card 
    !
1999 continue
    !
    return
    !
997 continue
    !
    call pc_info ( &
    ' error in fxdata_hist_to_info_n during fxdata_hist_to_info_1 ' )
    !
    go to 999
    !
    !
998 continue
    !
    call pc_info ( ' error in fxdata_hist_to_info_n f_inf does not exist ' )
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in fxdata_hist_to_info_n card=', h_card(1) )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_hist_to_info_n
  !
  integer function fxdata_first_gather_card ( card_info )
    !
    ! find the card index of the first shot card
    ! by looking for the header card
    !
    type ( fxcard_struct ),       pointer :: card_info       ! fxcard structure
    !
    integer                             :: i_card          ! card index
    integer                             :: j_card          ! card index
    !
    character(len=fxcard_card_len)     :: crd_xxx          ! card image
    character(len= 80)                  :: crd_80          ! card image
    !
    ! check each card
    !
    j_card = 1
    !
    do_cards : do i_card = 1 , fxcard_num_cards ( card_info )
      !
      call fxcard_get_card ( card_info, i_card,  crd_xxx, crd_80 )
      !
      j_card = i_card + 1
      !
      !print'(" j_card=",i8," c=",a)',j_card,trim(crd_xxx)
      !
      if ( string_upper_compare ( crd_xxx, fxdata_info_title_0 ) ) go to 1
      if ( string_upper_compare ( crd_xxx, fxdata_info_title_1 ) ) go to 1
      !
    end do do_cards 
    !
  1 continue
    !
    fxdata_first_gather_card = j_card 
    !
    !print'(" fxdata_num_cards=",i8," fxdata_first_gather_card=",i8)', &
    !fxcard_num_cards ( card_info ), fxdata_first_gather_card 
    !
    return
    !
  end function fxdata_first_gather_card
  !
  integer function fxdata_number_of_gathers ( card_info )
    !
    ! get the number of shots by counting cards
    !
    type ( fxcard_struct ),       pointer :: card_info       ! fxcard structure
    !


    !
    ! check each card
    !
    fxdata_number_of_gathers = max ( 0, fxcard_num_cards ( card_info ) - &
                                fxdata_first_gather_card ( card_info ) + 1 )
    !
    return
    !
  end function fxdata_number_of_gathers 
  !
  subroutine fxdata_gath_in_file ( &
                                   p, time_lock, &
                                   path_info, c4_inf, &
                                   i6_inf, i7_inf, &
                                   r1_inf, r2_inf, &
                                   r3_inf, r4_inf, &
                                   r5_inf, r6_inf, &
                                   gather_in_file, &
                                   mig_type,      &
                                   i_err &
                                 )
    !
    ! set image shot to true if this shot does not yet exist in the output file
    ! 
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    integer,           intent(in   ) :: time_lock        ! file lock time sec
    character(len=*),  intent(in   ) :: path_info        ! info  file name
    character(len=*),  intent(in   ) :: c4_inf           ! trin file name
    integer,           intent(in   ) :: i6_inf           ! first trace in shot
    integer,           intent(in   ) :: i7_inf           ! last trace in shot
    real,              intent(in   ) :: r1_inf           ! src x loc
    real,              intent(in   ) :: r2_inf           ! src y loc
    real,              intent(in   ) :: r3_inf           ! rec x loc min
    real,              intent(in   ) :: r4_inf           ! rec x loc max
    real,              intent(in   ) :: r5_inf           ! rec y loc min
    real,              intent(in   ) :: r6_inf           ! rec y loc max
    logical,           intent(inout) :: gather_in_file   ! gather in file flag
    character(len=*),  intent(in   ) :: mig_type         ! migration type
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !
    type ( fxdata_info ),    pointer :: f_inf       ! fxdata info struct
    !
    integer                          :: j_err            ! err 0=o.k. -1=error

    logical                          :: is_locked
    !
    nullify (f_inf) ! jpa
    !
    gather_in_file = .false.
    is_locked      = .false.
    !
    ! lock the files
    !
    call fxcard_file_lock ( p, path_info, time_lock, i_err  ) 
    !
    if ( i_err .ne. 0 ) go to 998
    is_locked      = .true.
    !
    ! read the info into f_inf
    !
    call fxdata_file_to_info ( path_info, f_inf, p, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    xxif_mig_type: &
    if ( .not. string_upper_compare ( mig_type,  'PLANE_WAVE') &
   .and. .not. string_upper_compare ( mig_type,  'CYLINDRICAL')) then
      !
      call fxdata_gath_in_info ( &
                                 f_inf, &
                                 c4_inf, &
                                 i6_inf, i7_inf, &
                                 r1_inf, r2_inf, &
                                 r3_inf, r4_inf, &
                                 r5_inf, r6_inf, &
                                 gather_in_file &
                                )
      ! 
    else xxif_mig_type
      !
      call fxdata_gath_in_info_pw ( &
                                    f_inf, &
                                    r1_inf, r2_inf, &
                                    gather_in_file &
                                  )
      !
    end if xxif_mig_type
    !
1999 continue
    !
    ! come to here to share error info
    !
    ! broadcast the error flag from the root pe to others in a group
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !
    call fxdata_info_delete ( f_inf )
    !
    j_err = 0
    !
    ! attempt to unlock only if the lock above succeeded
    if(is_locked) &
     call fxcard_file_unlock ( p, path_info, j_err  ) 
    !
    if ( j_err .ne. 0 ) go to 996
    !
    return
    !
996 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_gath_in_file pe= ", i8, &
    & " during fxcard_file_unlock " &
    & )' ) &
    fxpar_i_pel()
    !
    return
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_gath_in_file pe= ", i8, &
    & " during fxdata_file_to_info " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_gath_in_file pe= ", i8, &
    & " during fxcard_file_lock " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_gath_in_file pe= ", i8 &
    & )' ) &
    fxpar_i_pel()
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_gath_in_file 
  !
  subroutine fxdata_gath_in_info ( &
                                   f_inf, &
                                   c4_inf, &
                                   i6_inf, i7_inf, &
                                   r1_inf, r2_inf, &
                                   r3_inf, r4_inf, &
                                   r5_inf, r6_inf, &
                                   gather_in_info &
                                 )
    !
    ! set image shot to true if this shot does not yet exist in the output file
    ! 
    type ( fxdata_info ),    pointer :: f_inf            ! fxdata info struct
    character(len=*),  intent(in   ) :: c4_inf           ! trin file name
    integer,           intent(in   ) :: i6_inf           ! first trace in shot
    integer,           intent(in   ) :: i7_inf           ! last trace in shot
    real,              intent(in   ) :: r1_inf           ! src x loc
    real,              intent(in   ) :: r2_inf           ! src y loc
    real,              intent(in   ) :: r3_inf           ! rec x loc min
    real,              intent(in   ) :: r4_inf           ! rec x loc max
    real,              intent(in   ) :: r5_inf           ! rec y loc min
    real,              intent(in   ) :: r6_inf           ! rec y loc max
    logical,           intent(inout) :: gather_in_info   ! gather in file flag
    !
    ! local variables
    !
    integer                          :: j0_gat
    !
    gather_in_info = .false.
    !
    do_j0_gat : do j0_gat = 1 , f_inf%n0_gat 
      !
      ! if the input  first trace,       i6_inf is the same 
      ! as the gather first trace, f_inf%i6_inf(j0_gat),
      ! and 
      !    the input  trace file name,       c4_inf, is the same
      ! as the gather trace file name, f_inf%c4_inf(j0_gat)
      ! 
      ! or-
      ! 
      ! the distance from the input  x position,       r1_inf, 
      !              to   the gather x position, f_inf%r1_inf(j0_gat), 
      ! is less than the x bin size, f_inf%bin_x*.5 
      ! and  
      ! the distance from the input  y position,       r2_inf, 
      !              to   the gather y position, f_inf%r2_inf(j0_gat), 
      ! is less than the y bin size, f_inf%bin_y*.5 
      ! 
      ! this will be considered the same gather
      !
      xxif_same_gather_and_file : &
      if ( &
          ( &
                                  i6_inf .eq. f_inf%i6_inf(j0_gat) &
     .and. string_upper_compare ( c4_inf,     f_inf%c4_inf(j0_gat) ) &
          ) &
     .or. &
          ( &
            abs ( r1_inf - f_inf%r1_inf(j0_gat) ) .lt. f_inf%bin_x*.5 &
     .and.  abs ( r2_inf - f_inf%r2_inf(j0_gat) ) .lt. f_inf%bin_y*.5 &
     .and.  abs ( r3_inf - f_inf%r3_inf(j0_gat) ) .lt. f_inf%bin_x*.5 &
     .and.  abs ( r4_inf - f_inf%r4_inf(j0_gat) ) .lt. f_inf%bin_x*.5 &
     .and.  abs ( r5_inf - f_inf%r5_inf(j0_gat) ) .lt. f_inf%bin_y*.5 &
     .and.  abs ( r6_inf - f_inf%r6_inf(j0_gat) ) .lt. f_inf%bin_y*.5 &
          ) &
         ) then
        !
        gather_in_info = .true.
        !
        go to 1
        !
      end if xxif_same_gather_and_file 
      !
    end do do_j0_gat 
    !
  1 continue
    !
    return
    !
  end subroutine fxdata_gath_in_info  !
  !
  subroutine fxdata_gath_in_info_pw ( &
                                      f_inf, &
                                      r1_inf, r2_inf, &
                                      gather_in_info &
                                    )
    !
    ! set image shot to true if this shot does not yet exist in the output file
    ! 
    type ( fxdata_info ),    pointer :: f_inf            ! fxdata info struct
    real,              intent(in   ) :: r1_inf           ! src x loc
    real,              intent(in   ) :: r2_inf           ! src y loc
    logical,           intent(inout) :: gather_in_info   ! gather in file flag
    !
    ! local variables
    !
    integer                          :: j0_gat
    !
    gather_in_info = .false.
    !
    do_j0_gat : do j0_gat = 1 , f_inf%n0_gat 
      ! 
      ! the distance from the input  x position,       r1_inf, 
      !              to   the gather x position, f_inf%r1_inf(j0_gat), 
      ! is less than the x bin size, f_inf%bin_x*.5 
      ! and  
      ! the distance from the input  y position,       r2_inf, 
      !              to   the gather y position, f_inf%r2_inf(j0_gat), 
      ! is less than the y bin size, f_inf%bin_y*.5 
      ! 
      ! this will be considered the same gather
      !
      xxif_same_gather_and_file : &
      if ( &
          ( &
            abs ( r1_inf - f_inf%r1_inf(j0_gat) ) .lt. f_inf%bin_x*.5 &
     .and.  abs ( r2_inf - f_inf%r2_inf(j0_gat) ) .lt. f_inf%bin_y*.5  &
          ) &
         ) then
        !
        gather_in_info = .true.
        !
        go to 1
        !
      end if xxif_same_gather_and_file 
      !
    end do do_j0_gat 
    !
  1 continue
    !
    return
    !
  end subroutine fxdata_gath_in_info_pw  
  !
  subroutine fxdata_trc_init ( &
                               p, path_trin, io_mode, &
                               n_zero, nr_max, nr_inp, nh_inp, &
                               nt_inp, t0_inp, dt_inp, &
                               i_err &
                             )
    !
    ! initalize the trot file
    ! 
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    character(len=*),  intent(in   ) :: path_trin        ! trcio file name
    character(len=*),  intent(in   ) :: io_mode          ! io mode
    integer,           intent(in   ) :: n_zero           ! num trace to zero
    integer,           intent(in   ) :: nr_max           ! max trace in file
    integer,           intent(inout) :: nr_inp           ! num trace in file
    integer,           intent(inout) :: nh_inp           ! num trace head
    integer,           intent(inout) :: nt_inp           ! num trace time
    real,              intent(inout) :: t0_inp           ! min trace time
    real,              intent(inout) :: dt_inp           ! inc trace time
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !
    character(len=16)                :: c_time           ! current time
    integer                          :: j_err            ! local error
    integer                          :: i_zero           ! trace index
    !
    double precision                 :: hd_inp ( nh_inp ) ! header
    real                             :: tr_inp ( nt_inp ) ! traces
    !
    type ( trcio_struct ),   pointer :: trcio_obj        ! out trcio structure
    !
    nullify (trcio_obj) ! jpa
    !
    ! initialize the error flag
    !
    !print'(" top fxdata_trc_init p=",i5," c=",g12.6," path_trin=",a)',&
    !fxpar_i_pel(), getsys_seconds(), path_trin
    !
    i_err = 0
    !
    nr_inp = 0
    nr_inp = n_zero
    !
    ! open the input data file
    !
    !print'(" bq1 n_zero=",i8," nr_max=",i8," nr_inp=",i8," file=",a)',&
    !n_zero, nr_max, nr_inp, trim(path_trin)
    !
    call fxdata_trc_open ( &
                           p, trcio_obj, path_trin, io_mode, &
                           nr_max, nr_inp, nh_inp, nt_inp, t0_inp, dt_inp, &
                           i_err &
                         )
    !
!print'(" bq2 n_zero=",i8," nr_max=",i8," nr_inp=",i8," err=",i8," file=",a)',&
!n_zero, nr_max, nr_inp, i_err, trim(path_trin)
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! write zero traces
    !
    do_zero : do i_zero = 1 , n_zero
      !
      ! initialize the header and trace values
      !
      call string_time ( c_time ) ! current time
      !
      !if ( p%group_root .and. mod ( i_zero, 10000) .eq. 1) &
      !print'(" fxdata_trc_init zeroing ",a16,1x,i8," / ",i8," f=",a)',&
      !trim(c_time), i_zero, n_zero, trim(path_trin)
      !
      hd_inp ( 1:nh_inp ) = 0.
      tr_inp ( 1:nt_inp ) = 0.
      !
      !  write this trace on the group_root pe
      !
      call fxdata_trc_write ( &
                              p, trcio_obj, i_zero, .false., &
                              nh_inp, hd_inp, &
                              nt_inp, tr_inp, &
                              i_err &
                            )
      !
      if ( i_err .lt. 0 ) go to 997
      !
    end do do_zero
    !
    ! get the number of traces on this file on the group root pe
    !
    !if ( p%group_root ) &
    if ( n_zero .eq. 0 .and. p%group_root ) &
    nr_inp = fxdata_get_trace_count ( trcio_obj )
    !
    !print'(" bq3 n_zero=",i8," nr_max=",i8," nr_inp=",i8," file=",a)',&
    !n_zero, nr_max, nr_inp, trim(path_trin)
    !
    ! come to here to share error info
    !
1999 continue
    !
    ! broadcast the number of traces on this file
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, nr_inp )
    !
    ! broadcast the error flag from the root pe to others in a group
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !
    !  close the input trace file, yes flush, no remove
    !
    call fxdata_trc_close ( p, trcio_obj, .false., j_err )
    !
    if ( j_err .lt. 0 ) &
    call pc_info (' error in fxdata_trc_init during fxdata_trc_close')
    !
    return
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_init pe= ", i8, &
    & /,  " during fxdata_trc_write  " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_init pe= ", i8, &
    & /,  " during fxdata_trc_open " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_init pe= ", i8 &
    & )' ) &
    fxpar_i_pel()
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_trc_init  !
  !
  subroutine fxdata_trc_open ( &
                               p, trcio_obj, path_trot, io_mode, &
                               nr_max, nr_inp, nh_inp, nt_inp, t0_inp, dt_inp, &
                               i_err &
                             )
    !
    ! open a trcio file
    ! 
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    type ( trcio_struct ),   pointer :: trcio_obj        ! out trcio structure
    character(len=*),  intent(in   ) :: path_trot        ! trcio file name
    character(len=*),  intent(in   ) :: io_mode          ! trcio io mode
    integer,           intent(in   ) :: nr_max           ! max traces in file
    integer,           intent(inout) :: nr_inp           ! num traces in file
    integer,           intent(inout) :: nh_inp           ! num trace head
    integer,           intent(inout) :: nt_inp           ! num trace time
    real,              intent(inout) :: t0_inp           ! min trace time
    real,              intent(inout) :: dt_inp           ! inc trace time
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !
    integer                          :: cio_status       ! cio_status

    !
    integer                          :: nh_tmp           ! num trace header 
    integer                          :: nt_tmp           ! num trace time
    real                             :: t0_tmp           ! min trace time
    real                             :: dt_tmp           ! inc trace time
    !
    integer                          :: nh_fil           ! num trace header 
    integer                          :: nt_fil           ! num trace time
    real                             :: t0_fil           ! min trace time
    real                             :: dt_fil           ! inc trace time
    !
    real                             :: file_size_word_r

    real                             :: two_giga_words_r
    integer                          :: file_extent_byte
    integer                          :: file_extent_word
    integer                          :: file_size_word
    integer                          :: two_giga_bytes   
    integer                          :: two_giga_words    


    !
    two_giga_bytes = 2147482624
    !
    two_giga_words = two_giga_bytes / sizeof(1)
    !
    two_giga_words_r = two_giga_words 
    !
    file_size_word_r = ( float ( nr_max ) + 5. ) &
                       * float( ( nh_inp * 2 + nt_inp ) )
    !
    file_size_word   = ( nr_max + 5 ) * ( nh_inp * 2 + nt_inp )
    !
    !print'(" aa1 nr_max=",i12," nh_inp=",i4," nt_inp=",i6,&
    !& " file_size_word=",i12)', &
    !nr_max, nh_inp, nt_inp, file_size_word 
    !
    !print'(" aa1 file_size_word=",i12, &
    !& " file_size_word_r=",g12.6, " two_giga_words_r=",g12.6)', &
    !file_size_word, file_size_word_r, two_giga_words_r 
    !
    if ( file_size_word   .le. 0 &
    .or. file_size_word_r .gt. two_giga_words_r ) &
    file_size_word = two_giga_words
    !
    !print'(" aa2 nr_max=",i12," nh_inp=",i4," nt_inp=",i6,&
    !& " file_size_word=",i12)', &
    !nr_max, nh_inp, nt_inp, file_size_word 
    !
    xxif_too_big : if ( file_size_word   .gt. two_giga_words &
                   .or. file_size_word_r .gt. two_giga_words_r ) then
      !
      file_extent_word = two_giga_words
      !
    else xxif_too_big 
      !
      file_extent_word = file_size_word
      !
    end if xxif_too_big 
    !
    file_extent_byte = sizeof(1) * file_extent_word
    !
    !print'(" aa3 file_extent_word=",i12, " file_extent_byte=",i12 )', &
    !file_extent_word, file_extent_byte 
    !
    !if ( nr_max .eq. 0 ) file_extent_byte = 250000000
    !
    if ( nr_max .eq. 0 ) file_extent_byte = two_giga_bytes 
    !
    !print'(" aa4 file_extent_word=",i12, " file_extent_byte=",i12 )', &
    !file_extent_word, file_extent_byte 
    !
    file_extent_word = file_extent_byte / sizeof(1) 
    !
    !print'(" aa5 file_extent_word=",i12, " file_extent_byte=",i12 )', &
    !file_extent_word, file_extent_byte 
    !
    !print'(" aq0 nr_max=",i12," nr_inp=",i12," nh_inp=",i6," nt_inp=",i6,&
    !&/," aq0 byte=",i12," word=",i12," name=",a)',&
    !nr_max, nr_inp, nh_inp, nt_inp, &
    !file_extent_byte, file_extent_word, trim(path_trot)
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    !  replace the current globals with the input ones.
    !
    call pc_get_global ( 'NWIH' , nh_tmp )
    !
    call pc_put_global ( 'NWIH' , nh_inp )
    !
    call pc_get_global ( 'NWIH' , nh_fil )
    !
    call timeglob_get ( nt_tmp, t0_tmp, dt_tmp )
    !
    call timeglob_put ( nt_inp, t0_inp, dt_inp )
    !
    call timeglob_get ( nt_fil, t0_fil, dt_fil )
    !
    ! open the input data file on the root pe
    !
    xxif_root_pe : if ( p%group_root ) then
      !
      !print'(" fxdata_trc_open nr_max=",i12," nr_inp=",i12," byte=",i12,&
      !&" word=",i12," name=",a)',&
      !nr_max, nr_inp, file_extent_byte, file_extent_word, trim(path_trot)
      !
      !if ( nr_max .gt. 0 .and. file_extent_byte .lt. 250000000 + 1 ) stop
      !
      ! set the file extent size
      !
      cio_status = cio_set_file_ext_size ( file_extent_byte )
      !
      if ( cio_status .ne. cio_ok ) go to 998
      !
      !print'(" a fxdata_trc_open nr_max=",i12," nr_inp=",i12," byte=",i12,&
      !&" word=",i12," name=",a)',&
      !nr_max, nr_inp, file_extent_byte, file_extent_word, trim(path_trot)
      !
      !print'(" a fxdata_trc_open cio_ext_size=",i12)', cio_extsize()
      !
      ! reserve the disk space
      !!  call cio_set_file_space_commit(isw)
!    Purpose:  Pre-allocate file size for next file to open.
!              if isw = PREALLOCATE_FILE_SPACE_ENABLED  preallocate,
!                       PREALLOCATE_FILE_SPACE_DISABLED don't preallocate
!                       PREALLOCATE_FILE_SPACE_DEFAULT  preallacate cpstemp/data
!    note cpsdata file are automatically preallocated
!integer,parameter,public :: PREALLOCATE_FILE_SPACE_DISABLED =-1
!                           don't preallocate
!integer,parameter,public :: PREALLOCATE_FILE_SPACE_ENABLED  = 1
!                           preallocate space
!integer,parameter,public :: PREALLOCATE_FILE_SPACE_DEFAULT  = 0
!                           preallocate if file is cpstemp or cpsdata
      !
      !call cio_set_file_space_commit(1)
      !
      !call cio_set_file_space_commit(0)
      !
      call cio_set_file_space_commit(PREALLOCATE_FILE_SPACE_DEFAULT)
      !
      ! open the file
      !
      trcio_obj => trcio_open ( &
                                filename = path_trot, &
                                io_mode  = io_mode,   &
                                nwih     = nh_fil,    &
                                ndpt     = nt_fil,    &
                                srate    = dt_fil,    &
                                strt_val = t0_inp     &
                              )
      !
      if ( .not. associated ( trcio_obj ) ) i_err = -1
      !
      if ( i_err .ne. 0 ) go to 997
      !
      !trcio_unit = trcio_get_lun ( trcio_obj )
      !
      !print'(" aq2 trcio_unit=",i12)', trcio_unit
      !
      !trcio_size = cio_get_file_ext_size ( trcio_unit )
      !
      !print'(" aq2 trcio_size=",i12)', trcio_size
      !
      nr_inp = fxdata_get_trace_count ( trcio_obj )
      !
      !print'(" aq2 nr_max=",i12," nr_inp=",i12,&
      !& " byte=",i12," word=",i12," name=",a)',&
      !nr_max, nr_inp, file_extent_byte, file_extent_word, trim(path_trot)
      !
    end if xxif_root_pe
    !
    ! come to here to share error info
    !
1999 continue
    !
    ! replace the global time values
    !
    call timeglob_put ( nt_tmp, t0_tmp, dt_tmp )
    !
    ! broadcast the error flag from the root pe to others in a group
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, nr_inp )
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !
    return
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_open pe= ", i8, &
    & /,  " during trcio_open path_trot=", a &
    & )' ) &
    fxpar_i_pel(), trim ( path_trot )
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_open pe= ", i8, " setting file extent size.", &
    & /,  " during cio_set_file_ext_size file_extent_byte=", i8 &
    & )' ) &
    fxpar_i_pel(), file_extent_byte
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_open pe=", i8 &
    & )' ) &
    fxpar_i_pel()
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_trc_open 
  !
  subroutine fxdata_read_list_pw ( &
                                   path_trot, io_mode, file_list, &
                                   num_file_tot, num_file_sect, &
                                   num_trc_file, num_trc_sect, &
                                   pw_domain, p0_scl, &
                                   nt_inp, nt_fft, dw_fft,  &
                                   freq_min, if_min, freq_max, if_max, &
                                   i_err &
                                 )
    !
    ! read list file
    ! 
    character(len=*),  intent(in   ) :: path_trot        ! trcio file name
    character(len=*),  intent(in   ) :: io_mode          ! trcio io mode
    character(len=*),        pointer :: file_list(:, :)
    integer,           intent(inout) :: num_file_tot
    integer,           intent(inout) :: num_file_sect  ! num of files /section
    integer,           intent(inout) :: num_trc_sect 
    integer,                 pointer :: num_trc_file(:, :)
    character(len=*),  intent(inout) :: pw_domain
    integer,           intent(inout) :: nt_inp
    integer,           intent(inout) :: nt_fft
    real,              intent(inout) :: dw_fft
    real,              intent(inout) :: freq_min
    real,              intent(inout) :: freq_max
    real,              intent(inout) :: p0_scl
    integer,           intent(inout) :: if_min
    integer,           intent(inout) :: if_max
    !
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    integer                          :: file_list_id
    !
    ! local variables
    !


    !
    character(len=fxcard_card_len)   :: card
    character(len=filename_length)   :: file_name 
    integer                          :: lun
    integer                          :: num_trc

    integer                          :: num_sects
    integer                          :: i_f
    integer                          :: i_s
    !
    !print'(" top fxdata_read_list_pw p=",i4," path_trot=",a)', &
    !fxpar_i_pel(), trim(path_trot)
    !
    ! open the list file
    !
    file_list_id = cio_fopen ( path_trot, io_mode )
    !
    ! read the header file 
    ! 
    call fxdata_read_head_pw ( &
                               file_list_id,  &
                               num_file_tot, num_file_sect, &
                               pw_domain, p0_scl, &
                               nt_inp, nt_fft, dw_fft, &
                               freq_min, if_min, freq_max, if_max &
                             ) 
    !
    i_err = 0
    !
    ! allocate the buffer
    !
    num_sects = num_file_tot / num_file_sect
    !
    call memfun_all ( num_trc_file, num_file_sect, num_sects, &
                     'num_trc_file', i_err )
    !
    allocate ( file_list(num_file_sect, num_sects), stat = i_err )
    !
    !call fxcard_file_to_array ( &
    !                            o%p, 'path_trin', o%list_trin, &
    !                            o%n_path_trin, o%path_trin, &
    !                            i_err &
    !                          )
    !
    xxif_allocate_err : if (i_err .ne. 0) then
      !
      call pc_info( 'fxdata_read_list_pw: file_list allocate error ')
      !
      print*, 'requested memory: ', num_file_sect, num_sects
      !
      return
      !
    end if xxif_allocate_err 
    !
    ! open the input data file on the root pe
    !
    !xxif_group_root : if ( p%group_root ) then
    !
    do_i_s : do i_s = 1, num_sects
       !
       do_i_f : do i_f = 1, num_file_sect
       ! 
       lun = cio_fgetline(card, fxcard_card_len, file_list_id ) 
       !
       ! decode the card
       ! 
       read ( card, fxdata_pw_format_5 ) num_trc, file_name 
       !
       !print'(" aa1 card=",a)', trim(card)
       !
       num_trc_file(i_f, i_s ) = num_trc
       !
          file_list(i_f, i_s ) = file_name
       !
      end do do_i_f  
      !
    end do do_i_s 
    !
    !end if xxif_group_root 
    !
    return
    !
  end subroutine fxdata_read_list_pw
  !
  subroutine fxdata_trc_open_pw ( &
                                  p, f_trcio, file_inps, io_mode, &
                                  nr_max, nr_inp, nh_inp, &
                                  nt_inp,t0_inp, dt_inp, &
                                  num_file_sect, i_sect, &
                                  num_trc_sect, num_trc_file,  &
                                  i_err &
                                )
    !
    ! open a trcio file for plane wave migration 
    ! 
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    type ( fxdata_filestr ), pointer :: f_trcio(:)       ! input trcio structure
    character(len=*),  intent(in   ) :: file_inps(:, :)  ! trcio file name
    character(len=*),  intent(in   ) :: io_mode          ! trcio io mode
    integer,           intent(in   ) :: nr_max           ! max traces in file
    integer,           intent(inout) :: nr_inp           ! num traces in file
    integer,           intent(inout) :: nh_inp           ! num trace head
    integer,           intent(inout) :: nt_inp           ! num trace time
    real,              intent(inout) :: t0_inp           ! min trace time
    real,              intent(inout) :: dt_inp           ! inc trace time
    integer                          :: i_sect
    integer                          :: num_trc_file(:, :)
    integer                          :: num_file_sect  ! num of files /section
    integer                          :: num_trc_sect
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !


    !
    integer                          :: nh_tmp           ! num trace header 
    integer                          :: nt_tmp           ! num trace time
    real                             :: t0_tmp           ! min trace time
    real                             :: dt_tmp           ! inc trace time
    !
    integer                          :: nh_fil           ! num trace header 
    integer                          :: nt_fil           ! num trace time
    real                             :: t0_fil           ! min trace time
    real                             :: dt_fil           ! inc trace time
    !


    integer                ::               i  
    !
    i_err = 0
    !
    ! replace the current globals with the input ones.
    !
    call pc_get_global ( 'NWIH' , nh_tmp )
    !
    call pc_put_global ( 'NWIH' , nh_inp )
    !
    call pc_get_global ( 'NWIH' , nh_fil )
    !
    call timeglob_get ( nt_tmp, t0_tmp, dt_tmp )
    !
    call timeglob_put ( nt_inp, t0_inp, dt_inp )
    !
    call timeglob_get ( nt_fil, t0_fil, dt_fil )
    !
    ! open the input data file on the root pe
    !
    xxif_root_pe : if ( p%group_root ) then
      !
      xxdo_num_f: do i = 1, num_file_sect
        !
        ! open the input files
        !
        f_trcio(i)%trcio_obj => trcio_open ( &
                                             filename = file_inps(i, i_sect), &
                                             io_mode  = io_mode,              &
                                             nwih     = nh_fil,               &
                                             ndpt     = nt_inp,               &
                                             srate    = dt_fil,               &
                                             strt_val = t0_inp                &
                                           )
        !
        f_trcio(i)%num_trc = num_trc_file(i, i_sect)
        !
      end do xxdo_num_f
      !
      if ( i_err .ne. 0 ) go to 997
      !
      ! trace number in a section
      !
      nr_inp = num_trc_sect
      !
!print'(" aq2 nr_max=",i8," nr_inp=",i8," byte=",i12," word=",i12," name=",a)',&
!nr_max, nr_inp, file_extent_byte, file_extent_word, trim(path_trot)
      !
      !
    end if xxif_root_pe
    !
    ! come to here to share error info
    !
1999 continue
    !
    ! replace the global time values
    !
    call timeglob_put ( nt_tmp, t0_tmp, dt_tmp )
    !
    ! broadcast the error flag from the root pe to others in a group
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, nr_inp )
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !
    return
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_open_pw pe= ", i8, &
    & /,  " during trcio_open " &
    & )' ) &
    fxpar_i_pel()
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_open_pw pe=", i8 &
    & )' ) &
    fxpar_i_pel()
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_trc_open_pw  
  !
  subroutine fxdata_read_head_pw ( &
                                   file_list_id, &
                                   num_file_tot, num_file_sect, &
                                   pw_domain, p0_scl, &
                                   nt_inp, nt_fft, dw_fft,  &
                                   freq_min, if_min, freq_max, if_max &
                                 )
    !
    ! read the pw head file to retrieve global information
    !
    integer,           intent(inout) :: num_file_tot
    integer,           intent(inout) :: num_file_sect  ! num of files /section
    character(len=*),  intent(inout) :: pw_domain
    real,              intent(inout) :: p0_scl
    integer,           intent(inout) :: nt_inp
    integer,           intent(inout) :: nt_fft
    real,              intent(inout) :: dw_fft       ! inc trace time
    real,              intent(inout) :: freq_min
    real,              intent(inout) :: freq_max    
    integer,           intent(inout) :: if_min
    integer,           intent(inout) :: if_max
    !
    integer                          :: file_list_id   

    character(len=fxcard_card_len)   :: card

    integer                          :: cio_stat

    !
    ! read the header file 
    ! 
    cio_stat = cio_fgetline(card, fxcard_card_len, file_list_id )  
    !
    !print'(" aa2 card=",a)', trim(card)
    !
    read ( card, fxdata_pw_format_1 ) num_file_tot, num_file_sect, p0_scl
    !
    cio_stat = cio_fgetline(card, fxcard_card_len, file_list_id )  
    !
    !print'(" aa3 card=",a)', trim(card)
    !
    read ( card, fxdata_pw_format_2 ) pw_domain
    !
    pw_domain = adjustl ( pw_domain )
    !
    cio_stat = cio_fgetline(card, fxcard_card_len, file_list_id )  
    !
    !print'(" aa4 card=",a)', trim(card)
    !
    read ( card, fxdata_pw_format_3 )  nt_fft, nt_inp,  dw_fft
    !
    cio_stat = cio_fgetline(card, fxcard_card_len, file_list_id )  
    !
    !print'(" aa6 card=",a)', trim(card)
    !
    read ( card, fxdata_pw_format_4 )  freq_min, if_min, freq_max, if_max
    !
    ! set the new trace length 
    !
    xxif_frequency : &
    if ( string_upper_compare ( pw_domain(1:9), 'FREQUENCY') ) then
     !
     nt_inp = 2 * (if_max - if_min + 1)
     !
    else xxif_frequency 
     !
     nt_inp = nt_fft
     !
   end if xxif_frequency 
   !
   return
   !
  end subroutine fxdata_read_head_pw 
  !
  subroutine fxdata_trc_close ( p, trcio_obj, remove_file, i_err )
    !
    ! close a trcio file
    ! 
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    type ( trcio_struct ),   pointer :: trcio_obj        ! trcio structure
    logical,           intent(in   ) :: remove_file      ! remove trcio file
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !
    integer                          :: j_err            ! local error
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    !print'(" top fxdata_trc_close group_root=",l2," remove_file=",l2)', &
    !p%group_root, remove_file
    !
    j_err = 0
    !
    if ( p%group_root ) &
    j_err = trcio_close ( file = trcio_obj, remove = remove_file )
    !
    if ( j_err .lt. 0 ) go to 999
    !
1999 continue
    !
    !  broadcast the error from the root pe to the r
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !
    return
    !
999 continue
    !
    call pc_info (' error in fxdata_trc_close during out trcio_close ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_trc_close 
  !
  subroutine fxdata_trc_close_pw ( p, f_trcio, remove_file, &
                                   num_file_tot, i_err )
    !
    ! close a trcio file
    ! 
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    type ( fxdata_filestr ), pointer :: f_trcio(:)        ! trcio structure
    logical,           intent(in   ) :: remove_file      ! remove trcio file
    integer,           intent(in   ) :: num_file_tot
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !
    integer                          :: j_err , if           ! local error
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    j_err = 0
    !
    xxif_group_root : if ( p%group_root ) then
     !
     do_if : do if = 1, num_file_tot
      !
!     j_err = trcio_close ( file = f_trcio(if)%trcio_obj, &
!                        remove = remove_file )
      !
      if ( i_err .lt. 0 ) go to 999
      !
     end do do_if 
     !
    end if xxif_group_root 
    !
1999 continue
    !
    !  broadcast the error from the root pe to the r
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !
    return
    !
999 continue
    !
    call pc_info (' error in fxdata_trc_close during out trcio_close ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_trc_close_pw
  !
  subroutine fxdata_trc_read ( &
                               p, trcio_obj, i_trace, broadcast_trace, &
                               nh_inp, hd_inp, &
                               nt_inp, tr_inp, &
                               i_err &
                             )
    !
    ! read a trace from a trcio file
    !
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    type ( trcio_struct ),   pointer :: trcio_obj        ! trcio structure
    integer,           intent(in   ) :: i_trace          ! index of input trace
    logical,           intent(in   ) :: broadcast_trace  ! broadcast trace
    !
    integer,           intent(in   ) :: nh_inp           ! number of header
    double precision,  intent(inout) :: hd_inp(:)       ! headers
    !
    integer,           intent(in   ) :: nt_inp           ! number of traces
    real,              intent(inout) :: tr_inp(:)       ! traces
    !
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    !  initialize headers and trace
    !
    hd_inp (1:nh_inp) = 0.
    tr_inp (1:nt_inp) = 0.
    !
    !  read this trace on the root pe for this group
    !
    !print'(" fxdata_trc_read bef trcio_read_trace i_trace=",i8,1x,i8,1x,i8)',&
    !i_trace,nh_inp,nt_inp
    !
    if ( p%group_root ) &
    i_err = trcio_read_trace ( file = trcio_obj, &
                               hd   = hd_inp,    &
                               tr   = tr_inp,    &
                               tnum = i_trace    &
                               )
    !
    !print'(" rzz i=",i8," n=",i8," l=",i8," hd=",g12.6," tr=",g12.6)',&
    !i_trace,nt_inp,size(tr_inp,1),hd_inp(7),maxval(abs(tr_inp))
    !
    ! broadcast the error flag from the root pe to others in a group
    ! dwh 12-11-03 only broadcast error flag if all pes get data
    !
    if ( broadcast_trace ) &
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !
    ! broadcast the trace from the root pe to the rest
    !
    if ( broadcast_trace ) &
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, nt_inp, tr_inp )
    !
    ! broadcast the headers from the root pe to the rest
    !
    if ( broadcast_trace ) &
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, nh_inp, hd_inp )
    !
    return
    !
999 continue
    !
    !print'( &
    !& /,  " error in fxdata_trc_read during trcio_read pe=", i8, &
    !& " i_err=", i12, " i_trace=", i12 &
    !& )', &
    !fxpar_i_pel(), i_err, i_trace
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_read during trcio_read pe=", i8, &
    & " i_err=", i12, " i_trace=", i12 &
    & )' ) &
    fxpar_i_pel(), i_err, i_trace
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_trc_read
  !
  !read input data for plane wave migration
  !
  subroutine fxdata_trc_read_pw ( &
                                  p, f_trcio, i_trc_sect,&
                                  broadcast_trace, &
                                  nh_inp, hd_inp, &
                                  nt_inp, tr_inp, &
                                  num_file_sect, i_sect, &
                                  i_err &
                                )
    !
    ! read a trace from a trcio file
    !
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    type ( fxdata_filestr ), pointer :: f_trcio(:)       ! trcio structure
    integer,           intent(in   ) :: i_trc_sect    ! index of input trace
    logical,           intent(in   ) :: broadcast_trace  ! broadcast trace
    !
    integer,           intent(in   ) :: nh_inp           ! number of header
    double precision,  intent(inout) :: hd_inp(:)       ! headers
    !
    integer,           intent(in   ) :: nt_inp           ! number of traces
    real,              intent(inout) :: tr_inp(:)       ! traces
    !
    integer,           intent(in   ) :: num_file_sect
    integer,           intent(in   ) :: i_sect
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    integer                          :: n_trc, i_file, n_trc1, i_trc_file, if
    !
    ! local variables
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    !  initialize headers and trace
    !
    hd_inp (1:nh_inp) = 0.
    tr_inp (1:nt_inp) = 0.
    !
    !  read this trace on the root pe for this group
    !
    xxif_group_root : if ( p%group_root ) then
      !
      ! find the right file: i_file and  trace index in this file
      !
      n_trc = 0
      !
      i_file = 1
      !
      i_trc_file = i_trc_sect
      !
      do_if : do if = 1, num_file_sect 
        ! 
        n_trc1 = n_trc + f_trcio(if)%num_trc
        !
        xxif_i_trc_sect : &
        if ( i_trc_sect .gt. n_trc .and. i_trc_sect .le. n_trc1 ) then
          !
          i_file = if
          !
          i_trc_file = i_trc_sect - n_trc
          !
          goto 10
          !
        end if xxif_i_trc_sect 
        !
        n_trc = n_trc1 
        !
      end do do_if 
      !
   10 continue
      !
      i_err = trcio_read_trace ( file = f_trcio(i_file)%trcio_obj, &
                                 hd   = hd_inp,       &
                                 tr   = tr_inp,       &
                                 tnum = i_trc_file    &
                                )
      !
    end if xxif_group_root 
    !
    !print'(" rzz i=",i8," n=",i8," l=",i8," hd=",g12.6," tr=",g12.6)',&
    !i_trc_sect,nt_inp,size(tr_inp,1),hd_inp(7),maxval(abs(tr_inp))
    !
    ! broadcast the error flag from the root pe to others in a group
    ! dwh 12-11-03 only broadcast error flag if all pes get data
    !
    if ( broadcast_trace ) &
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !
    ! broadcast the trace from the root pe to the rest
    !
    if ( broadcast_trace ) &
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, nt_inp, tr_inp )
    !
    ! broadcast the headers from the root pe to the rest
    !
    if ( broadcast_trace ) &
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, nh_inp, hd_inp )
    !
    return
    !
999 continue
    !
    !print'( &
    !& /,  " error in fxdata_trc_read during trcio_read pe=", i8, &
    !& " i_err=", i12, " i_trc_sect=", i12 &
    !& )', &
    !fxpar_i_pel(), i_err, i_trc_sect
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_read during trcio_read pe=", i8, &
    & " i_err=", i12, " i_trc_sect=", i12 &
    & )' ) &
    fxpar_i_pel(), i_err, i_trc_sect
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_trc_read_pw
  !
  subroutine fxdata_trc_write ( &
                                p, trcio_obj, i_trace, reduce_trace, &
                                nh_inp, hd_inp, &
                                nt_inp, tr_inp, &
                                i_err &
                              )
    !
    ! write a trace to a trcio file
    !
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    type ( trcio_struct ),   pointer :: trcio_obj        ! trcio structure
    integer,           intent(in   ) :: i_trace          ! index of output trace
    logical,           intent(in   ) :: reduce_trace     ! reduce trace
    !
    integer,           intent(in   ) :: nh_inp           ! number of header
    double precision,  intent(inout) :: hd_inp(:)       ! headers
    !
    integer,           intent(in   ) :: nt_inp           ! number of traces
    real,              intent(inout) :: tr_inp(:)       ! traces
    !
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! 
    ! local variables
    !


    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! reduce the trace from the root pe to the rest
    !
    if ( reduce_trace ) &
    call pcpsx_sum_reduce_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, nt_inp, tr_inp )
    !
    ! reduce the headers from the root pe to the rest
    !
    if ( reduce_trace ) &
    call pcpsx_sum_reduce_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, nh_inp, hd_inp )
    !
    !  write this trace on the group_root pe
    !
    if ( p%group_root ) &
    i_err = trcio_write_trace ( file = trcio_obj, &
                                hd   = hd_inp,    &
                                tr   = tr_inp,    &
                                tnum = i_trace    &
                              )
    !
    !print'(" wzz i=",i8," n=",i8," l=",i8," hd=",g12.6," tr=",g12.6)',&
    !i_trace,nt_inp,size(tr_inp,1),hd_inp(7),maxval(abs(tr_inp))
    !
    if ( i_err .lt. 0 ) go to 998
    !
    ! come to here to share error info
    !
1999 continue
    !
    ! broadcast the error flag from the root pe to others in a group
    ! dwh 12-11-03 only broadcast error flag if all pes get data
    !
    if ( reduce_trace ) &
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_write pe= ", i8, &
    & /,  " during trcio_write_trace " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_write pe= ", i8 &
    & )' ) &
    fxpar_i_pel()
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_trc_write
  !
  subroutine fxdata_trc_read_b ( &
                                 f_trc, &
                                 i_trace, broadcast_trace, &
                                 hd_inp, tr_inp, &
                                 i_err &
                               )
    !
    ! read a trace from a trcio file
    !
    type ( fxdata_trc ),     pointer :: f_trc            ! fxso   trcio
    integer,           intent(in   ) :: i_trace          ! index of input trace
    logical,           intent(in   ) :: broadcast_trace  ! broadcast trace
    double precision,  intent(inout) :: hd_inp(:)        ! headers
    real,              intent(inout) :: tr_inp(:)        ! traces
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    !  initialize headers and trace
    !
    hd_inp (1:f_trc%nh_inp) = 0.
    tr_inp (1:f_trc%nt_inp) = 0.
    !
    !  read this trace on the root pe for this group
    !
    ! if using two trot files read from 1
    !
    call fxdata_trc_read ( &
                           f_trc%p, f_trc%trcio, &
                           i_trace, broadcast_trace, &
                           f_trc%nh_inp, hd_inp, &
                           f_trc%nt_inp, tr_inp, &
                           i_err &
                         )
    !
    !print'(" rzz i=",i8," n=",i8," l=",i8," hd=",g12.6," tr=",g12.6)',&
    !i_trace,f_trc%nt_inp,size(tr_inp,1),hd_inp(7),maxval(abs(tr_inp)))
    !
    if ( i_err .ne. 0 ) go to 999
    !
    return
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_read_b during trcio_read pe=", i8, &
    & " i_err=", i12, " i_trace=", i12 &
    & )' ) &
    fxpar_i_pel(), i_err, i_trace
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_trc_read_b
  !
  subroutine fxdata_trc_write_b ( &
                                  f_trc, &
                                  i_trace, reduce_trace, &
                                  hd_inp, tr_inp, &
                                  i_err &
                                )
    !
    ! write a trace to a binary trcio file
    !
    type ( fxdata_trc ),     pointer :: f_trc            ! fxso   trcio
    integer,           intent(in   ) :: i_trace          ! index of output trace
    logical,           intent(in   ) :: reduce_trace     ! reduce trace
    double precision,  intent(inout) :: hd_inp(:)        ! headers
    real,              intent(inout) :: tr_inp(:)        ! traces
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    ! 
    ! local variables
    !


    !
    ! initialize the error flag
    !
    i_err = 0
    !
    !  write this trace on the group_root pe
    !
    call fxdata_trc_write ( &
                            f_trc%p, f_trc%trcio, &
                            i_trace, reduce_trace, &
                            f_trc%nh_inp, hd_inp, &
                            f_trc%nt_inp, tr_inp, &
                            i_err &
                          )
    !
    if ( i_err .lt. 0 ) go to 999
    !
    return
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_write_b during fxdata_trc_write pe=", i8 &
    & )' ) &
    fxpar_i_pel()
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_trc_write_b
  !
  subroutine fxdata_trc_create ( &
                                 f_trc, p, &
                                 path_info, path_trot, file_stat, &
                                 info_open, trot_open, time_lock, &
                                 nr_max, nr_inp, nh_inp, &
                                 nt_inp, t0_inp, dt_inp, &
                                 i_err &
                               )
    !
    ! create a fxdat_trc structure for trot and info files
    ! 
    type ( fxdata_trc ),     pointer :: f_trc            ! fxso   trcio
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide 
    character(len=*),  intent(in   ) :: path_info        ! info  file name
    character(len=*),  intent(inout) :: path_trot        ! trcio file name
    character(len=*),  intent(in   ) :: file_stat        ! file status r, w, a+
    logical,           intent(in   ) :: info_open        ! leave info open
    logical,           intent(in   ) :: trot_open        ! leave trot open
    integer,           intent(in   ) :: time_lock        ! file lock time in sec
    integer,           intent(in   ) :: nr_max           ! max traces in file
    integer,           intent(inout) :: nr_inp           ! num traces in file
    integer,           intent(in   ) :: nh_inp           ! num trace head
    integer,           intent(in   ) :: nt_inp           ! num trace time
    real,              intent(in   ) :: t0_inp           ! min trace time
    real,              intent(in   ) :: dt_inp           ! inc trace time
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !
    integer                          :: j_err            ! out trace index
    logical   :: trot_locked
    logical   :: info_locked
    !
    ! initialize the error flag
    !
    i_err = 0
    trot_locked = .false.
    info_locked = .false.
    !
    ! allocate the structure
    !
    if ( .not. associated ( f_trc ) ) &
                 allocate ( f_trc, stat=i_err )
    !
    if ( i_err .ne. 0 ) go to 992
    !
    ! nullify pointers
    !
    nullify (f_trc%trcio) ! jpa
    nullify (f_trc%f_inf) ! jpa
    nullify (f_trc%p) ! jpa
    !
    call fxdata_trc_nullify ( f_trc )
    !
    if ( .not. associated ( f_trc%f_inf ) ) &
                 allocate ( f_trc%f_inf, stat=i_err )
    !
    if ( i_err .ne. 0 ) go to 991
    !
 !print'( &
 !&   " a fxdata_trc_create info_open=",l2,&
 !& /, " a fxdata_trc_create trot_open=",l2,&
 !& /," a fxdata_trc_create file_stat=",a, &
 !& /," a fxdata_trc_create path_trot=",a, &
 !& /," a fxdata_trc_create path_info=",a)',&
 !info_open, trot_open, trim(file_stat), trim(path_trot), trim(path_info)
    !
      nullify (f_trc%f_inf%path_trin) ! jpa
      nullify (f_trc%f_inf%i1_inf) ! jpa
      nullify (f_trc%f_inf%i2_inf) ! jpa
      nullify (f_trc%f_inf%i3_inf) ! jpa
      nullify (f_trc%f_inf%i4_inf) ! jpa
      nullify (f_trc%f_inf%i5_inf) ! jpa
      nullify (f_trc%f_inf%i6_inf) ! jpa
      nullify (f_trc%f_inf%i7_inf) ! jpa
      nullify (f_trc%f_inf%i8_inf) ! jpa
      nullify (f_trc%f_inf%r1_inf) ! jpa
      nullify (f_trc%f_inf%r2_inf) ! jpa
      nullify (f_trc%f_inf%r3_inf) ! jpa
      nullify (f_trc%f_inf%r4_inf) ! jpa
      nullify (f_trc%f_inf%r5_inf) ! jpa
      nullify (f_trc%f_inf%r6_inf) ! jpa
      nullify (f_trc%f_inf%c1_inf) ! jpa
      nullify (f_trc%f_inf%c2_inf) ! jpa
      nullify (f_trc%f_inf%c3_inf) ! jpa
      nullify (f_trc%f_inf%c4_inf) ! jpa
      nullify (f_trc%f_inf%c_history) ! jpa
      nullify (f_trc%f_inf%p) ! jpa
    !
    ! copy the input parameters
    !
    f_trc%path_trot   = path_trot  ! trcio file
    f_trc%path_trot_0 = f_trc%path_trot ! trot file original
    f_trc%path_info   = path_info  ! info  file
    f_trc%file_stat   = file_stat  ! file status
    f_trc%info_open   = info_open  ! leave info open
    f_trc%trot_open   = trot_open  ! leave trot open
    f_trc%time_lock   = time_lock  ! lock time in seconds
    f_trc%nr_max      = nr_max     ! max trace in file
    f_trc%nr_inp      = nr_inp     ! num trace in file
    f_trc%nh_inp      = nh_inp     ! num trace head
    f_trc%nt_inp      = nt_inp     ! num trace time
    f_trc%t0_inp      = t0_inp     ! min trace time
    f_trc%dt_inp      = dt_inp     ! inc trace time
    !
    ! create the local divide structure
    !
    call fxpar_copy ( p, f_trc%p, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    ! lock all files
    !
    if ( f_trc%trot_open &
 .and. ( string_upper_compare ( f_trc%file_stat, 'w'  ) &
  .or.   string_upper_compare ( f_trc%file_stat, 'a+' ) ) ) then
      call fxcard_file_lock ( f_trc%p, f_trc%path_trot,&
      f_trc%time_lock, i_err  )
      if(i_err ==0) trot_locked=.true.
    endif
    !
    if ( i_err .ne. 0 ) go to 996
    !
    call fxcard_file_lock ( f_trc%p, f_trc%path_info,&
      f_trc%time_lock, i_err  )
    if(i_err ==0) info_locked=.true.
    !
    if ( i_err .ne. 0 ) go to 996
    !
    ! read the info into f_trc%f_inf
    !
    call fxdata_file_to_info ( f_trc%path_info, f_trc%f_inf, &
      f_trc%p, i_err )
    !
    !print'(" fxdata_trc_create aft fxdata_file_to_info path_trot=",a)', &
    !trim(f_trc%f_inf%path_trot) 
    !
    if ( i_err .ne. 0 ) go to 995
    !
    ! when using an existing file get the path_trot from the info file
    ! otherwise get it from the input value
    !
    xxif_file_stat_w : &
    if ( string_upper_compare ( f_trc%file_stat, 'w' ) &
    .or. string_upper_compare ( f_trc%file_stat, 'a+' ) &
    .or. string_upper_compare ( f_trc%f_inf%path_trot, pathcheck_empty ) ) then
      !
      f_trc%f_inf%path_trot = f_trc%path_trot
      !
  !print'(" fxdata_trc_create aa1 path_trot=",a)', trim(f_trc%f_inf%path_trot) 
      !
    else xxif_file_stat_w 
      !
      f_trc%path_trot = f_trc%f_inf%path_trot 
      !
      f_trc%path_trot_0 = f_trc%path_trot ! trot file original
      !
  !print'(" fxdata_trc_create aa2 path_trot=",a)', trim(f_trc%f_inf%path_trot) 
      !
    end if xxif_file_stat_w 
    !
    ! open the trot file 
    !
    if ( f_trc%trot_open ) &
    call fxdata_trc_open ( &
                           f_trc%p, f_trc%trcio, &
                           f_trc%f_inf%path_trot, f_trc%file_stat, &
                           f_trc%nr_max, f_trc%nr_inp, f_trc%nh_inp, &
                           f_trc%nt_inp, f_trc%t0_inp, f_trc%dt_inp, &
                           i_err &
                         )
    !
    if ( i_err .ne. 0 ) go to 994
    !
    ! set the number of traces in the file
    !
    nr_inp = f_trc%nr_inp
    !
    ! unlock files
    !
    if(.not. trot_open) then
      !unlock files only  if not being left open
      !print*,'fxdata_trc_create: calling fxdata_file_unlock_n'
      call fxdata_file_unlock_n ( f_trc, i_err  ) 
    endif
    !
    if ( i_err .ne. 0 ) go to 993
    !
    ! come to here to share error info
    !
1999 continue
    !
    path_trot = f_trc%path_trot  ! trcio file
    !
!print'( &
!&   " b fxdata_trc_create info_open=", l2, &
!& /," b fxdata_trc_create file_stat=", a, &
!& /," b fxdata_trc_create path_trot=", a, &
!& /," b fxdata_trc_create path_info=", a )',&
!info_open, trim(file_stat), trim(path_trot), trim(path_info)
    !
    ! broadcast the error flag from the root pe to others in a group
    !
    call pcpsx_broadcast_group ( &
    p%p0_grp, p%np_grp, p%jp_grp, i_err )
    !
    if ( i_err .ne. 0 ) &
    call fxdata_trc_delete ( f_trc, .false., j_err )
    !
    return
    !
991 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_create pe= ", i8, &
    & /,  " during allocate 2 " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
992 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_create pe= ", i8, &
    & /,  " during allocate 1 " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
993 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_create pe= ", i8, &
    & /,  " during fxdata_file_unlock_n " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
994 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_create pe= ", i8, &
    & /,  " during output fxdata_trc_open 1 " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
995 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_create pe= ", i8, &
    & /,  " during fxdata_file_to_info " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
996 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_create pe= ", i8, &
    & /,  " during fxcard_file_lock " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_create pe= ", i8, &
    & /,  " during fxpar_copy " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_create pe= ", i8, &
    & /,  " during fxso_set_path_trot " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_create pe= ", i8 &
    & )' ) &
    fxpar_i_pel()
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_trc_create  ! create fxso trci structure
  !
  subroutine fxdata_trc_delete ( f_trc, info_write, i_err )
    !
    ! delete a fxso trci strucutre for binary trot and list files
    ! 
    type ( fxdata_trc ),     pointer :: f_trc            ! fxso trcio
    logical,           intent(in   ) :: info_write       ! write info 
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! Local variables
    !

    !
    ! initialize the error flag
    !
    i_err = 0
    !
    if ( .not. associated ( f_trc ) ) return
    !
    ! write the info to the output file path_info
    !
    !print'(" fxdata_trc_delete file_open=",l2," info_write=",l2," f=",a)', &
    !f_trc%info_open, info_write, trim(f_trc%f_inf%path_info)
    !
    if ( f_trc%info_open .and. info_write ) &
    call fxdata_info_to_file ( f_trc%f_inf%path_info, f_trc%f_inf, i_err )
    !
    !if ( i_err .ne. 0 ) go to 992
    !
    ! close the input trace file, no remove
    !
    if ( f_trc%trot_open ) &
    call fxdata_trc_close ( f_trc%p, f_trc%trcio, .false., i_err )
    !
    ! unlock files
    !
   !print*,'fxdata_trc_delete: calling fxdata_file_unlock_n'
    call fxdata_file_unlock_n ( f_trc, i_err  ) 
    !
    !if ( i_err .ne. 0 ) go to 995
    !
    ! delete the array memory
    !
    call fxdata_info_delete ( f_trc%f_inf )
    !
    ! delete the local divide structure
    !
    call fxpar_delete ( f_trc%p )
    !
    ! deallocate the structure
    !
    if ( associated ( f_trc ) ) &
         deallocate ( f_trc )
    !
    return
    !
  end subroutine fxdata_trc_delete  ! delete fxso trci structure
  !
  subroutine fxdata_trc_nullify ( f_trc )
    !
    ! nullify a fxdata trcio structure
    ! 
    type ( fxdata_trc ),     pointer :: f_trc            ! fxso trcio
    !
    ! Local variables
    !
    if ( .not. associated ( f_trc ) ) return
    !
    nullify ( f_trc%trcio )      ! trcio structure 
    nullify ( f_trc%f_inf )      ! fxdata info struct
    nullify ( f_trc%p )      ! fxpar divide 
    !
    return
    !
  end subroutine fxdata_trc_nullify  ! delete fxso trci structure
  !
  subroutine fxdata_trc_reset ( f_trc, i_gather, file_reset, i_err )
    !
    ! reset the trace file to the file of gather i_gather
    !
    type ( fxdata_trc ),     pointer :: f_trc            ! fxso   trcio
    integer,           intent(in   ) :: i_gather         ! file gather index 
    logical,           intent(inout) :: file_reset       ! file reset flag
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !







    integer, save                    :: i_call = 0
    !
    i_call = i_call + 1
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! allocate the structure
    !
    if ( .not. associated ( f_trc ) ) go to 998
    !
    ! check if the requested gather index is not within the list 
    !
    if ( i_gather .lt. 1 .or. i_gather .gt. f_trc%f_inf%n0_gat ) go to 997
    !
    ! if the current trace file is not the same as path_trin reset it
    !
    file_reset = trim ( f_trc%f_inf%path_trot ) &
            .ne. trim ( f_trc%f_inf%c4_inf(i_gather) )
    !
    if ( file_reset .or. i_call .eq. 1 ) &
    print'( &
    & /, " fxdata_trc_reset i_gather  =", i8, &
    & /, " fxdata_trc_reset n0_gat    =", i8, &
    & /, " fxdata_trc_reset file_reset=", l2, &
    & /, " fxdata_trc_reset info_open =", l2, &
    & /, " fxdata_trc_reset trot_open =", l2, &
    & /, " fxdata_trc_reset file_stat =", a, &
    & /, " fxdata_trc_reset path_trot =", a, &
    & /, " fxdata_trc_reset path_new  =", a &
    & )', &
    i_gather, f_trc%f_inf%n0_gat, file_reset, &
    f_trc%info_open, f_trc%trot_open, &
    trim ( f_trc%file_stat ), &
    trim ( f_trc%f_inf%path_trot ), &
    trim ( f_trc%f_inf%c4_inf(i_gather) )
    !
    !if ( file_reset ) stop
    !
    xxif_file_reset : if ( file_reset ) then
      !
      ! the trace file must be open and locked
      !
      !if ( i_call .ge. 5 ) stop
      !
      if ( .not. ( f_trc%info_open .and. f_trc%trot_open ) ) go to 996
      !
      ! close the current trace file
      !
      !print'( " fxdata_trc_reset bef fxdata_trc_close ")'
      !
      call fxdata_trc_close ( f_trc%p, f_trc%trcio, .false., i_err )
      !
      !print'( " fxdata_trc_reset aft fxdata_trc_close i_err=",i8)', i_err
      !
      if ( i_err .ne. 0 ) go to 995
      !
      ! unlock this trot file
      !
      if ( string_upper_compare ( f_trc%file_stat, 'w' ) &
      .or. string_upper_compare ( f_trc%file_stat, 'a+' ) ) go to 994
      !
      if ( string_upper_compare ( f_trc%file_stat, 'w' ) &
      .or. string_upper_compare ( f_trc%file_stat, 'a+' ) ) &
      call fxcard_file_unlock ( f_trc%p, f_trc%f_inf%path_trot, i_err  ) 
      !
      if ( i_err .ne. 0 ) go to 994
      !
      ! reset the trot file name
      !
      f_trc%f_inf%path_trot = f_trc%f_inf%c4_inf(i_gather) 
      !
      ! open the new trot file name
      !
      !print'( " fxdata_trc_reset bef fxdata_trc_open path_trot=",a)', &
      !trim(f_trc%f_inf%path_trot)
      !
      call fxdata_trc_open ( &
                             f_trc%p, f_trc%trcio, &
                             f_trc%f_inf%path_trot, f_trc%file_stat, &
                             f_trc%nr_max, f_trc%nr_inp, f_trc%nh_inp, &
                             f_trc%nt_inp, f_trc%t0_inp, f_trc%dt_inp, &
                             i_err &
                           )
      !
      !print'( " fxdata_trc_reset aft fxdata_trc_open i_err=",i8)', i_err
      !
      if ( i_err .ne. 0 ) go to 993
      !
      ! lock this trace file
      !
      if ( string_upper_compare ( f_trc%file_stat, 'w' ) &
      .or. string_upper_compare ( f_trc%file_stat, 'a+' ) ) &
      call fxcard_file_lock ( &
      f_trc%p, f_trc%f_inf%path_trot, f_trc%time_lock, i_err  )
      !
      !print'( " fxdata_trc_reset aft fxcard_file_lock i_err=",i8)', i_err
      !
      if ( i_err .ne. 0 ) go to 992
      !
    end if xxif_file_reset 
    !
    return
    !
992 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_reset pe= ", i8, &
    & /,  " during fxcard_file_lock " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
993 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_reset pe= ", i8, &
    & /,  " during fxdata_trc_open " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
994 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_reset pe= ", i8, &
    & /,  " file_stat=", a &
    & )' ) &
    fxpar_i_pel(), trim(f_trc%file_stat)
    !
    !write ( pc_get_lun(), '( &
    !& /,  " error in fxdata_trc_reset pe= ", i8, &
    !& /,  " during fxdata_file_unlock " &
    !& )' ) &
    !fxpar_i_pel()
    !
    go to 999
    !
995 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_reset pe= ", i8, &
    & /,  " during fxdata_trc_close " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
996 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_reset pe= ", i8, &
    & /,  " file not open info_open=",l2 &
    & /,  " file not open trot_open=",l2 &
    & )' ) &
    fxpar_i_pel(), f_trc%info_open, f_trc%trot_open
    !
    go to 999
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_reset pe= ", i8, &
    & /,  " gather outside expected range i_gather=",i8," n0_gat=",i8 &
    & )' ) &
    fxpar_i_pel(), &
    i_gather, f_trc%f_inf%n0_gat 
    !
    go to 999
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_reset pe= ", i8, &
    & /,  " f_trc not assoicated " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxdata_trc_reset pe= ", i8 &
    & )' ) &
    fxpar_i_pel()
    !
    i_err = -1
    !
    return
    !
  end subroutine fxdata_trc_reset
  !
  subroutine fxdata_trc_init_b ( &
                                  p, c_title, &
                                  n_zero, nr_max, nr_out, nh_out, &
                                  nt_out, t0_out, dt_out, &
                                  initialize_trot, path_trot, &
                                  initialize_info, path_info, &
                                  info_type, i_err &
                                )
    !
    ! initialize the trot file
    !    if initialize_trot is true and the files exist 
    !    fxso will abort and tell the user to explicitly delete the files
    !    if initialize_trot is true and the files do not exist 
    !    fxso will create the files and initialize them.
    !    if initialize_trot is true and the files do not exist fxso will abort
    !    if initialize_trot is false and the files exist 
    !    fxso image the requested shots
    !    if initialize_trot is false and the files do not exist 
    !    fxso will abort and tell the user to change initialize_trot to true
    !    Note all pes go through this
    !
    type ( fxpar_struct ),   pointer :: p                ! fxpar divide 
    character(len=*),  intent(in   ) :: c_title          ! print title
    integer,           intent(in   ) :: n_zero           ! num traces zero
    integer,           intent(inout) :: nr_max           ! num traces max
    integer,           intent(inout) :: nr_out           ! num traces
    integer,           intent(inout) :: nh_out           ! num header words
    integer,           intent(inout) :: nt_out           ! num time samples
    real,              intent(inout) :: t0_out           ! min time sample
    real,              intent(inout) :: dt_out           ! inc time sample
    logical,           intent(in   ) :: initialize_trot  ! init trot file
    character(len=*),  intent(inout) :: path_trot        ! trot file 1
    logical, optional, intent(in   ) :: initialize_info  ! init info file
    character(len=*),  intent(inout) :: path_info        ! list file 1
    character(len=*),  intent(inout) :: info_type         ! for info_type
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! local variables
    !
    integer                          :: exist_trot     ! trot 1 exist flag
    integer                          :: exist_info     ! list 1 exist flag
    !
    ! initialize the error flag
    !
    !print'(" top fxdata_trc_init_b p=",i5," c=",g12.6)',&
    !fxpar_i_pel(), getsys_seconds() 
    !
    !print'(" n_zero=",i8," nr_max=",i8," nr_out=",i8," nh_out=",i8)',&
    !n_zero, nr_max, nr_out, nh_out
    !
    i_err = 0
    !
    ! check for the existance of each of the list and trot files
    ! get staus on the root pe and broadcast
    !
    xxif_first_group_1 : if ( fxpar_i_pel() .eq. 0 ) then
      !
      exist_trot = finquire_file ( path_trot )
      !
      exist_info = finquire_file ( path_info )
      !
    end if xxif_first_group_1
    !
    call pcpsx_broadcast ( 0, exist_trot )
    !
    call pcpsx_broadcast ( 0, exist_info )
    !
    !print'(" pe=",i8," init=", l2, &
    !&" finquire_not_found=",i2," exist=",1x,i2,1x,i2)',&
    !fxpar_i_pel(), initialize_trot, finquire_not_found, &
    !exist_info, exist_trot
    !
    xxif_initialize_trot : if ( initialize_trot ) then
      !
      if ( exist_trot .ne. finquire_not_found ) go to 997
      !
      ! initialize the files on the first group of pes
      !
      if ( p%p0_grp .eq. 0 ) &
      call fxdata_trc_init ( &
                             p, path_trot, 'w', &
                             n_zero, nr_max, nr_out, nh_out, &
                             nt_out, t0_out, dt_out, &
                             i_err &
                           )
      !
      call pcpsx_broadcast ( 0, i_err )
      !
      if ( i_err .ne. 0 ) go to 996
      !
    else xxif_initialize_trot 
      !
      if ( exist_trot .eq. finquire_not_found ) go to 998
      !
    end if xxif_initialize_trot 
    !
    xxif_initialize_info : if ( initialize_info ) then
      !
      if ( exist_info .ne. finquire_not_found ) go to 997
      !
      ! initialize the files on the first group of pes
      !
      if ( p%p0_grp .eq. 0 ) &
      call fxdata_info_init ( p, path_info, path_trot, info_type, i_err )
      !
      call pcpsx_broadcast ( 0, i_err )
      !
      if ( i_err .ne. 0 ) go to 995
      !
    else xxif_initialize_info 
      !
      if ( exist_info .eq. finquire_not_found ) go to 998
      !
    end if xxif_initialize_info 
    !
    !print'(" aa2 fxdata_trc_init_b p=",i5," c=",g12.6)',&
    !fxpar_i_pel(), getsys_seconds() 
    !
    ! come to here to share error info
    !
1999 continue
    !
    !print'(" end fxdata_trc_init_b p=",i5," c=",g12.6)',&
    !fxpar_i_pel(), getsys_seconds() 
    !
    ! share the error status over all pes
    !
    call pcpsx_check_worker_errors ( i_err )
    !
    return
    !
995 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in fxdata_trc_init_b pe= ", i8, &
    & /, " during fxdata_info_init " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
996 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in fxdata_trc_init_b pe= ", i8, &
    & /, " during fxdata_trc_init " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
997 continue
    !
    if ( fxpar_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), '( &
    & /, " error in fxdata_trc_init_b pe= ", i8, &
    & /, " fxso initializeing files and the files already exist ", &
    & /, " you should remove these files and resubmit this job " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
998 continue
    !
    if ( fxpar_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), '( &
    & /, " error in fxdata_trc_init_b pe= ", i8, &
    & /, " fxso is not initializeing files and the files do not exist ", &
    & /, " you should run a job to initialize these files " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
999 continue
    !
    if ( fxpar_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), '( &
    & /, " error in fxdata_trc_init_b pe= ", i8, &
    & /, " initialize_trot = ", l2, &
    & /, " finquire_not_found=", i2, &
    & /, " list file exist =", i2, " f=", a, &
    & /, " trot file exist =", i2, " f=", a &
    & )' ) &
    fxpar_i_pel(), initialize_trot, finquire_not_found, &
    exist_info, trim(path_info), &
    exist_trot, trim(path_trot)
    !
    call pc_error ( ' error in fxdata_trc_init_b ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_trc_init_b 
  !
  subroutine fxdata_file_unlock_n ( f_trc, i_err  ) 
    !
    ! un lock files
    ! 
    type ( fxdata_trc ),     pointer :: f_trc            ! fxso   trcio
    integer,           intent(inout) :: i_err            ! err 0=o.k. -1=error
    !
    ! Local variables
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! unlock files
    !
    !print'(" fxdata_file_unlock_n trot_open=",l2," path_trot=",a)', &
    !f_trc%trot_open, trim(f_trc%f_inf%path_trot)
    !
    !print'(" fxdata_file_unlock_n info_open=",l2," path_info=",a)', &
    !f_trc%info_open, trim(f_trc%f_inf%path_info)
    !
    if ( f_trc%trot_open ) &
    call fxcard_file_unlock ( f_trc%p, f_trc%f_inf%path_trot, i_err  ) 
    !
    if ( f_trc%info_open ) &
    call fxcard_file_unlock ( f_trc%p, f_trc%f_inf%path_info, i_err  ) 
    !
    !if ( i_err .ne. 0 ) go to 995
    !
    return
    !
  end subroutine fxdata_file_unlock_n 
  !
  integer function fxdata_get_trace_count ( trcio_obj ) result(ncount)
    !
    type ( trcio_struct ),   pointer :: trcio_obj        ! trcio structure
    integer                          :: ccount
    integer                          :: recl
    integer                          :: i
    integer                          :: stat
    integer                          :: lun
    integer                          :: bsiz

    integer                          :: wblki
    integer                          :: wbyti
    integer                          :: wblk
    integer                          :: wbyt
    integer                          :: start_pos(2)
    double precision :: rsize
    !
    start_pos = trcio_get_data_start_pos(trcio_obj)
    recl   = trcio_get_recl(trcio_obj)
    !
    lun = trcio_get_lun(trcio_obj)
    bsiz  = 1024
    stat  = cio_ftell(lun,bsiz,wblki,wbyti)   !save byte position
    i     = cio_fseek(lun, 0, 2)              !go to end of file
    stat  = cio_ftell(lun,bsiz,wblk,wbyt)     !get byte position
    rsize = bsiz
    rsize = rsize*wblk
    rsize = rsize + wbyt
    !
    rsize = rsize - start_pos(2)
    rsize = rsize/ trcio_get_recl(trcio_obj)
    ccount = rsize
    !print*,' trcio count=',tcount, 'cio count=',ccount
    !reposition to original spot
    stat  =   cio_fseek(lun, bsiz ,wblki, wbyti, 0)
    ncount=ccount
    !
    return
    !
  end function fxdata_get_trace_count
  !
  subroutine fxdata_get_info_type ( p, path_info, info_type, i_err )
    !
    type ( fxpar_struct ),   pointer :: p            ! fxpar divide
    character(len=*),  intent(in   ) :: path_info        ! info file name
    character(len=*),  intent(inout) :: info_type        ! info type
    integer,           intent(inout) :: i_err            ! error flag
    !
    type ( fxdata_info ),    pointer :: f_inf            ! fxdata info struct
    !
    nullify ( f_inf )
    !
    info_type = 'SHOT'
    !
    call fxdata_file_to_info ( path_info, f_inf, p, i_err )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    info_type = f_inf%info_type
    !
    call fxdata_info_delete ( f_inf )
    !
    return
    !
999 continue
    !
    call pc_error ( ' error in fxdata_get_info_type ' )
    !
    i_err = -1
    !
  end subroutine fxdata_get_info_type
  !
  subroutine fxdata_hist_copy ( f_inp, f_out, i_err )
    !
    ! copy history cards from one info file to another
    !
    type ( fxdata_info ),    pointer :: f_inp           ! inp fxdata_info struct
    type ( fxdata_info ),    pointer :: f_out           ! fxdata_info struct
    integer,           intent(inout) :: i_err           ! err 0=o.k. -1=error
    !
    integer                          :: j_inp
    integer                          :: j_out
    !
    i_err = 0
    !
    !print'(" a fxdata_hist_copy n_history=",i8)', f_inp%n_history
    !print'(" a fxdata_hist_copy i=",i8," c=",a)', &
    !( j_inp, f_inp%c_history(j_inp), j_inp=1,f_inp%n_history )
    !
    !print'(" b fxdata_hist_copy n_history=",i8)', f_out%n_history
    !print'(" b fxdata_hist_copy i=",i8," c=",a)', &
    !( j_out, f_out%c_history(j_out), j_out=1,f_out%n_history )
    !
    do_j_inp : do j_inp = 1 , f_inp%n_history
      !
      do_j_out : do j_out = 1 , f_out%n_history
        !
        if ( f_inp%c_history(j_inp) &
        .eq. f_out%c_history(j_out) ) go to 1
        !
      end do do_j_out 
      !
      call fxdata_card_to_card ( &
    .true., f_inp%c_history(j_inp), f_out%n_history, f_out%c_history, i_err )
      !
      if ( i_err .ne. 0 ) go to 999
      !
    1 continue
      !
    end do do_j_inp
    !
    !print'(" c fxdata_hist_copy n_history=",i8)', f_out%n_history
    !print'(" c fxdata_hist_copy i=",i8," c=",a)', &
    !( j_out, f_out%c_history(j_out), j_out=1,f_out%n_history )
    !
1999 continue
    !
    call fxpar_check_worker_errors ( f_inp%p, i_err )
    !
    return
    !
999 continue
    !
    call pc_error ( ' error in fxdata_hist_copy during fxdata_card_to_card ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxdata_hist_copy 
  !
  subroutine fxdata_bin_gather ( &
                                 x0_bin, dx_bin, rx_scl, &
                                 y0_bin, dy_bin, ry_scl, &
                                 rx_gat_1, rx_gat_2, ry_gat_1, ry_gat_2, &
                                 jx_gat_1, jx_gat_2, jy_gat_1, jy_gat_2, &
                                 bin_separation, xy_separation, &
                                 bin_gathers, &
                                 same_gather, same_bin, move_gather &
                               )
    !
    ! compute gather indicies and same, bin, move flags
    !
    real,                intent (in   ) :: x0_bin      ! x bin min
    real,                intent (in   ) :: dx_bin      ! x bin inc
    real,                intent (in   ) :: rx_scl      ! x bin scl
    real,                intent (in   ) :: y0_bin      ! y bin min
    real,                intent (in   ) :: dy_bin      ! y bin inc
    real,                intent (in   ) :: ry_scl      ! y bin scl
    integer,             intent (inout) :: jx_gat_1 ! x gat index 1
    integer,             intent (inout) :: jx_gat_2 ! x gat index 2
    integer,             intent (inout) :: jy_gat_1 ! y gat index 1
    integer,             intent (inout) :: jy_gat_2 ! y gat index 2
    real,                intent (in   ) :: rx_gat_1 ! x gat value 1
    real,                intent (in   ) :: rx_gat_2 ! x gat value 2
    real,                intent (in   ) :: ry_gat_1 ! y gat value 1
    real,                intent (in   ) :: ry_gat_2 ! y gat value 2
    real,                intent (in   ) :: bin_separation ! bin sep
    real,                intent (inout) :: xy_separation  ! xy  sep
    logical,             intent (in   ) :: bin_gathers ! bin  gather flag
    logical,             intent (inout) :: same_gather ! same gather flag
    logical,             intent (inout) :: same_bin    ! same bin    flag
    logical,             intent (inout) :: move_gather ! move gather flag
    !
    ! Local variables
    !

    !
    jx_gat_1 = mth_bin_number ( x0_bin, dx_bin, rx_gat_1 ) 
    !
    jx_gat_2 = mth_bin_number ( x0_bin, dx_bin, rx_gat_2 ) 
    !
    jy_gat_1 = mth_bin_number ( y0_bin, dy_bin, ry_gat_1 ) 
    !
    jy_gat_2 = mth_bin_number ( y0_bin, dy_bin, ry_gat_2 ) 
    !
    xy_separation = sqrt ( ( ( rx_gat_1 - rx_gat_2 ) * rx_scl ) ** 2 &
                         + ( ( ry_gat_1 - ry_gat_2 ) * rx_scl ) ** 2 )

    !
    same_gather = xy_separation .le. bin_separation 
    !
    move_gather = same_gather 
    !
    same_bin = ( jx_gat_1 .eq. jx_gat_2 ) &
         .and. ( jy_gat_1 .eq. jy_gat_2 ) 
    !
    if ( bin_gathers ) &
    move_gather = move_gather .and. same_bin 
    !
    return
    !
  end subroutine fxdata_bin_gather 
  !
  ! simple non parallel parse of info to get fxcard information
  integer function fxdata_file_to_cards(path_info,cobj) result(status)
      character(len=*),intent(in)  :: path_info
      type(fxcard_struct),pointer  :: cobj

      integer                      :: nr,nc,lun
      integer                      :: i_err
      character(len=256)           :: card
      character(len=80)            :: msg

      integer  :: lc

      nullify(cobj)
      status = -1
      if(path_info==' ' .or. path_info=='NONE') then
        status = 0
        msg = 'fxdata_file_to_cards: error no info file specified'
        goto 99
      endif

      call fxcard_create ( path_info, cobj )

      card = ' '
      lun = cio_fopen(path_info,'r')
      if(lun < 0) then
        msg = 'fxdata_file_to_cards: open error'
        goto 99
      endif

      nr = 1
      nc = 0
      lc = len(card)
      do while (nr >= 0 )

        nr =   cio_fgetline (card, lc,  lun)
      !print'(" aa6 card=",a)', trim(card)
        if(nr<=0) exit
        if(card== ' ') cycle
        nc = nc+1
        call fxcard_add_card(cobj,card)

      enddo
      i_err = cio_fclose(lun)

      status = 0
      return
 99   continue
      status = -1
      print*,msg
      return
  end function fxdata_file_to_cards

  ! simple non parallel parse of file to get gather count
  integer function fxdata_gather_count(path_info) result(status)
      character(len=*),intent(in)  :: path_info

      type(fxcard_struct),pointer  :: cobj
     !integer                      :: card1  !1st gather card
     !integer                      :: ncards !card count
      character(len=80)            :: msg

      status = -1

      nullify(cobj)
      status = fxdata_file_to_cards(path_info,cobj)
      if(status /= 0) then
        status = -1
        msg = 'fxdata_gather_count: error in fxdata_file_to_cards'
        goto 99
      endif

      status = fxdata_number_of_gathers ( cobj )

     !card1  = 0
     !ncards = 0
     !card1  = fxdata_first_gather_card ( cobj ) 
     !ncards = fxcard_num_cards (cobj)
     !status = ncards - card1 + 1
      call fxcard_delete(cobj)

      return
 99   continue
      status = -1
      print*,msg
      return
  end function fxdata_gather_count

  integer function fxdata_info_to_fxcard(path_info,cobj,card1,ncards)&
      result(status)
      character(len=*),intent(in)  :: path_info
      type(fxcard_struct),pointer  :: cobj
      integer,intent(out)          :: card1  !1st gather card
      integer,intent(out)          :: ncards !card count

      character(len=80)            :: msg

      status = -1
      card1  = 0
      ncards = 0
      nullify(cobj)
      status =  fxdata_file_to_cards(path_info,cobj)
      if(status /= 0) then
        status = -1
        msg = 'fxdata_info_to_fxcard: error in fxdata_file_to_cards'
        goto 99
      endif

      card1  = fxdata_first_gather_card ( cobj ) 
      ncards = fxcard_num_cards (cobj)

      status = 0
      return
 99   continue
      status = -1
      print*,msg
  return
  end function fxdata_info_to_fxcard
  !
  !return information about the gathers
  integer function fxdata_rd_info(path_info, stdo, &
    ngat,n_path_trin, path_trins, hx_gat,hy_gat,gatx,gaty,gatn,&
    gxmin,gxmax,gymin,gymax) result(status)
      character(len=*),intent(in)      :: path_info
      integer,intent(in)               :: stdo
      integer,intent(out)              :: n_path_trin
      character(len=120),pointer       :: path_trins(:)
      integer,intent(inout)            :: ngat    !gather count
      integer,intent(out)              :: hx_gat  !gather x header
      integer,intent(out)              :: hy_gat  !gather y header
      real,intent(inout)               :: gxmin   !min gather x
      real,intent(inout)               :: gxmax   !max gather y
      real,intent(inout)               :: gymin   !min gather x
      real,intent(inout)               :: gymax   !max gather y
      real,pointer                     :: gatx(:) !gather x coord
      real,pointer                     :: gaty(:) !gather y coord
      integer,pointer                  :: gatn(:) !gather index



      type(fxcard_struct),pointer :: fcobj
      type(fxcard_struct),pointer :: c_tmp          ! fxcard structure
      type(cardset_struct),pointer :: cobj          ! cardset 
      character(len=256):: card
      integer           :: i_err
     !integer           :: lun,nr,nc
      integer           :: i      
      character(len=16) :: tokens(25)
      character(len=96) :: msg
      character         :: nilstring*8
      integer           :: ntokens
      real              :: lsh,lsx,lsy

      integer           :: cnt,loop
      character(len=8)  :: info_type
      integer           :: card1
      integer           :: ncards
      integer           :: n0_tot

      status = -1

      ngat  = 0
      gxmin = 0
      gxmax = 0
      gymin = 0
      gymax = 0
      hx_gat= 0
      hy_gat= 0
      nullify(gatx)
      nullify(gaty)
      nullify(gatn)
      nullify (c_tmp) ! jpa
      nullify (cobj) ! jpa
      n_path_trin= 0
      nullify(path_trins)

     !write(stdo,*) 'fxdata_rd_info: path=',trim(path_info)
      if(path_info==' ') return
      if(path_info=='NONE') return
      if(path_info=='none') return

      nullify(fcobj)
      ncards= 0
      card1 = 0
      i_err = fxdata_info_to_fxcard(path_info,fcobj,card1,ncards)
      if(i_err /=0) then
         write(stdo,*) 'fxdata_rd_info: error in fxdata_info_to_fxcard'
         return
      endif

      ! seperate out the dcode style cards
      call fxcard_card_to_card ( &
       .false., 1, card1-1, fcobj, c_tmp, i_err )
      call fxcard_card_to_cset ( c_tmp, cobj ) ! copy cardset cards 
      !
      !get key parameter values
      call cardset_get_scalar(cobj, 'number_of_gathers', ngat,msg )
      call cardset_get_scalar(cobj, 'number_of_traces',  n0_tot,msg )
      call cardset_get_scalar(cobj, 'min_gat_x',  gxmin,    msg)
      call cardset_get_scalar(cobj, 'max_gat_x',  gxmax,    msg)
      call cardset_get_scalar(cobj, 'min_gat_y',  gymin,    msg)
      call cardset_get_scalar(cobj, 'max_gat_y',  gymax,    msg)
      call cardset_get_scalar(cobj, 'hdr_gat_x',  hx_gat,   msg)
      call cardset_get_scalar(cobj, 'hdr_gat_y',  hy_gat,   msg)
      call cardset_get_scalar(cobj, 'info_type',  info_type,msg)
      !print*,'fxdata_rd_info: DBG info_type=', trim(info_type)

      !get the input data file names
      call cardset_alloc_array ( cobj, &
      'path_trin', path_trins, n_path_trin, msg )

      call fxcard_delete(c_tmp)
      call cardset_delete ( cobj)
      write(stdo,*) 'fxdata_rd_info: path_trins(1)=',&
      trim(path_trins(1)), ' n_path_trin=',n_path_trin

      !    ngat= f_inf%n0_gat
      !   gxmin = f_inf%r1_inf_1
      !   gxmax = f_inf%r1_inf_2
      !   gymin = f_inf%r2_inf_1
      !   gymax = f_inf%r2_inf_2

   !  print*,'fxdata_rd_info: ncards=',ncards
   !  print*,'fxdata_rd_info: first gather card=',card1
   !  print*,'fxdata_rd_info: n0_tot=',n0_tot
   !  print*,'fxdata_rd_info: ngat=',ngat

      allocate(gatx( ngat),stat=i_err)
      if(i_err /= 0) goto 999
      allocate(gaty( ngat),stat=i_err)
      if(i_err /= 0) goto 999
      allocate(gatn( ngat),stat=i_err)
      if(i_err /= 0) goto 999

      cnt = 0
      loop = 0
      do i = 1, ngat
        call fxcard_get_card ( fcobj, card1+i-1, card, msg )
        ntokens=0
        tokens = ' '
        call string_get_tokens(card, tokens, ntokens, nilstring)
        if(ntokens > 12) then
          loop = loop +1
         !if(mod(loop,10)/=0) cycle
          cnt = cnt+1
          read(tokens(7 ),*) lsh
          read(tokens(12),*) lsx
          read(tokens(13),*) lsy
        ! if(i==1) then
        !   read(tokens(6),*) xval
        !   if(lsx /= 0) xscale = xval/lsx
        !  !if(lsy /= 0) yscale = yval/lsy
        ! endif
          gatx(i) = lsx
          gaty(i) = lsy
          gatn(i) = lsh
          if(cnt >  ngat) exit
        else
          write(stdo,*)  'fxdata_rd_info: warning, ntokens=',ntokens
        endif
      enddo

      call fxcard_delete(fcobj)

      status = 0
 999  continue
      if(status < 0) then
       if(associated(gatx)) deallocate(gatx)
       if(associated(gaty)) deallocate(gaty)
       if(associated(gatn)) deallocate(gatn)
       write(stdo,*) 'fxdata_rd_info: error'
      endif
      write(stdo,*) 'fxdata_rd_info: error'
  return
  end function fxdata_rd_info
  !
end module fxdata_module
 
  ! wrapper for c for fxdata_gather_count
  integer function fxdata_gather_count_c(ipath_info) result(status)
    use fxdata_module
    use string_module
    integer,intent(in) :: ipath_info(*)

    character(len=120) :: path_info

    call string_hh2cc(ipath_info,path_info)
    status =  fxdata_gather_count(path_info)

    return
  end function fxdata_gather_count_c

  ! wrapper for c for fxdata_rd_info
  integer function fxdata_get_info_data_c(ipath_info,&
    stdo, ngat,hx_gat,hy_gat,nsiz,gx,gy,gn) result(status)
    use fxdata_module
    use string_module
    integer,intent(in)               :: ipath_info(*)
    integer,intent(inout)            :: ngat
    integer,intent(in)               :: stdo
    integer,intent(out)              :: hx_gat
    integer,intent(out)              :: hy_gat
    integer,intent(in)               :: nsiz
    real,intent(out)                 :: gx(*)
    real,intent(out)                 :: gy(*)
    integer,intent(out)              :: gn(*)

    character(len=120) :: cpath_info
    integer            :: n_path_trin
    real,pointer       :: gatx(:)
    real,pointer       :: gaty(:)
    integer,pointer    :: gatn(:)
    real               :: gxmin
    real               :: gxmax
    real               :: gymin
    real               :: gymax
    character(len=120),pointer :: path_trins(:)

    cpath_info = ' '
    status = 0;
    gx(1:nsiz) = 0.
    gy(1:nsiz) = 0.
    gn(1:nsiz) = 0
    nullify(path_trins)
    call string_hh2cc(ipath_info,cpath_info)
    status = fxdata_rd_info(cpath_info,stdo, ngat,n_path_trin, path_trins,&
               hx_gat,hy_gat,gatx,gaty,gatn,gxmin,gxmax,gymin,gymax )
    if(status==0) then
      gx(1:min(nsiz, ngat)) = gatx(1:min(nsiz, ngat))
      gy(1:min(nsiz, ngat)) = gaty(1:min(nsiz, ngat))
      gn(1:min(nsiz, ngat)) = gatn(1:min(nsiz, ngat))
      deallocate(gatx)
      deallocate(gaty)
      deallocate(gatn)
      deallocate(path_trins)
    else
      write(stdo,*) 'fxdata_get_info_data_c: error'
    endif
    if(nsiz < ngat) then
      print*,'fxdata_get_info_data_c: warning nsiz=',nsiz, '< ngat=',ngat
    endif
    return
  end function fxdata_get_info_data_c
  !
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!y
