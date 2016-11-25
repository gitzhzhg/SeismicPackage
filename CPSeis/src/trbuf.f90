!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- trbuf.f90 --------------------------------!!
!!------------------------------- trbuf.f90 --------------------------------!!
!!------------------------------- trbuf.f90 --------------------------------!!

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
! Name       : trbuf 
! Category   : io
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2006-12-11   by: Douglas Hanson Make file names unique.
! Maturity   : production
! Purpose    : memory and disk trace buffering
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Trace memory and disk buffering.
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS     
!
!
!-------------------------------------------------------------------------------
!</trace_io_doc>


!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS             
!
!
!-------------------------------------------------------------------------------
!</global_doc>

 
!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS            
!
!
!-------------------------------------------------------------------------------
!</header_word_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS        
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!   i =  intent(in)    = value required upon INPUT.
!   o =  intent(out)   = value set by the routine upon OUTPUT.
!   b =  intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!  create the io structure
!        call trbuf_create ( &
!                            o, &
!                            file_name, &
!                            add_job_name, add_cps_name, add_ipn_name, &
!                            nr_dsk, nr_mem, n2_dim, &
!                            nh_inp, nt_inp, t0_inp, dt_inp, &
!                            memory_size, &
!                            i_err, check_errors &
!                          )
!
!    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
!    character(len=*),     intent(in   ) :: file_name       ! disk file name
!    logical,              intent(in   ) :: add_job_name    ! add job_name 
!    logical,              intent(in   ) :: add_cps_name    ! add cpstemp  
!    logical,              intent(in   ) :: add_ipn_name    ! add ipn      
!    integer,              intent(in   ) :: nr_dsk          ! num traces on disk
!    integer,              intent(in   ) :: nr_mem          ! num traces in mem
!    integer,              intent(in   ) :: n2_dim          ! dim 2 size
!    integer,              intent(in   ) :: nh_inp          ! num header    val
!    integer,              intent(in   ) :: nt_inp          ! num trace tim val
!    real,                 intent(in   ) :: t0_inp          ! min trace tim val
!    real,                 intent(in   ) :: dt_inp          ! inc trace tim val
!    integer,              intent(inout) :: memory_size     ! memory size
!    integer,              intent(inout) :: i_err           ! error flag 
!    logical,              intent(in   ) :: check_errors    ! check error flags
!
! delete trbuf structure
!
!        call trbuf_delete ( o )
!
!    type ( trbuf_struct ),   pointer :: o            ! trbuf buffer structure
!
!    integer,           intent(inout) :: i_err        ! error flag 0 O.K. -1 err
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author  Description
!     ----        ------  -----------
! 10  2006-12-11  Hanson  Make file names unique.
!  9  2006-06-27  Glover  Added NULLIFY statements for Intel compiler.
!  8  2006-01-10  Menger  Removed Unused Variables.
!  7  2005-01-31  Douglas Hanson Fix do not use_disk read, write.
!  6  2004-03-10  Douglas Hanson Remove hardwired use_disk=.true.
!  5  2004-02-19  Douglas Hanson Enable trbuf_file_name.
!  4  2003-09-08  Douglas Hanson Add must_use_disk flag.
!  3  2003-06-10  R.S.Day corrected header count returned by trbuf_trcio_open
!  2  2002-04-17  Douglas Hanson Add trbuf_trcio_open.
!  1  2002-04-10  Douglas Hanson Original version.
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



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

module trbuf_module
  !
  ! memory and disk trace buffering.
  !
  use cio_module
  use matfun_module
  use memfun_module
  use named_constants_module
  use pathcheck_module
  use pc_module
  use pp_module
  use pcpsx_module
  use string_module
  use trcio_module
  !
  implicit  none
  !
  private
  !
  ! subroutines
  !
  public :: trbuf_create              ! create trbuf structure
  public :: trbuf_delete              ! delete trbuf structure
  public :: trbuf_print               ! print  trbuf structure variables
  public :: trbuf_mem_init            ! initialize memory counter
  public :: trbuf_mem_size            ! get memory size
  public :: trbuf_mem_nul             ! nullify  memory pointers
  public :: trbuf_mem_all             ! allocate memory pointers
  public :: trbuf_mem_del             ! delete   memory pointers
  public :: trbuf_init_newest_oldest  ! initialze new, old indices
  public :: trbuf_check_oldest_newest ! check new, old indices
  public :: trbuf_update_newest       ! update newest indices
  public :: trbuf_update_oldest       ! update oldest indices
  public :: trbuf_file_name           ! construct the file name
  public :: trbuf_open                ! open the file
  public :: trbuf_close               ! close the file
  public :: trbuf_zero                ! zero the file
  public :: trbuf_flush               ! flush buffer to file
  public :: trbuf_read                ! read  a trace
  public :: trbuf_write               ! write a trace
  public :: trbuf_get_index           ! get the memory index for disk index
  public :: trbuf_get_trace           ! get a trace 
  public :: trbuf_put_trace           ! put a trace 
  public :: trbuf_trcio_open          ! open with nh_inp defined
  !
  ! functions
  !
  type, public :: trbuf_struct
    !
    type ( trcio_struct ), pointer      :: io_dsk            ! trcio io struc
    character(len=filename_length)      :: file_root         ! file root
    character(len=filename_length)      :: file_name         ! file name
    logical                             :: add_job_name      ! add job_name 
    logical                             :: add_cps_name      ! add cpstemp  
    logical                             :: add_ipn_name      ! add ipn      
    logical                             :: must_use_disk     ! must use disk
    !
    integer                             :: nh_inp            ! num head points
    integer                             :: nt_inp            ! num time points
    real                                :: t0_inp            ! min time points
    real                                :: t1_inp            ! max time points
    real                                :: dt_inp            ! inc time points
    !
    character(len=filename_length)      :: job_name          ! job name
    !
    character(len=2)                    :: file_status       ! disk file status
    logical                             :: file_remove       ! file remove flag
    logical                             :: use_disk          ! disk usage flag
    integer                             :: ipn               ! ipn index
    integer                             :: nr_index          ! number of access
    integer                             :: nr_swaps          ! number of swaps
    !
    double precision,           pointer :: hd_mem ( :, :, : )! header buffer 
    real,                       pointer :: tr_mem ( :, :, : )! trace  buffer 
    !
    integer                             :: i_newest          ! newest trace indx
    integer                             :: i_oldest          ! oldest trace indx
    !
    integer,                    pointer :: i_older ( : )     ! older trace index
    integer,                    pointer :: i_newer ( : )     ! newer trace index
    !
    integer                             :: mr_dsk            ! nr_dsk * n2_dim
    integer                             :: nr_dsk            ! num trace in disk
    integer,                    pointer :: ir_dsk ( : )      ! dsk idx of t mem
    !
    integer                             :: mr_mem            ! nr_mem * n2_dim
    integer                             :: nr_mem            ! num trace in mem
    integer,                    pointer :: ir_mem ( : )      ! mem idx of t dsk
    !
    integer                             :: n2_dim            ! dim 2 size
    !
    integer                             :: memory_size       ! memory size
    !
    logical                             :: check_errors      ! check error flags
    !
  end type trbuf_struct
  !
  integer, public, save                 :: mem_temp = 0
  !
  ! rcs identifier string
  character(len=100),public,save :: trbuf_ident = &
  '$Id: trbuf.f90,v 1.10 2006/12/11 14:18:38 Hanson prod sps $'
  !
  contains
  !
  subroutine trbuf_create ( &
                            o, &
                            file_name, &
                            add_job_name, add_cps_name, add_ipn_name, &
                            must_use_disk, &
                            nr_dsk, nr_mem, n2_dim, &
                            nh_inp, nt_inp, t0_inp, dt_inp, &
                            memory_size, &
                            i_err, check_errors &
                          )
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    character(len=*),     intent(in   ) :: file_name       ! disk file name
    logical,              intent(in   ) :: add_job_name    ! add job_name 
    logical,              intent(in   ) :: add_cps_name    ! add cpstemp  
    logical,              intent(in   ) :: add_ipn_name    ! add ipn      
    logical,              intent(in   ) :: must_use_disk   ! must use disk
    integer,              intent(in   ) :: nr_dsk          ! num traces on disk
    integer,              intent(in   ) :: nr_mem          ! num traces in mem
    integer,              intent(in   ) :: n2_dim          ! dim 2 size
    integer,              intent(in   ) :: nh_inp          ! num header    val
    integer,              intent(in   ) :: nt_inp          ! num trace tim val
    real,                 intent(in   ) :: t0_inp          ! min trace tim val
    real,                 intent(in   ) :: dt_inp          ! inc trace tim val
    integer,              intent(inout) :: memory_size     ! memory size
    integer,              intent(inout) :: i_err           ! error flag 
    logical, optional,    intent(in   ) :: check_errors    ! check error flags
    !
    i_err = 0
    !
    if ( .not. associated ( o ) ) &
                 allocate ( o )
    nullify (o%io_dsk) ! jpa
    nullify (o%hd_mem) ! jpa
    nullify (o%tr_mem) ! jpa
    nullify (o%i_older) ! jpa
    nullify (o%i_newer) ! jpa
    nullify (o%ir_dsk) ! jpa
    nullify (o%ir_mem) ! jpa
    !
    ! copy the input parameters
    !
    o%check_errors = .false. ! check error flags
    !
    if ( present ( check_errors ) ) o%check_errors = check_errors  
    !
    !print'(" trbuf_create p=",i4," check_errors=",l2)', &
    !pcpsx_i_pel(), o%check_errors 
    !
    if ( .not. o%check_errors ) stop
    !
    o%file_root = file_name
    !
    o%file_name = file_name
    !
    o%add_job_name = add_job_name
    !
    o%add_cps_name = add_cps_name
    !
    o%add_ipn_name = add_ipn_name
    !
    o%must_use_disk = must_use_disk
    !
    !qqq
    !
    !o%must_use_disk = .true.
    !
    o%nr_dsk = nr_dsk
    !
    o%nr_mem = nr_mem
    !
    o%n2_dim = n2_dim
    !
    o%nh_inp = nh_inp
    !
    o%nt_inp = nt_inp
    !
    o%t0_inp = t0_inp
    !
    o%dt_inp = dt_inp
    !
    o%t1_inp = ( o%nt_inp - 1 ) * o%dt_inp + o%t0_inp
    !
    call pc_get_jdata ( 'jobname', o%job_name   )
    !
    o%ipn         = pc_get_ipn()
    !
    o%nr_index     = 0
    !
    o%nr_swaps     = 0
    !
    o%file_status = 'w+'
    !
    o%memory_size = 0
    !
    call trbuf_print ( o, 'top of trbuf_create' )
    !
    !o%file_remove = .true.
    !
    xxif_use_disk : if ( o%nr_dsk .gt. o%nr_mem .or. o%must_use_disk ) then
      !
      o%use_disk = .true.
      !
    else xxif_use_disk 
      !
      o%use_disk = .false.
      !
    end if xxif_use_disk 
    !
    ! nullify memory pointers
    !
    !print'(" trbuf_create bef trbuf_mem_nul p=",i4)', pcpsx_i_pel()
    !
    ! allocate memory pointers
    !
    !print'(" trbuf_create bef trbuf_mem_all p=",i4)', pcpsx_i_pel()
    !
    call trbuf_mem_all ( o, i_err )
    !
    if ( o%check_errors ) &
    call pcpsx_check_worker_errors ( i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    !print'(" trbuf_create bef trbuf_init_newest_oldest p=",i4)', pcpsx_i_pel()
    !
    call trbuf_init_newest_oldest ( o )
    !
    ! open the disk file, includes a zero
    !
    !print'(" trbuf_create bef trbuf_open p=",i4)', pcpsx_i_pel()
    !
    call trbuf_open ( o, i_err )
    !
    !print'(" trbuf_create aft trbuf_open p=",i4," i_err=",i8)', &
    !pcpsx_i_pel(), i_err
    !
    if ( i_err .ne. 0 ) go to 997
    !
1999 continue
    !
    if ( o%check_errors ) &
    call pcpsx_check_worker_errors ( i_err )
    !
    ! print the structture variables
    !
    call trbuf_print ( o, 'end of trbuf_create' )
    !
    memory_size = o%memory_size 
    !
    !print'(" trbuf_create end p=",i4)', pcpsx_i_pel()
    !
    return
    !
997 continue
    !
    print'( &
    & /," error in trbuf_create during trbuf_open p=",i4 &
    & )', &
    pcpsx_i_pel()
    !
    go to 999
    !
998 continue
    !
    print'( &
    & /," error in trbuf_create during memory allocation p=",i4 &
    & )', &
    pcpsx_i_pel()
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in trbuf_create ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine trbuf_create
  !
  subroutine trbuf_delete ( o, file_remove, i_err )
    !
    ! delete trbuf strucutre
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    logical,              intent(in   ) :: file_remove     ! remove file flag
    integer,              intent(inout) :: i_err           ! error flag 
    !
    i_err = 0
    !
    call trbuf_print ( o, 'top of trbuf_delete' )
    !
    ! close the trace file
    !
    call trbuf_close ( o, file_remove, i_err )
    !
    ! delete the memory pointers
    !
    call trbuf_mem_del ( o )
    !
    ! delallocate the structure
    !
    if ( associated ( o ) ) &
         deallocate ( o )
    !
    return
    !
  end subroutine trbuf_delete
  !
  subroutine trbuf_print ( o, c_title )
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    character(len=*),     intent(in   ) :: c_title         ! title string
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'( &
    & /, " trcbuf Revision: ", &
    & " 10  2006-09-21  Hanson  Make file names unique. " &
    & )' 
    !
    print'( &
    & /, " title                             =", a, &
    & /, " file_root    =", a, &
    & /, " file_name    =", a, &
    & /, " add_job_name =", l2, &
    & /, " add_cps_name =", l2, &
    & /, " add_ipn_name =", l2, &
    & /, " number of traces on disk nr_dsk      = ", i12, &
    & /, " number of traces in mem  nr_mem      = ", i12, &
    & /, " dimension 2 size         n2_dim      = ", i12, &
    & /, " header length            nh_inp      = ", i12, &
    & /, " trace time length        nt_inp      = ", i12, &
    & /, " trace min time           t0_inp      = ", g12.6, &
    & /, " trace max time           t1_inp      = ", g12.6, &
    & /, " trace time increment     dt_inp      = ", g12.6, &
    & /, " number of trace acesses  nr_index    = ", i12, &
    & /, " number of trace swaps    nr_swaps    = ", i12, &
    & /, " memory usage             memory_size = ", i12 &
    & )', &
    trim(c_title), &
    trim(o%file_root), &
    trim(o%file_name), &
    o%add_job_name, o%add_cps_name, o%add_ipn_name, &
    o%nr_dsk, o%nr_mem, o%n2_dim, &
    o%nh_inp, o%nt_inp, o%t0_inp, o%t1_inp, o%dt_inp, &
    o%nr_index, &
    o%nr_swaps, &
    o%memory_size
    !
  end subroutine trbuf_print
  !
  subroutine trbuf_mem_init 
    !
    ! init memory_size counter
    !
    call memfun_sum_get ( mem_temp )
    !
    call memfun_sum_put ( 0             )
    !
    return
    !
  end subroutine trbuf_mem_init
  !
  subroutine trbuf_mem_size ( memory_size )
    !
    ! init memory_size counter
    !
    integer,              intent(inout) :: memory_size
    !
    call memfun_sum_get ( memory_size )
    !
    call memfun_sum_put ( mem_temp )
    !
    mem_temp = 0
    !
    return
    !
  end subroutine trbuf_mem_size
  !
  subroutine trbuf_mem_nul ( o )
    !
    ! nullify memory pointers
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    !
    call memfun_nul ( o%tr_mem  )
    !
    call memfun_nul ( o%hd_mem  )
    !
    call memfun_nul ( o%i_older )
    !
    call memfun_nul ( o%i_newer )
    !
    call memfun_nul ( o%ir_dsk  )
    !
    call memfun_nul ( o%ir_mem  )
    !
    return
    !
  end subroutine trbuf_mem_nul 
  !
  subroutine trbuf_mem_all ( o, i_err )
    !
    ! allocate memory pointers
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    integer,              intent(inout) :: i_err           ! error flag 
    !
    i_err = 0
    !
    call memfun_sum_put ( 0 )
    !
    call memfun_all ( o%hd_mem,  o%nh_inp, o%n2_dim, o%nr_mem, 'hd_mem',  i_err)
    !
    call memfun_all ( o%tr_mem,  o%nt_inp, o%n2_dim, o%nr_mem, 'tr_mem',  i_err)
    !
    call memfun_all ( o%i_older,             o%nr_mem, 'i_older', i_err )
    !
    call memfun_all ( o%i_newer,             o%nr_mem, 'i_newer', i_err )
    !
    call memfun_all ( o%ir_dsk,              o%nr_mem, 'ir_dsk',  i_err )
    !
    call memfun_all ( o%ir_mem,              o%nr_dsk, 'ir_mem',  i_err )
    !
    call memfun_sum_get ( o%memory_size )
    !
    call trbuf_mem_size ( o%memory_size )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    o%hd_mem  ( 1:o%nh_inp, 1:o%n2_dim, 1:o%nr_mem ) = 0.
    !
    o%tr_mem  ( 1:o%nt_inp, 1:o%n2_dim, 1:o%nr_mem ) = 0.
    !
    o%i_older (             1:o%nr_mem             ) = 0
    !
    o%i_newer (             1:o%nr_mem             ) = 0
    !
    o%ir_dsk  (             1:o%nr_mem             ) = 0
    !
    o%ir_mem  (             1:o%nr_dsk             ) = 0
    !
    return
    !
999 continue
    !
    call pc_error ( ' error in trbuf_create ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine trbuf_mem_all 
  !
  subroutine trbuf_mem_del ( o )
    !
    ! delete trbuf memory pointers
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    !
    call memfun_del ( o%hd_mem  )
    !
    call memfun_del ( o%tr_mem  )
    !
    call memfun_del ( o%i_older )
    !
    call memfun_del ( o%i_newer )
    !
    call memfun_del ( o%ir_dsk  )
    !
    call memfun_del ( o%ir_mem  )
    !
    return
    !
  end subroutine trbuf_mem_del
  !
  subroutine trbuf_init_newest_oldest ( o )
    !
    ! initialize older, newer pointers
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    !
    ! Local variables
    !
    integer                             :: jr_mem
    !
    !print'( &
    !& /, " trbuf_init_newest_oldest", " nr_mem=", i8 )'o%nr_mem
    !
    do_jr_mem : do jr_mem = 1 , o%nr_mem
      !
      o%i_older(jr_mem) = jr_mem - 1
      !
      o%i_newer(jr_mem) = jr_mem + 1
      !
    end do do_jr_mem
    !
    o%i_oldest = 1
    !
    o%i_newest = o%nr_mem
    !
    o%i_older ( o%i_oldest ) = 0
    !
    o%i_newer ( o%i_newest ) = 0
    !
    return
    !
  end subroutine trbuf_init_newest_oldest
  !
  subroutine trbuf_check_oldest_newest ( o, c_title )
    !
    ! check oldest, newest pointers to make sure they are well behaved
    ! used for debugging only
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    character(len=*),     intent(in   ) :: c_title
    !
    ! Local variables
    !
    integer                             :: jr_mem
    integer, save                       :: i_call = 0
    !
    i_call = i_call + 1
    !
    print'( &
    & /, " trbuf_check_oldest_newest p=",i4," i_call=", i8, &
    & /, " c_title=", a, &
    & /, " nr_mem=", i8, &
    & /, " i_oldest=", i8, " i_newer=", i8, " i_older=", i8, &
    & /, " i_newest=", i8, " i_newer=", i8, " i_older=", i8&
    & )', &
    pcpsx_i_pel(), i_call, trim(c_title), o%nr_mem, &
    o%i_oldest, o%i_newer(o%i_oldest), o%i_older(o%i_oldest), &
    o%i_newest, o%i_newer(o%i_newest), o%i_older(o%i_newest)
    !
    print'( &
    & " i=", i8, " i_newer=", i8, " i_older=", i8&
    & )', &
    ( jr_mem, o%i_newer(jr_mem), o%i_older(jr_mem), jr_mem = 1, o%nr_mem )
    !
    ! this should never happen
    !
    xxif_error : if ( o%i_oldest .lt. 1 .or. o%i_oldest .gt. o%nr_mem &
                 .or. o%i_newest .lt. 1 .or. o%i_newest .gt. o%nr_mem &
                 .or. o%i_older(o%i_oldest) .ne. 0 &
                 .or. o%i_newer(o%i_oldest) .lt. 1 &
                 .or. o%i_newer(o%i_oldest) .gt. o%nr_mem &
                 .or. o%i_newer(o%i_newest) .ne. 0 &
                 .or. o%i_older(o%i_newest) .lt. 1 &
                 .or. o%i_older(o%i_newest) .gt. o%nr_mem ) then
      !
      print'( &
      & /, " error in trbuf_check_oldest_newest", &
      & /, " i_call=", i8, " c_title=", a, &
      & /, " nr_mem=", i8, &
      & /, " i_oldest=", i8, " i_newer=", i8, " i_older=", i8, &
      & /, " i_newest=", i8, " i_newer=", i8, " i_older=", i8&
      & )', &
      i_call, trim(c_title), o%nr_mem, &
      o%i_oldest, o%i_newer(o%i_oldest), o%i_older(o%i_oldest), &
      o%i_newest, o%i_newer(o%i_newest), o%i_older(o%i_newest)
      !
      print'( &
      & " i=", i8, " i_newer=", i8, " i_older=", i8&
      & )', &
      ( jr_mem, o%i_newer(jr_mem), o%i_older(jr_mem), &
        jr_mem = 1, o%nr_mem )
      !
      stop
      !
    end if xxif_error 
    !
    return
    !
  end subroutine trbuf_check_oldest_newest
  !
  subroutine trbuf_update_newest ( o, jr_mem )
    !
    ! update the newest trace pointer to jr_mem
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    integer,              intent(in   ) :: jr_mem
    !
    integer                             :: j_older
    integer                             :: j_newer
    !
    if ( o%i_newest .eq. jr_mem ) return
    !
    ! this should never happen
    !
    xxif_error : if ( jr_mem .lt. 1 .or. jr_mem .gt. o%nr_mem ) then
      !
      print'( &
      & /, " error in trbuf_update_newest p=",i4," jr_mem=", i8&
      & )', pcpsx_i_pel(), jr_mem
      !
      call trbuf_check_oldest_newest ( o, 'update_newest' )
      !
      stop
      !
    end if xxif_error 
    !
    j_older = o%i_older(jr_mem)
    !
    j_newer = o%i_newer(jr_mem)
    !
    if ( j_older .ge. 1 .and. j_older .le. o%nr_mem) &
    o%i_newer(j_older)      = j_newer
    !
    if ( j_newer .ge. 1 .and. j_newer .le. o%nr_mem) &
    o%i_older(j_newer)      = j_older
    !
    o%i_newer(jr_mem)       = 0
    !
    o%i_older(jr_mem)       = o%i_newest
    !
    o%i_newer(o%i_newest) = jr_mem
    !
    o%i_newest              = jr_mem
    !
    xxif_oldest : if ( o%i_oldest .eq. jr_mem ) then
      !
      o%i_oldest              = j_newer
      !
      o%i_older(o%i_oldest) = 0
      !
    end if xxif_oldest
    !
    !call trbuf_check_oldest_newest ( o, ' end update_newest' )
    !
    return
    !
  end subroutine trbuf_update_newest
  !
  subroutine trbuf_update_oldest ( o, jr_mem )
    !
    ! set the newest location to the oldest
    ! set newer and older pointers
    ! set jr_mem to the newest index
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    integer,              intent(inout) :: jr_mem
    !
    ! Local variables
    !
    integer                             :: j_oldest
    integer                             :: j_newest
    !
    ! this should never happen
    !
    xxif_error : &
    if ( o%i_oldest .lt. 1 .or. o%i_oldest .gt. o%nr_mem ) then
      !
      print'( &
      & /, " error in trbuf_update_oldest p=",i4," i_oldest=",i8 &
      & )', pcpsx_i_pel(), o%i_oldest
      !
      call trbuf_check_oldest_newest ( o, ' update_oldest' )
      !
      stop
      !
    end if xxif_error
    !
    j_oldest                  = o%i_oldest         ! j_oldest = old oldest trc
    !
    j_newest                  = o%i_newest         ! j_newest = old newest trc
    !
    o%i_oldest              = o%i_newer(j_oldest)! new oldest = newer of old
    !
    o%i_newest              = j_oldest             ! new newest = old oldest
    !
    o%i_older(o%i_newest) = j_newest             ! older of new newest = old
    !
    o%i_newer(  j_newest)   = o%i_newest         ! newer of old newest = new
    !
    o%i_older(o%i_oldest) = 0                    ! older of old oldest = 0
    !
    o%i_newer(o%i_newest) = 0                    ! newer of new newest = 0
    !
    jr_mem                    = o%i_newest         ! new newest
    !
    return
    !
  end subroutine trbuf_update_oldest
  !
  subroutine trbuf_file_name ( o )
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    !
    ! construct the file name
    !
    ! create the disk file name, file_name, incorporating the job name and ipn
    ! put on the temp disk
    !
    !print'(" a trbuf_file_name add_job_name=", l2, " file_root=", a)', &
    !o%add_job_name, trim ( o%file_root )
    !
    o%file_name = trim ( o%file_root ) // '_trbuf'
    !
    !print'(" b trbuf_file_name add_job_name=", l2, " file_name=", a)', &
    !o%add_job_name, trim ( o%file_name )
    !
    if ( o%add_job_name ) &
    o%file_name = trim ( o%job_name ) // '_' // trim ( o%file_name ) 
    !
    !print'(" c trbuf_file_name add_cps_name=", l2, " file_name=", a)', &
    !o%add_cps_name, trim ( o%file_name )
    !
    if ( o%add_cps_name ) &
    o%file_name = "~/cpstemp/" // trim ( o%file_name )
    !
    !print'(" d trbuf_file_name add_cps_name=", l2, " file_name=", a)', &
    !o%add_cps_name, trim ( o%file_name )
    !
    if ( o%add_cps_name ) &
    call exptilde ( o%file_name )
    !
    !print'(" e trbuf_file_name add_cps_name=", l2, " file_name=", a)', &
    !o%add_cps_name, trim ( o%file_name )
    !
    ! get a unique file name, including path, job, ipn
    !
    if ( o%add_ipn_name ) &
    call pp_create_file_name (                           &
                               base_name = o%file_name,  &
                               file_name = o%file_name,  &
                               i_pn      = o%ipn,        &
                               i_worker  = pcpsx_i_pel() &
                              !i_worker  = 0             &
                             )
    !
    o%file_name = trim ( o%file_name ) // '.trc'
    !
    print'(" ftrbuf_file_name p=",i4," ipn=", i8, " file_name=", a)', &
    pcpsx_i_pel(), o%ipn, trim ( o%file_name )
    !
    return
    !
  end subroutine trbuf_file_name 
  !
  subroutine trbuf_open ( o, i_err )
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    integer,              intent(inout) :: i_err           ! error flag 
    !
    ! initialize the error flag, i_err
    !
    i_err = 0
    !
    ! modify the file name to incorporate job, cpstemp, ipn
    !
    !print'(" top trbuf_open p=",i4)', pcpsx_i_pel()
    !
    call trbuf_file_name ( o )
    !
    xxif_use_disk : if ( o%use_disk ) then
      !
    !print'(" trbuf_open bef trcio_open p=",i4)', pcpsx_i_pel()
      !
      o%io_dsk  => trcio_open ( &
                                  filename = o%file_name,    &
                                  io_mode  = o%file_status,  &
                                  scratch  = .false.,        &
                                  nwih     = o%nh_inp,       &
                                  ndpt     = o%nt_inp,       &
                                  strt_val = o%t0_inp,       &
                                  srate    = o%dt_inp        &
                                )
      !
      if ( .not. associated ( o%io_dsk ) ) i_err = -1
      !
    !print'(" trbuf_open aft trcio_open p=",i4," i_err=",i8)', &
    !pcpsx_i_pel(), i_err
      !
      if ( o%check_errors ) &
      call pcpsx_check_worker_errors ( i_err )
      !
      if ( i_err .ne. 0 ) go to 998
      !
      ! set the trcio structure globals
      !
      !o%io_dsk%common%stdout = pc_get_lun()
      o%io_dsk%tmin          = o%t0_inp
      o%io_dsk%tmax          = o%t1_inp
      o%io_dsk%dt            = o%dt_inp
      !
    end if xxif_use_disk 
    !
    ! zero the disk file
    !
    !print'(" trbuf_open bef trbuf_zero p=",i4)', pcpsx_i_pel()   
    !
    call trbuf_zero ( o, i_err )
    !
    if ( o%check_errors ) &
    call pcpsx_check_worker_errors ( i_err )
    !
    !print'(" trbuf_open aft trbuf_zero p=",i4)', pcpsx_i_pel()   
    !
    if ( i_err .ne. 0 ) go to 997
    !
1999 continue
    !
    if ( o%check_errors ) &
    call pcpsx_check_worker_errors ( i_err )
    !
    return
    !
997 continue
    !
    print'(" error in trbuf_open during trbuf_zero p=",i4)', pcpsx_i_pel()
    !
    go to 999
    !
998 continue
    !
    print'(" error in trbuf_open during trcio_open p=",i4)', pcpsx_i_pel()
    !
    go to 999
    !
999 continue
    !
    print'(" error in trbuf_open p=",i4)', pcpsx_i_pel()
    !
    call pc_error ( ' error in trbuf_open ' )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine trbuf_open
  !
  subroutine trbuf_close ( o, file_remove, i_err )
    !
    ! close a disk structure
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    logical,              intent(in   ) :: file_remove     ! remove file flag
    integer,              intent(inout) :: i_err           ! error flag 
    !
    ! initialize the error flag, i_err
    !
    i_err = 0
    !
    ! flush the memory buffer to disk
    !
    call trbuf_flush ( o, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    ! close the disk file
    !
    if ( o%use_disk ) &
    i_err = trcio_close ( file = o%io_dsk, remove = file_remove )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    return
    !
997 continue
    !
    call pc_error ( ' error in trbuf_close during trbuf_flush ' )
    !
    i_err = -1
    !
    !
998 continue
    !
    call pc_error ( ' error in trbuf_close during trcio_close ' )
    !
    i_err = -1
    !
    !
999 continue
    !
    call pc_error ( ' error in trbuf_close ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine trbuf_close
  !
  subroutine trbuf_zero ( o, i_err )
    !
    ! zero a trace file
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    integer,              intent(inout) :: i_err           ! error flag 
    !
    integer                             :: jr_dsk          ! trace disk index
    character(len=12)                   :: c_time
    double precision                    :: file_size
    !
    call string_time ( c_time )
    !
    file_size = &
    ( o%nh_inp * sizeof ( o%hd_mem ( 1,1,1 ) ) &
    + o%nt_inp * sizeof ( o%tr_mem ( 1,1,1 ) ) ) &
  * o%n2_dim * o%nr_mem 
    !
    if ( pcpsx_i_pel() .eq. 0 ) &
    print'(" top trbuf_zero p=",i4, &
    & " nt_inp=",i12," n2_dim=",i12," nr_mem=",i12, " nr_dsk=",i12, &
    & " file_size=",g12.6 &
    & )', &
    pcpsx_i_pel(), o%nt_inp, o%n2_dim, o%nr_mem, o%nr_dsk, file_size
    !
    ! initialize the error flag, i_err
    !
    i_err = 0
    !
    o%hd_mem ( 1:o%nh_inp, 1:o%n2_dim, 1:o%nr_mem ) = 0.
    !
    o%tr_mem ( 1:o%nt_inp, 1:o%n2_dim, 1:o%nr_mem ) = 0.
    !
    if ( .not. o%use_disk ) return
    !
    !print'(" trbuf_open bef write p=",i4)', pcpsx_i_pel()   
    !
    do_jr_dsk : do jr_dsk = 1 , o%nr_dsk
      !
      call trbuf_write ( o, 1, jr_dsk, i_err  )
      !
      if ( o%check_errors ) &
      call pcpsx_check_worker_errors ( i_err )
      !
      if ( i_err .ne. 0 ) go to 999
      !
    end do do_jr_dsk 
    !
    !print'(" trbuf_open aft write p=",i4)', pcpsx_i_pel()   
    !
    return
    !
999 continue
    !
    print'(" error in trbuf_zero during trbuf_write p=",i4," jr_dsk= ",i8)', &
    pcpsx_i_pel(), jr_dsk 
    !
    if ( i_err .ne. -999 ) stop
    !
    call pc_error ( &
    ' error in trbuf_zero during trbuf_write jr_dsk= ', jr_dsk )
    !
    i_err = -1
    !
    return
    !
  end subroutine trbuf_zero
  !
  subroutine trbuf_flush ( o, i_err )
    !
    ! flush a trace file
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    integer,              intent(inout) :: i_err           ! error flag 
    !
    integer                             :: jr_mem          ! trace memory index
    !
    ! initialize the error flag, i_err
    !
    i_err = 0
    !
    if ( .not. o%use_disk ) return
    !
    do_jr_mem : do jr_mem = 1 , o%nr_mem
      !
      ! if memory index ir_dsk(jr_mem) is occupied, write it to disk
      ! write memory index jr_mem to disk index ir_dsk(jr_mem) 
      !
      !print'(" trbuf_flush jr_mem=",i8," ir_dsk=",i8)',&
      !jr_mem, o%ir_dsk ( jr_mem ) 
      !
      if ( o%ir_dsk ( jr_mem ) .gt. 0 ) &
      call trbuf_write ( o, jr_mem, o%ir_dsk ( jr_mem ), i_err  )
      !
      if ( i_err .ne. 0 ) go to 999
      !
    end do do_jr_mem 
    !
    return
    !
999 continue
    !
    call pc_error ( &
    ' error in trbuf_flush during trbuf_write jr_mem= ', jr_mem )
    !
    i_err = -1
    !
    return
    !
  end subroutine trbuf_flush
  !
  subroutine trbuf_read ( o, jr_mem, jr_dsk, i_err  )
    !
    ! read disk index jr_dsk into memory index jr_mem 
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    integer,              intent(in   ) :: jr_mem          ! trace memory index
    integer,              intent(in   ) :: jr_dsk          ! trace disk   index
    integer,              intent(inout) :: i_err           ! error flag 
    !
    ! Local variables
    !
    integer                              :: i2_dim          ! dim 2 index
    !
    ! initialize the error flag, i_err
    !
    i_err = 0
    !
    if ( .not. o%use_disk ) return
    !
    if ( .not. o%use_disk ) go to 998
    !
    xxif_have_trace : &
    if ( jr_mem .gt. 0 .and. jr_mem .le. o%nr_mem &
   .and. jr_dsk .gt. 0 .and. jr_dsk .le. o%nr_dsk ) then
      !
      do_i2_dim : do i2_dim = 1 , o%n2_dim 
        !
        i_err = trcio_read_trace ( &
                                   file = o%io_dsk, &
                                   hd   = o%hd_mem(:,i2_dim,jr_mem), &
                                   tr   = o%tr_mem(:,i2_dim,jr_mem), &
                                   tnum = (jr_dsk-1)*o%n2_dim+i2_dim &
                                  )
        !
        if ( i_err .lt. 0 ) goto 997
        !
      end do do_i2_dim 
      !
    end if xxif_have_trace 
    !
    !print'(" trbuf_read  jr_mem=",i8," jr_dsk=",i8)', jr_mem, jr_dsk
    !
    return
    !
997 continue
    !
    print'( &
    & /," error in trbuf_read during trcio_read_trace p=",i4 &
    & )', &
    pcpsx_i_pel()
    !
    go to 999
    !
998 continue
    !
    print'( &
    & /," error in trbuf_read should not be here p=",i4 &
    & )', &
    pcpsx_i_pel()
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in trbuf_read jr_dsk= ', jr_dsk )
    !
    i_err = -1
    !
    return
    !
  end subroutine trbuf_read
  !
  subroutine trbuf_write ( o, jr_mem, jr_dsk, i_err  )
    !
    ! write memory index jr_mem into disk index jr_dsk
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    integer,              intent(in   ) :: jr_mem          ! trace memory index
    integer,              intent(in   ) :: jr_dsk          ! trace disk   index
    integer,              intent(inout) :: i_err           ! error flag 
    !
    ! Local variables
    !
    integer                              :: i2_dim          ! dim 2 index
    !
    ! initialize the error flag, i_err
    !
    i_err = 0
    !
    if ( .not. o%use_disk ) return
    !
    xxif_have_trace : &
    if ( jr_mem .gt. 0 .and. jr_mem .le. o%nr_mem &
   .and. jr_dsk .gt. 0 .and. jr_dsk .le. o%nr_dsk ) then
      !
!if ( mod(jr_dsk,10) .eq. 1 .or. ( jr_dsk .ge. 50 .and. jr_dsk .lt. 60 ) ) &
!print'(" aa1 trbuf_write p=",i4," e=",i4," jr_dsk=",i8," n2_dim=",i8)', &
!pcpsx_i_pel(), i_err, jr_dsk, o%n2_dim 
      !
      do_i2_dim : do i2_dim = 1 , o%n2_dim 
        !
        i_err = trcio_write_trace ( &
                                    file = o%io_dsk, &
                                    hd   = o%hd_mem(:,i2_dim,jr_mem), &
                                    tr   = o%tr_mem(:,i2_dim,jr_mem), &
                                    tnum = (jr_dsk-1)*o%n2_dim+i2_dim &
                                  )
        !
!if ( jr_dsk .eq. 55 ) &
!print'(" aa2 trbuf_write p=",i4," e=",i4," jr_dsk=",i8," i2_dim=",i8," tnum=",i8)', &
!pcpsx_i_pel(), i_err, jr_dsk, i2_dim, (jr_dsk-1)*o%n2_dim+i2_dim
        !
        if ( i_err .lt. 0 ) goto 998
        !
      end do do_i2_dim 
      !
    end if xxif_have_trace 
    !
    !print'(" trbuf_write jr_mem=",i8," jr_dsk=",i8)', jr_mem, jr_dsk
    !
    return
    !
998 continue
    !
    print'( &
    & /," error in trbuf_write during trcio_write_trace p=",i4 &
    & )', &
    pcpsx_i_pel()
    !
    print'(" trbuf_write p=",i4, &
    & " jr_mem=",i8," jr_dsk=",i8," n2_dim=",i8," i2_dim=",i8," tnum=",i8)', &
    pcpsx_i_pel(), jr_mem, jr_dsk, o%n2_dim, i2_dim, (jr_dsk-1)*o%n2_dim+i2_dim
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' error in trbuf_write jr_dsk= ', jr_dsk )
    !
    i_err = -1
    !
    return
    !
  end subroutine trbuf_write
  !
  subroutine trbuf_get_index ( o, jr_mem, jr_dsk, l_change, i_err  )
    !
    ! get the memory index, jr_mem, for disk index, jr_dsk
    ! this will update the trace in memory if necessary
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    integer,              intent(inout) :: jr_mem          ! trace memory index
    integer,              intent(in   ) :: jr_dsk          ! trace disk   index
    logical,              intent(  out) :: l_change        ! trace change flag
    integer,              intent(inout) :: i_err           ! error flag 
    !
    ! Local variables
    !


    !
    ! initialize the error flag, i_err
    !
    i_err = 0
    !
    ! if this trace is not in memory do steps 1 - 5
    ! then always finish with steps 6-7 
    !
    ! 0) set the trace change flag, l_change
    ! 1) increment the number of trace accesses
    ! 2) increment the number of trace swaps
    ! 3) write the trace in memory index i_oldest 
    !    to disk index ir_dsk(i_oldest)
    ! 4) set the memory index for disk index ir_dsk ( i_oldest ) to 0
    !    to indicate it is no longer in memory
    ! 5) update the oldest index, set the newest location to the oldest
    !    set jr_mem to the newest location 
    ! 6) set the disk index for memory index i_newest to jr_dsk
    ! 7) set the memory index for disk index jr_dsk to i_newest
    ! 8) read the trace in disk index ir_dsk(i_newest)
    !     into memory index i_newest
    ! 9) copy the memory address
    !10) update the newest index
    !
    ! if disk trace jr_dsk is not in memory ir_mem(jr_dsk) <= 0
    !
    jr_mem = 0
    !
    !print'(" trbuf_get_index a i_newest=",i8," i_oldest=",i8, &
    !& " jr_mem=",i8," jr_dsk=",i8, " ir_mem=",i8," ir_dsk=",i8)',&
    !o%i_newest, o%i_oldest, &
    !jr_mem, jr_dsk, o%ir_mem(max(1,jr_dsk)), o%ir_dsk(max(1,jr_mem))
    !
    ! 0) set the trace change flag, l_change
    !
    l_change = o%ir_mem ( jr_dsk ) .le. 0 
    !
    ! 1) increment the number of trace accesses
    !
    o%nr_index = o%nr_index + 1
    !
    xxif_in_memory : if ( l_change ) then
      !
      ! 2) increment the number of trace swaps
      !
      o%nr_swaps = o%nr_swaps + 1
      !
      ! 3) write the trace in memory index i_oldest 
      !    to disk index ir_dsk(i_oldest)
      !
      call trbuf_write ( o, o%i_oldest, o%ir_dsk ( o%i_oldest ), i_err )
      !
      if ( i_err .ne. 0 ) go to 998
      !
      ! 4) set the memory index for disk index ir_dsk ( i_oldest ) to 0
      !    to indicate it is no longer in memory
      !
      if (       o%ir_dsk ( o%i_oldest ) .gt. 0 ) &
      o%ir_mem ( o%ir_dsk ( o%i_oldest ) ) = 0
      !
      ! 5) update the oldest index, set the newest location to the oldest
      !    set jr_mem to the newest location 
      !
      call trbuf_update_oldest ( o, jr_mem )
      !
      ! 6) set the disk index for memory index i_newest to jr_dsk
      !
      o%ir_dsk ( o%i_newest ) = jr_dsk
      !
      ! 7) set the memory index for disk index jr_dsk to i_newest
      !
      o%ir_mem ( jr_dsk ) = o%i_newest 
      !
      ! 8) read the trace in disk index ir_dsk ( i_newest )
      !     into memory index i_newest
      !
      call trbuf_read ( o, o%i_newest, o%ir_dsk ( o%i_newest ), i_err )
      !
      if ( i_err .ne. 0 ) go to 997
      !
    end if xxif_in_memory 
    !
    ! 9) copy the memory address
    !
    !print*,' jr_dsk=',jr_dsk,' ir_mem=', o%ir_mem ( jr_dsk )  
    !
    jr_mem = o%ir_mem ( jr_dsk )  
    !
    ! 10) update the newest index
    !
    call trbuf_update_newest ( o, jr_mem )
    !
    !print'(" trbuf_get_index b i_newest=",i8," i_oldest=",i8, &
    !& " jr_mem=",i8," jr_dsk=",i8, " ir_mem=",i8," ir_dsk=",i8," l=",l2)',&
    !o%i_newest, o%i_oldest, &
    !jr_mem, jr_dsk, o%ir_mem(max(1,jr_dsk)), o%ir_dsk(max(1,jr_mem)), l_change
    !
    return
    !
997 continue
    !
    print'( &
    & /," error in trbuf_get_index during trbuf_read p=",i4 &
    & )', &
    pcpsx_i_pel()
    !
    go to 999
    !
998 continue
    !
    print'( &
    & /," error in trbuf_get_index during trbuf_write p=",i4 &
    & )', &
    pcpsx_i_pel()
    !
    go to 999
    !
999 continue
    !
    print'( &
    & /," error in trbuf_get_index p=",i4 &
    & )', &
    pcpsx_i_pel()
    !
    call pc_error ( ' error in trbuf_get_index ' )
    !
    i_err = -1
    !
    return
    !
  end subroutine trbuf_get_index
  !
  subroutine trbuf_get_trace ( o, jr_dsk, hd_out, tr_out, i_err )
    !
    ! get a trace 
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    integer,              intent(in   ) :: jr_dsk          ! trace disk index
    double precision,     intent(inout) :: hd_out ( :, : ) ! output header 
    real,                 intent(inout) :: tr_out ( :, : ) ! output trace  
    integer,              intent(inout) :: i_err           ! error flag 
    !
    ! Local variables
    !
    integer                             :: jr_mem          ! memory index
    logical                             :: l_change        ! trace change flag
    !
    ! initialize the error flag, i_err
    !
    i_err = 0
    !
    ! get the memory index, jr_mem, for disk index, jr_dsk
    !
    call trbuf_get_index ( o, jr_mem, jr_dsk, l_change, i_err  )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    ! copy the header and trace
    !
      hd_out ( 1:o%nh_inp, 1:o%n2_dim ) = &
    o%hd_mem ( 1:o%nh_inp, 1:o%n2_dim, o%ir_mem ( jr_dsk ) ) 
    !
      tr_out ( 1:o%nt_inp, 1:o%n2_dim ) = &
    o%tr_mem ( 1:o%nt_inp, 1:o%n2_dim, o%ir_mem ( jr_dsk ) ) 
    !
    return
    !
999 continue
    !
    call pc_error ( &
    ' error in trbuf_get_trace during trbuf_get_index jr_dsk= ', jr_dsk )
    !
    i_err = -1
    !
    return
    !
  end subroutine trbuf_get_trace
  !
  subroutine trbuf_put_trace ( o, jr_dsk, hd_out, tr_out, i_err  )
    !
    ! put a trace 
    !
    type ( trbuf_struct ),      pointer :: o               ! trbuf structure
    integer,              intent(in   ) :: jr_dsk          ! trace disk index
    double precision,     intent(in   ) :: hd_out ( :, : ) ! output header 
    real,                 intent(in   ) :: tr_out ( :, : ) ! output trace  
    integer,              intent(inout) :: i_err           ! error flag 
    !
    ! Local variables
    !
    integer                             :: jr_mem          ! memory index
    logical                             :: l_change        ! trace change flag
    !
    ! initialize the error flag, i_err
    !
    i_err = 0
    !
    ! get the memory index, jr_mem, for disk index, jr_dsk
    !
    call trbuf_get_index ( o, jr_mem, jr_dsk, l_change, i_err  )
    !
    if ( i_err .ne. 0 ) go to 999
    !
    ! copy the header and trace
    !
    o%hd_mem ( 1:o%nh_inp, 1:o%n2_dim, o%ir_mem ( jr_dsk ) ) = &
      hd_out ( 1:o%nh_inp, 1:o%n2_dim ) 
    !
    o%tr_mem ( 1:o%nt_inp, 1:o%n2_dim, o%ir_mem ( jr_dsk ) ) = &
      tr_out ( 1:o%nt_inp, 1:o%n2_dim ) 
    !
    return
    !
999 continue
    !
    call pc_error ( &
    ' error in trbuf_put_trace during trbuf_get_index jr_dsk= ', jr_dsk )
    !
    i_err = -1
    !
    return
    !
  end subroutine trbuf_put_trace
  !
  subroutine trbuf_trcio_open ( &
                                io_dsk, file_name, file_stat, n_zero, &
                                nr_inp, nh_inp, nt_inp, t0_inp, dt_inp, &
                                i_err &
                              )
    !
    type ( trcio_struct ),      pointer :: io_dsk          ! disk io structure
    character(len=*),     intent(in   ) :: file_name       ! disk file name
    character(len=*),     intent(in   ) :: file_stat       ! disk file status
    integer,              intent(in   ) :: n_zero          ! num tr to zero
    integer,              intent(inout) :: nr_inp          ! num traces
    integer,              intent(inout) :: nh_inp          ! num header    val
    integer,              intent(inout) :: nt_inp          ! num trace tim val
    real,                 intent(inout) :: t0_inp          ! min trace tim val
    real,                 intent(inout) :: dt_inp          ! inc trace tim val
    integer,              intent(inout) :: i_err           ! error flag 
    !
    double precision                    :: hd_tmp ( nh_inp ) ! sum header 
    real                                :: tr_tmp ( nt_inp ) ! sum trace 
    !
    integer                             :: i_zero                ! record index
    !
    ! initialize the error flag, i_err
    !
    i_err = 0
    !
    !print' ( &
    !& /, " trbuf_trcio_open n_zero=", i8, " file_stat=", a2, " file_name=", a &
    !& )' &
    !n_zero, trim ( file_stat ), trim ( file_name )
    !
    io_dsk  => trcio_open ( &
                            filename = file_name, &
                            io_mode  = file_stat, &
                            scratch  = .false.,   &
                            nwih     = nh_inp,    &
                            ndpt     = nt_inp,    &
                            strt_val = t0_inp,    &
                            srate    = dt_inp     &
                          )
    !
    if ( .not. associated ( io_dsk ) ) i_err = -1
    !
    if ( i_err .ne. 0 ) go to 999
    !
    xxif_read_mode : &
    if ( string_upper_compare ( file_stat, 'r' ) ) then
      !
      ! get the file trace characteristics
      !
      nr_inp = trcio_get_number_traces ( io_dsk )
      !
      nh_inp = io_dsk%nwih !trcio_get_nhd1          ( io_dsk )
      !
      nt_inp = trcio_get_num_values    ( io_dsk )
      !
      t0_inp = trcio_get_tmin          ( io_dsk )
      !
      dt_inp = trcio_get_dt            ( io_dsk )
      !
    else xxif_read_mode 
      !
      ! set the file trace characteristics
      !
      ! initialize the file traces
      !
      xxif_zero : if ( n_zero .gt. 0 ) then
        !
        hd_tmp ( 1:nh_inp ) = 0.
        tr_tmp ( 1:nt_inp ) = 0.
        !
        do_i_zero : do i_zero = 1 , n_zero
          !
          i_err = trcio_write_trace( &
                                     file = io_dsk, &
                                     hd   = hd_tmp, &
                                     tr   = tr_tmp, &
                                     tnum = i_zero  &
                                 )
          !
          if ( i_err .ne. 0 ) go to 999
          !
        end do do_i_zero 
        !
      end if xxif_zero 
      !
    end if xxif_read_mode 
    !
    return
    !
998 continue
    !
    call pc_error ( ' error in trbuf_trcio_open during write ' )
    !
    return
    !
999 continue
    !
    call pc_error ( ' error in trbuf_trcio_open during open ' )
    !
    return
    !
  end subroutine trbuf_trcio_open
  !
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
  !
end module trbuf_module
!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
