!<CPS_v1 type="PRIMITIVE"/>

!!------------------------------- fxpar.f90 -------------------------------!!
!!------------------------------- fxpar.f90 -------------------------------!!
!!------------------------------- fxpar.f90 -------------------------------!!

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
! Name       : fxpar    ( FX IO routines )
! Category   : migrations
! Written    : 2001-02-02   by: Douglas Hanson
! Revised    : 2007-08-02   by: Douglas Hanson Remove print.
! Maturity   : beta
! Purpose    : IO routines for FX migrations.
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
! create the fxpar structure, f_s
!  call fxpar_create ( &
!                         o
!                     f_s, p, &
!                          i         i      i
!                     load_on_root, ipn, disk_opt, &
!                        i         
!                     file_prefix, &
!                         i      i
!                     lw_mig, lw_ext, &
!                         i       i
!                     iw_mig_1, iw_mig_2, &
!                         i      i       i
!                     sign_tw, nt_fft, t0_fft, &
!                        b      
!                     ig_mig, &
!                        b      b       
!                     nq_inp, nq_out, &
!                       i       i       
!                     nt_inp, nt_out, &
!                       i       i       i     
!                     nx_mig, ny_mig, nz_mig, &
!                       i       i       i     
!                     nw_mig, iw_mig, dw_mig, &
!                      o
!                     i_err &
!                    )
!
! delete the fxpar structure, f_s
!                         b      o
!  call fxpar_delete ( f_s, i_err )
!
!
! open the fxpar files
!                         b      o
!  call fxpar_open ( f_s, i_err )
!
!
! close the fxpar files
!                         b      o
!  call fxpar_close ( f_s, i_err )
!
!  call fxpar_put_input_count ( f_s, nq_inp )
!
! get the input count
!                                   b       i        
!  call fxpar_get_input_count ( f_s, nq_inp )
!
! put the output count
!                                   b       i        
!  call fxpar_put_output_count ( f_s, nq_out )
!
! get the output count
!                                   b       i        
!  call fxpar_get_output_count ( f_s, nq_out )
!
! allocate memory for the section buffers
!                               b     o
!  call fxpar_all_section ( f_s, i_err )
!
! deallocate memory for the section buffers
!                              b      o
!  call fxpar_del_section ( f_s, i_err )
!
! nullify memory for the section buffers
!                              b     
!  call fxpar_nul_section ( f_s )
!
! get the iy_want y section
!                              b       i       o
!  call fxpar_get_section ( f_s, iy_want, i_err )
!
! initialize the section buffers
!                               b      o
!  call fxpar_zero_z ( f_s, i_err )
!
! allocate the slice memory buffers
!                             b      o
!  call fxpar_all_slice  ( f_s, i_err )
!
! deallocate the slice memory buffers
!                             b    
!  call fxpar_del_slice  ( f_s )
!
! put the z slice jz_mig from az_slc
!                              b       i       i      o
!  call fxpar_put_z_slice ( f_s, jz_mig_1, jz_mig_2, az_slc, i_err )
!
! get the z slice jz_mig into az_slc
!                              b       i       i       i      o
!  call fxpar_get_z_slice ( f_s, jz_mig_1, jz_mig_2, az_slc, i_err )
!
! put the migration offset information
!                           b       b      o
!  call fxpar_put_info ( f_s, ig_mig, i_err )
!
! get the migration offset information
!                           b       b      o
!  call fxpar_get_info ( f_s, ig_mig, i_err )
!
! apply a phase shift to complex trace ctfft 
! to correct for a non zero time origin
!                                b      i       b
!   call fxpar_origin_shift ( f_s, i_dir, ct_fft )
!
! ipn             integer         process number
! disk_opt        character       disk buffer flag
!
! file_prefix     character       disk file prefix
!
! iw_mig_1        integer         min w index in job
! iw_mig_2        integer         max w index in job
!
! sign_tw         integer         t-w fft sign -1 is normal
! nt_fft          integer         t-w fft length
! t0_fft          real            t-w fft origin shift
!
! iy_want         integer         y value want in buffer
!
! nq_inp          integer         input  trace count
! nq_out          integer         output trace count
!
! nx_mig          integer         number of samples along in-line
! ny_mig          integer         number of samples along cross-line
! nz_mig          integer         number of depth extraplotaion steps
!
! nw_mig          integer         number of frequencies
! iw_mig          integer         migration frequencies index array
! dw_mig          real            frequencies increment in radians/second
!
! ig_mig          integer         group index
! jz_mig_1        integer         min z slice index to put or get
! jz_mig_2        integer         max z slice index to put or get
! jw_mig_1        integer         min w slice index to put or get
! jw_mig_2        integer         max w slice index to put or get
!
! i_dir           integer         direction of phase shift
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
! 17  2007-08-02 Douglas Hanson Remove print.
! 16  2007-05-01 Douglas Hanson Remove print.
! 15  2006-09-21 Douglas Hanson Fix work_load error.
! 14  2006-08-15 D. Glover      Added NULLIFY statements for Intel compiler.
! 13  2006-08-10 Douglas Hanson Fix pg_grp_0 error.
! 12  2006-08-01 R.S.Day        set group_root = true for 1 cpu
! 11  2006-06-22 Douglas Hanson Change f_par to p.
! 10  2005-08-25 Ioan Vlad      Remove unused variables.
!  9  2005-01-31 R.S.Day        Promoted to beta.
!  8  2004-06-24 R.S.Day        Correct fxpar_copy for lock_file
!  7  2004-06-22 Faqi Liu       Add file_lock option.
!  6  2004-01-21 Douglas Hanson Add work_load capability.
!  5  2003-09-15 Faqi Liu       Add fxpar_reset for plane wave migration 
!                                using internal IO
!  4  2003-06-10 R.S.Day        Fixed fxpar_check_worker_errors compiler quirk
!  3  2003-02-12 Douglas Hanson Remove root in group test.
!  2  2002-09-23 Douglas Hanson Fix lu_out in fxpar_create. Nullify pointers.
!  1  2002-09-18 Douglas Hanson Extracted from fxshot.
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
! The following notes refer to forward migration
!
! t refers to time in seconds
! w refers to frequency in radians / second
! x refers to the fast horizontal spatial dimension
! y refers to the slow horizontal spatial dimension
! z refers to the depth
!
! The input data has nw_mig complex frequency slices in it.
! The input data has nw_mig complex frequency slices in it.
! Each slice has nx_mig x value by ny_mig y values in it.
!
! The w slices are broken into np_grp w components in each group
! The minimum w slice index in w node iw_par is fw_min(iw_par)
! The maximum w slice index in w node iw_par is fw_max(iw_par)
! The number of w slices in w node iw_par is fw_num(iw_par) =
! fw_max(iw_par) - fw_min(iw_par) + 1
!
! For all jobs the user defines the number of frequency files defined.
!
! During the input phase:
! A single y slice of input data for all w,x values is gathered into a
! memory buffer.
! When fxpar detects an end of input or a change in yslice value it writes
! the current buffer to disk or memory and starts over with the new
! yslice.  This continues until all the input data has been entered.
!
! During the migration phase the user defines the range of w nodes which
! should be imaged during any particular job.  Only those files are opened
! and used.
!
! During output phase the w nodes are not used.
!
!-------------------------------------------------------------------------------
!</portability_doc>
!
!<gui_def>
! 
! `- Parrallel controls --------------------------------------------------------
!
! CPUS_PER_GROUP =`IIIIIII  PAR_GROUPS=`IIIIIII NUM_CPUS=`IIIIIII 
! LOAD_ON_ROOT=`FFFFFFFF    IO_NODE=`CC 
!
!`------------------------------------------------------------------------------
!
!</gui_def>
!
!<HelpSection>
!
!<Help KEYWORD="CPUS_PER_GROUP">
!<Tip> Number of cpus per parallel processing group. </Tip>
! Default = 1
! Allowed = 1 <= int <= number pes in job
! fxmig will break the NUM_CPUS = number of pes in the job, processors 
! into PAR_GROUPS groups of processors.
! Each group will have a maximum of CPUS_PER_GROUP proecessors in the job,
! and will image a subset of the input traces and construct
! an image indiependantly from th eother groups.  At the end
! of processing the separate images will be summed to gether 
! by fxmig and the result output on all pes.
! It is strongly reccommended that you set the number of 
! processors, NUM_CPUS, in a job be a multiple of CPUS_PER_GROUP, 
! so each group will have the same number of processors.  It is 
! also best that all processors in a job be the same speed.
! 
! The maximum number of processors in any group is :
! m_pe_grp = ( NUM_CPUS + par_groups - 1 ) / par_groups
! The number of groups which use m_pe_grp - 1 processors is :
! n_pe_grp_2 = par_groups * m_pe_grp - NUM_CPUS
! The number of groups which use m_pe_grp - 1 processors is :
! n_pe_grp_1 = NUM_CPUS - n_pe_grp_2
!</Help>
!
!<Help KEYWORD="PAR_GROUPS">
!<Tip> Number of parallel processing groups. </Tip>
! Default = 1
! Allowed = 1 <= int <= number pes in job
! fxmig will break the NUM_CPUS = number of pes in the job, processors 
! into PAR_GROUPS groups of processors.
! Each group will have a maximum of CPUS_PER_GROUP proecessors in the job,
! and will image a subset of the input traces and construct
! an image indiependantly from th eother groups.  At the end
! of processing the separate images will be summed to gether 
! by fxmig and the result output on all pes.
! It is strongly reccommended that you set the number of 
! processors, NUM_CPUS, in a job be a multiple of CPUS_PER_GROUP, 
! so each group will have the same number of processors.  It is 
! also best that all processors in a job be the same speed.
! 
! The maximum number of processors in any group is :
! m_pe_grp = ( NUM_CPUS + par_groups - 1 ) / par_groups
! The number of groups which use m_pe_grp - 1 processors is :
! n_pe_grp_2 = par_groups * m_pe_grp - NUM_CPUS
! The number of groups which use m_pe_grp - 1 processors is :
! n_pe_grp_1 = NUM_CPUS - n_pe_grp_2
!</Help>
!
!<Help KEYWORD="NUM_CPUS">
!<Tip> Total number of cpus in job. </Tip>
! Default = 1
! Allowed = 1 <= int <= number pes in job
! fxmig will break the NUM_CPUS = number of pes in the job, processors 
! into PAR_GROUPS groups of processors.
! Each group will have a maximum of CPUS_PER_GROUP proecessors in the job,
! and will image a subset of the input traces and construct
! an image indiependantly from th eother groups.  At the end
! of processing the separate images will be summed to gether 
! by fxmig and the result output on all pes.
! It is strongly reccommended that you set the number of 
! processors, NUM_CPUS, in a job be a multiple of CPUS_PER_GROUP, 
! so each group will have the same number of processors.  It is 
! also best that all processors in a job be the same speed.
! 
! The maximum number of processors in any group is :
! m_pe_grp = ( NUM_CPUS + par_groups - 1 ) / par_groups
! The number of groups which use m_pe_grp - 1 processors is :
! n_pe_grp_2 = par_groups * m_pe_grp - NUM_CPUS
! The number of groups which use m_pe_grp - 1 processors is :
! n_pe_grp_1 = NUM_CPUS - n_pe_grp_2
!</Help>
!
!<Help KEYWORD="IO_NODE">
!<Tip> Use pe 0 as an io node only. </Tip>
! Default = NO
! Allowed = YES
! Allowed = NO
!</Help>
!
!<Help KEYWORD="LOAD_ON_ROOT">
!<Tip> Relative work load assigned to the root pe in each group. </Tip>
! Default = 1.
! Allowed = 0. <= LOAD_ON_ROOT <= 1.
! 
! The LOAD_ON_ROOT is intended to allow the user to balance the work load
! across pes to improve efficieny.
! 
! If IMAGE_ON_ROOT = YES the image is read and written only by the 
! group root pe.  In that case hte group root pe is doing work the other 
! group pes are not executing and it is possible that the work load will
! become unbalanced with a loss in efficiency.
! 
! If LOAD_ON_ROOT = 1. the group root pe will be assigned as many frequency 
! slices as any other pe in the group.  This means that if the group root pe is
! delayed by IO operations that other pes may not be used efficiently.
! 
! If LOAD_ON_ROOT = 0. the group root pe will not be assigned any frequency 
! slices.  This means the group will only have IO responsibilities and may not 
! be used efficiently.
! 
! It is recommended that LOAD_ON_ROOT be evaluated to find optimal performance.
!</Help>
!
!</HelpSection>
!
module fxpar_module
  !
  use amod_module
  use matfun_module
  use memfun_module
  use named_constants_module
  use pc_module
  use pcpsx_module
  use pp_module
  use ppio_module
  use timeglob_module
  use string_module
  !
  implicit  none
  !
  private
  !
  public :: fxpar_create              ! create  a fxpar divde structure 
  public :: fxpar_delete              ! delete  a fxpar divde structure
  public :: fxpar_nullify             ! nullify a fxpar divde structure
  public :: fxpar_get                 ! get     a fxpar divde structure 
  public :: fxpar_put                 ! put     a fxpar divde structure 
  public :: fxpar_verify              ! verify  a fxpar divde structure 
  public :: fxpar_prep                ! update  a fxpar divde structure 
  public :: fxpar_copy                ! copy    a fxpar divde structure 
  public :: fxpar_reset 
  public :: fxpar_work_load_compute   ! compute   work load coefficients
  public :: fxpar_work_load_normalize ! normalize work load coefficients
  public :: fxpar_work_load_print     ! print     work load coefficients
  public :: fxpar_reset_lock_file
  public :: fxpar_print_0 
  !
  ! functions
  !
  public :: fxpar_print                    ! print flag
  public :: fxpar_root_in_list             ! logical root in list value
  public :: fxpar_index_in_list            ! index of root in list
  public :: fxpar_check_worker_errors      ! sum all reduce over list of pes
  public :: fxpar_send_r1                  ! send 1 real value
  public :: fxpar_i_pel                    ! return pe index
  public :: fxpar_n_pel                    ! number of pes
  !
  ! interfaces
  !
  interface fxpar_create 
    !
    module procedure fxpar_create 
    module procedure fxpar_create_gui 
    !
  end interface 
  !
  ! rcs identifier string
  character(len=100),public,save :: fxpar_ident = &
  '$Id: fxpar.f90,v 1.17 2007/08/03 13:41:14 Hanson beta sps $'
  !
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
!!---------------------- parameter structure -------------------------------!!
  !
  type,  public :: fxpar_struct
    !
    character(len=filename_length)          :: c_title        ! char title
    !
    integer                          :: cpus_per_group   ! num cpus per group
    integer                          :: par_groups       ! num par groups
    integer                          :: par_groups_old   ! num par groups
    integer                          :: num_cpus         ! num cpus in job
    logical                          :: io_node          ! io node option
    real                             :: load_on_root     ! ratio of freq on root
    !
    logical                          :: lock_file     ! lock file flag
    logical                          :: mem_allocated ! mem allocated flag
    logical                          :: mem_computed  ! mem allocated flag
    logical                          :: group_root    ! group root flag
    character(len=5)                 :: opt_type      ! option flag
    integer                          :: np_grp_1      ! num grps w/mp_grp pes
    integer                          :: np_grp_2      ! num grps w/mp_grp-1 pes
    integer                          :: nc_pel        ! num   compute node
    integer                          :: c1_pel        ! first compute node
    integer                          :: c2_pel        ! last  compute node
    integer                          :: p0_grp        ! first pe in grp this grp
    integer                          :: sg_grp        ! shot index this group
    integer                          :: ig_grp        ! grp index this pe
    integer                          :: ip_grp        ! pe within grp index
    integer                          :: np_grp        ! num pes this grp
    integer                          :: mp_grp        ! max pes all grps
    integer,          pointer        :: pg_grp(:)  ! pe to grp index all pes
    integer,          pointer        :: pp_grp(:)  ! pe in grp index all pes
    integer,          pointer        :: jp_grp(:)  ! pe in grp index this grp
    real,             pointer        :: wl_grp(:)  ! work load coefficients
    integer,          pointer        :: pn_grp(:)  ! num pes in grp
    integer,          pointer        :: p1_grp(:)  ! first pe in group
    integer,          pointer        :: p2_grp(:)  ! last  pe in group
    !
  end type fxpar_struct
  !
  !
  integer,    save       :: i_fxpar_print = 0
  integer,    save       :: debug_level    = 0
  !
  integer,    parameter :: n_yes_no = 2
  character(len=3),save :: c_yes_no(n_yes_no) &
              = (/ 'YES', 'NO ' /)
  !
  contains
  !
  subroutine fxpar_create ( p, i_err )
    !
    ! divide fgather_n_epl pes into par_groups groups
    !
    type ( fxpar_struct ),   pointer :: p        ! fxpar  strucutre
    integer,           intent(  out) :: i_err        ! error flag
    !
    ! Local variables
    !
    ! initialize the error flag, i_err
    ! this is only used for memory allocaiton
    !
    i_err = 0
    !
    allocate ( p )
    !
    nullify (p%pg_grp)  ! jpa
    nullify (p%pp_grp)  ! jpa
    nullify (p%jp_grp)  ! jpa
    nullify (p%wl_grp)  ! jpa
    nullify (p%pn_grp)  ! jpa
    nullify (p%p1_grp)  ! jpa
    nullify (p%p2_grp)  ! jpa
    !
    ! initialize parameters
    !
    call fxpar_initialize ( p )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    return
    !
1999 continue
    !
    call fxpar_check_worker_errors ( p, i_err )
    !
    if ( i_err .ne. 0 ) call fxpar_delete ( p )
    !
    call memfun_prn_res ( )
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in fxpar_create p=", i4, &
    & /, " during fxpar_prep " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' fxpar_create error p=', fxpar_i_pel() )
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxpar_create 
  !
  subroutine fxpar_create_gui ( p, c_title, i_err )
    !
    ! create the fxpar structure
    !
    type ( fxpar_struct ),   pointer :: p              ! fxpar structure
    character(len=*),  intent(in   ) :: c_title        ! char title
    integer,           intent(inout) :: i_err          ! err 0=o.k. -1=error
    !
    i_err = 0
    !
    allocate ( p )
    !
    nullify (p%pg_grp)  ! jpa
    nullify (p%pp_grp)  ! jpa
    nullify (p%jp_grp)  ! jpa
    nullify (p%wl_grp)  ! jpa
    nullify (p%pn_grp)  ! jpa
    nullify (p%p1_grp)  ! jpa
    nullify (p%p2_grp)  ! jpa
    !
    ! nullify fxpar pointers
    !
    p%c_title = c_title
    !
    call fxpar_nullify ( p )
    !
    call fxpar_initialize ( p )
    !
    return
    !
  end subroutine fxpar_create_gui 
  !
  subroutine fxpar_delete ( p )
    !
    ! delete the fxpar struture
    !
    type ( fxpar_struct ),   pointer :: p              ! fxpar structure
    !
    if ( .not. associated ( p ) ) return
    !
    call fxpar_mem_del ( p )
    !
    ! delete the select strucutres
    !
    if ( associated ( p ) ) &
         deallocate ( p )
    !
    return
    !
  end subroutine fxpar_delete 
  !
  subroutine fxpar_nullify ( p )
    !
    ! nullify fxpar pointers
    !
    type ( fxpar_struct ),   pointer :: p              ! fxpar structure
    !
    ! Local variables
    !
    call fxpar_mem_nul ( p )
    !
    return 
    !
  end subroutine fxpar_nullify 
  !
  subroutine fxpar_mem_del ( p )
    !
    ! delete the divide structure
    !
    type ( fxpar_struct ),   pointer :: p          ! fxpar  strucutre
    !
    if ( .not. associated ( p ) ) return
    !
    call memfun_del ( p%pg_grp          )
    call memfun_del ( p%pp_grp          )
    call memfun_del ( p%jp_grp          )
    call memfun_del ( p%wl_grp          )
    call memfun_del ( p%pn_grp          )
    call memfun_del ( p%p1_grp          )
    call memfun_del ( p%p2_grp          )
    !
    p%mem_allocated = .false.
    !
    return
    !
  end subroutine fxpar_mem_del 
  !
  subroutine fxpar_mem_nul ( p )
    !
    ! nullify  the fxpar structure
    !
    type ( fxpar_struct ),   pointer :: p          ! fxpar  strucutre
    !
    call memfun_nul ( p%pg_grp          )
    call memfun_nul ( p%pp_grp          )
    call memfun_nul ( p%jp_grp          )
    call memfun_nul ( p%wl_grp          )
    call memfun_nul ( p%pn_grp          )
    call memfun_nul ( p%p1_grp          )
    call memfun_nul ( p%p2_grp          )
    !
    return
    !
  end subroutine fxpar_mem_nul 
  !
  subroutine fxpar_mem_all ( p, i_err )
    !
    ! allocate memory for fxpar structure 
    !
    type ( fxpar_struct ),  pointer :: p            ! fxpar  strucutre
    integer,          intent(  out) :: i_err        ! error flag
    !
    ! Local variables
    !
    ! initialize the error flag, i_err
    ! this is only used for memory allocaiton
    !
    i_err = 0
    !
    call memfun_prn_put ( .false. )
    !
    call memfun_all ( p%pg_grp, fxpar_n_pel(), 'pg_grp', i_err )
    call memfun_all ( p%pp_grp, fxpar_n_pel(), 'pp_grp', i_err )
    call memfun_all ( p%jp_grp, fxpar_n_pel(), 'jp_grp', i_err )
    call memfun_all ( p%wl_grp, fxpar_n_pel(), 'wl_grp', i_err )
    call memfun_all ( p%pn_grp, p%par_groups,  'pn_grp', i_err )
    call memfun_all ( p%p1_grp, p%par_groups,  'p1_grp', i_err )
    call memfun_all ( p%p2_grp, p%par_groups,  'p2_grp', i_err )
    !
    p%par_groups_old = p%par_groups      ! number of parrellel groups
    !
    p%mem_allocated = .true.
    !
    call memfun_prn_res ( )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxpar_mem_all during memory allocation p=", i4, &
    &" num pes=", i8 &
    & )' ) &
    fxpar_i_pel(), fxpar_n_pel()
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxpar_mem_all p=", i4 &
    & )' ) &
    fxpar_i_pel()
    !
    i_err = -1
    !
    return
    !
  end subroutine fxpar_mem_all 
  !
  subroutine fxpar_initialize ( p )
    !
    ! initialize input parameters
    !
    type ( fxpar_struct ),   pointer :: p              ! fxpar structure
    !
    ! Local variables
    !
    integer                          :: i_err            ! err 0=o.k. -1=error
    !
    ! initialize the error flag
    !
    i_err = 0
    !
    ! nullify pointers
    !
    call fxpar_nullify ( p )
    !
    p%c_title = ' '       ! char title
    !
    !p%cpus_per_group = 0 ! num cpus per group
    !p%par_groups = 0 ! num par groups
    !p%par_groups_old = p%par_groups ! num par groups
    !p%num_cpus = 0 ! num par groups
    !
    p%num_cpus        = fxpar_n_pel()! number of parrellel groups
    p%cpus_per_group  = fxpar_n_pel()! number of cpus per group
    p%par_groups      = 1            ! number of parrellel groups
    p%par_groups_old = p%par_groups ! num par groups
    !
    p%load_on_root  = 1       ! root pe work load
    p%io_node       = .false. ! io node option
    p%lock_file     = .false. ! lock file flag
    p%group_root    = .false. ! group root flag
    if ( p%num_cpus .eq. 1 ) p%group_root= .true.
    p%mem_allocated = .false. ! mem allocated flag
    p%mem_computed  = .false. ! mem computed flag
    p%opt_type      = ' '     ! option flag
    p%nc_pel        = 1       ! num   compute node
    p%c1_pel        = 0       ! first compute node
    p%c2_pel        = 0       ! last  compute node
    p%p0_grp        = 0       ! first pe in grp this grp
    p%sg_grp        = 0       ! shot index this group
    p%ig_grp        = 0       ! grp index this pe
    p%ip_grp        = 0       ! pe within grp index
    p%np_grp        = 0       ! num pes this grp
    p%mp_grp        = 0       ! max pes all grps
    !
    ! initialize fxpar memory pointers
    !
    call fxpar_mem_all ( p, i_err ) 
    !
    return
    !
  end subroutine fxpar_initialize
  !
  subroutine fxpar_get ( p )
    !
    type ( fxpar_struct ),   pointer :: p              ! fxpar structure
    !
    integer                          :: i_err
    !
    i_err = 0
    !
    ! get the debug level
    !
    call pc_get_jdata ( 'debug_level', debug_level ) ! 0 none 3 lots of print
    !
    i_fxpar_print = debug_level
    !
    if ( fxpar_i_pel() .eq. 0 ) i_fxpar_print = max ( i_fxpar_print, 1 )
    !
    ! adjust the memory printing
    !
    if ( i_fxpar_print .ge. 0 ) call memfun_prn_off ( )
    !
    p%cpus_per_group = -1
    call pc_get ( 'cpus_per_group',    p%cpus_per_group    )
    call pc_get ( 'par_groups',        p%par_groups        )
    call pc_get ( 'num_cpus',          p%num_cpus          )
    call pc_get ( 'load_on_root',      p%load_on_root      )
    call pc_get ( 'io_node',           p%io_node           )
    if ( p%cpus_per_group .lt. 0 ) &
    p%cpus_per_group = ( p%num_cpus + p%par_groups - 1 ) / p%par_groups
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in fxpar_get p=", i4, &
    & /, " during xxx " &
    & )' ) &
    fxpar_i_pel()
    !
    call pc_error ( 'fxpar_get 998 error ' )
    !
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in fxpar_get" &
    & )' )
    !
    call pc_error ( 'fxpar_get error ' )
    !
    return
    !
  end subroutine fxpar_get
  !
  subroutine fxpar_put ( p )
    !
    type ( fxpar_struct ),   pointer :: p              ! fxpar structure
    !
    call pc_put_options_field ( 'io_node',         c_yes_no,     n_yes_no     )
    !
    call amod_line_feed ( 'fxpar_parameters' )
    call pc_put ( 'cpus_per_group',    p%cpus_per_group    )
    call pc_put ( 'par_groups',        p%par_groups        )
    call pc_put ( 'num_cpus',          p%num_cpus          )
    call pc_put ( 'load_on_root',      p%load_on_root      )
    call pc_put ( 'io_node',           p%io_node           )
    !
    return
    !
  end subroutine fxpar_put
  !
  subroutine fxpar_verify ( p )
    !
    ! verify parameters
    !
    type ( fxpar_struct ),   pointer :: p              ! fxpar structure
    !
    integer                          :: i_err
    !
    i_err = 0
    !
    p%num_cpus       =   fxpar_n_pel() 
    !
    !call pc_put_visible_flag ( 'cpus_per_group',    p%single_job_io )
    !call pc_put_visible_flag ( 'par_groups',        p%single_job_io ) 
    !call pc_put_visible_flag ( 'num_cpus',          p%single_job_io ) 
    !
    return
    !
  end subroutine fxpar_verify 
  !
  subroutine fxpar_prep ( p, i_err )
    !
    ! divide fgather_n_epl pes into par_groups groups
    !
    type ( fxpar_struct ),   pointer :: p        ! fxpar  strucutre
    integer,           intent(  out) :: i_err        ! error flag
    !
    ! Local variables
    !
    integer                          :: j_pel
    integer                          :: j0_grp
    integer                          :: pg_grp_0
    integer                          :: pp_grp_0
    logical                          :: mem_allocate
    !
    !print'(" top fxpar_prep p=",i4," return=",l2, &
    !& " mem_allocated=",l2, " mem_computed=",l2, &
    !& " fxpar_n_pel=",i8," size ( p%wl_grp, 1 )=",i8, &
    !& " p%par_groups =",i8," p%par_groups_old =",i8)', &
    !( p%mem_allocated &
    !.and. p%mem_computed &
    !.and. fxpar_n_pel() .le. size ( p%wl_grp, 1 ) &
    !.and. p%par_groups .eq. p%par_groups_old ) , &
    !fxpar_i_pel(), fxpar_n_pel(), &
    !p%mem_allocated, &
    !p%mem_computed, &
    !size ( p%wl_grp, 1 ), &
    !p%par_groups, p%par_groups_old 
    !
    ! initialize the error flag, i_err
    ! this is only used for memory allocaiton
    !
    i_err = 0
    !
    if ( p%mem_allocated &
   .and. p%mem_computed &
   .and. fxpar_n_pel() .le. size ( p%wl_grp, 1 ) &
   .and. p%par_groups .eq. p%par_groups_old ) &
    return
    !
    ! allocate memory
    !
    xxif_mem : if ( p%mem_allocated ) then
      !
      ! allocate memory if we need more space
      !
      mem_allocate = fxpar_n_pel() .gt. size ( p%wl_grp, 1 ) &
                .or. p%par_groups  .gt. size ( p%pn_grp, 1 ) 
      !
    else xxif_mem 
      !
      ! allocate memory if memory has not yet been allocated 
      !
      mem_allocate = .true.
      !
    end if xxif_mem 
    !
    if ( mem_allocate ) &
    call fxpar_mem_all ( p, i_err )
    !
    if ( i_err .ne. 0 ) go to 997
    !
    p%mem_allocated = .true.
    !
    ! determine the first, c1_pel, and last, c2_pel, 
    ! and the number, nc_pel,  compute nodes
    !
    xxif_io_node_1 : if ( p%io_node .and. fxpar_n_pel() .gt. 1 ) then
      !
      p%c1_pel = 1
      !
    else xxif_io_node_1 
      !
      p%c1_pel = 0
      !
    end if xxif_io_node_1
    !
    p%c2_pel = fxpar_n_pel() - 1
    !
    p%nc_pel = p%c2_pel - p%c1_pel + 1
    !
    p%nc_pel = max ( 1, p%nc_pel )
    !
    !  determine which group each pe is in
    !  there are par_groups groups and fxpar_n_pel pes
    !  nc_pel of these pes from pe c1_pel to c2_pel will be included in
    !  processing groups
    !  if fxpar_n_pel>1 and io_node=.true. pe 0 will be used for io only
    !  otherwise it will be included in a processing group
    !  there are a maixmum of mp_grp pes in any group
    !  groups 1 - np_grp_1 will have mp_grp pes
    !  groups np_grp_1 - par_groups will have mp_grp - 1 pes
    !
    p%mp_grp = ( p%nc_pel + p%par_groups - 1 ) / p%par_groups ! max pes in grp
    !
    p%np_grp_2 = p%par_groups * p%mp_grp - p%nc_pel! num with mp_grp-1
    !
    p%np_grp_1 = p%par_groups - p%np_grp_2         ! num with mp_grp   
    !
    pg_grp_0 = p%c1_pel                            ! first pe this group
    !
    ! for each group determine the number of pe s in the group pn_grp
    ! and the first, p1_grp, and last, p2_grp, pe s in the group
    ! note p1_grp and p2_grp are the pe value 0 to fxpar_n_pel-1
    ! the for each pe iditifiy which group it is in, pg_grp
    ! and the location of that pe within the group pp_grp
    ! Note each pe will have the same values for all these parameters
    !
    ! only the local group index, ip_grp, will not be the same on all pes at
    ! the end of the do_j0_grp_1 loop
    !
    do_j0_grp_1 : do j0_grp = 1 , p%par_groups
      !
      xxif_j0_grp_1 : if ( j0_grp .le. p%np_grp_1 ) then
        !
        p%pn_grp ( j0_grp ) = p%mp_grp
        !
      else xxif_j0_grp_1
        !
        p%pn_grp ( j0_grp ) = p%mp_grp - 1
        !
      end if xxif_j0_grp_1
      !
      ! first and last pes this group
      !
      !
      p%p1_grp ( j0_grp ) = pg_grp_0
      !
      p%p2_grp ( j0_grp ) = p%p1_grp ( j0_grp ) &
                          + p%pn_grp ( j0_grp ) - 1 
      !
      ! set pg_grp_0 to the index of the last pe
      ! this is pe value + 1
      !
      pg_grp_0            = p%p2_grp ( j0_grp ) + 1
      !
      ! make sure we have counted right
      !
      if ( .not. pc_do_not_process_traces() &
    .and. ( p%p2_grp ( j0_grp ) .gt. p%c2_pel &
     .or. ( p%p2_grp ( j0_grp ) .ne. p%c2_pel &
     .and. j0_grp .eq. p%par_groups ) ) ) go to 996
      !
      ! set the group index, pg_grp, for each pe in this group
      ! and the location within the group index, pp_grp for this pe
      ! set the local pe index within the group, ip_grp, for this pe
      ! ip_grp is local info, each pe has different values
      ! if there is an io_node its ip_grp value will not get set here and will
      ! be set below
      !
      pp_grp_0 = 0
      !
      do_j_pel_1 : &
      do j_pel = p%p1_grp ( j0_grp ) + 1 , p%p2_grp ( j0_grp ) + 1
        !
        pp_grp_0 = pp_grp_0 + 1
        !
        if ( j_pel .eq. fxpar_i_pel()+1 ) p%ip_grp = pp_grp_0 
        !
        p%pg_grp ( j_pel ) = j0_grp
        !
        p%pp_grp ( j_pel ) = pp_grp_0
        !
      end do do_j_pel_1
      !
    end do do_j0_grp_1
    !
    ! if there is an io_node c1_pel > 0
    ! then we have to set the group and index values for pe 0
    !
    xxif_io_node_2 : if ( p%c1_pel .gt. 0 ) then
      !
      if ( fxpar_i_pel() .eq. 0 ) p%ip_grp = 1 ! index of this pe in grp
      p%pg_grp ( 1 ) = 0    ! group index of this pe
      p%pp_grp ( 1 ) = 1    ! pe within group index list
      !
    end if xxif_io_node_2 
    !
    ! set the local group number, ig_grp for this pe
    !
    p%ig_grp = p%pg_grp ( fxpar_i_pel() + 1 )  ! group for this pe
    !
    ! set the number of pes in the group, np_grp, for this pe
    !
    xxif_io_node_3 : if ( p%ig_grp .eq. 0 ) then
      !
      p%np_grp = 1                          ! num pes in this pes group
      !
    else xxif_io_node_3 
      !
      p%np_grp = p%pn_grp ( p%ig_grp )  ! num pes in this pes group
      !
    end if xxif_io_node_3 
    !
    !print'(" p=",i4," j_pel=",i5," pg_grp=",i12)',&
    !(fxpar_i_pel(),j_pel,p%pg_grp(j_pel),j_pel=1,p%nc_pel)
    !
    ! set the first pe in this group, p0_grp 
    ! and make a list, jp_grp, of the pe s within this group
    !
    xxif_io_node_4 : if ( p%ig_grp .eq. 0 ) then
      !
      p%p0_grp = 0
      !
      p%jp_grp ( 1 ) = 0
      !
    else xxif_io_node_4  
      !
      p%p0_grp = p%p1_grp ( p%ig_grp )
      !
      p%jp_grp = 0 
      !
      ! create a list of pe s within this group
      !
      do_j_pel_2 : &
      do j_pel = p%p1_grp ( p%ig_grp ) , p%p2_grp ( p%ig_grp ) 
        !
        p%jp_grp ( j_pel - p%p1_grp ( p%ig_grp ) + 1 ) = j_pel
        !
      end do do_j_pel_2
      !
    end if xxif_io_node_4 
    !
    ! set the group root flag
    !
    p%group_root = .false.
    !
    ! set lock_file logic
    !
    p%lock_file  =.true.
    !
    ! setup operation type (used for plane wave composition )
    !
    p%opt_type = 'NULL'
    !
    if ( fxpar_i_pel() .eq. p%p0_grp ) &
    p%group_root = .true.
    !
    call fxpar_work_load_compute ( p, 'fxpar_prep' )
    !
    !  print the fxpar info
    !
    call fxpar_print_0 ( p, ' fxpar_prep ' )
    !
    ! check for a logical error in index computation
    !
    !if ( p%ig_grp .eq. 0 .or. p%ip_grp .eq. 0 ) go to 995
    !
    if ( p%ip_grp .eq. 0 ) go to 995
    !
    p%mem_computed = .true.
    !
    return
    !
1999 continue
    !
    call fxpar_check_worker_errors ( p, i_err )
    !
    if ( i_err .ne. 0 ) call fxpar_delete ( p )
    !
    call memfun_prn_res ( )
    !
    return
    !
995 continue
    !
    call pc_error ( ' fxpar_prep 995 error in ip_grp p=', fxpar_i_pel() )
    call pc_error ( ' ig_grp  =', p%ig_grp )
    call pc_error ( ' ip_grp  =', p%ip_grp )
    !
    go to 999
    !
996 continue
    !
    call pc_error ( ' fxpar_prep 996 error in p2_grp p=', fxpar_i_pel() )
    call pc_error ( ' num pes =', fxpar_n_pel()       )
    call pc_error ( ' nc_pel  =', p%nc_pel            )
    call pc_error ( ' mp_grp  =', p%mp_grp            )
    call pc_error ( ' np_grp_1=', p%np_grp_1          )
    call pc_error ( ' j0_grp  =',            j0_grp   )
    call pc_error ( ' pn_grp  =', p%pn_grp ( j0_grp ) )
    call pc_error ( ' p1_grp  =', p%p1_grp ( j0_grp ) )
    call pc_error ( ' p2_grp  =', p%p2_grp ( j0_grp ) )
    !
    go to 999
    !
997 continue
    !
    write ( pc_get_lun(), '( &
    & /, "error in fxpar_prep p=", i4, &
    & /, " during memory allocation " &
    & )' ) &
    fxpar_i_pel()
    !
    go to 999
    !
999 continue
    !
    call pc_error ( ' fxpar_prep error p=', fxpar_i_pel() )
    !
    i_err = -1
    !
    call fxpar_print_0 ( p, ' fxpar_prep ' )
    !
    go to 1999
    !
  end subroutine fxpar_prep
  !
  subroutine fxpar_print_0 ( p, c_title )
    !
    ! print the fxpar info
    !
    type ( fxpar_struct ),   pointer :: p              ! fxpar structure
    character(len=*),  intent(in   ) :: c_title
    !
    ! local variables
    !
    integer                          :: j_pel
    integer                          :: j0_grp
    integer, save                    :: i_call = 0
    !
    ! local variables
    !
    i_call = i_call + 1
    !
    !if ( fxpar_i_pel() .eq. 0 ) &
    !print'(" fxpar_print c=",i8, &
    !& /, " c_title=", a &
    !& )', &
    !i_call, trim ( c_title ) 
    !
    if ( fxpar_i_pel() .eq. 0 ) &
    write ( pc_get_lun(), '( &
    & /," fxpar_print_0 ", &
    & /, " fxpar REVISION: ", &
    & / " 17  2007-08-02 Douglas Hanson Remove print. " &
    & /," c_title=", a, &
    & /," cpus_per_group          =", i8, &
    & /," par_groups              =", i8, &
    & /," par_groups_old          =", i8, &
    & /," num_cpus                =", i8, &
    & /," load_on_root            =", f8.4, &
    & /," io_node                 =", l2, &
    & /," number of total pes     =", i8, &
    & /," number of compute pes   =", i8, &
    & /," first     compute pe    =", i8, &
    & /," last      compute pe    =", i8, &
    & /," number of groups        =", i8  &
    & )') &
    trim ( p%c_title ), &
    p%cpus_per_group, p%par_groups, p%par_groups_old, &
    p%num_cpus, p%load_on_root, p%io_node, &
    fxpar_n_pel(), &
    p%nc_pel, p%c1_pel, p%c2_pel, &
    p%par_groups
    !
    if ( fxpar_i_pel() .eq. 0 ) &
    write ( pc_get_lun() , '( &
    &   1x,i8," groups ",i8," - ",i8," have ",i8," pes", &
    & /,1x,i8," groups ",i8," - ",i8," have ",i8," pes"  &
    & )') &
    p%np_grp_1, 1, p%np_grp_1, p%mp_grp, &
    p%np_grp_2, p%np_grp_1+1, p%par_groups, p%mp_grp-1
    !
    !print'(" p=", i4, " ig_grp=", i8, " ip_grp=", i8, " np_grp=", i8 )', &
    !fxpar_i_pel(), p%ig_grp, p%ip_grp, p%np_grp
    !
    if ( fxpar_i_pel() .eq. 0 ) &
    write ( pc_get_lun() , '( &
    & /, " number of pes per group and first and last pe for each group ", &
    & /, "    index  num_pes first_pe  last_pe " &
    & )')
    !
    if ( fxpar_i_pel() .eq. 0 ) &
    write ( pc_get_lun() , '( &
    &   1x, i8, 1x, i8, 1x, i8, 1x, i8 &
    & )') &
    ( j0_grp, p%pn_grp ( j0_grp ), p%p1_grp ( j0_grp ), &
             p%p2_grp ( j0_grp ), &
      j0_grp = 1 , p%par_groups )
    !
    if ( fxpar_i_pel() .eq. 0 ) &
    write ( pc_get_lun() , '( &
    & /, " group index and location within each group for each pe ", &
    & /, "    index    group    pe in group  " &
    & )')
    !
    if ( fxpar_i_pel() .eq. 0 ) &
    write ( pc_get_lun() , '( &
    &   1x, i8, 1x, i8, 1x, i8 &
    & )') &
    ( j_pel, p%pg_grp(j_pel), p%pp_grp(j_pel), &
      j_pel = 1 , p%nc_pel )
    !
    return
    !
  end subroutine fxpar_print_0 
  !
  subroutine fxpar_reset_lock_file ( p, lock_file ) 
    !
    type ( fxpar_struct ),  pointer :: p
    !
    logical                         :: lock_file
    !
    p%lock_file = lock_file
    !
    return
    !
  end subroutine fxpar_reset_lock_file     
  !
  subroutine fxpar_copy ( f_inp, f_cop, i_err )
    !
    ! copy a fxpar_divde structure 
    !
    type ( fxpar_struct ),  pointer :: f_inp        ! fxpar  strucutre
    type ( fxpar_struct ),  pointer :: f_cop        ! fxpar  strucutre copy
    !
    integer,           intent(  out) :: i_err        ! error flag
    !
    ! Local variables
    !
    ! initialize the error flag, i_err
    ! this is only used for memory allocaiton
    !
    i_err = 0
    !
    if ( .not. associated ( f_cop ) ) &
                 allocate ( f_cop )
    !
    ! initialize parameters
    !
    call fxpar_initialize ( f_cop )
    !
    f_cop%mem_allocated = f_inp%mem_allocated 
    f_cop%mem_computed  = f_inp%mem_computed 
    !
    f_cop%c_title         = f_inp%c_title         ! char title
    f_cop%cpus_per_group  = f_inp%cpus_per_group  ! number of cpus per group
    f_cop%par_groups      = f_inp%par_groups      ! number of parrellel groups
    f_cop%par_groups_old  = f_inp%par_groups_old  ! number of parrellel groups
    f_cop%num_cpus        = f_inp%num_cpus        ! number of parrellel groups
    f_cop%load_on_root    = f_inp%load_on_root 
    f_cop%io_node         = f_inp%io_node    
    !
    f_cop%lock_file       = f_inp%lock_file
    f_cop%opt_type        = f_inp%opt_type
    f_cop%par_groups      = f_inp%par_groups 
    f_cop%group_root      = f_inp%group_root 
    f_cop%nc_pel          = f_inp%nc_pel 
    f_cop%c1_pel          = f_inp%c1_pel 
    f_cop%c2_pel          = f_inp%c2_pel 
    f_cop%p0_grp          = f_inp%p0_grp 
    f_cop%ig_grp          = f_inp%ig_grp 
    f_cop%ip_grp          = f_inp%ip_grp 
    f_cop%np_grp          = f_inp%np_grp 
    f_cop%mp_grp          = f_inp%mp_grp 
    f_cop%np_grp_1        = f_inp%np_grp_1   ! num grps w/mp_grp pes
    f_cop%np_grp_2        = f_inp%np_grp_2   ! num grps w/mp_grp-1 pes
    !
    ! allocate the memory
    !
    call fxpar_mem_all ( f_cop, i_err )
    !
    if ( i_err .ne. 0 ) go to 998
    !
    f_cop%pg_grp(1:fxpar_n_pel())    = f_inp%pg_grp(1:fxpar_n_pel()) 
    f_cop%pp_grp(1:fxpar_n_pel())    = f_inp%pp_grp(1:fxpar_n_pel()) 
    f_cop%jp_grp(1:fxpar_n_pel())    = f_inp%jp_grp(1:fxpar_n_pel()) 
    f_cop%wl_grp(1:fxpar_n_pel())    = f_inp%wl_grp(1:fxpar_n_pel()) 
    f_cop%pn_grp(1:f_cop%par_groups) = f_inp%pn_grp(1:f_inp%par_groups) 
    f_cop%p1_grp(1:f_cop%par_groups) = f_inp%p1_grp(1:f_inp%par_groups) 
    f_cop%p2_grp(1:f_cop%par_groups) = f_inp%p2_grp(1:f_inp%par_groups) 
    !
    return
    !
1999 continue
    !
    call fxpar_check_worker_errors ( f_inp, i_err )
    !
    if ( i_err .ne. 0 ) call fxpar_delete ( f_cop )
    !
    call memfun_prn_res ( )
    !
    return
    !
998 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxpar_copy p=", i4, " num pes=", i8 &
    & )' ) &
    fxpar_i_pel(), fxpar_n_pel()
    go to 999
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /,  " error in fxpar_copy p=", i4 &
    & )' ) &
    fxpar_i_pel()
    !
    i_err = -1
    !
    go to 1999
    !
  end subroutine fxpar_copy 
  !
  integer function fxpar_print ( )
    !
    ! set the print flag
    ! 1 = normal
    ! 2 = debug
    !
    xxif_fxpar_print : if ( fxpar_i_pel() .eq. 0 ) then
      !
      fxpar_print = 0
      !
    else xxif_fxpar_print
      !
      fxpar_print = 1
      !
    end if xxif_fxpar_print
    !
    return
    !
  end function fxpar_print
  ! 
  logical function fxpar_root_in_list ( i_root, np_grp, jp_grp )
    ! 
    ! determine if pe i_root is in the list jp_grp
    !
    integer,           intent(in   ) :: i_root      ! num pes this group
    integer,           intent(in   ) :: np_grp      ! num pes this group
    integer,           intent(in   ) :: jp_grp(:)   ! pe indicies this group
    !
    xxif_in_list : &
    if ( fxpar_index_in_list ( i_root, np_grp, jp_grp ) .ne. 0 ) then
      !
      fxpar_root_in_list = .true.
      !
    else xxif_in_list 
      !
      fxpar_root_in_list = .false.
      !
    end if xxif_in_list 
    !
    return
    !
  end function fxpar_root_in_list 
  ! 
  integer function fxpar_index_in_list ( i_root, np_grp, jp_grp )
    ! 
    ! the index of i_root in the list jp_grp
    !
    integer,           intent(in   ) :: i_root      ! num pes this group
    integer,           intent(in   ) :: np_grp      ! num pes this group
    integer,           intent(in   ) :: jp_grp(:)   ! pe indicies this group
    !
    integer                          :: i_pel
    !
    fxpar_index_in_list = 0
    !
    do_i_pel : do i_pel = 1 , np_grp
      !
      xxif_in_list : if ( i_root .eq. jp_grp ( i_pel ) ) then
        !
        fxpar_index_in_list = i_pel
        !
        return
        !
      end if xxif_in_list 
      !
    end do do_i_pel 
    !
    return
    !
  end function fxpar_index_in_list 
  !
  integer function fxpar_i_pel ( )
    !
    ! return the current pe index
    !
    fxpar_i_pel = pcpsx_i_pel()
    !
    return
    !
  end function fxpar_i_pel
  !
  integer function fxpar_n_pel ( )
    !
    ! return the number of pes
    !
    integer :: num_cpus
    !
    xxif_process_traces : if ( .not. pc_do_not_process_traces() ) then
      !
      fxpar_n_pel = pcpsx_n_pel()
      !
    else xxif_process_traces
      !
      call pc_get_jdata ( 'num_cpus', num_cpus )
      !
      fxpar_n_pel = num_cpus
      !
    end if xxif_process_traces
    !
    return
    !
  end function fxpar_n_pel
  !
  subroutine fxpar_work_load_compute ( p, c_title )
    !
    ! Compute the work load factor 
    !
    type ( fxpar_struct ),   pointer :: p          ! fxpar  strucutre
    character(len=*),  intent(in   ) :: c_title ! print title
    !
    ! Local variables
    !
    integer                          :: n0_pel
    real                             :: r0_pel
    real                             :: r1_pel
    !
    ! There are total of nw_mig frequencies to be imaged
    ! There are total of p%np_grp pes in this group
    ! These frequencies are distributed over the pes within the group
    ! each pe has an assigned work load of wl_grp ( ) which 
    ! can be adjusted for processor speed or other duties.
    ! For example the work load for the root pe may be reduced 
    ! so it can handle io duties as well.
    !
    ! Total work = 1. sum wl_grp
    ! Work of root pe, r0_pel, is load_on_root of others, r1_pel
    ! sum wl_grp = ( n0_pel - 1 ) * r1_pel + r0_pel
    ! sum wl_grp = ( n0_pel - 1 + r0_pel ) * r1_pel 
    ! r0_pel = load_on_root * r1_pel
    ! 1. = ( n0_pel - 1) * r1_pel + load_on_root * r1_pel
    ! r1_pel = 1. / ( n0_pel - 1 + max ( 0., min ( 1., load_on_root ) ) )
    !
    !if ( fxpar_i_pel() .eq. 0 ) &
    !print'( " fxpar_work_load_compute np_grp=",i8," load_on_root=", f8.4)', &
    !p%np_grp, p%load_on_root 
    !
    n0_pel = p%np_grp
    !
    r1_pel = 1. / ( n0_pel - 1 + max ( 0., min ( 1., p%load_on_root ) ) )
    !
    r0_pel = r1_pel * max ( 0., min ( 1., p%load_on_root ) )
    !
    p%wl_grp ( 1:p%np_grp ) = r1_pel
    !
    if ( n0_pel .eq. 1 ) r0_pel = 1.
    !
    p%wl_grp ( 1 ) = r0_pel
    !
    ! normalize the work load factors to sum to 1.
    !
    call fxpar_work_load_normalize ( p )
    !
    ! print he work load factors
    !
    call fxpar_work_load_print ( p, c_title )
    !
    return
    !
  end subroutine fxpar_work_load_compute 
  !
  subroutine fxpar_work_load_normalize ( p )
    !
    ! Normalize the work load factor to sum to 1.
    !
    type ( fxpar_struct ),   pointer :: p          ! fxpar  strucutre
    !
    ! Local variables
    !
    real                             :: wl_sum ! wum of work load factors
    !
    wl_sum = sum ( p%wl_grp ( 1:p%np_grp ) )
    !
    p%wl_grp ( 1:p%np_grp ) = &
    p%wl_grp ( 1:p%np_grp ) / wl_sum 
    !
    return
    !
  end subroutine fxpar_work_load_normalize 
  !
  subroutine fxpar_work_load_print ( p, c_title )
    !
    ! Print the work load factor 
    !
    type ( fxpar_struct ),   pointer :: p          ! fxpar  strucutre
    character(len=*),  intent(in   ) :: c_title ! print title
    !
    ! Local variables
    !
    integer                          :: j0_grp ! grp pe counter
    real                             :: wl_sum ! wum of work load factors
    !
    wl_sum = sum ( p%wl_grp ( 1:p%np_grp ) )
    !
    if ( p%group_root ) &
    print'( &
    & /, " fxpar_work_load_print title=", a, &
    & /, " fxpar_work_load_print group index        =", i8, &
    & /, " fxpar_work_load_print num   pes in group =", i8, &
    & /, " fxpar_work_load_print first pe  in group =", i8, &
    & /, " fxpar_work_load_print last  pe  in group =", i8, &
    & /, " fxpar_work_load_print work_load sum      =", g12.6, &
    & /, " fxpar_work_load_print group index pe work_load " &
    & )', &
    trim(c_title), &
    p%ig_grp, &
    p%np_grp, &
    p%p1_grp ( p%ig_grp ), &
    p%p2_grp ( p%ig_grp ), &
    wl_sum
    !
    if ( p%group_root ) &
    print'( &
    & " fxpar_work_load_print ", i8, 1x, i8, 1x, i8, 1x, g12.6 &
    & )', &
    ( p%ig_grp, j0_grp, p%jp_grp ( j0_grp ), p%wl_grp ( j0_grp ), &
      j0_grp = 1 , p%np_grp )
    !
    return
    !
  end subroutine fxpar_work_load_print
  !
  subroutine fxpar_check_worker_errors ( p, i_err )
    ! 
    ! sum all reduce data for a list of pes
    ! 4d real data ax_inp into ax_out, may be in place
    !
    type ( fxpar_struct ),   pointer :: p        ! fxpar  strucutre
    integer,           intent(inout) :: i_err      ! error flag

    !
    ! reduce the data to pe i_root
    !
    if( string_upper_compare(p%opt_type(1:5),'PSHOT')) &
    return         
    !
    if ( i_err .ne. 0 ) &
    call pc_error ( ' fxpar_check_worker_errors ' , i_err )
    !
    !call pcpsx_sum_all_reduce_group ( p%np_grp, p%jp_grp, i_err )
    !
    !j_err = 0
    !
    !call fxpar_sum_all_reduce_list ( p, i_err, j_err )
    !
    !i_err = j_err
    !
    return
    !
  end subroutine fxpar_check_worker_errors
  !
  subroutine fxpar_reset( p, opt_type )
    ! 
    ! reset the paramter
    !
    type ( fxpar_struct ),   pointer :: p        ! fxpar  strucutre
    character (len= *)               :: opt_type
    !
    p%opt_type = opt_type
    !
    return
    !
  end subroutine fxpar_reset
  !
  subroutine fxpar_send_r1 ( i_send, i_recv, n_data, ax_inp )
    ! 
    ! send ax_inp from i_send to i_recv
    ! 1d real data ax_inp 
    !
    integer,           intent(in   ) :: i_send      ! send pe
    integer,           intent(in   ) :: i_recv      ! recv pe
    integer,           intent(in   ) :: n_data      ! num in ax_inp to reduce
    real,              intent(inout) :: ax_inp(:)   !  input data 
    !
    ! initialize the output data on the root pe
    !
    if ( i_send .eq. i_recv ) return
    !
    xxif_send : if ( fxpar_i_pel() .eq. i_send ) then
      !
      ! send    data from i_send to   i_root
      !
      call pcpsx_send_data    ( i_recv, n_data, ax_inp, 22 )
      !
    else if ( fxpar_i_pel() .eq. i_recv ) then
      !
      ! receive data at   i_recv from i_send
      !
      call pcpsx_receive_data ( i_send, n_data, ax_inp, 22 )
      !
    end if xxif_send 
    !
  end subroutine fxpar_send_r1 
  !
end module fxpar_module
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
