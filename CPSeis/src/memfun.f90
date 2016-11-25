!
!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- MEMFUN.f90 --------------------------------!!
!!------------------------------- MEMFUN.f90 --------------------------------!!
!!------------------------------- MEMFUN.f90 --------------------------------!!

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
! Name       : MEMFUN 
! Category   : MEMORY
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2005-01-31   by: Douglas Hanson Add memfun_print.
! Maturity   : production
! Purpose    : memory allocationa nd deallocation with counters and prints
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! memory allocationa nd deallocation with counters and prints
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
!   i = intent(in)    = value required upon INPUT.
!   o = intent(out)   = value set by the routine upon OUTPUT.
!   b = intent(inout) = value BOTH required upon input and changed upon output.
!
! Optional arguments are also flagged as follows:
!   opt = this argument is optional.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE               
!
!                  o                i     b      o
!                hello = MEMFUN     (aaaa, cvar , msg)
!
!                                   i     b      o
!                call    MEMFUN_YYY (bbbb, cvar , msg)
!
!                                        opt    opt
!                                   i     i      o
!                call    MEMFUN_ZZZ (bbbb, indx, value)
!
!
! character(len=*)           aaaa(*) =    --> description 
! character(len=8),pointer   bbbb(:) =    --> description 
! double precision           cvar    =    --> description
! character(len=*)           msg     =    --> description 
! integer                    hello   =    --> description
! integer         ,optional  indx    =    --> description
! double precision,optional  value   =    --> description
!
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
!     Date        Author         Description
!     ----        ------         -----------
! 13  2005-01-31  Douglas Hanson Add memfun_print.
! 12  2003-09-30  R.S.Day        removed factor of 2 from memfun_all_c0,
!                                memfun_del_c0 memory size calculation.
! 11  2003-07-11  Douglas Hanson Add memfun_sys_mem_prn 
! 10  2003-06-05  Bill Done      Add option parameter init_value to _init
!                                and _all routines to allow input to be
!                                initialized to user specified value. If
!                                not specified, current default value is
!                                used.
!                                add memfun_size 
!  9  2003-04-11  Douglas Hanson Add init routines.
!  8  2003-02-27  Douglas Hanson Fix memfun_all_r3 print'
!  7  2002-09-19  Douglas Hanson Remove delete before nullify.
!     2002-05-14  Douglas Hanson Initialize memory after alloc.
!     2002-04-17  Douglas Hanson Add l1 functions.
!  6  2002-02-04  Douglas Hanson Add some prints.
!  5  2001-03-21  Douglas Hanson fix size 4 error
!  4  2001-01-10  Douglas Hanson add 5 d functions
!  3  2000-09-22  Douglas Hanson add memory_prn_off, res
!  2  2000-08-25  Douglas Hanson cpsfcr
!  1  1999-12-01  Douglas Hanson Initial version.
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
!
module memfun_module
  !
  ! Memory allocation routines
  !
  use getsys_module
  use mem_module
  use named_constants_module
  use pc_module
  use pcps_module
  use string_module
  !
  implicit  none
  !
  ! set default to all routines public
  !
  public
  !
  ! set default to all routines private
  !
  !private
  !
  ! subroutines
  !
  public :: memfun_sys_mem_prn      ! print sysytem memory usage
  public :: memfun_sys_mem          ! memory on per cpu
  public :: memfun_print          ! print memory on per cpu
  public :: memfun_size             ! memory size
  public :: memfun_sum_put          ! put the memory sum counter
  public :: memfun_sum_get          ! get the memory sum counter
  public :: memfun_prn_put          ! put the print flag .true. or .false.
  public :: memfun_prn_get          ! put the print flag .true. or .false.
  public :: memfun_prn_on           ! set the print flag .true.
  public :: memfun_prn_off          ! set the print flag .false.
  public :: memfun_prn_res          ! restore the print flag to previous
  public :: memfun_prn_all          ! print memory allocation message
  public :: memfun_all_c0           !   allocate 1d character         array
  public :: memfun_del_c0           ! deallocate 1d character         array
  public :: memfun_nul_c0           ! nullify    1d character         array
  public :: memfun_all_l1           !   allocate 1d logical           array
  public :: memfun_del_l1           ! deallocate 1d logical           array
  public :: memfun_nul_l1           ! nullify    1d logical           array
  public :: memfun_all_r1           !   allocate 1d real              array
  public :: memfun_del_r1           ! deallocate 1d real              array
  public :: memfun_nul_r1           ! nullify    1d real              array
  public :: memfun_all_i1           !   allocate 1d integer           array
  public :: memfun_del_i1           ! deallocate 1d integer           array
  public :: memfun_nul_i1           ! nullify    1d integer           array
  public :: memfun_all_c1           !   allocate 1d complex           array
  public :: memfun_del_c1           ! deallocate 1d complex           array
  public :: memfun_nul_c1           ! nullify    1d complex           array
  public :: memfun_all_d1           !   allocate 1d double precicison array
  public :: memfun_del_d1           ! deallocate 1d double precicison array
  public :: memfun_nul_d1           ! nullify    1d double precicison array
  public :: memfun_all_r2           !   allocate 2d real              array
  public :: memfun_del_r2           ! deallocate 2d real              array
  public :: memfun_nul_r2           ! nullify    2d real              array
  public :: memfun_all_i2           !   allocate 2d integer           array
  public :: memfun_del_i2           ! deallocate 2d integer           array
  public :: memfun_nul_i2           ! nullify    2d integer           array
  public :: memfun_all_c2           !   allocate 2d complex           array
  public :: memfun_del_c2           ! deallocate 2d complex           array
  public :: memfun_nul_c2           ! nullify    2d complex           array
  public :: memfun_all_d2           !   allocate 2d double precicison array
  public :: memfun_del_d2           ! deallocate 2d double precicison array
  public :: memfun_nul_d2           ! nullify    2d double precicison array
  public :: memfun_all_r3           !   allocate 3d real              array
  public :: memfun_del_r3           ! deallocate 3d real              array
  public :: memfun_nul_r3           ! nullify    3d real              array
  public :: memfun_all_i3           !   allocate 3d integer           array
  public :: memfun_del_i3           ! deallocate 3d integer           array
  public :: memfun_nul_i3           ! nullify    3d integer           array
  public :: memfun_all_c3           !   allocate 3d complex           array
  public :: memfun_del_c3           ! deallocate 3d complex           array
  public :: memfun_nul_c3           ! nullify    3d complex           array
  public :: memfun_all_d3           !   allocate 3d double precicison array
  public :: memfun_del_d3           ! deallocate 3d double precicison array
  public :: memfun_nul_d3           ! nullify    3d double precicison array
  public :: memfun_all_r4           !   allocate 4d real              array
  public :: memfun_del_r4           ! deallocate 4d real              array
  public :: memfun_nul_r4           ! nullify    4d real              array
  public :: memfun_all_i4           !   allocate 4d integer           array
  public :: memfun_del_i4           ! deallocate 4d integer           array
  public :: memfun_nul_i4           ! nullify    4d integer           array
  public :: memfun_all_c4           !   allocate 4d complex           array
  public :: memfun_del_c4           ! deallocate 4d complex           array
  public :: memfun_nul_c4           ! nullify    4d complex           array
  public :: memfun_all_d4           !   allocate 4d double precicison array
  public :: memfun_del_d4           ! deallocate 4d double precicison array
  public :: memfun_nul_d4           ! nullify    4d double precicison array
  public :: memfun_all_r5           !   allocate 5d real              array
  public :: memfun_del_r5           ! deallocate 5d real              array
  public :: memfun_nul_r5           ! nullify    5d real              array
  public :: memfun_all_i5           !   allocate 5d integer           array
  public :: memfun_del_i5           ! deallocate 5d integer           array
  public :: memfun_nul_i5           ! nullify    5d integer           array
  public :: memfun_all_c5           !   allocate 5d complex           array
  public :: memfun_del_c5           ! deallocate 5d complex           array
  public :: memfun_nul_c5           ! nullify    5d complex           array
  public :: memfun_all_d5           !   allocate 5d double precicison array
  public :: memfun_del_d5           ! deallocate 5d double precicison array
  public :: memfun_nul_d5           ! nullify    5d double precicison array
  !
  ! functions
  !
  ! saved parameters
  !
  logical, private, save :: memory_print = .false.
  logical, private, save :: memory_restore = .false.
  integer, private, save :: n_mem_sum = 0
  !
  character (len=100), public, save :: MEMFUN_IDENT = &
  '$Id: memfun.f90,v 1.13 2005/01/31 14:06:39 Hanson prod sps $'
  !
  ! interfaces 
  !
  interface memfun_all
    !
    module procedure  memfun_all_c0
    module procedure  memfun_all_l1
    module procedure  memfun_all_r1
    module procedure  memfun_all_i1
    module procedure  memfun_all_c1
    module procedure  memfun_all_d1
    module procedure  memfun_all_l2
    module procedure  memfun_all_r2
    module procedure  memfun_all_i2
    module procedure  memfun_all_c2
    module procedure  memfun_all_d2
    module procedure  memfun_all_l3
    module procedure  memfun_all_r3
    module procedure  memfun_all_i3
    module procedure  memfun_all_c3
    module procedure  memfun_all_d3
    module procedure  memfun_all_l4
    module procedure  memfun_all_r4
    module procedure  memfun_all_i4
    module procedure  memfun_all_c4
    module procedure  memfun_all_d4
    module procedure  memfun_all_l5
    module procedure  memfun_all_r5
    module procedure  memfun_all_i5
    module procedure  memfun_all_c5
    module procedure  memfun_all_d5
    !
  end interface
  !
  interface memfun_del
    !
    module procedure  memfun_del_c0
    module procedure  memfun_del_l1
    module procedure  memfun_del_r1
    module procedure  memfun_del_i1
    module procedure  memfun_del_c1
    module procedure  memfun_del_d1
    module procedure  memfun_del_l2
    module procedure  memfun_del_r2
    module procedure  memfun_del_i2
    module procedure  memfun_del_c2
    module procedure  memfun_del_d2
    module procedure  memfun_del_l3
    module procedure  memfun_del_r3
    module procedure  memfun_del_i3
    module procedure  memfun_del_c3
    module procedure  memfun_del_d3
    module procedure  memfun_del_l4
    module procedure  memfun_del_r4
    module procedure  memfun_del_i4
    module procedure  memfun_del_c4
    module procedure  memfun_del_d4
    module procedure  memfun_del_l5
    module procedure  memfun_del_r5
    module procedure  memfun_del_i5
    module procedure  memfun_del_c5
    module procedure  memfun_del_d5
    !
  end interface
  !
  interface memfun_nul
    !
    module procedure  memfun_nul_c0
    module procedure  memfun_nul_r1
    module procedure  memfun_nul_i1
    module procedure  memfun_nul_l1
    module procedure  memfun_nul_c1
    module procedure  memfun_nul_d1
    module procedure  memfun_nul_r2
    module procedure  memfun_nul_i2
    module procedure  memfun_nul_l2
    module procedure  memfun_nul_c2
    module procedure  memfun_nul_d2
    module procedure  memfun_nul_r3
    module procedure  memfun_nul_i3
    module procedure  memfun_nul_c3
    module procedure  memfun_nul_d3
    module procedure  memfun_nul_r4
    module procedure  memfun_nul_i4
    module procedure  memfun_nul_l4
    module procedure  memfun_nul_c4
    module procedure  memfun_nul_d4
    module procedure  memfun_nul_r5
    module procedure  memfun_nul_i5
    module procedure  memfun_nul_l5
    module procedure  memfun_nul_c5
    module procedure  memfun_nul_d5
    !
  end interface
  !
  interface memfun_init
    !
    module procedure  memfun_init_c
    module procedure  memfun_init_i
    module procedure  memfun_init_r
    module procedure  memfun_init_d
    module procedure  memfun_init_l
    module procedure  memfun_init_l1
    module procedure  memfun_init_l2
    module procedure  memfun_init_l3
    module procedure  memfun_init_l4
    module procedure  memfun_init_l5
    module procedure  memfun_init_i1
    module procedure  memfun_init_i2
    module procedure  memfun_init_i3
    module procedure  memfun_init_i4
    module procedure  memfun_init_i5
    module procedure  memfun_init_r1
    module procedure  memfun_init_r2
    module procedure  memfun_init_r3
    module procedure  memfun_init_r4
    module procedure  memfun_init_r5
    module procedure  memfun_init_d1
    module procedure  memfun_init_d2
    module procedure  memfun_init_d3
    module procedure  memfun_init_d4
    module procedure  memfun_init_d5
    !
  end interface
  !
  contains
  !
  integer function memfun_size ( )
    !
    ! get the memory sum counter
    !
    memfun_size = n_mem_sum
    !
    !if ( memory_print ) &
    !write ( pc_get_lun(), '( &
    !& " memfun_size total=", i12 &
    !& )') &
    !& n_mem_sum
    !
    !print'(" memfun_size total=", i12 &
    !& )', &
    !& n_mem_sum
    !
    return
    !
  end function memfun_size 
  !
  subroutine memfun_sum_put ( n_mem_sum_0 )
    !
    ! put the memory sum counter
    !
    integer,   intent (in   ) :: n_mem_sum_0
    !
    if ( memory_print ) &
    write ( pc_get_lun(), '( &
    & " memfun_sum_put old total=", i12, " new total=", i12 &
    & )') &
    & n_mem_sum, n_mem_sum_0
    !
    !print'(" memfun_sum_put old total=", i12, " new total=", i12 &
    !& )', &
    !& n_mem_sum, n_mem_sum_0
    !
    n_mem_sum = n_mem_sum_0
    !
    return
    !
  end subroutine memfun_sum_put
  !
  subroutine memfun_sum_get ( n_mem_sum_0 )
    !
    ! get the memory sum counter
    !
    integer,   intent (inout) :: n_mem_sum_0
    !
    n_mem_sum_0 = memfun_size ( )
    !
    if ( memory_print ) &
    write ( pc_get_lun(), '( &
    & " memfun_sum_get total=", i12 &
    & )') &
    & n_mem_sum
    !
    !print'(" memfun_sum_get old total=", i12, " new total=", i12 &
    !& )', &
    !& n_mem_sum, n_mem_sum_0
    !
    return
    !
  end subroutine memfun_sum_get
  !
  subroutine memfun_prn_get ( memory_print_0 )
    !
    ! get the memory flag to true or false
    !
    logical,   intent (inout) :: memory_print_0
    !
    memory_print_0 = memory_print
    !
    return
    !
  end subroutine memfun_prn_get
  !
  subroutine memfun_prn_put ( memory_print_0 )
    !
    ! put the memory flag to true or false
    !
    logical,   intent (in   ) :: memory_print_0
    !
    memory_print = memory_print_0
    !
    memory_restore = memory_print
    !
    !write ( pc_get_lun(), * ) &
    !& ' memfun_prn_put memory_print=', memory_print
    !
    return
    !
  end subroutine memfun_prn_put
  !
  subroutine memfun_prn_on ( )
    !
    ! save the current status of memory_print into memory_restore
    ! set the memory flag to true
    !
    !
    memory_restore = memory_print
    !
    memory_print = .true.
    !
    !write ( pc_get_lun(), * ) &
    !& ' memfun_prn_put memory_print=', memory_print
    !
    return
    !
  end subroutine memfun_prn_on
  !
  subroutine memfun_prn_off ( )
    !
    ! save the current status of memory_print into memory_restore
    ! set the memory flag to false
    !
    memory_restore = memory_print
    memory_print = .false.
    !
    !write ( pc_get_lun(), * ) &
    !& ' memfun_prn_put memory_print=', memory_print
    !
    return
    !
  end subroutine memfun_prn_off
  !
  subroutine memfun_prn_res ( )
    !
    ! restore the previouys memory_print status
    !
    memory_print = memory_restore
    !
    return
    !
  end subroutine memfun_prn_res
  !
  subroutine memfun_prn_all ( c_title, n_mem, c_mem)
    !
    ! print message for memory allocation
    !
    character(len=*), intent (in   ) :: c_title
    integer,          intent (in   ) :: n_mem
    character(len=*), intent (in   ) :: c_mem
    !
    integer                         :: vsize
    integer                         :: rss 
    integer                         :: used_mem
    !
    call memfun_sys_mem ( vsize, rss, used_mem )
    !
    ! print memory usage message
    !
    if ( memory_print .and. .not. pc_do_not_process_traces() ) &
    write ( pc_get_lun(), '( &
    & " memfun_all tot=", i12, " req=", i12, &
    & " v=", i8, " r=", i8," d=", i8, &
    & " t=", a2, 1x, a &
    & )') &
    n_mem_sum, n_mem, &
    vsize, rss, used_mem, &
    c_title(1:min(4, len_trim(c_title))), &
    trim(c_mem)
    !
    if ( memory_print .and. pc_do_not_process_traces() ) &
    print'( &
    & " memfun_all tot=", i12, " req=", i12, &
    & " v=", i8, " r=", i8," d=", i8, &
    & " t=", a2, 1x, a &
    & )', &
    n_mem_sum, n_mem, &
    vsize, rss, used_mem, &
    c_title(1:min(4, len_trim(c_title))), &
    trim(c_mem)
    !
    return
    !
  end subroutine memfun_prn_all
  !
  subroutine memfun_all_c0 ( x_mem, n1, c_mem, i_err, init_value )
    !
    ! allocate 1d character array
    !
    character(len=*), pointer        :: x_mem(:)
    integer,          intent (in   ) :: n1
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    character(len=*), optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_c0 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = ' '
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_c0" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n_mem, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'c0', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_c0
  !
  subroutine memfun_del_c0 ( x_mem )
! deallocate 1d character array
!
    character(len=*), pointer        :: x_mem(:)
    !
! decrement sum
    if ( associated ( x_mem ) ) n_mem_sum = max(0, n_mem_sum &
     & - size(x_mem,1) )
     !
! deallocate memory
    call mem_free ( x_mem )
    !
    return
  end subroutine memfun_del_c0
  !
  subroutine memfun_nul_c0 ( x_mem )
! nullify 1d real array
!
    character(len=*), pointer        :: x_mem(:)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_c0
  !
  subroutine memfun_all_l1 ( x_mem, n1, c_mem, i_err, init_value )
! allocate 1d real array
!
    logical,          pointer        :: x_mem(:)
    integer,          intent (in   ) :: n1
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    logical, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_l1 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = .true.
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_l1" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n_mem, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'l1', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_l1
  !
  subroutine memfun_del_l1 ( x_mem )
! deallocate 1d real array
!
    logical,          pointer        :: x_mem(:)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_l1
  !
  subroutine memfun_nul_l1 ( x_mem )
! nullify 1d real array
!
    logical,          pointer        :: x_mem(:)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_l1
  !
  subroutine memfun_all_r1 ( x_mem, n1, c_mem, i_err, init_value )
! allocate 1d real array
!
    real,             pointer        :: x_mem(:)
    integer,          intent (in   ) :: n1
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    real, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_r1 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_r1" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n_mem, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'r1', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_r1
  !
  subroutine memfun_del_r1 ( x_mem )
! deallocate 1d real array
!
    real,             pointer        :: x_mem(:)
    integer                          :: n_mem_del,err
    !
    if ( associated ( x_mem ) ) then
      deallocate ( x_mem )
    endif
    
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * size(x_mem,1)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_r1
  !
  subroutine memfun_nul_r1 ( x_mem )
! nullify 1d real array
!
    real,             pointer        :: x_mem(:)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_r1
  !
  subroutine memfun_all_i1 ( x_mem, n1, c_mem, i_err, init_value )
! allocate 1d integer array
!
    integer,          pointer        :: x_mem(:)
    integer,          intent (in   ) :: n1
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    integer, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_i1 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_i1" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n_mem, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'i1', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_i1
  !
  subroutine memfun_del_i1 ( x_mem )
! deallocate 1d integer array
!
    integer,          pointer        :: x_mem(:)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_i1
  !
  subroutine memfun_nul_i1 ( x_mem )
! nullify 1d real array
!
    integer,          pointer        :: x_mem(:)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_i1
  !
  subroutine memfun_all_c1 ( x_mem, n1, c_mem, i_err, init_value )
! allocate 1d complex array
!
    complex,          pointer        :: x_mem(:)
    integer,          intent (in   ) :: n1
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    complex, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_c1 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * 2
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = cmplx(0.,0.)
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_c1" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n_mem, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'c1', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_c1
  !
  subroutine memfun_del_c1 ( x_mem )
! deallocate 1d complex array
!
    complex,          pointer        :: x_mem(:)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 2 * &
size(x_mem,1)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_c1
  !
  subroutine memfun_nul_c1 ( x_mem )
! nullify 1d real array
!
    complex,          pointer        :: x_mem(:)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_c1
  !
  subroutine memfun_all_d1 ( x_mem, n1, c_mem, i_err, init_value )
! allocate 1d double precision array
!
    double precision, pointer        :: x_mem(:)
    integer,          intent (in   ) :: n1
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    double precision, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_d1 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * 2
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_d1" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n_mem, trim(c_mem)
  !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'd1', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_d1
  !
  subroutine memfun_del_d1 ( x_mem )
! deallocate 1d double precision array
!
    double precision, pointer        :: x_mem(:)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 2 * &
size(x_mem,1)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_d1
  !
  subroutine memfun_nul_d1 ( x_mem )
! nullify 1d real array
!
    double precision, pointer        :: x_mem(:)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_d1
  !
  subroutine memfun_all_r2 ( x_mem, n1, n2, c_mem, i_err, init_value )
! allocate 2d real array
!
    real,             pointer        :: x_mem(:, :)
    integer,          intent (in   ) :: n1, n2
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    real, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_r2 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, n2, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_r2" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'r2', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_r2
  !
  subroutine memfun_del_r2 ( x_mem )
! deallocate 2d real array
!
    real,             pointer        :: x_mem(:, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1) * size(x_mem,2)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_r2
  !
  subroutine memfun_nul_r2 ( x_mem )
! nullify 2d real array
!
    real,             pointer        :: x_mem(:, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_r2
  !
  subroutine memfun_all_i2 ( x_mem, n1, n2, c_mem, i_err, init_value )
! allocate 2d integer array
!
    integer,          pointer        :: x_mem(:, :)
    integer,          intent (in   ) :: n1, n2
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    integer, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_i2 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, n2, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_i2" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'i2', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_i2
  !
  subroutine memfun_del_i2 ( x_mem )
! deallocate 2d integer array
!
    integer,          pointer        :: x_mem(:, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1) * size(x_mem,2)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_i2
  !
  subroutine memfun_nul_i2 ( x_mem )
! nullify 2d real array
!
    integer,          pointer        :: x_mem(:, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_i2
  !
  subroutine memfun_all_l2 ( x_mem, n1, n2, c_mem, i_err, init_value )
! allocate 2d logical array
!
    logical,          pointer        :: x_mem(:, :)
    integer,          intent (in   ) :: n1, n2
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    logical, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_l2 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, n2, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = .true.
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_l2" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'l2', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_l2
  !
  subroutine memfun_del_l2 ( x_mem )
! deallocate 2d logical array
!
    logical,          pointer        :: x_mem(:, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1) * size(x_mem,2)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_l2
  !
  subroutine memfun_nul_l2 ( x_mem )
! nullify 2d logical array
!
    logical,          pointer        :: x_mem(:, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_l2
  !
  subroutine memfun_all_c2 ( x_mem, n1, n2, c_mem, i_err, init_value )
! allocate 2d complex array
!
    complex,          pointer        :: x_mem(:, :)
    integer,          intent (in   ) :: n1, n2
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    complex, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_c2 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * 2
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, n2, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = cmplx(0.,0.)
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_c2" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'c2', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_c2
  !
  subroutine memfun_del_c2 ( x_mem )
! deallocate 2d complex array
!
    complex,          pointer        :: x_mem(:, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 2 * &
size(x_mem,1) * size(x_mem,2)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_c2
  !
  subroutine memfun_nul_c2 ( x_mem )
! nullify 2d real array
!
    complex,          pointer        :: x_mem(:, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_c2
  !
  subroutine memfun_all_d2 ( x_mem, n1, n2, c_mem, i_err, init_value )
! allocate 2d double precision array
!
    double precision, pointer        :: x_mem(:, :)
    integer,          intent (in   ) :: n1, n2
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    double precision, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_d2 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * 2
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, n2, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_d2" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2, trim(c_mem)
  !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'd2', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_d2
  !
  subroutine memfun_del_d2 ( x_mem )
! deallocate 2d double precision array
!
    double precision, pointer        :: x_mem(:, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 2 * &
size(x_mem,1) * size(x_mem,2)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
    return
  end subroutine memfun_del_d2
  !
  subroutine memfun_nul_d2 ( x_mem )
! nullify 2d real array
!
    double precision, pointer        :: x_mem(:, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_d2
  !
  subroutine memfun_all_r3 ( x_mem, n1, n2, n3, c_mem, i_err, init_value )
! allocate 3d real array
!
    real,             pointer        :: x_mem(:, :, :)
    integer,          intent (in   ) :: n1, n2, n3
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    real, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_r3 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, n2, n3, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_r3" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'r3', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_r3
  !
  subroutine memfun_del_r3 ( x_mem )
! deallocate 3d real array
!
    real,             pointer        :: x_mem(:, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_r3
  !
  subroutine memfun_nul_r3 ( x_mem )
! nullify 3d real array
!
    real,             pointer        :: x_mem(:, :, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_r3
  !
  subroutine memfun_all_i3 ( x_mem, n1, n2, n3, c_mem, i_err, init_value )
! allocate 3d integer array
!
    integer,          pointer        :: x_mem(:, :, :)
    integer,          intent (in   ) :: n1, n2, n3
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    integer, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_i3 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, n2, n3, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_i3" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'i3', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_i3
  !
  subroutine memfun_del_i3 ( x_mem )
! deallocate 3d integer array
!
    integer,          pointer        :: x_mem(:, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_i3
  !
  subroutine memfun_nul_i3 ( x_mem )
! nullify 3d real array
!
    integer,          pointer        :: x_mem(:, :, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_i3
  !
  subroutine memfun_all_l3 ( x_mem, n1, n2, n3, c_mem, i_err, init_value )
! allocate 3d logical array
!
    logical,          pointer        :: x_mem(:, :, :)
    integer,          intent (in   ) :: n1, n2, n3
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    logical, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_l3 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, n2, n3, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = .true.
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_l3" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'l3', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_l3
  !
  subroutine memfun_del_l3 ( x_mem )
! deallocate 3d integer array
!
    logical,          pointer        :: x_mem(:, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_l3
  !
  subroutine memfun_nul_l3 ( x_mem )
! nullify 3d real array
!
    logical,          pointer        :: x_mem(:, :, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_l3
  !
  subroutine memfun_all_c3 ( x_mem, n1, n2, n3, c_mem, i_err, init_value )
! allocate 3d complex array
!
    complex,          pointer        :: x_mem(:, :, :)
    integer,          intent (in   ) :: n1, n2, n3
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    complex, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_c3 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3 * 2
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, n2, n3, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = cmplx(0.,0.)
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_c2" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'c3', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_c3
  !
  subroutine memfun_del_c3 ( x_mem )
! deallocate 3d complex array
!
    complex,          pointer        :: x_mem(:, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 2 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_c3
  !
  subroutine memfun_nul_c3 ( x_mem )
! nullify 3d real array
!
    complex,          pointer        :: x_mem(:, :, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_c3
  !
  subroutine memfun_all_d3 ( x_mem, n1, n2, n3, c_mem, i_err, init_value )
! allocate 3d double precision array
!
    double precision, pointer        :: x_mem(:, :, :)
    integer,          intent (in   ) :: n1, n2, n3
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    double precision, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_d3 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3 * 2
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    call mem_alloc ( x_mem, n1, n2, n3, j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_d2" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3, trim(c_mem)
  !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'd3', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_d3
  !
  subroutine memfun_del_d3 ( x_mem )
! deallocate 3d double precision array
!
    double precision, pointer        :: x_mem(:, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 2 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_d3
  !
  subroutine memfun_nul_d3 ( x_mem )
! nullify 3d real array
!
    double precision, pointer        :: x_mem(:, :, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_d3
  !
  subroutine memfun_all_r4 ( x_mem, n1, n2, n3, n4, c_mem, i_err, init_value )
! allocate 4d real array
!
    real,             pointer        :: x_mem(:, :, :, :)
    integer,          intent (in   ) :: n1, n2, n3, n4
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    real, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_r4 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3 * n4
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    allocate ( x_mem(n1, n2, n3, n4) , stat=j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_r2" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'r4', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_r4
  !
  subroutine memfun_del_r4 ( x_mem )
! deallocate 4d real array
!
    real,             pointer        :: x_mem(:, :, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3) * size(x_mem,4)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_r4
  !
  subroutine memfun_nul_r4 ( x_mem )
! nullify 4d real array
!
    real,             pointer        :: x_mem(:, :, :, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_r4
  !
  subroutine memfun_all_i4 ( x_mem, n1, n2, n3, n4, c_mem, i_err, init_value )
! allocate 4d integer array
!
    integer,          pointer        :: x_mem(:, :, :, :)
    integer,          intent (in   ) :: n1, n2, n3, n4
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    integer, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_i4 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3 * n4
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    allocate ( x_mem(n1, n2, n3, n4) , stat=j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_l4" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3* n4, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'i4', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_i4
  !
  subroutine memfun_del_i4 ( x_mem )
! deallocate 4d integer array
!
    integer,          pointer        :: x_mem(:, :, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3) * size(x_mem,4)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
    return
  end subroutine memfun_del_i4
  !
  subroutine memfun_nul_i4 ( x_mem )
! nullify 4d real array
!
    integer,          pointer        :: x_mem(:, :, :, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_i4
  !
  subroutine memfun_all_l4 ( x_mem, n1, n2, n3, n4, c_mem, i_err, init_value )
    ! allocate 4d logical array
    !
    logical,          pointer        :: x_mem(:, :, :, :)
    integer,          intent (in   ) :: n1, n2, n3, n4
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    logical, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_l4 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3 * n4
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    allocate ( x_mem(n1, n2, n3, n4) , stat=j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = .true.
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_il4" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3* n4, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'l4', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_l4
  !
  subroutine memfun_del_l4 ( x_mem )
! deallocate 4d logical array
!
    logical,          pointer        :: x_mem(:, :, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3) * size(x_mem,4)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
    return
  end subroutine memfun_del_l4
  !
  subroutine memfun_nul_l4 ( x_mem )
! nullify 4d logical array
!
    logical,          pointer        :: x_mem(:, :, :, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_l4
  !
  subroutine memfun_all_c4 ( x_mem, n1, n2, n3, n4, c_mem, i_err, init_value )
! allocate 4d complex array
!
    complex,          pointer        :: x_mem(:, :, :, :)
    integer,          intent (in   ) :: n1, n2, n3, n4
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    complex, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_c4 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3 * n4 * 2
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    allocate ( x_mem(n1, n2, n3, n4) , stat=j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = cmplx(0.,0.)
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_c2" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3* n4, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'c4', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_c4
  !
  subroutine memfun_del_c4 ( x_mem )
! deallocate 4d complex array
!
    complex,          pointer        :: x_mem(:, :, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 2 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3) * size(x_mem,4)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_c4
  !
  subroutine memfun_nul_c4 ( x_mem )
! nullify 4d real array
!
    complex,          pointer        :: x_mem(:, :, :, :)
    !
! nullify memory
    nullify  ( x_mem )
    !
    return
  end subroutine memfun_nul_c4
  !
  subroutine memfun_all_d4 ( x_mem, n1, n2, n3, n4, c_mem, i_err, init_value )
! allocate 4d double precision array
!
    double precision, pointer        :: x_mem(:, :, :, :)
    integer,          intent (in   ) :: n1, n2, n3, n4
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    double precision, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_d4 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3 * n4 * 2
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    allocate ( x_mem(n1, n2, n3, n4) , stat=j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_d2" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3* n4, trim(c_mem)
  !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'd4', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_d4
  !
  subroutine memfun_del_d4 ( x_mem )
! deallocate 4d double precision array
!
    double precision, pointer        :: x_mem(:, :, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 2 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3) * size(x_mem,4)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_d4
  !
  subroutine memfun_nul_d4 ( x_mem )
! nullify 4d real array
!
    double precision, pointer        :: x_mem(:, :, :, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_d4
  !
  subroutine memfun_all_r5 (x_mem, n1, n2, n3, n4, n5, c_mem, i_err, init_value)
! allocate 4d real array
!
    real,             pointer        :: x_mem(:, :, :, :, :)
    integer,          intent (in   ) :: n1, n2, n3, n4, n5
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    real, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_r5 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3 * n4 * n5
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    allocate ( x_mem(n1, n2, n3, n4, n5) , stat=j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_r2" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'r5', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_r5
  !
  subroutine memfun_del_r5 ( x_mem )
! deallocate 4d real array
!
    real,             pointer        :: x_mem(:, :, :, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3) * size(x_mem,4) * size(x_mem,5)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
    return
  end subroutine memfun_del_r5
  !
  subroutine memfun_nul_r5 ( x_mem )
! nullify 4d real array
!
    real,             pointer        :: x_mem(:, :, :, :, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_r5
  !
  subroutine memfun_all_i5 (x_mem, n1, n2, n3, n4, n5, c_mem, i_err, init_value)
! allocate 4d integer array
!
    integer,          pointer        :: x_mem(:, :, :, :, :)
    integer,          intent (in   ) :: n1, n2, n3, n4, n5
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    integer, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_i5 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3 * n4 * n5
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    allocate ( x_mem(n1, n2, n3, n4, n5) , stat=j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_i5" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3* n4, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'i5', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_i5
  !
  subroutine memfun_del_i5 ( x_mem )
! deallocate 4d integer array
!
    integer,          pointer        :: x_mem(:, :, :, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3) * size(x_mem,4) * size(x_mem,5)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_i5
  !
  subroutine memfun_nul_i5 ( x_mem )
! nullify 4d real array
!
    integer,          pointer        :: x_mem(:, :, :, :, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_i5
  !
  subroutine memfun_all_l5 (x_mem, n1, n2, n3, n4, n5, c_mem, i_err, init_value)
! allocate 5d logical array
!
    logical,          pointer        :: x_mem(:, :, :, :, :)
    integer,          intent (in   ) :: n1, n2, n3, n4, n5
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    logical, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_l5 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3 * n4 * n5
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    allocate ( x_mem(n1, n2, n3, n4, n5) , stat=j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = .true.
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_l5" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3* n4, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'l5', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_l5
  !
  subroutine memfun_del_l5 ( x_mem )
! deallocate 4d logical array
!
    logical,          pointer        :: x_mem(:, :, :, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 1 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3) * size(x_mem,4) * size(x_mem,5)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_l5
  !
  subroutine memfun_nul_l5 ( x_mem )
! nullify 4d integer array
!
    logical,          pointer        :: x_mem(:, :, :, :, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_l5
  !
  subroutine memfun_all_c5 (x_mem, n1, n2, n3, n4, n5, c_mem, i_err, init_value)
! allocate 5d complex array
!
    complex,          pointer        :: x_mem(:, :, :, :, :)
    integer,          intent (in   ) :: n1, n2, n3, n4, n5
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    complex, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_c5 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3 * n4 * n5 * 2
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    allocate ( x_mem(n1, n2, n3, n4, n5) , stat=j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = cmplx(0.,0.)
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_c2" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3* n4, trim(c_mem)
     !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'c5', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_c5
  !
  subroutine memfun_del_c5 ( x_mem )
! deallocate 4d complex array
!
    complex,          pointer        :: x_mem(:, :, :, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 2 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3) * size(x_mem,4) * size(x_mem,5)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
    return
  end subroutine memfun_del_c5
  !
  subroutine memfun_nul_c5 ( x_mem )
! nullify 4d real array
!
    complex,          pointer        :: x_mem(:, :, :, :, :)
    !
! nullify memory
    nullify  ( x_mem )
    !
    return
  end subroutine memfun_nul_c5
  !
  subroutine memfun_all_d5 (x_mem, n1, n2, n3, n4, n5, c_mem, i_err, init_value)
! allocate 5d double precision array
!
    double precision, pointer        :: x_mem(:, :, :, :, :)
    integer,          intent (in   ) :: n1, n2, n3, n4, n5
    character(len=*), intent (in   ) :: c_mem
    integer,          intent (inout) :: i_err
    double precision, optional, intent(in) :: init_value
    !
    integer                          :: j_err
    integer                          :: n_mem
    !
! deallocate anything that exists
    call memfun_del_d5 ( x_mem )
    !
! increment the total memory allocated
    n_mem = n1 * n2 * n3 * n4 * n5 * 2
    n_mem_sum = n_mem_sum + n_mem
    !
! allocate memory
    allocate ( x_mem(n1, n2, n3, n4, n5) , stat=j_err )
    !
! for no error normal return
    xxif_j_err : if ( j_err .eq. 0 ) then
    !
      xxif_present : if ( present(init_value)) then
        x_mem = init_value
      else xxif_present
        x_mem = 0
      end if xxif_present
      !
    else xxif_j_err
    !
      i_err = i_err - 1
      !
! print error message
      write ( pc_get_lun(), '( &
     & /, " error in memfun_all_d2" &
     & /, " total=", i12, " requested=", i12, " name=", a)') &
     & n_mem_sum, n1* n2* n3* n4, trim(c_mem)
  !
    end if xxif_j_err
    !
    call memfun_prn_all ( 'd5', n_mem, c_mem)
    !
    return
  end subroutine memfun_all_d5
  !
  subroutine memfun_del_d5 ( x_mem )
! deallocate 4d double precision array
!
    double precision, pointer        :: x_mem(:, :, :, :, :)
    integer                          :: n_mem_del
    !
    if ( associated ( x_mem ) ) deallocate ( x_mem )
    !
    xxif_associated : if ( n_mem_sum .eq. -999 ) then
      !
      n_mem_del = 2 * &
size(x_mem,1) * size(x_mem,2) * size(x_mem,3) * size(x_mem,4) * size(x_mem,5)
      !
      if ( memory_print ) write ( pc_get_lun(), '( &
      & " memfun_del total=", i12, " - ", i12, " = ", i12 &
      & )')&
      & n_mem_sum, n_mem_del, n_mem_sum-n_mem_del
      !
      ! decrement sum
      !
      n_mem_sum = max ( 0, n_mem_sum - n_mem_del )
      !
      ! deallocate memory
      !
      deallocate ( x_mem )
      !
    end if xxif_associated
    !
    return
    !
  end subroutine memfun_del_d5
  !
  subroutine memfun_nul_d5 ( x_mem )
! nullify 4d real array
!
    double precision, pointer        :: x_mem(:, :, :, :, :)
    !
! nullify memory
    nullify ( x_mem )
    !
    return
  end subroutine memfun_nul_d5
  !
  subroutine memfun_init_c ( x_mem, init_value )
    !
    ! initialize a character value
    !
    character(len=*),           intent(inout) :: x_mem
    character(len=*), optional, intent(in)    :: init_value
    !
    xxif_present : if ( present(init_value)) then
      x_mem = init_value
    else xxif_present
      x_mem = ' '
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_c
  !
  subroutine memfun_init_i ( x_mem, init_value )
    !
    ! initialize a integer value
    !
    integer,           intent(inout) :: x_mem
    integer, optional, intent(in)    :: init_value
    !
    xxif_present : if ( present(init_value)) then
      x_mem = init_value
    else xxif_present
      x_mem = 0
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_i
  !
  subroutine memfun_init_r ( x_mem, init_value )
    !
    ! initialize a real value
    !
    real,           intent(inout) :: x_mem
    real, optional, intent(in)    :: init_value
    !
    xxif_present : if ( present(init_value)) then
      x_mem = init_value
    else xxif_present
      x_mem = 0
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_r
  !
  subroutine memfun_init_d ( x_mem, init_value )
    !
    ! initialize a dp value
    !
    double precision,           intent(inout) :: x_mem
    double precision, optional, intent(in)    :: init_value
    !
    xxif_present : if ( present(init_value)) then
      x_mem = init_value
    else xxif_present
      x_mem = 0
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_d
  !
  subroutine memfun_init_l ( x_mem, init_value )
    !
    ! initialize a logical value
    !
    logical,           intent(inout) :: x_mem
    logical, optional, intent(in)    :: init_value
    !
    xxif_present : if ( present(init_value)) then
      x_mem = init_value
    else xxif_present
      x_mem = .true.
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_l
  !
  subroutine memfun_init_l1 ( x_mem, init_value )
    !
    ! initialize
    !
    logical,           pointer       :: x_mem(:)
    logical, optional, intent(in)    :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_l1
  !
  subroutine memfun_init_l2 ( x_mem, init_value )
    !
    ! initialize
    !
    logical,           pointer       :: x_mem(:, :)
    logical, optional, intent(in)    :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_l2
  !
  subroutine memfun_init_l3 ( x_mem, init_value )
    !
    ! initialize
    !
    logical,           pointer       :: x_mem(:, :, :)
    logical, optional, intent(in)    :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_l3
  !
  subroutine memfun_init_l4 ( x_mem, init_value )
    !
    ! initialize
    !
    logical,           pointer       :: x_mem(:, :, :, :)
    logical, optional, intent(in)    :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_l4
  !
  subroutine memfun_init_l5 ( x_mem, init_value )
    !
    ! initialize
    !
    logical,           pointer       :: x_mem(:, :, :, :, :)
    logical, optional, intent(in)    :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 1, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 1, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_l5
  !
  subroutine memfun_init_i1 ( x_mem, init_value )
    !
    ! initialize
    !
    integer,           pointer       :: x_mem(:)
    integer, optional, intent(in)    :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_i1
  !
  subroutine memfun_init_i2 ( x_mem, init_value )
    !
    ! initialize
    !
    integer,           pointer       :: x_mem(:, :)
    integer, optional, intent(in)    :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_i2
  !
  subroutine memfun_init_i3 ( x_mem, init_value )
    !
    ! initialize
    !
    integer,           pointer       :: x_mem(:, :, :)
    integer, optional, intent(in)    :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_i3
  !
  subroutine memfun_init_i4 ( x_mem, init_value )
    !
    ! initialize
    !
    integer,           pointer       :: x_mem(:, :, :, :)
    integer, optional, intent(in)    :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_i4
  !
  subroutine memfun_init_i5 ( x_mem, init_value )
    !
    ! initialize
    !
    integer,           pointer       :: x_mem(:, :, :, :, :)
    integer, optional, intent(in)    :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 1, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 1, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_i5
  !
  subroutine memfun_init_r1 ( x_mem, init_value )
    !
    ! initialize
    !
    real,           pointer          :: x_mem(:)
    real, optional, intent(in)       :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_r1
  !
  subroutine memfun_init_r2 ( x_mem, init_value )
    !
    ! initialize
    !
    real,           pointer          :: x_mem(:, :)
    real, optional, intent(in)       :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_r2
  !
  subroutine memfun_init_r3 ( x_mem, init_value )
    !
    ! initialize
    !
    real,           pointer          :: x_mem(:, :, :)
    real, optional, intent(in)       :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_r3
  !
  subroutine memfun_init_r4 ( x_mem, init_value )
    !
    ! initialize
    !
    real,           pointer          :: x_mem(:, :, :, :)
    real, optional, intent(in)       :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_r4
  !
  subroutine memfun_init_r5 ( x_mem, init_value )
    !
    ! initialize
    !
    real,           pointer          :: x_mem(:, :, :, :, :)
    real, optional, intent(in)       :: init_value
    !
    integer                          :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 1, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 1, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_r5
  !
  subroutine memfun_init_d1 ( x_mem, init_value )
    !
    ! initialize
    !
    double precision, pointer              :: x_mem(:)
    double precision, optional, intent(in) :: init_value
    !
    integer                                :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_d1
  !
  subroutine memfun_init_d2 ( x_mem, init_value )
    !
    ! initialize
    !
    double precision, pointer              :: x_mem(:, :)
    double precision, optional, intent(in) :: init_value
    !
    integer                                :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_d2
  !
  subroutine memfun_init_d3 ( x_mem, init_value )
    !
    ! initialize
    !
    double precision, pointer              :: x_mem(:, :, :)
    double precision, optional, intent(in) :: init_value
    !
    integer                                :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_d3
  !
  subroutine memfun_init_d4 ( x_mem, init_value )
    !
    ! initialize
    !
    double precision, pointer              :: x_mem(:, :, :, :)
    double precision, optional, intent(in) :: init_value
    !
    integer                                :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_d4
  !
  subroutine memfun_init_d5 ( x_mem, init_value )
    !
    ! initialize
    !
    double precision, pointer              :: x_mem(:, :, :, :, :)
    double precision, optional, intent(in) :: init_value
    !
    integer                                :: j_err
    !
    ! allocate memory
    !
    xxif_present : if ( present(init_value)) then
      call memfun_all ( x_mem, 1, 1, 1, 1, 1, 'x_mem', j_err, init_value )
    else xxif_present
      call memfun_all ( x_mem, 1, 1, 1, 1, 1, 'x_mem', j_err )
    end if xxif_present
    !
    return
    !
  end subroutine memfun_init_d5
  !
  subroutine memfun_sys_mem_prn ( c_title )
    !
    character(len=*), intent(in   ) :: c_title
    !
    integer                         :: vsize
    integer                         :: rss 
    integer                         :: used_mem
    !
    call memfun_sys_mem ( vsize, rss, used_mem )
    !
    write ( pc_get_lun(), ' ( &
    & " memfun_sys_mem_prn  vsize=", i8, &
    & " rss=", i8, " used_mem=", i8, 1x, a &
    & )') &
    vsize, rss, used_mem, trim ( c_title )
    !
    return
    !
  end subroutine memfun_sys_mem_prn  
  !
  subroutine memfun_sys_mem ( vsize, rss, used_mem )
    !
    integer,        intent(  out) :: vsize     ! argument
    integer,        intent(  out) :: rss       ! argument
    integer,        intent(  out) :: used_mem  ! argument
    !
    call getsys_my_pid_memory ( vsize, rss )
    !
    used_mem = vsize - rss 
    !
    return
    !
  end subroutine memfun_sys_mem
  !
  subroutine memfun_print ( print_i_pel, c_title )
    !
    character(len=*), intent(in   ) :: c_title
    integer,          intent(in   ) :: print_i_pel
    !
    integer                         :: vsize
    integer                         :: rss     
    character(len=120)              :: line
    !
    call getsys_my_pid_memory ( vsize, rss )
    !
    if ( print_i_pel .lt. 0 ) then
      !
      write(line,'("memfun_print(",i3,"):",a," vsize=",i8," rss=",i8)') &
      pcps_i_pel(), trim(c_title), vsize, rss
      !
      print *,trim(line)
      !
    else
      !
      if ( pcps_i_pel() .eq. print_i_pel ) then
        !
        write(line,'("memfun_print(",i3,"):",a," vsize=",i8," rss=",i8)') &
        pcps_i_pel(), trim(c_title), vsize, rss
        !
        print*, trim(line)
        !
      endif
      !
    endif
    !
  return
  !
  end subroutine memfun_print
  !
end module memfun_module
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
