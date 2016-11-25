!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ cpucount.f90 ------------------------------!!
!!------------------------------ cpucount.f90 ------------------------------!!
!!------------------------------ cpucount.f90 ------------------------------!!


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
! Name       : CPUCOUNT   (CPU hit COUNT)
! Category   : miscellaneous
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2006-01-09   by: Douglas Hanson Fix test in top_bot.
! Maturity   : production
! Purpose    : count cpu hits and seconds
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
!  Utilities for counting and printing cpu usage and processing steps.
!
!-------------------------------------------------------------------------------
!</descript_doc>


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
!
! create the cpucount structure
!
!        call cpucount_create ( c, m0_cpu )
!
! delete the cpucount structure
!
!        call cpucount_delete ( c, m0_cpu )
!
! increment a cpucount counter
!
!        call cpucount_delete ( c )
!
!    type(cpucount_struct),   pointer :: c     ! cpucount structure
!    integer,          intent (in   ) :: m0_cpu  ! max num cpu counters
!    integer,          intent (inout) :: n0_cpu  ! cpu num hits
!    double precision, intent (inout) :: t0_cpu  ! cpu time
!    double precision, intent (inout) :: t1_cpu  ! cpu counter
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
! 11  2006-01-09  Douglas Hanson Fix test in top_bot.
! 10  2005-01-31  R.S.Day        Fix cpucount_print parallel behavior
!  9. 2004-09-14  R.S.Day        Promoted to beta.
!  8  2004-07-29  Douglas Hanson Ave instead of reduced elapsed 
!  7  2004-01-19  Douglas Hanson Add group capability.
!  6  2004-01-13  Douglas Hanson Add double precision.
!  5  2003-06-04  Douglas Hanson Add reduce, clear
!  4  2003-03-14  Douglas Hanson Add cpucount_struct
!  3  2001-01-10  Douglas Hanson cpsfcr
!  2  2000-05-19  Brad Kruse     Converted from old system.
!  1  2000-05-01  Douglas Hanson Initial version.
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
module cpucount_module
  !
  ! - Module references
  !
  use getsys_module
  use mem_module
  use named_constants_module
  use pc_module
  use pcpsx_module
  use string_module
  use unix_module
  !
  implicit none
  !
  private
  !
  public :: cpucount_create
  public :: cpucount_delete
  public :: cpucount_nullify
  public :: cpucount_print
  public :: cpucount_print_r
  public :: cpucount_print_g
  public :: cpucount_print_n
  public :: cpucount_print_ng
  public :: cpucount_add
  public :: cpucount_reduce
  public :: cpucount_reduce_g
  public :: cpucount_clear
  public :: cpucount
  !
  interface cpucount
    !
    module procedure cpucount
    module procedure cpucount_d
    module procedure cpucount_dr
    module procedure cpucount_0
    module procedure cpucount_top_bot
    !
  end interface 
  !
  type, public :: cpucount_struct
    !
    integer                          :: m0_cpu
    integer                          :: l0_cpu 
    character(len=80),       pointer :: c0_cpu ( : )
    integer,                 pointer :: n0_cpu ( : )
    integer,                 pointer :: n1_cpu ( : )
    integer,                 pointer :: n2_cpu ( : )
    double precision,        pointer :: e0_cpu ( : )
    double precision,        pointer :: e1_cpu ( : )
    double precision,        pointer :: e2_cpu ( : )
    double precision,        pointer :: t0_cpu ( : )
    double precision,        pointer :: t1_cpu ( : )
    double precision,        pointer :: t2_cpu ( : )
    !
  end type cpucount_struct
  !
  ! - RCS Ident String
  !
  character(len=100), public, save :: CPUCOUNT_IDENT = &
    '$Id: cpucount.f90,v 1.11 2006/01/09 12:37:20 Hanson prod sps $'
  !
  contains
  !
  subroutine cpucount_create ( c, m0_cpu )
    !
    ! create the cpucount structure
    !
    ! - Arguments
    !
    type(cpucount_struct),   pointer :: c  ! cpucount structure
    !
    integer,          intent (in   ) :: m0_cpu
    !
    ! - Local variables
    !
    !
    ! - Begin 
    !
    allocate ( c )
    !
    c%m0_cpu = m0_cpu
    c%l0_cpu = 0
    !
    allocate ( c%c0_cpu ( c%m0_cpu ) )
    allocate ( c%n0_cpu ( c%m0_cpu ) )
    allocate ( c%n1_cpu ( c%m0_cpu ) )
    allocate ( c%n2_cpu ( c%m0_cpu ) )
    allocate ( c%e0_cpu ( c%m0_cpu ) )
    allocate ( c%e1_cpu ( c%m0_cpu ) )
    allocate ( c%e2_cpu ( c%m0_cpu ) )
    allocate ( c%t0_cpu ( c%m0_cpu ) )
    allocate ( c%t1_cpu ( c%m0_cpu ) )
    allocate ( c%t2_cpu ( c%m0_cpu ) )
    !
    call cpucount_clear ( c, .true. )
    !
  end subroutine cpucount_create
  !
  subroutine cpucount_delete ( c )
    !
    ! delete the cpucount structure
    !
    ! - Arguments
    !
    type(cpucount_struct),   pointer :: c  ! cpucount structure
    !
    ! - Local variables
    !
    !
    ! - Begin 
    !
    if ( associated ( c%c0_cpu  ) ) &
         deallocate ( c%c0_cpu )
    !
    if ( associated ( c%n0_cpu  ) ) &
         deallocate ( c%n0_cpu )
    !
    if ( associated ( c%n1_cpu  ) ) &
         deallocate ( c%n1_cpu )
    !
    if ( associated ( c%n2_cpu  ) ) &
         deallocate ( c%n2_cpu )
    !
    if ( associated ( c%e0_cpu  ) ) &
         deallocate ( c%e0_cpu )
    !
    if ( associated ( c%e1_cpu  ) ) &
         deallocate ( c%e1_cpu )
    !
    if ( associated ( c%e2_cpu  ) ) &
         deallocate ( c%e2_cpu )
    !
    if ( associated ( c%t0_cpu  ) ) &
         deallocate ( c%t0_cpu )
    !
    if ( associated ( c%t1_cpu  ) ) &
         deallocate ( c%t1_cpu )
    !
    if ( associated ( c%t2_cpu  ) ) &
         deallocate ( c%t2_cpu )
    !
    if ( associated ( c ) ) &
         deallocate ( c )
    !
  end subroutine cpucount_delete
  !
  subroutine cpucount_nullify ( c )
    !
    ! - Arguments
    !
    type(cpucount_struct),   pointer :: c  ! cpucount structure
    !
    ! - Local variables
    !
    !
    ! - Begin 
    !
    !nullify ( c%n0_cpu )
    !
    !nullify ( c%e0_cpu )
    !
    !nullify ( c%t0_cpu )
    !
    nullify ( c )
    !
  end subroutine cpucount_nullify
  !
  subroutine cpucount_print ( c, c_title )
    !
    ! print statistics
    !
    type(cpucount_struct),   pointer :: c  ! cpucount structure
    character(len=*), intent (in   ) :: c_title ! char title
    !
    ! - Local variables
    !
    integer                          :: j0_cpu
    integer                          :: n0_cpu ( c%l0_cpu )
    double precision                 :: e0_cpu ( c%l0_cpu )
    double precision                 :: t0_cpu ( c%l0_cpu )
    !
    ! - Begin 
    !
    write ( pc_get_lun(), ' ( &
    & /, " cpu0 ", a8, " cpucount_print ", a, &
    & /, " cpu0 ", a8, " cpucount_print pe=",i8, &
    & /, " cpu0 ", a8, " max    columns=", i8, &
    & /, " cpu0 ", a8, " filled columns=", i8, &
    & /, " cpu0 ", a8, "  pe index   hits  elapsed      cpu         title ",&
    & /, " cpu0 ", a8, "                   time in      time in           ",&
    & /, " cpu0 ", a8, "                   seconds      seconds           " &
    & )') &
    trim ( c_title ), trim ( c_title ), &
    trim ( c_title ), pcpsx_i_pel(), &
    trim ( c_title ), c%m0_cpu, &
    trim ( c_title ), c%l0_cpu, &
    trim ( c_title ), &
    trim ( c_title ), &
    trim ( c_title )
    !
    n0_cpu ( 1:c%l0_cpu ) = c%n0_cpu ( 1:c%l0_cpu )
    !
    e0_cpu ( 1:c%l0_cpu ) = c%e0_cpu ( 1:c%l0_cpu )
    !
    t0_cpu ( 1:c%l0_cpu ) = c%t0_cpu ( 1:c%l0_cpu )
    !
    ! print info for each counter
    !
    write(pc_get_lun(), '( &
    & " cpu0 ", a8, 1x, i3, 1x, i3, 1x, i8, 1x, g12.6, 1x, g12.6, 1x, a &
    & )') &
    ( trim ( c_title ), &
    pcpsx_i_pel(), j0_cpu, &
    n0_cpu ( j0_cpu ), e0_cpu ( j0_cpu ), t0_cpu ( j0_cpu ), &
    trim ( c%c0_cpu ( j0_cpu ) ), &
    j0_cpu = 1 , c%l0_cpu )
    !
    return
    !
  end subroutine cpucount_print 
  !
  subroutine cpucount_print_r ( c, c_title, l_reduce )
    !
    ! print cpu statistics with possible reduce
    ! if reduce all pes must enter
    !
    type(cpucount_struct),   pointer :: c        ! cpucount structure
    character(len=*), intent (in   ) :: c_title  ! char title
    logical,          intent (in   ) :: l_reduce ! reduce info
    !
    ! - Local variables
    !
    integer                          :: i_err
    integer                          :: k0_pel
    integer                          :: j0_pel
    integer                          :: j0_cpu
    integer                          :: n0_cpu ( c%l0_cpu )
    double precision                 :: e0_cpu ( c%l0_cpu )
    double precision                 :: t0_cpu ( c%l0_cpu )
    !
    ! - Begin 
    !
    j0_pel = pcpsx_i_pel()
    !
    if ( l_reduce ) j0_pel = -1
    !
    !print '( &
    !& )', &
    !
    if ( .not. l_reduce .or. ( l_reduce .and. pcpsx_i_pel() .eq. 0 ) ) &
    write ( pc_get_lun(), ' ( &
    & /, " cpu0 ", a8, " cpucount_print_r ", a, &
    & /, " cpu0 ", a8, " cpucount_print_r pe=",i8, " l_reduce=", l2, &
    & /, " cpu0 ", a8, " max    columns=", i8, &
    & /, " cpu0 ", a8, " filled columns=", i8, &
    & /, " cpu0 ", a8, "  pe index   hits  elapsed      cpu         title ",&
    & /, " cpu0 ", a8, "                   time in      time in           ",&
    & /, " cpu0 ", a8, "                   seconds      seconds           " &
    & )') &
    trim ( c_title ), trim ( c_title ), &
    trim ( c_title ), j0_pel, l_reduce, &
    trim ( c_title ), c%m0_cpu, &
    trim ( c_title ), c%l0_cpu, &
    trim ( c_title ), &
    trim ( c_title ), &
    trim ( c_title )
    !
    n0_cpu ( 1:c%l0_cpu ) = c%n0_cpu ( 1:c%l0_cpu )
    !
    e0_cpu ( 1:c%l0_cpu ) = c%e0_cpu ( 1:c%l0_cpu )
    !
    t0_cpu ( 1:c%l0_cpu ) = c%t0_cpu ( 1:c%l0_cpu )
    !
    ! reduce values over all pes
    !
    xxif_reduce : if ( l_reduce ) then
      !
      call pcpsx_sum_all_reduce ( c%l0_cpu, n0_cpu, n0_cpu )
      !
      call pcpsx_sum_all_reduce ( c%l0_cpu, e0_cpu, e0_cpu )
      !
      call pcpsx_sum_all_reduce ( c%l0_cpu, t0_cpu, t0_cpu )
      !
    end if xxif_reduce 
    !
    ! print info in pe order
    !
    do_k0_pel : do k0_pel = 1 , pcpsx_n_pel()
      !
      j0_pel = k0_pel - 1
      !
      ! print info for each counter
      !
      if ( j0_pel .eq. pcpsx_i_pel() &
      .and. ( .not. l_reduce .or. ( l_reduce .and. pcpsx_i_pel() .eq. 0 ) ) ) &
      print'( &
      & " cpu0 ", a8, 1x, i3, 1x, i3, 1x, i8, 1x, g12.6, 1x, g12.6, 1x, a &
      & )', &
      ( trim ( c_title ), &
      j0_pel, j0_cpu, &
      n0_cpu ( j0_cpu ), e0_cpu ( j0_cpu ), t0_cpu ( j0_cpu ), &
      trim ( c%c0_cpu ( j0_cpu ) ), &
      j0_cpu = 1 , c%l0_cpu )
      !
      ! force sync of pes so printout is organized by pe
      !
      i_err = 0
      !
      call pcpsx_sum_all_reduce ( i_err, i_err )
      !
    end do do_k0_pel 
    !
    return
    !
  end subroutine cpucount_print_r 
  !
  subroutine cpucount_print_g ( c, c_title, l_reduce, np_grp, jp_grp )
    !
    ! print cpu statistics with possible reduce
    ! if reduce all pes must enter
    !
    type(cpucount_struct),   pointer :: c        ! cpucount structure
    character(len=*), intent (in   ) :: c_title  ! char title
    logical,          intent (in   ) :: l_reduce ! reduce info
    integer,          intent (in   ) :: np_grp   ! num pes in group
    integer,          intent (in   ) :: jp_grp(:)! list pes in group
    !
    ! - Local variables
    !
    integer                          :: i_err
    integer                          :: k0_pel
    integer                          :: j0_pel
    integer                          :: j0_cpu
    integer                          :: n0_cpu ( c%l0_cpu )
    double precision                 :: e0_cpu ( c%l0_cpu )
    double precision                 :: t0_cpu ( c%l0_cpu )
    !
    ! - Begin 
    !
    j0_pel = pcpsx_i_pel()
    !
    if ( l_reduce ) j0_pel = -1
    !
    if ( pcpsx_i_pel() .eq. jp_grp(1) ) &
    print'( &
    & /, " cpu0 ", a8, " cpucount_print_rg ", a, &
    & /, " cpu0 ", a8, " cpucount_print_rg pe=",i8, " l_reduce=", l2, &
    & /, " cpu0 ", a8, " max    columns=", i8, &
    & /, " cpu0 ", a8, " filled columns=", i8, &
    & /, " cpu0 ", a8, "  pe index   hits  elapsed      cpu         title ",&
    & /, " cpu0 ", a8, "                   time in      time in           ",&
    & /, " cpu0 ", a8, "                   seconds      seconds           " &
    & )', &
    trim ( c_title ), trim ( c_title ), &
    trim ( c_title ), j0_pel, l_reduce, &
    trim ( c_title ), c%m0_cpu, &
    trim ( c_title ), c%l0_cpu, &
    trim ( c_title ), &
    trim ( c_title ), &
    trim ( c_title )
    !
    n0_cpu ( 1:c%l0_cpu ) = c%n0_cpu ( 1:c%l0_cpu )
    !
    e0_cpu ( 1:c%l0_cpu ) = c%e0_cpu ( 1:c%l0_cpu )
    !
    t0_cpu ( 1:c%l0_cpu ) = c%t0_cpu ( 1:c%l0_cpu )
    !
    ! reduce values over all pes in group
    !
    xxif_reduce : if ( l_reduce ) then
      !
      call pcpsx_sum_all_reduce_group ( np_grp, jp_grp, c%l0_cpu, n0_cpu )
      !
      call pcpsx_sum_all_reduce_group ( np_grp, jp_grp, c%l0_cpu, e0_cpu )
      !
      call pcpsx_sum_all_reduce_group ( np_grp, jp_grp, c%l0_cpu, t0_cpu )
      !
      ! make e0_cpu the average elapsed time 
      ! rather than the reudce elapsed time
      !
      e0_cpu ( 1:c%l0_cpu ) = &
      e0_cpu ( 1:c%l0_cpu ) / np_grp
      !
    end if xxif_reduce 
    !
    ! print info in pe order
    !
    do_k0_pel : do k0_pel = 1 , np_grp
      !
      j0_pel = jp_grp ( k0_pel )
      !
      ! print info for each counter
      !
      if ( j0_pel .eq. pcpsx_i_pel() &
        .and. &
      ( .not. l_reduce &
        .or. ( l_reduce .and. pcpsx_i_pel() .eq. jp_grp(1) ) ) ) &
      print'( &
      & " cpu0 ", a8, 1x, i3, 1x, i3, 1x, i8, 1x, g12.6, 1x, g12.6, 1x, a &
      & )', &
      ( trim ( c_title ), &
      j0_pel, j0_cpu, &
      n0_cpu ( j0_cpu ), e0_cpu ( j0_cpu ), t0_cpu ( j0_cpu ), &
      trim ( c%c0_cpu ( j0_cpu ) ), &
      j0_cpu = 1 , c%l0_cpu )
      !
      ! force sync of pes in group so printout is organized by pe
      !
      i_err = 0
      !
      call pcpsx_sum_all_reduce_group ( np_grp, jp_grp, i_err )
      !
    end do do_k0_pel 
    !
    return
    !
  end subroutine cpucount_print_g 
  !
  subroutine cpucount_print_n ( c, c_title )
    !
    ! print cpu statistics for pes i1 : i2
    ! all pes must enter
    !
    ! - Arguments
    !
    type(cpucount_struct),   pointer :: c  ! cpucount structure
    character(len=*), intent (in   ) :: c_title ! char title
    !
    ! - Local variables
    !
    integer                          :: i_err
    !
    ! - Begin 
    !
    i_err = 0
    !
    ! print reduced statistics for all pes
    !
    call cpucount_print_r ( c, c_title, .true. )
    !
    ! if there is more than 1 pe print statistics for each pe independantly
    !
    if ( pcpsx_n_pel() .gt. 1 ) &
    call cpucount_print_r ( c, c_title, .false. )
    !
    return
    !
  end subroutine cpucount_print_n 
  !
  subroutine cpucount_print_ng ( c, c_title, np_grp, jp_grp )
    !
    ! print cpu statistics for pes i1 : i2
    ! all pes must enter
    !
    ! - Arguments
    !
    type(cpucount_struct),   pointer :: c  ! cpucount structure
    character(len=*), intent (in   ) :: c_title ! char title
    integer,          intent (in   ) :: np_grp   ! num pes in group
    integer,          intent (in   ) :: jp_grp(:)! list pes in group
    !
    ! - Local variables
    !
    integer                          :: i_err
    !
    ! - Begin 
    !
    i_err = 0
    !
    ! print reduced statistics for all pes
    !
    call cpucount_print_g ( c, c_title, .true., np_grp, jp_grp )
    !
    ! if there is more than 1 pe print statistics for each pe independantly
    !
    if ( np_grp .gt. 1 ) &
    call cpucount_print_g  ( c, c_title, .false., np_grp, jp_grp )
    !
    return
    !
  end subroutine cpucount_print_ng 
  !
  subroutine cpucount_add ( c, c0_cpu, l0_cpu, i_err )
    !
    ! - count cpu time and number of uses
    !
    type(cpucount_struct),   pointer :: c  ! cpucount structure
    character(len=*), intent (in   ) :: c0_cpu  ! char title
    integer,          intent (inout) :: l0_cpu  ! cpu num hits
    integer,          intent (inout) :: i_err
    !
    ! - Local variables
    !
    !
    ! - Begin 
    !
    i_err = 0
    !
    c%l0_cpu = c%l0_cpu + 1
    !
    l0_cpu = c%l0_cpu 
    !
    xxif_add_memory : if ( c%l0_cpu .gt. c%m0_cpu ) then
      !
      c%m0_cpu = c%m0_cpu + 10
      !
      call mem_realloc ( c%c0_cpu, c%m0_cpu, i_err )
      call mem_realloc ( c%n0_cpu, c%m0_cpu, i_err )
      call mem_realloc ( c%n1_cpu, c%m0_cpu, i_err )
      call mem_realloc ( c%n1_cpu, c%m0_cpu, i_err )
      call mem_realloc ( c%e0_cpu, c%m0_cpu, i_err )
      call mem_realloc ( c%e1_cpu, c%m0_cpu, i_err )
      call mem_realloc ( c%e2_cpu, c%m0_cpu, i_err )
      call mem_realloc ( c%t0_cpu, c%m0_cpu, i_err )
      call mem_realloc ( c%t1_cpu, c%m0_cpu, i_err )
      call mem_realloc ( c%t2_cpu, c%m0_cpu, i_err )
      !
      if ( i_err .ne. 0 ) go to 999
      !
    end if xxif_add_memory 
    !
    c%c0_cpu(l0_cpu) = trim ( c0_cpu )
    !
    return
    !
999 continue
    !
    write ( pc_get_lun(), '( &
    & /, " error in cput_count_0 ", &
    & /, " during memory reallocation ", &
    & /, " l0_cpu=",i8," m0_cpu=",i8," c0_cpu=",a &
    & )') &
    c%l0_cpu, c%m0_cpu, trim(c0_cpu)
    !
    i_err = -1
    !
    return
    !
  end subroutine cpucount_add
  !
  subroutine cpucount_reduce ( c )
    !
    ! - reduce counters
    !
    type(cpucount_struct),   pointer :: c  ! cpucount structure
    !
    ! - Local variables
    !
    ! - Begin 
    !
    call pcpsx_sum_all_reduce ( c%m0_cpu, c%e0_cpu, c%e0_cpu )
    !
    call pcpsx_sum_all_reduce ( c%m0_cpu, c%t0_cpu, c%t0_cpu )
    !
    call pcpsx_sum_all_reduce ( c%m0_cpu, c%n0_cpu, c%n0_cpu )
    !
    return
    !
  end subroutine cpucount_reduce 
  !
  subroutine cpucount_reduce_g ( c, np_grp, jp_grp )
    !
    ! - reduce counters
    !
    type(cpucount_struct),   pointer :: c  ! cpucount structure
    integer,          intent (in   ) :: np_grp   ! num pes in group
    integer,          intent (in   ) :: jp_grp(:)! list pes in group
    !
    ! - Local variables
    !
    integer                          :: n0_cpu
    double precision                 :: e0_cpu
    double precision                 :: t0_cpu
    !
    ! - Begin 
    !
    call pcpsx_sum_all_reduce_group ( np_grp, jp_grp, n0_cpu )
    !
    call pcpsx_sum_all_reduce_group ( np_grp, jp_grp, e0_cpu )
    !
    call pcpsx_sum_all_reduce_group ( np_grp, jp_grp, t0_cpu )
    !
    return
    !
  end subroutine cpucount_reduce_g 
  !
  subroutine cpucount_clear ( c, clear_c0_cpu )
    !
    ! - clear counters
    !
    type(cpucount_struct),   pointer :: c  ! cpucount structure
    logical,           intent(in   ) :: clear_c0_cpu 
    !
    ! - Local variables
    !
    ! - Begin 
    !
    if ( clear_c0_cpu ) &
    c%c0_cpu ( 1:c%m0_cpu ) = ' '
    c%n0_cpu ( 1:c%m0_cpu ) = 0
    c%n1_cpu ( 1:c%m0_cpu ) = 0
    c%n2_cpu ( 1:c%m0_cpu ) = 0
    c%e0_cpu ( 1:c%m0_cpu ) = 0.
    c%e1_cpu ( 1:c%m0_cpu ) = 0.
    c%e2_cpu ( 1:c%m0_cpu ) = 0.
    c%t0_cpu ( 1:c%m0_cpu ) = 0.
    c%t1_cpu ( 1:c%m0_cpu ) = 0.
    c%t2_cpu ( 1:c%m0_cpu ) = 0.
    !
    return
    !
  end subroutine cpucount_clear 
  !
  subroutine cpucount ( n0_cpu, t0_cpu, t1_cpu )
    !
    ! - Arguments
    !
    integer,          intent (inout) :: n0_cpu  ! cpu num hits
    real,             intent (inout) :: t0_cpu  ! cpu time
    real,             intent (inout) :: t1_cpu  ! cpu counter
    !
    ! - Local variables
    !
    real                             :: t2_cpu
    double precision                 :: t2_cpu_d
    !
    ! - Begin 
    !
    !t2_cpu = getsys_seconds()
    !
    t2_cpu_d = unix_utime ( )
    !
    t2_cpu = t2_cpu_d
    !
    n0_cpu = n0_cpu + 1
    !
    t0_cpu = t0_cpu + t2_cpu - t1_cpu
    !
    t1_cpu = t2_cpu
    !
    return
    !
  end subroutine cpucount
  !
  subroutine cpucount_d ( n0_cpu, t0_cpu, t1_cpu )
    !
    ! - Arguments
    !
    integer,          intent (inout) :: n0_cpu  ! cpu num hits
    double precision, intent (inout) :: t0_cpu  ! cpu time
    double precision, intent (inout) :: t1_cpu  ! cpu counter
    !
    ! - Local variables
    !
    double precision                 :: t2_cpu
    double precision                 :: t2_cpu_d
    !
    ! - Begin 
    !
    !t2_cpu = getsys_seconds()
    !
    t2_cpu_d = unix_utime ( )
    !
    t2_cpu = t2_cpu_d
    !
    n0_cpu = n0_cpu + 1
    !
    t0_cpu = t0_cpu + t2_cpu - t1_cpu
    !
    t1_cpu = t2_cpu
    !
    return
    !
  end subroutine cpucount_d 
  !
  subroutine cpucount_dr ( n0_cpu, t0_cpu, t1_cpu )
    !
    ! - Arguments
    !
    integer,          intent (inout) :: n0_cpu  ! cpu num hits
    double precision, intent (inout) :: t0_cpu  ! cpu time
    real,             intent (inout) :: t1_cpu  ! cpu counter
    !
    ! - Local variables
    !
    double precision                 :: t2_cpu
    double precision                 :: t2_cpu_d
    !
    ! - Begin 
    !
    !t2_cpu = getsys_seconds()
    !
    t2_cpu_d = unix_utime ( )
    !
    t2_cpu = t2_cpu_d
    !
    n0_cpu = n0_cpu + 1
    !
    t0_cpu = t0_cpu + t2_cpu - t1_cpu
    !
    t1_cpu = t2_cpu
    !
    return
    !
  end subroutine cpucount_dr 
  !
  subroutine cpucount_0 ( c, l0_cpu, t1_cpu )
    !
    ! - Arguments
    !
    type(cpucount_struct),   pointer :: c  ! cpucount structure
    integer,          intent (in   ) :: l0_cpu  ! cpu num hits
    real,             intent (inout) :: t1_cpu ! cpu counter
    !
    ! - Local variables
    !
    real                             :: t0_cpu
    !
    ! - Begin 
    !
    if ( l0_cpu .lt. 0 .or. l0_cpu .gt. c%l0_cpu ) return
    !
    call cpucount ( c%n0_cpu(l0_cpu), t0_cpu, t1_cpu )
    !
    c%t0_cpu(l0_cpu) = t0_cpu
    !
    return
    !
  end subroutine cpucount_0
  !
  subroutine cpucount_top_bot ( c, l0_cpu, i0_top )
    !
    ! - Arguments
    !
    type(cpucount_struct),   pointer :: c  ! cpucount structure
    integer,          intent (in   ) :: l0_cpu  ! cpu num hits
    integer,          intent (in   ) :: i0_top ! top=1 bot = 2
    !
    ! - Local variables
    !
    integer                          :: j0_cpu
    !
    ! - Begin 
    !
    j0_cpu = max ( 1 , min ( c%l0_cpu, c%m0_cpu, l0_cpu ) )
    !
    if ( l0_cpu .lt. 1 .or. l0_cpu .gt. min ( c%l0_cpu, c%m0_cpu ) ) return
    !
    !if ( l0_cpu .lt. 1 .or. l0_cpu .gt. min ( c%l0_cpu, c%m0_cpu ) ) &
    !print'(" a1 cpucount_top_bot warning p=",i8, &
    !& " l=",i16, " l=",i16, " m=",i16, " n1=",i16," n2=",i16," c=",a)',&
    !pcpsx_i_pel(), &
    !l0_cpu, c%l0_cpu, c%m0_cpu, &
    !c%n1_cpu(j0_cpu) , c%n2_cpu(j0_cpu), trim(c%c0_cpu(j0_cpu))
    !
    !if ( l0_cpu .lt. 1 .or. l0_cpu .gt. min ( c%l0_cpu, c%m0_cpu ) ) &
    !stop
    !
    xxif_top : if ( i0_top .eq. 1 ) then
      !
      c%n1_cpu(l0_cpu) = c%n1_cpu(l0_cpu) + 1
      !
      c%e1_cpu(l0_cpu) = unix_wtime ( ) ! elapsed time
      !
      c%t1_cpu(l0_cpu) = unix_utime ( ) ! user time
      !
    else xxif_top 
      j0_cpu = max ( 1 , min ( c%l0_cpu, c%m0_cpu, l0_cpu ) )
      !
      if ( l0_cpu .lt. 1 .or. l0_cpu .gt. min ( c%l0_cpu, c%m0_cpu ) ) &
      print'(" a2 cpucount_top_bot warning p=",i8, &
      & " l=",i16, " l=",i16, " m=",i16, " n1=",i16," n2=",i16," c=",a)',&
      pcpsx_i_pel(), &
      l0_cpu, c%l0_cpu, c%m0_cpu, &
      c%n1_cpu(j0_cpu) , c%n2_cpu(j0_cpu), trim(c%c0_cpu(j0_cpu))
      !
      if ( l0_cpu .lt. 1 .or. l0_cpu .gt. min ( c%l0_cpu, c%m0_cpu ) ) &
      stop
      !
      c%n2_cpu(l0_cpu) = c%n2_cpu(l0_cpu) + 1
      !
      c%n0_cpu(l0_cpu) = c%n0_cpu(l0_cpu) + 1
      !
      c%e2_cpu(l0_cpu) = unix_wtime ( )
      !
      c%e0_cpu(l0_cpu) = c%e0_cpu(l0_cpu) + c%e2_cpu(l0_cpu) - c%e1_cpu(l0_cpu)
      !
      c%e1_cpu(l0_cpu) = c%e2_cpu(l0_cpu) 
      !
      c%t2_cpu(l0_cpu) = unix_utime ( )
      !
      c%t0_cpu(l0_cpu) = c%t0_cpu(l0_cpu) + c%t2_cpu(l0_cpu) - c%t1_cpu(l0_cpu)
      !
      c%t1_cpu(l0_cpu) = c%t2_cpu(l0_cpu) 
      !
      j0_cpu = max ( 1 , min ( c%l0_cpu, c%m0_cpu, l0_cpu ) )
      !
      if ( l0_cpu .lt. 1 .or. l0_cpu .gt. min ( c%l0_cpu, c%m0_cpu ) &
      .or. c%n2_cpu(j0_cpu) .ne. c%n1_cpu(j0_cpu) ) &
      print'(" a3 cpucount_top_bot warning p=",i8, &
      & " l=",i16, " l=",i16, " m=",i16, " n1=",i16," n2=",i16," c=",a)',&
      pcpsx_i_pel(), &
      l0_cpu, c%l0_cpu, c%m0_cpu, &
      c%n1_cpu(j0_cpu) , c%n2_cpu(j0_cpu), trim(c%c0_cpu(j0_cpu))
      !
      if ( l0_cpu .lt. 1 .or. l0_cpu .gt. min ( c%l0_cpu, c%m0_cpu ) &
      .or. c%n2_cpu(j0_cpu) .ne. c%n1_cpu(j0_cpu) ) &
      stop
      !
    end if xxif_top 
    !
    return
    !
  end subroutine cpucount_top_bot
  !
end module cpucount_module
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

