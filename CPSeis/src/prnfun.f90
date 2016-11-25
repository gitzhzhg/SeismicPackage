!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- prnfun.f90 --------------------------------!!
!!------------------------------- prnfun.f90 --------------------------------!!
!!------------------------------- prnfun.f90 --------------------------------!!


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
! Name       : PRNFUN     (PRiNt FUNctions)
! Category   : miscellaneous
! Written    : 2000-05-01   by: Douglas Hanson
! Revised    : 2000-08-25   by: Douglas Hanson cpsfcr
! Maturity   : production   2001-07-10
! Purpose    : print utilities.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! print utilities.
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
!</header_word_doc


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
!                hello = PRNFUN     (aaaa, cvar , msg)
!
!                                   i     b      o
!                call    PRNFUN_YYY (bbbb, cvar , msg)
!
!                                        opt    opt
!                                   i     i      o
!                call    PRNFUN_ZZZ (bbbb, indx, value)
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
!     Date        Author       Description
!     ----        ------       -----------
!  3. 2001-07-10  Douglas Hanson cpsfcr  PRODUCTION.
!  2. 2000-06-28  Brad Kruse     Review for standards.
!  1. 1999-12-01  Douglas Hanson Initial version.
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


module prnfun_module
  !
  ! - Module references
  !
  use pc_module
  use named_constants_module
  use getsys_module
  use string_module
  !
  implicit none
  !
  ! - set default to all routines private
  !
  private
  !
  public :: prnfun                ! print a single value and title
  public :: prnfun_clock         ! print clock info
  public :: prnfun_clock_to_card ! print clock info on card
  public :: prnfun_pmax_1d       ! print min, max loc and values of 1d array
  public :: prnfun_pmax_2d       ! print min, max loc and values of 2d array
  public :: prnfun_pmax_3d       ! print min, max loc and values of 3d array

  character(len=100), public, save :: PRNFUN_IDENT = &
    '$Id: prnfun.f90,v 1.3 2001/07/09 19:59:32 sps prod sps $'

  !!---------------------------- interfaces ---------------------------------!!
  !!---------------------------- interfaces ---------------------------------!!
  !!---------------------------- interfaces ---------------------------------!!

  !
  ! - interfaces
  !
! interfaces
      interface prnfun
        module procedure prnfun_i
        module procedure prnfun_r
        module procedure prnfun_c
        module procedure prnfun_l
        module procedure prnfun_2r_1i
        module procedure prnfun_1c
        module procedure prnfun_1r
        module procedure prnfun_2r
        module procedure prnfun_3r
        module procedure prnfun_4r
        module procedure prnfun_5r
        module procedure prnfun_6r
      end interface

  interface prnfun_pmax
    module procedure prnfun_pmax_1d
    module procedure prnfun_pmax_2d
    module procedure prnfun_pmax_3d
  end interface 


contains

  !!------------------------------ prnfun_i --------------------------------!!
  !!------------------------------ prnfun_i --------------------------------!!
  !!------------------------------ prnfun_i --------------------------------!!

  !
  ! - print a single integer value and a title
  !
  subroutine prnfun_i (lu_out, c_title, x)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    integer,             intent (in) :: x
    !
    ! - Begin prnfun_i
    !
    write(lu_out, '(1x, a10, '' = '', i16)') trim (c_title) // '          ', x
    !
  end subroutine prnfun_i


  !!------------------------------ prnfun_r --------------------------------!!
  !!------------------------------ prnfun_r --------------------------------!!
  !!------------------------------ prnfun_r --------------------------------!!

  !
  ! - print a single integer value and a title
  !
  subroutine prnfun_r (lu_out, c_title, x)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    real,                intent (in) :: x
    !
    ! - Begin prnfun_r
    !
    write(lu_out, '(1x, a10, '' = '', g16.9)') trim (c_title) // '          ', x
    !
  end subroutine prnfun_r


  !!------------------------------ prnfun_c --------------------------------!!
  !!------------------------------ prnfun_c --------------------------------!!
  !!------------------------------ prnfun_c --------------------------------!!

  !
  ! - print a single character value and a title
  !
  subroutine prnfun_c(lu_out, c_title, x)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    character (len = *), intent (in) :: x
    !
    ! - Begin prnfun_c
    !
    write(lu_out, '(1x, '' l='', i4, '' '', a16, '' = '', a)') &
       len_trim (x), trim (c_title) // '                ', trim (x)
    !
  end subroutine prnfun_c


!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine prnfun_l ( lu_out, c_title, x)
! print a single integer valu_oute and a title
      implicit  none

      integer   lu_out
      character c_title*(*)
      logical   x

      write ( lu_out, '(1x, a10, '' = '')') &
     & c_title(1:min(10, len_trim(c_title)))
      write ( lu_out, *) x

      return
      end subroutine prnfun_l

  !!------------------------------ prnfun_1c --------------------------------!!
  !!------------------------------ prnfun_1c --------------------------------!!
  !!------------------------------ prnfun_1c --------------------------------!!

  !
  ! - print a single character array and a title
  !
!23456789012345678901234567890123456789012345678901234567890123456789012
      subroutine prnfun_1c ( lu_out, c_title, nx, x1)
! print a single real array and a title
      implicit  none

      integer   lu_out
      character c_title*(*)
      integer   nx
      character x1(:)*(*)
      integer   ix
      integer   lx

      write ( lu_out, '(/, 1x, a10, '' = '', i10)') &
     & c_title(1:min(10, len_trim(c_title))), nx

      do ix = 1 , nx

        lx = len_trim(x1(ix))
        write ( lu_out, '(1x, i10, 1x, i10, 1x, a)')ix, lx, x1(ix)(1:lx)

      end do    ! do ix = 1 , nx

      return
      end subroutine prnfun_1c

  !!------------------------------ prnfun_1r --------------------------------!!
  !!------------------------------ prnfun_1r --------------------------------!!
  !!------------------------------ prnfun_1r --------------------------------!!

  !
  ! - print a single real array and a title
  !
  subroutine prnfun_1r (lu_out, c_title, nx, x1)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    integer,             intent (in) :: nx
    real,                intent (in) :: x1 (nx)
    !
    ! - Local variables
    !
    integer :: ix
    !
    ! - Begin prnfun_1r
    !
    write(lu_out, "(/ 1x, a10, ' = ', i10)")    &
      trim (c_title) // '          ', nx
    !
    do ix = 1 , nx
      write (lu_out, '(1x, i10, 1x, g16.9)') ix, x1 (ix)
    end do    ! do ix = 1 , nx
    !
  end subroutine prnfun_1r


  !!------------------------------ prnfun_2r --------------------------------!!
  !!------------------------------ prnfun_2r --------------------------------!!
  !!------------------------------ prnfun_2r --------------------------------!!

  !
  ! - print a two real arrays and a title
  !
  subroutine prnfun_2r (lu_out, c_title, nx, x1, x2)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    integer,             intent (in) :: nx
    real,                intent (in) :: x1 (nx)
    real,                intent (in) :: x2 (nx)
    !
    ! - Local variables
    !
    integer :: ix
    !
    ! - Begin prnfun_2r
    !
    write (lu_out, "(/ 1x, a10, ' = ', i10)") &
          trim (c_title) // '          ', nx
    !
    do ix = 1 , nx
      write (lu_out, '(1x, i10, 2 (1x, g16.9))') ix, x1 (ix), x2 (ix)
    end do    ! do ix = 1 , nx
    !
  end subroutine prnfun_2r

  !!------------------------------ prnfun_3r --------------------------------!!
  !!------------------------------ prnfun_3r --------------------------------!!
  !!------------------------------ prnfun_3r --------------------------------!!

  !
  ! - print a three real arrays and a title
  !
  subroutine prnfun_3r(lu_out, c_title, nx, x1, x2, x3)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    integer,             intent (in) :: nx
    real,                intent (in) :: x1 (nx)
    real,                intent (in) :: x2 (nx)
    real,                intent (in) :: x3 (nx)
    !
    ! - Local variables
    !
    integer :: ix
    !
    ! - Begin prnfun_3r
    !
    write(lu_out, "(/ 1x, a10, ' = ', i10)")     &
      trim (c_title) // '          ', nx
    !
    do ix = 1 , nx
      write(lu_out, '(1x, i10, 3 (1x, g16.9))') ix, x1 (ix), x2 (ix), x3 (ix)
    end do    ! do ix = 1 , nx
    !
  end subroutine prnfun_3r


  !!------------------------------ prnfun_4r --------------------------------!!
  !!------------------------------ prnfun_4r --------------------------------!!
  !!------------------------------ prnfun_4r --------------------------------!!

  !
  ! - print a four real arrays and a title
  !
  subroutine prnfun_4r(lu_out, c_title, nx, x1, x2, x3, x4)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    integer,             intent (in) :: nx
    real,                intent (in) :: x1 (nx)
    real,                intent (in) :: x2 (nx)
    real,                intent (in) :: x3 (nx)
    real,                intent (in) :: x4 (nx)
    !
    ! - Local variables
    !
    integer :: ix
    !
    ! - Begin prnfun_4r
    !
    write(lu_out, "(/ 1x, a10, ' = ', i10)") &
       trim (c_title) // '          ', nx
    !
    do ix = 1 , nx
      write (lu_out, '(1x, i10, 4 (1x, g16.9))')    &
            ix, x1 (ix), x2 (ix), x3 (ix), x4 (ix)
    end do    ! do ix = 1 , nx
    !
  end subroutine prnfun_4r

  !!------------------------------ prnfun_5r --------------------------------!!
  !!------------------------------ prnfun_5r --------------------------------!!
  !!------------------------------ prnfun_5r --------------------------------!!

  !
  ! - print five real arrays and a title
  !
  subroutine prnfun_5r (lu_out, c_title, nx, x1, x2, x3, x4, x5)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    integer,             intent (in) :: nx
    real,                intent (in) :: x1 (nx)
    real,                intent (in) :: x2 (nx)
    real,                intent (in) :: x3 (nx)
    real,                intent (in) :: x4 (nx)
    real,                intent (in) :: x5 (nx)
    !
    ! - Local variables
    !
    integer :: ix
    !
    ! - Begin prnfun_5r
    !
    write (lu_out, "(/ 1x, a10, ' = ', i10)") &
     & trim (c_title) // '          ', nx
    !
    do ix = 1 , nx
      write (lu_out, '(1x, i10, 5 (1x, g16.9))') &
            ix, x1 (ix), x2 (ix), x3 (ix), x4 (ix), x5 (ix)
    end do    ! do ix = 1 , nx
    !
  end subroutine prnfun_5r


  !!------------------------------ prnfun_6r --------------------------------!!
  !!------------------------------ prnfun_6r --------------------------------!!
  !!------------------------------ prnfun_6r --------------------------------!!

  !
  ! - print six real arrays and a title
  !
  subroutine prnfun_6r (lu_out, c_title, nx, x1, x2, x3, x4, x5, x6)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    integer,             intent (in) :: nx
    real,                intent (in) :: x1 (nx)
    real,                intent (in) :: x2 (nx)
    real,                intent (in) :: x3 (nx)
    real,                intent (in) :: x4 (nx)
    real,                intent (in) :: x5 (nx)
    real,                intent (in) :: x6 (nx)
    !
    ! - Local variables
    !
    integer :: ix
    !
    ! - Begin prnfun_6r
    !
    write (lu_out, "(/ 1x, a10, ' = ', i10)") &
      trim (c_title) // '          ', nx
    !
    do ix = 1 , nx
      write (lu_out, '(1x, i10, 6 (1x, g16.9))') &
            ix, x1 (ix), x2 (ix), x3 (ix), x4 (ix), x5 (ix), x6 (ix)
    end do    ! do ix = 1 , nx
    !
  end subroutine prnfun_6r


  !!---------------------------- prnfun_2r_1i ------------------------------!!
  !!---------------------------- prnfun_2r_1i ------------------------------!!
  !!---------------------------- prnfun_2r_1i ------------------------------!!

  !
  ! - print two real and one integer arrays and a title
  !
  subroutine prnfun_2r_1i (lu_out, c_title, nx, x1, x2, x3)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    integer,             intent (in) :: nx
    real,                intent (in) :: x1 (nx)
    real,                intent (in) :: x2 (nx)
    integer,             intent (in) :: x3 (nx)
    !
    ! - Local variables
    !
    integer :: ix
    !
    ! - Begin prnfun_2r_1i
    !
    write (lu_out, "(/ 1x, a10, ' = ', i10)") &
     & trim (c_title) // '          ', nx
    !
    do ix = 1 , nx
      write (lu_out, '(1x, i10, 2(1x, g16.9), 1x, i8)') &
             ix, x1 (ix), x2 (ix), x3 (ix)
    end do    ! do ix = 1 , nx
    !
  end subroutine prnfun_2r_1i


  !!---------------------------- prnfun_clock ------------------------------!!
  !!---------------------------- prnfun_clock ------------------------------!!
  !!---------------------------- prnfun_clock ------------------------------!!

  !
  ! - print the current datE, time and cpu usage
  !
  subroutine prnfun_clock (lu_out, c_title)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    !
    ! - Local variables
    !
    character (len = 16) :: c_date
    character (len = 16) :: c_time
    real                 :: t_cpu
    !
    ! - Begin prnfun_clock
    !
    call string_date(c_date)   ! current date
    call string_time(c_time)   ! current time
    t_cpu = getsys_seconds()   ! current cpu time
    !
    write (lu_out, '(1x, a20, '' date='', a12, '' &
                     & time='', a12, '' cpu='', f12.5)') &
            trim (c_title) // '                    ', &
            trim (c_date), &
            trim (c_time), &
            t_cpu
    !
  end subroutine prnfun_clock


  !!------------------------ prnfun_clock_to_card --------------------------!!
  !!------------------------ prnfun_clock_to_card --------------------------!!
  !!------------------------ prnfun_clock_to_card --------------------------!!

  !
  ! - print clock info on card
  !
  subroutine prnfun_clock_to_card (card)
    !
    ! - Arguments
    !
    character (len = *), intent (inout) :: card
    !
    ! - Local variables
    !
    character (len = 16) :: c_date
    character (len = 16) :: c_time
    character (len = 80) :: crd80
    !
    ! - Begin prnfun_clock_to_card
    !
    call string_date(c_date)   ! current date
    call string_time(c_time)   ! current time
    !
    write (crd80, '('' date='', a12, '' time='',a12 )') &
          trim (c_date), &
          trim (c_time)
    !
    card = trim (card) // ' ' // trim (crd80)
    !
  end subroutine prnfun_clock_to_card


  !!--------------------------- prnfun_pmax_1d -----------------------------!!
  !!--------------------------- prnfun_pmax_1d -----------------------------!!
  !!--------------------------- prnfun_pmax_1d -----------------------------!!

  subroutine prnfun_pmax_1d (lu_out, c_title, n1, x)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    integer,             intent (in) :: n1
    real,                intent (in) :: x (n1)
    !
    ! - Local variables
    !
    integer :: ix_min (1)
    integer :: ix_max (1)
    !
    ! - Begin prnfun_pmax_1d
    !
    ix_min = minloc (x)
    ix_max = minloc (x)
    !
    write (lu_out, '(a12, '' n1='', i8,                       &
                     & /, '' min='', 1x, g14.6, 1x, i8,       &
                     & /, '' max='', 1x, g14.6, 1x, i8)')     &
          trim (c_title) // '            ',                   &
          n1, x (ix_min (1)), ix_min,                         &
              x (ix_max (1)), ix_max
    !
  end subroutine prnfun_pmax_1d

  !!----------------------------- subroutines ------------------------------!!
  subroutine prnfun_pmax_2d (lu_out, c_title, n1, n2, x)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    integer,             intent (in) :: n1
    integer,             intent (in) :: n2
    real,                intent (in) :: x (n1, n2)
    !
    ! - Local variables
    !
    integer :: ix_min (2)
    integer :: ix_max (2)
    !
    ! - Begin prnfun_pmax_2d
    !
    ix_min = minloc (x)
    ix_max = minloc (x)
    !
    write (lu_out, '(a12, '' n1='', i8, '' n2='', i8,                &
                     & /, '' min='', 1x, g14.6, 1x, i8, 1x, i8,      &
                     & /, '' max='', 1x, g14.6, 1x, i8, 1x, i8)')    &
          trim (c_title) // '            ',                          &
          n1, n2, x (ix_min (1), ix_min (2)), ix_min,                &
                  x (ix_max (1), ix_max (2)), ix_max
    !
  end subroutine prnfun_pmax_2d


  !!--------------------------- prnfun_pmax_3d -----------------------------!!
  !!--------------------------- prnfun_pmax_3d -----------------------------!!
  !!--------------------------- prnfun_pmax_3d -----------------------------!!

  subroutine prnfun_pmax_3d (lu_out, c_title, n1, n2, n3, x)
    !
    ! - Arguments
    !
    integer,             intent (in) :: lu_out
    character (len = *), intent (in) :: c_title
    integer,             intent (in) :: n1
    integer,             intent (in) :: n2
    integer,             intent (in) :: n3
    real,                intent (in) :: x (n1, n2, n3)
    !
    ! - Local variables
    !
    integer :: ix_min (3)
    integer :: ix_max (3)
    !
    ! - Begin prnfun_pmax_3d
    !
    ix_min = minloc (x)
    ix_max = minloc (x)
    !
    write (lu_out, '(a12, '' n1='', i8, '' n2='', i8, '' n3='', i8,    &
                     & /, '' min='', 1x, g14.6, 3 (1x, i8),            &
                     & /, '' max='', 1x, g14.6, 3 (1x, i8))')          &
          trim (c_title) // '            ',                            &
          n1, n2, n3, x (ix_min (1), ix_min (2), ix_min (3)), ix_min,  &
                      x (ix_max (1), ix_max (2), ix_max (3)), ix_max
    !
  end subroutine prnfun_pmax_3d


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

end module prnfun_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

