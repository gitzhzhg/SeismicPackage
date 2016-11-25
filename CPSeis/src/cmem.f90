!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- cmem.f90 --------------------------------!!
!!------------------------------- cmem.f90 --------------------------------!!
!!------------------------------- cmem.f90 --------------------------------!!

        ! other files are:  cmem_crou.c
 
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
! Name       : CMEM 
! Category   : memory
! Written    : 2003-12-17   by: R. Selzler
! Revised    : 2004-01-21   by: R. Selzler
! Maturity   : production
! Purpose    : memory utilities, similar to the Standard C library.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Fortran interface to memory utilities, similar to the standard C library.
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
!!                 o    i   i
! call cmem_cpy_c(s,  ct,  n)
!     Copy n characters (bytes) from ct to s.
!
!                  o    i   i
! call cmem_move_c(s,  ct,  n)
!     NOT IMPLEMENTED YET
!     Same as cmem_cpy except that is works even if the object overlap.
!
!                 o   i   i
! call cmem_set_c(s,  c,  n)
!     NOT IMPLEMENTED YET
!     Place character c (low order byte) into first n characters (bytes) of s.
!
! These are subroutine, not functions, and do not return a value.
!
! Arg: s = any assignable array or scalar.
! Arg: ct = any array or scalar.
! Arg: n = integer, greater than or equal to zero.
! Arg: c = integer representation of a character.
!            Zero is suitable for zeroing memory.
!            ichar('x') is suitable for character fills.
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2004-01-21  R Selzler  Overloaded interface with more routines.
!  1. 2003-12-17  R Selzler  Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module cmem_module
      implicit none

      private
      public :: cmem_cpy
      !public :: cmem_move
      !public :: cmem_set

      character(len=100),public,save :: CMEM_IDENT = &
'$Id: cmem.f90,v 1.2 2004/01/21 12:36:06 Selzler prod sps $'

!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!



!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

      external cmem_cpy_c
      external cmem_move_c
      external cmem_set_c

      interface cmem_cpy
        module procedure cmem_cpy_dbl_1d_int_1d
        module procedure cmem_cpy_int_1d_dbl_1d
        module procedure cmem_cpy_int_1d_dbl_2d
        module procedure cmem_cpy_int_1d_real_1d
        module procedure cmem_cpy_int_1d_str
        module procedure cmem_cpy_int_1d_str_1d
        module procedure cmem_cpy_int_str
        module procedure cmem_cpy_int_str_1d
        module procedure cmem_cpy_real_1d_dbl_1d
        module procedure cmem_cpy_real_1d_int_1d
        module procedure cmem_cpy_real_1d_str
        module procedure cmem_cpy_str_1d_int_1d
        module procedure cmem_cpy_str_1d_real_1d
        module procedure cmem_cpy_str_int
        module procedure cmem_cpy_str_int_1d
      end interface

      contains


!!----------------------------- cmem_cpy ------------------------------------!!
!!----------------------------- cmem_cpy ------------------------------------!!
!!----------------------------- cmem_cpy ------------------------------------!!


  subroutine cmem_cpy_dbl_1d_int_1d(s, ct, n)
    double precision, intent(out), dimension(:) :: s
    integer, intent(in), dimension(:)           :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_dbl_1d_int_1d

  subroutine cmem_cpy_int_1d_dbl_1d(s, ct, n)
    integer, intent(out), dimension(:)          :: s
    double precision, intent(in), dimension(:)  :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_int_1d_dbl_1d

  subroutine cmem_cpy_int_1d_dbl_2d(s, ct, n)
    integer, intent(out), dimension(:)          :: s
    double precision, intent(in), dimension(:,:)  :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_int_1d_dbl_2d

  subroutine cmem_cpy_int_1d_real_1d(s, ct, n)
    integer, intent(out), dimension(:)          :: s
    real, intent(in), dimension(:)              :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_int_1d_real_1d

  subroutine cmem_cpy_int_1d_str(s, ct, n)
    integer, intent(out), dimension(:)          :: s
    character(len=*),intent(in)                 :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_int_1d_str

  subroutine cmem_cpy_int_1d_str_1d(s, ct, n)
    integer, intent(out), dimension(:)          :: s
    character(len=*),intent(in),dimension(:)    :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_int_1d_str_1d

  subroutine cmem_cpy_int_str(s, ct, n)
    integer, intent(out)                        :: s
    character(len=*), intent(in)                :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_int_str

  subroutine cmem_cpy_int_str_1d(s, ct, n)
    integer, intent(out)                        :: s
    character(len=*),intent(in),dimension(:)    :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_int_str_1d

  subroutine cmem_cpy_real_1d_dbl_1d(s, ct, n)
    real, intent(out), dimension(:)             :: s
    double precision, intent(in), dimension(:)  :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_real_1d_dbl_1d

  subroutine cmem_cpy_real_1d_str(s, ct, n)
    real, intent(out), dimension(:)             :: s
    character(len=*),intent(in)                 :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_real_1d_str

  subroutine cmem_cpy_real_1d_int_1d(s, ct, n)
    real, intent(out), dimension(:)             :: s
    integer, intent(in), dimension(:)           :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_real_1d_int_1d

  subroutine cmem_cpy_str_1d_int_1d(s, ct, n)
    character(len=*),intent(out),dimension(:)   :: s
    integer, intent(in), dimension(:)           :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_str_1d_int_1d

  subroutine cmem_cpy_str_1d_real_1d(s, ct, n)
    character(len=*),intent(out),dimension(:)   :: s
    real, intent(in), dimension(:)              :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_str_1d_real_1d

  subroutine cmem_cpy_str_int(s, ct, n)
    character(len=*), intent(out)               :: s
    integer, intent(in)                         :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_str_int

  subroutine cmem_cpy_str_int_1d(s, ct, n)
    character(len=*), intent(out)               :: s
    integer, intent(in), dimension(:)           :: ct
    integer, intent(in)                         :: n
    call cmem_cpy_c(s, ct, n)
  end subroutine cmem_cpy_str_int_1d

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module cmem_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

