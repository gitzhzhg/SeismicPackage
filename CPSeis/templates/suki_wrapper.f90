!!!
C<license>
C-------------------------------------------------------------------------------
C Copyright (c) 2007 ConocoPhillips Company
C
C Permission is hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to deal
C in the Software without restriction, including without limitation the rights
C to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
C copies of the Software, and to permit persons to whom the Software is
C furnished to do so, subject to the following conditions:
C
C The above copyright notice and this permission notice shall be included in all
C copies or substantial portions of the Software.
C
C THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
C IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
C AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
C LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
C OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
C SOFTWARE.
C-------------------------------------------------------------------------------
C</license>
!!!          Fortran Wrapper Template for Fortran to C++ Interfaces
!!!
!!!-----------------------------------------------------------------------------
!!!                   REVISION HISTORY FOR THIS TEMPLATE
!!!
!!!     Date        Author     Description
!!!     ----        ------     -----------
!!!  4. 2002-10-23  Stoeckley  Minor history doc and ident string changes.
!!!  3. 2002-06-07  Stoeckley  Remove unnecessary IMPLICIT NONE and RETURN
!!!                             statements.
!!!  2. 2002-05-16  Stoeckley  Add this template revision history.
!!!  1. 2001-10-23  Stoeckley  Initial version.
!!!
!!!-----------------------------------------------------------------------------
!!!
!!! This file can be used as a template for Fortran to C++ interfaces.
!!! See c2f_interface.h (and below) for details.
!!!
!!! Note: This module is identical to kathy_wrapper.f90
!!!       except for the names suki and kathy.
!!!
!!! This primitive named SUKI_WRAPPER is a working example of a Fortran-style
!!! wrapper class which interfaces to the C++ style class named SUKI.
!!!
!!!-----------------------------------------------------------------------------
!!!
!!! The primitives SUKI_WRAPPER and SUKI do no useful work.
!!! The primitives SUKI_WRAPPER and SUKI include the following illustrations:
!!!
!!!  (1) use of a creatable/deletable C++ class with a hidden data structure.
!!!  (2) capability of Fortran code to store a C++ pointer to the C++ object.
!!!  (3) conversions of variables between Fortran and C variable types.
!!!  (4) dealing correctly with char strings passed between C++ and Fortran.
!!!
!!! The following files are used (in the order called):
!!!
!!!  suki_wrapper.f90  Fortran wrapper class which interfaces to the C++ class.
!!!  suki_crou.cc      Private auxiliary file.
!!!  suki.hh           Original C++ class header file.
!!!  suki.cc           Original C++ class implementation file.
!!!
!!! To use the original C++ class SUKI from Fortran, call this Fortran wrapper
!!! class SUKI_WRAPPER, which looks like any other Fortran class.  For example:
!!!
!!!          program test
!!!            use suki_wrapper_module
!!!            type(suki_wrapper_struct),pointer :: suki
!!!            call suki_wrapper_create (suki)
!!!            call suki_wrapper_solve  (suki)
!!!            call suki_wrapper_delete (suki)
!!!          end program
!!!
!!!-----------------------------------------------------------------------------


!<CPS_v1 type="PRIMITIVE"/>
!!--------------------------- suki_wrapper.f90 ---------------------------!!
!!--------------------------- suki_wrapper.f90 ---------------------------!!
!!--------------------------- suki_wrapper.f90 ---------------------------!!

         ! other files are:  suki.cc  suki.hh  suki_crou.cc



!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : SUKI_WRAPPER
! Category   : --> should match the main file of this primitive.          
! Written    : 2001-01-01   by: NNNN
! Revised    : 2001-01-01   by: NNNN
! Maturity   : beta
! Purpose    : --> should match the main file of this primitive.          
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!
!-------------------------------------------------------------------------------
!</descript_doc>


!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!        i = value required upon INPUT.
!        o = value set by the routine upon OUTPUT.
!        b = value BOTH required upon input and changed upon output.
!
!  For pointers, the flag (i,o,b) refers to the contents pointed to
!  by the pointer, not to the value of the pointer itself.  The pointer
!  value is required upon INPUT in all cases.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!                                       o
!           call  suki_wrapper_create (obj)
!
!            o                          b    o    i    b
!           iii = suki_wrapper_solve  (obj, fff, ddd, ccc);
!
!                                       b
!           call  suki_wrapper_delete (obj);
!
!
! type(suki_wrapper_struct) obj    = pointer to the SUKI_WRAPPER object.
! real                      fff(:) = description of this output array.
! double precision          ddd    = description of this input variable.
! character(len=*)          ccc    = description of this input/output string.
! integer                   iii    = description of this returned variable.
!
! fff must be dimensioned at least (SUKI_WRAPPER_NFFF).
! ccc must have length at least    (SUKI_WRAPPER_NCCC).
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2001-01-01  NNNN       Initial version.
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module suki_wrapper_module
      use named_constants_module
      use string_module
      implicit none
      public

      character(len=100),public,save :: SUKI_WRAPPER_IDENT = &
'$Id: suki_wrapper.f90,v 1.4 2002/10/24 13:15:57 Stoeckley custom sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


 integer,public,parameter :: SUKI_WRAPPER_NFFF =  4    ! same as SUKI_NFFF
 integer,public,parameter :: SUKI_WRAPPER_NCCC = 20    ! same as SUKI_NCCC - 1


      type,public :: suki_wrapper_struct
        private
        type(CPOINTER) :: cpoint
        !!! plus anything not used by the original C class.
      end type suki_wrapper_struct


!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!


      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine suki_wrapper_create (obj)
      type(suki_wrapper_struct),pointer :: obj        ! arguments

      allocate (obj)
      call suki_crou_create (obj%cpoint)
      end subroutine suki_wrapper_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine suki_wrapper_delete (obj)
      type(suki_wrapper_struct),pointer :: obj        ! arguments

      call suki_crou_delete (obj%cpoint)
      deallocate (obj)
      end subroutine suki_wrapper_delete


!!-------------------------------- solve -----------------------------------!!
!!-------------------------------- solve -----------------------------------!!
!!-------------------------------- solve -----------------------------------!!


      function suki_wrapper_solve (obj,fff,ddd,ccc) result (iii)
      type(suki_wrapper_struct),intent(inout)  :: obj             ! arguments
      real                     ,intent(out)    :: fff(:)          ! arguments
      double precision         ,intent(in)     :: ddd             ! arguments
      character(len=*)         ,intent(inout)  :: ccc             ! arguments
      integer                                  :: iii             ! result
      integer                   :: ccc9(SUKI_WRAPPER_NCCC/4 + 1)  ! local
      integer                   :: suki_crou_solve                ! local

      call   string_cc2hh    (ccc,ccc9)     ! only if ccc is (in) or (inout).
      iii  = suki_crou_solve (obj%cpoint,fff,ddd,ccc9)
      call   string_hh2cc    (ccc9,ccc)     ! only if ccc is (out) or (inout).
      end function suki_wrapper_solve


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module suki_wrapper_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

