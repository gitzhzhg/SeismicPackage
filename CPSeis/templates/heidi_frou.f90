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
!!!         Auxiliary Fortran Template for Fortran to C Interfaces
!!!
!!!-----------------------------------------------------------------------------
!!!                   REVISION HISTORY FOR THIS TEMPLATE
!!!
!!!     Date        Author     Description
!!!     ----        ------     -----------
!!!  4. 2002-10-23  Stoeckley  Minor history doc and ident string changes.
!!!  3. 2002-06-07  Stoeckley  Remove unnecessary RETURN statements.
!!!  2. 2002-05-20  Stoeckley  Add brief doc section and this template
!!!                             revision history.
!!!  1. 2001-10-23  Stoeckley  Initial version (replaces BOTCH).
!!!
!!!-----------------------------------------------------------------------------
!!!
!!! This file can be used as a template for Fortran-to-C interfaces.
!!! See c2f_interface.h (and heidi_wrapper.c) for details.
!!!
!!! Note: This module is identical to andrew_frou.f90
!!!       except for the names andrew and heidi.
!!!
!!!-----------------------------------------------------------------------------


!<CPS_v1 type="AUXILIARY_FILE"/>
!!----------------------------- heidi_frou.f90 -----------------------------!!
!!----------------------------- heidi_frou.f90 -----------------------------!!
!!----------------------------- heidi_frou.f90 -----------------------------!!

       ! other files are:  heidi_wrapper.c  heidi_wrapper.h  heidi.f90



!<brief_doc>
!-------------------------------------------------------------------------------
!                   C P S   A U X I L I A R Y   F I L E
!
! Name       : HEIDI_FROU
! Category   : --> should match the main file of this primitive.
! Written    : 2001-01-01   by: NNNN
! Revised    : 2001-01-01   by: NNNN
! Maturity   : beta
! Purpose    : --> should match the main file of this primitive.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                     AUXILIARY FILE REVISION HISTORY 
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2001-01-01  NNNN       Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>



!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!
!!------------------------------ module -----------------------------------!!


      module heidi_frou_module
      use heidi_module
      use string_module
      implicit none

      character(len=100),public,save :: HEIDI_FROU_IDENT = &
'$Id: heidi_frou.f90,v 1.4 2002/10/23 21:22:03 Stoeckley custom sps $'

      type :: heidi_frou_struct
        type(heidi_struct),pointer :: obj
      end type

      end module heidi_frou_module


!!------------------------------ create -----------------------------------!!
!!------------------------------ create -----------------------------------!!
!!------------------------------ create -----------------------------------!!


      subroutine heidi_frou_create (fpoint)
      use heidi_frou_module
      implicit none
      type(heidi_frou_struct),intent(out) :: fpoint     ! arguments
      type(heidi_struct)     ,pointer     :: obj        ! local

      call heidi_create (obj)
      fpoint%obj => obj
      end subroutine heidi_frou_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine heidi_frou_delete (fpoint)
      use heidi_frou_module
      implicit none
      type(heidi_frou_struct),intent(inout) :: fpoint     ! arguments
      type(heidi_struct)     ,pointer       :: obj        ! local

      obj => fpoint%obj
      call heidi_delete (obj)
      fpoint%obj => obj
      end subroutine heidi_frou_delete


!!------------------------------ solve -----------------------------------!!
!!------------------------------ solve -----------------------------------!!
!!------------------------------ solve -----------------------------------!!


      function heidi_frou_solve (fpoint,fff,ddd,ccc) result (iii)
      use heidi_frou_module
      implicit none
      type(heidi_frou_struct),intent(inout) :: fpoint          ! arguments
      real                   ,intent(out)   :: fff(HEIDI_NFFF) ! arguments
      double precision       ,intent(in)    :: ddd             ! arguments
      integer                ,intent(inout) :: ccc(*)          ! arguments
      integer                               :: iii             ! result
      type(heidi_struct)     ,pointer       :: obj             ! local
      character(len=HEIDI_NCCC)             :: ccc9            ! local

      obj => fpoint%obj
      call  string_hh2cc (ccc,ccc9)          ! only if ccc is (in) or (inout).
      iii = heidi_solve  (obj,fff,ddd,ccc9)
      call  string_cc2hh (ccc9,ccc)          ! only if ccc is (out) or (inout).
      end function heidi_frou_solve


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

