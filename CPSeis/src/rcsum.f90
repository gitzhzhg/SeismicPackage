!<CPS_v1 type="PRIMITIVE"/>
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
! Name       : rcsum  (ReCursive SUM of a vector)
! Category   : math
! Written    : 1999-08-12   by: Randy L. Selzler, Data-Warp, Inc.
! Revised    : 2000-07-20   by: Randy L. Selzler, Data-Warp, Inc.
! Maturity   : production   2000-08-22
! Purpose    : Recursive sum of a vector
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!
! Recursive sum of a vector.
! Portable implementation of RCSUM from the Cray BNCHLIB library.
!
! "After a call to RCSUM the user can, with one operation, calculate the sum
!   of the values in (A) from any value to any other value.  For Example, the
!   sum of (A) from 22 to 35 equals B(35)-B(21).  The best use of this routine
!   is the case where the user needs many sums that overlay each other.
!   RCSUM does a minimum of operations and provides vector performance.
!
!   A and B must be different addresses."
!
!   Excerpt taken from Cray's RCSUM Man page.
!
!
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<calling_doc>
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!            i  i  o
! call RCSUM(N, A, B)
!
! integer           N   =       --> Length of A and B vectors
! real              A   =       --> Vector to be summed
! real              B   =       --> Output Vector B(1)=A(1),B(2)=A(1)+A(2),...
!
!   NOTE: "A and B must be different addresses."
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  4. 2000-08-22  Selzler      Corrected header comments
!  3. 2000-01-25  Selzler      Clean up trailing blanks and block labels
!  2. 1999-11-19  Selzler      Added RCS "Id" strings to tag executeable
!  1. 1999-08-12  Selzler      Initial version.
!
!
!-------------------------------------------------------------------------------
!</history_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module rcsum_module
      implicit none

      private
      public :: rcsum

      character(len=100),public :: rcsum_ident = &
        "$Id: rcsum.f90,v 1.4 2000/08/22 14:12:20 sps prod sps $"

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

      subroutine rcsum(n, a, b)
      implicit none
      integer             :: n  ! argument
      real, dimension(n)  :: a  ! argument
      real, dimension(n)  :: b  ! argument

      integer             :: i  ! local

      b(1) = a(1)

      do i = 2, n
        b(i) = b(i-1) + a(i)
      end do

      return

      end subroutine rcsum

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module rcsum_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

