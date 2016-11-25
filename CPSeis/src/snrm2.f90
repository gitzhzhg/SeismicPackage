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
! Name       : snrm2 (Euclidean NoRM of a vector)
! Category   : math
! Written    : 1999-08-27   by: Randy L. Selzler
! Revised    : 2000-07-19   by: Randy Selzler, Data-Warp, Inc.
! Maturity   : production   
! Purpose    : Computes the Euclidean norm of a vector
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!
! Note: comments and description are derived from Cray manual pages.
!
! snrm2_real computes the Euclidean (l2) norm of a real vector.
!
! snrm2_complex computes the Euclidean (l2) norm of a complex vector.
!
! For performance reasons, they do not scale the input values.
! Input for the routines must be within a certain range of numbers.
!
!
!-------------------------------------------------------------------------------
!</descript_doc>

!<trace_io_doc>
!-------------------------------------------------------------------------------
!                     TRACE INPUT/OUTPUT REQUIREMENTS
!
!-------------------------------------------------------------------------------
!</trace_io_doc>

!<global_doc>
!-------------------------------------------------------------------------------
!                           GLOBAL PARAMETERS
!
!-------------------------------------------------------------------------------
!</global_doc>

!<header_word_doc>
!-------------------------------------------------------------------------------
!                          TRACE HEADER WORDS
!
!-------------------------------------------------------------------------------
!</header_word_doc

!<calling_doc>
!-------------------------------------------------------------------------------
!                      INPUT AND OUTPUT ARGUMENTS
!
! For each subroutine or function documented below, each argument is
! flagged as follows:
!      i = value required upon INPUT.
!      o = value set by the routine upon OUTPUT.
!      b = value BOTH required upon input and changed upon output.
!
!-------------------------------------------------------------------------------
!                          CALLING SEQUENCE
!
!      o                 i   i   i

!     enrm = snrm2_real (n, rx, incx)
!
!      o                    i   i   i

!     enrm = snrm2_complex (n, cx, incx)
!
! real enrm         = output scalar

! integer n         = number of elements in operand vector.
!                     If zero, enrm = 0.0.

! real rx           = real input array of (n-1)*|incx|+1

! complex cx        = complex input array of (n-1)*|incx|+1

! integer incx      = incremenet between elements of X.  If incx = 0,
!                     the results will be unpredictable.

!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  4. 2000-07-19  Selzler      Fixed problems found by CPS Fortran Code Review.
!  3. 2000-01-25  Selzler      Clean up trailing blanks and block labels
!  2. 1999-11-19  Selzler      Added RCS "Id" strings to tag executeable
!  1. 1999-08-27  Selzler      Initial version. New implementation of
!                              CrayLibs 2.0 functions in Fortran 90.
!
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
!                     SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>

!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS
!
!  snrm2_real computes the Euclidean (l2) norm of a real vector.

!                                     n
!       enrm <-- || rx ||  =  SQRT ( Sum  rx(i)**2 )
!                        2           i=1

!  snrm2_complex computes the Euclidean (l2) norm of a complex vector.
!                                     n
!       enrm <-- || cx ||  =  SQRT ( Sum  (conjugate of cx(i))*cx(i) )
!                        2           i=1
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES
!
!-------------------------------------------------------------------------------
!</programming_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module snrm2_module
      implicit none

      private
      public :: snrm2_real
      public :: snrm2_complex

      character(len=100),public :: snrm2_ident = &
        "$Id: snrm2.f90,v 1.4 2000/07/18 22:17:11 sps prod sps $"

      contains

!!----------------------------- snrm2_real -------------------------------!!
!!----------------------------- snrm2_real -------------------------------!!
!!----------------------------- snrm2_real -------------------------------!!

      function snrm2_real (n, rx, incx)
      implicit none
      real snrm2_real           ! result
      integer n                 ! argument
      real, dimension(:) :: rx  ! argument
      integer incx              ! argument

      if(incx > 0) then
        snrm2_real = sqrt(sum(rx(1:n:incx) * rx(1:n:incx)))
      else
        snrm2_real = sqrt(sum(rx(n:1:incx) * rx(n:1:incx)))
      endif

      return
      end function snrm2_real

!!----------------------------- snrm2_complex -------------------------------!!
!!----------------------------- snrm2_complex -------------------------------!!
!!----------------------------- snrm2_complex -------------------------------!!

      function snrm2_complex (n, cx, incx)
      implicit none
      real snrm2_complex           ! result
      integer n                    ! argument
      complex, dimension(:) ::cx   ! argument
      integer incx                 ! argument

      if(incx > 0) then
        snrm2_complex = sqrt(sum(cx(1:n:incx) * conjg(cx(1:n:incx))))
      else
        snrm2_complex = sqrt(sum(cx(n:1:incx) * conjg(cx(n:1:incx))))
      endif

      return
      end function snrm2_complex

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module snrm2_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
