!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------ gausselim.f90 -------------------------------!!
!!------------------------------ gausselim.f90 -------------------------------!!
!!------------------------------ gausselim.f90 -------------------------------!!

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
! Name       : GAUSSELIM (Gaussian Elimination)
! Category   : math
! Written    : 1987-07-15   by: Bob Baumel (originally LINEQS & MATINV)
! Revised    : 2000-02-17   by: Bob Baumel
! Maturity   : production   2000-07-12
! Purpose    : Linear Equations and Matrix Inversion by Gaussian Elimination.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! Gaussian Elimination is a brute-force method of solving a set of linear
! equations. It uses "elementary row operations", which is pretty much the
! same way you might solve the equations by hand (by adding and subtracting
! multiples of the equations to successively eliminate variables until only
! the solution remains).
!
! This method is suitable for small systems of equations that lack special
! symmetries or other features that would allow use of a more efficient
! method. For N equations in N unknowns, this method executes on the order of
! N**3 calculations, so is very slow for large systems. Also, it may easily
! run out of numerical precision for large systems. To partially deal with
! this problem, GAUSSELIM offers a double precision option (Currently, it
! offers choices of real, double precision, or complex arithmetic; however,
! it currently doesn't offer a double-complex option).
!
! As examples of equations for which more efficient methods are available, the
! Toeplitz system that arises in designing a Wiener-Levinson optimal filter
! can be solved more efficiently by Levinson recursion, performed in CPS by
! the OPFILT primitive. Specialized solvers exist for many other cases, such
! as tri-diagonal systems. If you have a large system and you need only an
! approximate solution, you might consider a conjugate gradient method, see
! CONJGRAD primitive.
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
! To solve an n x n system of linear equations of the form "y = a x" :
!
!                                       i  i  o  i   o
!               call  gausselim_lineqs (n, a, x, y, sing)
!
!
! To invert an n x n matrix "a" :
!
!                                       i  i   o     o
!               call  gausselim_matinv (n, a, ainv, sing)
!
!
! integer                   n      = Size of linear system to solve.
! real, double or complex   a(n,n) = Matrix in linear system, or to invert.
! real, double or complex   x(n)   = Returned solution vector in "y = a x".
! real, double or complex   y(n)   = Known column vector in "y = a x".
! logical                   sing   = Returned as true if matrix "a" singular.
! real, double or complex   ainv(n,n) = Returned inverse of "a" matrix.
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS
!
! This module contains overloaded routines so that a, x, y, and ainv may be
! real, double precision, or complex. However, you may not mix types within
! the same call. Thus, a, x, and y must all be the same type when calling
! gausselim_lineqs;  a and ainv must both be the same type when calling
! gausselim_matinv.
!
! Arrays a, x, y, and ainv must have exactly the shapes indicated; e.g.,
! "a" must be dimensioned exactly (n,n), "x" must be exactly (n), etc.
! In case your arrays are dimensioned bigger in the calling program, simply
! pass a slice of this exact size to the subroutine; e.g., you might pass
! a(:n,:n), x(:n), etc.
!
! When matrix "a" is singular (i.e., the equations cannot be solved) both
! routines return sing = true.  Also, in this case, gausselim_lineqs fills
! vector "x" with zeros, and gausselim_matinv fills matrix "ainv" with zeros.
!
! GAUSSELIM reports the matrix as singular (i.e., returns sing = true) only
! when division by an exact machine zero would have occurred in solving the
! equations. Otherwise, it makes no check to ensure accuracy of the solution.
! Thus, even when it returns sing = false, accuracy may sometimes be poor:
! perhaps because the matrix was _nearly_ singular, or perhaps because the
! calculation simply wasn't done with enough numerical precision. In cases
! where numerical precision is of concern, you may call GAUSSELIM with double
! precision arguments.
!
!-------------------------------------------------------------------------------
!</advice_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  4. 2000-02-17  Bob Baumel   Full fortran90 conversion; renamed GAUSSELIM.
!  3. 1999-01-11  K Goodger    Begin using the fortran90 compiler.
!  2. 1987-07-22  Bob Baumel   Added entries for matrix inversion (MATINV).
!  1. 1987-07-15  Bob Baumel   Initial version (originally LINEQS).
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

      module gausselim_module

      implicit none

      private
      public :: gausselim_lineqs
      public :: gausselim_matinv

      character(len=100),public,save :: GAUSSELIM_IDENT = &
       '$Id: gausselim.f90,v 1.4 2000/07/11 13:09:48 sps prod sps $'

!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!

      interface gausselim_lineqs
        module procedure gausselim_lineqsr
        module procedure gausselim_lineqsd
        module procedure gausselim_lineqsc
      end interface

      interface gausselim_matinv
        module procedure gausselim_matinvr
        module procedure gausselim_matinvd
        module procedure gausselim_matinvc
      end interface

      contains

!!------------------------- gausselim_lineqsr -----------------------------!!
!!------------------------- gausselim_lineqsr -----------------------------!!
!!------------------------- gausselim_lineqsr -----------------------------!!

      subroutine gausselim_lineqsr (n, a, x, y, sing)
!
      implicit none
!
      integer , intent (in)  :: n
      real    , intent (in)  :: a (n, n)
      real    , intent (out) :: x (n)
      real    , intent (in)  :: y (n)
      logical , intent (out) :: sing
!
      real :: matrix (n, n+1)
!
      matrix(:, :n ) = a
      matrix(:, n+1) = y
      call gausselim_real (n, n+1, matrix, sing)
      if (.not. sing) then
        x = matrix(:, n+1)
      else
        x = 0.
      end if
      return
!
      end subroutine gausselim_lineqsr

!!------------------------- gausselim_lineqsd -----------------------------!!
!!------------------------- gausselim_lineqsd -----------------------------!!
!!------------------------- gausselim_lineqsd -----------------------------!!

      subroutine gausselim_lineqsd (n, a, x, y, sing)
!
      implicit none
!
      integer          , intent (in)  :: n
      double precision , intent (in)  :: a (n, n)
      double precision , intent (out) :: x (n)
      double precision , intent (in)  :: y (n)
      logical          , intent (out) :: sing
!
      double precision :: matrix (n, n+1)
!
      matrix(:, :n ) = a
      matrix(:, n+1) = y
      call gausselim_double (n, n+1, matrix, sing)
      if (.not. sing) then
        x = matrix(:, n+1)
      else
        x = 0.
      end if
      return
!
      end subroutine gausselim_lineqsd

!!------------------------- gausselim_lineqsc -----------------------------!!
!!------------------------- gausselim_lineqsc -----------------------------!!
!!------------------------- gausselim_lineqsc -----------------------------!!

      subroutine gausselim_lineqsc (n, a, x, y, sing)
!
      implicit none
!
      integer , intent (in)  :: n
      complex , intent (in)  :: a (n, n)
      complex , intent (out) :: x (n)
      complex , intent (in)  :: y (n)
      logical , intent (out) :: sing
!
      complex :: matrix (n, n+1)
!
      matrix(:, :n ) = a
      matrix(:, n+1) = y
      call gausselim_complex (n, n+1, matrix, sing)
      if (.not. sing) then
        x = matrix(:, n+1)
      else
        x = (0.,0.)
      end if
      return
!
      end subroutine gausselim_lineqsc

!!------------------------- gausselim_matinvr -----------------------------!!
!!------------------------- gausselim_matinvr -----------------------------!!
!!------------------------- gausselim_matinvr -----------------------------!!

      subroutine gausselim_matinvr (n, a, ainv, sing)
!
      implicit none
!
      integer , intent (in)  :: n
      real    , intent (in)  :: a (n, n)
      real    , intent (out) :: ainv (n, n)
      logical , intent (out) :: sing
!
      real    :: matrix (n, 2*n)
      integer :: i
!
      matrix(:,  :n ) = a
      matrix(:, n+1:) = 0.
      do i = 1, n
        matrix(i, n+i) = 1.
      end do
      call gausselim_real (n, 2*n, matrix, sing)
      if (.not. sing) then
        ainv = matrix(:, n+1:)
      else
        ainv = 0.
      end if
      return
!
      end subroutine gausselim_matinvr

!!------------------------- gausselim_matinvd -----------------------------!!
!!------------------------- gausselim_matinvd -----------------------------!!
!!------------------------- gausselim_matinvd -----------------------------!!

      subroutine gausselim_matinvd (n, a, ainv, sing)
!
      implicit none
!
      integer          , intent (in)  :: n
      double precision , intent (in)  :: a (n, n)
      double precision , intent (out) :: ainv (n, n)
      logical          , intent (out) :: sing
!
      double precision :: matrix (n, 2*n)
      integer          :: i
!
      matrix(:,  :n ) = a
      matrix(:, n+1:) = 0.
      do i = 1, n
        matrix(i, n+i) = 1.
      end do
      call gausselim_double (n, 2*n, matrix, sing)
      if (.not. sing) then
        ainv = matrix(:, n+1:)
      else
        ainv = 0.
      end if
      return
!
      end subroutine gausselim_matinvd

!!------------------------- gausselim_matinvc -----------------------------!!
!!------------------------- gausselim_matinvc -----------------------------!!
!!------------------------- gausselim_matinvc -----------------------------!!

      subroutine gausselim_matinvc (n, a, ainv, sing)
!
      implicit none
!
      integer , intent (in)  :: n
      complex , intent (in)  :: a (n, n)
      complex , intent (out) :: ainv (n, n)
      logical , intent (out) :: sing
!
      complex :: matrix (n, 2*n)
      integer :: i
!
      matrix(:,  :n ) = a
      matrix(:, n+1:) = (0.,0.)
      do i = 1, n
        matrix(i, n+i) = (1.,0.)
      end do
      call gausselim_complex (n, 2*n, matrix, sing)
      if (.not. sing) then
        ainv = matrix(:, n+1:)
      else
        ainv = (0.,0.)
      end if
      return
!
      end subroutine gausselim_matinvc

!!--------------------------- gausselim_real ------------------------------!!
!!--------------------------- gausselim_real ------------------------------!!
!!--------------------------- gausselim_real ------------------------------!!

      subroutine gausselim_real (nrows, ncols, matrix, sing)
!
      implicit none
!
      integer , intent (in)    :: nrows, ncols
      real    , intent (inout) :: matrix (nrows, ncols)
      logical , intent (out)   :: sing
!
      real    :: temp (ncols)
      real    :: factor
      integer :: i, i1, iarray(1), imax, j
!
      sing = .false.
      do i = 1, nrows
        i1 = i + 1
        iarray = maxloc ( abs(matrix(i: ,i)) )
        imax = i - 1 + iarray(1)
        if (matrix(imax,i) == 0.) then
          sing = .true.
          return
        end if
        factor = 1.0 / matrix(imax,i)
        if (imax > i) temp(i:) = matrix(i, i:)
        matrix(i, i1:) = factor * matrix(imax, i1:)
        if (imax > i) matrix(imax, i:) = temp(i:)
        do j = 1, nrows
          if (j == i) cycle
          factor = -matrix(j,i)
          matrix(j, i1:) = matrix(j, i1:) + factor*matrix(i, i1:)
        end do
      end do
      return
!
      end subroutine gausselim_real

!!-------------------------- gausselim_double -----------------------------!!
!!-------------------------- gausselim_double -----------------------------!!
!!-------------------------- gausselim_double -----------------------------!!

      subroutine gausselim_double (nrows, ncols, matrix, sing)
!
      implicit none
!
      integer          , intent (in)    :: nrows, ncols
      double precision , intent (inout) :: matrix (nrows, ncols)
      logical          , intent (out)   :: sing
!
      double precision :: temp (ncols)
      double precision :: factor
      integer          :: i, i1, iarray(1), imax, j
!
      sing = .false.
      do i = 1, nrows
        i1 = i + 1
        iarray = maxloc ( abs(matrix(i: ,i)) )
        imax = i - 1 + iarray(1)
        if (matrix(imax,i) == 0.) then
          sing = .true.
          return
        end if
        factor = 1.0 / matrix(imax,i)
        if (imax > i) temp(i:) = matrix(i, i:)
        matrix(i, i1:) = factor * matrix(imax, i1:)
        if (imax > i) matrix(imax, i:) = temp(i:)
        do j = 1, nrows
          if (j == i) cycle
          factor = -matrix(j,i)
          matrix(j, i1:) = matrix(j, i1:) + factor*matrix(i, i1:)
        end do
      end do
      return
!
      end subroutine gausselim_double

!!------------------------- gausselim_complex -----------------------------!!
!!------------------------- gausselim_complex -----------------------------!!
!!------------------------- gausselim_complex -----------------------------!!

      subroutine gausselim_complex (nrows, ncols, matrix, sing)
!
      implicit none
!
      integer , intent (in)    :: nrows, ncols
      complex , intent (inout) :: matrix (nrows, ncols)
      logical , intent (out)   :: sing
!
      complex :: temp (ncols)
      complex :: factor
      integer :: i, i1, iarray(1), imax, j
!
      sing = .false.
      do i = 1, nrows
        i1 = i + 1
        iarray = maxloc ( abs(matrix(i: ,i)) )
        imax = i - 1 + iarray(1)
        if (matrix(imax,i) == (0.,0.)) then
          sing = .true.
          return
        end if
        factor = (1.,0.) / matrix(imax,i)
        if (imax > i) temp(i:) = matrix(i, i:)
        matrix(i, i1:) = factor * matrix(imax, i1:)
        if (imax > i) matrix(imax, i:) = temp(i:)
        do j = 1, nrows
          if (j == i) cycle
          factor = -matrix(j,i)
          matrix(j, i1:) = matrix(j, i1:) + factor*matrix(i, i1:)
        end do
      end do
      return
!
      end subroutine gausselim_complex

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module gausselim_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
