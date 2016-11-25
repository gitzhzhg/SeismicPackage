!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- polyfit.f90 --------------------------------!!
!!---------------------------- polyfit.f90 --------------------------------!!
!!---------------------------- polyfit.f90 --------------------------------!!
 

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
! Name       : POLYFIT (Polynomial Fitting)
! Category   : math
! Written    : 1990-01-30   by: Baumel
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Convenient least-squares polynomial fitting and associated
!              utilities.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This module performs least-squares polynomial fitting.
!
! It also has convenience functions for evaluating y from x given a set of
! coefficients (calculated here or elsewhere).
!
! Essentially it provides a convenience layer that builds the N x N matrix and
! "column vector" that define the "normal equations" needed to solve the
! least-squares problem.  Calling programs are thereby relieved of the burden of
! building the required sums and matrices--they can just focus on constructing
! the input x and y data arrays.
!
!
! For example, the normal equations for the linear case (norder=1) are:
!
!   sum(  y) = a*N      + b*sum(x)
!   sum(x*y) = a*sum(x) + b*sum(x^2)
!
! The normal equations for the quadratic case (norder=2) are:
!
!   sum(    y) = a*N        + b*sum(x)   + c*sum(x^2)
!   sum(x  *y) = a*sum(x)   + b*sum(x^2) + c*sum(x^3)
!   sum(x^2*y) = a*sum(x^2) + b*sum(x^3) + c*sum(x^4)
!
!
! An underlying Gaussian Elimination primitive module does the matrix math.
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
!                                 i     i    i  i  o
!       call polyfit_evaluate (norder, coef, n, x, y)
!
! integer               norder    =    polynomial degree
! real or double          coef    =    input polynomial coefficients
! integer                    n    =    number of data points
! real or double             x    =    input array
! real or double             y    =    output array
!
!
!                          i  i  i    i      o
!       call polyfit_calc (n, x, y, norder, coef)
!
! integer                    n    =    number of data points
! real or double             x    =    independent array
! real or double             y    =    dependent array
! integer               norder    =    desired polynomial degree
! real or double          coef    =    resulting regression coefficients  ***
!
! *** all coef elements are set to FNIL in the event of failure to solve
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
! No provisions are made for "null" input values--they need to be stripped out
! by the calling program (excluded from the input x and y arrays).
!
! Note that, in the event of failure to solve the matrix, coef values are all
! set to FNIL.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  6. 2006-06-20  B. Menger    Removed Unused Variables.
!  5. 2001-01-09  Cook         Added routines to evaluate a polynomial given
!                              its order, the coefficients, and an input x val.
!  4. 2000-08-23  Cook         Internal calculations now are double precision
!                              even for the REAL input case.  Also, failure to
!                              calculate matrix solution results in all coefs
!                              being set to FNIL.
!  3. 2000-08-21  Cook         Converted from old system.  Bares little
!                              resemblance to previous VAX version because,
!                              among other things, the matrix elements are
!                              grouped differently to match new underlying
!                              primitive "gausselim" rather than old "LINEQS"
!                              (matrix and column vectors now are separated).
!  2. 1999-01-11  Goodger      Begin using the fortran90 compiler.
!  1. 1990-01-30  Bob Baumel   Initial version.
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
! 1.  Calculate terms for a "column vector" and N x N matrix, where N is
!     the degree of the polynomial + 1.
!
! 2.  Submit to linear equations solution routine (gausselim in this case).
!
! 3.  Return the resulting coeffients (first element is coefficient of
!     zero-order term, second element is first-order term, etc.)
!
!-------------------------------------------------------------------------------
!</algorithm_doc>


!<programming_doc>
!-------------------------------------------------------------------------------
!                           PROGRAMMING NOTES             
!
! REAL version uses double precision internally.
!
!-------------------------------------------------------------------------------
!</programming_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

      module polyfit_module
      use gausselim_module
      use named_constants_module
      implicit none

      private
      public :: polyfit_evaluate
      public :: polyfit_calc

      character(len=100),public,save :: POLYFIT_IDENT = &
       '$Id: polyfit.f90,v 1.6 2006/06/20 13:12:03 Menger prod sps $'

!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
!!---------------------------- interfaces ---------------------------------!!
      interface polyfit_evaluate
        module procedure polyfit_evaluate_r
        module procedure polyfit_evaluate_d
      end interface

      interface polyfit_calc
        module procedure polyfit_calc_r
        module procedure polyfit_calc_d
      end interface


      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

!-------------------------------------------------------------------------------
! Subroutine to evaluate polynomial (REAL version).
!-------------------------------------------------------------------------------
      subroutine polyfit_evaluate_r(nord, coeff, n, x, y)
      implicit none
      integer, intent(in)    :: nord
      real, intent(in)       :: coeff(:)
      integer, intent(in)    :: n
      real, intent(in)       :: x(:)
      real, intent(out)      :: y(:)

      integer                ::  i,j
      double precision       ::  xterm,yterm

      do i=1,n
        xterm = x(i)
        yterm = coeff(nord+1)

        do j=nord,1,-1
          yterm = yterm*xterm + coeff(j)
        end do

        y(i) = yterm
      end do

      end subroutine polyfit_evaluate_r


!-------------------------------------------------------------------------------
! Subroutine to evaluate polynomial (DOUBLE PRECISION version).
!-------------------------------------------------------------------------------
      subroutine polyfit_evaluate_d(nord, coeff, n, x, y)
      implicit none
      integer, intent(in)             :: nord
      double precision, intent(in)    :: coeff(:)
      integer, intent(in)             :: n
      double precision, intent(in)    :: x(:)
      double precision, intent(out)   :: y(:)

      integer                ::  i,j
      double precision       ::  xterm,yterm

      do i=1,n
        xterm = x(i)
        yterm = coeff(nord+1)

        do j=nord,1,-1
          yterm = yterm*xterm + coeff(j)
        end do

        y(i) = yterm
      end do

      end subroutine polyfit_evaluate_d


!-------------------------------------------------------------------------------
! REAL version.
!-------------------------------------------------------------------------------
      subroutine polyfit_calc_r (n,x,y,norder,coef)
      integer, intent(in)    :: n, norder
      real, intent(in)       :: x(:), y(:)
      real, intent(out)      :: coef(:)

      logical            :: is_singular
      integer            :: i,j,k

      double precision   :: value
      double precision   :: work(n)
      double precision   :: dcoef(norder+1)
      double precision   :: column_vect(norder+1)
      double precision   :: matrix(norder+1,norder+1)


!---- Load the Column Vector:
      work(:n) = y(:n)
      column_vect(1) = sum(work)

      do k=1,norder
        work(:n) = work(:n)*x(:n)
        column_vect(k+1) = sum(work)
      end do

!---- Load the Square Matrix:
      work = 1.
      matrix(1,1) = real(n)

      do k=1,2*norder

        work(:n) = work(:n)*x(:n)

        value = sum(work)

        do i=1, norder + 1
          do j=1, norder + 1
            if( (i + j) == (k + 2) ) matrix(i,j) = value
          end do
        end do

      end do

!---- print matrix for debugging
!      write(*,*) 'begin matrix:'
!      do i=1,norder+1
!        do j=1,norder+1
!        write(6,*) matrix(i,j)
!        end do
!      write(*,*) ''
!      end do

!---- Solve Equations & Return Solution:
      call gausselim_lineqs(norder+1,matrix,dcoef,column_vect,is_singular)

      if(is_singular) then
        coef = FNIL
      else
        coef(:norder + 1) = dcoef(:norder + 1)
      end if

      end subroutine polyfit_calc_r


!-------------------------------------------------------------------------------
! DOUBLE PRECISION version.
!-------------------------------------------------------------------------------
      subroutine polyfit_calc_d (n,x,y,norder,coef)
      integer, intent(in)           :: n, norder
      double precision, intent(in)  :: x(:), y(:)
      double precision, intent(out) :: coef(:)

      logical            :: is_singular
      integer            :: i,j,k

      double precision   :: value
      double precision   :: work(n)
      double precision   :: column_vect(norder+1)
      double precision   :: matrix(norder+1,norder+1)


!---- Load the Column Vector:
      work(:n) = y(:n)
      column_vect(1) = sum(work)

      do k=1,norder
        work(:n) = work(:n)*x(:n)
        column_vect(k+1) = sum(work)
      end do

!---- Load the Square Matrix:
      work = 1.
      matrix(1,1) = real(n)

      do k=1,2*norder

        work(:n) = work(:n)*x(:n)

        value = sum(work)

        do i=1, norder + 1
          do j=1, norder + 1
            if( (i + j) == (k + 2) ) matrix(i,j) = value
          end do
        end do

      end do

!---- print matrix for debugging
!      write(*,*) 'begin matrix:'
!      do i=1,norder+1
!        do j=1,norder+1
!        write(6,*) matrix(i,j)
!        end do
!      write(*,*) ''
!      end do

!---- Solve Equations & Return Solution:
      call gausselim_lineqs(norder+1,matrix,coef,column_vect,is_singular)

      if(is_singular) coef = FNIL

      end subroutine polyfit_calc_d


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module polyfit_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
