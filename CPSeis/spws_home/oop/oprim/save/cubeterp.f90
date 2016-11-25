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
! Name       : CUBETERP       (CUBIC INTERPOLATION)
! Category   : math
! Written    : 2002-10-24   by: Tom Stoeckley
! Revised    : 2004-05-03   by: Tom Stoeckley
! Maturity   : production
! Purpose    : Low level 4-point cubic interpolation.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION
!
! This primitive is used to obtain a single interpolated value from an
! array, or from four adjacent equally-spaced or unequally-spaced values.
!
! Two algorithms are available for interpolating between input samples.
! These are 2-point linear interpolation and 4-point cubic interpolation.
!
! The 4-point cubic interpolation algorithm used in this primitive uses
! two points on each side of the specified location to calculate the returned
! value.  Weights are assigned to these four points in such a way as to
! guarantee that the interpolated values and their first derivatives will
! be continuous over the entire length of the arrays.
!
! Points off the ends of the input array are assumed to be zero.
!
!-------------------------------------------------------------------------------
!                 LIST OF CUBIC INTERPOLATION PRIMITIVES
!
! The 4-point cubic interpolation algorithm is used in the following
! primitives (and possibly others as well):
!
!     STATCC:   Performs a static trace shift.
!
!     DYNCC:    Performs a dynamic trace shift using unequally spaced
!               (input,output) index pairs.
!
!     DYNSAMP:  Performs a dynamic trace shift using an array of input
!               or output indices at the trace sample interval.  This
!               primitive is a class which must be instantiated.  This
!               primitive uses the CUBETERP primitive.
!
!     ZIPPITY:  Calculates an interpolated value from a 1D or 2D or 3D
!               grid of unequally spaced data.
!
!     CUBETERP: Low level primitive which returns an interpolated value
!               from equally or unequally spaced 1D data using linear or
!               cubic interpolation.
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
! To use an exact array index to obtain a single interpolated value from
! an array:
!
!                 o                       i  i    i
!               answer = CUBETERP_CUBIC  (A, n, exact)
!               answer = CUBETERP_LINEAR (A, n, exact)
!
! integer  N      = length of array A.
! real     A(N)   = input array (not changed by this routine).
! real     exact  = exact index whose interpolated value is requested.
! real     answer = interpolated value.
!
! EXACT will normally have a fractional component.
! Flat extrapolation occurs if EXACT is less than 1 or greater than N.
!
! These functions return a value which is interpolated from the input array
! A(n) at exact index A(exact).
!
! CUBETERP_CUBIC uses 4-point cubic interpolation.
! CUBETERP_LINEAR uses 2-point linear interpolation.
!
! CUBETERP_CUBIC calls CUBETERP_EVEN.
!
!-------------------------------------------------------------------------------
! To get a value interpolated from four input values:
!
!            o                         i       i  i      i  i  i  i
!          answer = CUBETERP_EVEN   (exact,    i2,i3,    a1,a2,a3,a4)
!          answer = CUBETERP_UNEVEN (xwant, x1,x2,x3,x4, a1,a2,a3,a4)
!            o                         i    i  i  i  i   i  i  i  i
!
! real           exact = exact  index   whose interpolated value is requested.
! real           xwant = exact location whose interpolated value is requested.
! integer  i1,i2,i3,i4 = four consecutive  indices  of input values.
! real     x1,x2,x3,x4 = four consecutive locations of input values.
! real     a1,a2,a3,a4 = input values at locations x1,x2,x3,x4 or i1,i2,i3,i4.
! real          answer = output value interpolated from four input values.
!
! EXACT will normally have a fractional component.
! EXACT should be between I2 and I3 in an evenly sampled input dataset.
! XWANT should be between X2 and X3 in an unevenly sampled input dataset.
!
! These functions return a value which is interpolated from four input
! values by 4-point cubic interpolation.
!
! CUBETERP_EVEN uses evenly-spaced input values at indices I1,I2,I3,I4.
! These indices must be consecutive and increasing, but are not required
! to be positive.
!
! CUBETERP_UNEVEN uses unevenly-spaced input values at locations X1,X2,X3,X4.
! These locations must satisfy the inequality X1 < X2 < X3 < X4.
!
! CUBETERP_EVEN is called by CUBETERP_CUBIC.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  1. 2004-05-03  Stoeckley    Initial version.
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


      module cubeterp_module
      implicit none
      public

      character(len=100),public :: cubeterp_ident = &
        "$Id: cubeterp.f90,v 1.1 2004/05/03 11:29:25 Stoeckley prod sps $"

      contains


!!--------------------------- cubeterp linear -------------------------------!!
!!--------------------------- cubeterp linear -------------------------------!!
!!--------------------------- cubeterp linear -------------------------------!!


      function CUBETERP_linear (a, n, exact) result (answer)
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: N                       ! argument
      REAL   ,INTENT(IN) :: a(N)                    ! argument
      REAL   ,INTENT(IN) :: exact                   ! argument
      REAL               :: answer                  ! result      ! a(exact)
      INTEGER            :: i2,i3                   ! local
      real               :: w2,w3                   ! local
      real               :: a2,a3                   ! local

      i2 = exact               !!  i2 <= exact <= i3
      i3 = i2 + 1              !!  i2 <= exact <= i3

      w2 = i3 - exact
      w3 = exact - i2

      if (i2 >= 1 .and. i3 <= n) then
           a2 = a(i2)
           a3 = a(i3)
      else
           a2 = 0.0
           a3 = 0.0
           if (i2 >=1 .and. i2 <= n) a2 = a(i2)
           if (i3 >=1 .and. i3 <= n) a3 = a(i3)
      end if

      answer = w2*a2 + w3*a3
      RETURN
      END function CUBETERP_linear


!!----------------------------- cubeterp cubic -------------------------------!!
!!----------------------------- cubeterp cubic -------------------------------!!
!!----------------------------- cubeterp cubic -------------------------------!!


      function CUBETERP_cubic (a, n, exact) result (answer)
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: N                       ! argument
      REAL   ,INTENT(IN) :: a(N)                    ! argument
      REAL   ,INTENT(IN) :: exact                   ! argument
      REAL               :: answer                  ! result      ! a(exact)
      INTEGER            :: i1,i2,i3,i4             ! local
      real               :: a1,a2,a3,a4             ! local

      i2 = exact            !!  i1 < i2 <= exact <= i3 < i4
      i3 = i2 + 1           !!  i1 < i2 <= exact <= i3 < i4
      i4 = i2 + 2           !!  i1 < i2 <= exact <= i3 < i4
      i1 = i2 - 1           !!  i1 < i2 <= exact <= i3 < i4

      if (i1 >= 1 .and. i4 <= n) then
           a1 = a(i1)
           a2 = a(i2)
           a3 = a(i3)
           a4 = a(i4)
      else
           a1 = 0.0
           a2 = 0.0
           a3 = 0.0
           a4 = 0.0
           if (i1 >=1 .and. i1 <= n) a1 = a(i1)
           if (i2 >=1 .and. i2 <= n) a2 = a(i2)
           if (i3 >=1 .and. i3 <= n) a3 = a(i3)
           if (i4 >=1 .and. i4 <= n) a4 = a(i4)
      end if

      answer = cubeterp_even (exact, i2,i3, a1,a2,a3,a4)
      RETURN
      END function CUBETERP_cubic


!!------------------------ cubeterp even ----------------------------------!!
!!------------------------ cubeterp even ----------------------------------!!
!!------------------------ cubeterp even ----------------------------------!!


      function cubeterp_even (exact, i2,i3, a1,a2,a3,a4) result (answer)
      implicit none
      real   ,intent(in)  :: exact, a1,a2,a3,a4                 ! arguments
      integer,intent(in)  :: i2,i3                              ! arguments
      real                :: answer                             ! result
      real                :: e2,e3,ee2,ee3                      ! local
      real                :: w1,w2,w3,w4                        ! local

      e2 = exact - i2                      ! positive
      e3 = i3 - exact                      ! positive

      ee2 = e2 * e2
      ee3 = e3 * e3

      w1 = - ee3 * e2                      ! negative
      w4 = - ee2 * e3                      ! negative
      w2 =   e3 * (1.0 + e3) - w1          ! positive
      w3 =   e2 * (1.0 + e2) - w4          ! positive

      answer = (w1*a1 + w2*a2 + w3*a3 + w4*a4) / (ee3 + 1.0 + ee2)
      return
      end function cubeterp_even


!!------------------------ cubeterp uneven --------------------------------!!
!!------------------------ cubeterp uneven --------------------------------!!
!!------------------------ cubeterp uneven --------------------------------!!


      function cubeterp_uneven  &
                        (xwant, x1,x2,x3,x4, a1,a2,a3,a4) result (answer)
      implicit none
      real,intent(in)     :: xwant, x1,x2,x3,x4, a1,a2,a3,a4     ! arguments
      real                :: answer                              ! result
      real                :: d21,d32,d43                         ! local
      real                :: e2,e3,ee2,ee3                       ! local
      real                :: w1,w2,w3,w4                         ! local

      d21 = x2 - x1                        ! positive
      d32 = x3 - x2                        ! positive
      d43 = x4 - x3                        ! positive

      e2 = xwant - x2                      ! positive
      e3 = x3 - xwant                      ! positive

      ee2 = e2 * e2         
      ee3 = e3 * e3

      w1 = - ee3 * e2 / d21                ! negative
      w4 = - ee2 * e3 / d43                ! negative
      w2 =   e3 * (d32 + e3) - w1          ! positive
      w3 =   e2 * (d32 + e2) - w4          ! positive

      answer = (w1*a1 + w2*a2 + w3*a3 + w4*a4) / (ee3 + d32 * d32 + ee2)
      return
      end function cubeterp_uneven


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module cubeterp_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
