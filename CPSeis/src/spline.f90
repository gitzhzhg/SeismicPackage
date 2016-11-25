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
! Name       : spline (cubic SPLINE interpolation)
! Category   : math
! Written    : 1999-01-11   by: Bob Baumel
! Revised    : 2006-06-12   by: B. Menger
! Maturity   : production
! Purpose    : Perform cubic spline interpolation to a set of data points.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!            This routine adapted from code written by Clark R. Wilson
!            of the SEER project at University of Texas, Austin.
!
! subroutines:
!    spline:  Compute coefficients.
!    spline_v: Evaluate function.
!    spline_d: Compute coef when dX/dY has discontinuities.
!                                                  has discontinuities
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
!                  i  i  i  o  o  o
!    CALL SPLINE  (N, X, Y, B, C, D)
!
!                   i  i  i  i  i  i   i      i      o
!    CALL spline_v (N, X, Y, B, C, D, XVAL, IDERIV, YVAL)
!
!                   i  i  i   i   o  o  o
!    CALL spline_d (N, X, Y, IDS, B, C, D)
!
! Subroutine SPLINE parameters:
! integer                    N       = Number of data points.
! real                       X,Y     = Arrays of length N containing abscissas
!                                      and ordinates of the data points.
!                                      Array X must be strictly monotone
!                                      increasing or decreasing.
! real                       B,C,D   = Arrays of length N containing computed
!                                      spline coefficients.  If XVAL lies
!                                      between X(i) and X(i+1), then F(XVAL) =
!                                      Y(i) + B(i)*H + C(i)*H**2 + D(i)*H**3
!                                      where    H = XVAL - X(i).
!                                      Note: C(1) = C(N) = 0
!                                      Note: B(N) and D(N) are not set.
!
! Subroutine spline_v parameters:
! integer                    N       = Number of data points.
! real                       X,Y     = Arrays of length N containing abscissas
!                                      and ordinates of the data points.
!                                      Array X must be strictly monotone
!                                      increasing or decreasing.
! real                       B,C,D   = Arrays of length N containing spline
!                                      coefficients previously computed by
!                                      entry SPLINE.
! real                       XVAL    = Value of X for which interpolation is
!                                      desired.
! integer                    IDERIV  = 0 to compute function, or 1 to compute
!                                      derivative.
! real                       YVAL    = Computed value of function or derivative
!
!  Entry spline_d parameters:
! integer                    N       = Number of data points.
! real                       X,Y     = Arrays of length N containing abscissas
!                                      and ordinates of the data points.
!                                      Array X need not be strictly monotone
!                                      increasing or decreasing but can reverse
!                                      or have discontinuities in its gradient.
! integer                    IDS     = Integer array of length N containing a
!                                      flag to indicate whether the derivitive
!                                      is continuous (IDS >=0) or
!                                      discontinuous (IDS<0).
! real                       B,C,D   = Arrays of length N containing computed
!                                      spline coefficients.  If XVAL lies
!                                      between X(i) and X(i+1), then F(XVAL) =
!                                      Y(i) + B(i)*H + C(i)*H**2 + D(i)*H**3
!                                      where    H = XVAL - X(i).
!                                      Note:  C(1) = C(N) = 0
!                                      Note: B(N) and D(N) are not set.
!                                      Note: if X(i) = X(i+1),
!                                         B(i) = C(i) = D(i) = 0.
!                                         has discontinuities
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
!  9. 2006-06-12  B. Menger   Removed Unused Variables.
!  8. 2000-01-25  Selzler     Cleaned up trailing blanks and block labels
!  7. 1999-11-19  Selzler     Added RCS "Id" strings to tag executeable
!  6. 1999-10-04  Selzler     Conversion to f90
!  5. 1999-01-11  Goodger     Begin using the fortran90 compiler.
!  4. 1993-08-23  J. Reed     Added check for zeros in loop 1000
!  3. 1989-03-17  D.Hanson    Add check for divide by zero if X(i) = X(i+1)
!                             in DO loop 1030
!  2. 1989-03-16  D.Hanson    Add SPLINED subroutine.
!  1. 1987-03-27  B.Baumel    Original version.
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

!-------------------------------------------------------------------------------
!<compile_doc>
!
!</compile_doc>
!-------------------------------------------------------------------------------

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
!  Calculation of the coefficients B,C,D is highly recursive, so
!    much of this code does not vectorize; thus it should preferably
!    be used for functions sampled at only a few points.
!  If anybody wants to make the code more efficient, they are welcome.
!
!-------------------------------------------------------------------------------
!</programming_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!

      module spline_module
      implicit none

      private
      public :: spline
      public :: spline_v
      public :: spline_d

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      character(len=100),public :: spline_ident = &
        "$Id: spline.f90,v 1.9 2006/06/12 13:03:56 Menger prod sps $"

      contains

!!----------------------------- spline -------------------------------!!
!!----------------------------- spline -------------------------------!!
!!----------------------------- spline -------------------------------!!

      SUBROUTINE SPLINE(N, X, Y, B, C, D)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: N     ! argument
      REAL, INTENT(IN)    :: X(*)  ! argument
      REAL, INTENT(IN)    :: Y(*)  ! argument
      REAL, INTENT(OUT)   :: B(*)  ! argument
      REAL, INTENT(OUT)   :: C(*)  ! argument
      REAL, INTENT(OUT)   :: D(*)  ! argument

      INTEGER :: N1, N2, M1, M2, K, L   
      REAL :: S, R   
      REAL, ALLOCATABLE :: R1U(:), R1V(:)

      N1 = 1
      N2 = N
      M1 = N1 + 1
      M2 = N2 - 1
      S = 0.

      ALLOCATE (R1U(M2-N1+1), R1V(M2-N1+1))

      D(N1:M2) = X(N1+1:M2+1) - X(N1:M2)

      WHERE (D(N1:M2) == 0.0)
        D(N1:M2) = 0.000001
      END WHERE

      R1U = (Y(N1+1:M2+1)-Y(N1:M2))/D(N1:M2)

      DO K = 1, M2 - N1 + 1
        R1V(K) = S
        S = R1U(K)
      END DO

      C(N1:M2) = R1U - R1V

      DEALLOCATE (R1U, R1V)

      C(N2) = 0.
      C(N1) = 0.
      S = 0.
      R = 0.

      DO K = M1, M2
        C(K) = C(K) + R*C(K-1)
        B(K) = 2.*(X(K-1)-X(K+1)) - R*S
        S = D(K)
        R = S/B(K)
      END DO

      DO L = M2, M1, -1
        C(L) = (D(L)*C(L+1)-C(L))/B(L)
      END DO

      WHERE (D(N1:M2) == 0)
        B(N1:M2) = 0
        D(N1:M2) = 0
      ELSEWHERE
        B(N1:M2) = (Y(N1+1:M2+1)-Y(N1:M2))/D(N1:M2) - (C(N1:M2)+C(N1:M2)+C(N1+1&
          :M2+1))*D(N1:M2)
        D(N1:M2) = (C(N1+1:M2+1)-C(N1:M2))/D(N1:M2)
      END WHERE

      C(N1:M2) = 3.*C(N1:M2)

      RETURN
      END SUBROUTINE SPLINE

!!----------------------------- spline_v -------------------------------!!
!!----------------------------- spline_v -------------------------------!!
!!----------------------------- spline_v -------------------------------!!

      SUBROUTINE spline_v (N, X, Y, B, C, D, XVAL, IDERIV, YVAL)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: N       ! argument
      REAL, INTENT(IN)    :: X(*)    ! argument
      REAL, INTENT(IN)    :: Y(*)    ! argument
      REAL, INTENT(IN)    :: B(*)    ! argument
      REAL, INTENT(IN)    :: C(*)    ! argument
      REAL, INTENT(IN)    :: D(*)    ! argument
      REAL, INTENT(IN)    :: XVAL    ! argument
      INTEGER, INTENT(IN) :: IDERIV  ! argument
      REAL, INTENT(OUT)   :: YVAL    ! argument

      integer i  ! local
      real h     ! local

      IF (IDERIV<0 .OR. IDERIV>1) THEN
        YVAL = 0.
        RETURN
      ENDIF

      IF (X(N) >= X(1)) THEN
        DO I = N - 1, 2, -1
          IF (XVAL >= X(I)) GO TO 2020
        END DO

        I = 1
      ELSE
        DO I = N - 1, 2, -1
          IF (XVAL <= X(I)) GO TO 2020
        END DO

        I = 1
      ENDIF

 2020 CONTINUE
      H = XVAL - X(I)

      IF (IDERIV == 0) THEN
        YVAL = ((D(I)*H+C(I))*H+B(I))*H + Y(I)
      ELSE
        YVAL = (3.*D(I)*H+2.*C(I))*H + B(I)
      ENDIF

      RETURN
      END SUBROUTINE spline_v

!!----------------------------- spline_d -------------------------------!!
!!----------------------------- spline_d -------------------------------!!
!!----------------------------- spline_d -------------------------------!!

      SUBROUTINE spline_d(NX, X, Y, IDS, B, C, D)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: NX       ! argument
      REAL, INTENT(IN)    :: X(*)     ! argument
      REAL, INTENT(IN)    :: Y(*)     ! argument
      INTEGER, INTENT(IN) :: IDS(*)   ! argument
      REAL, INTENT(OUT)   :: B(*)     ! argument
      REAL, INTENT(OUT)   :: C(*)     ! argument
      REAL, INTENT(OUT)   :: D(*)     ! argument

      INTEGER :: IX1, IX, IX2, NX1
      REAL :: XSIGN, XNEXT

      ! INITIALIZE POINTER FOR THE PREVIOUS DISCONTINUITY TO THE FIRST POINT
      ! INITIALIZE COUNTER FOR NUMBER OF SPLINE FIT POINTS
      IX1 = 1

      ! CYCLE OVER POINTS IN A BOUNDARY
      boundary_loop: do
        XSIGN = SIGN(1.,X(IX1+1)-X(IX1))

        DO IX = IX1 + 1, NX
          IX2 = IX

          IF (IX < NX) THEN
            XNEXT = X(IX+1)
          ELSE
            XNEXT = X(IX) + XSIGN
          ENDIF

          IF (.NOT.(IDS(IX) < 0 .OR. (XNEXT-X(IX)) * XSIGN <= 0 .OR. &
            (X(IX) - X(IX-1)) * XSIGN <= 0)) CYCLE

          EXIT
        END DO

        ! IF THIS IS A DISCONTINUITY OR THE LAST POINT
        ! COMPUTE THE SPLINE COEFFICIENTS BETWEEN
        ! THE PREVIOUS DISCONTINUITY OR THE FIRST POINT
        NX1 = IX2 - IX1 + 1

        IF (X(IX1) == X(IX1+1)) THEN
          ! FOR A VERTICAL SEGMENT SET ALL COEFFICIENTS TO 0
          B(IX1:IX2) = 0
          C(IX1:IX2) = 0
          D(IX1:IX2) = 0
        ELSE
          ! COMPUTE SPLINE COEFFICIENTS
          CALL SPLINE (NX1, X(IX1), Y(IX1), B(IX1), C(IX1), D(IX1))
        ENDIF

        IX1 = IX2

        if(ix1 >= nx) exit boundary_loop
      end do boundary_loop

      RETURN
      END SUBROUTINE spline_d

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module spline_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
