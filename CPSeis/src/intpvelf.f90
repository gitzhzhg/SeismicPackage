!<CPS_v1 type="PRIMITIVE"/>
!!----------------------------- intpvelf.f90 ------------------------------!!
!!----------------------------- intpvelf.f90 ------------------------------!!
!!----------------------------- intpvelf.f90 ------------------------------!!
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
! Name       : INTPVELF  (INTerPolate rms VELocity Function)
! Category   : math
! Written    : 1987-05-03   by: Bob Baumel
! Revised    : 2001-11-15   by: Bob Baumel
! Maturity   : production   2001-12-10
! Purpose    : Resample an RMS velocity function to a finer (uniform)
!              time grid with various options including spline interpolation
!              and conversion to interval velocity.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! 1. This routine assumes that you have already fetched an RMS velocity
!    function using velio. Thus, the velocity function is passed to the
!    present subroutine in the TIM and VEL arguments.
!
! 2. Spline interpolation is used in the cases IFLAG=0,1,2. The IFLAG=1
!    and 2 options produce SMOOTH interval velocity models (except for
!    a discontinuity at base of an assumed "water" layer--see Note 6).
!
! 3. All options except IFLAG=0 are really designed for computation of
!    INTERVAL velocity, although a variety of different interpolation
!    methods are used, and different quantities are output. For IFLAG=0,
!    output RMS velocity is constant past the end of the input velocity
!    function; however, for IFLAG=1,2,3,4, computed INTERVAL velocity
!    (not RMS velocity) will be constant past the end of the input
!    function. Also, for IFLAG=1,2,3,4, the routine executes an error
!    return if it detects that a computed interval velocity will be
!    imaginary.
!
! 4. When IFLAG=1 (smoothed) interval velocity is output directly. For
!    IFLAG=2,3,4, other quantities are output, requiring you to complete
!    the computation of interval velocity in your own code. Interval
!    velocities may be computed by the (continuous) version of the Dix
!    Formula, i.e., Interval velocity is equal to the square root of
!    the time derivative of the quantity  TIME * VRMS**2.
!
! 5. The IFLAG=1 and 2 options both perform spline interpolation of the
!    quantity  TIM * VEL**2, allowing computation of optimally smooth
!    interval velocities.  IFLAG=3 performs LINEAR interpolation of
!    TIM * VEL**2, leading to a BLOCKY interval velocity, piecewise-
!    constant in between your input (TIM,VEL) pairs.  IFLAG=4 does a
!    LINEAR interpolation of your input RMS velocity, matching the
!    procedure of most standard seismic processing. This leads to an
!    interval velocity that grades slightly in between the input
!    (TIM,VEL) pairs (and does have jumps at those input points).
!
! 6. The spline interpolation options (IFLAG=0,1,2) assume an initial
!    constant velocity layer ("water" layer) if several of the initial
!    VEL values are identical, or if TIM(1) is greater than zero. The
!    spline interpolation is performed only BELOW this assumed water
!    layer, resulting in discontinuous interval velocity at the base of
!    this assumed water layer.
!
! 7. For IFLAG=1, 2, and 3, the input VEL array gets overwritten.
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
!
!                  i     i    b     o     i      i      i      i      o      o
! call intpvelf (NVELP, TIM, VEL, WORK, IFLAG, TONEW, DTNEW, NTNEW, VELNEW, IER)
!
! integer                    NVELP   = Number of (Time,Velocity) pairs in
!                                      given vel function (TIM and VEL).
!                                      NVELP > 0 required.
! real                       TIM     = Times from given function (dimension
!                                      NVELP).
! real                       VEL     = Vels from given function (dimension
!                                      NVELP), assumed to be RMS velocity.
!                                      Array overwritten when IFLAG=1,2,3.
! real                       WORK    = Work space (dimension 3*NVELP) used when
!                                      IFLAG=0,1,2  (not used when IFLAG=3,4).
! integer                    IFLAG   = Flag for interpolation type & output:
!                                      0= Spline interpolation, RMS velocities
!                                         output.
!                                      1= Spline interpolation, SMOOTHED
!                                         INTERVAL velocities output.
!                                      2= Spline interpolation,  T * V(RMS)**2
!                                         output.
!                                      3= LINEAR interpolation,  T * V(RMS)**2
!                                         output.
!                                      4= LINEAR interpolation,  V(RMS) output.
! real                       TONEW   = Starting time for interpolated output.
! real                       DTNEW   = Time increment for interpolated output.
!                                      DTNEW > 0.0 required.
! integer                    NTNEW   = Number of points in new (regular) time
!                                      grid.  NTNEW > 0 required.
! real                       VELNEW  = Interpolated output array (dimension
!                                      NTNEW).
! integer                    IER     = Error flag (0 implies no error)
!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author      Description
!     ----        ------      -----------
! 11. 2001-12-10  B Baumel    Change WORK array dimension from (*) to (:) and
!                             modify calls to SPLINE primitive so should keep
!                             working when change (*) to (:) in that primitive.
! 10. 2000-06-13  B Baumel    Modify IFLAG=4 option to avoid jump in interval
!                             velocity at deepest time in input function.
!  9. 2000-01-25  Selzler     Clean up trailing blanks and block labels
!  8. 1999-11-19  Selzler     Added RCS "Id" strings to tag executeable
!  7. 1999-10-04  Selzler     Conversion to f90
!  6. 1999-01-11  Goodger     Begin using the fortran90 compiler.
!  5. 1995-09-18  B Baumel    Minor bug fix for IFLAG=4 if velocity=const.
!  4. 1994-02-25  B Baumel    Add IFLAG=4 option.
!  3. 1989-08-14  B Baumel    Add IFLAG=3 option.
!  2. 1989-08-11  B Baumel    Add IFLAG=2 option.
!  1. 1987-05-03  B Baumel    Original version.
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

      module intpvelf_module
      use pc_module
      use spline_module
      implicit none

      private
      public :: intpvelf

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      character(len=100),public :: intpvelf_ident = &
"$Id: intpvelf.f90,v 1.11 2001/12/06 19:34:54 Baumel prod sps $"

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

      SUBROUTINE INTPVELF(NVELP, TIM, VEL, WORK, IFLAG, T0NEW, DTNEW, NTNEW, &
        VELNEW, IER)
      IMPLICIT NONE

      INTEGER, INTENT(IN)  :: NVELP          ! argument
      REAL, intent(in)     :: TIM(NVELP)     ! argument
      REAL, intent(inout)  :: VEL(NVELP)     ! argument
      REAL, intent(out)    :: WORK(:)        ! argument
      INTEGER, INTENT(IN)  :: IFLAG          ! argument
      REAL, INTENT(IN)     :: T0NEW          ! argument
      REAL, INTENT(IN)     :: DTNEW          ! argument
      INTEGER, INTENT(IN)  :: NTNEW          ! argument
      REAL, intent(out)    :: VELNEW(NTNEW)  ! argument
      INTEGER, intent(out) :: IER            ! argument

      INTEGER :: J1, I, NSTART, NSPLINE, IPOINT1, IPOINT1E, IPOINT2, &
                 IPOINT2E, NTEST, J2
      REAL :: VSTART, TSTART, TSTOP, VSTOP, TIMNEW, TV2STOP, V2STOP, &
              TV2LAST, TV2

      ier = 0
      VSTART = VEL(1)

      DO I = 2, NVELP
        IF (VEL(I) == VSTART) CYCLE

        NSTART = I - 1
        GO TO 3
      END DO

      IF (IFLAG<2 .OR. IFLAG==4) THEN
        VELNEW = VSTART
      ELSE
        VELNEW = (T0NEW + (/(J1,J1=0,NTNEW - 1)/)*DTNEW)*VSTART**2
      ENDIF

      RETURN

    3 CONTINUE
      TSTART = TIM(NSTART)
      TSTOP = TIM(NVELP)

      IF (IFLAG>0 .AND. IFLAG<4) THEN
        VEL(NSTART:NVELP) = TIM(NSTART:NVELP)*VEL(NSTART:NVELP)**2

        IF (IFLAG > 1) VSTART = VSTART**2
      ENDIF

      IF (IFLAG < 3) THEN
        NSPLINE  = NVELP - NSTART + 1
        IPOINT1  = 1 + NVELP
        IPOINT1E = IPOINT1 + NSPLINE - 1
        IPOINT2  = IPOINT1 + NVELP
        IPOINT2E = IPOINT2 + NSPLINE - 1

        CALL SPLINE (NSPLINE, TIM(NSTART:NVELP), VEL(NSTART:NVELP), &
          WORK(1:NSPLINE), WORK(IPOINT1:IPOINT1E), WORK(IPOINT2:IPOINT2E))
      ELSE IF (IFLAG == 3) THEN
        DO I = NSTART + 1, NVELP
          IF (VEL(I) <= VEL(I-1)) GO TO 502
        END DO
      ENDIF

      iflag_choice: SELECT CASE (IFLAG)
      CASE (0)
        VSTOP = VEL(NVELP)

        DO I = 1, NTNEW
          TIMNEW = T0NEW + (I - 1)*DTNEW

          IF (TIMNEW < TSTART) THEN
            VELNEW(I) = VSTART
          ELSE IF (TIMNEW >= TSTOP) THEN
            VELNEW(I) = VSTOP
          ELSE
            CALL spline_v (NSPLINE, TIM(NSTART:NVELP), VEL(NSTART:NVELP), &
              WORK(1:NSPLINE), WORK(IPOINT1:IPOINT1E), &
              WORK(IPOINT2:IPOINT2E), TIMNEW, 0, VELNEW(I))

            IF (VELNEW(I) <= 0.) GO TO 501
          ENDIF
        END DO
      CASE (1)
        CALL spline_v (NSPLINE, TIM(NSTART:NVELP), VEL(NSTART:NVELP), &
          WORK(1:NSPLINE), WORK(IPOINT1:IPOINT1E), WORK(IPOINT2:IPOINT2E), &
          TSTOP, 1, VSTOP)

        IF (VSTOP <= 0.) GO TO 502

        VSTOP = SQRT(VSTOP)

        DO I = 1, NTNEW
          TIMNEW = T0NEW + (I - 1)*DTNEW

          IF (TIMNEW < TSTART) THEN
            VELNEW(I) = VSTART
          ELSE IF (TIMNEW >= TSTOP) THEN
            VELNEW(I) = VSTOP
          ELSE
            CALL spline_v (NSPLINE, TIM(NSTART:NVELP), VEL(NSTART:NVELP), &
              WORK(1:NSPLINE), WORK(IPOINT1:IPOINT1E), &
              WORK(IPOINT2:IPOINT2E), TIMNEW, 1, VELNEW(I))

            IF (VELNEW(I) <= 0.) GO TO 502

            VELNEW(I) = SQRT(VELNEW(I))
          ENDIF
        END DO
      CASE (2)
        CALL spline_v (NSPLINE, TIM(NSTART:NVELP), VEL(NSTART:NVELP), &
          WORK(1:NSPLINE), WORK(IPOINT1:IPOINT1E), WORK(IPOINT2:IPOINT2E), &
          TSTOP, 1, VSTOP)

        IF (VSTOP <= 0.) GO TO 502

        DO I = 1, NTNEW
          TIMNEW = T0NEW + (I - 1)*DTNEW

          IF (TIMNEW < TSTART) THEN
            VELNEW(I) = TIMNEW*VSTART
          ELSE IF (TIMNEW >= TSTOP) THEN
            VELNEW(I) = VEL(NVELP) + VSTOP*(TIMNEW - TSTOP)
          ELSE
            CALL spline_v (NSPLINE, TIM(NSTART:NVELP), VEL(NSTART:NVELP), &
              WORK(1:NSPLINE), WORK(IPOINT1:IPOINT1E), &
              WORK(IPOINT2:IPOINT2E), TIMNEW, 0, VELNEW(I))

            IF (I > 1) THEN
              IF (VELNEW(I) <= VELNEW(I-1)) GO TO 502
            ENDIF
          ENDIF
        END DO
      CASE (3)
        VSTOP = (VEL(NVELP)-VEL(NVELP-1))/(TIM(NVELP)-TIM(NVELP-1))
        NTEST = NSTART + 1

        DO I = 1, NTNEW
          TIMNEW = T0NEW + (I - 1)*DTNEW

          IF (TIMNEW <= TSTART) THEN
            VELNEW(I) = TIMNEW*VSTART
          ELSE IF (TIMNEW >= TSTOP) THEN
            VELNEW(I) = VEL(NVELP) + VSTOP*(TIMNEW - TSTOP)
          ELSE
            do
              if(timnew <= tim(ntest)) exit
              ntest = ntest + 1
            end do

            VELNEW(I) = VEL(NTEST-1) + (TIMNEW - TIM(NTEST-1))* &
              (VEL(NTEST)-VEL(NTEST-1))/(TIM(NTEST)-TIM(NTEST-1))
          ENDIF
        END DO
      CASE (4)
        TV2STOP = TIM(NVELP)*VEL(NVELP)**2
        V2STOP = VEL(NVELP)**2  +  2.0*TIM(NVELP)*VEL(NVELP)* &
                  (VEL(NVELP)-VEL(NVELP-1))/(TIM(NVELP)-TIM(NVELP-1))
        IF (V2STOP <= 0.) GO TO 502

        NTEST = NSTART + 1

        DO I = 1, NTNEW
          TIMNEW = T0NEW + (I - 1)*DTNEW

          IF (TIMNEW <= TSTART) THEN
            VELNEW(I) = VSTART
            TV2LAST = TIMNEW*VELNEW(I)**2
          ELSE IF (TIMNEW >= TSTOP) THEN
            VELNEW(I) = SQRT((TV2STOP + V2STOP*(TIMNEW - TSTOP))/TIMNEW)
          ELSE
            do
              if(timnew <= tim(ntest)) exit
              ntest = ntest + 1
            end do

            VELNEW(I) = VEL(NTEST-1) + (TIMNEW - TIM(NTEST-1))* &
              (VEL(NTEST)-VEL(NTEST-1))/(TIM(NTEST)-TIM(NTEST-1))
            TV2 = TIMNEW*VELNEW(I)**2

            IF (I > 1) THEN
              IF (TV2 <= TV2LAST) GO TO 502
            ENDIF

            TV2LAST = TV2
          ENDIF
        END DO
      END SELECT iflag_choice

      RETURN

  501 CONTINUE
      call pc_error( &
        '**** Error in INTPVELF -- Found Negative RMS Velocity ****')
      ier = 1
      RETURN

  502 CONTINUE
      call pc_error( &
        '**** Error in INTPVELF -- Found Imaginary Interval Velocity ****')
      ier = 1
      RETURN

      end subroutine INTPVELF
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module intpvelf_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
