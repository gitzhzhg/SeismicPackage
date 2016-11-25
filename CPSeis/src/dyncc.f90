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
! Name       : dyncc       (DYNAMIC TRACE SHIFT BY CUBIC INTERPOLATION)
! Category   : velocity
! Written    : 1988-12-01   by: Tom Stoeckley
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : This primitive dynamically adjusts the timings on a
!              seismic trace by using a four-point cubic interpolation
!              algorithm.  Can be used for NMO, depth conversion,
!              time-variant statics, etc.  Several subroutines are
!              available which are efficient for different purposes.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!  The algorithm used in this primitive uses two points on each side
!  of the specified location to calculate the returned value.  Weights
!  are assigned to these four points in such a way as to guarantee that
!  the interpolated values and their first derivatives will be continuous
!  over the entire length of the arrays.  Edge effects (at the edges of
!  the arrays where there are less than two points on one or both sides)
!  are handled automatically.
!
!  This algorithm is also used in the CPS primitive STATCC, which
!  shifts a trace statically.  STATCC is used in SHFT and in most
!  residual statics programs (IMS, FISH, SISC, RTC, CC3D, CS3D, etc.).
!
!  DYNCC Subroutine:
!  This subroutine dynamically adjusts an input trace and returns an
!  output trace.  This subroutine uses arrays of sparsely-specified exact
!  input (TA) and output (TB) indices.  Used in SDIP, SDIP3D, IMS, RTC,
!  RTC3D, HADC, and perhaps other places.  This is the original routine
!  in this primitive.
!
!  DYNCC_FORWARD and DYNCC_FORWARD2 Subroutine:
!  This subroutine is similar to DYNCC, except that the TB array is
!  eliminated and the TA array has the same length as the trace.
!  [The implied TB array contains indices such that TB(I) = I.]  This
!  subroutine should be faster than DYNCC, and should be more accurate
!  for NMO correction.  This subroutine is appropriate for forward
!  NMO correction, but can be used for any dynamic trace adjustment
!  involving stretching and/or compression.
!
!  DYNCC_REVERSE and DYNCC_REVERSE2 Subroutine:
!  This subroutine is similar to DYNCC, except that the TA array is
!  eliminated and the TB array has the same length as the trace.
!  [The implied TA array contains indices such that TA(I) = I.]  This
!  subroutine should be faster than DYNCC, and should be more accurate
!  for NMO correction.  This subroutine is appropriate for reverse
!  NMO correction, but can be used for any dynamic trace adjustment
!  involving stretching and/or compression.
!
!  DYNCC_CONVERT Subroutine:
!  This subroutine converts array TB(N) to array TA(N), which can be
!  passed to DYNCC_FORWARD.  This procedure is needed for reverse NMO,
!  or for any procedure in which TB(N) is known instead of TA(N).
!
!  DYNCC_INTERP Subroutine:
!  This is a general-purpose function which returns a single interpolated
!  value.  It is much less efficient than the subroutines which operate
!  on an entire array.
!
!  DYNCC_PITY Subroutine:
!  This is an old subroutine used on unix workstations for cubic
!  interpolation/resampling of velocity functions in the velocity
!  analysis program.  This subroutine is to be used in combination
!  with DYNCC_ZIP1.
!
!  DYNCC_ZIP1 Subroutine:
!  This is an old subroutine used on unix workstations for cubic
!  interpolation/resampling of velocity functions in the velocity
!  analysis program.  This subroutine is to be used in combination
!  with DYNCC_PITY.  Subroutines similar to this one, but useable for
!  2-D and 3-D cubic interpolation, are available and can be added
!  to this primitive if needed.  They would be called DYNCC_ZIP2 and
!  DYNCC_ZIP3.  They would also be used in combination with DYNCC_PITY.
!
!  DYNCC_PITY must be called first, specifying the abscissa array and
!  the exact abscissa whose matching ordinate you want.  Then the weights
!  returned by DYNCC_PITY, plus the ordinate array, are to be passed to
!  this function which returns your desired interpolated ordinate.
!  If you have several ordinate arrays corresponding to the abscissa
!  array passed to DYNCC_PITY, you can call DYNCC_ZIP1 for each ordinate
!  array, passing the same weights each time.
!
!  DYNCC_QUICK Subroutine:
!  This subroutine returns a single interpolated value from an input trace.
!
!  DYNCC_SHIFT Subroutine:
!  This subroutine dynamically adjusts an input trace and returns an
!  output trace.  This subroutine is similar to DYNCC, except that the TB
!  array is eliminated and the TA array has the same length as the trace.
!  [The implied TB array contains indices such that TB(I) = I.]  Like
!  DYNCC_FORWARD, this subroutine should be faster than DYNCC.  This
!  subroutine calls DYNCC_QUICK for each trace sample.
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
! To dynamically shift a trace:
!
!                         i  i i  i   i   i    o   o
!            CALL DYNCC (DOP,N,TA,TB,NVAL,A,   B,BMUTE)
!
! real     DOP     = Doppler stretch factor greater than one (see below).
! integer  N       = Length of index arrays TA and TB.
! real     TA(N)   = Array of exact trace indices on input trace.
! real     TB(N)   = Array of corresponding trace indices on output trace.
! integer  NVAL    = Number of trace values.
! real     A(NVAL) = Input trace (not changed by this routine).
! real     B(NVAL) = Output trace (must not be same address as input trace).
! real     BMUTE   = Mute index (due to doppler mute) to be placed into header
!                     word 2.  Output trace is dead above this index.
!
!  Input and output trace indices are linearly interpolated within
!      range  (TA(i),TB(i))  to  (TA(i+1),TB(i+1)) .
!  Values of TA and TB beyond the trace boundaries are OK.
!  Output trace is dead before TB(1) and after TB(N).
!  Output trace is dead in regions of stretch or compression exceeding
!      the value of DOP.  Therefore it is possible for the output trace
!      to contain embedded dead zones when this subroutine is used.
!  Output trace is dead in regions of time reversals.
!  Trace indices are related to trace times as follows:
!      INDEX = 1.0 + (TIME-TSTRT)/DT    where TSTRT and DT are globals.
!      These indices can (and usually will) be fractional.
!
!-------------------------------------------------------------------------------
! To dynamically shift a trace forward (e.g. forward NMO correction):
!
!                                  i  i i i     o   o
!            CALL DYNCC_FORWARD  (DOP,N,A,TA,   B,NMUTE)
!
!                                  i       i   i i i     o
!            CALL DYNCC_FORWARD2 (ISTART,ISTOP,N,A,TA,   B)
!
! real     DOP     = Doppler stretch factor greater than one.
! integer  ISTART  = Index of first element of array B(N) to set.
! integer  ISTOP   = Index of last element of array B(N) to set.
! integer  N       = Length of arrays A, B, and TA.
! real     A(N)    = Input trace (not changed by this routine).
! real     B(N)    = Output trace (must be different from input trace).
! real     TA(N)   = Array of exact trace indices on input trace which will
!                     move to output trace (see below).
! integer  NMUTE   = Mute index (first live index on output trace).
!
!  DYNCC_FORWARD calculates ISTART, ISTOP, and NMUTE from DOP and TA(N),
!  clears the portion of array B(N) before ISTART and after ISTOP, and
!  then calls DYNCC_FORWARD2.
!
!  DYNCC_FORWARD2 assumes that array B(N) has already been cleared
!  before ISTART and after ISTOP.
!
!  Example of the meaning of the TA(N) array:
!
!  If TA(12) is 17.42, this means that the value of B(12) should come
!  from A(17.42).  Since 17.42 is not an integer, array A(N) must be
!  interpolated.  A linear interpolation would use A(17) and A(18).
!  But in this subroutine, the values of A(16), A(17), A(18), and A(19)
!  would be used to calculate B(12).
!  Values of TA beyond the trace boundaries are OK.
!
!  Note regarding DOP:
!  The trace will be killed down to the last index where it is
!  stretched by more than the factor DOP.  If DOP is less than 1.0,
!  its reciprocal is used.  If DOP is 0.0 or less, infinite stretch
!  is allowed.  If DOP is negative, NMUTE will be 1, and DYNCC_FORWARD2
!  will be called with ISTART=1 and ISTOP=N.
!
!-------------------------------------------------------------------------------
! To dynamically shift a trace reverse (e.g. reverse NMO correction):
!
!                                  i       i   i i i     o   o
!            CALL DYNCC_REVERSE  (DOP         ,N,A,TB,   B,NMUTE)
!            CALL DYNCC_REVERSE2 (ISTART,ISTOP,N,A,TB,   B)
!
! real     DOP     = Doppler stretch factor greater than one.
! integer  ISTART  = Index of first element of array B(N) to set.
! integer  ISTOP   = Index of last element of array B(N) to set.
! integer  N       = Length of arrays A, B, and TB.
! real     A(N)    = Input trace (not changed by this routine).
! real     B(N)    = Output trace (must be different from input trace).
! real     TB(N)   = Array of exact trace indices on OUTPUT trace which will
!                     move from INPUT trace (see below).
! integer  NMUTE   = Mute index (first live index on output trace).
!
!  DYNCC_REVERSE2 assumes that array B(N) has already been cleared
!  before ISTART and after ISTOP.  Normally ISTART and ISTOP should be
!  set to 1 and N respectively, since the calling program probably does
!  not have the information to calculate ISTART and ISTOP.  This is
!  because ISTART and ISTOP must be calculated from TA(N), which is
!  calculated inside DYNCC_REVERSE2.
!
!  Example of the meaning of the TB(N) array:
!
!  If TB(12) is 17.42, this means that the value of A(12) should be
!  moved to B(17.42).  Since 17.42 is not an integer, array A(N) must be
!  interpolated to give values which can be moved to integral indices
!  of B, such as B(17) and B(18).  This routine uses array TB(N) to
!  calculate an array TA(N) which will do this.
!  Values of TB beyond the trace boundaries are OK.
!
!  Note regarding DOP:
!  The trace will be killed down to the last index where it is
!  stretched by more than the factor DOP.  If DOP is less than 1.0,
!  its reciprocal is used.  If DOP is 0.0 or less, infinite stretch
!  is allowed.  (For reverse NMO correction, where the trace is
!  compressed rather than stretched, this argument will do nothing.)
!
!  DYNCC_REVERSE calls DYNCC_CONVERT and DYNCC_FORWARD and therefore
!  is less efficient than DYNCC_FORWARD.
!
!-------------------------------------------------------------------------------
! To convert index array TB(N) to TA(N):
!
!                                       i i     o
!                   CALL DYNCC_CONVERT (N,TB,   TA)
!
! integer  N       = Length of arrays TA and TB.
! real     TB(N)   = Array of exact trace indices on OUTPUT trace which will
!                     move from INPUT trace.
! real     TA(N)   = Array of exact trace indices on input trace which will
!                     move to output trace (returned).
!
!  See the documentation for DYNCC_FORWARD and DYNCC_REVERSE for an
!  explanation of the TB(N) and TA(N) arrays.
!
!-------------------------------------------------------------------------------
! To obtain a single interpolated value:
!
!                      o                    i i i   i
!                   YANSWER = DYNCC_INTERP (X,Y,N,XWANT)
!
! real     X(N)    = Array of abscissae (independent variables).
! real     Y(N)    = Array of ordinates (dependent variables).
! integer  N       = Length of arrays X and Y.
! real     XWANT   = Exact abscissa whose matching ordinate is requested.
! real     YANSWER = Ordinate corresponding to XWANT (returned).
!
!  Note: The abscissae in array X(N) do not have to be evenly spaced.
!  However, array X(N) must be in either ascending or descending order
!  with no adjacent values equal.
!
!  DYNCC_INTERP calls DYNCC_PITY and DYNCC_ZIP1.
!
!-------------------------------------------------------------------------------
! To get weights and subscripts for obtaining a single interpolated value:
!
!                                      i   i i    o
!                   CALL DYNCC_PITY (XWANT,X,N,   W)
!
! real     XWANT   = Exact abscissa whose matching ordinate is requested.
! real     X(N)    = Array of abscissae (independent variables).
! integer  N       = Length of array X.
! real     W(9)    = Weights and subscripts (returned).
!
!  Note: The abscissae in array X(N) do not have to be evenly spaced.
!  However, array X(N) must be in either ascending or descending order
!  with no adjacent values equal.
!
!  Given XWANT and abscissae X(1)...X(N), returns W(1)...W(9)
!        so that YANSWER = W(1)*Y(L1)+W(2)*Y(L2)+...+W(J)*Y(LJ)  where
!        W(1),W(2),W(3),W(4) = weights
!        W(5),W(6),W(7),W(8) = subscripts = L1,L2,L3,L4
!        W(9) = number of weights and subscripts to use = J = 1, 2, 3, O.
!  YANSWER above is the desired ordinate to be calculated from array Y(N).
!
!  The W(9) array will cause flat extrapolation outside the range of
!  the abscissae.
!
!-------------------------------------------------------------------------------
! To use weights and subscripts to obtain a single interpolated value:
!
!                           o                  i i
!                        YANSWER = DYNCC_ZIP1 (W,Y)
!
! real     W(9)    = Weights and subscripts from DYNCC_PITY.
! real     Y(N)    = Array of ordinates (dependent variables).  Dimension
!                     must be the same length as array X(N) in DYNCC_PITY.
! real     YANSWER = Ordinate corresponding to XWANT in DYNCC_PITY (returned).
!
!  NOTE:  Subroutines similar to this one, but useable for 2-D and 3-D
!  cubic interpolation, are available and can be added to this primitive
!  if needed.  They would be called DYNCC_ZIP2 and DYNCC_ZIP3.  They
!  would also be used in combination with DYNCC_PITY.
!  In these cases, DYNCC_PITY will be called once for each dimension,
!  and DYNCC_ZIP2 or DYNCC_ZIP3 will then be called, passing the weights
!  returned by DYNCC_PITY for each dimension.
!
!-------------------------------------------------------------------------------
! To use an exact index to obtain a single interpolated value:
!
!                o                   i  i    i
!              answer = DYNCC_quick (A, n, exact)
!
! integer  N       = Length of array X.
! real     A(N)    = Input trace (not changed by this routine).
! real     exact   = Exact index whose interpolated value is requested.
! real     answer  = Interpolated trace value.
!
!  NOTE: EXACT  is the same as one element of TA(N) in other routines.
!  NOTE: ANSWER is the same as one element of B(N) in other routines.
!
!-------------------------------------------------------------------------------
! To dynamically shift a trace:
!
!                                  i  i  i   o
!                call DYNCC_shift (A, N, TA, B)
!
! integer  N       = Length of arrays A, B, and TA.
! real     A(N)    = Input trace (not changed by this routine).
! real     B(N)    = Output trace (must be different from input trace).
! real     TA(N)   = Array of exact trace indices on input trace which will
!                     move to output trace.
!
!  This routine repeatedly calls DYNCC_QUICK.
!  This routine gives the same results as DYNCC_FORWARD2 but is twice as fast.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
! 10. 2006-06-20  B. Menger    Removed Unused Variables.
!  9. 2001-12-10  Stoeckley    Add DYNCC_QUICK and DYNCC_SHIFT; modify the use
!                               of a negative DOP argument in DYNCC_FORWARD;
!                               add intent(in) and intent(out) where missing.
!  8. 2001-04-30  Selzler      Corrected closing header_word_doc tag.
!  7. 2000-07-19  Selzler      Fixed problems found by CPS Fortran Code Review.
!  6. 2000-01-25  Selzler      Clean up trailing blanks and block labels
!  5. 1999-11-19  Selzler      Added RCS "Id" strings to tag executeable
!  4. 1999-10-05  Selzler      Conversion to F90
!  3. 1999-01-11  Goodger      Begin using the fortran90 compiler.
!  2. 1998-03-02  Stoeckley    Added DYNCC_FORWARD, DYNCC_REVERSE,
!                               DYNCC_FORWARD2, DYNCC_REVERSE2,
!                               DYNCC_CONVERT, DYNCC_INTERP, DYNCC_PITY,
!                               and DYNCC_ZIP1.  Also converted to
!                               Fortran-90 to allow use of an automatic
!                               array in DYNCC_REVERSE and DYNCC_REVERSE2.
!  1. 1993-05-07  Stoeckley    Fixed one card to keep indices on output trace
!                               from going negative when index arrays have
!                               negative indices.
!  0. 1988-12-01  Stoeckley    Initial version.
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


      module dyncc_module
      implicit none
      public

      character(len=100),public :: dyncc_ident = &
        "$Id: dyncc.f90,v 1.10 2006/06/20 13:11:51 Menger prod sps $"

      contains


!!----------------------------- dyncc -------------------------------------!!
!!----------------------------- dyncc -------------------------------------!!
!!----------------------------- dyncc -------------------------------------!!


      SUBROUTINE DYNCC (DOP, N, TA, TB, NVAL, A, B, BMUTE)
      IMPLICIT NONE
      INTEGER,INTENT(IN)  :: N                                   ! argument
      INTEGER,INTENT(IN)  :: NVAL                                ! argument
      REAL,   INTENT(IN)  :: DOP                                 ! argument
      REAL,   INTENT(OUT) :: BMUTE                               ! argument
      REAL,   INTENT(IN)  :: TA(N)                               ! argument
      REAL,   INTENT(IN)  :: TB(N)                               ! argument
      REAL,   INTENT(IN)  :: A(NVAL)                             ! argument
      REAL,   INTENT(OUT) :: B(NVAL)                             ! argument
      REAL,   PARAMETER   :: GLO = 2.0                           ! local
      INTEGER             :: J1, J2   , NFIRST, J, ILO, IUP  ! local
      REAL                :: DOPP3, DOPP4, GUP, HUP, HLO, BLO    ! local
      real                :: BUP, ALO, AUP, SLOPE, AAA     ! local
      INTEGER,ALLOCATABLE :: L1U(:)                              ! local
      REAL   ,ALLOCATABLE :: F1U(:), G1U(:)                      ! local

      ! SET UP NEEDED CONSTANTS.
      DOPP3 = 1./DOP
      DOPP4 = DOP
      GUP = NVAL - 2
      HUP = NVAL

      ! INITIALIZE VARIABLES.
      B = 0.
      NFIRST = NVAL
      HLO = 0.

      ! GO THRU OUTER LOOP.
      outer_loop: DO J = 2, N
        BLO = TB(J-1)
        BUP = TB(J)

        IF (BUP <= BLO) CYCLE outer_loop

        ALO = TA(J-1)
        AUP = TA(J)
        SLOPE = (AUP - ALO)/(BUP - BLO)

        IF (SLOPE<DOPP3 .OR. SLOPE>DOPP4) CYCLE outer_loop

        ! ILO=MAX1(BLO+(AMAX1(ALO,GLO)-ALO)/SLOPE,HLO+1.)
        ILO = MAX1(BLO + (AMAX1(ALO,GLO) - ALO)/SLOPE,HLO) + 1
        IUP = MIN1(BLO + (AMIN1(AUP,GUP) - ALO)/SLOPE,HUP)
        ! HLO=IUP         this card replaced by the following card 5/7/93
        HLO = MAX0(IUP,0)

        IF (IUP < ILO) CYCLE outer_loop

        NFIRST = MIN0(NFIRST,ILO)
        AAA = ALO + (ILO - BLO)*SLOPE - SLOPE
        ! GO THRU INNER LOOP.

        ALLOCATE (L1U(IUP-ILO+1), F1U(IUP-ILO+1), G1U(IUP-ILO+1))

        L1U = AAA + SLOPE + SLOPE*(/(J1,J1=0,IUP - ILO)/)
        F1U = AAA + SLOPE + SLOPE*(/(J2,J2=0,IUP - ILO)/) - L1U
        G1U = 1. - F1U
        B(ILO:IUP) = ((-G1U*G1U*F1U*A(L1U-1))+(G1U*G1U*F1U+G1U*G1U+G1U)*A(L1U)+&
          (F1U*F1U*G1U+F1U*F1U+F1U)*A(L1U+1)-F1U*F1U*G1U*A(L1U+2))/(F1U*F1U + &
          G1U*G1U + 1.)

        DEALLOCATE (L1U, F1U, G1U)
      END DO outer_loop

      ! SET MUTE HEADER WORD.
      BMUTE = NFIRST
      RETURN
      END SUBROUTINE DYNCC


!!----------------------------- dyncc reverse -------------------------------!!
!!----------------------------- dyncc reverse -------------------------------!!
!!----------------------------- dyncc reverse -------------------------------!!


      SUBROUTINE DYNCC_REVERSE (DOP, N, A, TB, B, NMUTE)
      IMPLICIT NONE
      INTEGER,intent(in)  :: N                        ! argument
      INTEGER,intent(out) :: NMUTE                    ! argument
      REAL   ,intent(in)  :: DOP                      ! argument
      REAL   ,intent(in)  :: A(N)                     ! argument
      REAL   ,intent(in)  :: TB(N)                    ! argument
      REAL   ,intent(out) :: B(N)                     ! argument
      REAL                :: TA(N)                    ! local

      CALL DYNCC_CONVERT (N, TB, TA)
      CALL DYNCC_FORWARD (DOP, N, A, TA, B, NMUTE)
      RETURN
      END SUBROUTINE DYNCC_REVERSE


!!----------------------------- dyncc reverse2 -------------------------------!!
!!----------------------------- dyncc reverse2 -------------------------------!!
!!----------------------------- dyncc reverse2 -------------------------------!!


      SUBROUTINE DYNCC_REVERSE2 (ISTART, ISTOP, N, A, TB, B)
      IMPLICIT NONE
      INTEGER,intent(in)  :: ISTART                   ! argument
      INTEGER,intent(in)  :: ISTOP                    ! argument
      INTEGER,intent(in)  :: N                        ! argument
      REAL   ,intent(in)  :: A(N)                     ! argument
      REAL   ,intent(in)  :: TB(N)                    ! argument
      REAL   ,intent(out) :: B(N)                     ! argument
      REAL                :: TA(N)                    ! local

      CALL DYNCC_CONVERT  (N, TB, TA)
      CALL DYNCC_FORWARD2 (ISTART, ISTOP, N, A, TA, B)
      RETURN
      END SUBROUTINE DYNCC_REVERSE2


!!----------------------------- dyncc forward -------------------------------!!
!!----------------------------- dyncc forward -------------------------------!!
!!----------------------------- dyncc forward -------------------------------!!


      SUBROUTINE DYNCC_FORWARD (DOP, N, A, TA, B, NMUTE)
      IMPLICIT NONE
      INTEGER,intent(in)  :: N                                ! argument
      INTEGER,INTENT(OUT) :: NMUTE                            ! argument
      REAL   ,INTENT(IN)  :: DOP                              ! argument
      REAL   ,intent(in)  :: A(N)                             ! argument
      REAL   ,intent(in)  :: TA(N)                            ! argument
      REAL   ,intent(out) :: B(N)                             ! argument
      INTEGER             :: ISTART, ISTOP, I                 ! local
      REAL                :: DOP2                    ! local

      ! get mute index:

  !   IF (DOP >= 1.0) THEN
  !     DOP2 = 1.0/DOP
  !   ELSE IF (DOP >= 0.0) THEN
  !     DOP2 = DOP
  !   ELSE
  !     DOP2 = 0.0
  !   ENDIF
  !                  ! now DOP2 is between 0.0 and 1.0

      IF (DOP >= 1.0) THEN
        DOP2 = 1.0/DOP
      ELSE
        DOP2 = DOP
      ENDIF
                     ! now DOP2 is between 0.0 and 1.0 or is negative.

      NMUTE = 1

      if (dop2 >= 0.0) then

           DO I = 2, N
                IF (TA(I) - TA(I-1) >= DOP2) CYCLE

                IF (.NOT.(TA(I)>=1 .AND. TA(I-1)>=1 .AND. &
                          TA(I)<=N .AND. TA(I-1)<=N)) CYCLE

                NMUTE = I
           END DO

           ! clear muted area:

           ISTART = NMUTE
           ISTOP = N
           B(:ISTART-1) = 0.0
           B(ISTOP+1:N) = 0.0

      else
           istart = 1
           istop = n
      end if

      ! calculate non-muted area:

      CALL DYNCC_FORWARD2 (ISTART, ISTOP, N, A, TA, B)
      RETURN
      END SUBROUTINE DYNCC_FORWARD


!!----------------------------- dyncc forward2 -------------------------------!!
!!----------------------------- dyncc forward2 -------------------------------!!
!!----------------------------- dyncc forward2 -------------------------------!!


      SUBROUTINE DYNCC_FORWARD2 (ISTART, ISTOP, N, A, TA, B)
      IMPLICIT NONE
      INTEGER,INTENT(IN)  :: ISTART                                  ! argument
      INTEGER,INTENT(IN)  :: ISTOP                                   ! argument
      INTEGER,INTENT(IN)  :: N                                       ! argument
      REAL   ,INTENT(IN)  :: A(N)                                    ! argument
      REAL   ,INTENT(IN)  :: TA(N)                                   ! argument
      REAL   ,INTENT(OUT) :: B(N)                                    ! argument


      INTEGER,ALLOCATABLE :: L1U(:)                                  ! local
      REAL   ,ALLOCATABLE :: F1U(:), G1U(:)                          ! local
      REAL   ,ALLOCATABLE :: A11U(:), A21U(:), A31U(:), A41U(:)      ! local
      LOGICAL,ALLOCATABLE :: L1V(:), L2V(:), L3V(:), L4V(:), L5V(:)  ! local

      ALLOCATE (L1U(ISTOP-ISTART+1), F1U(ISTOP-ISTART+1), G1U(ISTOP-ISTART+1), &
        A11U(ISTOP-ISTART+1), A21U(ISTOP-ISTART+1), A31U(ISTOP-ISTART+1), A41U(&
        ISTOP-ISTART+1), L1V(ISTOP-ISTART+1), L2V(ISTOP-ISTART+1), L3V(ISTOP-&
        ISTART+1), L4V(ISTOP-ISTART+1), L5V(ISTOP-ISTART+1))

      L5V = .FALSE.
      L4V = .FALSE.
      L3V = .FALSE.
      L1V = .FALSE.

      L1U = TA(ISTART:ISTOP)   ! L and L+1 bracket the desired value.
      F1U = TA(ISTART:ISTOP) - L1U
      G1U = 1.0 - F1U

      L2V = L1U - 1>=1 .AND. L1U+2<=N

      WHERE (L2V)
        B(ISTART:ISTOP) = ((-G1U*G1U*F1U*A(L1U-1))+(G1U*G1U*F1U+G1U*G1U+G1U)*A(&
          L1U)+(F1U*F1U*G1U+F1U*F1U+F1U)*A(L1U+1)-F1U*F1U*G1U*A(L1U+2))/(F1U*&
          F1U + G1U*G1U + 1.)
      ELSEWHERE
        A11U = 0.0
        A21U = 0.0
        A31U = 0.0
        A41U = 0.0
        L1V = L1U - 1>=0 .AND. L1U-1<=N
      END WHERE

      WHERE (L1V)
        A11U = A(L1U-1)
      END WHERE

      WHERE (.NOT.L2V)
        L3V = L1U>=0 .AND. L1U<=N
      END WHERE

      WHERE (L3V)
        A21U = A(L1U)
      END WHERE

      WHERE (.NOT.L2V)
        L4V = L1U + 1>=0 .AND. L1U+1<=N
      END WHERE

      WHERE (L4V)
        A31U = A(L1U+1)
      END WHERE

      WHERE (.NOT.L2V)
        L5V = L1U + 2>=0 .AND. L1U+2<=N
      END WHERE

      WHERE (L5V)
        A41U = A(L1U+2)
      END WHERE

      WHERE (.NOT.L2V)
        B(ISTART:ISTOP) = ((-G1U*G1U*F1U*A11U) + (G1U*G1U*F1U + G1U*G1U + G1U)*&
          A21U + (F1U*F1U*G1U + F1U*F1U + F1U)*A31U - F1U*F1U*G1U*A41U)/(F1U*&
          F1U + G1U*G1U + 1.)
      END WHERE

      DEALLOCATE (L1U, F1U, G1U, A11U, A21U, A31U, A41U, L1V, L2V, L3V, L4V, &
        L5V)
      RETURN
      END SUBROUTINE DYNCC_FORWARD2


!!----------------------------- dyncc convert -------------------------------!!
!!----------------------------- dyncc convert -------------------------------!!
!!----------------------------- dyncc convert -------------------------------!!


      SUBROUTINE DYNCC_CONVERT (N, TB, TA)
      IMPLICIT NONE
      INTEGER,INTENT(IN)  :: N                       ! argument
      REAL   ,INTENT(IN)  :: TB(N)                   ! argument
      REAL   ,INTENT(OUT) :: TA(N)                   ! argument
      INTEGER             :: I, J                    ! local

!      J=1
!      DO I=1,N
!          IF (J.GT.N) THEN
!             TA(I) = 0.0
!          ELSE IF (TB(J).GT.I) THEN
!             TA(I) = 0.0
!          ELSE IF (TB(J).LE.I) THEN
!10           IF (J.LT.N) THEN
!                     IF (TB(J+1).GT.I) THEN
!                          TA(I) = J + (I - TB(J)) / (TB(J+1) - TB(J))
!                     ELSE
!                          J = J+1
!                          GO TO 10
!                     END IF
!             END IF
!          ELSE
!             TA(I) = N+1
!          END IF
!      END DO

      J = 1

      DO I = 1, N
   10   CONTINUE

        IF (TB(J) > I) THEN
          TA(I) = 0.0
        ELSE IF (TB(J) == I) THEN
          TA(I) = J
        ELSE IF (J == N) THEN
          TA(I) = N + 1.0
        ELSE IF (TB(J+1) >= I) THEN
          TA(I) = J + (I - TB(J))/(TB(J+1)-TB(J))
        ELSE
          J = J + 1
          GO TO 10
        ENDIF
      END DO
      RETURN
      END SUBROUTINE DYNCC_CONVERT


!!----------------------------- dyncc interp -------------------------------!!
!!----------------------------- dyncc interp -------------------------------!!
!!----------------------------- dyncc interp -------------------------------!!


      FUNCTION DYNCC_INTERP (X, Y, N, XWANT) result (yanswer)
      IMPLICIT NONE
      INTEGER,intent(in) :: N                       ! argument
      REAL   ,intent(in) :: XWANT                   ! argument
      REAL   ,intent(in) :: X(N)                    ! argument
      REAL   ,intent(in) :: Y(N)                    ! argument
      REAL               :: yanswer                 ! result
      REAL               :: W(9)                    ! local

      CALL DYNCC_PITY (XWANT, X, N, W)
      yanswer = DYNCC_ZIP1(W,Y)
      RETURN
      END FUNCTION DYNCC_INTERP


!!----------------------------- dyncc zip1 -------------------------------!!
!!----------------------------- dyncc zip1 -------------------------------!!
!!----------------------------- dyncc zip1 -------------------------------!!


      FUNCTION DYNCC_ZIP1 (WX, H) result (yanswer)
      IMPLICIT NONE
      REAL   ,INTENT(IN) :: WX(9)                       ! argument
      REAL   ,INTENT(IN) :: H(:)                        ! argument
      REAL               :: yanswer                     ! result
      INTEGER            :: JX      ! local
      REAL               :: AA                          ! local

      JX = WX(9)
      AA = 0.
      AA = DOT_PRODUCT(WX(:JX),H(INT(WX(5:JX+4))))
      yanswer = AA
      RETURN
      END FUNCTION DYNCC_ZIP1


!!----------------------------- dyncc pity -------------------------------!!
!!----------------------------- dyncc pity -------------------------------!!
!!----------------------------- dyncc pity -------------------------------!!


      SUBROUTINE DYNCC_PITY (XWANT, X, N, W)
      IMPLICIT NONE
      INTEGER,INTENT(IN)  :: N                                  ! argument
      REAL   ,INTENT(IN)  :: XWANT                              ! argument
      REAL   ,INTENT(IN)  :: X(N)                               ! argument
      REAL   ,INTENT(OUT) :: W(9)                               ! argument
      INTEGER             :: I,J                                ! local
      REAL                :: F,A3,R3,XA,XB,XC,XD,A1,A2,S,R1,R2  ! local

!-----------------------------------------------
!     GIVEN XWANT AND ABSCISSAE X(1)...X(N), RETURNS W(1)...W(9)
!        SO THAT YWANT = W(1)*Y(L1)+W(2)*Y(L2)+...+W(J)*Y(LJ)  WHERE
!        W(1),W(2),W(3),W(4) = WEIGHTS
!        W(5),W(6),W(7),W(8) = SUBSCRIPTS = L1,L2,L3,L4
!        W(9) = NUMBER OF WEIGHTS AND SUBSCRIPTS TO USE = J = 1, 2, 3, O
!     ARRAY X MUST BE IN ASCENDING OR DESCENDING ORDER WITH NO VALUES EQ
!     DOES FLAT EXTRAPOLATION OUTSIDE THE RANGE OF THE ABSCISSAE.
!----------LOOK FOR NEEDED POINTS AND FIND EXACT MATCH IF POSSIBLE.

      I = 1

      IF (N > 1) THEN
        F = 1.

        IF (X(N) < X(1)) F = -1.

        DO I = 1, N
          IF (F*(X(I)-XWANT) < 0.) CYCLE

          IF (F*(X(I)-XWANT) == 0.) GO TO 950

          IF (I == N) GO TO 13

          IF (I - 2 < 0) GO TO 950

          IF (I - 2 == 0) GO TO 40

          GO TO 300
        END DO

        ! WE HAVE AN EXACT MATCH.
        I = N
      ENDIF

  950 CONTINUE
      W(5) = I
      W(1) = 1.
      W(9) = 1

      RETURN

      ! WE HAVE ONE POINT BEFORE AND ONE POINT AFTER.
   17 CONTINUE
      W(5) = I - 1
      W(6) = I
      W(9) = 2
      W(2) = (XWANT - X(I-1))/(X(I)-X(I-1))
      W(1) = 1. - W(2)

      RETURN

   13 CONTINUE
      IF (N == 2) GO TO 17
      ! WE HAVE TOTAL OF 3 POINTS.

   40 CONTINUE
      W(9) = 3
      A3 = 0.
      R3 = 0.
      IF (I > 2) GO TO 310

      ! WE HAVE 1 POINT BEFORE AND 2 POINTS AFTER.
      W(5) = I + 1
      W(6) = I
      W(7) = I - 1
      XA = X(I+1)
      XB = X(I)
      XC = X(I-1)
      GO TO 320

      ! WE HAVE 2 POINTS BEFORE AND 2 POINTS AFTER.
  300 CONTINUE
      W(9) = 4
      XD = X(I+1)

  310 CONTINUE
      XC = X(I)
      XB = X(I-1)
      XA = X(I-2)
      W(8) = I + 1
      W(7) = I
      W(6) = I - 1
      W(5) = I - 2

  320 CONTINUE
      J = W(9)
      A1 = (XC - XWANT)**2
      A2 = (XC - XB)**2

      IF (J == 4) A3 = (XWANT - XB)**2

      S = A1 + A2 + A3
      R1 = A1*(XWANT - XA)/(XB - XA)
      R2 = A2*(XWANT - XB)/(XC - XB)

      IF (J == 4) R3 = A3*(XWANT - XC)/(XD - XC)

      W(1) = (A1 - R1)/S
      W(2) = (R1 + A2 - R2)/S
      W(3) = (R2 + A3 - R3)/S

      IF (J == 4) W(4) = R3/S
      RETURN
      END SUBROUTINE DYNCC_PITY


!!----------------------------- dyncc quick -------------------------------!!
!!----------------------------- dyncc quick -------------------------------!!
!!----------------------------- dyncc quick -------------------------------!!

!! returns answer = interpolated value of a(n) at exact index a(exact).


      function DYNCC_quick (a, n, exact) result (answer)
      IMPLICIT NONE
      INTEGER,INTENT(IN) :: N                       ! argument
      REAL   ,INTENT(IN) :: a(N)                    ! argument
      REAL   ,INTENT(IN) :: exact                   ! argument
      REAL               :: answer                  ! result      ! a(exact)
      INTEGER            :: iexact                  ! local
      INTEGER            :: i1,i2,i3,i4             ! local
      real               :: e1,e2,e3,e4             ! local
      real               :: a2,a3                   ! local
      real               :: w1,w2,w3,w4             ! local

      if (n <= 0) then
           answer = 0.0
           return
      end if

      iexact = exact

      if (iexact == exact) then
           if (iexact >= 1 .and. iexact <= n) then
                answer = a(iexact)
                return
           end if
      end if

      i1 = iexact - 1
      i2 = iexact
      i3 = iexact + 1
      i4 = iexact + 2
                                !!  i1 < i2 <= exact <= i3 < i4

      e1 = exact - i1           !!  exact >  i1       e1 >  1.0
      e2 = i2 - exact           !!  exact >= i2       e2 <= 0.0
      e3 = exact - i3           !!  exact <= i3       e3 <= 0.0
      e4 = i4 - exact           !!  exact <  i4       e4 >  1.0

      A2 = e2 * e2          
      A3 = e3 * e3         

      W1 = A3 * e2        
      W2 = A3 * e1 - e3  
      W3 = A2 * e4 - e2 
      W4 = A2 * e3     

      if (i1 < 1 .or. i4 > n) then
           i1 = min(n,max(i1,1))
           i2 = min(n,max(i2,1))
           i3 = min(n,max(i3,1))
           i4 = min(n,max(i4,1))
      end if

      answer = (w1*a(i1) + w2*a(i2) + w3*a(i3) + w4*a(i4)) / (A3 + 1.0 + A2)
      RETURN
      END function DYNCC_quick


            !!   exact =   i2 + 0.5       i2        i3
            !!
            !!      e1 =     1.5          1.0       2.0
            !!      e2 =   - 0.5          0.0     - 1.0
            !!      e3 =   - 0.5        - 1.0       0.0
            !!      e4 =     1.5          2.0       1.0
            !!
            !!      A2 =     0.25         0.0       1.0
            !!      A3 =     0.25         1.0       0.0
            !!
            !!      W1 =   - 0.125        0.0       0.0
            !!      W2 =     0.875        2.0       0.0
            !!      W3 =     0.875        0.0       2.0
            !!      W4 =   - 0.125        0.0       0.0
            !!
            !!   sum w =     1.5          2.0       2.0
            !!   denom =     1.5          2.0       2.0


!!----------------------------- dyncc shift -------------------------------!!
!!----------------------------- dyncc shift -------------------------------!!
!!----------------------------- dyncc shift -------------------------------!!


      subroutine DYNCC_shift (a, n, ta, b)
      IMPLICIT NONE
      INTEGER,INTENT(IN)  :: n                       ! argument
      REAL   ,INTENT(IN)  :: a (n)                   ! argument
      REAL   ,INTENT(IN)  :: ta(n)                   ! argument
      REAL   ,intent(out) :: b (n)                   ! argument
      INTEGER             :: j                       ! local

      do j = 1,n
           b(j) = DYNCC_quick (a, n, ta(j))
      end do
      RETURN
      END subroutine DYNCC_shift


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module dyncc_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
