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
! Name       : opfilt
! Category   : filters
! Written    : 1980-03-20   by: Mike Ess ?
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Recursive solution to the Wiener-Levinson optimal filter
!            equation.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
! 1. This is an improved version of the OPFILT routine in SCILIB.
!
! 2. OPFILT solves the optimal filter equation:   R*A = B.
!
! 3. Reference:  N. Levinson, "Journal of Mathematics and Physics",
!                Vol 25, No 4, Jan 1947, pp 261-278.
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
!</header_word_doc>

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
!             i o i o i
! call opfilt(N,A,B,C,R)
!
! integer                    N   = Number of filter coefficients to compute
!                                  (i.e., order of the system of equations
!                                  to solve). N > 1 required.
! real                       A   = Array (length N) of filter coefficients
!                                  returned by OPFILT.
! real                       B   = Array (length N) containing
!                                  cross-correlation of the input data with
!                                  the desired output.
! real                       C   = SCRATCH array of length at least 2*N words.
! real                       R   = Array (length N) containing the
!                                  autocorrelation of the input data.
!
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
!  9. 2006-06-20  B. Menger    Removed Unused Variables.
!  8  2005-10-10  Goodger      Changed argument C from intent out to
!                              intent inout to satisfy absoft 9.0 compiler.
!  7. 2001-04-26  Selzler      Corrected closing header_word_doc tag.
!  6. 2000-07-07  Selzler      Fixed problems found by CPS Fortran Code Review.
!  5. 2000-01-25  Selzler      Clean up trailing blanks and block labels
!  4. 1999-11-12  Selzler      Conversion to Fortran 90.
!  3. 1999-01-07  Goodger      Begin using the fortran90 compiler.
!  2. 1987-10-30  Tom Hill     Eliminated 2*N calls to SDOT, 2*N divides,
!                              2*N IF statements and N/2 branches.  Preserves
!                              original arithmetic but runs in 77% time of
!                              old version (more dramatic improvement for
!                              very short vectors).
!  1. 1980-03-20  Mike Ess     Removed subroutine calls to CSOLV, ASOLV and
!                              SAXPY and substituted inline code.
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


      module opfilt_module
      implicit none

      private
      public :: opfilt

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      character(len=100),public :: rcsum_ident = &
        "$Id: opfilt.f90,v 1.9 2006/06/20 13:12:02 Menger prod sps $"

      contains

!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

      SUBROUTINE OPFILT(N, A, B, C, R)
      IMPLICIT NONE

      INTEGER, INTENT(IN) :: N
      REAL, dimension(n), intent(out)  :: A
      REAL, dimension(n), INTENT(IN) :: B
      REAL, dimension(2*n), intent(inout), target  :: C
      REAL, dimension(n), intent(in)  :: R

      INTEGER :: M, MP1, MP2   , NM1, NM2 
      REAL, DIMENSION(:), pointer :: D
      REAL :: R1INV, SCRI, D1, SDRI, SA, C1

      d => c(n+1:)
!
!  FIRST RECURSIVE STEPS
!
      R1INV = 1.0/R(1)
      A(1) = B(1)*R1INV
      C(1) = R(2)*R1INV
      SCRI = 1.0/(R(1)-C(1)*R(2))
      A(2) = (B(2)-A(1)*R(2))*SCRI
      A(1) = A(1) - A(2)*C(1)
!
!  NEXT LOOP PROVIDES ITERATIONS ON THE FILTER LENGTH (2 PER PASS)
!
      DO M = 2, N - 2, 2
        MP1 = M + 1
        MP2 = MP1 + 1
!**ODD
        D(1) = (R(MP1)-DOT_PRODUCT(C(:M-1),R(2:M)))*SCRI
        D1 = D(1)
        D(2:M) = C(:M-1) - D1*C(MP1-2:MP1-M:(-1))
        SDRI = 1.0/(R(1)-DOT_PRODUCT(D(:M),R(M+1:2:-1)))
        A(MP1) = (B(MP1)-DOT_PRODUCT(A(:M),R(M+1:2:-1)))*SDRI
        SA = A(MP1)
        A(:M) = A(:M) - SA*D(:M)
!**EVEN
        C(1) = (R(MP2)-DOT_PRODUCT(D(:M),R(2:M+1)))*SDRI
        C1 = C(1)
        C(2:MP1) = D(:MP1-1) - C1*D(MP2-2:MP2-MP1:(-1))
        SCRI = 1.0/(R(1)-DOT_PRODUCT(C(:MP1),R(MP1+1:2:-1)))
        A(MP2) = (B(MP2)-DOT_PRODUCT(A(:MP1),R(MP1+1:2:-1)))*SCRI
        SA = A(MP2)
        A(:MP1) = A(:MP1) - SA*C(:MP1)
      END DO
!
!      FINAL ITERATION IF N IS ODD
!
      if(mod(n,2) == 1) then
        NM1 = N - 1
        NM2 = NM1 - 1
        D(1) = (R(N)-DOT_PRODUCT(C(:NM2),R(2:NM2+1)))*SCRI
        D1 = D(1)
        D(2:NM1) = C(:NM1-1) - D1*C(N-2:N-NM1:(-1))
        A(N) = (B(N)-DOT_PRODUCT(A(:NM1),R(NM1+1:2:-1))) / &
               (R(1)-DOT_PRODUCT(D(:NM1),R(NM1+1:2:-1)))
        SA = A(N)
        A(:NM1) = A(:NM1) - SA*D(:NM1)
      ENDIF
      RETURN
      END SUBROUTINE OPFILT

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module opfilt_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
