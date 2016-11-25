!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- eureka.f90 --------------------------------!!
!!------------------------------- eureka.f90 --------------------------------!!
!!------------------------------- eureka.f90 --------------------------------!!
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
! Name       : eureka 
! Category   : math 
! Written    : 2003-04-01   by: Sam H. Bickel
! Revised    : 2005-01-31   by: Michael Ried
! Maturity   : production
! Purpose    : Finds the solutions of single-channel normal equations which
!              arise in least-squares filtering and prediction problems for
!              single-channel time series.
!
! Portability: No known limitations.
!
!!!  --> Choose the category from this list of subdirectories:
!!!
!!!      math 
!!!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! Purpose    : Finds the solutions of single-channel normal equations which
!              arise in least-squares filtering and prediction problems for
!              single-channel time series.
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
!!!                            i      i     i    o     o
!!!              call eureka (lfltr, acor, rhs, fltr, err)
!!!
!!!
!!! integer                    lfltr   =    --> Length of the filter
!!! real                       acor(:) =    --> Autocorrelation coefficients
!!! real                       rhs(:)  =    --> Right hand side coefficients
!!! real                       fltr(:) =    --> Filter coefficients
!!! real                       err(:)  =    --> Prediction error operator
!!!
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author        Description
!     ----        ------        -----------
! 3.  2005-01-31  Michael Ried  Convert to CPS (Fortran 90)
! 2.  1992-11-10  ESN           Convert to ProMAX.
! 1.  1980-09-01  Sam Bickel    Initial version
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
!
      MODULE EUREKA_MODULE
!
      IMPLICIT NONE
!
      PUBLIC
!
      character(len=100),public,save :: EUREKA_IDENT = &
'$Id: eureka.f90,v 1.3 2005/01/31 14:05:30 Ried prod sps $'
!
      contains
!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!
!----------------------------------------------------------------------
!A      Author       Sam H. Bickel
!A      Designer     Sam H. Bickel
!A      Language     FORTRAN
!A      Written      September, 1980
!       Revised      11-10-92  ESN. Convert to ProMAX.
!A
!A
!A      CALL EUREKA (LFLTR, ACOR, RHS, ERR, FLTR)
!A
!A      IN/OUT  ARGUMENT  TYPE  DESCRIPTION
!A
!A      IN      LFLTR     I4    Length of the filter
!A      IN      ACOR      R4    Autocorrelation coefficients
!A      IN      RHS       R4    Right hand side coefficients
!A      OUT     ERR       R4    Prediction error operator
!A      OUT     FLTR      R4    Filter coefficients
!A
!A
!A      Finds the solutions of single-channel normal equations which
!A      arise in least-squares filtering and prediction problems for
!A      single-channel time series.
!A
!----------------------------------------------------------------------

        SUBROUTINE EUREKA (LFLTR, ACOR, RHS, FLTR, ERR)
        IMPLICIT NONE
        INTEGER          ,INTENT(IN)    :: LFLTR     ! ARGUMENTS
        REAL             ,INTENT(IN)    :: ACOR(:)   ! ARGUMENTS
        REAL             ,INTENT(IN)    :: RHS(:)    ! ARGUMENTS
        REAL             ,INTENT(OUT)   :: FLTR(:)   ! ARGUMENTS
        REAL             ,INTENT(OUT)   :: ERR(:)    ! ARGUMENTS

        INTEGER  L, L1, L2, J, K, L3, I

        REAL     V, D, Q, HOLD

        V       = ACOR(1)
        D       = ACOR(2)
        ERR(1)  = 1.0
        FLTR(1) = RHS(1) / V
        Q       = FLTR(1) * ACOR(2)
        IF (LFLTR .EQ. 1) GO TO                                     70
!
        DO 60 L = 2, LFLTR
            ERR(L) = - D / V
            IF (L .EQ. 2) GO TO                                     30
            L1 = (L - 2) / 2
            L2 = L1 + 1
            IF (L2 .LT. 2) GO TO                                    20
!
            DO 10 J = 2, L2
                HOLD = ERR(J)
                K = L - J + 1
                ERR(J) = ERR(J) + ERR(L) * ERR(K)
                ERR(K) = ERR(K) + ERR(L) * HOLD
   10       CONTINUE
!
   20       IF (2*L1 .EQ. L-2) GO TO                                30
            ERR(L2+1) = ERR(L2+1) + ERR(L) * ERR(L2+1)
   30       V = V + ERR(L) * D
            FLTR(L) = (RHS(L) - Q) / V
            L3 = L - 1
!
            DO 40 J = 1, L3
                K = L - J + 1
                FLTR(J) = FLTR(J) + FLTR(L) * ERR(K)
   40       CONTINUE
!
            IF (L .EQ. LFLTR) GO TO                                 70
            D = 0.0
            Q = 0.0
!
            DO 50 I = 1, L
                K = L - I + 2
                D = D + ERR(I)  * ACOR(K)
                Q = Q + FLTR(I) * ACOR(K)
   50       CONTINUE
!
   60   CONTINUE

   70   RETURN
        END SUBROUTINE EUREKA
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
      END MODULE EUREKA_MODULE
