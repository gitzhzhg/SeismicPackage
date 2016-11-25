!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- least.f90 --------------------------------!!
!!------------------------------- least.f90 --------------------------------!!
!!------------------------------- least.f90 --------------------------------!!


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
! Name       : LEAST
! Category   : math
! Written    : 2000-12-18   by: Tom Stoeckley
! Revised    : 2008-12-11   by: Bill Menger
! Maturity   : beta
! Purpose    : Least squares solver.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
!      ELEGANT LEAST SQUARES SOLVER, DEVELOPED FROM PROGRAM WULSQR
!      AND FUNCTION NECSD, BOTH SUPPLIED BY SLAVEK RUCINSKI TO TOM
!      STOECKLEY IN 1976.
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
!                                   o    i
!            call least_create    (obj,nterms)
!
!                                   b    i        i        i
!            call least_add_point (obj,terms,observation,weight)
!
!                                   b    o     o      o
!            call least_delete    (obj,coefs,ecoefs,sigma)
!
!
! type(least_struct)           obj = pointer to the LEAST data structure.
! integer                   nterms = number of unknown coefficients (no limit).
! double precision   terms(nterms) = multipliers of each coefficient.
! double precision     observation = observed value.
! double precision          weight = weight for observed value.
! double precision   coefs(nterms) = coefficients to be solved for.
! double precision  ecoefs(nterms) = errors of each coefficient.
! double precision           sigma = standard deviation of solution.
!
! LEAST_CREATE:
!  (1) allocate the data structure.
!
! LEAST_ADD_POINT:
!  (1) add one observation point.
!  (2) call this routine for each observation point.
!  (3) TERMS and OBSERVATION values equal to DNIL are treated as exactly zero.
!  (4) points with WEIGHT == zero or WEIGHT == DNIL are ignored.
!  (5) points with negative WEIGHT are used.
!
! LEAST_DELETE:
!  (1) get least squares results.
!  (2) deallocate the data structure.
!  (3) if an error occurs, all output arguments are set to DNIL.
!  (4) if COEF(i) cannot be derived, COEF(i) and ECOEF(i) are set to DNIL.
!
! Example:  To solve  Z = A + B*X*Y + C*X  for  COEFS = A, B, C
!           set  TERMS = 1, X*Y, X  for each observation  Z .
!
! Note: If Y above were zero for all observations, it would not be possible
!       to solve for B because the multiplier of B would always be zero.
!       In this case, B would be set to DNIL and the following equation
!       would be solved instead:   Z = A + C*X
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                            ADVICE FOR USERS             
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  3. 2008-12-11  Bill Menger Nullified pointers.
!  2. 2002-04-11  Stoeckley  Add special treatment for nil values; add
!                             ability to get partial solution if some
!                             coefficients cannot be found.
!  1. 2000-12-18  Stoeckley  Initial version, taken from math_util.f in SPWS.
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module least_module
      use named_constants_module
      implicit none
      public
      private :: least_private_reduce
      private :: least_private_solve

      character(len=100),public,save :: LEAST_IDENT = &
'$Id: least.f90,v 1.2 2002/04/10 18:00:39 Stoeckley prod sps $'


      type,public :: least_struct              
         private
         integer                  :: n,ncoef
         double precision,pointer :: matrix(:,:)
         logical         ,pointer :: active(:)   ! whether each term is active.
      end type least_struct

      double precision,private,parameter :: ZERO = 0.0d0
      double precision,private,parameter :: ONE  = 1.0d0

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine least_create (obj,nterms)
      implicit none
      type(least_struct),pointer    :: obj             ! arguments
      integer           ,intent(in) :: nterms          ! arguments

      nullify(obj)
      allocate (obj)
      nullify(obj%matrix)
      nullify(obj%active)
      allocate (obj%matrix(nterms+1,nterms+1))
      allocate (obj%active(nterms))
      obj%n           = 0
      obj%ncoef       = nterms
      obj%matrix(:,:) = ZERO
      obj%active(:)   = .false.
      return
      end subroutine least_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      SUBROUTINE LEAST_DELETE (obj,coefs,ecoefs,sigma)
      implicit none
      type(least_struct),pointer       :: obj            ! arguments
      double precision  ,intent(out)   :: coefs (:)      ! arguments
      double precision  ,intent(out)   :: ecoefs(:)      ! arguments
      double precision  ,intent(out)   :: sigma          ! arguments

      call LEAST_private_reduce (obj,coefs,ecoefs,sigma)
      deallocate (obj%matrix)
      deallocate (obj%active)
      deallocate (obj)
      RETURN
      END SUBROUTINE LEAST_DELETE


!!----------------------------- add point ---------------------------------!!
!!----------------------------- add point ---------------------------------!!
!!----------------------------- add point ---------------------------------!!


      SUBROUTINE LEAST_ADD_POINT (obj,TERMS,observation,WEIGHT)
      implicit none
      type(least_struct),intent(inout) :: obj                  ! arguments
      DOUBLE PRECISION  ,intent(in)    :: TERMS(:)             ! arguments
      double precision  ,intent(in)    :: observation,weight   ! arguments
      integer                          :: J,K,NC,NC1           ! local

      if (weight == DNIL .or. weight == ZERO) return
      NC    = obj%ncoef
      NC1   = NC + 1
      obj%N = obj%N + 1

      DO J = 1,NC
           if (terms(J) == DNIL .or. terms(J) == ZERO) cycle
           obj%active(J) = .true.

           DO K = J,NC
                if (terms(K) == DNIL .or. terms(K) == ZERO) cycle
                obj%MATRIX(J,K) = obj%MATRIX(J,K) + TERMS(J)*TERMS(K)*weight
           end do

           if (observation == DNIL .or. observation == ZERO) cycle
           obj%MATRIX(J,NC1) = obj%MATRIX(J,NC1) + TERMS(J)*observation*weight
      end do

      if (observation == DNIL .or. observation == ZERO) return
      obj%MATRIX(NC1,NC1) = obj%MATRIX(NC1,NC1) + observation*observation*weight
      RETURN
      END SUBROUTINE LEAST_ADD_POINT


!!--------------------------- private reduce -------------------------------!!
!!--------------------------- private reduce -------------------------------!!
!!--------------------------- private reduce -------------------------------!!

! remove rows and columns associated with inactive terms before solving.


      SUBROUTINE LEAST_private_reduce (obj,coefs,ecoefs,sigma)
      implicit none
      type(least_struct),intent(inout) :: obj                 ! arguments
      double precision  ,intent(out)   :: coefs (:)           ! arguments
      double precision  ,intent(out)   :: ecoefs(:)           ! arguments
      double precision  ,intent(out)   :: sigma               ! arguments
      integer                          :: J,NC,NC1            ! local
      integer                          :: jnew,ncnew          ! local

      NC    = obj%ncoef
      NC1   = NC + 1
      jnew  = 0
      ncnew = nc

      DO J = 1,NC
           if (obj%active(j)) then
                jnew = jnew + 1
           else
                obj%matrix(jnew+1,1:nc1) = obj%matrix(j+1,1:nc1)
                obj%matrix(1:nc1,jnew+1) = obj%matrix(1:nc1,j+1)
                ncnew = ncnew - 1
           end if
      end do

      if (ncnew < nc) then
           obj%matrix(ncnew+1,1:nc1) = obj%matrix(nc1,1:nc1)
           obj%matrix(1:nc1,ncnew+1) = obj%matrix(1:nc1,nc1)
      end if

      obj%ncoef = ncnew

      call LEAST_private_solve (obj,coefs,ecoefs,sigma)

      jnew = ncnew

      DO J = NC,1,-1
           if (obj%active(j)) then
                coefs (j) = coefs (jnew)
                ecoefs(j) = ecoefs(jnew)
                jnew = jnew - 1
           else
                coefs (j) = DNIL
                ecoefs(j) = DNIL
           end if
      end do

      RETURN
      END SUBROUTINE LEAST_private_reduce


!!--------------------------- private solve --------------------------------!!
!!--------------------------- private solve --------------------------------!!
!!--------------------------- private solve --------------------------------!!


      SUBROUTINE LEAST_private_solve (obj,coefs,ecoefs,sigma)
      implicit none
      type(least_struct),intent(inout) :: obj               ! arguments
      double precision  ,intent(out)   :: coefs (:)         ! arguments
      double precision  ,intent(out)   :: ecoefs(:)         ! arguments
      double precision  ,intent(out)   :: sigma             ! arguments
      integer                          :: J,K,L,JJ,NC,NC1   ! local
      DOUBLE PRECISION                 :: SSS,WWW,HHH       ! local

!----------INITIALIZE OUTPUT ARGUMENTS.

      NC  = obj%ncoef
      NC1 = NC + 1

      coefs (1:NC) = DNIL
      ecoefs(1:NC) = DNIL
      sigma        = DNIL

      if (obj%n < obj%ncoef) return

!----------FIRST STEP OF ARRAY OPERATIONS.

      DO J = 1,NC1
           DO K = J,NC1
                SSS = obj%MATRIX(J,K)
                IF (J > 1) then
                     JJ = J-1
                     DO L = 1,JJ
                          SSS = SSS - obj%MATRIX(L,J) * obj%MATRIX(L,K)
                     end do
                end if
                IF (K /= J) then
                     obj%MATRIX(J,K) = SSS/HHH
                else if (SSS <= ZERO) then
                     IF (J /= NC1) return
                     HHH             = ZERO
                     obj%MATRIX(J,J) = HHH
                else
                     HHH             = DSQRT(SSS)
                     obj%MATRIX(J,J) = HHH
                end if
           end do
      end do

!----------SECOND STEP OF ARRAY OPERATIONS.

      DO J = 1,NC
           HHH = obj%MATRIX(J,J)
           DO K = 1,J
                if (K == J) then
                     obj%MATRIX(J+1,K) = ONE/HHH
                else
                     SSS = ZERO
                     JJ = J-1
                     DO L = K,JJ
                          SSS = SSS - obj%MATRIX(L,J) * obj%MATRIX(L+1,K)
                     end do
                     obj%MATRIX(J+1,K) = SSS/HHH
                end if
           end do
      end do

!----------GET RESULTS FOR COEF,ECOEF,SIGMA.

      sigma = obj%MATRIX(NC1,NC1) / SQRT(AMAX0(obj%N-NC,1))
      DO J = 1,NC
           SSS = ZERO
           WWW = ZERO
           DO L = J,NC
                SSS = SSS + obj%MATRIX(L,NC1) * obj%MATRIX(L+1,J)
                WWW = WWW + obj%MATRIX(L+1,J)**2
           end do
           coefs (J) = SSS
           ecoefs(J) = DSQRT(WWW)*sigma
      end do
      RETURN
      END SUBROUTINE LEAST_private_solve


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module least_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

