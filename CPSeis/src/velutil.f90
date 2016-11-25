!<CPS_v1 type="PRIMITIVE"/>
!!----------------------------- velutil.f90 --------------------------------!!
!!----------------------------- velutil.f90 --------------------------------!!
!!----------------------------- velutil.f90 --------------------------------!!

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
! Name       : VELUTIL
! Category   : velocity
! Written    : 1989-06-13   by: Tom Stoeckley
! Revised    : 2006-04-25   by: B. Menger
! Maturity   : production
! Purpose    : General velocity function manipulation utility.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive contains several utilities for manipulating velocity
! functions.  Currently, only one utility is provided to generate several
! types of velocity functions from one input type.  Other utilities will be
! added as needed.  Many such utilities reside in the Motif program VA and
! other locations.
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
!             GENERATE SEVERAL TYPES OF VELOCITY FUNCTIONS
!                      (two equivalent interfaces)
!                                                            
!                             i      i    i i    o     o    o    o   o    o
!   CALL VELUTIL_GENERATE (veltype,npicks,X,V,  TIME,DEPTH,VRMS,VAV,VINT,IERR)
!
!                             i      i    i i       i       o    o        o
!   CALL VELUTIL_CONVERT  (veltype,npicks,X,V,  veltypeout,xout,vout,    IERR)
!
! char*4    veltype        = type of input  velocity function (see below).
! char*4    veltypeout     = type of output velocity function (see below).
! integer   npicks   >=2   = number of velocity picks (>=2).
! real      X     (npicks) = abscissae of input  velocity function.
! real      V     (npicks) = ordinates of input  velocity function.
! real      XOUT  (npicks) = abscissae of output velocity function.
! real      VOUT  (npicks) = ordinates of output velocity function.
! real      TIME  (npicks) = array of times.
! real      DEPTH (npicks) = array of depths.
! real      VRMS  (npicks) = array of RMS velocities.
! real      VAV   (npicks) = array of average velocities.
! real      VINT  (npicks) = array of interval velocities.
! integer   IERR           = error condition.  Set to VELUTIL_ERROR if an
!                             error occurs during conversion.  Otherwise set
!                             to VELUTIL_OK.  See ADVICE FOR USERS.
!
! This subroutine supports only the following velocity function types (see
! the VELOCITY FUNCTION TYPES section below):
!                         VTRM  VTAV  VTIN
!                         VZRM  VZAV  VZIN
! In addition, type VTNM is accepted, but treated as if it were type VTRM.
! That is, a stacking velocity function is treated as if it were an RMS
! velocity function.  The reason for a separate VTNM type is to facilitate
! different treatments of VTRM and VTNM types in other contexts.
!
! If an error occurs upon conversion, then (in addition to setting IERR to
! VELUTIL_ERROR) some of the output values will be set to zero.  All output
! values can be printed, which means that IERR need not be interrogated if
! the user intends to use the converted velocities for a display only. 
!
! A conversion error (causing IERR to be set to VELUTIL_ERROR) will occur if:
!    (a) VELTYPE is invalid.
!    (b) Depth or time is negative.
!    (b) Layer thickness is zero or negative.
!    (c) Depth or time is not monotonically increasing.
!    (d) Velocity is zero or negative.
!    (e) Conversion from RMS or NMO velocity causes a negative
!            argument for the SQRT function.
!
! This primitive does not require that the first time (or depth) pick be zero.
! It is permissible for the first pick to correspond to the bottom of the first
! layer.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                       VELOCITY FUNCTION TYPES
!
! The type of velocity function should be one of these string values:
!
!          veltype             ordinates           abscissae
!          -------             ---------           ---------
!          'VTNM'          stacking velocity       2-way time
!          'VTRM'               RMS velocity       2-way time
!          'VTAV'           average velocity       2-way time
!          'VTIN'          interval velocity       2-way time
!          'VTDP'                depth             2-way time
!          'VZRM'               RMS velocity          depth
!          'VZAV'           average velocity          depth
!          'VZIN'          interval velocity          depth
!          'VLRM'               RMS velocity     layer thickness
!          'VLAV'           average velocity     layer thickness
!          'VLIN'          interval velocity     layer thickness
!
! Note that the velocity function types begin with the characters
! VT or VZ or VL, depending on whether the picks are versus time or
! depth or layer thickness.  Also note that there is no allowance for
! NMO (stacking) velocity versus depth or layer thickness.
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2006-04-25  B. Menger    Removed Unused Variables.
!  4. 2000-10-20  Stoeckley    Remove tab character and add missing required
!                               documentation section.
!  3. 2000-02-08  Stoeckley    Converted from VELGEN in the old system.
!  2. 1999-01-25  Goodger      Begin using the fortran90 compiler.
!  1. 1989-06-13  Stoeckley    Initial version.
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


      module velutil_module
      implicit none

      character(len=100),public,save :: VELUTIL_IDENT = &
       '$Id: velutil.f90,v 1.5 2006/04/25 13:24:30 Menger prod sps $'

      integer,parameter,public :: VELUTIL_OK    = 0
      integer,parameter,public :: VELUTIL_ERROR = 1

      contains


!!------------------------ velutil generate -------------------------------!!
!!------------------------ velutil generate -------------------------------!!
!!------------------------ velutil generate -------------------------------!!


      SUBROUTINE VELUTIL_GENERATE   &
                    (veltype,npicks,X,V,   TIME,DEPTH,VRMS,VAV,VINT,IERR)
      implicit none
      character(len=*),intent(in)  :: veltype                 ! arguments
      integer         ,intent(in)  :: npicks                  ! arguments
      real            ,intent(in)  :: X(:),V(:)               ! arguments
      real            ,intent(out) :: TIME(:),DEPTH(:)        ! arguments
      real            ,intent(out) :: VRMS(:),VAV(:),VINT(:)  ! arguments
      integer         ,intent(out) :: ierr                    ! arguments
      integer                      ::   k,l ! local
      real                         :: aaaa,bbbb,cccc          ! local
      real                         :: abcd,delta              ! local

!----------GET STARTED.

      IERR    = VELUTIL_OK
      VRMS(1) = V(1)
      VAV (1) = V(1) 
      VINT(1) = V(1)

!----------GET THE TIME FOR THE FIRST PICK.

      IF (V(1) <= 0.0 .OR. X(1) < 0.0) THEN
           IERR    = VELUTIL_ERROR
           TIME(1) = 0.0
      ELSE IF (veltype(1:2) == 'VT') THEN
           TIME(1) = X(1) 
      ELSE IF (veltype(1:2) == 'VZ') THEN
           TIME(1) = 2.0 * X(1) / V(1) 
      ELSE
           IERR    = VELUTIL_ERROR
           TIME(1) = 0.0
      END IF

!----------FINISH UP WITH THE FIRST PICK.

      DEPTH(1) = V(1) * TIME(1) / 2.0
      IF (npicks <= 1) THEN
           IERR = VELUTIL_ERROR
           RETURN
      END IF

!----------GO THROUGH THE LOOP. 

      DO K = 2, npicks 
           L = K - 1 

!----------GET THE TIME.

           IF (V(K) <= 0.0) THEN
                IERR    = VELUTIL_ERROR
                TIME(K) = 0.0
           ELSE IF (veltype(1:2) == 'VT') THEN
                TIME(K) = X(K) 
           ELSE IF (veltype == 'VZIN') THEN
                TIME(K) = TIME(L) + 2.0 * (X(K) - X(L)) / V(K) 
           ELSE IF (veltype == 'VZAV') THEN
                TIME(K) = 2.0 * X(K) / V(K) 
           ELSE IF (veltype == 'VZRM') THEN
                AAAA = V(K)**2
                BBBB = -TIME(L) * (V(K)**2 + V(L)**2) / 2.0
                CCCC = TIME(L)**2 * V(L)**2 / 4.0 - (X(K) - X(L))**2
                ABCD = BBBB**2 - 4.0 * AAAA * CCCC
                IF (ABCD >= 0.0) THEN
                     TIME(K) = 2.0 * (-BBBB + SQRT(ABCD)) / (2.0 * AAAA)
                ELSE 
                     IERR    = VELUTIL_ERROR
                     TIME(K) = 0.0
                END IF
           ELSE 
                IERR    = VELUTIL_ERROR
                TIME(K) = 0.0
           END IF
           DELTA = TIME(K) - TIME(L) 

!----------GET THE INTERVAL VELOCITY.

           IF (DELTA <= 0.0) THEN
                IERR    = VELUTIL_ERROR
                VINT(K) = 0.0
           ELSE IF (veltype(3:4) == 'IN') THEN
                VINT(K) = V(K)
           ELSE IF (veltype(3:4) == 'AV') THEN
                VINT(K) = (V(K) * TIME(K) - V(L) * TIME(L)) / DELTA
           ELSE IF (veltype(3:4) == 'RM' .OR. veltype(3:4) == 'NM') THEN
                ABCD = V(K)**2 * TIME(K) - V(L)**2 * TIME(L)
                IF (ABCD > 0.0) THEN
                     VINT(K) = SQRT(ABCD / DELTA)      
                ELSE
                     IERR    = VELUTIL_ERROR
                     VINT(K) = 0.0
                END IF
           ELSE
                IERR    = VELUTIL_ERROR
                VINT(K) = 0.0
           END IF

!----------GET RMS VELOCITY AND AVERAGE VELOCITY AND DEPTH.

           IF (DELTA <= 0.0 .OR. TIME(K) <= 0.0 .OR. TIME(L) < 0.0) THEN
                IERR     = VELUTIL_ERROR
                VRMS (K) = 0.0
                VAV  (K) = 0.0
                DEPTH(K) = 0.0
           ELSE
                VRMS (K) = SQRT((VRMS(L)**2 * TIME(L) + VINT(K)**2 * DELTA) &
                                                                / TIME(K))
                VAV  (K) = (VAV(L) * TIME(L) + VINT(K) * DELTA) / TIME(K) 
                DEPTH(K) = DEPTH(L) + VINT(K) * DELTA / 2.0
           END IF
      END DO
      RETURN
      END SUBROUTINE VELUTIL_GENERATE


!!----------------------------- velutil convert ---------------------------!!
!!----------------------------- velutil convert ---------------------------!!
!!----------------------------- velutil convert ---------------------------!!


      SUBROUTINE VELUTIL_CONVERT   &
                    (veltype,npicks,X,V,  veltypeout,xout,vout,    IERR)
      implicit none
      character(len=*),intent(in)  :: veltype,veltypeout          ! arguments
      integer         ,intent(in)  :: npicks                      ! arguments
      real            ,intent(in)  :: X(:),V(:)                   ! arguments
      real            ,intent(out) :: Xout(:),Vout(:)             ! arguments
      integer         ,intent(out) :: ierr                        ! arguments
      real                         :: TIME(npicks),DEPTH(npicks)  ! local
      real                         :: VRMS(npicks),VAV(npicks)    ! local
      real                         :: VINT(npicks)                ! local

      call VELUTIL_GENERATE (veltype,npicks,X,V,  TIME,DEPTH,VRMS,VAV,VINT,IERR)
      select case (veltypeout)
        case ('VTNM')
          xout(1:npicks) = time (1:npicks)
          vout(1:npicks) = vrms (1:npicks)
        case ('VTRM')
          xout(1:npicks) = time (1:npicks)
          vout(1:npicks) = vrms (1:npicks)
        case ('VTAV')
          xout(1:npicks) = time (1:npicks)
          vout(1:npicks) = vav  (1:npicks)
        case ('VTIN')
          xout(1:npicks) = time (1:npicks)
          vout(1:npicks) = vint (1:npicks)
        case ('VZRM')
          xout(1:npicks) = depth(1:npicks)
          vout(1:npicks) = vrms (1:npicks)
        case ('VZAV')
          xout(1:npicks) = depth(1:npicks)
          vout(1:npicks) = vav  (1:npicks)
        case ('VZIN')
          xout(1:npicks) = depth(1:npicks)
          vout(1:npicks) = vint (1:npicks)
        case default
          xout(1:npicks) = 0.0
          vout(1:npicks) = 0.0
          ierr = VELUTIL_ERROR
      end select
      return
      end subroutine velutil_convert


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module velutil_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

