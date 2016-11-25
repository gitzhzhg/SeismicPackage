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
! Name       : cvptom  (ConVert Velocity Primaries TO Multiple Velocity)
! Category   : velocity
! Written    : 1987-11-30   by: JB Sinton
! Revised    : 2001-03-13   by: Randy Selzler, Data-Warp, Inc.
! Maturity   : production   2001-04-30
! Purpose    : To convert primary velocities to multiple velocities using a
!              stacking responce criteria.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION
!
!  CVPTOM Subroutine: Convert one function.
!
!  CVPTOM_CREATE Subroutine: create and setup CVPTOM data structure.
!
!  You must call CVPTOM_CREATE first to pass the control parameters and set
!  up several internal variable.  Then you must call CVPTOM for each
!  function that you want to convert.  Original velocity function is
!  over written by the new function.
!
!  See programmers doc for more information on calling arguments.
!
!  1. Note that CVPTOM assumes that the velocity array is gridded in
!     time.
!  2. Some useful values of passed arguments are:
!     RATIO =
!     FMN   = 20 hz.
!     VMN   = 4000 ft/sec.
!     ZM    = 0.0 ft.
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
!                i   i  b  i  i o
! CALL CVPTOM  (obj,NVP,V,T0,DT,W)
!
! cvptom_struct              obj     = structure for precomputed constants
! integer                    NVP     = Number of values in velocity array V.
! real                       V       = Array of velocity values.
!                                      Dimension NVP.
! real                       T0      = Time at V(1)
! real                       DT      = Time increment between successive
!                                      velocity values.
! real                       W       = Work array dimensioned at least 3*NVP.
!
!
!                      o    i    i    i     i    i   i  i  i  i
! CALL CVPTOM_CREATE (obj,RATIO,OMAX,OMIN,NFOLD,FMN,AM,VM,ZM,VMN)  Setup.
!
! cvptom_struct              obj     = structure for precomputed constants
! real                       RATIO   = Ratio of primary stack with DE-MULT to
!                                      stack without DE-MULT.  (0 < RATIO < 1)
! real                       OMAXI   = Maximum offset in data.
! real                       OMINI   = Minimum offset in data.
! integer                    NFOLD   = Fold of stack.  OMAX, OMIN, and NFOLD
!                                      are used to compute the offset
!                                      increment, OINC = (OMAX-OMIN)/(NFOLD+1).
! real                       FMN     = Mean frequency in data.
! real                       AM      = AMUT parameter in CPS MUTE process
!                                      (time units).
! real                       VM      = VMUT parameter in CPS MUTE process
!                                      (velocity units)
! real                       ZM      = Minimum offset out of mute calculation
!                                      (should be 0.0).
! real                       VMN     = Minimum converted velocity, ie. V array
!                                      will contain no velocities less than VMN.
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
! 10. 2001-04-30  Selzler     Corrected closing header_word_doc tag.
!  9. 2000-07-07  Selzler     Fixed problems found by CPS Fortran Code Review.
!  8. 2000-01-25  Selzler     Clean up trailing blanks and block labels
!  7. 1999-11-19  Selzler     Corrected some documentation.
!  6. 1999-10-27  Selzler     Added RCS "Id" strings to tag executeable
!  5. 1999-10-05  Selzler     Conversion to F90
!  4. 1999-01-25  Goodger     Begin using the fortran90 compiler.
!  3. 1996-06-21  Vunderink   Added ZMUT to SAVE list
!  2. 1988-05-30  JB Sinton   Changed so that calculation of parameter
!                             BETA2 is consistant with CONSEIS version.
!  1. 1987-11-30  JB Sinton   First version.
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

      module cvptom_module
      implicit none

      private
      public :: cvptom
      public :: cvptom_create
      public :: cvptom_delete

!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!

      type,public :: cvptom_struct
      private
        real :: fmean
        real :: amut
        real :: vmut
        real :: zmut
        real :: vmin
        real :: omax
        real :: omin
        real :: beta2
        real :: oinc
        integer :: n2
        integer :: n1
        integer :: n
        real :: avl
      end type cvptom_struct

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      character(len=100),public :: cvptom_ident = &
        "$Id: cvptom.f90,v 1.10 2001/04/26 15:48:47 sps prod sps $"

      contains

!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!

      subroutine CVPTOM_CREATE (obj, RATIO, OMAXI, OMINI, NFOLD, &
        FMN, AM, VM, ZM, VMN)
      implicit none

      type(cvptom_struct),pointer :: obj    ! arguments
      REAL,    INTENT(IN) :: RATIO
      REAL,    INTENT(IN) :: OMAXI
      REAL,    INTENT(IN) :: OMINI
      INTEGER, INTENT(IN) :: NFOLD
      REAL,    INTENT(IN) :: FMN
      REAL,    INTENT(IN) :: AM
      REAL,    INTENT(IN) :: VM
      REAL,    INTENT(IN) :: ZM
      REAL,    INTENT(IN) :: VMN

      real, dimension(11), parameter :: BETA = &
        (/0.0 ,.06 ,.11 ,.16 ,.205 ,.26 ,.333 ,.51 ,1.0,1.0,1.0 /)

      integer index

      allocate(obj)

      obj%FMEAN = FMN
      obj%AMUT = AM
      obj%VMUT = VM
      obj%ZMUT = ZM
      obj%VMIN = VMN
      obj%OMAX = OMAXI
      obj%OMIN = OMINI
      INDEX = IFIX(RATIO*10.)
      obj%BETA2 = BETA(INDEX) + (RATIO*10. - INDEX) * &
        (BETA(INDEX+1)-BETA(INDEX))
      obj%OINC = (obj%OMAX - obj%OMIN)/(NFOLD + 1)
      obj%N2 = NINT(obj%OMAX/obj%OINC)
      obj%N1 = NINT(obj%OMIN/obj%OINC)
      obj%N = obj%N2 - obj%N1 + 1
      obj%AVL = obj%OINC**2/6/obj%N*(obj%N2*(obj%N2 + 1)*(2*obj%N2 + 1) - &
        obj%N1*(obj%N1 - 1)*(2*obj%N1 - 1))
      obj%AVL = obj%AVL*obj%FMEAN

      return
      end subroutine cvptom_create

!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!

      subroutine cvptom_delete (obj)
      implicit none

      type(cvptom_struct),pointer :: obj    ! arguments

      deallocate(obj)

      return
      end subroutine cvptom_delete

!!----------------------------- cvptom -------------------------------!!
!!----------------------------- cvptom -------------------------------!!
!!----------------------------- cvptom -------------------------------!!

      SUBROUTINE CVPTOM(obj, NVP, V, T0, DT, W)
      IMPLICIT NONE

      type(cvptom_struct),pointer :: obj    ! arguments
      INTEGER, INTENT(IN)    :: NVP
      REAL,    INTENT(INOUT) :: V(*)
      REAL,    INTENT(IN)    :: T0
      REAL,    INTENT(IN)    :: DT
      REAL,    INTENT(INOUT) :: W(*)

      INTEGER :: I1, I2, I, NO
      REAL :: OI2, RED

      I1 = NVP
      I2 = NVP*2
      OI2 = obj%OINC**2

      !************************************************
      !**** W(I) is the max offset using AMUT & VMUT.
      !**** W(I+I1) is the travel time.
      !**** W(I+I2) is the average offset**2.
      !************************************************

      DO I = 1, NVP
        W(I+I1) = T0 + (I - 1)*DT
        W(I) = MAX((W(I+I1)-obj%AMUT)*obj%VMUT,obj%ZMUT)

        IF (W(I) > obj%OMAX) THEN
          W(I+I2) = obj%AVL
        ELSE
          NO = MAX(obj%N1 + 1,1 + NINT(W(I)/obj%OINC))
          RED = OI2/6*(obj%N2*(obj%N2 + 1)*(2*obj%N2 + 1) - &
            NO*(NO + 1)*(2*NO + 1))*obj%FMEAN
          W(I+I2) = 1.0/(NO - obj%N1)*(obj%N*obj%AVL - RED)
        ENDIF
      END DO

      DO I = 1, NVP
        IF (W(I) <= obj%OMIN) THEN
          V(I) = obj%VMIN
        ELSE
          V(I) = MAX(obj%VMIN, &
            V(I)*SQRT(W(I+I2)/(2.*W(I+I1)*obj%BETA2*V(I)**2+W(I+I2))))
        ENDIF
      END DO

      !************************************************
      !**** W(I) is the max offset using AMUT & VMUT.
      !**** W(I+I1) is the travel time.
      !**** W(I+I2) is the average offset**2.
      !************************************************

      RETURN

      end subroutine cvptom

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module cvptom_module

!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
