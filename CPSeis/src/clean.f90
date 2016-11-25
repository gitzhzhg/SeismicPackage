!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- clean.f90 --------------------------------!!
!!------------------------------- clean.f90 --------------------------------!!
!!------------------------------- clean.f90 --------------------------------!!

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
! Name       : clean 
! Category   : math
! Written    : 2000-09-07   by: Randy L. Selzler
! Revised    : 2004-08-23   by: Bill Menger
! Maturity   : production
! Purpose    : Test and repair IEEE float INF and NAN numbers.
! Portability: Assumes the architecture uses IEEE floating point format.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! These primitive functions test a floating point scalar or vector for
! IEEE INF and NAN values.
! If found, the values are replaced by 0.0.
! The function return value is the number of values replaced, if any.
!
! Calling processes should recompute the LAV.
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
! Examine each element of sp_vector.
! Replace IEEE NANs with zero.
! Clip values at + or - sp_magnitude.
! Return the number of elements changed (clipped or zeroed).
! Note: clipping also catches + and - IEEE INF.
!
!   o                     b            i
! clip_count = clean_clip(sp_vector, sp_magnitude)
!
!
! Examine each element of sp_vector.
! Replace IEEE NANs, +INF and -INF with zero.
! Return the number of elements changed (zeroed).
!
!   o                     b
! zero_count = clean_zero(sp_vector)
!
! Examine each element of sp_vector.
! Replace IEEE NANs, +INF and -INF with FNIL.
! Return the number of elements changed (FNIL'd)
!
!   o                     b
! fnil_count = clean_fnil(sp_vector)
!
! Examine elements of sp_vector.
! If any IEEE NAN, +INF or -INF is found, zero all elements in the vector.
! Return .true. iff an IEEE NAN, +INF or -INF is found.
!
!   o                     b
! kill_flag = clean_kill(sp_vector)
!
! integer           clip_count = number of bad values found and clipped.
! integer           zero_count = number of bad values found and zeroed.
! logical           kill_flag  = true iff vector was kill (>= 1 bad value)
!
!                   SP (single precision floating point version)
! real              sp_vector = vector value to test and repair.
! real              sp_magnitude = maximum magnitude to retain.
!                                  Note: sp_magnitude must be > 0.0
!-------------------------------------------------------------------------------
!</calling_doc>

!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author                 Description
!     ----        ------                 -----------
!  2. 2004-08-23  Bill Menger            Added clean_fnil function.
!  1. 2000-09-19  Randy L. Selzler       Initial version.
!
!-------------------------------------------------------------------------------
!</history_doc>


!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! Assumes the architecture uses IEEE floating point format.
!
! Assumes hosts byte swap 32 bit integers and real the same way.
!
!-------------------------------------------------------------------------------
!</portability_doc>


!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
!
! Care must be taken to avoid compiler optimizations that may defeat
! special tests for NAN.
!
!-------------------------------------------------------------------------------
!</compile_doc>


!<algorithm_doc>
!-------------------------------------------------------------------------------
!                   ALGORITHM DESCRIPTION FOR DEVELOPERS   
!
! IEEE NAN values are not equal to anything, including themselves.
!
!-------------------------------------------------------------------------------
!</algorithm_doc>

!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module clean_module
      use named_constants_module
      implicit none

      private
      public :: clean_fnil
      public :: clean_clip
      public :: clean_zero
      public :: clean_kill

      character(len=100),public,save :: clean_IDENT = &
'$Id: clean.f90,v 1.2 2004/08/23 13:15:22 Menger prod sps $'

!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      ! decimal +2139095040 = hex 7F80 0000 = IEEE +INF
      integer, parameter :: INT_INF_PLUS = +2139095040

      ! decimal -8388608 = hex FF80 0000 = IEEE -INF
      integer, parameter :: INT_INF_MINUS = -8388608

      ! decimal +8388607 = hex 007F FFFF = IEEE mantissa mask
      integer, parameter :: MASK_MANTISSA = +8388607

      contains

!!----------------------------- clean_clip -------------------------------!!
!!----------------------------- clean_clip -------------------------------!!
!!----------------------------- clean_clip -------------------------------!!

      function clean_clip(sp_vector, sp_magnitude)

      integer            :: clean_clip
      real, dimension(:) :: sp_vector
      real               :: sp_magnitude

! Examine each element of sp_vector.
! Replace IEEE NANs with zero.
! Clip values at + or - sp_magnitude.
! Return the number of elements changed (clipped or zeroed).
! Note: clipping also catches + and - IEEE INF.

      integer j

      real :: real_value
      integer :: int_value
      equivalence (real_value, int_value)

      clean_clip = 0

      do j = 1, size(sp_vector)
        real_value = sp_vector(j)

        if(iand(int_value, INT_INF_PLUS) == INT_INF_PLUS .and. &
           iand(int_value, MASK_MANTISSA) /= 0) then
          ! NAN detected
          sp_vector(j) = 0.0
          clean_clip = clean_clip + 1
        else if(real_value > + sp_magnitude) then
          sp_vector(j) = + sp_magnitude
          clean_clip = clean_clip + 1
        else if(real_value < - sp_magnitude) then
          sp_vector(j) = - sp_magnitude
          clean_clip = clean_clip + 1
        end if
      end do

      end function clean_clip

!!----------------------------- clean_zero -------------------------------!!
!!----------------------------- clean_zero -------------------------------!!
!!----------------------------- clean_zero -------------------------------!!

      function clean_zero(sp_vector)

      integer            :: clean_zero
      real, dimension(:) :: sp_vector

! Examine each element of sp_vector.
! Replace IEEE NANs, +INF and -INF with zero.
! Return the number of elements changed (zeroed).

      integer j

      real :: real_value
      integer :: int_value
      equivalence (real_value, int_value)

      clean_zero = 0

      do j = 1, size(sp_vector)
        real_value = sp_vector(j)

        if(iand(int_value, INT_INF_PLUS) == INT_INF_PLUS) then
          ! IEEE NAN, +INF, or -INF  detected
          sp_vector(j) = 0.0
          clean_zero = clean_zero + 1
        end if
      end do

      end function clean_zero

!!----------------------------- clean_fnil -------------------------------!!
!!----------------------------- clean_fnil -------------------------------!!
!!----------------------------- clean_fnil -------------------------------!!

      function clean_fnil(sp_vector)

      integer            :: clean_fnil
      real, dimension(:) :: sp_vector

! Examine each element of sp_vector.
! Replace IEEE NANs, +INF and -INF with fnil.
! Return the number of elements changed (FNIL'd).

      integer j

      real :: real_value
      integer :: int_value
      equivalence (real_value, int_value)

      clean_fnil = 0

      do j = 1, size(sp_vector)
        real_value = sp_vector(j)

        if(iand(int_value, INT_INF_PLUS) == INT_INF_PLUS) then
          ! IEEE NAN, +INF, or -INF  detected
          sp_vector(j) = FNIL
          clean_fnil = clean_fnil + 1
        end if
      end do

      end function clean_fnil

!!----------------------------- clean_kill -------------------------------!!
!!----------------------------- clean_kill -------------------------------!!
!!----------------------------- clean_kill -------------------------------!!

      function clean_kill(sp_vector)

      logical            :: clean_kill
      real, dimension(:) :: sp_vector

! Examine elements of sp_vector.
! If any IEEE NAN, +INF or -INF is found, zero all elements in the vector.
! Return .true. iff an IEEE NAN, +INF or -INF is found.

      integer j

      real :: real_value
      integer :: int_value
      equivalence (real_value, int_value)

      clean_kill = .false.

      do j = 1, size(sp_vector)
        real_value = sp_vector(j)

        if(iand(int_value, INT_INF_PLUS) == INT_INF_PLUS) then
          ! IEEE NAN, +INF, or -INF  detected
          sp_vector = 0.0
          clean_kill = .true.
          exit
        end if
      end do

      end function clean_kill

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module clean_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

