!!!
C<license>
C-------------------------------------------------------------------------------
C Copyright (c) 2007 ConocoPhillips Company
C
C Permission is hereby granted, free of charge, to any person obtaining a copy
C of this software and associated documentation files (the "Software"), to deal
C in the Software without restriction, including without limitation the rights
C to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
C copies of the Software, and to permit persons to whom the Software is
C furnished to do so, subject to the following conditions:
C
C The above copyright notice and this permission notice shall be included in all
C copies or substantial portions of the Software.
C
C THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
C IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
C FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
C AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
C LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
C OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
C SOFTWARE.
C-------------------------------------------------------------------------------
C</license>
!!!         Primitive Fortran Template for Fortran to C Interfaces
!!!
!!!-----------------------------------------------------------------------------
!!!                   REVISION HISTORY FOR THIS TEMPLATE
!!!
!!!     Date        Author     Description
!!!     ----        ------     -----------
!!!  4. 2002-10-23  Stoeckley  Minor history doc and ident string changes.
!!!  3. 2002-06-07  Stoeckley  Remove unnecessary IMPLICIT NONE and RETURN
!!!                             statements.
!!!  2. 2002-05-16  Stoeckley  Add this template revision history.
!!!  1. 2001-10-23  Stoeckley  Initial version (replaces BOTCH).
!!!
!!!-----------------------------------------------------------------------------
!!!
!!! This file can be used as a template for Fortran-to-C interfaces.
!!! See c2f_interface.h (and heidi_wrapper.c) for details.
!!!
!!! Note: This module is identical to andrew.f90
!!!       except for the names andrew and heidi.
!!!
!!! This primitive named HEIDI is a working example of a Fortran-style class
!!! which is called from a C-style wrapper class named HEIDI_WRAPPER.
!!! See the HEIDI_WRAPPER primitive for additional information.
!!!
!!!-----------------------------------------------------------------------------


!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- heidi.f90 --------------------------------!!
!!------------------------------- heidi.f90 --------------------------------!!
!!------------------------------- heidi.f90 --------------------------------!!

       ! other files are:  heidi_wrapper.c  heidi_wrapper.h  heidi_frou.f90



!<brief_doc>
!-------------------------------------------------------------------------------
!                        C P S   P R I M I T I V E
!
! Name       : HEIDI
! Category   : --> math, filters, io, etc. (subdirectory)
! Written    : 2001-01-01   by: NNNN
! Revised    : 2001-01-01   by: NNNN
! Maturity   : beta
! Purpose    : --> description for single-line context-sensitive help.
! Portability: No known limitations.
!
!!!  --> Choose the category from this list of subdirectories:
!!!
!!!      character     math          moves      io           velocity
!!!      filters       memory        packs      sorts        miscellaneous
!!!      main_prog     migrations    plot       synthetics
!!!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
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
!                                o
!           call  heidi_create (obj)
!
!            o                   b    o    i    b
!           iii = heidi_solve  (obj, fff, ddd, ccc);
!
!                                b
!           call  heidi_delete (obj);
!
!
! type(heidi_struct),pointer  obj    = pointer to the HEIDI object.
! real                        fff(:) = description of this output array.
! double precision            ddd    = description of this input variable.
! character(len=*)            ccc    = description of this input/output string.
! integer                     iii    = description of this returned variable.
!
! fff must be dimensioned at least [HEIDI_NFFF].
! ccc must have length at least    [HEIDI_NCCC].
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY              
!
!     Date        Author     Description
!     ----        ------     -----------
!  1. 2001-01-01  NNNN       Initial version.
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


      module heidi_module
      implicit none
      public

      character(len=100),public,save :: HEIDI_IDENT = &
'$Id: heidi.f90,v 1.4 2002/10/23 21:19:24 Stoeckley custom sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      integer,public,parameter :: HEIDI_NFFF =  4
      integer,public,parameter :: HEIDI_NCCC = 20


      type,public :: heidi_struct              
        private
        integer                   :: iii
        real                      :: fff(HEIDI_NFFF)
        double precision          :: ddd
        character(len=HEIDI_NCCC) :: ccc
      end type heidi_struct


!!------------------------------ data --------------------------------------!!
!!------------------------------ data --------------------------------------!!
!!------------------------------ data --------------------------------------!!


      integer,parameter,private :: lunprint = 6

      contains


!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!
!!----------------------------- create ------------------------------------!!


      subroutine heidi_create (obj)
      type(heidi_struct),pointer :: obj       ! arguments
      integer                    :: i         ! local

      allocate (obj)

      do i=1,HEIDI_NFFF
           obj%fff(i) = 123.456 * (i-1)
      end do

      obj%iii = 5
      obj%ddd = 123.456789e-33
      obj%ccc = 'hello planet'
      end subroutine heidi_create


!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!
!!------------------------------ delete -----------------------------------!!


      subroutine heidi_delete (obj)
      type(heidi_struct),pointer :: obj       ! arguments

      deallocate(obj)
      end subroutine heidi_delete


!!-------------------------------- solve ------------------------------------!!
!!-------------------------------- solve ------------------------------------!!
!!-------------------------------- solve ------------------------------------!!


      function heidi_solve (obj,fff,ddd,ccc) result (iii)
      type(heidi_struct),intent(inout) :: obj       ! arguments
      real              ,intent(out)   :: fff(:)    ! arguments
      double precision  ,intent(in)    :: ddd       ! arguments
      character(len=*)  ,intent(inout) :: ccc       ! arguments
      integer                          :: iii       ! result
      character(len=HEIDI_NCCC)        :: temp      ! local
      integer                          :: i         ! local

      write (lunprint,*) 'received by heidi:          ddd = ',ddd
      write (lunprint,*) 'received by heidi:          ccc = ',ccc
      write (lunprint,*) ' '

      do i=1,HEIDI_NFFF
           fff(i) = obj%fff(i)
      end do

      temp    = obj%ccc
      obj%ccc = ccc
      ccc     = temp
      obj%ddd = ddd
      iii     = obj%iii;

      write (lunprint,*) 'returning from heidi:       fff = ',fff
      write (lunprint,*) 'returning from heidi:       ddd = ',ddd
      write (lunprint,*) 'returning from heidi:       ccc = ',ccc
      write (lunprint,*) 'returning from heidi:       iii = ',iii

      end function heidi_solve


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module heidi_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

