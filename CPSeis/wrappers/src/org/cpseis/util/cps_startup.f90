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
!!-------------------------- cps_startup.f90 ------------------------------!!
!!-------------------------- cps_startup.f90 ------------------------------!!
!!-------------------------- cps_startup.f90 ------------------------------!!

!        C-callable Fortran-90 code to do essential CPS initializations.

      subroutine cps_startup
      use manhist_module
!     use pcpsx_module
      implicit none
!     include "mpif.h"
      logical,save :: starting = .true.
!     logical      :: initialized
      integer      :: istat

      if(starting) then
          call manhist_initialize (istat)  ! requires parameter cache to be initialized.
!         call mpi_initialized (initialized, istat)
!         if (initialized) call pcpsx_init_processing       ! calls ppio_init.
          starting = .false.
      endif

! ppio_init (called from pcpsx_init_processing) aborts with an error message if
! mpi has not been initialized and lam is not running.  Therefore we check here
! to make sure that mpi has been initialized before calling pcpsx.  The assumption
! here is that mpi probably is not needed if it has not already been initialized.
! This assumption might be false.  ppio_init will attempt to initialize mpi if
! it has not yet been initialized, but this attempt will fail if lam is not
! running.  We want this routine to succeed even if lam is not running.

      end subroutine cps_startup

!!---------------------------------- end -------------------------------------!!
!!---------------------------------- end -------------------------------------!!
!!---------------------------------- end -------------------------------------!!
