!<CPS_v1 type="AUXILIARY_FILE"/>

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
!                        C P S  A U X I L I A R Y  F I L E
!
! Name       : cfebld_frou
! Category   : stand-alone 
! Written    : 1999-07-08   by: Donna K. Vunderink
! Revised    : 2002-06-07   by: Donna K. Vunderink
! Maturity   : beta
! Purpose    : Interface between C and Fortran 90
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY
!
!     Date        Author       Description
!     ----        ------       -----------
!  6. 2002-06-07  Vunderink    Added support for different mpi versions.
!  5. 2002-05-31  Vunderink    Increased workfile/jobfile path to 260 characters
!  4. 2001-12-17  Vunderink    Make subroutine naming changes.
!  3. 2001-11-07  Vunderink    Added platform option.
!  2. 2000-12-20  Vunderink    Added batch system option.
!  1. 1999-07-08  Vunderink    Initial version.
!
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
!                      SPECIAL COMPILING REQUIREMENTS
!
! No special requirements.
!
!-------------------------------------------------------------------------------
!</compile_doc>


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


!!--------------------------- start of code -------------------------------!!
!!--------------------------- start of code -------------------------------!!
!!--------------------------- start of code -------------------------------!!


      subroutine cfebld_f (h_workfile, h_jobmode,h_batchsystem,h_platform,h_mpi)

      use buildjob_module
      use string_module

      implicit none

      integer              :: h_workfile(*)                           ! argument
      integer              :: h_jobmode(*)                            ! argument
      integer              :: h_batchsystem(*)                        ! argument
      integer              :: h_platform(*)                           ! argument
      integer              :: h_mpi(*)                                ! argument

      character(len=260)   :: workfile                                ! local
      character(len=260)   :: jobfile                                 ! local
      character(len=80)    :: jobmode                                 ! local
      character(len=80)    :: batchsystem                             ! local
      character(len=80)    :: platform                                ! local
      character(len=80)    :: mpi                                     ! local
      integer              :: istat                                   ! local
      integer              :: nworkfile                               ! local
      integer              :: njobfile                                ! local
      integer              :: i                                       ! local

      call string_hh2cc (h_workfile   ,workfile   )
      call string_hh2cc (h_jobmode    ,jobmode    )
      call string_hh2cc (h_batchsystem,batchsystem)
      call string_hh2cc (h_platform   ,platform   )
      call string_hh2cc (h_mpi        ,mpi        )

      call string_strip_blanks(workfile,nworkfile)
      i = index(workfile(1:nworkfile),'.wrk')
      if (i .eq. 0) then
         jobfile = workfile(1:nworkfile) // '.job'
         workfile(nworkfile+1:nworkfile+4) = '.wrk'
         nworkfile = nworkfile + 4
      else
         jobfile = workfile(1:i-1) // '.job'
      endif
      call string_strip_blanks(jobfile,njobfile)

      call buildjob(workfile(1:nworkfile),jobfile(1:njobfile),  &
                    trim(jobmode),trim(batchsystem),trim(platform),trim(mpi),  &
                    istat)

      end subroutine cfebld_f
