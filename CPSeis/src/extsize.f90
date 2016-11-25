!<CPS_v1 type="PRIMITIVE"/>
!!---------------------------- extsize.f90 ------------------------------!!
!!---------------------------- extsize.f90 ------------------------------!!
!!---------------------------- extsize.f90 ------------------------------!!

 
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
!                         C P S   P R I M I T I V E
!
! Name       : EXTSIZE                    (set extent size)
! Category   : io
! Written    : 2001-06-29   by: Tom Stoeckley
! Revised    : 2001-10-23   by: Tom Stoeckley
! Maturity   : production   2001-11-05
! Purpose    : Set an extent size for output files written by CIO.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                          GENERAL DESCRIPTION              
!
! This primitive is to be used to set the extent size for writing a file
! with a known record size and estimated number of records.  It purpose is
! to set the extent size so that small files will have small extents and
! large files will have large extents, without creating too many extents
! and without taking an unnecessarily long time to preset the extent.
!
! This primitive calculates an optimal extent size and then calls CIO
! to set this extent size.  This extent size will then be used (only)
! the next time a file is opened for write using CIO.
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
!                         opt        opt        opt    opt
!                          i          i          i      o
!         call extsize (lunprint, maxrecords, recsize, err)
!
! integer      lunprint = unit number for printing.
! integer    maxrecords = estimated maximum number of traces to write.
! integer       recsize = record size in bytes.
! integer           err = error flag (set to CIO_OK or CIO_ERROR).
!
! The arguments are optional for convenience in case the arguments are also
! optional in the calling routine.  Since all the arguments are integers,
! care must be taken to specify them in the right order.
!
! If LUNPRINT   is present and > 0, a summary will be printed.
! If RECSIZE    is missing or <= 0, the default extent size will be retained.
! If MAXRECORDS is missing or <= 0, the default extent size will be retained.
!
! It is up to the calling program to decide whether an error in this routine
! should be considered a fatal error, since an error in this routine will
! simply leave the default extent size in place.
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<advice_doc>
!-------------------------------------------------------------------------------
!                          ADVICE FOR USERS
!
!
!-------------------------------------------------------------------------------
!</advice_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                           REVISION HISTORY                
!
!     Date        Author     Description
!     ----        ------     -----------
!  2. 2001-11-05  Stoeckley  Add tests for RECSIZE and MAXRECORDS <= 0.
!  1. 2001-08-27  Stoeckley  Initial version, made from a private subroutine
!                             in TEMPTFILE.
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


!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!
!!--------------------------- start of module ------------------------------!!


      module extsize_module
      use cio_module
      implicit none
      public

      character(len=100),public,save :: EXTSIZE_IDENT = &
       '$Id: extsize.f90,v 1.2 2001/11/02 14:03:03 Stoeckley prod sps $'

      contains


!!----------------------------- extsize ---------------------------------!!
!!----------------------------- extsize ---------------------------------!!
!!----------------------------- extsize ---------------------------------!!


      subroutine extsize (lunprint,maxrecords,recsize,err)
      implicit none
      integer,optional,intent(in)  :: lunprint                    ! arguments
      integer,optional,intent(in)  :: maxrecords                  ! arguments
      integer,optional,intent(in)  :: recsize                     ! arguments
      integer,optional,intent(out) :: err                         ! arguments
      integer                      :: istat,nextents              ! local
      integer                      :: ext_size                    ! local
      real                         :: filesize,fudgesize          ! local
      integer                      :: num_allowed_extents         ! local
      integer                      :: max_allowed_extent_size     ! local
      integer                      :: default_extent_size         ! local
      integer           ,parameter :: GIGA         = 1000000000   ! local
      integer           ,parameter :: MEGA         = 1000000      ! local
      real              ,parameter :: LARGE_FILE   = 10.0 * GIGA  ! local
      real              ,parameter :: FUDGE_FACTOR = 2.0          ! local

      if (present(err)) err = CIO_OK

      if (.not.present(maxrecords)) return
      if (.not.present(recsize))    return

      if (maxrecords <= 0) return
      if (recsize    <= 0) return

      filesize = real(maxrecords) * real(recsize)
      fudgesize = FUDGE_FACTOR * filesize

      num_allowed_extents     = 256              ! later get from CIO.
      max_allowed_extent_size = 2 * GIGA         ! later get from CIO.
      default_extent_size     = 256 * MEGA       ! later get from CIO.

      if (fudgesize < default_extent_size) then
           ext_size = nint(fudgesize)
      else if (fudgesize > LARGE_FILE) then
           ext_size = max_allowed_extent_size
      else if (fudgesize / default_extent_size > num_allowed_extents) then
           ext_size = nint(fudgesize / num_allowed_extents)
      else
           ext_size = default_extent_size
      end if

      nextents = max(nint(filesize / ext_size), 1)

      istat = cio_set_file_ext_size(ext_size)

      if (present(err)) then
           if (istat == 0) then
                err = CIO_OK
           else
                err = CIO_ERROR
           end if
      end if

      if (present(lunprint)) then
           if (lunprint > 0) then
                write (lunprint,4002) recsize
                write (lunprint,4003) maxrecords
                write (lunprint,4004) real(ext_size) / GIGA
                write (lunprint,4005) filesize / GIGA
                write (lunprint,4006) nextents
                if (istat /= 0) write (lunprint,4007)
           end if
      end if

4002  format (' EXTSIZE: record size        = ',I9  ,' bytes')
4003  format (' EXTSIZE: maximum # records  = ',I9)
4004  format (' EXTSIZE: extent size        = ',F9.4,' gigabytes')
4005  format (' EXTSIZE: expected file size = ',F9.4,' gigabytes')
4006  format (' EXTSIZE: approx # extents   = ',I9)
4007  format (' EXTSIZE: error setting extent size')
      return
      end subroutine extsize


!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!
!!----------------------------- end of module ------------------------------!!


      end module extsize_module


!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!
!!--------------------------------- end ------------------------------------!!

