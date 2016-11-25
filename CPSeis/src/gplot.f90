!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- gplot.f90 --------------------------------!!
!!------------------------------- gplot.f90 --------------------------------!!
!!------------------------------- gplot.f90 --------------------------------!!

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
! Name       : gplot
! Category   : plot
! Written    : 2002-09-09   by: Tom Stoeckley
! Revised    : 2002-09-09   by: Tom Stoeckley
! Maturity   : production   2002-09-11
! Purpose    : Easy interface to GPI which uses GNUPLOT.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>


!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! This primitive is a wrapper around Ed Schmauch's GPI primitive which
! enables a process module to utilize GNUPLOT to plot a graph in the CPS
! front end.  This primitive is specifically designed as a convenience
! to make 2D (X,Y) plots using a temporary (X,Y) coordinate file, and
! therefore is not as flexible as the GPI primitive.  However, this
! primitive could be expanded to provide additional plotting conveniences
! if desired.
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
!                                  b     i       i
!           call gplot_add_point (obj, xcoord, ycoord)
!           call gplot_add_break (obj)
!           call gplot_plot      (obj)
!           call gplot_replot    (obj)
!           call gplot_delete    (obj)
!
!
! type(gplot_struct)    obj = pointer to the GPLOT data structure.
! real               xcoord = X coordinate.
! real               ycoord = Y coordinate.
!
! OBJ must be nullified before first use.
!
! OBJ will be created by GPLOT_ADD_POINT or GPLOT_ADD_BREAK or
! GPLOT_PLOT or GPLOT_REPLOT if necessary.
!
! GPLOT_ADD_POINT must be called to add each point to an upcoming plot.
! GPLOT_ADD_BREAK can be called between two calls to GPLOT_ADD_POINT to
! interrupt connecting the previous and following points with lines.
!
! After all points are added by calling GPLOT_ADD_POINT and GPLOT_ADD_BREAK,
! GPLOT_PLOT or GPLOT_REPLOT must be called to plot the points.  If
! GPLOT_PLOT is called, the previous plot will be deleted (if it exists)
! and a new plot will be created.  If GPLOT_REPLOT is called, the points
! will be replaced using the previous plot (if it exists); otherwise a new
! plot will be created.
!
! After calling GPLOT_PLOT or GPLOT_REPLOT, the routines GPLOT_ADD_POINT
! and GPLOT_ADD_BREAK can be called to add points for a new upcoming plot.
! All points previously plotted will be discarded.
!
! GPLOT_DELETE should be called when the plot is no longer needed.
! GPLOT_DELETE will do nothing if OBJ is not associated.
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
!  1. 2002-09-11  Stoeckley  Initial version.
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


      module gplot_module
      use pc_module
      use gpi_module
      use getlun_module
      use tempname_module
      implicit none
      public
      private :: gplot_private_create

      character(len=100),public,save :: GPLOT_IDENT = &
'$Id: gplot.f90,v 1.1 2002/09/10 17:51:18 Stoeckley prod sps $'


!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!
!!-------------------------- data structure -------------------------------!!


      type,public :: gplot_struct              
      private

        integer                          :: gpi
        integer                          :: lun
        character(len=FILENAME_LENGTH)   :: tempname
        logical                          :: opened

      end type gplot_struct

      contains


!!--------------------------- private create ------------------------------!!
!!--------------------------- private create ------------------------------!!
!!--------------------------- private create ------------------------------!!


      subroutine gplot_private_create (obj)
      type(gplot_struct),pointer       :: obj                 ! arguments

!----------return if this object already exists:

      if (associated(obj)) then
           if (obj%lun <= 0) then
               call pc_error ('previous error on temporary gnuplot file')
           end if
           return
      end if

!----------allocate and initialize the object:

      allocate (obj)
      obj%gpi      = -1
      obj%lun      = -1
      obj%tempname = ' '
      obj%opened   = .false.

!----------get lun for temporary file:

      call getlun (obj%lun)
      if (obj%lun <= 0) then
           call pc_error ('error getting lun for temporary gnuplot file')
           obj%lun = -1
           return
      end if

!----------get tempname for temporary file:

      obj%tempname = tempname('gnuplot')
      if (obj%tempname == ' ') then
           call pc_error ('error getting filename for temporary gnuplot file')
           obj%lun = -1
      end if
      end subroutine gplot_private_create


!!----------------------------- add point ---------------------------------!!
!!----------------------------- add point ---------------------------------!!
!!----------------------------- add point ---------------------------------!!


      subroutine gplot_add_point (obj,xcoord,ycoord)
      type(gplot_struct),pointer       :: obj                 ! arguments
      real              ,intent(in)    :: xcoord,ycoord       ! arguments
      integer                          :: status              ! local

!----------create this object if it does not yet exist:

      call gplot_private_create (obj)
      if (pc_update_error()) return

!----------open the temporary file if necessary:

      if (.not.obj%opened) then
           open (obj%lun, file=obj%tempname, iostat=status)
           if (status /= 0) then
                call pc_error ('error opening temporary gnuplot file')
                obj%lun = -1
                return
           end if
           obj%opened = .true.
      end if

!----------add a point to the temporary file:

      write (obj%lun,*,iostat=status) xcoord, ycoord
      if (status /= 0) then
           call pc_error ('error adding a point to temporary gnuplot file')
           close (obj%lun, status='delete')
           obj%opened = .false.
           obj%lun = -1
      end if
      end subroutine gplot_add_point


!!--------------------------- add break ------------------------------------!!
!!--------------------------- add break ------------------------------------!!
!!--------------------------- add break ------------------------------------!!


      subroutine gplot_add_break (obj)
      type(gplot_struct),pointer       :: obj                 ! arguments
      integer                          :: status              ! local

!----------create this object if it does not yet exist:

      call gplot_private_create (obj)
      if (pc_update_error()) return

!----------open the temporary file if necessary:

      if (.not.obj%opened) then
           open (obj%lun, file=obj%tempname, iostat=status)
           if (status /= 0) then
                call pc_error ('error opening temporary gnuplot file')
                obj%lun = -1
                return
           end if
           obj%opened = .true.
      end if

!----------add a blank line to the temporary file:

      write (obj%lun,*,iostat=status) ' '
      if (status /= 0) then
           call pc_error ('error adding blank line to temporary gnuplot file')
           close (obj%lun, status='delete')
           obj%opened = .false.
           obj%lun = -1
      end if
      end subroutine gplot_add_break


!!-------------------------------- plot -----------------------------------!!
!!-------------------------------- plot -----------------------------------!!
!!-------------------------------- plot -----------------------------------!!


      subroutine gplot_plot (obj)
      type(gplot_struct),pointer       :: obj                ! arguments
      integer                          :: status             ! local
      character(len=222)               :: command            ! local

!----------create this object if it does not yet exist:

      call gplot_private_create (obj)
      if (pc_update_error()) return

!----------close the temporary file if it is open:

      if (obj%opened) then
           close (obj%lun, status='keep')
           obj%opened = .false.
      end if

!----------close previous gnuplot:

      if (obj%gpi >= 0) then
           status = gpi_close(obj%gpi)
           if (status < 0) then
                call pc_error ('error closing gnuplot before re-opening')
           end if
           obj%gpi = -1
      end if

!----------open a new gnuplot to plot the points:

      obj%gpi = gpi_open()
      if (obj%gpi < 0) then
           call pc_error ('error opening new gnuplot')
           return
      end if

!----------plot the points:

      command = 'plot "'//trim(obj%tempname)//'" with linespoints'
      status  = gpi_command(obj%gpi,command)
      if (status < 0) then
           call pc_error ('gnuplot command error')
      end if
      end subroutine gplot_plot


!!-------------------------------- replot -----------------------------------!!
!!-------------------------------- replot -----------------------------------!!
!!-------------------------------- replot -----------------------------------!!


      subroutine gplot_replot (obj)
      type(gplot_struct),pointer       :: obj                ! arguments
      integer                          :: status             ! local
      character(len=222)               :: command            ! local

!----------create this object if it does not yet exist:

      call gplot_private_create (obj)
      if (pc_update_error()) return

!----------close the temporary file if it is open:

      if (obj%opened) then
           close (obj%lun, status='keep')
           obj%opened = .false.
      end if

!----------try to reuse the same gnuplot to plot the points:

      if (obj%gpi >= 0) then
           command = 'replot "'//trim(obj%tempname)//'" with linespoints'
           status  = gpi_command(obj%gpi,command)
           if (status < 0) then
                call pc_error ('gnuplot command error')
           end if
           return
      end if

!----------open a new gnuplot to plot the points:

      obj%gpi = gpi_open()
      if (obj%gpi < 0) then
           call pc_error ('error opening new gnuplot')
           return
      end if

!----------plot the points:

      command = 'plot "'//trim(obj%tempname)//'" with linespoints'
      status  = gpi_command(obj%gpi,command)
      if (status < 0) then
                call pc_error ('gnuplot command error')
      end if
      end subroutine gplot_replot


!!-------------------------------- delete -----------------------------------!!
!!-------------------------------- delete -----------------------------------!!
!!-------------------------------- delete -----------------------------------!!


      subroutine gplot_delete (obj)
      type(gplot_struct),pointer       :: obj       ! arguments
      integer                          :: status    ! local

!----------return if this object does not exist:

      if (.not.associated(obj)) return

!----------close previous gnuplot:

      if (obj%gpi >= 0) then
           status = gpi_close(obj%gpi)
      end if

!----------delete temporary file:

      if (obj%lun > 0) then
           open (obj%lun, file=obj%tempname, iostat=status)
           if (status == 0) then
                close (obj%lun, status='delete')
           end if
      end if

!----------deallocate this object:

      deallocate (obj)
      end subroutine gplot_delete


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module gplot_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!

