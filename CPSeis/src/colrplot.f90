!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- cpsplot.f90 --------------------------------!!
!!------------------------------- cpsplot.f90 --------------------------------!!
!!------------------------------- cpsplot.f90 --------------------------------!!
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
! Name       : COLRPLOT
! Category   : plot
! Written    : 2000-01-01   by: GOODGER
! Revised    : 2002-04-15   by: Goodger
! Maturity   : production   2002-06-11
! Purpose    : Support routines for COLOR process and COLOR2CGM
! Portability: No known limitations.
!
!</brief_doc>

!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION            
!
! These routines are used by the COLOR process to write a file for use with
! the stand alone program COLOR2CGM which uses these routines to read
! the file for making subsequent cgm calls.
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
!
!                         i   i  i    i       i      i      i      i       i
!   call colrplot_axis (iunit,x, y, ititle, nchar, axlen, angle, fvalue, deltav)
!
! integer          iunit  = unit number of plot file
! real             x      = starting x coordinate for axis
! real             y      = starting y coordinate for axis
! character(len=*) ititle = text string which will be centered & axis label
! integer          nchar  = number of characters in title
! real             axlen  = axis length in inches
! real             angle  = angle (in degrees) at which to draw the axis
! real             fvalue = first value used for annotating the axis
! real             deltav = delta value added to fvalue for successive tics
!
!                       i     i  i    i      i
!   call colrplot_circ (iunit,x, y, radius, iwid)
!                  
! integer          iunit  = unit number of plot file
! real             x      = x coordinate of center of circle
! real             y      = y coordinate of center of circle
! real             radius = radius of circle
! integer          iwid   = width of outline of circle in dots
!
!                         i     i      i   
!   call colrplot_dash (iunit,dshpat, ictr)
!
! integer          iunit  = unit number of plot file
! real             dshpat = array which defines pattern for dashed line
! integer          ictr   = counter for # of elem's in dshpat & on/off flag
!
!                              opt
!                         i      i
!   call colrplot_fini (iflun,dispose)
!
! integer          iflun   = unit number of plot file
! character*(*)    dispose = if included may be 'delete' or 'keep'
!
!                         i    i
!   call colrplot_fnt  (iunit,ifnt)
!
! integer          iunit  = unit number of plot file
! integer          ifnt   = font pattern number from conplot manual
!
!
!   id = colrplot_idptr ()
!
! integer, pointer, dimension(:) :: id = pointer to indexing array for lib
!
!                         i     o     i
!   call colrplot_init (type, error,iunit)
!
! character*(*)    type   = file open status
! integer          error  = returned error status (0 is normal return)
! integer          iunit  = unit number of plot file
!
!                       i     o      i       o
!   call colrplot_lfl (iunit,ilfo, ilflmx, ilflo)
!
! integer          iunit  = unit number of plot file
! character*(*)    ilfo   = name of file to read lib array from
! integer          ilflmx = maximum length for file name ilfo allowed
! integer          ilflo  = returned length of file name ilfo
!
!
!   lib = colrplot_libptr ()
!
! integer, pointer, dimension(:) :: lib = pointer to array read
!
!
!   libsz = colrplot_libsz ()
!
! integer          libsz  = size of lib array
!
!
!
!                         i    i
!   call colrplot_lwid (iunit,width)
!
! integer          iunit  = unit number of plot file
! real             width  = width of line in inches
!
!                        i     i
!   call colrplot_olay (iunit,mode)
!
! integer          iunit  = unit number of plot file
! integer          mode   = overlay or superimposed
!
!                        i     i      i
!   call colrplot_nclr (iunit,ipen, icolr)
!
! integer          iunit  = unit number of plot file
! integer          ipen   = pen number (range 0 - 15)
! integer          icolr  = pen color (range 1 - 9)
!
!                       i     i  i    i      i      i     i
!   call colrplot_numb (iunit,x, y, height, fnum, angle, ndig)
!
! integer          iunit  = unit number of plot file
! real             x      = x coord (in.) of lower left-hand corner (1st ch)
! real             y      = y coord (in.) of lower left-hand corner (1st ch)
! real             height = character height (in inches)
! real             fnum   = floating point number to be converted and plotted
! real             angle  = angle at which to plot the numeric value
! integer          ndig   = number of digits and format in which to plot
!
!                        i     i
!   call colrplot_npen (iunit,ipen)
!
! integer          iunit  = unit number of plot file
! integer          ipen   = pen number (range 0 - 15)
!
!                      i     i   i   i   i   i   i     i     i
!   call colrplot_pix (iunit,x1, y1, x2, y2, nx, ny, kpack, isw)
!
! integer          iunit    = unit number of plot file
! real             x1       = x coord for placement of the 1st corner of image
! real             y1       = y coord for placement of the 1st corner of image
! real             x2       = x coord for placmnt of the oppos corner of image
! real             y2       = y coord for placmnt of the oppos corner of image
! integer          nx       = number of pixels in the x direction
! integer          ny       = number of pixels in the y direction
! integer          kpack(*) = pckd byte vlu arry of color # for ea pxl in imag
! integer          isw      = flag for pixel resampling
!
!                                   opt
!                       i     i  i   i     i
!   call colrplot_plot (iunit,x, y, ipen, irot)
!
! integer          iunit  = unit number of plot file
! real             x      = destination x coordinate in inches
! real             y      = destination y coordinate in inches
! integer          ipen   = draw (2) or move (3)
! integer          irot   = optional rotation flag
!
!                       i     i   i   i   i     o
!   call colrplot_poly (iunit,xa, ya, ne, na, istat)
!
! integer          iunit  = unit number of plot file
! real             xa(*)  = x-coordinate array of area
! real             ya(*)  = y-coordinate array of area
! integer          ne     = number of pairs defining each polygon
! integer          na     = number of polygons
! integer          istat  = 0 normal return, otherwise error occured
!
!                         i     o
!   call colrplot_read (iunit,error)
!
! integer          iunit  = unit number of plot file.
! integer          error  = 0 for normal return
!
!                       i     i   i   i   i    i     i
!   call colrplot_rect (iunit,x1, y1, x2, y2, iflg, iclr)
!
! integer          iunit    = unit number of plot file
! real             x1       = x-coordinate of first corner
! real             y1       = y-coordinate of first corner
! real             x2       = x-coordinate of opposite corner
! real             y2       = y-coordinate of opposite corner
! integer          iflg     = flag to outline rectangle
! integer          iclr     = conplot color code
!
!                       i     i     i     i     i
!   call colrplot_rgb (iunit,iclr, red, green, blue)
!
! integer          iunit  = unit number of plot file
! integer          iclr     = color index range (1 - 256)(within 8 bits)
! real             red      = percentage of red   (range 0.0 to 1.0)
! real             green    = percentage of green (range 0.0 to 1.0)
! real             blue     = percentage of blue  (range 0.0 to 1.0)
!
!                       i     i  i  i   i   i     i
!   call colrplot_seis (iunit,x, y, tr, n, alen, fac)
!
! integer          iunit    = unit number of plot file
! real             x        = x-coordinate for start of trace in inches
! real             y        = y-coordinate for start of trace in inches
! real             tr(*)    = array of seismic trace values
! integer          n        = number of values in tr
! real             alen     = length (in.) to plot the trace along the y-axis
! real             fac      = scale fctr usd to convert trace values to in.
!
!                         i     i     i     i      i     i     i     i    i
!   call colrplot_sets (iunit,xmmin,xmmax,imfill,xpmin,xpmax,ipfill,iform,iw)
!
! integer          iunit    = unit number of plot file
! real             xmmin    = minimum fill line for troughs in inches
! real             xmmax    = maximim fill line for troughs in inches
! integer          imfill   = fill pattern for trough shading 0 to 16
! real             xpmin    = minimum fill line for peaks in inches
! real             xpmax    = maximum fill line for peaks in inches
! integer          ipfill   = fill pattern for peak shadig 0 to 16
! integer          iform    = LtoR (1), RtoL (2)
! integer          iw       = width of trace wiggle in dots
!
!                          i     i      i     i     i    i   i     i
!   call colrplot_settxt (iunit,ifnt, ipath, ihal, ival, xp, sp, imode)
!
! integer          iunit    = unit number of plot file
! integer          ifnt     = font pattern number
! integer          ipath    = text path
! integer          ihal     = horizontal alignment
! integer          ival     = vertical aligment
! real             xp       = character expansion factor
! real             sp       = addtionl spacng to be insrtd between characters
! integer          imode    = character spacing mode
!
!                         i    i      i       i     i       i       i      i
!   call colrplot_setx (iunit,node, charge, route, ncpy, jobname, jobid, ivect,
!      opt    opt    opt
!       i      i      i
!     nodefr, irot, iflpt)
!
! integer          iunit    = unit number of plot file
! character*8      node     = node name
! character*12     charge   = charge code
! character*16     route    = route info
! integer          ncpy     = copies to plot
! character*8      jobname  = name of cps job
! character*8      jobid    = job identifier
! character*8      ivect    = name to save vector file
! character*8      nodefr   = optional name of node job was built on
! integer          irot     = optional rotation flag (rotate(1), otherwise(0))
! character*4      iflpt    = optional 'YES ' or 'NO  ' to fold plot or not
!
!                         i     i      i     i      b      i     i   i
!   call colrplot_slen (iunit,itext, nchar, tlen, tymin, height, xp, sp)
!
! integer          iunit    = unit number of plot file
! character*(*)    itext    = text string
! integer          nchar    = number of characters in itext
! real             tlen     = length of string returned in inches
! real             tymin    = # inches strng will plot below the base line
! real             height   = character height
! real             xp       = must be 1.0
! real             sp       = must be 0.0
!
!                      i     i  i   i     i      i    i
!   call colrplot_sym (iunit,x, y, size, itxt, theta, nc)
!
! integer          iunit    = unit number of plot file
! real             x        = x coordinate
! real             y        = y coordinate
! real             size     = character size in inches
! character*(*)    itxt     = text array
! real             theta    = angle to plot text
! integer          nc       = number of charcters in itxt
!
!                        i     i
!   call colrplot_tclr (iunit,iclr)
!
! integer          iunit    = unit number of plot file
! integer          iclr     = conplot color code
!
!                         i     i
!   call colrplot_tflg (iunit,iclr)
!
! integer          iunit    = unit number of plot file
! integer          iclr     = conplot color code
!
!
!   call colrplot_write(iunit)
!
! integer          iunit    = unit number of plot file
!
!-------------------------------------------------------------------------------
!</calling_doc>


!<history_doc>
!-------------------------------------------------------------------------------
!                             REVISION HISTORY              
!
!     Date        Author             Description
!     ----        ------             -----------
!  3. 2002-06-11  Goodger            Change kpack array from type character to
!                                    type integer on routine colrplot_pix.  
!                                    Change documentation to reflect switch from
!                                    conplot to cgm.  Take care of warning
!                                    messages from the intel compiler.
!  2. 2001-02-15  Goodger            Make the plot file unit number an argument
!                                    to all routines.  Having it global prevents
!                                    a process from being reentrant.
!  1. 2000-12-15  Corn/Goodger       Initial version.
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
!
!-------------------------------------------------------------------------------
!</portability_doc>



!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module colrplot_module
      use getlun_module

      implicit none

      character(len=100),public,save :: COLRPLOT_IDENT = &
   '$Id: colrplot.f90,v 1.3 2002/06/10 15:47:02 Goodger prod sps $'

      public
      private :: colrplot_write

! data for colrplot calls that are initialized
      character(len=1),private :: seqncr = 'A' ! current seq char for file
      integer,private   :: seqn     = 0        ! current sequence number
      integer,private   :: mxseqn   = 25       ! maximum sequence number
      character(len=6),private :: lf = 'CONPLT' ! base name of file
      character(len=7),private :: ilf           ! complete name of file
      integer,private   :: ilfl     = 7        ! length of ilf
      integer,private   :: lfrwl    = 64       ! record leng in words
      integer,private   :: lfrec               ! current record
      integer,private   :: libsz    = 10000    ! size of lib array
      integer,private,target :: lib(10000)     ! integer i/o array
      integer,private   :: libw     = 0        ! # of i/o words in lib
      integer,private,target :: id(9)          ! indices array for i/o
      integer,private   :: mxid     = 9        ! size of indices array
      integer,private   :: wbytes              ! bytes per word of machine
      integer,private   :: print_lun=6         ! print unit



!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      contains

      subroutine colrplot_axis (iunit,x,y,ititle,nchar,axlen,angle,fvalue, &
        deltav)
      implicit none
      real,                intent(in) :: x
      real,                intent(in) :: y
      character (len = *), intent(in) :: ititle
      integer,             intent(in) :: iunit,nchar
      real,                intent(in) :: axlen
      real,                intent(in) :: angle
      real,                intent(in) :: fvalue
      real,                intent(in) :: deltav
!
!       draw an axis
!
!  arg    i/o   description
!  x       i    starting x coordinate for axis
!  y       i    starting y coordinate for axis
!  ititle  i    text string which will be centered and used to label
!               axis
!  nchar   i    number of characters in title
!               > 0  y-axis
!               < 0  x-axis
!  axlen   i    axis length in inches
!  angle   i    angle (in degrees) at which to draw the axis.  angle
!               is measured counter clockwise from the horizontal.
!  fvalue  i    first value used for annotating the axis
!  deltav  i    delta value added to fvalue to determine the location
!               of successive tic marks along the axis
!
!          width = width of line in inches
!
      libw = 0
      call colrplot_mvc ('AXIS    ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (x, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (y, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (nchar, 1, lib(id(4)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (axlen, 1, lib(id(5)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (angle, 1, lib(id(6)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (fvalue, 1, lib(id(7)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (deltav, 1, lib(id(8)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (ititle, 1, lib (id(9)), 1, nchar)
      libw = libw + (nchar + wbytes - 1) / wbytes
      call colrplot_write(iunit)
      return
      end subroutine colrplot_axis
!
      subroutine colrplot_circ (iunit,x, y, radius, iwid)
      implicit none
      real,    intent(in) :: x
      real,    intent(in) :: y
      real,    intent(in) :: radius
      integer, intent(in) :: iwid,iunit
!
!        draw a circle centered at x,y
!
!          x    = x coordinate of center of circle
!          y    = y coordinate of center of circle
!        radius = radius of circle
!         iwid  = width of outline of circle in dots
!                 0 = no outline
!                 max wid = 7 dots
!
      libw = 0
      call colrplot_mvc ('CIRCLE  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (x, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (y, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (radius, 1, lib(id(4)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (iwid, 1, lib(id(5)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_circ
!
      subroutine colrplot_dash (iunit,dshpat, ictr)
      implicit none
      real, dimension(*), intent(in) :: dshpat
      integer,            intent(in) :: iunit,ictr
!
!          turn on/off dash line mode for all subsequent calls to plot
!
!
!          dshpat = array which defines pattern for dashed line.
!                   (see conplot manual)
!          ictr   = counter for number of elements in dshpat and
!                   flag for turning on or off dash pattern
!                   (see conplot manual)
!
      integer num, k, i
!
      libw = 0
      call colrplot_mvc ('DASHS   ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (ictr, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      num = abs (ictr)
      if (num.gt.1) then
        if (id(3)+num-1 .gt. libsz) then
          write(print_lun,*)' Error in COLRPLOT_DASH, exceeded buffer size ',&
                              libsz
          stop
        end if
        call colrplot_mvc (dshpat(1), 1, lib(id(3)), 1, num*wbytes)
        libw = libw + num
      end if
      call colrplot_write(iunit)
      return
      end subroutine colrplot_dash
!
      subroutine colrplot_fini(iflun,dispose)
      implicit none
!!      character*(*), optional, intent(in) :: dispose
      character(len=*), optional, intent(in) :: dispose
      integer, intent(in) :: iflun
!
      if (present(dispose)) then
        close (iflun, status=dispose)
      else
        close (iflun)
      end if
      return
      end subroutine colrplot_fini
!
      subroutine colrplot_fnt (iunit,ifnt)
      implicit none
      integer, intent(in) :: iunit,ifnt
!
!          ifnt = font pattern number from conplot manual
!
      libw = 0
      call colrplot_mvc ('SETFNT  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (ifnt, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_fnt
!
      function colrplot_idptr () 
      implicit none
      integer, pointer, dimension(:) :: colrplot_idptr
      colrplot_idptr => id
      return
      end function colrplot_idptr
!
      subroutine colrplot_init (type, error,iflun)
! error =  0 => normal return
! error = -1 => # of colrplot files has exceeded: 26 (letters in alphabit)
! error = -2 => error getting logical unit #
! error = -3 => error opening file
! iflun = unit number for plot file provided by calling routine
      implicit none
!
      character(len=*), intent(in)  :: type
!!      character*(*), intent(in)  :: type
      integer,       intent(out) :: error
      integer,       intent(in)  :: iflun
!
      integer k2, iseq

      integer :: istat

      logical :: first=.true.
!
      error = 0
      if (first) then
        first=.false.
        wbytes = bit_size (lib(1)) / 8
        id(1) = 1
        id(2) = 1 + (8 + wbytes - 1) / wbytes ! 8 characters per descriptor
        do k2 = 3, mxid
          id(k2) = id(k2-1) + 1
        end do
        lib(1:libsz)=0
      else
! increment to the next colrplot in sequence
        seqn = seqn + 1
        if (seqn .ge. mxseqn) error = -1
        if (error .eq. 0) then
          iseq = iachar (seqncr) + 1
          seqncr = achar (iseq)
        end if
      end if
      if (error .eq. 0) then
! name and open file
          ilf = lf // seqncr
          open (unit=iflun, file=ilf(1:ilfl), status=type, &
            access='direct', recl=lfrwl*wbytes, action='readwrite', &
            iostat=istat)
          if (istat.eq. 0)then
             lfrec = 0
           else
             error=-3
           endif
      end if
      return
      end subroutine colrplot_init
!
      subroutine colrplot_lfl (iunit,ilfo, ilflmx, ilflo)
      implicit none
!
      character (len=*), intent(out) :: ilfo
      integer,           intent(in)  :: iunit,ilflmx
      integer,           intent(out) :: ilflo
!
      if (ilflmx .lt. ilfl) then
        ilfo(1:ilflmx) = ilf(1:ilflmx)
        ilflo = ilflmx
      else
        ilfo(1:ilfl) = ilf(1:ilfl)
        ilflo = ilfl
      end if
      return
      end subroutine colrplot_lfl
!
      function colrplot_libptr ()
      implicit none
      integer, pointer, dimension(:) :: colrplot_libptr
      colrplot_libptr => lib
      return
      end function colrplot_libptr
!
      integer function colrplot_libsz ()
      implicit none
      colrplot_libsz = libsz
      return
      end function colrplot_libsz
!
!
      subroutine colrplot_lwid (iunit,width)
      implicit none
      real,    intent(in) :: width
      integer, intent(in) :: iunit
!
!        define the width of lines used in call colrplot
!
!          width = width of line in inches
!
      libw = 0
      call colrplot_mvc ('LINEWT  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (width, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_lwid

      subroutine colrplot_olay (iunit,mode)
      implicit none
      integer, intent(in) :: mode,iunit
!
!        select overlay or superimposed mode
!
      libw = 0
      call colrplot_mvc ('OVRLAY  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (mode, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_olay

      subroutine colrplot_nclr (iunit,ipen, icolr)
      implicit none
      integer, intent(in) :: ipen,iunit
      integer, intent(in) :: icolr
!
!          set the color for a pen
!
!         ipen  = pen number (range 0 - 15)
!        icolr  = pen color
!                 1 = black
!                 2 = cyan
!                 3 = magenta
!                 4 = yellow
!                 5 = blue
!                 6 = red
!                 7 = green
!                 8 = black
!                 9 = white
!
      libw = 0
      call colrplot_mvc ('PENCLR  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (ipen, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (icolr, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_nclr
!
      subroutine colrplot_numb (iunit,x, y, height, fnum, angle, ndig)
      implicit none
      real,    intent(in) :: x
      real,    intent(in) :: y
      real,    intent(in) :: height
      real,    intent(in) :: fnum
      real,    intent(in) :: angle
      integer, intent(in) :: ndig,iunit
!
!          converts a floating point number into a character string
!           and plots the string
!
!         x     = x coordinate (in inches) of the lower left-hand
!                 corner of the first character to be drawn
!         y     = y coordinate (in inches) of the lower left-hand
!                 corner of the first character to be drawn
!       height  = character height (in inches)
!        fnum   = floating point number to be converted and plotted
!        angel  = angle at which to plot the numeric value
!        ndig   = number of digits and format in which to plot the
!                 numeric value
!                 > 0  = number of digits to the right of decimal point
!                        to plot
!                 = 0  = only number'S ROUNDED INTEGER PORTING WITH A
!                        decimal point is plotted
!                 = -1 = only number'S ROUNDED INTEGER PORTION IS
!                        plotted
!                 < -1 = number'S ROUNDED INTEGER PORTION IS PLOTTED
!                        after truncating |ndig| - 1 least significant
!                        digits
!
      libw = 0
      call colrplot_mvc ('NUMBER  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (x, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (y, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (height, 1, lib(id(4)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (fnum, 1, lib(id(5)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (angle, 1, lib(id(6)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (ndig, 1, lib(id(7)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_numb
!
      subroutine colrplot_npen (iunit,ipen)
      implicit none
      integer, intent(in) :: ipen,iunit
!
!          select a pen
!
!        ipen  =  pen number (range 0 - 15)
!
      libw = 0
      call colrplot_mvc ('NEWPEN  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (ipen, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_npen
!
      subroutine colrplot_pix (iunit,x1, y1, x2, y2, nx, ny, kpack, isw)
      implicit none
      real,                              intent(in) :: x1
      real,                              intent(in) :: y1
      real,                              intent(in) :: x2
      real,                              intent(in) :: y2
      integer,                           intent(in) :: nx,iunit
      integer,                           intent(in) :: ny
!!      character (len = 1), dimension(*), intent(in) :: kpack
      integer,                           intent(in) :: isw,kpack(:)
!
!          x1    = x - coordinate for placement of the first corner
!                  of the image
!          y1    = y - coordinate for placement of the first corner
!                  of the image
!          x2    = x - coordinate for placement of the opposite corner
!                  of the image
!          y2    = y - coordinate for placement of the oppostie corner
!                  of the image
!          nx    = number of pixels in the x direction
!          ny    = number of pixels in the y direction
!          kpack = a one dimensional packed byte value array containing
!                  the color number for each pixel to be imaged.
!          isw   = 1 - pixel replication
!                  2 - interpolation of color indices
!                  3 - interpolation of color representations
!
      integer nw
!
      libw = 0
      call colrplot_mvc ('PIXPLT  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (x1, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (y1, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (x2, 1, lib(id(4)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (y2, 1, lib(id(5)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (nx, 1, lib(id(6)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (ny, 1, lib(id(7)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (isw, 1, lib(id(8)), 1, wbytes)
      libw = libw + 1
      nw = (nx*ny + wbytes - 1) / wbytes
      if (libw + nw .gt. libsz) then
        write(print_lun,*)' colrplot_pix attempting to exceed ',libsz,&
                          ' buf size by ',&
          libw+nw-libsz,' words'
        stop
      end if
      call colrplot_mvc (kpack, 1, lib(id(9)), 1, nx*ny)
      libw = libw + nw
      call colrplot_write(iunit)
      return
      end subroutine colrplot_pix
!
      subroutine colrplot_plot (iunit,x, y, ipen, irot)
      implicit none
      real,              intent(in) :: x
      real,              intent(in) :: y
      integer,           intent(in) :: iunit,ipen
      integer, optional, intent(in) :: irot
!
!           x  = destination x coordinate in inches
!           y  = destination y coordinate in inches
!         ipen = 2 = draw to (x,y)
!                3 = move to (x,y)
!
      real a, b
      integer j, irot_tmp
!
      if (present(irot))then
        irot_tmp=irot
      else
        irot_tmp=0
      endif
!
      if (irot_tmp .eq. 0) then
         a = x
         b = y
      else
         a = y
         b = x
      endif
!
      if (a.lt.0.0.or.b.lt.0.0.or.b.gt.41.00001) then
         write(print_lun,*) ' X = ', a, ' Y = ', b, ' IROT = ', irot_tmp
         write(print_lun,*) ' BAD COORDINATE IN PLOT ROUTINE'
         stop
      endif
!
      libw = 0
      call colrplot_mvc ('PLOT    ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (x, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (y, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (ipen, 1, lib(id(4)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_plot
!
      subroutine colrplot_poly (iunit,xa, ya, ne, na, istat)
      implicit none
      real, dimension(*), intent(in)  :: xa
      real, dimension(*), intent(in)  :: ya
      integer,            intent(in)  :: ne,iunit
      integer,            intent(in)  :: na
      integer,            intent(out) :: istat
!
!        draw a polygon using conplot tone routine
!
!          xa   = x-coordinate array of area
!          ya   = y-coordinate array of area
!          ne   = number of pairs defining each polygon
!          na   = number of polygons
!
      integer i
!
      libw = 0
      call colrplot_mvc ('TONE    ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (na, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (ne, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      istat = 0
      do i = 1, ne
        if (xa(i) .lt. 0.0 .or. ya(i) .gt. 40.0) then
          write(print_lun,*)' *** COLR ERROR *** INVALID COORDINATE IN &
                            &POLYGON CALL'
          write(print_lun,*) ' X = ', xa(i) , ' Y = ', ya(i)
          write(print_lun,*) ' POLYGON NOT DRAWN'
          istat = -1
          exit
        end if
      end do
      if (istat .eq. 0) then
        if (id(3)+2*ne .gt. libsz) then
          write(print_lun,*) ' COLR - ERROR - MORE THAN ',libsz, &
            ' WORDS NEEDED FOR COLRPOLY & routine - polygon not drawn'
          istat = -2
        else
          call colrplot_mvc (xa(1), 1, lib(id(4)), 1, ne*wbytes)
          call colrplot_mvc (ya(1), 1, lib(id(4)+ne), 1, ne*wbytes)
          libw = libw + 2 * ne
          call colrplot_write(iunit)
        end if
      end if
      return
      end subroutine colrplot_poly
!
      subroutine colrplot_read (iflun,error)
      implicit none
!
      integer, intent(out) :: error
      integer, intent(in ) :: iflun
!
      integer k2, nw0, nw1
!
      nw1 = lfrwl - 1
      lfrec = lfrec + 1
      read (unit=iflun, iostat=error, rec=lfrec) &
        libw,(lib(k2),k2=1,nw1)
      if (libw .ge. libsz) then
        write(print_lun,*)' Too many input words on colrplot file: ',ilf(1:ilfl)
        stop
      else if (error .eq. 0) then
        if (libw .ge. lfrwl) then
          nw0 = lfrwl
          nw1 = nw0 + lfrwl - 1
          do while (nw1 .gt. 0)
            if (libw .lt. nw1) nw1 = libw
            lfrec = lfrec + 1
            read (unit=iflun, iostat=error, rec=lfrec) &
              (lib(k2),k2=nw0,nw1)
            if (error .eq. 0) then
              nw0 = nw1 + 1
              if (libw-nw0+1 .le. lfrwl) then
                if (libw-nw0+1 .le. 0) then
                  nw1 = 0
                else
                  nw1 = libw
                end if
              else
                nw1 = nw0 + lfrwl - 1
              end if
            else
              nw1 = 0
            end if
          end do
        end if
      end if
      return
      end subroutine colrplot_read

      subroutine colrplot_rect (iunit,x1, y1, x2, y2, iflg, iclr)
      implicit none
      real,    intent(in) :: x1
      real,    intent(in) :: y1
      real,    intent(in) :: x2
      real,    intent(in) :: y2
      integer, intent(in) :: iflg,iunit
      integer, intent(in) :: iclr
!
!          x1   = x-coordinate of first corner
!          y1   = y-coordinate of first corner
!          x2   = x-coordinate of opposite corner
!          y2   = y-coordinate of opposite corner
!          iflg = flag to outline rectangle
!                 0 = no outline, otherwise outline using current pen
!
      call colrplot_tclr (iunit,iclr)
!
      if (y1 .lt. 0.0 .or. y1 .gt. 39.50001 .or. y2 .lt. 0.0 .or. &
                                                 y2 .gt. 39.50001) then
        write(print_lun,*)' Y1 = ', y1, ' Y2 = ', y2
        write(print_lun,*)' ABORT IN COLR RECT ROUTINE'
        stop
      endif
!
      libw = 0
      call colrplot_mvc ('RECT    ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (x1, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (x2, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (y1, 1, lib(id(4)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (y2, 1, lib(id(5)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (iflg, 1, lib(id(6)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_rect
!
      subroutine colrplot_rgb (iunit,iclr, red, green, blue)
      implicit none
      integer, intent(in) :: iclr,iunit
      real,    intent(in) :: red
      real,    intent(in) :: green
      real,    intent(in) :: blue
!
!            define a user color
!
!          iclr   = color index range (1 - 256)(must keep within 8 bits)
!          red    = percentage of red   (range 0.0 to 1.0)
!          green  = percentage of green (range 0.0 to 1.0)
!          blue   = percentage of blue  (range 0.0 to 1.0)
!
      libw = 0
      call colrplot_mvc ('DEFRGB  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (iclr, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (red, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (green, 1, lib(id(4)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (blue, 1, lib(id(5)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_rgb
!
      subroutine colrplot_seis (iunit,x, y, tr, n, alen, fac)
      implicit none
      integer,            intent(in) :: iunit
      real,               intent(in) :: x
      real,               intent(in) :: y
      real, dimension(*), intent(in) :: tr
      integer,            intent(in) :: n
      real,               intent(in) :: alen
      real,               intent(in) :: fac
!
!          plot a seismic trace
!
!          x    = x-coordinate for start of trace in inches
!          y    = y-coordinate for start of trace in inches
!          tr   = array of seismic trace values
!          n    = number of values in tr
!          alen = length in inches to plot the trace along the y-axis
!          fac  = scale factor used to convert trace values to inches
!
      integer k2, k3
!
      libw = 0
      call colrplot_mvc ('PLSEIS  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (x, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (y, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (n, 1, lib(id(4)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (alen, 1, lib(id(5)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (fac, 1, lib(id(6)), 1, wbytes)
      libw = libw + 1
      if (id(6)+n .gt. libsz) then
        write(print_lun,*)' Error in COLRPLOT_SEIS, exceeded buffer size of ', &
          libsz
        stop
      end if
      call colrplot_mvc (tr(1), 1, lib(id(7)), 1, n*wbytes)
      libw = libw + n
      call colrplot_write(iunit)
      return
      end subroutine colrplot_seis
!
      subroutine colrplot_sets (iunit,xmmin, xmmax,imfill,xpmin,xpmax,ipfill, &
      iform, iw)
      implicit none
      real,    intent(in) :: xmmin
      real,    intent(in) :: xmmax
      integer, intent(in) :: imfill,iunit
      real,    intent(in) :: xpmin
      real,    intent(in) :: xpmax
      integer, intent(in) :: ipfill
      integer, intent(in) :: iform
      integer, intent(in) :: iw
!
!             set the seismic plotting parameters
!
!          xmmin = minimum fill line for troughs in inches
!          xmmax = maximim fill line for troughs in inches
!         imfill = fill pattern for trough shading 0 to 16
!          xpmin = minimum fill line for peaks in inches
!          xpmax = maximum fill line for peaks in inches
!         ipfill = fill pattern for peak shadig 0 to 16
!          iform = 1 = left to right
!                  2 = right to left
!          iw    = width of trace wiggle in dots
!
      libw = 0
      call colrplot_mvc ('SETSEIS ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (xmmin, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (xmmax, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (imfill, 1, lib(id(4)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (xpmin, 1, lib(id(5)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (xpmax, 1, lib(id(6)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (ipfill, 1, lib(id(7)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (iform, 1, lib(id(8)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (iw, 1, lib(id(9)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_sets
!
      subroutine colrplot_settxt (iunit,ifnt, ipath, ihal, ival, xp, sp, imode)
      implicit none
      integer, intent(in) :: ifnt
      integer, intent(in) :: ipath,iunit
      integer, intent(in) :: ihal
      integer, intent(in) :: ival
      real,    intent(in) :: xp
      real,    intent(in) :: sp
      integer, intent(in) :: imode
!
!             set the seismic plotting parameters
!
!          ifnt  = font pattern number
!          ipath = text path
!                0 = right
!                1 = left    colr always uses 0
!                2 = up
!                3 = down
!          ihal  = horizontal alignment
!                0 = depends on path
!                1 = left
!                2 = center   colr uses this for text justification
!                3 = right
!          ival  = vertical aligment
!                0 = depends on path
!                1 = top (currently same as cap)
!                2 = cap                            colr always uses 4
!                3 = half
!                4 = base
!                5 = bottom (currently same as base)
!          xp    = character expansion factor
!          sp    = additional spacing to be inserted between characters
!          imode = character spacing mode
!                0 = variable character width
!                1 = character width = character height
!
      libw = 0
      call colrplot_mvc ('SETTXT  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (ifnt, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (ipath, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (ihal, 1, lib(id(4)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (ival, 1, lib(id(5)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (xp, 1, lib(id(6)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (sp, 1, lib(id(7)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (imode, 1, lib(id(8)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_settxt
!
      subroutine colrplot_setx (iunit,node, charge, route, ncpy, jobname, &
        jobid, ivect, nodefr, irot, iflpt)
      implicit none
      integer,                        intent(in) :: iunit
      character (len =  8),           intent(in) :: node
      character (len = 12),           intent(in) :: charge
      character (len = 16),           intent(in) :: route
      integer,                        intent(in) :: ncpy
      character (len =  8),           intent(in) :: jobname
      character (len =  8),           intent(in) :: jobid
      character (len =  8),           intent(in) :: ivect
      character (len =  8), optional, intent(in) :: nodefr
      integer,              optional, intent(in) :: irot
      character (len =  4), optional, intent(in) :: iflpt
!
!       sends information needed by the conplot routine
!
!        node     = node name
!        charge   = charge code
!        route    = route info
!        ncpy     = number of copies to plot
!        jobname  = name of cps job
!        jobid    = job identifier
!        ivect    = name to save vector file
!        nodefr   = name of node job was built on
!        irot     = rotate plot
!                   1 = rotate
!                   0 = do not rotate
!        iflpt    = fold the plot
!                   'YES' = fold
!                   'NO'  = roll
!
      integer wc, nw, irot_tmp
      character(len=8) :: nodefr_tmp
      character(len=4) :: iflpt_tmp
!
      libw = 0
      wc   = id(1)
      call colrplot_mvc ('SETUP   ', 1, lib(wc), 1, 8)
      nw = (8 + wbytes - 1) / wbytes
      libw = libw + nw
      wc = wc + nw
      call colrplot_mvc (node(1:1), 1, lib(wc), 1, 8)
      nw = (8 + wbytes - 1) / wbytes
      libw = libw + nw
      wc = wc + nw
      call colrplot_mvc (charge(1:1), 1, lib(wc), 1, 12)
      nw = (12 + wbytes - 1) / wbytes
      libw = libw + nw
      wc = wc + nw
      call colrplot_mvc (route(1:1), 1, lib(wc), 1, 16)
      nw = (16 + wbytes - 1) / wbytes
      libw = libw + nw
      wc = wc + nw
      call colrplot_mvc (ncpy, 1, lib(wc), 1, wbytes)
      nw = 1
      libw = libw + nw
      wc = wc + nw
      call colrplot_mvc (jobname(1:1), 1, lib(wc), 1, 8)
      nw = (8 + wbytes - 1) / wbytes
      libw = libw + nw
      wc = wc + nw
      call colrplot_mvc (jobid(1:1), 1, lib(wc), 1, 8)
      nw = (8 + wbytes - 1) / wbytes
      libw = libw + nw
      wc = wc + nw
      call colrplot_mvc (ivect(1:1), 1, lib(wc), 1, 8)
      nw = (8 + wbytes - 1) / wbytes
      libw = libw + nw
      wc = wc + nw
!
      if (present(nodefr)) then
        nodefr_tmp(1:8) = nodefr(1:8)
      else
        nodefr_tmp(1:8) = 'UNKNOWN '
      endif
      call colrplot_mvc (nodefr_tmp(1:1), 1, lib(wc), 1, 8)
      nw = (8 + wbytes - 1) / wbytes
      libw = libw + nw
      wc = wc + nw
!
      if (present(irot)) then
        irot_tmp = irot
      else
        irot_tmp = 0
      endif
      call colrplot_mvc (irot_tmp, 1, lib(wc), 1, wbytes)
      nw = 1
      libw = libw + nw
      wc = wc + nw
!
      if (present(iflpt)) then
        iflpt_tmp(1:4) = iflpt(1:4)
      else
        iflpt_tmp(1:4) = 'NO  '
      end if
      call colrplot_mvc (iflpt_tmp(1:1), 1, lib(wc), 1, 4)
      nw = (4 + wbytes - 1) / wbytes
      libw = libw + nw
!
      call colrplot_write(iunit)
      return
      end subroutine colrplot_setx
!
      subroutine colrplot_slen (iunit,itext, nchar, tlen, tymin, height, xp, sp)
      implicit none
      character (len = *), intent(in)    :: itext
      integer,             intent(in)    :: nchar,iunit
      real,                intent(in)    :: tlen
      real,                intent(inout) :: tymin
      real,                intent(in)    :: height
      real,                intent(in)    :: xp
      real,                intent(in)    :: sp
!
!        returns the length in inches of a text string using the
!         current font
!
!         itext = text string
!         nchar = number of characters in itext
!         tlen  = length of string returned in inches
!        tymin  = amount in inches string will plot below the base line
!                  returned
!        height = character height
!        xp     = must be 1.0
!        sp     = must be 0.0
!
      integer nw
!
      libw = 0
      call colrplot_mvc ('STRLEN  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (nchar, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (tlen, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (tymin, 1, lib(id(4)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (height, 1, lib(id(5)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (xp, 1, lib(id(6)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (sp, 1, lib(id(7)), 1, wbytes)
      libw = libw + 1
!
      nw = (nchar + wbytes - 1) / wbytes
      if (id(7)+nw .gt. libsz) then
        write(print_lun,*)' Error in COLRPLOT_SLEN, exceeded buffer size of ', &
          libsz
        stop
      end if
      call colrplot_mvc (itext(1:1), 1, lib(id(8)), 1, nchar)
      libw = libw + nw
!
      call colrplot_write(iunit)
      return
      end subroutine colrplot_slen
!
      subroutine colrplot_sym (iunit,x, y, size, itxt, theta, nc)
      implicit none
      real,                intent(in) :: x
      real,                intent(in) :: y
      real,                intent(in) :: size
      character (len = *), intent(in) :: itxt
      real,                intent(in) :: theta
      integer,             intent(in) :: nc,iunit
!
!           x     = x coordinate
!           y     = y coordinate
!           size  = character size in inches
!           itxt  = text array
!           theta = angle to plot text
!           nc    = number of charcters in itxt
!
      integer nw
!
      libw = 0
      call colrplot_mvc ('SYMBOL  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (x, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (y, 1, lib(id(3)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (size, 1, lib(id(4)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (theta, 1, lib(id(5)), 1, wbytes)
      libw = libw + 1
      call colrplot_mvc (nc, 1, lib(id(6)), 1, wbytes)
      libw = libw + 1
!
      nw = (nc + wbytes - 1) / wbytes
      if (id(6)+nw .gt. libsz) then
        write(print_lun,*)' Error in COLRPLOT_SYM, exceeded buffer size of ', &
          libsz
        stop
      end if
      call colrplot_mvc (itxt(1:1), 1, lib(id(7)), 1, nc)
      libw = libw + nw
!
      call colrplot_write(iunit)
      return
      end subroutine colrplot_sym
!
      subroutine colrplot_tclr (iunit,iclr)
      implicit none
      integer, intent(in) :: iunit,iclr
!
!        change colors
!
!          iclr = conplot color code
!
      libw = 0
      call colrplot_mvc ('TONCLR  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (iclr, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_tclr
!
      subroutine colrplot_tflg (iunit,iclr)
      implicit none
      integer, intent(in) :: iclr,iunit
!
!        change colors
!
!          iclr = conplot color code
!
      libw = 0
      call colrplot_mvc ('TONFLG  ', 1, lib(id(1)), 1, 8)
      libw = libw + (8 + wbytes - 1) / wbytes
      call colrplot_mvc (iclr, 1, lib(id(2)), 1, wbytes)
      libw = libw + 1
      call colrplot_write(iunit)
      return
      end subroutine colrplot_tflg
!
      subroutine colrplot_write(iflun)
      implicit none
      integer, intent(in) :: iflun
!
      integer istat, nw0, nw1, k2, left
!
      if (libw .gt. libsz) then
        write(print_lun,*)' colrplot_write found buf size of ',libsz,&
                          ' exceeded by ',&
                            libw-libsz,' words'
        stop
      end if
      if (libw .le. 0) then
        istat = 0
      else
        nw0 = 1
        nw1 = lfrwl - 1
        if (libw .lt. lfrwl)then
          call colrplot_fillw (lib(libw+1:), 0, lfrwl-libw-1)
        endif
        lfrec = lfrec + 1
        write (unit=iflun, iostat=istat, rec=lfrec) &
          libw,(lib(k2),k2=nw0,nw1)
        if (libw .lt. lfrwl) nw1 = 0
        do while (nw1 .gt. 0)
          if (istat .eq. 0) then
            nw0 = nw1 + 1
            left = libw - nw0 + 1
            if (left .le. 0) then
              nw1 = 0
            else
              nw1 = nw0 + lfrwl - 1
              if (left .le. lfrwl) then
                call colrplot_fillw (lib(libw+1:), 0, lfrwl-left)
              end if
              lfrec = lfrec + 1
              write (unit=iflun, iostat=istat, rec=lfrec) &
                (lib(k2),k2=nw0,nw1)
            end if
          else
            nw1 = 0
          end if
        end do
      end if
      if (istat .ne. 0) then
        write(print_lun,*)' Write error: ',istat,' on colrplot file: ',&
                            ilf(1:ilfl)
        stop
      end if
      return
      end subroutine colrplot_write
      SUBROUTINE colrplot_FILLW (A,B,N)

      implicit none
!
!        FILL N WORDS OF ARRAY A TO WITH B

      integer,intent(inout),target :: A(:)
      integer,intent(in)           :: B,N
!
      INTEGER :: i
      
!
      IF (N.GT.0) THEN
        DO I=1,N
          A(I) = B
        END DO
      ENDIF
!
      RETURN
      END SUBROUTINE colrplot_FILLW
      
!

!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!

      end module colrplot_module
      SUBROUTINE colrplot_MVC (CARRAYI, INDXI, CARRAYO, INDXO, NUMC)
      IMPLICIT NONE
!
!     MOVE BYTES IN CHARACTER*1 ARRAY TO ANOTHER CHARACTER*1 ARRAY
!
      INTEGER INDXI, INDXO, NUMC
      CHARACTER(len=1) :: CARRAYI(*), CARRAYO(*)
!
      CALL colrplot_MOVEB (CARRAYI(INDXI), CARRAYO(INDXO), NUMC)
      RETURN
      END SUBROUTINE colrplot_MVC
      SUBROUTINE colrplot_MOVEB (A,B,N)
      IMPLICIT NONE
!
!        MOVE N BYTES FROM ARRAY A TO ARRAY B
!
      CHARACTER(len=1) :: A(*),B(*)
      INTEGER N
      INTEGER I
!
      IF (N.GT.0) THEN
        DO I=1,N
          B(I) = A(I)
        END DO
      ENDIF
!
      RETURN
      END SUBROUTINE colrplot_MOVEB
      SUBROUTINE colrplot_MOVEW2B (A, B, N)

      implicit none

      integer,intent(in) :: A(N),N
      character(len=1),intent(inout) :: B(N)
!  
!
      INTEGER K2
!
      DO K2 = 1, N
        B(K2) = ACHAR (A(K2))
      END DO
!
      RETURN
      END SUBROUTINE colrplot_MOVEW2B
