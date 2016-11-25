!<CPS_v1 type="PRIMITIVE"/>
!!------------------------------- cpsplot.f90 -------------------------------!!
!!------------------------------- cpsplot.f90 -------------------------------!!
!!------------------------------- cpsplot.f90 -------------------------------!!
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
! Name       : CPSPLOT 
! Category   : plot
! Written    : 1989-01-11   by: Dean Peterson
! Revised    : 2006-06-20   by: B. Menger
! Maturity   : production
! Purpose    : Routines for plotting.
! Portability: No known limitations.
!
!-------------------------------------------------------------------------------
!</brief_doc>
!<descript_doc>
!-------------------------------------------------------------------------------
!                         GENERAL DESCRIPTION  
!
!    This set of routines is called from a plotting program which plots
!    to an HSR device.  These routines prepare an input file which will
!    be read by the DPLT program.          
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
!
!                       i     i  i    i      i
!   call cpsplot_circ (iunit,x, y, radius, iwid)
!                  
! integer          iunit  = unit number of plot file
! real             x      = x coordinate of center of circle
! real             y      = y coordinate of center of circle
! real             radius = radius of circle
! integer          iwid   = width of outline of circle in dots
!
!
!                      i     i
!     call cpsplot_fnt(iunit,font)
!
! integer    iunit = unit number of plot file
! integer    font  = The number indicating the font
!
!
!                        i    i
!     call cpsplot_lwid(iunit,width)
! 
! integer    iunit = unit number of plot file
! real       width = width of line in inches
!
!
!                        i     i    i     i    i  i    i  i  i  i
!     call cpsplot_seis (iunit,glav,nsamp,lrrl,yo,yend,ct,ti,va,wt,
!
!                        i      i
!                        vastrt,vaend)
!
! integer    iunit - unit number of plot file
! real       glav  - the largest absolute value of the date being plotted
! integer   nsamp  = the number of sample points in the trace
! integer    lrrl  = lr or rl for left to right or right to left plot
! real        yo   = the y-origin of the section (sample 1)
! real        yend = the y-end of the section (last sample)
! real        ct   = the number of trace channels for amplitude swing
! real       ti    = the number of traces plotted in one inch
! char        va   = variable area fill - yes or no
! integer     wt   = number of dots for wiggle trace - 0 = no wiggle
!                    trace
! real      vastrt = starting percentage of the largest absolute value
!                    for shading variable area
! real       vaend = ending percentage of the largest absolute value for
!                    shading variable area

!                                 i     i    i    i    i     i       i
!     subroutine cpsplot_section (iunit,file,rllr,kype,velev,refelev,elevlm,&
!
!                         i
!    &                    stack,
!
!                         i    i     i     i  i    i     i    i  i
!    &                    trsp,timsc,scfac,dt,ramp,ampsc,rsiz,t0,tn,
!
!                               opt opt opt opt opt opt opt opt opt opt
!                         i  i  i   i   i   i   i   i   i   i   i   i
!    &                    y0,yn,v1, v2, v3, v4, v5, v6, v7, v8, v9, v10,
!
!                         opt  opt  opt  opt  opt  opt
!                         i    i    i    i    i    i
!    &                    v11, v12, v13, v14, v15, v16)
!
!
! integer    file     - file name: name of the file containing traces
!                       SPLT passes a zero
! integer     rllr    - 1  right/left
!                     - 0  left/right
! integer     kype    - 0  v/d plot
!                     - 1  wiggle trace 1 dot
!                     - 2  wiggle trace 2 dots
!                     - 4  v/a plot
!                     - 5  wiggle v/a 1 dot
!                     - 6  wiggle v/a 2 dots
!                     - 12  v/a plot pseudo v/d
!                     - 13  wiggle v/a 1 dot pseudo v/d
!                     - 14  wiggle v/a 2 dots pseudo v/d
! integer     velev   - elevation velocity - feet -0 (no elevation plot)
!                       SPLT passes zero
! integer     refelev - reference elevation - feet
!                       SPLT passes zero
! integer     elevlm  - elevation limit - feet
!                     - <0  minimum limit
!                     - >0  maximum limit
!                       SPLT passes zero
! integer     stack   - maxfold - 0 (no stack plot)
!                       SPLT pases zero
! real        trsp    - traces/x-unit
! real        timsc   - y-units/sec
! real        scfac   - scale factor - 0 (not used)  (polarity +1 or -1)
! real        dt      - sample rate - seconds
! real        ramp    - reference amplitude (amplitude units)
!                     - if (-) each trace normalized to this value
! real        ampsc   - amplitude scale (x-units/amp-units)
! integer     rsiz    - record size (number of characters)
! real        t0      - time lst sample to plot (seconds)
!                       SPLT passes zero
! real        tn      - time lst sample to plot (seconds)
! real        y0      - y-coordinate of t0 in (y-units)
! real        yn      - y-coordinate of tn in (y-units)
!       optional arguements
! real        v1      - variable area start (% peak amplitude)
! real        v2      - variable area end   (% peak amplitude)
!                  or
! real        v1 thru v16      (% peak amplitude for each of
!                               16 intensities)
!
!
!                       i     
!     call cpsplot_setx(iunit,node)
!
! integer    iunit = unit number of plot file
! character  node  = device to plot on passed in node field of conplot call
!
!
!                      i     i
!     call cpsplot_tclr(iunit,color)
!
! integer    iunit = unit number of plot file
! integer    color = The conplot color number.
!
!
!                                                            opt
!                         i  i  i  i  i     i    i     i     i
!     call cpsplot_xdash (x1,y1,x2,y2,dashl,gapl,width,inten,ierr)
!
! real       (x1,y1) and (x2,y2)-the coordinates of the two points to
!                                 be connected.
! real       +,- dashl          -the length of the dash desired in inches.
! real       +,- gapl           -the length of the gap between dashes in
!                                inches.
! real       width              -the width of the dashed line in inches.
! integer    inten              -the intensity level at which the line
!                                will be drawn.
!       optional arguements
! integer    ierr               -not used by xdash.user may use ierr to
!                                identify the call in the event of an error.
!
!                           i     i
!     call cpsplot_xfinish (iunit,narg)
!
! integer    iunit - unit number of plot file
! integer    narg  - 1 endfile plot file
!                  - 2 double endfile plot file & rewind plot file
!                   if narg.ne.1 it is assumed to be 2
!
!
!                                                       opt   opt  opt
!                          i     i i i   i   i    i     i     i    i
!     call cpsplot_xfixno (iunit,x,y,nch,num,size,inten,theta,nerr,thick)
!
! integer  iunit - unit number of plot file
! real       x,y - the coordinates in the user'S SYSTEM,OF THE
!                  leftmost character position available.this point
!                  is the lower left corner or the center of the
!                  first character,depending on the sign of nch.
! integer +,-nch - the maximum number of character positions
!                  available for plotting the number.the number will
!                  be right adjusted in this field.if nch is
!                  negative the x,y point specified refers to the
!                  center of the first character position.otherwise,
!                  it refers to the lower left corner.a maximum of
!                  20 character positions mey be specified.
! integer    num - the number to be plotted.
!           size - the height of the characters in inches.
! integer  inten - the intensity level at which the characters are
!                  to be plotted.
!       optional arguments
! real     theta - the angle,in the plotter'S COORDINATE SYSTEM,AT
!                  which the characters are to be plotted,in
!                  degrees.this argument is optional,and if omitted
!                  is assumed to be zero.
! integer   nerr - not used by xfixno. user may use nerr to identify
!                  the call in the event of an error.
! real     thick - the desired thickness of the lines in inches.
!                  if this argument is omitted, a standard thickness
!                   will be assumed, equal to 0.13 of the given height.
!
!
!
!                                                             opt   opt  opt
!                           i     i i i   i    i   i    i     i     i    i
!     call cpsplot_xflotno (iunit,x,y,nch,ndec,val,size,inten,theta,nerr,thick)
!
! integer  iunit - unit number of plot file
! real      x,y  -the coordinates in the user'S SYSTEM,OF THE
!                 leftmost character position available. this
!                 point is the lower left corner or the center of
!                 the first character,depending on the sign of nch.
! integer +,-nch -the maximum number of character positions
!                 available for plotting the number.the number will be
!                 right adjusted in this field.if nch is negative,the
!                 x,y point speecified refers to the center of the first
!                 character position.otherwise it refers to the lower
!                 left corner.a maximum of 20 character positions may
!                 be specified.
! integer +,-ndec-the number of digits to be plotted to the right
!                 side of the decimal point.if ndec is negative,no digits
!                 will be plotted right of the decimal point,and the
!                 decimal point itself will be suppressed.
! real     val   -the floating point number to be plotted.
! real     size  -the height of the characters in inches.
! integer  inten -the intensity level at which the characters are to
!                 be plotted.
!       optional arguments
! real       theta-the angle,in the plotters coordinate system,at which
!                  the characters are to be plotted,in degrees.this argume
!                  is optional,and,if omitted is assumed to be zero.
! integer     nerr- not used by flotno. user may use nerr to identify the
!                call in the event of an error.
! real       thick-the desired thickness of the lines in inches.
!                  if this argument is omitted, a standard thickness
!                   will be assumed, equal to 0.13 of the given height.
!
!                                                      opt
!                         i     i  i  i  i  i     i     i
!     call cpsplot_xline (iunit,x1,y1,x2,y2,width,inten,nerr)
!
! integer      iunit          - unit number of the plot file
! real         x1,x2 and y1,y2-the coordinates of the points to
!                              be connected.
! real         width          -the width of the symbol in inches. if this
!                              argument is omitted, a standard symbol width
!                              will be assumed, relative to the given height.
! integer      inten          -the intensity level at which the symbol
!                              is to be plotted.
!       optional arguements
! integer         nerr        -not used by xline. user may use nerr to identify
!                              the call in the event of an error.  
!
! 
!
!                                                     opt  opt  opt
!                          i     i i i    i     i     i    i    i
!     call cpsplot_xlines (iunit,x,y,npts,width,inten,incx,incy,nerr)
!
! real         x,y           arrays containing the x and y
!                            coordinates of the points to be connected.
! integer      npts          the number of points to be connected.
! real         width         the width of the symbol in inches.optional,
!                            if omitted,a standard symbol width,relative
!                            to the given height will be assumed.
! integer      inten         the intensity level at which the symbol
!                            is to be plotted.
!       optional arguements
! integer      incx          the increment of the index of array x in
!                            which coordinates of successive points to be
!                            connected are stored.(if incx=3,then the x
!                            coordinates to be connected are in locations
!                            x(1),x(4),x(7),etc. optional,if omitted,
!                            it is assumed to be one(1).
! integer      incy          this argument serves the same function for
!                            the y array as does inx for the x array.
!                            optional ,if omitted ,it is assumed to be
!                            one (1).
! integer      nerr          not used by xlines, user may use nerr to
!                            identify the call in the event of an error.
!
!
!                                           opt
!                         i     i  i  i    i      i
!     call cpsplot_xpoly (iunit,xd,yd,npts,inten,nerr)
!
! integer      iunit         unit number of the plot file
! real         xd,yd         arrays containing the x and y
!                            coordinates of the points to be connected.
! integer      npts          the number of points to be connected.
! integer      inten         the intensity level at which the lines
!                            are to be plotted.
!       optional arguements
! integer      nerr          not used by xpoly, user may use nerr to
!                           identify the call in the event of an error.
!
!
!                           i     i i i  i  i      i      i   i
!     call cpsplot_xsector (iunit,x,y,r1,r2,theta1,theta2,arx,inten)
!
!  integer       iunit    - unit number of the plot file.
!  real           x,y     - coordinates of the center of the circular
!                           figure.
!                 r1      - the outside radius of the circular figure in the
!                           user'S UNITS.
!                 r2      - inside radius of the circular figure in the
!                           user'S UNITS
!                 theta1  - angle in degrees, in the user'S COORDINATE
!                           system, at which the figure starts.
!                 theta2  - angle in degrees subtended by the figure
!                           measured counterclockwise - positive from theta1
!                 arc     - the maximum arc,in degrees, along the periphery
!                           of the sector, which may be plotted as a chord.
!                           if a value of zero is given, 2.0 degrees will
!                 inten   - intensity
!
!
!                                                                  opt    opt
!                          i     i   i  i  i     i     i  i  i  i  i      i
!     call cpsplot_xsetup (iunit,nfu,xb,yb,xscal,yscal,x1,y1,xn,yn,nerrs1,nerr)
!
! integer         nfu    - fortran unit number corresponding to plot file
! real            xb     - the x-coordinate of the base point in the user'S
!                          coordinate system
! real            yb     - the y-coordinate of the base point in the user'S
!                          coordinate system
! real            xscal  - the number (signed) of units in the user'S
!                          coordinate system corresponding to 1 inch on
!                          the plotting surface to the right of the base
!                          point.
! real            yscal  - the number (signed) of units in the user'S
!                          coordinate system corresponding to 1 inch on
!                          the plotting surface above the base point
! real            x1     - the leftmost boundary for this plot,measured
!                          in the x-direction from the base point (inches)
! real            y1     - the lowermost boundary for this plot,measured
!                          in the y-direction from the base point (inches)
! real            xn     - the rightmost boundary for this plot,measured
!                          in the x-direction from the base point (inches)
! real            yn     - the uppermost boundary for this plot,measured
!                          in the y-direction from the base point (inches)
!       optional arguments
! integer         nerrs1 - an integer (signed) which specifies the number of
!                          error diagnostic messages desired by the user.
!                          if nerrs1.lt.0, the job will be terminated upon
!                           reaching a number errors = abs(nerrs1). if nerrs1
!                          .gt.0,a maximum number of error messages = nerrs1
!                          will be produced. thereafter,processing continues
!                          without error indications. if nerrs1 is zero or
!                          not specified, no error messages will be issued.
! integer         nerr  -  not used by setup. user may use nerr to identify
!                          the call in event of an error.
!
!                                               opt opt  opt   opt opt
!                           i     i i i       i   i   i   i    i     i   i
!     call cpsplot_xsymbol (iunit,x,y,ichstrg,nch,hta,int,iorg,alpha,wta,thka,&
!
!                           opt opt  opt   opt    opt
!                           i   i    i     i      i
!                           spb,beta,gamma,mirror,nerr)
!
! integer         iunit  - unit number to write to
! real            x      - x-coordinate at which character string begins
! real            y      - y-coordinate at which character string begins
! character       ichstrg- array containing characters to be plotted,
!                          or a literal specifying the characters to be
!                          plotted, or a single fixed-point number
!                          specifying the character to be plotted
! integer         nch    - if nch.gt.0, the number of characters to plot
!                          if nch.eq.0, ichstrg has a single fixed-point
!                          number specifying the character to be plotted.
!                          if nch.lt.0, ichstrg has a single fixed-point
!                          number specifying the index of the character
!                          to be plotted.
! real            hta    - the height of each symbol (inches)
!       optional arguements
! integer         int    - intensity level at which symbol is to be plotted
!                      (0.le.int.le.15),default (int=15)
! integer         iorg   - an integer specifying one of nine points within
!                      the first character to which the (x,y) coord-
!                      inates refer, as follows
!                      3.4.5
!                      .   .
!                      2.0.6
!                      .   .
!                      1.8.7
!                      default (iorg=0)
! real             alpha  - the rotation angle of the string, relative to
!                           the plus-x axis of the plotter'S SYSTEM, AT
!                           which the string is to be plotted. (degrees)
!                           default (alpha=0.0)
! real             wta    - the width of each symbol (inches)
!                           default (wt=0.7*ht)!
! real             thka   - the desired thickness of the symbol (inches)
!                           default (thk=0.13*ht)
! real             spb    - the spacing from symbol-to-symbol within the
!                           string. (inches) if sp.lt.wt symbols will
!                           overlap.
!                           default (sp=0.9*ht)
! real             beta   - the angle at which each symbol is to be rotated
!                           within the string,relative to the string rotat-
!                           ion angle. (degrees)
!                           default (beta=0.0)
! real             gamma  - the angle at which individual symbols are to be
!                           sloped or slanted.this angle is measured plus to
!                           the right of vertical and minus to the left of
!                           vertical. (degrees)
!                           default (gamma=0.0)
! integer          mirror - a code denoting the manner in which a symbol'S
!                           axis may be flip-flopped.
!                           0 - no axis swapping
!                           1 - right-and-left reversal
!                           2 - top-and-bottom reversal
!                           default (mirror=0)
! integer          nerr   - not used by xsymbol. user may use nerr to
!                           identify the call in the event of an error.!
!
!
!                                 opt
!                  i    i i   i   i
!     call cpsplot_xtrace (trno,x,flg,int,nerr) 
!
! real        trno   - trace number
! real        x      - x coordinate of the zero amplitude (x-units)
! real        flg    - 2 = 2 dot line (at zero amplitude)
!                      1 = 1 dot lile
!                      0 = no line
!                     -1 = 1 dot blank line
! integer      int    - the intensity level at which the traces
!                      are to be plotted.
!       optional arguements
! integer     nerr     not used by xtrace, user may use nerr to
!                      identify the call in the event of an error.
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
!                           REVISION HISTORY              
!
!     Date        Author       Description
!     ----        ------       -----------
!  5. 2006-06-20  B. Menger    Removed Unused Variables.
!  4. 2001-02-14  Goodger      Add routines cpsplot_seis, cpsplot_fnt, 
!                              cpsplot_lwid, cpsplot_setx, cpsplot_tclr.
!  3. 2000-12-08  Goodger      Added some error checking to cpsplot_xsymbol.
!  2. 2000-09-27  Goodger      Added routine cpsplot_xfixno.
!  1. 2000-06-28  Goodger      Converted from old system.
!
!
!-------------------------------------------------------------------------------
!</history_doc>
!<portability_doc>
!-------------------------------------------------------------------------------
!                         PORTABILITY LIMITATIONS         
!
! No known limitations.
!-------------------------------------------------------------------------------
!</portability_doc>
!<compile_doc>
!-------------------------------------------------------------------------------
!                      SPECIAL COMPILING REQUIREMENTS      
!
! No special requirements.
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
!-------------------------------------------------------------------------------
!</programming_doc>
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module cpsplot_module
      use colrplot_module
      use pc_module
      implicit none

      public

      character(len=100),public,save :: CPSPLOT_IDENT = &
       '$Id: cpsplot.f90,v 1.5 2006/06/20 13:11:50 Menger prod sps $'



!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!
!!------------------------------ data -------------------------------------!!

      contains



!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!
!!----------------------------- subroutines -------------------------------!!

      subroutine cpsplot_circ (iunit,x, y, radius, iwid)
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iunit,iwid
      real, intent(in) :: x,y,radius
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(100) :: ia 
      real , dimension(100) :: a 
      integer :: i         ,nw  
      character(len=8) :: icall='CIRCLE'

!-----------------------------------------------
!
!          Specify the conplot font.  
!
      equivalence (a(1), ia(1)) 
      ia=0
      a(2) = x
      a(3) = y
      a(4) = radius
      ia(5)= iwid
      nw=5
      write(iunit)icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_circ
      subroutine cpsplot_fnt(iunit,font)
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iunit,font
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(100) :: ia 
      real , dimension(100) :: a 
      integer :: i         ,nw  
      character(len=8) :: icall='SETFNT'

!-----------------------------------------------
!
!          Specify the conplot font.  
!
      equivalence (a(1), ia(1)) 
      ia=0
      ia(2) = font 
      nw=2
      write(iunit)icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_fnt
 
      subroutine cpsplot_lwid(iunit,width)
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iunit
      real , intent(in) :: width
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(100) :: ia 
      real , dimension(100) :: a 
      integer :: i         ,nw  
      character(len=8) :: icall='LINEWT'

!-----------------------------------------------
!
!          Specify the conplot font.  
!
      equivalence (a(1), ia(1)) 
      ia=0
      a(2) = width 
      nw=2
      write(iunit)icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_lwid 

      subroutine cpsplot_seis(iunit,glv,nsmp,lrrl,yyo,yyend, &
                              cct,ti,va,iwt,vastrt,vaend) 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iunit 
      integer , intent(in) :: nsmp 
      character(len=8) , intent(in) :: lrrl 
      character(len=8),  intent(in) :: va 
      integer , intent(in) :: iwt 
      real , intent(in) :: glv 
      real , intent(in) :: yyo 
      real , intent(in) :: yyend 
      real , intent(in) :: cct 
      real , intent(in) :: ti 
      real , intent(in) :: vastrt 
      real , intent(in) :: vaend 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(100) :: ia 
      real , dimension(100) :: a 
      integer :: i,idir,iva,nw
      character(len=8) :: icall='SECTION'

!-----------------------------------------------
!
!          set up information necessary for plotting the trace
!
!          glv   = the largest absolute value of data being plotted
!          nsmp  = the number of sample points in the trace
!          lrrl  = lr or rl for left to right or right to left plot
!          yyo   = the y-origin of the section (sample 1)
!          yyend = the y-end of the section (last sample)
!          cct   = the number of trace channels for amplitude swing
!          ti    = the number of traces plotted in one inch
!          iva   = variable area fill - yes or no
!          iwt   = number of dots for wiggle trace - 0 = no wiggle
!                  trace
!         vastrt = starting percentage of the largest absolute value
!                  for shading variable area
!          vaend = ending percentage of the largest absolute value for
!                  shading variable area
      equivalence (a(1), ia(1)) 
      if(lrrl.eq.'LR')then
        idir=0
      else
        idir=1
      endif
      if(va.eq.'YES')then
        iva=1
      else
        iva=0
      endif
      ia=0
      a(2) = glv 
      ia(3) = nsmp 
      ia(4) = idir 
      a(5) = yyo 
      a(6) = yyend 
      a(7) = cct 
      a(8) = ti 
      ia(9) = iva 
      ia(10) = iwt 
      a(11) = vastrt 
      a(12) = vaend 
      nw=12
      write(iunit)icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_seis 

      subroutine cpsplot_section(iunit,file,rllr,kype,velev,refelev,elevlm,&
     &                           stack,trsp&
        , timsc, scfac, dt, ramp, ampsc, rsiz, t0, tn, y0, yn,v1,v2,v3, v4, &
        v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16) 
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iunit 
      integer , intent(in) :: kype 
      integer , intent(in) :: file 
      integer , intent(in) :: rllr 
      integer,  intent(in) :: velev 
      integer,  intent(in) :: refelev 
      integer , intent(in) :: elevlm 
      integer,  intent(in) :: stack 
      real , intent(in) :: trsp 
      real , intent(in) :: timsc 
      real , intent(in) :: scfac 
      real , intent(in) :: dt 
      real , intent(in) :: ramp 
      real , intent(in) :: ampsc 
      integer, intent(in) :: rsiz 
      real , intent(in) :: t0 
      real , intent(in) :: tn 
      real , intent(in) :: y0 
      real , intent(in) :: yn 
      real,optional, intent(in) :: v1 
      real,optional, intent(in) :: v2 
      real,optional, intent(in) :: v3 
      real,optional, intent(in) :: v4 
      real,optional, intent(in) :: v5 
      real,optional, intent(in) :: v6 
      real,optional, intent(in) :: v7 
      real,optional, intent(in) :: v8 
      real,optional, intent(in) :: v9 
      real,optional, intent(in) :: v10 
      real,optional, intent(in) :: v11 
      real,optional, intent(in) :: v12 
      real,optional, intent(in) :: v13 
      real,optional, intent(in) :: v14 
      real,optional, intent(in) :: v15 
      real,optional, intent(in) :: v16 
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer , dimension(100) :: ia 
      real,     dimension(100) :: a
      integer :: nw, i 
      character(len=8) :: icall='SECTION'
      equivalence (a, ia) 
!
      ia=0
      ia(2) = file 
      ia(3) = rllr 
      ia(4) = kype 
      a(5) = velev 
      a(6) = refelev 
      a(7) = elevlm 
      a(8) = stack 
      a(9) = trsp 
      a(10) = timsc 
      a(11) = scfac 
      a(12) = dt 
      a(13) = ramp 
      a(14) = ampsc 
      ia(15) = rsiz 
      a(16) = t0 
      a(17) = tn 
      a(18) = y0 
      a(19) = yn 
      nw=19
      if(present(v1))then
         nw=nw+1
         a(nw) = v1 
      endif
      if(present(v2))then
         nw=nw+1
         a(nw) = v2 
      endif
      if(present(v3))then
         nw=nw+1
         a(nw) = v3 
      endif
      if(present(v4))then
         nw=nw+1
         a(nw) = v4 
      endif
      if(present(v5))then
         nw=nw+1
         a(nw) = v5 
      endif
      if(present(v6))then
         nw=nw+1
         a(nw) = v6 
      endif
      if(present(v7))then
         nw=nw+1
         a(nw) = v7 
      endif
      if(present(v8))then
         nw=nw+1
         a(nw) = v8 
      endif
      if(present(v9))then
         nw=nw+1
         a(nw) = v9 
      endif
      if(present(v10))then
         nw=nw+1
         a(nw) = v10
      endif
      if(present(v11))then
         nw=nw+1
         a(nw) = v11 
      endif
      if(present(v12))then
         nw=nw+1
         a(nw) = v12 
      endif
      if(present(v13))then
         nw=nw+1
         a(nw) = v13 
      endif
      if(present(v14))then
         nw=nw+1
         a(nw) = v14 
      endif
      if(present(v15))then
         nw=nw+1
         a(nw) = v15 
      endif
      if(present(v16))then
         nw=nw+1
         a(nw) = v16 
      endif
      write (iunit)icall,nw
      write (iunit)(a(i),i=2,nw) 
      return  
      end subroutine cpsplot_section 

      subroutine cpsplot_setx(iunit,node)
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iunit
      character(len=8) :: node
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(100) :: ia 
      real , dimension(100) :: a 
      integer :: i         ,nw  
      character(len=8) :: icall='CONPLOT'

!-----------------------------------------------
!
!
      equivalence (a(1), ia(1)) 
      ia=0
      call colrplot_mvc(node,1,ia(2),1,8)
      nw=3
      write(iunit)icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_setx

      subroutine cpsplot_tclr(iunit,color)
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iunit,color
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(100) :: ia 
      real , dimension(100) :: a 
      integer :: i         ,nw  
      character(len=8) :: icall='TONCLR'

!-----------------------------------------------
!
!
      equivalence (a(1), ia(1)) 
      ia=0
      ia(2)=color
      nw=2
      write(iunit)icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_tclr


      subroutine cpsplot_xdash(iunit,x1,y1,x2,y2,dashl,gapl,width,inten,ierr) 
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: inten,iunit
      integer,optional, intent(in) :: ierr 
      real , intent(in) :: x1 
      real , intent(in) :: y1 
      real , intent(in) :: x2 
      real , intent(in) :: y2 
      real , intent(in) :: dashl 
      real , intent(in) :: gapl 
      real , intent(in) :: width 
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer , dimension(5000) :: ia 
      real :: a(5000)
      character(len=8) :: icall='XDASH'
      integer :: narg, nw, i 
!-----------------------------------------------
      equivalence (a, ia) 
!
      narg=9
      if(present(ierr))narg=10
      ia=0
      a(2) = x1 
      a(3) = y1 
      a(4) = x2 
      a(5) = y2 
      a(6) = dashl 
      a(7) = gapl 
      a(8) = width 
      ia(9) = inten 
      ia(10) = -1 
      if (narg >= 10) ia(10) = ierr 
      nw = 10 
      write (iunit)icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_xdash 



      subroutine cpsplot_xfinish(iunit,narg) 
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iunit 
      integer , intent(in) :: narg 
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------

      character(len=8) :: icall='XFINISH'
!
      write (iunit) icall,narg 
! double end of file for plot call file
!!!      endfile(unit=iunit) 
!!!      endfile(unit=iunit) 
      rewind(unit=iunit) 
      return  
      end subroutine cpsplot_xfinish 

      subroutine cpsplot_xfixno(iunit,x,y,nch,num,size,inten,theta,nerr,thick) 
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: nch,iunit
      integer , intent(in) :: num 
      integer , intent(in) :: inten 
      integer,optional, intent(in) :: nerr 
      real , intent(in) :: x 
      real , intent(in) :: y 
      real , intent(in) :: size 
      real,optional, intent(in) :: theta,thick
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      real :: a(5000)

      integer , dimension(5000) :: ia 
      integer :: narg, nw, i 

      character(len=8) :: icall='XFIXNO'

      equivalence (ia,a)
      narg = 7
      if(present(theta))narg=8
      if(present(nerr))narg=9
      if(present(thick))narg=10
      ia=0
      a(2) = x 
      a(3) = y 
      ia(4) = nch 
      ia(5) = num 
      a(6) = size 
      ia(7) = inten 
      a(8) = 0.0 
      ia(9) = -1 
      a(10) = 0.13*a(6) 
      if (narg >= 8) a(8) = theta 
      if (narg >= 9) ia(9) = nerr 
      if (narg >= 10) a(10) = thick 
      nw = 10 
      write(iunit)icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_xfixno 

      subroutine cpsplot_xflotno(iunit,x,y,nch,ndec,val,size,inten,theta,&
                                 nerr,thick) 
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: nch,iunit
      integer , intent(in) :: ndec 
      integer , intent(in) :: inten 
      integer,optional , intent(in) :: nerr 
      real , intent(in) :: x 
      real , intent(in) :: y 
      real , intent(in) :: val 
      real , intent(in) :: size 
      real,optional , intent(in) :: theta 
      real,optional , intent(in) :: thick 
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer , dimension(5000) :: ia 
       real :: a(5000)
      integer :: narg, nw, i 
      character(len=8) :: icall='XFLOTNO'
      equivalence (a,ia) 
!
      narg=8
      if(present(theta))narg=9
      if(present(nerr))narg=10
      if(present(thick))narg=11
      ia=0
      a(2) = x 
      a(3) = y 
      ia(4) = nch 
      ia(5) = ndec 
      a(6) = val 
      a(7) = size 
      ia(8) = inten 
      a(9) = 0.0 
      ia(10) = -1 
      a(11) = 0.13*a(7) 
      if (narg >= 9) a(9) = theta 
      if (narg >= 10) ia(10) = nerr 
      if (narg >= 11) a(11) = thick 
      nw = 11 
      write(iunit)icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_xflotno 


      subroutine cpsplot_xline(iunit, x1, y1, x2, y2, width, inten, nerr) 
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: inten,iunit
      integer,optional , intent(in) :: nerr 
      real , intent(in) :: x1 
      real , intent(in) :: y1 
      real , intent(in) :: x2 
      real , intent(in) :: y2 
      real , intent(in) :: width 
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer , dimension(5000) :: ia 
       real :: a(5000)
      character(len=8) :: icall='XLINE'
      integer :: narg, nw, i 
      equivalence (a,ia) 
!
      narg=7
      if(present(nerr))narg=8
      ia=0
      a(2) = x1 
      a(3) = y1 
      a(4) = x2 
      a(5) = y2 
      a(6) = width 
      ia(7) = inten 
      ia(8) = -1 
      if (narg >= 8) ia(8) = nerr 
      nw = 8 
      write(iunit)icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_xline 

      subroutine cpsplot_xlines(iunit,x,y,npts,width,inten,incx,incy,nerr) 
!-----------------------------------------------
!   m o d u l e s 
!-----------------------------------------------
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: npts,iunit 
      integer , intent(in) :: inten 
      integer ,optional, intent(in) :: incx 
      integer ,optional, intent(in) :: incy 
      integer ,optional, intent(in) :: nerr 
      real , intent(in) :: width 
      real , intent(in) :: x(npts) 
      real , intent(in) :: y(npts) 
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer , dimension(5000) :: ia 
      real :: a(5000)
      integer :: narg, nw, nwchk, ix, iy, i 
      character(len=8) :: icall='XLINES'


      equivalence (a,ia) 
!
      narg=6
      if(present(incx))narg=7
      if(present(incy))narg=8
      if(present(nerr))narg=9
      ia=0
      ia(2) = npts 
      a(3) = width 
      ia(4) = inten 
      ia(5) = 1 
      ia(6) = 1 
      ia(7) = -1 
      if (narg >= 7) ia(5) = incx 
      if (narg >= 8) ia(6) = incy 
      if (narg >= 9) ia(7) = nerr 
      nw = 7 
!
      nwchk = nw + 2*npts 
      if (nwchk > 5000) write (6, *) &
        ' * ERROR   CALL XLINES   EXCEEDED BUFFER SIZE *' 
!
      ix = ia(5) 
      iy = ia(6) 
      if (ix == 1) then 
        a(nw+1:npts+nw) = x 
      else 
        a(nw+1:npts+nw) = x(:(npts-1)*ix+1:ix) 
        ia(5) = 1 
      endif 
      nw = nw + npts 
      if (iy == 1) then 
        a(nw+1:npts+nw) = y 
      else 
        a(nw+1:npts+nw) = y(:(npts-1)*iy+1:iy) 
        ia(6) = 1 
      endif 
      nw = nw + npts 
      write(iunit)icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_xlines 

      subroutine cpsplot_xpoly(iunit, xd, yd, npts, inten, nerr) 
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: npts,iunit 
      integer , intent(in) :: inten 
      integer ,optional, intent(in) :: nerr 
      real , intent(in) :: xd(npts) 
      real , intent(in) :: yd(npts) 
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: ia(5000)
      integer :: narg, nw, nwchk, i 
      real :: a(5000)
      character(len=8) :: icall='XPOLY'
!-----------------------------------------------
      equivalence (a,ia) 
!
      narg=5
      if(present(nerr))narg=6
      ia=0
      ia(2) = npts 
      ia(3) = inten 
      ia(4) = -1 
      if (narg >= 6) ia(4) = nerr 
      nw = 4 
!
      nwchk = nw + 2*npts
      if (nwchk.gt.5000) then
        call pc_print('* ERROR   CALL CPSPLOT_XPOLY    EXCEEDED BUFFER SIZE *')
      endif
!
      do 1 i=1,npts
        a(nw+i) = xd(i)
    1 continue 
      nw = nw + npts
      do 2 i=1,npts
        a(nw+i) = yd(i)
    2 continue 
      nw = nw + npts
      write (iunit)icall,nw
      write (iunit) (a(i),i=2,nw) 

      return  
      end subroutine cpsplot_xpoly 

      subroutine cpsplot_xsector(iunit,x,y,r1,r2,theta1,theta2,arx,inten) 
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: inten,iunit
      real , intent(in) :: x 
      real , intent(in) :: y 
      real , intent(in) :: r1 
      real , intent(in) :: r2 
      real , intent(in) :: theta1 
      real , intent(in) :: theta2 
      real , intent(in) :: arx 
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer , dimension(5000) :: ia 
      real :: a(5000)
      character(len=8) :: icall='XSECTOR'
      integer :: nw, i 
!-----------------------------------------------
      equivalence (a,ia) 
!
      ia=0
      a(2) = x 
      a(3) = y 
      a(4) = r1 
      a(5) = r2 
      a(6) = theta1 
      a(7) = theta2 
      a(8) = arx 
      ia(9) = inten 
      nw = 9
      write(iunit)icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_xsector 


      subroutine cpsplot_xsetup(iunit,nfu,xb,yb,xscal,yscal,x1,y1,xn,yn,&
                                nerrs1,nerr)
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iunit 
      integer , intent(in) :: nfu 
      integer,optional, intent(in) :: nerrs1 
      integer,optional, intent(in) :: nerr 
      real , intent(in) :: xb 
      real , intent(in) :: yb 
      real , intent(in) :: xscal 
      real , intent(in) :: yscal 
      real , intent(in) :: x1 
      real , intent(in) :: y1 
      real , intent(in) :: xn 
      real , intent(in) :: yn 
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer , dimension(12) :: ia 
      real,     dimension(12) :: a
      integer :: nw, i 
      character(len=8) :: icall='XSETUP'
      equivalence (a(1), ia(1)) 
!
      ia=0
      ia(2) = nfu 
      a(3) = xb 
      a(4) = yb 
      a(5) = xscal 
      a(6) = yscal 
      a(7) = x1 
      a(8) = y1 
      a(9) = xn 
      a(10) = yn 
      ia(11) = 0 
      ia(12) = -1 
      if(present(nerrs1))ia(11)=nerrs1
      if(present(nerr))ia(12)=nerr
      nw = 12
      write(iunit)icall,nw 
      write(iunit)(a(i),i=2,nw) 
      return  
      end subroutine cpsplot_xsetup 

      subroutine cpsplot_xsymbol(iunit,x,y,ichstrg,nch,hta,int,iorg,alpha,&
                                 wta,thka,&
                                 spb,beta,gamma,mirror,nerr,istat)
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      character(len=*):: ichstrg 
      integer , intent(in) :: nch 
      integer , intent(in) :: iunit
      integer ,optional, intent(in) :: int 
      integer ,optional, intent(in) :: iorg 
      integer ,optional, intent(in) :: mirror 
      integer ,optional, intent(in) :: nerr 
      integer ,optional, intent(inout) :: istat 
      real , intent(in) :: x 
      real , intent(in) :: y 
      real , intent(in) :: hta 
      real ,optional, intent(in) :: alpha 
      real ,optional, intent(in) :: wta 
      real ,optional, intent(in) :: thka 
      real ,optional, intent(in) :: spb 
      real ,optional, intent(in) :: beta 
      real ,optional, intent(in) :: gamma 
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer , dimension(5000) :: ia 
      integer :: narg, nc, ncw, nw, nchar,i,nbits,ibuf(200),lun=6
      real :: a(5000)
      character(len=8) :: icall='XSYMBOL'
      equivalence (a, ia) 
!
      if(present(istat))istat=0
      nchar=len(ichstrg)
      if(nchar.lt.nch)then
        write(lun,*)'ERROR in cpsplot_xsymbol'
        write(lun,*)'The text array is only ',nchar,' characters long'
        write(lun,*)'but you have asked for ',nch,' characters to be plotted' 
        if(present(istat))istat=1
      endif
      narg=5
      if(present(int))narg=6
      if(present(iorg))narg=7
      if(present(alpha))narg=8
      if(present(wta))narg=9
      if(present(thka))narg=10
      if(present(spb))narg=11
      if(present(beta))narg=12
      if(present(gamma))narg=13
      if(present(mirror))narg=14
      if(present(nerr))narg=15
      ia=0
      a(2) = x 
      a(3) = y 
      ia(4) = nch 
      a(5) = hta 
      ia(6) = 9999 
      ia(7) = 9999 
      a(8) = 9999.0 
      a(9) = 9999.0 
      a(10) = 9999.0 
      a(11) = 9999.0 
      a(12) = 9999.0 
      a(13) = 9999.0 
      ia(14) = 9999 
      ia(15) = 9999 
      if (narg >= 6) ia(6) = int 
      if (narg >= 7) ia(7) = iorg 
      if (narg >= 8) a(8) = alpha 
      if (narg >= 9) a(9) = wta 
      if (narg >= 10) a(10) = thka 
      if (narg >= 11) a(11) = spb 
      if (narg >= 12) a(12) = beta 
      if (narg >= 13) a(13) = gamma 
      if (narg >= 14) ia(14) = mirror 
      if (narg >= 15) ia(15) = nerr 
      nc = iabs(nch) 
      nbits=bit_size(ncw)
      if (nch > 0) then 
        ncw = (nch + 7)/8 
        if(nbits.eq.32)then
          ncw = (nch + 3)/4    
        endif
      else 
        ncw = 1 
        nc = 8 
        if(nbits.eq.32)nc=4
      endif 
      ibuf=transfer(ichstrg,(/0/))
      ia(16:16+ncw-1)=ibuf(1:ncw)
      nw = 15 + ncw 
      write (iunit) icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_xsymbol 


      subroutine cpsplot_xtrace(iunit,trno, x, flg, int, nerr) 
      implicit none
!-----------------------------------------------
!   d u m m y   a r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: iunit
      integer , intent(in) :: int 
      integer,optional, intent(in) :: nerr 
      integer , intent(in) :: trno 
      real , intent(in) :: x 
      real , intent(in) :: flg 
!-----------------------------------------------
!   l o c a l   v a r i a b l e s
!-----------------------------------------------
      integer , dimension(6) :: ia 
      real    , dimension(6) :: a
      character(len=8)       :: icall='XTRACE'
      integer :: nw, i 
!-----------------------------------------------
      equivalence (a, ia) 
      ia=0
      ia(2) = trno 
      a(3) = x 
      a(4) = flg 
      ia(5) = int 
      nw=5
      if(present(nerr))then
        ia(6) = nerr 
         nw = 6 
      endif
      write (iunit) icall,nw
      write (iunit) (a(i),i=2,nw) 
      return  
      end subroutine cpsplot_xtrace 


!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!
!!----------------------------- end of module -----------------------------!!


      end module cpsplot_module


!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
!!--------------------------------- end -----------------------------------!!
