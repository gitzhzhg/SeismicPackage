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
! Name       : cgm
! Category   : io
! Written    : 2001-04-16   by: Donna K. Vunderink
! Revised    : 2001-04-16   by: Donna K. Vunderink
! Maturity   : beta
! Purpose    : CGM library.
! Portability: No known limitations.
!
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
!                          CALLING SEQUENCE
!
!
! Name:        GACWK
!
! Abstract:    Activate Workstation
!
! Description: Activates the workstation
!
! Calling Sequence from Fortran 90:
!       call cgm_gsacwk(wk)
!
! Arguments:
!       Name            Type    I/O     Description
!       wk              integer  I      workstation number
!
! Calling Sequence from C:
!       cgmGsacwk(wk);
!
! Arguments:
!       Name            Type    I/O     Description
!       wk              int      I      workstation number
!
!-------------------------------------------------------------------------------
!
! Name:        GCLKS
!
! Abstract:    Close GKS
!
! Description: Close CGM GKS
!
! Calling Sequence from Fortran 90:
!       call cgm_gclks()
!
! Arguments:
!       Name            Type    I/O     Description
!
! Calling Sequence from C:
!       cgmGclks();
!
! Arguments:
!       Name            Type    I/O     Description
!
!-------------------------------------------------------------------------------
!
! Name:        GCLWK
!
! Abstract:    Close Workstation
!
! Description: End the CGM file
!
! Calling Sequence from Fortran 90:
!       call cgm_gclwk(wk)
!
! Arguments:
!       Name            Type    I/O     Description
!       wk              int      I      workstation number
!
! Calling Sequence from C:
!       cgmGclwk(wk);
!
! Arguments:
!       Name            Type    I/O     Description
!       wk              int      I      workstation number
!
!-------------------------------------------------------------------------------
!
! Name:        GDAWK
!
! Abstract:    Deactivate Workstation
!
! Description: Deactivates the workstation
!
! Calling Sequence from Fortran 90:
!       call cgm_gsdawk(wk)
!
! Arguments:
!       Name            Type    I/O     Description
!       wk              integer  I      workstation number
!
! Calling Sequence from C:
!       cgmGsdawk(wk);
!
! Arguments:
!       Name            Type    I/O     Description
!       wk              int      I      workstation number
!
!-------------------------------------------------------------------------------
!
! Name:        GFA
!
! Abstract:    Draw Fill Area
!
! Description: Draw a filled area using X and Y arrays to define
!              the area
!
! Calling Sequence from Fortran 90:
!       call cgm_gfa(n,x,y)
!
! Arguments:
!       Name            Type    I/O     Description
!       n               integer  I      Number of points
!       x(:)            real     I      X values of points
!       y(:)            real     I      Y values of points
!
! Calling Sequence from C:
!       cgmGfa(n,x,y);
!
! Arguments:
!       Name            Type    I/O     Description
!       n               int      I      Number of points
!       x[]             float    I      X values of points
!       y[]             float    I      Y values of points
!
!-------------------------------------------------------------------------------
!
! Name:        GOPKS
!
! Abstract:    Open GKS
!
! Description: Opens CGM GKS and defines a FORTRAN logical unit number
!              to write error messages.
!
! Calling Sequence from Fortran 90:
!       call cgm_gopks(unit)
!
! Arguments:
!       Name            Type    I/O     Description
!       unit            integer  I      FORTRAN logical unit number
!                                       for error messages.
!
! Calling Sequence from C:
!       cgmGopks(unit);
!
! Arguments:
!       Name            Type    I/O     Description
!       unit            int      I      FORTRAN logical unit number
!                                       for error messages.
!
!-------------------------------------------------------------------------------
!
! Name:        GOPWK
!
! Abstract:    Open Workstation
!
! Description: Opens the CGM file
!
! Calling Sequence from Fortran 90:
!       call cgm_gopwk(wk,file,type)
!
! Arguments:
!       Name            Type     I/O     Description
!       wk              integer   I      workstation number
!       file            character I      CGM file name
!       type            character I      workstation type
!                                        (must be "CGM" or "CGMPIP")
!
! Calling Sequence from C:
!       cgmGopwk(wk,file,type);
!
! Arguments:
!       Name            Type    I/O     Description
!       wk              int      I      workstation number
!       file            char *   I      CGM file name
!       type            char *   I      workstation type
!                                       (must be "CGM" or "CGMPIP")
!
!-------------------------------------------------------------------------------
!
! Name:        GPL
!
! Abstract:    Draw Polyline
!
! Description: Draw a line given X and Y arrays to define points
!
! Calling Sequence from Fortran 90:
!       call cgm_gpl(n,x,y)
!
! Arguments:
!       Name            Type    I/O     Description
!       n               integer  I      Number of points
!       x(:)            real     I      X values of points
!       y(:)            real     I      Y values of points
!
! Calling Sequence from C:
!       cgmGpl(n,x,y);
!
! Arguments:
!       Name            Type    I/O     Description
!       n               int      I      Number of points
!       x[]             float    I      X values of points
!       y[]             float    I      Y values of points
!
!-------------------------------------------------------------------------------
!
! Name:        GPM
!
! Abstract:    Draw Polymarker
!
! Description: Draw a series of polymarkers using X and Y arrays to
!              the points
!
! Calling Sequence from Fortran 90:
!       call cgm_gpm(n,x,y)
!
! Arguments:
!       Name            Type    I/O     Description
!       n               integer  I      Number of points
!       x(:)            real     I      X values of points
!       y(:)            real     I      Y values of points
!
! Calling Sequence from C:
!       cgmGpm(n,x,y);
!
! Arguments:
!       Name            Type    I/O     Description
!       n               int      I      Number of points
!       x[]             float    I      X values of points
!       y[]             float    I      Y values of points
!
!-------------------------------------------------------------------------------
!
! Name:        GSCELL
!
! Abstract:    Draw Cell Array
!
! Description: Draws pixel data
!
! Calling Sequence from Fortran 90:
!       call cgm_gscell(x1,y1,x2,y2,nx,ny,array,dummy)
!
! Arguments:
!       Name            Type    I/O     Description
!       x1              real     I      X value of first point
!       y1              real     I      Y value of first point
!       x2              real     I      X value of second point
!       y2              real     I      Y value of second point
!       nx              integer  I      Number of values in X
!       ny              integer  I      Number of values in Y
!       array(:)        integer  I      Packed array of color indices
!       dummy           integer  I      Dummy parameter
!                                       (should be set to 1)
!
! Calling Sequence from C:
!       cgmGscell(x1,y1,x2,y2,nx,ny,array,dummy);
!
! Arguments:
!       Name            Type    I/O     Description
!       x1              float    I      X value of first point
!       y1              float    I      Y value of first point
!       x2              float    I      X value of second point
!       y2              float    I      Y value of second point
!       nx              int      I      Number of values in X
!       ny              int      I      Number of values in Y
!       array           char *        I Array of color indices
!       dummy           int      I      Dummy parameter
!                                       (should be set to 1)
!
!-------------------------------------------------------------------------------
!
! Name:        GSCHH
!
! Abstract:    Set Character Height
!
! Description: Set the character height using y-world (vertical)
!              coordinates
!
! Calling Sequence from Fortran 90:
!       call cgm_gschh(height)
!
! Arguments:
!       Name            Type    I/O     Description
!       height          real     I      character height
!
! Calling Sequence from C:
!       cgmGschh(height);
!
! Arguments:
!       Name            Type    I/O     Description
!       height          float    I      character height
!
!-------------------------------------------------------------------------------
!
! Name:        GSCHSP
!
! Abstract:    Set Character Spacing
!
! Description: The desired additional space to be added between
!              characters of a text string
!
! Calling Sequence from Fortran 90:
!       call cgm_gschsp(sp)
!
! Arguments:
!       Name            Type    I/O     Description
!       sp              real     I      character spacing
!
! Calling Sequence from C:
!       cgmGschsp(sp);
!
! Arguments:
!       Name            Type    I/O     Description
!       sp              float    I      character spacing
!
!-------------------------------------------------------------------------------
!
! Name:        GSCHUP
!
! Abstract:    Set Character Up Vector
!
! Description: Set the up direction for characters
!
! Calling Sequence from Fortran 90:
!       call cgm_gschup(x,y)
!
! Arguments:
!       Name            Type    I/O     Description
!       x               real     I      X vector component
!       y               real     I      Y vecotr component
!
! Calling Sequence from C:
!       cgmGschup(x,y);
!
! Arguments:
!       Name            Type    I/O     Description
!       x               float    I      X vector component
!       y               float    I      Y vecotr component
!
!-------------------------------------------------------------------------------
!
! Name:        GSCHXP
!
! Abstract:    Set Character Expansion Factor
!
! Description: The character expansion factor specifies the deviation
!              of the width-to-height ratio of the characters from the
!              ratio indicated by the font designer.
!
! Calling Sequence from Fortran 90:
!       call cgm_gschxp(xp)
!
! Arguments:
!       Name            Type    I/O     Description
!       xp              real     I      expansion factor
!
! Calling Sequence from C:
!       cgmGschxp(xp);
!
! Arguments:
!       Name            Type    I/O     Description
!       xp              float    I      expansion factor
!
!-------------------------------------------------------------------------------
!
! Name:        GSCLIP
!
! Abstract:    Set Clipping Indicator
!
! Description: Turn clipping off and on.  The clipping window will be
!              the current transformation.
!
! Calling Sequence from Fortran 90:
!       call cgm_gsclip(clip)
!
! Arguments:
!       Name            Type    I/O     Description
!       clip            integer  I      Clip Indicator
!                                       0 - clip off
!                                       1 - clip on
!
! Calling Sequence from C:
!       cgmGsclip(clip);
!
! Arguments:
!       Name            Type    I/O     Description
!       clip            int      I      Clip Indicator
!                                       0 - clip off
!                                       1 - clip on
!
!-------------------------------------------------------------------------------
!
! Name:        GSCR
!
! Abstract:    Set Color Representation
!
! Description: Define a color using red, green, and blue
!
! Calling Sequence from Fortran 90:
!       call cgm_gscr(wk,color,red,green,blue)
!
! Arguments:
!       Name            Type    I/O     Description
!       wk              integer  I      Workstation number
!       color           integer  I      Color Index
!       red             real     I      Red component (range 0 - 1)
!       green           real     I      Green component (range 0 - 1)
!       blue            real     I      Blue component (range 0 - 1)
!
! Calling Sequence from C:
!       cgmGscr(wk,color,red,green,blue);
!
! Arguments:
!       Name            Type    I/O     Description
!       wk              int      I      Workstation number
!       color           int      I      Color Index
!       red             float    I      Red component (range 0 - 1)
!       green           float    I      Green component (range 0 - 1)
!       blue            float    I      Blue component (range 0 - 1)
!
!-------------------------------------------------------------------------------
!
! Name:        GSELNT
!
! Abstract:    Select Normalization Transformation
!
! Description: Selects a transformation by transformation number.
!
! Calling Sequence from Fortran 90:
!       call cgm_gselnt(trans)
!
! Arguments:
!       Name            Type    I/O     Description
!       trans           integer  I      Transformation number
!
! Calling Sequence from C:
!       cgmGselnt(trans);
!
! Arguments:
!       Name            Type    I/O     Description
!       trans           int      I      Transformation number
!
!-------------------------------------------------------------------------------
!
! Name:        GSFACI
!
! Abstract:    Set Fill Area Color Index
!
! Description: Set the color index for filled areas
!
! Calling Sequence from Fortran 90:
!       call cgm_gsfaci(color)
!
! Arguments:
!       Name            Type    I/O     Description
!       color           integer  I      Color index
!
! Calling Sequence from C:
!       cgmGsfaci(color);
!
! Arguments:
!       Name            Type    I/O     Description
!       color           int      I      Color index
!
!-------------------------------------------------------------------------------
!
! Name:        GSFAIS
!
! Abstract:    Set Fill Area Interior Style
!
! Description: Set the fill style
!
! Calling Sequence from Fortran 90:
!       call cgm_gsfais(type)
!
! Arguments:
!       Name            Type    I/O     Description
!       type            integer  I      fill area interior style
!                                       0 - hollow
!                                       1 - solid
!                                       2 - pattern
!                                       3 - hatch
!
! Calling Sequence from C:
!       cgmGsfais(type);
!
! Arguments:
!       Name            Type    I/O     Description
!       type            int      I      fill area interior style
!                                       0 - hollow
!                                       1 - solid
!                                       2 - pattern
!                                       3 - hatch
!
!-------------------------------------------------------------------------------
!
! Name:        GSLN
!
! Abstract:    Set Linetype
!
! Description: Set the line type
!
! Calling Sequence from Fortran 90:
!       call cgm_gsln(type)
!
! Arguments:
!       Name            Type    I/O     Description
!       type            integer  I      Line type
!                                       1 = solid
!                                       2 = dash
!                                       3 = dot
!                                       4 = dash-dot
!
! Calling Sequence from C:
!       cgmGsln(type);
!
! Arguments:
!       Name            Type    I/O     Description
!       type            int      I      Line type
!                                       1 = solid
!                                       2 = dash
!                                       3 = dot
!                                       4 = dash-dot
!
!-------------------------------------------------------------------------------
!
! Name:        GSLWSC
!
! Abstract:    Set Linewidth Scale Factor
!
! Description: Defines the line width in terms of a scale
!              factor applied to the default line width
!
! Calling Sequence from Fortran 90:
!       call cgm_gslwsc(width)
!
! Arguments:
!       Name            Type    I/O     Description
!       width           real     I      linewidth scale factor
!
! Calling Sequence from C:
!       cgmGslwsc(width);
!
! Arguments:
!       Name            Type    I/O     Description
!       width           float    I      linewidth scale factor
!
!-------------------------------------------------------------------------------
!
! Name:        GSMK
!
! Abstract:    Set Marker Type
!
! Description: Set the marker type
!
! Calling Sequence from Fortran 90:
!       call cgm_gsmk(type)
!
! Arguments:
!       Name            Type    I/O     Description
!       type            integer  I      marker type
!                                       GKS marker types are:
!                                       1 - point
!                                       2 - plus
!                                       3 - asterisk
!                                       4 - o mark
!                                       5 - x mark
!
! Calling Sequence from C:
!       cgmGsmk(type);
!
! Arguments:
!       Name            Type    I/O     Description
!       type            int      I      marker type
!                                       GKS marker types are:
!                                       1 - point
!                                       2 - plus
!                                       3 - asterisk
!                                       4 - o mark
!                                       5 - x mark
!
!-------------------------------------------------------------------------------
!
! Name:        GSPLCI
!
! Abstract:    Set Line Color Index
!
! Description: Set the color index for lines
!
! Calling Sequence from Fortran 90:
!       call cgm_gsplci(color)
!
! Arguments:
!       Name            Type    I/O     Description
!       color           integer  I      Color index
!
! Calling Sequence from C:
!       cgmGsplci(color);
!
! Arguments:
!       Name            Type    I/O     Description
!       color           int      I      Color index
!
!-------------------------------------------------------------------------------
!
! Name:        GSPMCI
!
! Abstract:    Set Marker Color Index
!
! Description: Set the color index for markers
!
! Calling Sequence from Fortran 90:
!       call cgm_gspmci(color)
!
! Arguments:
!       Name            Type    I/O     Description
!       color           integer  I      Color index
!
! Calling Sequence from C:
!       cgmGspmci(color);
!
! Arguments:
!       Name            Type    I/O     Description
!       color           int      I      Color index
!
!-------------------------------------------------------------------------------
!
! Name:        GSTAL
!
! Abstract:    Set Text Alignment
!
! Description: Set alignment for text
!
! Calling Sequence from Fortran 90:
!       call cgm_gstxal(h,v)
!
! Arguments:
!       Name            Type    I/O     Description
!       h               integer  I      horizonal text alignment
!                                       0 - normal (depends on path)
!                                       1 - left
!                                       2 - center
!                                       3 - right
!       v               integer  I      vertical text alignment
!                                       0 - normal (depends on path)
!                                       1 - top
!                                       2 - cap
!                                       3 - half
!                                       4 - base
!                                       5 - bottom
!
! Calling Sequence from C:
!       cgmGstxal(h,v);
!
! Arguments:
!       Name            Type    I/O     Description
!       h               int      I      horizonal text alignment
!                                       0 - normal (depends on path)
!                                       1 - left
!                                       2 - center
!                                       3 - right
!       v               int      I      vertical text alignment
!                                       0 - normal (depends on path)
!                                       1 - top
!                                       2 - cap
!                                       3 - half
!                                       4 - base
!                                       5 - bottom
!
!       Path    Normal Horizonal        Normal Vertical
!       ----    ----------------        ---------------
!       right   left                    baseline
!       left    right                   baseline
!       up      center                  baseline
!       down    center                  top
!
!-------------------------------------------------------------------------------
!
! Name:        GSTXCI
!
! Abstract:    Set Text Color Index
!
! Description: Set the color index for text
!
! Calling Sequence from Fortran 90:
!       call cgm_gstxci(color)
!
! Arguments:
!       Name            Type    I/O     Description
!       color           integer  I      Color index
!
! Calling Sequence from C:
!       cgmGstxci(color);
!
! Arguments:
!       Name            Type    I/O     Description
!       color           int      I      Color index
!
!-------------------------------------------------------------------------------
!
! Name:        GSTXFP
!
! Abstract:    Set Text Font and Precision
!
! Description: Set the text font index and precision.  The precision
!              controls the accuracy of the execution of text attributes.
!              If "string" precision, only the text position is
!              guaranteed, and the manner in which string clipping
!              is implemented is implementation dependent.
!              If "character" precision, the starting position of each
!              character satisfies the relevant text attributes, thus
!              guaranteeing orientation and placement; however, skew,
!              orientation, and size of each character are not guaranteed.
!              If "stroke" precision, the placement, skew, orientation,
!              and size of all characters satisfy all text attributes.
!
! Calling Sequence from Fortran 90:
!       call cgm_gstxfp(font,prec)
!
! Arguments:
!       Name            Type    I/O     Description
!       font            integer  I      text font
!                                       1  - PIP/Mono_Sans_Serif
!                                       2  - Hershey/Cartographic_Roman
!                                       3  - Hershey/Complex_Script
!                                       4  - Hershey/Simplex_Script
!                                       5  - Hershey/Complex_Italic
!                                       6  - Hershey/Triplex_Italic
!                                       7  - Hershey/Complex_Greek
!                                       8  - Hershey/Complex_Greek
!                                       9  - Hershey/Triplex_Roman
!                                       10 - Hershey/Complex_Roman
!                                       11 - Hershey/Gothic_English
!                                       12 - Hershey/Gothic_German
!                                       13 - Hershey/Gothic_Italian
!                                       14 - Hershey/Duplex_Roman
!                                       15 - Hershey/Simplex_Roman
!                                       16 - Hershey/Complex_Cyrillic
!                                       17 - Hershey/Simplex_Roman
!                                       18 - Hershey/Simplex_Roman
!                                       19 - Hershey/Simplex_Roman
!                                       20 - Helvetica
!                                       21 - Hershey/Duplex_Roman
!                                       22 - Helvetica
!                                       23 - Helvetica_Oblique
!                                       24 - Helvetica_Bold
!                                       25 - Helvetica_Bold_Oblique
!                                       26 - Times_Roman
!                                       27 - Times_Italic
!                                       28 - Times_Bold
!                                       29 - Times_Bold_Italic
!       precision       integer  I      text precision
!                                       0 - string
!                                       1 - character
!                                       2 - stroke
!
! Calling Sequence from C:
!       cgmGstxfp(font,prec);
!
! Arguments:
!       Name            Type    I/O     Description
!       font            int      I      text font
!                                       1  - PIP/Mono_Sans_Serif
!                                       2  - Hershey/Cartographic_Roman
!                                       3  - Hershey/Complex_Script
!                                       4  - Hershey/Simplex_Script
!                                       5  - Hershey/Complex_Italic
!                                       6  - Hershey/Triplex_Italic
!                                       7  - Hershey/Complex_Greek
!                                       8  - Hershey/Complex_Greek
!                                       9  - Hershey/Triplex_Roman
!                                       10 - Hershey/Complex_Roman
!                                       11 - Hershey/Gothic_English
!                                       12 - Hershey/Gothic_German
!                                       13 - Hershey/Gothic_Italian
!                                       14 - Hershey/Duplex_Roman
!                                       15 - Hershey/Simplex_Roman
!                                       16 - Hershey/Complex_Cyrillic
!                                       17 - Hershey/Simplex_Roman
!                                       18 - Hershey/Simplex_Roman
!                                       19 - Hershey/Simplex_Roman
!                                       20 - Helvetica
!                                       21 - Hershey/Duplex_Roman
!                                       22 - Helvetica
!                                       23 - Helvetica_Oblique
!                                       24 - Helvetica_Bold
!                                       25 - Helvetica_Bold_Oblique
!                                       26 - Times_Roman
!                                       27 - Times_Italic
!                                       28 - Times_Bold
!                                       29 - Times_Bold_Italic
!       precision       int      I      text precision
!                                       0 - string
!                                       1 - character
!                                       2 - stroke
!
!-------------------------------------------------------------------------------
!
! Name:        GSTXP
!
! Abstract: Set Text Path
!
! Description:
!
! Calling Sequence from Fortran 90:
!       call cgm_gstxp(path)
!
! Arguments:
!       Name            Type    I/O     Description
!       path            integer  I      text path
!                                       0 - right
!                                       1 - left
!                                       2 - up
!                                       3 - down
!
! Calling Sequence from C:
!       cgmGstxp(path);
!
! Arguments:
!       Name            Type    I/O     Description
!       path            int      I      text path
!                                       0 - right
!                                       1 - left
!                                       2 - up
!                                       3 - down
!
!-------------------------------------------------------------------------------
!
! Name:        GSVP
!
! Abstract:    Set Viewport
!
! Description: Defines a view port for a transformation
!
! Calling Sequence from Fortran 90:
!       call cgm_gsvp(trans,xmin,xmax,ymin,ymax)
!
! Arguments:
!       Name            Type    I/O     Description
!       trans           integer  I      Transformation number
!       xmin            real     I      Minimum X normalized coordinate
!       xmax            real     I      Maximum X normalized coordinate
!       ymin            real     I      Minumum Y normalized coordinate
!       ymax            real     I      Maximum Y normalized coordinate
!
! Calling Sequence from C:
!       cgmGsvp(trans,xmin,xmax,ymin,ymax);
!
! Arguments:
!       Name            Type    I/O     Description
!       trans           int      I      Transformation number
!       xmin            float    I      Minimum X normalized coordinate
!       xmax            float    I      Maximum X normalized coordinate
!       ymin            float    I      Minumum Y normalized coordinate
!       ymax            float    I      Maximum Y normalized coordinate
!
!-------------------------------------------------------------------------------
!
! Name:        GSWKVP
!
! Abstract:    Set Workstation Viewport
!
! Description: Maps the normalized coordinates into plotter inches
!
! Calling Sequence from Fortran 90:
!       call cgm_gswkvp(wk,xmin,xmax,ymin,ymax)
!
! Arguments:
!       Name            Type    I/O     Description
!       wk              integer  I      workstation number
!       xmin            real     I      Minimum X plotter coordinate
!       xmax            real     I      Maximum X plotter coordinate
!       ymin            real     I      Minumum Y plotter coordinate
!       ymax            real     I      Maximum Y plotter coordinate
!
! Calling Sequence from C:
!       cgmGswkvp(wk,xmin,xmax,ymin,ymax);
!
! Arguments:
!       Name            Type    I/O     Description
!       wk              int      I      workstation number
!       xmin            float    I      Minimum X plotter coordinate
!       xmax            float    I      Maximum X plotter coordinate
!       ymin            float    I      Minumum Y plotter coordinate
!       ymax            float    I      Maximum Y plotter coordinate
!
!-------------------------------------------------------------------------------
!
! Name:        GSWN
!
! Abstract:    Set Window
!
! Description: Defines a world coordinate transformation
!
! Calling Sequence from Fortran 90:
!       call cgm_gswn(trans,xmin,xmax,ymin,ymax)
!
! Arguments:
!       Name            Type    I/O     Description
!       trans           integer  I      Transformation number
!       xmin            real     I      Minimum X world coordinate
!       xmax            real     I      Maximum X world coordinate
!       ymin            real     I      Minumum Y world coordinate
!       ymax            real     I      Maximum Y world coordinate
!
! Calling Sequence from C:
!       cgmGswn(trans,xmin,xmax,ymin,ymax);
!
! Arguments:
!       Name            Type    I/O     Description
!       trans           int      I      Transformation number
!       xmin            float    I      Minimum X world coordinate
!       xmax            float    I      Maximum X world coordinate
!       ymin            float    I      Minumum Y world coordinate
!       ymax            float    I      Maximum Y world coordinate
!
!-------------------------------------------------------------------------------
!
! Name:        GTX
!
! Abstract:    Draw Text
!
! Description: Using the point (x,y) and text attributes draw text
!
! Calling Sequence from Fortran 90:
!       call cgm_gtx(x,y,text)
!
! Arguments:
!       Name            Type     I/O     Description
!       x               real      I      X value of point
!       y               real      I      Y value of point
!       text            character I      text string
!
! Calling Sequence from C:
!       cgmGtx(x,y,text);
!
! Arguments:
!       Name            Type    I/O     Description
!       x               float    I      X value of point
!       y               float    I      Y value of point
!       text            char *   I      text string
!
!-------------------------------------------------------------------------------
!
! Name:        GTXR
!
! Abstract:    Draw Restricted Text
!
! Description: Draw text constrained to the parallelogram determined
!              by delta x, delta y, and the point (x,y).  The text
!              attributes will be honored if possible.
!
! Calling Sequence from Fortran 90:
!       call cgm_gtxr(dx,dy,x,y,text)
!
! Arguments:
!       Name            Type     I/O     Description
!       dx              real      I      delta width
!       dy              real      I      delta height
!       x               real      I      X value of point
!       y               real      I      Y value of point
!       text            character I      text string
!
! Calling Sequence from C:
!       cgmGtxr(dx,dy,x,y,text);
!
! Arguments:
!       Name            Type    I/O     Description
!       dx              float    I      delta width
!       dy              float    I      delta height
!       x               float    I      X value of point
!       y               float    I      Y value of point
!       text            char *   I      text string
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_BEGIN_TRACE_GROUP
!
! Abstract:    CGM*PIP Begin Trace Group
!
! Description: Indicates the beginning of a trace group.  Up until the
!              next PIP_END_TRACE_GROUP call, all traces whose
!              member group identifier matches the identifier of this
!              group are members of this group.  A picture may have
!              multiple trace groups; however, one group definition
!              shall be complete before another may begin.
!
! Calling Sequence from Fortran 90:
!       call cgm_pip_begin_trace_group(grp,pos_pri,neg_pri,pos_mode,
!                                   neg_mode)
!
! Arguments:
!       Name            Type    I/O     Description
!       grp             integer  I      Group Identifier
!       pos_pri         integer  I      Positive display priority
!                                       0 - lowest
!                                       1 - highest
!       neg_pri         integer  I      Negative display priority
!                                       0 - lowest
!                                       1 - highest
!       pos_mode        integer  I      Positive lobe drawing mode
!                                       1 - opaque
!                                       2 - color blending
!       neg_mode        integer  I      Negative lobe drawing mode
!                                       1 - opaque
!                                       2 - color blending
!
! Calling Sequence from C:
!       cgmPipBeginTraceGroup(grp,pos_pri,neg_pri,pos_mode,
!                                   neg_mode);
!
! Arguments:
!       Name            Type    I/O     Description
!       grp             int      I      Group Identifier
!       pos_pri         int      I      Positive display priority
!                                       0 - lowest
!                                       1 - highest
!       neg_pri         int      I      Negative display priority
!                                       0 - lowest
!                                       1 - highest
!       pos_mode        int      I      Positive lobe drawing mode
!                                       1 - opaque
!                                       2 - color blending
!       neg_mode        int      I      Negative lobe drawing mode
!                                       1 - opaque
!                                       2 - color blending
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_BG_FILL_ALIGNMENT
!
! Abstract:    CGM*PIP Trace Background Fill Alignment
!
! Description: Specifies whether the background fill cells are edge
!              or center aligned on each sample point.
!
! Calling Sequence from Fortran 90:
!       call cgm_pip_bg_fill_alignment(bg_align)
!
! Arguments:
!       Name            Type    I/O     Description
!       bg_align        integer  I      Specifies how background fill
!                                       is aligned on each sample point
!                                       0 - edge
!                                       1 - center
!
! Calling Sequence from C:
!       cgmPipBgFillAlignment(bg_align);
!
! Arguments:
!       Name            Type    I/O     Description
!       bg_align        int      I      Specifies how background fill
!                                       is aligned on each sample point
!                                       0 - edge
!                                       1 - center
!
!-------------------------------------------------------------------------------
!
! Name:        BG_FILL_BOUNDARIES
!
! Abstract:    CGM*PIP Trace Background Fill Boundaries
!
! Description: Defines the boundaries to use when the "backgorund fill"
!              trace display mode is selected.  The values are signed
!              offsets from the baseline and are specified as a fraction
!              of the nominal maximum sample displacement from the
!              baseline, which is the product of the amplitude scale
!              factor and the magnitude of the amplitude vector.
!
! Calling Sequence from Fortran 90:
!       call cgm_pip_trace_bg_fill(pos_fill,neg_fill)
!
! Arguments:
!       Name            Type    I/O     Description
!       pos_fill        real     I      Positive fill boundary
!                                       (a signed offset from the baseline)
!       neg_fill        real     I      Negative fill boundary
!                                       (a signed offset from the baseline)
!
! Calling Sequence from C:
!       cgmPipTraceBgFill(pos_fill,neg_fill);
!
! Arguments:
!       Name            Type    I/O     Description
!       pos_fill        float    I      Positive fill boundary
!                                       (a signed offset from the baseline)
!       neg_fill        float    I      Negative fill boundary
!                                       (a signed offset from the baseline)
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_BG_FILL_CONSTANT_COLOR
!
! Abstract:    CGM*PIP Trace Background Fill Constant Color
!
! Description: The color to be used when the background fill style
!              is constant color.
!
! Calling Sequence from Fortran 90:
!       call cgm_pip_bg_fill_const_color(const_col)
!
! Arguments:
!       Name            Type    I/O     Description
!       const_col       integer  I      Constant color index
!                                       (used only when fill_style = 0)
!
! Calling Sequence from C:
!       cgmPipBgFillConstColor(const_col);
!
! Arguments:
!       Name            Type    I/O     Description
!       const_col       int      I      Constant color index
!                                       (used only when fill_style = 0)
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_BG_FILL_INTERPOLATION
!
! Abstract:    CGM*PIP Trace Background Fill Color Interpolation Mode
!
! Description: Indicates whether or not color interpolation is to be
!              performed when the background fill style is "variant
!              color".
!
! Calling Sequence from Fortran 90:
!       call cgm_pip_bg_fill_interpolation(imode)
!
! Arguments:
!       Name            Type    I/O     Description
!       imode           integer  I      Interpolation mode
!                                       0 - no interpolation
!                                       1 - 2D interpolation
!
! Calling Sequence from C:
!       cgmPipBgFillInterpolation(imode);
!
! Arguments:
!       Name            Type    I/O     Description
!       imode           int      I      Interpolation mode
!                                       0 - no interpolation
!                                       1 - 2D interpolation
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_BG_FILL_NULL_COLOR
!
! Abstract:    CGM*PIP Trace Background Fill Null Color
!
! Description: The color value which is used to inhibit color
!              interpolation.
!
! Calling Sequence from Fortran 90:
!       call cgm_pip_bg_fill_null_color(null_col)
!
! Arguments:
!       Name            Type    I/O     Description
!       null_col        integer  I      Null color
!
! Calling Sequence from C:
!       cgmPipBgFillNullColor(null_col);
!
! Arguments:
!       Name            Type    I/O     Description
!       null_col        int      I      Null color
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_VA_FILL_STYLE
!
! Abstract:    CGM*PIP Trace Variable-Area Fill Style
!
! Description: The variable-area fill style indicates whether the
!              VA fill is "constant color", "variant color", or
!              "constant pattern".
! Calling Sequence from Fortran 90:
!       call cgm_pip_va_fill_style(istyle)
!
! Arguments:
!       Name            Type    I/O     Description
!       istyle          integer  I      VA fill style type
!                                       0 - constant color
!                                       1 - variant color
!                                       2 - constant pattern
!
! Calling Sequence from C:
!       cgmPipVaFillStyle(istyle)
!
! Arguments:
!       Name            Type    I/O     Description
!       istyle          int      I      VA fill style type
!                                       0 - constant color
!                                       1 - variant color
!                                       2 - constant pattern
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_END_TRACE_GROUP
!
! Abstract:    CGM*PIP End Trace Group
!
! Description: Indicates the end of a trace group.
!
! Calling Sequence from Fortran 90:
!       call cgm_pip_end_trace_group()
!
! Arguments:
!       Name            Type    I/O     Description
!
! Calling Sequence from C:
!       cgmPipEndTraceGroup();
!
! Arguments:
!       Name            Type    I/O     Description
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_TRACE
!
! Abstract:    Draw CGM*PIP Trace
!
! Description: Draw a seismic trace using data provided and apply
!              all trace attributes previously defined.
!
! Calling Sequence from Fortran 90:
!       call cgm_pip_trace(x0,y0,grp,fac,tr,n_tr,va_cols,n_va_cols,
!                       bg_cols,n_bg_cols)
!
! Arguments:
!       Name            Type    I/O     Description
!       x0              real     I      X starting position of trace
!       y0              real     I      Y starting position of trace
!       grp             integer  I      Trace group member identifier
!       fac             real     I      sample scaling factor into
!                                       world coordinates
!       tr(:)           real     I      trace sample values
!       n_tr            integer  I      number of trace samples
!       va_cols(:)      integer  I      Variable area fill color array
!       n_va_cols       integer  I      number of VA colors
!       bg_cols(:)      integer  I      Background fill color array
!       n_bg_cols       integer  I      number of background fill
!                                       colors
!
! Calling Sequence from C:
!       cgmPipTrace(x0,y0,grp,fac,tr,n_tr,va_cols,n_va_cols,
!                       bg_cols,n_bg_cols);
!
! Arguments:
!       Name            Type    I/O     Description
!       x0              float    I      X starting position of trace
!       y0              float    I      Y starting position of trace
!       grp             int      I      Trace group member identifier
!       fac             float    I      sample scaling factor into
!                                       world coordinates
!       tr[]            float    I      trace sample values
!       n_tr            int      I      number of trace samples
!       va_cols[]       int      I      Variable area fill color array
!       n_va_cols       int      I      number of VA colors
!       bg_cols[]       int      I      Background fill color array
!       n_bg_cols       int      I      number of background fill
!                                       colors
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_TRACE_BG_FILL
!
! Abstract:    CGM*PIP Trace Background Fill Attributes
!
! Description: Specify the background fill-area attributes for drawing
!              the trace.
!
! Calling Sequence from Fortran 90:
!       call cgm_pip_trace_bg_fill(pos_fill,neg_fill,imode,fill_style,
!                               null_col,const_col,bg_align)
!
! Arguments:
!       Name            Type    I/O     Description
!       pos_fill        real     I      Positive fill boundary
!                                       (a signed offset from the baseline)
!       neg_fill        real     I      Negative fill boundary
!                                       (a signed offset from the baseline)
!       imode           integer  I      Interpolation mode
!                                       0 - no interpolation
!                                       1 - 2D interpolation
!       fill_style      integer  I      Background fill style
!                                       0 - constant color
!                                       1 - variant color
!       null_col        integer  I      Null color
!                                       (used only when imode = 1)
!       const_col       integer  I      Constant color index
!                                       (used only when fill_style = 0)
!       bg_align        integer  I      Specifies how background fill
!                                       is aligned on each sample point
!                                       0 - edge
!                                       1 - center
!
! Calling Sequence from C:
!       cgmPipTraceBgFill(pos_fill,neg_fill,imode,fill_style,
!                               null_col,const_col,bg_align);
!
! Arguments:
!       Name            Type    I/O     Description
!       pos_fill        float    I      Positive fill boundary
!                                       (a signed offset from the baseline)
!       neg_fill        float    I      Negative fill boundary
!                                       (a signed offset from the baseline)
!       imode           int      I      Interpolation mode
!                                       0 - no interpolation
!                                       1 - 2D interpolation
!       fill_style      int      I      Background fill style
!                                       0 - constant color
!                                       1 - variant color
!       null_col        int      I      Null color
!                                       (used only when imode = 1)
!       const_col       int      I      Constant color index
!                                       (used only when fill_style = 0)
!       bg_align        int      I      Specifies how background fill
!                                       is aligned on each sample point
!                                       0 - edge
!                                       1 - center
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_TRACE_DISPLAY_MODES
!
! Abstract:    CGM*PIP Trace Display Modes
!
! Description: Specify the trace display modes
! Calling Sequence from Fortran 90:
!       call cgm_pip_trace_display_modes(modes,n)
!
! Arguments:
!       Name            Type    I/O     Description
!       modes(:)        integer  I      Array of display modes
!                                       List may contain:
!                                       1 - Wiggle
!                                       2 - Positive VA fill
!                                       3 - Negative VA fill
!                                       4 - Background fill
!
! Calling Sequence from C:
!       cgmPipTraceDisplayModes(modes,n);
!
! Arguments:
!       Name            Type    I/O     Description
!       modes[]         int      I      Array of display modes
!                                       List may contain:
!                                       1 - Wiggle
!                                       2 - Positive VA fill
!                                       3 - Negative VA fill
!                                       4 - Background fill
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_TRACE_ORIENTATION
!
! Abstract:    CGM*PIP Trace Orientation
!
! Description: Specify the baseline and amplitude direction vectors
!              which define the baseline axis (zero line) and amplitude
!              axis (positive sample direction) of the trace.  These
!              vectors must be orthogonal.
! Calling Sequence from Fortran 90:
!       call cgm_pip_trace_orientation(bx,by,ax,ay)
!
! Arguments:
!       Name            Type    I/O     Description
!       bx              real     I      X component of baseline vector
!       by              real     I      Y component of baseline vector
!       ax              real     I      X component of amplitude vector
!       ay              real     I      Y component of amplitude vector
!
! Calling Sequence from C:
!       cgmPipTraceOrientation(bx,by,ax,ay);
!
! Arguments:
!       Name            Type    I/O     Description
!       bx              float    I      X component of baseline vector
!       by              float    I      Y component of baseline vector
!       ax              float    I      X component of amplitude vector
!       ay              float    I      Y component of amplitude vector
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_TRACE_SCALE_FACTORS
!
! Abstract:    CGM*PIP Trace Scale Factors
!
! Description: Specify the baseline and amplitude scale factors
!              which define the scaling of the trace in the baseline
!              and amplitude directions
! Calling Sequence from Fortran 90:
!       call cgm_pip_trace_scale_factors(base,ampl)
!
! Arguments:
!       Name            Type    I/O     Description
!       base            real     I      Baseline scale factor
!       ampl            real     I      Amplitude scale factor
!
! Calling Sequence from C:
!       cgmPipTraceScaleFactors(base,ampl);
!
! Arguments:
!       Name            Type    I/O     Description
!       base            float    I      Baseline scale factor
!       ampl            float    I      Amplitude scale factor
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_TRACE_VA_FILL
!
! Abstract:    CGM*PIP Trace Variable-Area Attributes
!
! Description: Specify the variable-area attributes for drawing the trace.
! Calling Sequence from Fortran 90:
!       call cgm_pip_trace_va_fill(xpmin,xpmax,xmmin,xnmax,istyle,
!                               ipcol,imcol,ippat,impat,vc_align)
!
! Arguments:
!       Name            Type    I/O     Description
!       xpmin           real     I      Minimum positive fill boundary
!       xpmax           real     I      Maximum positive fill boundary
!       xmmin           real     I      Minimum negative fill boundary
!       xmmax           real     I      Maximum negative fill boundary
!       istyle          integer  I      VA fill style type
!                                       0 - constant color
!                                       1 - variant color
!                                       2 - constant pattern
!       ipcol           integer  I      Color index for positive VA fill
!                                       (used oly when istyle is 0)
!       imcol           integer  I      Color index for negative VA fill
!                                       (used oly when istyle is 0)
!       ippat           integer  I      Pattern index for positive VA fill
!                                       (used oly when istyle is 2)
!       impat           integer  I      Pattern index for negative VA fill
!                                       (used oly when istyle is 2)
!       vc_align        integer  I      Specifies how the colored bands
!                                       of the VA fill are positioned with
!                                       respect to the sample positions
!                                       0 - edge
!                                       1 - center
!                                       (used oly when istyle is 1)
!
! Calling Sequence from C:
!       cgmPipTraceVaFill(xpmin,xpmax,xmmin,xnmax,istyle,
!                               ipcol,imcol,ippat,impat,vc_align);
!
! Arguments:
!       Name            Type    I/O     Description
!       xpmin           float    I      Minimum positive fill boundary
!       xpmax           float    I      Maximum positive fill boundary
!       xmmin           float    I      Minimum negative fill boundary
!       xmmax           float    I      Maximum negative fill boundary
!       istyle          int      I      VA fill style type
!                                       0 - constant color
!                                       1 - variant color
!                                       2 - constant pattern
!       ipcol           int      I      Color index for positive VA fill
!                                       (used oly when istyle is 0)
!       imcol           int      I      Color index for negative VA fill
!                                       (used oly when istyle is 0)
!       ippat           int      I      Pattern index for positive VA fill
!                                       (used oly when istyle is 2)
!       impat           int      I      Pattern index for negative VA fill
!                                       (used oly when istyle is 2)
!       vc_align        int      I      Specifies how the colored bands
!                                       of the VA fill are positioned with
!                                       respect to the sample positions
!                                       0 - edge
!                                       1 - center
!                                       (used oly when istyle is 1)
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_VA_FILL_ALIGNMENT
!
! Abstract:    CGM*PIP Trace Variable-Area Fill Alignment
!
! Description: The value of the fill alignment parameter specifies how
!              the colored bands of the VA fill area, when the VA fill
!              style is "variant color", are positioned with respect to
!              the sample positions of the trace.
! Calling Sequence from Fortran 90:
!       call cgm_pip_va_fill_alignment(vc_align)
!
! Arguments:
!       Name            Type    I/O     Description
!       vc_align        integer  I      Specifies how the colored bands
!                                       of the VA fill are positioned with
!                                       respect to the sample positions
!                                       0 - edge
!                                       1 - center
!
! Calling Sequence from C:
!       cgmPipVaFillAlignment(vc_align);
!
! Arguments:
!       Name            Type    I/O     Description
!       vc_align        int      I      Specifies how the colored bands
!                                       of the VA fill are positioned with
!                                       respect to the sample positions
!                                       0 - edge
!                                       1 - center
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_VA_FILL_CONSTANT_COLOR
!
! Abstract:    CGM*PIP Trace Variable-Area Fill Constant Color
!
! Description: When positive VA fill mode is selected, and when the
!              VA Fill Style is "constant" color, then the color
!              specified by ipcol is used to fill positive lobes.
!              Similarly when negative VA fill mode is selected,
!              and when VA Fill Style is "constant color", then
!              imcol defines the color used to fill negative lobes.
! Calling Sequence from Fortran 90:
!       call cgm_pip_va_fill_const_color(ipcol,imcol)
!
! Arguments:
!       Name            Type    I/O     Description
!       ipcol           integer  I      Color index for positive VA fill
!                                       (used oly when istyle is 0)
!       imcol           integer  I      Color index for negative VA fill
!                                       (used oly when istyle is 0)
!
! Calling Sequence from C:
!       cgmPipVaFillConstColor(ipcol,imcol);
!
! Arguments:
!       Name            Type    I/O     Description
!       ipcol           int      I      Color index for positive VA fill
!                                       (used oly when istyle is 0)
!       imcol           int      I      Color index for negative VA fill
!                                       (used oly when istyle is 0)
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_VA_FILL_CONST_PATTERN
!
! Abstract:    CGM*PIP Trace Variable-Area Fill Constant Patterns
!
! Description: When VA fill mode is selected, and when the VA fill
!              style is "constant pattern", then ippat and impat
!              define the pattern indexes used to fill positive and
!              negative lobes, respectively.
! Calling Sequence from Fortran 90:
!       call cgm_pip_va_fill_const_pattern(ippat,impat)
!
! Arguments:
!       Name            Type    I/O     Description
!       ippat           integer  I      Pattern index for positive VA fill
!                                       (used oly when istyle is 2)
!       impat           integer  I      Pattern index for negative VA fill
!                                       (used oly when istyle is 2)
!
! Calling Sequence from C:
!       cgmPipVaFillConstPattern(ippat,impat);
!
! Arguments:
!       Name            Type    I/O     Description
!       ippat           int      I      Pattern index for positive VA fill
!                                       (used oly when istyle is 2)
!       impat           int      I      Pattern index for negative VA fill
!                                       (used oly when istyle is 2)
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_VA_FILL_NEG_BOUNDARIES
!
! Abstract:    CGM*PIP Trace Variable-Area Negative Fill Boundaries
!
! Description: The minumum and maximum negative fill boundaries are
!              signed offsets from the baseline.  They are specified
!              as a fraction of the nominal maximum sample displacement
!              from the baseline, which is the product of the amplitude
!              scale factor and the magnitude of the amplitude vector.
! Calling Sequence from Fortran 90:
!       call cgm_pip_va_fill_neg_bound(xmmin,xnmax)
!
! Arguments:
!       Name            Type    I/O     Description
!       xmmin           real     I      Minimum negative fill boundary
!       xmmax           real     I      Maximum negative fill boundary
!
! Calling Sequence from C:
!       cgmPipVaFillNegBound(xmmin,xnmax)
!
! Arguments:
!       Name            Type    I/O     Description
!       xmmin           float    I      Minimum negative fill boundary
!       xmmax           float    I      Maximum negative fill boundary
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_VA_FILL_POS_BOUNDARIES
!
! Abstract:    CGM*PIP Trace Variable-Area Positive Fill Boundaries
!
! Description: The minumum and maximum positive fill boundaries are
!              signed offsets from the baseline.  They are specified
!              as a fraction of the nominal maximum sample displacement
!              from the baseline, which is the product of the amplitude
!              scale factor and the magnitude of the amplitude vector.
! Calling Sequence from Fortran 90:
!       call cgm_pip_va_fill_pos_bound(xpmin,xpmax)
!
! Arguments:
!       Name            Type    I/O     Description
!       xpmin           real     I      Minimum positive fill boundary
!       xpmax           real     I      Maximum positive fill boundary
!
! Calling Sequence from C:
!       cgmPipVaFillPosBound(xpmin,xpmax)
!
! Arguments:
!       Name            Type    I/O     Description
!       xpmin           float    I      Minimum positive fill boundary
!       xpmax           float    I      Maximum positive fill boundary
!
!-------------------------------------------------------------------------------
!
! Name:        PIP_VA_FILL_STYLE
!
! Abstract:    CGM*PIP Trace Variable-Area Fill Style
!
! Description: The variable-area fill style indicates whether the
!              VA fill is "constant color", "variant color", or
!              "constant pattern".
! Calling Sequence from Fortran 90:
!       call cgm_pip_va_fill_style(istyle)
!
! Arguments:
!       Name            Type    I/O     Description
!       istyle          integer  I      VA fill style type
!                                       0 - constant color
!                                       1 - variant color
!                                       2 - constant pattern
!
! Calling Sequence from C:
!       cgmPipVaFillStyle(istyle)
!
! Arguments:
!       Name            Type    I/O     Description
!       istyle          int      I      VA fill style type
!                                       0 - constant color
!                                       1 - variant color
!                                       2 - constant pattern
!
!-------------------------------------------------------------------------------
!
! Name:        SET_MARKER_ANGLE
!
! Abstract:    Set Marker Angle
!
! Description: Set the marker angle in degrees
!
! Calling Sequence from Fortran 90:
!       call cgm_set_marker_angle(angle)
!
! Arguments:
!       Name            Type    I/O     Description
!       angle           real     I      Marker angle in degrees
!
! Calling Sequence from C:
!       cgmSetMarkerAngle(angle);
!
! Arguments:
!       Name            Type    I/O     Description
!       angle           float    I      Marker angle in degrees
!
!-------------------------------------------------------------------------------
!
! Name:        SET_MARKER_FONT
!
! Abstract:    Set Marker Font
!
! Description: Load stroked marker font set
!
! Calling Sequence from Fortran 90:
!       call cgm_set_marker_font(set)
!
! Arguments:
!       Name            Type    I/O     Description
!       set             integer  I      marker font set
!                                       0 - CGM default markers
!                                       1 - Congraf markers
!                                       2 - Geobase markers
!
! Calling Sequence from C:
!       cgmSetMarkerFont(set);
!
! Arguments:
!       Name            Type    I/O     Description
!       set             int      I      marker font set
!                                       0 - CGM default markers
!                                       1 - Congraf markers
!                                       2 - Geobase markers
!
!-------------------------------------------------------------------------------
!
! Name:        SET_MARKER_SIZE
!
! Abstract:    Set Absolute Marker Size
!
! Description: Set the marker size in inches
!
! Calling Sequence from Fortran 90:
!       call cgm_set_marker_size(size)
!
! Arguments:
!       Name            Type    I/O     Description
!       size            real     I      Marker size in inches
!
! Calling Sequence from C:
!       cgmSetMarkerSize(size);
!
! Arguments:
!       Name            Type    I/O     Description
!       size            float    I      Marker size in inches

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
!     Date        Author       Description
!     ----        ------       -----------
!  1. 2001-04-16  Vunderink    Initial version.
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


!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!
!!-------------------------- start of module ------------------------------!!


      module cgm_module
      implicit none
      private

      public :: cgm_pip_trace 
      public :: cgm_pip_trace_orientation 
      public :: cgm_pip_trace_scale_factors 
      public :: cgm_pip_trace_display_modes 
      public :: cgm_pip_trace_va_fill 
      public :: cgm_pip_va_fill_pos_bound 
      public :: cgm_pip_va_fill_neg_bound 
      public :: cgm_pip_va_fill_style 
      public :: cgm_pip_va_fill_const_color 
      public :: cgm_pip_va_fill_const_pattern 
      public :: cgm_pip_va_fill_alignment 
      public :: cgm_pip_trace_bg_fill 
      public :: cgm_pip_bg_fill_bound 
      public :: cgm_pip_bg_fill_interpolation 
      public :: cgm_pip_bg_fill_style 
      public :: cgm_pip_bg_fill_null_color 
      public :: cgm_pip_bg_fill_const_color 
      public :: cgm_pip_bg_fill_alignment 
      public :: cgm_pip_begin_trace_group 
      public :: cgm_gopks 
      public :: cgm_gopwk 
      public :: cgm_gacwk 
      public :: cgm_gswkvp 
      public :: cgm_gswn 
      public :: cgm_gsvp 
      public :: cgm_gselnt 
      public :: cgm_gsclip 
      public :: cgm_gscell 
      public :: cgm_gscr 
      public :: cgm_gsln 
      public :: cgm_gslwsc 
      public :: cgm_gsplci 
      public :: cgm_gpl 
      public :: cgm_gsfais 
      public :: cgm_gsfaci 
      public :: cgm_gfa 
      public :: cgm_set_marker_font 
      public :: cgm_gsmk 
      public :: cgm_set_marker_size 
      public :: cgm_set_marker_angle 
      public :: cgm_gspmci 
      public :: cgm_gpm 
      public :: cgm_gschh 
      public :: cgm_gschxp 
      public :: cgm_gschsp 
      public :: cgm_gstxal 
      public :: cgm_gstxp 
      public :: cgm_gstxfp 
      public :: cgm_gstxci 
      public :: cgm_gschup 
      public :: cgm_gtx 
      public :: cgm_gtxr 
      public :: cgm_gdawk 
      public :: cgm_gclwk 
      public :: cgm_gclks

      interface cgm_gtx
        module procedure cgm_gtx_i
        module procedure cgm_gtx_c
      end interface

      interface cgm_gtxr
        module procedure cgm_gtxr_i
        module procedure cgm_gtxr_c
      end interface

      interface cgm_gopwk
        module procedure cgm_gopwk_i
        module procedure cgm_gopwk_c
      end interface

      character(len=100),public,save :: cgm_ident = &
       '$Id: cgm.f90,v 1.1 2008/02/15 18:27:32 mengewm Exp $'

      contains

      subroutine cgm_pip_trace (x0, y0, grp, fac, tr, n_tr,  &
                                va_cols, n_va_cols, bg_cols, n_bg_cols)

      real            ,intent(in)   :: x0
      real            ,intent(in)   :: y0
      integer         ,intent(in)   :: grp
      real            ,intent(in)   :: fac
      real            ,intent(in)   :: tr(:)
      integer         ,intent(in)   :: n_tr
      integer         ,intent(in)   :: va_cols(:)
      integer         ,intent(in)   :: n_va_cols
      integer         ,intent(in)   :: bg_cols(:)
      integer         ,intent(in)   :: n_bg_cols

      call cgm_c_pip_trace (x0, y0, grp, fac, tr, n_tr,  &
                            va_cols, n_va_cols, bg_cols, n_bg_cols)

      end subroutine cgm_pip_trace


      subroutine cgm_pip_trace_orientation (bx, by, ax, ay)

      real            ,intent(in)   :: bx
      real            ,intent(in)   :: by
      real            ,intent(in)   :: ax
      real            ,intent(in)   :: ay

      call cgm_c_pip_trace_orientation (bx, by, ax, ay)

      end subroutine cgm_pip_trace_orientation


      subroutine cgm_pip_trace_scale_factors (base, ampl)

      real            ,intent(in)   :: base
      real            ,intent(in)   :: ampl

      call cgm_c_pip_trace_scale_factors (base, ampl)

      end subroutine cgm_pip_trace_scale_factors


      subroutine cgm_pip_trace_display_modes (modes, n)

      integer         ,intent(in)   :: modes(:)
      integer         ,intent(in)   :: n

      call cgm_c_pip_trace_display_modes (modes, n)

      end subroutine cgm_pip_trace_display_modes


      subroutine cgm_pip_trace_va_fill (xpmin, xpmax, xnmin, xnmax, istyle,  &
                                        ipcol, imcol, ippat, impat, vc_align)

      real            ,intent(in)   :: xpmin
      real            ,intent(in)   :: xpmax
      real            ,intent(in)   :: xnmin
      real            ,intent(in)   :: xnmax
      integer         ,intent(in)   :: istyle
      integer         ,intent(in)   :: ipcol
      integer         ,intent(in)   :: imcol
      integer         ,intent(in)   :: ippat
      integer         ,intent(in)   :: impat
      integer         ,intent(in)   :: vc_align

      call cgm_c_pip_trace_va_fill (xpmin, xpmax, xnmin, xnmax, istyle,  &
                                    ipcol, imcol, ippat, impat, vc_align)

      end subroutine cgm_pip_trace_va_fill


      subroutine cgm_pip_va_fill_pos_bound (xpmin, xpmax)

      real            ,intent(in)   :: xpmin
      real            ,intent(in)   :: xpmax

      call cgm_c_pip_va_fill_pos_bound (xpmin, xpmax)

      end subroutine cgm_pip_va_fill_pos_bound


      subroutine cgm_pip_va_fill_neg_bound (xnmin, xnmax)

      real            ,intent(in)   :: xnmin
      real            ,intent(in)   :: xnmax

      call cgm_c_pip_va_fill_neg_bound (xnmin, xnmax)

      end subroutine cgm_pip_va_fill_neg_bound


      subroutine cgm_pip_va_fill_style (istyle)

      integer         ,intent(in)   :: istyle

      call cgm_c_pip_va_fill_style (istyle)

      end subroutine cgm_pip_va_fill_style


      subroutine cgm_pip_va_fill_const_color (ipcol, imcol)

      integer         ,intent(in)   :: ipcol
      integer         ,intent(in)   :: imcol

      call cgm_c_pip_va_fill_const_color (ipcol, imcol)

      end subroutine cgm_pip_va_fill_const_color


      subroutine cgm_pip_va_fill_const_pattern (ippat, impat)

      integer         ,intent(in)   :: ippat
      integer         ,intent(in)   :: impat

      call cgm_c_pip_va_fill_const_pattern (ippat, impat)

      end subroutine cgm_pip_va_fill_const_pattern


      subroutine cgm_pip_va_fill_alignment (vc_align)

      integer         ,intent(in)   :: vc_align

      call cgm_c_pip_va_fill_alignment (vc_align)

      end subroutine cgm_pip_va_fill_alignment


      subroutine cgm_pip_trace_bg_fill (pos_fill, neg_fill, imode,  &
                                        fill_style, null_col, const_col,  &
                                        bg_align)

      real            ,intent(in)   :: pos_fill
      real            ,intent(in)   :: neg_fill
      integer         ,intent(in)   :: imode
      integer         ,intent(in)   :: fill_style
      integer         ,intent(in)   :: null_col
      integer         ,intent(in)   :: const_col
      integer         ,intent(in)   :: bg_align

      call cgm_c_pip_trace_bg_fill (pos_fill, neg_fill, imode,  &
                                    fill_style, null_col, const_col,  &
                                    bg_align)

      end subroutine cgm_pip_trace_bg_fill


      subroutine cgm_pip_bg_fill_bound (pos_fill, neg_fill)

      real            ,intent(in)   :: pos_fill
      real            ,intent(in)   :: neg_fill

      call cgm_c_pip_bg_fill_bound (pos_fill, neg_fill)

      end subroutine cgm_pip_bg_fill_bound


      subroutine cgm_pip_bg_fill_interpolation (imode)

      integer         ,intent(in)   :: imode

      call cgm_c_pip_bg_fill_interpolation (imode)

      end subroutine cgm_pip_bg_fill_interpolation


      subroutine cgm_pip_bg_fill_style (fill_style)

      integer         ,intent(in)   :: fill_style

      call cgm_c_pip_bg_fill_style (fill_style)

      end subroutine cgm_pip_bg_fill_style


      subroutine cgm_pip_bg_fill_null_color (null_col)

      integer         ,intent(in)   :: null_col

      call cgm_c_pip_bg_fill_null_color (null_col)

      end subroutine cgm_pip_bg_fill_null_color


      subroutine cgm_pip_bg_fill_const_color (const_col)

      integer         ,intent(in)   :: const_col

      call cgm_c_pip_bg_fill_const_color (const_col)

      end subroutine cgm_pip_bg_fill_const_color


      subroutine cgm_pip_bg_fill_alignment (bg_align)

      integer         ,intent(in)   :: bg_align

      call cgm_c_pip_bg_fill_alignment (bg_align)

      end subroutine cgm_pip_bg_fill_alignment


      subroutine cgm_pip_begin_trace_group (grp, pos_pri, neg_pri,  &
                                            pos_mode, neg_mode)

      integer         ,intent(in)   :: grp
      integer         ,intent(in)   :: pos_pri
      integer         ,intent(in)   :: neg_pri
      integer         ,intent(in)   :: pos_mode
      integer         ,intent(in)   :: neg_mode

      call cgm_c_pip_begin_trace_group (grp, pos_pri, neg_pri,  &
                                        pos_mode, neg_mode)

      end subroutine cgm_pip_begin_trace_group


      subroutine cgm_pip_end_trace_group

      call cgm_c_pip_end_trace_group

      end subroutine cgm_pip_end_trace_group


      subroutine cgm_gopks (unit)

      integer         ,intent(in)   :: unit

      call cgm_c_gopks (unit)

      end subroutine cgm_gopks


      subroutine cgm_gopwk_i (wk, file, type)

      integer         ,intent(in)   :: wk
      integer         ,intent(in)   :: file(:)
      integer         ,intent(in)   :: type(:)

      call cgm_c_gopwk (wk, file, type)

      end subroutine cgm_gopwk_i


      subroutine cgm_gopwk_c (wk, file, type)

      integer         ,intent(in)   :: wk
      character(len=*),intent(in)   :: file
      character(len=*),intent(in)   :: type

      integer                       :: i                             !local
      integer                       :: j                             !local
      integer                       :: length                        !local
      integer                       :: len_ifile                     !local
      integer                       :: len_itype                     !local
      integer                       :: nbpw                          !local
      integer                       :: istat                         !local
      integer,allocatable           :: ifile(:)                      !local
      integer,allocatable           :: itype(:)                      !local

      nbpw   = bit_size(i) / 8

      length = len_trim(file) + 1
      len_ifile = length / nbpw
      if (len_ifile*nbpw .ne. length) len_ifile = len_ifile + 1
      allocate(ifile(len_ifile),stat=istat)
      do i = 1, length - 1
        j = ichar(file(i:i))
        call cgm_c_insert_char (j,i,ifile)
      enddo
      call cgm_c_insert_char (0,length,ifile)

      length = len_trim(type) + 1
      len_itype = length / nbpw
      if (len_itype*nbpw .ne. length) len_itype = len_itype + 1
      allocate(itype(len_itype),stat=istat)
      do i = 1, length - 1
        j = ichar(type(i:i))
        call cgm_c_insert_char (j,i,itype)
      enddo
      call cgm_c_insert_char (0,length,itype)

      call cgm_c_gopwk (wk, ifile, itype)

      deallocate (ifile,stat=istat)
      deallocate (itype,stat=istat)

      end subroutine cgm_gopwk_c


      subroutine cgm_gacwk (wk)

      integer         ,intent(in)   :: wk

      call cgm_c_gacwk (wk)

      end subroutine cgm_gacwk


      subroutine cgm_gswkvp (wk, xmin, xmax, ymin, ymax)

      integer         ,intent(in)   :: wk
      real            ,intent(in)   :: xmin
      real            ,intent(in)   :: xmax
      real            ,intent(in)   :: ymin
      real            ,intent(in)   :: ymax

      call cgm_c_gswkvp (wk, xmin, xmax, ymin, ymax)

      end subroutine cgm_gswkvp


      subroutine cgm_gswn (trans, xmin, xmax, ymin, ymax)

      integer         ,intent(in)   :: trans
      real            ,intent(in)   :: xmin
      real            ,intent(in)   :: xmax
      real            ,intent(in)   :: ymin
      real            ,intent(in)   :: ymax

      call cgm_c_gswn (trans, xmin, xmax, ymin, ymax)

      end subroutine cgm_gswn


      subroutine cgm_gsvp (trans, xmin, xmax, ymin, ymax)

      integer         ,intent(in)   :: trans
      real            ,intent(in)   :: xmin
      real            ,intent(in)   :: xmax
      real            ,intent(in)   :: ymin
      real            ,intent(in)   :: ymax

      call cgm_c_gsvp (trans, xmin, xmax, ymin, ymax)

      end subroutine cgm_gsvp


      subroutine cgm_gselnt (trans)

      integer         ,intent(in)   :: trans

      call cgm_c_gselnt (trans)

      end subroutine cgm_gselnt


      subroutine cgm_gsclip (clip)

      integer         ,intent(in)   :: clip

      call cgm_c_gsclip (clip)

      end subroutine cgm_gsclip


      subroutine cgm_gscell (x1, y1, x2, y2, nx, ny, array, dummy)

      real            ,intent(in)   :: x1
      real            ,intent(in)   :: y1
      real            ,intent(in)   :: x2
      real            ,intent(in)   :: y2
      integer         ,intent(in)   :: nx
      integer         ,intent(in)   :: ny
      integer         ,intent(in)   :: array(:)
      integer         ,intent(in)   :: dummy

      call cgm_c_gscell (x1, y1, x2, y2, nx, ny, array, dummy)

      end subroutine cgm_gscell


      subroutine cgm_gscr (wk, color, red, green, blue)

      integer         ,intent(in)   :: wk
      integer         ,intent(in)   :: color
      real            ,intent(in)   :: red
      real            ,intent(in)   :: green
      real            ,intent(in)   :: blue

      call cgm_c_gscr (wk, color, red, green, blue)

      end subroutine cgm_gscr


      subroutine cgm_gsln (type)

      integer         ,intent(in)   :: type

      call cgm_c_gsln (type)

      end subroutine  cgm_gsln


      subroutine cgm_gslwsc (width)

      real            ,intent(in)   :: width

      call cgm_c_gslwsc (width)

      end subroutine cgm_gslwsc


      subroutine cgm_gsplci (color)

      integer         ,intent(in)   :: color

      call cgm_c_gsplci (color)

      end subroutine cgm_gsplci


      subroutine cgm_gpl (n, x, y)

      integer         ,intent(in)   :: n
      real            ,intent(in)   ::x(:)
      real            ,intent(in)   ::y(:)

      call cgm_c_gpl (n, x, y)

      end subroutine cgm_gpl


      subroutine cgm_gsfais (type)

      integer         ,intent(in)   :: type

      call cgm_c_gsfais (type)

      end subroutine cgm_gsfais


      subroutine cgm_gsfaci (color)

      integer         ,intent(in)   :: color

      call cgm_c_gsfaci (color)

      end subroutine cgm_gsfaci


      subroutine cgm_gfa (n, x, y)

      integer         ,intent(in)   :: n
      real            ,intent(in)   :: x(:)
      real            ,intent(in)   :: y(:)

      call cgm_c_gfa (n, x, y)

      end subroutine cgm_gfa


      subroutine cgm_set_marker_font (set)

      integer         ,intent(in)   :: set

      call cgm_c_set_marker_font (set)

      end subroutine cgm_set_marker_font


      subroutine cgm_gsmk (type)

      integer         ,intent(in)   :: type

      call cgm_c_gsmk (type)

      end subroutine cgm_gsmk


      subroutine cgm_set_marker_size (size)

      real            ,intent(in)   :: size

      call cgm_c_set_marker_size (size)

      end subroutine cgm_set_marker_size


      subroutine cgm_set_marker_angle (angle)

      real            ,intent(in)   :: angle

      call cgm_c_set_marker_angle (angle)

      end subroutine cgm_set_marker_angle


      subroutine cgm_gspmci (color)

      integer         ,intent(in)   :: color

      call cgm_c_gspmci (color)

      end subroutine cgm_gspmci


      subroutine cgm_gpm (n, x, y)

      integer         ,intent(in)   :: n
      real            ,intent(in)   :: x(:)
      real            ,intent(in)   :: y(:)

      call cgm_c_gpm (n, x, y)

      end subroutine cgm_gpm


      subroutine cgm_gschh (height)

      real            ,intent(in)   :: height

      call cgm_c_gschh (height)

      end subroutine cgm_gschh


      subroutine cgm_gschxp (xp)

      real            ,intent(in)   :: xp

      call cgm_c_gschxp (xp)

      end subroutine cgm_gschxp


      subroutine cgm_gschsp (sp)

      real            ,intent(in)   :: sp

      call cgm_c_gschsp (sp)

      end subroutine cgm_gschsp


      subroutine cgm_gstxal (h, v)

      integer         ,intent(in)   :: h
      integer         ,intent(in)   :: v

      call cgm_c_gstxal (h, v)

      end subroutine  cgm_gstxal


      subroutine cgm_gstxp (path)

      integer         ,intent(in)   :: path

      call cgm_c_gstxp (path)

      end subroutine cgm_gstxp


      subroutine cgm_gstxfp (font, type)

      integer         ,intent(in)   :: font
      integer         ,intent(in)   :: type

      call cgm_c_gstxfp (font, type)

      end subroutine cgm_gstxfp


      subroutine cgm_gstxci (color)

      integer         ,intent(in)   :: color

      call cgm_c_gstxci (color)

      end subroutine cgm_gstxci


      subroutine cgm_gschup (x, y)

      real            ,intent(in)   :: x
      real            ,intent(in)   :: y

      call cgm_c_gschup (x, y)

      end subroutine cgm_gschup


      subroutine cgm_gtx_i (x, y, text)

      real            ,intent(in)   :: x
      real            ,intent(in)   :: y
      integer         ,intent(in)   :: text(:)

      call cgm_c_gtx (x, y, text)

      end subroutine cgm_gtx_i


      subroutine cgm_gtx_c (x, y, text)

      real            ,intent(in)   :: x
      real            ,intent(in)   :: y
      character(len=*),intent(in)   :: text

      integer                       :: i                             !local
      integer                       :: j                             !local
      integer                       :: length                        !local
      integer                       :: len_itext                     !local
      integer                       :: nbpw                          !local
      integer                       :: istat                         !local
      integer,allocatable           :: itext(:)                      !local

      nbpw   = bit_size(i) / 8
      length = len_trim(text) + 1
      len_itext = length / nbpw
      if (len_itext*nbpw .ne. length) len_itext = len_itext + 1
      allocate(itext(len_itext),stat=istat)
      do i = 1, length - 1
        j = ichar(text(i:i))
        call cgm_c_insert_char (j,i,itext)
      enddo
      call cgm_c_insert_char (0,length,itext)
      call cgm_c_gtx (x, y, itext)
      deallocate (itext,stat=istat)

      end subroutine cgm_gtx_c


      subroutine cgm_gtxr_i (dx, dy, x, y, text)

      real            ,intent(in)   :: dx
      real            ,intent(in)   :: dy
      real            ,intent(in)   :: x
      real            ,intent(in)   :: y
      integer         ,intent(in)   :: text(:)

      call cgm_c_gtxr (dx, dy, x, y, text)

      end subroutine cgm_gtxr_i


      subroutine cgm_gtxr_c (dx, dy, x, y, text)

      real            ,intent(in)   :: dx
      real            ,intent(in)   :: dy
      real            ,intent(in)   :: x
      real            ,intent(in)   :: y
      character(len=*),intent(in)   :: text

      integer                       :: i                             !local
      integer                       :: j                             !local
      integer                       :: length                        !local
      integer                       :: len_itext                     !local
      integer                       :: nbpw                          !local
      integer                       :: istat                         !local
      integer,allocatable           :: itext(:)                      !local

      nbpw   = bit_size(i) / 8
      length = len_trim(text) + 1
      len_itext = length / nbpw
      if (len_itext*nbpw .ne. length) len_itext = len_itext + 1
      allocate(itext(len_itext),stat=istat)
      do i = 1, length - 1
        j = ichar(text(i:i))
        call cgm_c_insert_char (j,i,itext)
      enddo
      call cgm_c_insert_char (0,length,itext)
      call cgm_c_gtxr (dx, dy, x, y, itext)
      deallocate (itext,stat=istat)

      end subroutine cgm_gtxr_c


      subroutine cgm_gdawk (wk)

      integer         ,intent(in)   :: wk

      call cgm_c_gdawk (wk)

      end subroutine cgm_gdawk


      subroutine cgm_gclwk (wk)

      integer         ,intent(in)   :: wk

      call cgm_c_gclwk (wk)

      end subroutine cgm_gclwk


      subroutine cgm_gclks

      call cgm_c_gclks

      end subroutine cgm_gclks


      end module cgm_module

