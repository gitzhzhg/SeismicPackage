/*<license>
 -------------------------------------------------------------------------------
  Copyright (c) 2007 ConocoPhillips Company
 
  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:
 
  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.
 
  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
 -------------------------------------------------------------------------------
 </license>*/
/*------------------------------------------------------------------
 *USER DOC
 *Name   : setGridSize 
 *Purpose: Set size of grid image.
 *                            
 *Author : Michael L. Sherrill
 *Date   : 09/92 (C++ version 4/97)
 *
 * Function Definition:
 * void setGridSize()
 *
 *
 *NOTES:
 * 1. 
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/
 
#include "plot_image.hh"


void PlotImage::setGridSize()

{
 Display *dpy;
 int screen;
 float hpixels_per_inch;
 float vpixels_per_inch;
 float x1,x2,y1,y2;


  if( (dpy = XtDisplay(_graphic)) == NULL) return;

  screen = DefaultScreen(XtDisplay(_graphic));
  hpixels_per_inch =  horizontalPixelsPerInch(dpy, screen);
  vpixels_per_inch =  verticalPixelsPerInch(dpy, screen);

  if(_user->_metric) 
     {
     hpixels_per_inch /=  2.54;
     vpixels_per_inch /=  2.54;
     }

  x1 = min(_user->_grid_x1, _user->_grid_x2);
  x2 = max(_user->_grid_x1, _user->_grid_x2);
  y1 = min(_user->_grid_y1, _user->_grid_y2);
  y2 = max(_user->_grid_y1, _user->_grid_y2);
 
  _user->_grid_width = (int) (  _user->_ti * hpixels_per_inch);
  _user->_grid_height= (int) (  _user->_is * vpixels_per_inch);

}
