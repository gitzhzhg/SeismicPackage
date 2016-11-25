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
 *Name   : blank_image
 *Purpose: Creates a blank image
 *Author : Michael L. Sherrill
 *Date   : 02/93 (C++ version 4/97)
 *
 * Function Definition:
 * void blankImage( float xloc, float yloc)
 * 
 * xloc      in         Xbin label to annotate.    
 * yloc      in         Ybin Label to annotate.
 *
 *NOTES:
 * 1. 
 *   
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/

#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include <stdlib.h>
#include <stdio.h>
#include "plot_image.hh"




void PlotImage::blankImage( float              xloc,
                            float              yloc)

{
  GC temp_gc;
  GC bold_gc;
  Screen *scr;
  Display *dpy;
  int x1, y1;
  int labellen;
  char label[50];


  scr = XtScreen(_graphic);
  dpy = XtDisplay(_graphic);

  if(_ximage.depth == 1)
     {
     temp_gc = _bitmap_gc1;
     bold_gc = _bitmap_gc2;
     }
  else   
     {
     temp_gc = _gc1;
     bold_gc = _gc2;
     }




/*white out x annotation*/
   x1 = 0;
   y1 = 0;
   if(_ximage.depth == 1)
      {
      XSetForeground(dpy, temp_gc, 0);
      XSetBackground(dpy, temp_gc, 1);
      }
   else
      { 
      XSetForeground(dpy, temp_gc, _white_pixel);
      XSetBackground(dpy, temp_gc, _black_pixel);
      }

   XFillRectangle(dpy, _pixmary[_cpixm], temp_gc, x1, y1,
                  (_ximage.width+(int)(_left_border+_right_border)), 
                  (int) _boldcharheight + 1 );


/*black out image*/
   if(_ximage.depth == 1)
      {
      XSetForeground(dpy, temp_gc, 1);
      XSetBackground(dpy, temp_gc, 0);
      }
   else
      {
      XSetForeground(dpy, temp_gc, _black_pixel);
      XSetBackground(dpy, temp_gc, _white_pixel);
      }

   XFillRectangle(dpy, _pixmary[_cpixm], temp_gc, 
                  (int)_left_border,(int)_top_border,
                  _ximage.width, _ximage.height);

   x1 = (int)_left_border;
   y1 = (int)_boldcharheight + 1; 
   sprintf(label,"NEW AT XBIN %8f YBIN %f",xloc,yloc);
   labellen = strlen(label);
   XDrawString(dpy,_pixmary[_cpixm],bold_gc, x1,y1, label,labellen);



/*copy the image to screen*/
  if(_user->getMode() >= PlotCOLOR) 
     {
     XCopyArea(  dpy, _pixmary[_cpixm], XtWindow(_graphic),
                 _gc1,0,0,(int)(_ximage.width+_left_border+_right_border),
                 (int)(_ximage.height+_top_border+_bottom_border), 0,0);
     }
  else   /*wiggles*/
     {
     XCopyPlane(  dpy, _pixmary[_cpixm], XtWindow(_graphic),
                  _gc1,0,0,(int)(_ximage.width+_left_border+_right_border),
                  (int)(_ximage.height+_top_border+_bottom_border), 0,0,1);
  
     }

  //redraw the top border so new label will show
  refreshMain(0,0,_ximage.width,_top_border,0,0,True,_graphic);
}

