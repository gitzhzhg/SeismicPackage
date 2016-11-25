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
 *Name   : maximize 
 *Purpose: Set parameters to fill the entire screen with an image.
 *Author : Michael L. Sherrill 
 *Date   : 07/91 (C++ version 4/97)
 *
 * Function Definition:     
 * void maximize ()
 *
 *NOTES:
 * 1. Some original user parameters may be redefined if the requested display 
 *    will not fit on the visible screen area. 
 *
 *Revisions:
 * DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *                                                                 
 *END DOC
 *------------------------------------------------------------------*/

#include <X11/Intrinsic.h>
#include "plot_image.hh"


void PlotImage::maximize()

{
  long max_width, max_height, ImageWidth, ImageHeight;
  long  nsamp;
  int screen;
  long trace_delta;
  float width_percent = .95;
  float height_percent= .80;
  float vpixels_per_inch, hpixels_per_inch;
  double total_inches, total_time, time_reduction;
  Screen *scr;
  Display *dpy;



/*determine device screen info*/
  scr = XtScreen(_graphic);
  dpy = XtDisplay(_graphic);
  screen = DefaultScreen(XtDisplay(_graphic));
  hpixels_per_inch =  horizontalPixelsPerInch(dpy, screen);
  vpixels_per_inch =  verticalPixelsPerInch(dpy, screen);
  if(_user->_metric)
  {
   vpixels_per_inch /=  2.54;
   hpixels_per_inch /=  2.54;
  }

/*determine trace delta*/
  max_width = (long)((DisplayWidth(dpy,screen) 
            - (_left_border+_right_border)) * width_percent);
  total_inches = ((float)max_width) / (hpixels_per_inch + 1) ;
  _user->_ti =((float)(_user->_nplt + 1)) / total_inches;
  trace_delta = (long)((hpixels_per_inch / _user->_ti +.5));
  if(_user->getMode() < PlotCOLOR && trace_delta < 2) trace_delta = 2;
  if(_user->getMode() >= PlotCOLOR && trace_delta < 1) trace_delta = 1;


/*set trace display width params*/
  max_width = (long)((DisplayWidth(dpy,screen) 
            - (_left_border+_right_border+trace_delta + 8))
            * width_percent);
  total_inches = ((float)max_width) / (hpixels_per_inch + 1) ;
  _user->_ti = ((float)(_user->_nplt + 1)) / total_inches;


/*set trace display height params*/
  max_height = (long)((DisplayHeight(dpy,screen) 
             - (_top_border+_bottom_border) - 1) 
             * height_percent);
  total_time = _user->_tmax - _user->_tmin;
  _user->_is = max_height / vpixels_per_inch / total_time;


/*determine image width and height*/
  nsamp =  (long)((float)(_user->_tmax - _user->_tmin - _user->_G.tstrt) 
           / _user->_G.srval / _user->_tdec + 1);
  ImageWidth = _user->_nplt * trace_delta;
  ImageHeight = (long)((float)(_user->_is * vpixels_per_inch 
                * (_user->_tdec*_user->_G.srval)*nsamp));


/*now reduce the number of traces if all wont fit on screen*/
  if(ImageWidth > max_width) 
     {
     _user->_ti   =  ((float) _user->_nplt ) / (total_inches * width_percent);
     if(_user->_ti > hpixels_per_inch)
        { 
        _user->_nplt = max_width / trace_delta;
        _user->_ti   =  ((float) _user->_nplt ) / total_inches;
        }
     }


/*now reduce the trace time if too large*/
  if (max_height < ImageHeight)
     {
     time_reduction = ((float) max_height) / ((float)ImageHeight);
     nsamp = (long)((float)nsamp * time_reduction);
     _user->_tmax = (nsamp-1) * (_user->_tdec * _user->_G.srval) + _user->_tmin;
     }

                    

}
