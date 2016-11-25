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
 *Name   : imageMarks
 *Purpose: Mark active velocity on image iso plot or annotate
 *         image as being out of date if massive changes have rendered
 *         it inaccurate.                       
 *
 *Author : Michael L. Sherrill
 *Date   : 10/92 (C++ version 4/97)
 *
 *Function Definition:
 *void image_marks (long              active_func,
 *                  double            xloc,
 *                  long              num_marks,
 *                  float             *times)
 *
 * active_func      in    Current function, 0 if image is out of date
 * xloc             in    xbin location of function
 * num_marks        in    number of x's to mark
 * times            in    array of times to mark
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

#include <stdio.h>
#include <stdlib.h>
#include "plot_image.hh"

#define OUTDATED "OUT OF DATE !" 


void PlotImage::imageMarks (long              active_func,
                            float             *xloc,
                            long              num_marks,
                            float             *times)


{
  char msg[20]; 
  Display *dpy;
  Screen *scr;
  static GC temp_gc = NULL;
  long i;
  int x1, x2, y1, y2, labellen;
  int offset = 6;
  long maxy;
  long index; 
  long skip_traces = 0;


  if ( !_displayed &&  !_cpixm ) return;


  for(i=1;i<=_cpixm;i++) skip_traces += _tpnl[i-1];
  index = skip_traces * _nhdrs;


  dpy = XtDisplay(_graphic);
  scr = XtScreen(_graphic);
  if(temp_gc == NULL)
    {
    temp_gc = XCreateGC( dpy, XtWindow(_graphic), None, NULL);
    XSetBackground(dpy, temp_gc, _black_pixel);
    XSetForeground(dpy, temp_gc, _white_pixel);
    }

/*mark left annotation area with out of date if requested*/
  if(!active_func)
     {
     strcpy(msg,OUTDATED);
     labellen      = strlen(msg);
     int num_times = (getGraphHeight() - _top_border - _bottom_border + 1)
                   / (_boldcharheight + 2);

     for(i = 0; i < num_times; i++)
       {
       x1 = 0; 
       y1 = (i * (_boldcharheight + 1) ) + _top_border;
       if(_user->getYannotationHeader()&&_user->getMovieOption())/*slice movies*/
          for(i=0;i<_frames;i++)
              XDrawString(dpy,_pixmary[i],_gc2,
                          x1,y1, msg, labellen); 
       else
          XDrawString(dpy,_pixmary[_cpixm],_gc2,
                          x1, y1, msg, labellen);
       }
     }


  /*redraw x vertical lines*/
  /*
  y1 =  (int)_top_border;
  y2 = y1 + _ximage.height;                
  for(i=_user->getFirstTraceToAnnotate()-1;i<_tpnl[_cpixm];
     i+=_user->getXlabelIncrement())
     {
     x1 = x2 = (int)((_hd[i*_nhdrs+_user->getPrimaryAnnotationHeader()-1+index]
             - _grid_x1)/_x_value_per_pixel
             + _left_border + getFirstTraceLocation() + .5);
     if(x1 >= _left_border && 
        x2 <= _ximage.width + _left_border)
       {
       XDrawLine(dpy, _pixmary[_cpixm], _gc1, x1,y1,x2,y2);
       }
     }
   */

  if(_over_image == NULL)//no seismic overlay image
    {
    refresh( 0, 0, getGraphWidth(), getGraphHeight());
    }
  else//have seismic over this image
    {
    _over_image->refreshMain( 0, 0, _over_image->getGraphWidth(), 
                              _over_image->getGraphHeight(),
                              0, 0, False, _over_image->graphicWidget());
    }


/*mark white line and x's at active function*/
  if(xloc != NULL)
     { 
     x1 = x2 = (int)((*xloc - _grid_x1)/_x_value_per_pixel
             + _left_border + .5);
     if(x1 >= _left_border && 
        x2 <= _ximage.width + _left_border)
        {
        if(x1 == _left_border)x1 = x2 = (int)_left_border+1;
        if(x1 == _ximage.width + _left_border)x1 = x2 = x1 - 1;  
        XDrawLine(dpy, XtWindow(_graphic), temp_gc, x1,y1,x2,y2);
        }

     maxy = _ximage.height + _top_border;
     for(i=0;i<num_marks;i++)
        {
         y1 = y2 = (int)((times[i] - _tmin)/_y_value_per_pixel 
                 + _top_border+.5);
         if(y1 <= maxy)
            {
            XDrawLine(dpy, XtWindow(_graphic), temp_gc,
                      x1-offset,y1-offset,x2+offset,y2+offset);
            XDrawLine(dpy, XtWindow(_graphic), temp_gc,
                      x1-offset,y2+offset,x2+offset,y1-offset);
            }
        }
     }
 
}
