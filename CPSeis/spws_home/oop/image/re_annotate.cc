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
 *Name   : reAnnotate
 *Purpose: Change image annotation and y axis transformation without
 *         re-reading data.
 *
 *Author : Michael L. Sherrill
 *Date   : 04/94 (C++ version 4/97)
 *
 * Function Definition:
 * int  process_zoom()
 *
 *NOTES:
 * 1.
 *
 *
 *Revisions:
 *DATE      WHO         DESCRIPTION
 * --------  --------    --------------------------------------------
 *
 *END DOC
 *------------------------------------------------------------------*/
#include "plot_image.hh"


void PlotImage::reAnnotate()
{
Display *dpy;
GC temp_gc;
int x,y,width,height;

  dpy = XtDisplay(_graphic);

  
  if(_manual_annotate)
     {
     if( _manual_grid_x1 == _manual_grid_x2)
        {
        _manual_grid_x1 = _grid_x1;
        _manual_grid_x2 = _grid_x2;
        }
     _manual_x_value_per_pixel = (_manual_grid_x2 
                                     - _manual_grid_x1)
                                     / (_ximage.width - 1);    

     if( _manual_grid_y1 == _manual_grid_y2)
        {
        _manual_grid_y1 = _grid_y1;
        _manual_grid_y2 = _grid_y2;
        }
     _manual_y_value_per_pixel = (_manual_grid_y2 
                                     - _manual_grid_y1)
                                     / (_ximage.height - 1);
     _manual_scaler = (_manual_grid_y2 - _manual_grid_y1)
                          / (_grid_y2 - _grid_y1);
     }


/*fill background of image in white*/
  if(_ximage.depth == 1) 
     {
     temp_gc = _bitmap_gc1;
     XSetForeground(dpy, temp_gc, 0);
     XSetBackground(dpy, temp_gc, 1);
     }
  else
     {
     temp_gc = _gc1;
     XSetForeground(dpy, temp_gc, _white_pixel);
     XSetBackground(dpy, temp_gc, _black_pixel);
     }


/*top*/
  x = 0;
  y = 0;
  width = _ximage.width +(int)_left_border+(int)_right_border;
  height= (int)_top_border;
  XFillRectangle(dpy, _pixmary[_cpixm], temp_gc, x,y,width,height);

/*left*/
  x = 0;
  y = (int)_top_border;
  width = (int)_left_border;
  height= _ximage.height + (int)_bottom_border; 
  XFillRectangle(dpy, _pixmary[_cpixm], temp_gc,x,y,width,height);

/*right*/
  x = _ximage.width+(int)_left_border;
  y = (int)_top_border;
  width = (int)_right_border;
  height= _ximage.height + (int)_bottom_border;
  XFillRectangle(dpy, _pixmary[_cpixm], temp_gc,x,y,width,height);

/*bottom*/
  x = (int)_left_border;
  y = _ximage.height+(int)_top_border;
  width = _ximage.width;
  height= (int)_bottom_border;
  XFillRectangle(dpy, _pixmary[_cpixm], temp_gc,x,y,width,height);


/*put background and foreground colors back the way they were*/
  if(_ximage.depth == 1) 
     {
     temp_gc = _bitmap_gc1;
     XSetForeground(dpy, temp_gc, 1);
     XSetBackground(dpy, temp_gc, 0);
     }
  else
     {
     temp_gc = _gc1;
     XSetForeground(dpy, temp_gc, _black_pixel);
     XSetBackground(dpy, temp_gc, _white_pixel);
     }


  annotatePlot(0);

  refresh(0,0,ImageAll,ImageAll);

}
