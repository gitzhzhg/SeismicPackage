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
/*
C      refresh.c
C************************* COPYRIGHT NOTICE ****************************
C*      CONFIDENTIAL AND PROPRIETARY INFORMATION OF CONOCO INC.        *
C*       PROTECTED BY THE COPYRIGHT LAW AS AN UNPUBLISHED WORK         *
C************************* COPYRIGHT NOTICE ****************************
C\USER DOC
C-----------------------------------------------------------------------
C                     SEISMIC PROCESSING WORKSTATION
C                             U T I L I T Y 
C              written in c -- designed to be called from c
C
C     Utility Name:  refresh            (refresh traces or picks)         
C          Written:  92/03/15  by:  Trey Roby and Mike Sherrill
C     Last revised:  93/11/09  by:  Tom Stoeckley (C++ version 4/97 M. Sherrill
C
C  Purpose:       To refresh seismic traces or picks in the image
C                 display.
C
C  Related Documentation:
C-----------------------------------------------------------------------
C                            THIS UTILITY     
C
C  node:                   pospsv (ultrix)
C  source code directory:  ~spws/util/image   (shared)
C  library:                libimage.a            (shared)
C  header file:            plot_image.hh            (shared)
C  source file:            refresh.cc
C
C  static functions:       none
C  documented functions:   refresh  refreshTraces  refreshPicks
C
C  The user should include the above header file in his code.
C-----------------------------------------------------------------------
C                        EXTERNAL REFERENCES 
C           (this utility references X, Xt, and/or Motif)
C      (standard C, X, Xt, and Motif references not listed here)
C
C  libraries:     ligimage.a
C  header files:  plot_image.h    cprim.h
C  functions:     reduce_segments  draw_segments
C-----------------------------------------------------------------------
C                           REVISION HISTORY
C
C     Date      Author         Description
C     ----      ------         -----------
C  5. 97/04/17  Sherrill       (C++ version)
C  4. 93/11/09  Stoeckley      Added refreshPoints.
C  3. 93/09/14  Stoeckley      Added plot of points for zero-ed picks.
C  2. 93/07/20  Stoeckley      Added refreshTraces and refreshPicks.
C  1. 92/03/15  Roby/Sherrill  Initial version.
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C To refresh a portion of the seismic traces in the display from the 
C pixmap:
C
C         void refresh( x, y, width, height)
C
C long       x = x-pixel coord of upper left corner of area to refresh.
C long       y = y-pixel coord of upper left corner of area to refresh.
C long   width = width (in pixels) of area to refresh.
C long  height = height (in pixels) of area to refresh.
C
C If width  == ImageAll and x == 0, the entire width  is refreshed.
C If height == ImageAll and y == 0, the entire height is refreshed.
C-----------------------------------------------------------------------
C To refresh a portion of the seismic traces in the display from the 
C pixmap:
C
C     void refreshTraces(first_trace, last_trace,
C                        first_time, last_time)
C
C long first_trace = first trace of area to refresh.
C long last_trace  = last trace of area to refresh.
C float first_time = first time (seconds) of area to refresh.
C float last_time  = last time (seconds) of area to refresh.
C
C The parameters first_trace and last_trace have these attributes:
C     - They should both be between 1 (which means the first displayed
C         trace) and getNumberDisplayedTraces() (the last displayed trace).
C     - If these parameters are outside the above range, the refresh
C         will be constrained to be within range.
C     - The actual range refreshed includes two traces on either side
C         of the specified range, to allow for erasing line draws
C         associated with traces in the specified range.
C     - If these two parameters are both zero, the entire width of the
C         display (including annotation) is refreshed.
C
C The parameters first_time and last_time have these attributes:
C     - They should both be between I_sdatTmin(image) (the first
C         displayed time) and I_sdatTmax(image) (the last displayed
C         time).
C     - If these parameters are outside the above range, the refresh
C         will be constrained to be within range.
C     - The actual range refreshed includes 0.2 seconds above and
C         below the specified range, to allow for erasing line draws
C         associated with traces in the specified range.
C     - If these two parameters are both zero, the entire height of the
C         display (including annotation) is refreshed.
C
C This routine calls refresh.
C-----------------------------------------------------------------------
C To refresh a portion of the line draws associated with picks in
C the display:
C
C    void refreshPicks( gc, picks, missing_pick, zero_pick,
C                       first_trace, last_trace, x, y, width, height)
C
C GC gc    = graphics context for drawing picks.
C float picks[] = array of pick values in seconds.
C float missing_pick = special pick value representing a missing pick.
C float    zero_pick = special pick value representing a zero-ed pick.
C long   first_trace = first trace of area to refresh.
C long    last_trace = last trace of area to refresh.
C long       x = x-pixel coord of upper left corner of area to draw.
C long       y = y-pixel coord of upper left corner of area to draw.
C long   width = width (in pixels) of area to draw.
C long  height = height (in pixels) of area to draw.
C
C The pick array must contain one pick value for each trace in the
C    array.  Therefore, this routine is appropriate for some types of
C    picking, but not for other types.
C
C The missing_pick and zero_pick parameters are special values
C    representing picks which should always be displayed ten pixels
C    above the top edge of the display (missing_pick) and at the top
C    edge of the display (zero_pick), regardless of the time value
C    (given by I_sdatTmin(image)) at the top edge of the display.
C
C The actual meaning of missing_pick and zero_pick is not relevant here.
C
C The parameters first_trace and last_trace have the same attributes
C    described above for the routine refreshTraces.
C
C If x,y,width,height are all set to zero, there is no restriction
C    over the time range where the picks may be drawn.  Otherwise,
C    y and height restrict the time range, so that portions of lines
C    outside that range are not drawn, and x and width currently are
C    not used.
C
C This routine draws a single line (as connected straight line segments)
C    connecting all the picks.  No points (special symbols) are plotted
C    at the pick locations.
C
C This routine calls refresh, reduce_segments, and draw_segments.
C-----------------------------------------------------------------------
C To refresh a portion of the points which are plotted in the display:
C
C    void refreshPoints( gc, xpoints, ypoints, npoints,
C                        x, y, width, height)
C
C GC gc           = graphics context for drawing points.
C float xpoints[] = array of x-coordinates of points.
C float ypoints[] = array of y-coordinates of points.
C long  npoints   = number of points.
C long  x         = minimum x pixel value for draws.
C long  y         = minimum y pixel value for draws.
C long  width     = x pixel range for draws.
C long  height    = y pixel range for draws.
C
C Points are drawn only within the x-pixel range from x to x + width.
C Points are drawn only within the y-pixel range from y to y + height.
C If width  == 0, the entire x-pixel range in the display is used.
C If height == 0, the entire y-pixel range in the display is used.
C Nothing is drawn in the border areas.
C
C For expose events, x,y,width,height can be the values in the XEvent
C   structure.
C-----------------------------------------------------------------------
C                                NOTES
C
C  1.
C-----------------------------------------------------------------------
C\END DOC
*/

/*
 * Name        : refresh
 * File        : refresh.c
 * Executable  : - 
 * Author      : Trey Roby / Michael Sherrill
 * Date        : 3/15/92 (C++ version 4/97)
 *
 * refresh the seismic image displayed 
 *
 * CHANGES:
 */

/* -------------------------------------------------------------------------*/

#include "plot_image.hh"
#include "pixmap_set.hh"
#include "sl/ximage.hh"
#include "sl/colorset.hh"
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "wproc.h"
#include "cprim.h"
#include "named_constants.h"



/************************** REFRESH IMAGE *****************************/

void PlotImage::refreshMain(long             x,
                            long             y,
                            long             width,
                            long             height,
                            long             dest_x,
                            long             dest_y,
                            Boolean          border_only,
                            Widget           w)

{
Display *dpy;
GC temp_gc;
Screen *scr;
long x1,x2,y1,y2,fx,fy;
int x3, y3;
long maxx, maxy;
Boolean can_copy = True;
long under_dest_x, under_dest_y;
Pixel *pixels = NULL;
PixmapSet *pixmap_set = NULL;

   if(!_displayed)return;

/*refresh underlay only if no overlay present*/
   if(_chain_image != NULL && _displayed == False)
      {
/*///////////////////////// old ////////////////////////////
      XCopyArea( XtDisplay(w), 
                 _chain_image->_pixmary[_chain_image->_cpixm],
                 XtWindow(w),_chain_image->_gc1,
                 (int)x, (int)y, (int)width, (int)height,(int)x,(int)y);
*////////////////////////// old ////////////////////////////
/////////////////////////// new ////////////////////////////
      if (_ximage_ptr->colorset()->readOnly()) {
	if (!border_only) {
	  int now = 1;
	}
        _ximage_ptr->colorset()->copyDrawable (XtWindow(w),
          _chain_image->_pixmap_set,
	  (int)x, (int)y, (int)width, (int)height,(int)x,(int)y);
      }
      else {
        XCopyArea (XtDisplay(w), 
          _chain_image->_pixmary[_chain_image->_cpixm],
          XtWindow(w),_chain_image->_gc1,
          (int)x, (int)y, (int)width, (int)height,(int)x,(int)y);
      }
/////////////////////////// new ////////////////////////////
      return;
      }

/*if movie of image and chain image make sure same pixmaps are matched*/
   if(_user->_movie && _chain_image != NULL)
    if(_chain_image->_user->_movie)_chain_image->_cpixm = _cpixm;

   dpy=  XtDisplay(w);
   scr = XtScreen(w);
  

   if ( (width  == ImageAll) && (x == 0) ) width = _graph_width;
   if ( (height == ImageAll) && (y == 0) ) height= _graph_height;


   if ( _displayed &&  getCurrentPixmap() ) 
      {
      /*recursive call to overlay bitmap on pixmap*/
      if(_chain_image != NULL)
         { /*images have to have any overlapping areas handled in expose*/
         if(_graph_width   != _chain_image->_graph_width  ||
            _graph_height  != _chain_image->_graph_height ||
            _dest_x        != _chain_image->_dest_x       ||
            _dest_y        != _chain_image->_dest_y       ||
            _ximage.width  != _chain_image->_ximage.width ||
            _ximage.height != _chain_image->_ximage.height  )
	    { // images not same size
            if(_chain_image->_dest_x + _chain_image->_ximage.width <
               _dest_x + _ximage.width)
                fx = dest_x;
            else
                fx = x;
            if(_chain_image->_dest_y + _chain_image->_ximage.height <
               _dest_y + _ximage.height)
                fy = dest_y;
            else
                fy = y;
            XSetForeground(dpy, _chain_image->_gc1,
                           _chain_image->_white_pixel);
            _ximage_ptr->colorset()->paintRectangle (XtWindow(w),
                           _chain_image->_gc1,(int)fx,(int)fy,
                           (int)width,(int)height);

            x1 = max(0,x - _chain_image->_dest_x);
            y1 = max(0,y - _chain_image->_dest_y);
            x2 = min(x + width  - _chain_image->_dest_x - x1,
                     _chain_image->_graph_width - x1);
            y2 = min(y + height - _chain_image->_dest_y - y1,
                     _chain_image->_graph_height - y1);

            under_dest_x = dest_x;
            under_dest_y = dest_y;
            if(border_only && y == 0)/*top annotation being redrawn*/
              {
              if(_dest_x > _chain_image->_dest_x)
                {
                under_dest_x = dest_x - _dest_x + x;
                under_dest_x = min(under_dest_x,_left_border);
                }
              else
                {
                under_dest_x = _chain_image->_dest_x - (x - dest_x);
                under_dest_x = max(under_dest_x,_left_border);
                }
              }
            if (x+width > _chain_image->_dest_x &&
              x<_chain_image->_dest_x+_chain_image->_graph_width && 
              y+height > _chain_image->_dest_y &&
              y<_chain_image->_dest_y + _chain_image->_graph_height) {
	      if (_ximage_ptr->colorset()->readOnly() && !border_only) {
                pixmap_set = _chain_image->_pixmap_set;
		if (!_ximage_ptr->colorset()->comboExists(_pixmap_set,
                  pixmap_set)) {
		  _chain_image->refreshMain (x1, y1, x2, y2,
                    under_dest_x, under_dest_y, border_only,w);
		}
	      }
	      else {
		_chain_image->refreshMain (x1, y1, x2, y2,
                  under_dest_x, under_dest_y, border_only,w);
	      }
	      }
/*
	      if (_ximage_ptr->colorset()->readOnly() && !border_only) {
		_ximage_ptr->colorset()->padPixmap (_pixmap_set,
                  _chain_image->_pixmap_set);
	      }
*/
/*
              if (_ximage_ptr->colorset()->readOnly() && !border_only) {
		_chain_image->_pixmap_set->modifyRange (x2, y2, &x3, &y3,
                  under_dest_x, under_dest_y, _chain_image->_cpixm);
                pixels = _ximage_ptr->colorset()->getPixels (
                  _chain_image->_pixmary[_chain_image->_cpixm], x3, y3,
	          under_dest_x, under_dest_y);
		pixels = _ximage_ptr->colorset()->padPixels (width, height,
		  pixels, x3, y3);
	      }
*/
            }
          else /*both images same size*/
            {
	      if (_ximage_ptr->colorset()->readOnly() && !border_only) {
		pixmap_set = _chain_image->_pixmap_set;
		if (_ximage_ptr->colorset()->comboExists(_pixmap_set,
                  pixmap_set)) {
		  _chain_image->refreshMain(x,y,width,height,dest_x,dest_y,
                    border_only,w);
		}
	      }
	      else {
		_chain_image->refreshMain(x,y,width,height,dest_x,dest_y,
                  border_only,w);
	      }
/*
	      if (_ximage_ptr->colorset()->readOnly() && !border_only) {
		_ximage_ptr->colorset()->padPixmap (_pixmap_set,
		  _chain_image->_pixmap_set);
	      }
*/
/*
            if (_ximage_ptr->colorset()->readOnly() && !border_only) {
	      _chain_image->_pixmap_set->modifyRange (width, height, &x3, &y3,
                dest_x, dest_y, _chain_image->_cpixm);
              pixels = _ximage_ptr->colorset()->getPixels (
                _chain_image->_pixmary[_chain_image->_cpixm],
	        x3, y3, dest_x, dest_y);
	      pixels = _ximage_ptr->colorset()->padPixels (width, height,
		pixels, x3, y3);
	    }
*/
            }   
         temp_gc = _chain_image->_gc1;
         XSetPlaneMask( dpy,temp_gc, _chain_image->_col->pmsk[0]);
         XSetForeground(dpy,temp_gc, _chain_image->_overlay_pixel );
         XSetBackground(dpy,temp_gc, 0 );
         x1 = max(0,x - _dest_x);
         y1 = max(0,y - _dest_y);
         x2 = min(x + width  - _dest_x - x1,_graph_width - x1);
         y2 = min(y + height - _dest_y - y1,_graph_height - y1);
         if(x+width > _dest_x && x < _dest_x + _graph_width &&
            y+height> _dest_y && y < _dest_y + _graph_height)
            {
            x = x1; y = y1; width = x2; height = y2;
            }
         else /*overlay bitmap wasnt exposed*/
            {
            x = y = width = height = 0;
            } 
         }
      else /*no chain image*/
         {
         temp_gc = _gc1;
         }

      if(!border_only) /*set to image structure coordinates*/
	 {
         dest_x = x + _dest_x;
         dest_y = y + _dest_y;
         }


      maxx = (int)_left_border + (int)_right_border
           + _ximage.width;
      maxy = (int)_top_border + (int)_bottom_border
           + _ximage.height;
      if(x > maxx || y > maxy) can_copy = False;
      if(x + width > maxx) width = maxx - x;
      if(y + height> maxy) height= maxy - y;


      if(_ximage.depth > 1)   /*gray scale or color*/
         {
	   if (can_copy) {
/*////////////////////////// old ////////////////////////////
             XCopyArea(  dpy, _pixmary[_cpixm],
               XtWindow(w), temp_gc,
               (int)x, (int)y, (int)width,(int) height,
               (int)dest_x,(int) dest_y );
*/////////////////////////// old ////////////////////////////
/////////////////////////// new ////////////////////////////
             if (_ximage_ptr->colorset()->readOnly()) { 
//             assert (!pixels); // pixels only allowed when depth = 1
               assert (!pixmap_set); // pixmap_set only allowed when depth = 1
	       if (!border_only) {
		 int now = 1;
	       }
               _ximage_ptr->colorset()->copyDrawable (XtWindow(w),
	         _pixmap_set,
	         (int)x, (int)y, (int)width,(int) height,
	         (int)dest_x,(int) dest_y);
	     }
	     else {
               XCopyArea (dpy, _pixmary[_cpixm],
                 XtWindow(w), temp_gc,
                 (int)x, (int)y, (int)width,(int) height,
                 (int)dest_x,(int) dest_y );
	     }
/////////////////////////// new ////////////////////////////
           }
         }
      else /*single bit plane wiggles*/
         {
         if(can_copy)
           {
/*////////////////////////// old ////////////////////////////
           XCopyPlane(  dpy, _pixmary[_cpixm],
                        XtWindow(w), temp_gc,
                        (int)x, (int)y, (int)width, (int)height,
                        (int)dest_x, (int)dest_y, 1);
           XSetPlaneMask(dpy,temp_gc,XAllPlanes());//Allow a future overlay
*/////////////////////////// old ////////////////////////////
/////////////////////////// new ////////////////////////////
           if (_ximage_ptr->colorset()->readOnly()) { 
/*
             if (pixels) {
               _ximage_ptr->colorset()->putOverlayOnPixels (XtWindow(w),
                 _pixmary[_cpixm], pixels,
	         (int)x, (int)y, (int)width, (int)height,
                 (int)dest_x, (int)dest_y);
	       pixels = NULL; // putOverlayOnPixels deallocates pixels
	     }
*/
	     if (pixmap_set) {
               _ximage_ptr->colorset()->copyOverlayOnUnderlay (XtWindow(w),
                 _pixmap_set, pixmap_set,
	         (int)x, (int)y, (int)width, (int)height,
                 (int)dest_x, (int)dest_y);
	       pixmap_set = NULL;
	     }
	     else {
               _ximage_ptr->colorset()->putOverlay (XtWindow(w),
                 _pixmap_set,
	         (int)x, (int)y, (int)width, (int)height,
                 (int)dest_x, (int)dest_y);
	     }
	   }
	   else {
             XCopyPlane(  dpy, _pixmary[_cpixm],
               XtWindow(w), temp_gc,
               (int)x, (int)y, (int)width, (int)height,
               (int)dest_x, (int)dest_y, 1);
             XSetPlaneMask(dpy,temp_gc,XAllPlanes());//Allow a future overlay
	   }
/////////////////////////// new ////////////////////////////
           }
         }          
      } 
   else /*no image just clear window*/
      {
/*////////////////////////// old ////////////////////////////
      XClearWindow( dpy, XtWindow(w) );
*/////////////////////////// old ////////////////////////////
/////////////////////////// new ////////////////////////////
      if (_ximage_ptr->colorset()->readOnly()) { 
        _ximage_ptr->colorset()->clearDrawable (XtWindow(w));
      }
      else {
	XClearWindow( dpy, XtWindow(w) );
      }
/////////////////////////// new ////////////////////////////
      }

/////////////////////////// new ////////////////////////////
// assert (!pixels);
   assert (!pixmap_set);
/////////////////////////// new ////////////////////////////

}




/** the work of the following routine was put into refreshMain in order
    to accomodate passing a drawing area widget to work with. There
    was already a great deal of existing code making the old call
    to refresh that we did not want to change **/

void PlotImage::refresh(  long             x,
                          long             y,
                          long             width,
                          long             height)
{

  refreshMain( x, y, width, height, x, y, False, _graphic);

}

/***************************** REFRESH TRACES **************************/

#define TR_EXP     2   /* amount (# traces) to expand trace range */
#define TIME_EXP 0.2   /* amount (seconds)  to expand time  range */
#define ABOVE     10   /* distance (pixels) above top to plot missing pick */
#define SLIGHTLY   2   /* distance (pixels) above top to plot zeroed  pick */


void PlotImage::refreshTraces(long  first_trace, 
                              long  last_trace, 
                              float first_time,
                              float last_time)
{
  long x1, x2, y1, y2, width, height;

  if(last_trace < first_trace) return;
  if(last_time  < first_time ) return;
  if(first_trace == 0 && last_trace == 0)
     {
     x1 = 0;
     x2 = ImageAll;
     }
  else
     {
     x1 = getXpixelFromTraceNumber((int)first_trace - TR_EXP);
     x2 = getXpixelFromTraceNumber((int)last_trace  + TR_EXP);
     x1 = ConstrainValue(x1, 0, _graph_width);
     x2 = ConstrainValue(x2, 0, _graph_width);
     }
  if(first_time == 0.0 && last_time == 0.0)
     {
     y1 = 0;
     y2 = ImageAll;
     }
  else
     {
     y1 = getYpixelFromTime(first_time - TIME_EXP);
     y2 = getYpixelFromTime(last_time  + TIME_EXP);
     y1 = ConstrainValue(y1, 0, _graph_height);
     y2 = ConstrainValue(y2, 0, _graph_height);
     }
  width  = x2 - x1;
  height = y2 - y1;
  refresh( x1, y1, width, height);
}




/*************************** REFRESH PICKS *************************/

void PlotImage::refreshPicks(GC gc, float picks[],
                             float missing_pick, float zero_pick,
                             long first_trace, long last_trace,
                             long x, long y, long width, long height)
{
  long *xpixels, *ypixels, number;
  int i, trace_number;
  Display *disp = XtDisplay(_graphic);
  Window   wind = XtWindow (_graphic);

  if(last_trace < first_trace) return;
  if(first_trace == 0 && last_trace == 0)
     {
     first_trace = 1;
     last_trace = getNumberDisplayedTraces();
     }
  else
     {
     first_trace = ConstrainValue( first_trace - TR_EXP, 1,
                                   getNumberDisplayedTraces());
     last_trace  = ConstrainValue( last_trace + TR_EXP, 1, 
                                   getNumberDisplayedTraces());
     }
  number = last_trace - first_trace + 1;
  xpixels = (long*)malloc((int)number * sizeof(long));
  ypixels = (long*)malloc((int)number * sizeof(long));
  for(i = 0; i < number; i++)
     {
     trace_number = (int)first_trace + i;
     xpixels[i] = getXpixelFromTraceNumber(trace_number);
     if (picks[trace_number - 1] == missing_pick)
        {
        ypixels[i] = (int)_top_border - ABOVE;
        }
     else if(picks[trace_number - 1] == zero_pick)
        {
        ypixels[i] = (int)_top_border - SLIGHTLY;
        XFillRectangle(disp, wind, gc, (int)xpixels[i]-2,(int)ypixels[i]-2, 
                       5 , 5 );
        }
     else
        {
        ypixels[i] = getYpixelFromTime(picks[trace_number - 1]);
        }
     }

/*
       The lines drawn by draw_segments are badly messed up if
       reduce_segments is called while XFillRectangle is included
       above.  If XFillRectangle is omitted, then reduce_segments
       works fine.
  number = reduce_segments(xpixels, ypixels, number);
*/
  draw_segments(_graphic, gc, xpixels, ypixels,
                                   number, (int)x, (int)y, (int)(x+width), 
                                   (int)(y+height));
  free(xpixels);
  free(ypixels);

}

/*************************** REFRESH POINTS ************************/

#define POINT_SIZE  7    /* should be odd number */

void PlotImage::refreshPoints(GC gc,
                              float xpoints[], float ypoints[], long npoints,
                              long x, long y, long width, long height)
{
  int xpixel, ypixel;
  int i;
  Display *disp = XtDisplay(_graphic);
  Window   wind = XtWindow (_graphic);
  long xmin = (int)_left_border;
  long ymin = _top_border;
  long xmax = _graph_width - _right_border;
  long ymax = _graph_height - (int)_bottom_border;
  long half = POINT_SIZE / 2;

  if(width)  
     { 
     xmin = MaximumValue(xmin, x          - half);
     xmax = MinimumValue(xmax, x + width  + half); 
     }
  if(height) 
     { 
     ymin = MaximumValue(ymin, y          - half);
     ymax = MinimumValue(ymax, y + height + half); 
     }
  for(i = 0; i < npoints; i++)
     {
     xpixel = getXpixelFromX(xpoints[i]);
     ypixel = getYpixelFromY(ypoints[i]);
     if(xpixel >= xmin && xpixel <= xmax && ypixel >= ymin && ypixel <= ymax)
        {
        XFillRectangle(disp, wind, gc, (int)(xpixel - half), 
                       (int)(ypixel - half), POINT_SIZE, POINT_SIZE);
        }
     }
}
