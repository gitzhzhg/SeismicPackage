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
 *Name   : create_overlay 
 *Purpose: Generate a image to be used as the background for stacked
 *         trace overlays. If the overlay image is to be created from
 *         a byte or tfile the image->user->mode should be set to
 *         PlotCOLOR. If the overlay image is to be created
 *         from some other type file the image->user->mode should be
 *         set to PlotGrid and the data should be already read into
 *         the byte array of the overlay image.
 *
 *Author : Michael L. Sherrill
 *Date   : 12/93
 *
 * Function Definition:
 *
 * int image_overlay( struct PlotImage *over,
 *                    struct PlotImage *parent)
 *
 * over         in      Pixmap image to be overlayed by bitmap.
 * parent       in      Bitmap image which will overlay the pixmap
 *
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
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <Xm/Xm.h>
#include "plot_image.hh"

#define resource_err "Not enough memory for the overlay image."
#define bounds_err "Could not match image boundaries."
#define planes_err "Overlay plane colors have not been allocated."


long PlotImage::createOverlay( PlotImage *over,
                               PlotImage *parent)

{
 float hpixels_per_inch;
 float vpixels_per_inch;
 Display *dpy;
 int screen;
 long stat;
 Boolean out_of_bounds = False;
 char *tempstr;
 float overmin,overmax,parentmin,parentmax;




 if (parent->_statusw) tempstr= wprocPushMsg(  parent->_statusw, "");

/*make sure we have enough planes*/
  if(!over->_col->numplanes)
     {
     strcpy(_errstr,planes_err);
     over->_displayed= False;
     if (parent->_statusw) wprocPopMsg(  parent->_statusw, tempstr);
     return ( ResourceFail );
     }


/*check image area matching*/
  if(over->_user->_tmax <= parent->_user->_tmin ||
     over->_user->_tmin >= parent->_user->_tmax) out_of_bounds = True;
  overmin   = min(over->_user->_grid_x1,over->_user->_grid_x2);
  overmax   = max(over->_user->_grid_x1,over->_user->_grid_x2);
  parentmin = min(parent->_user->_grid_x1,parent->_user->_grid_x2);
  parentmax = max(parent->_user->_grid_x1,parent->_user->_grid_x2);

  if(overmin < parentmin && overmax < parentmin)out_of_bounds = True;
  if(overmin > parentmax && overmax > parentmax)out_of_bounds = True;

  if(out_of_bounds)
     {
     strcpy(_errstr,bounds_err);
     over->_displayed= False;
     if (parent->_statusw) wprocPopMsg(  parent->_statusw, tempstr);
     return ( ResourceFail );
     }

/*set up overlay image*/
  over->_graphic = parent->_graphic;
  over->_can_overlay = True;  
  dpy = XtDisplay(over->_graphic);
  screen = DefaultScreen(XtDisplay(over->_graphic));
  hpixels_per_inch = horizontalPixelsPerInch(dpy, screen);
  vpixels_per_inch = verticalPixelsPerInch(dpy, screen);
  over->_user->_ti = ((float)(parent->_ximage.width-1)) / hpixels_per_inch;
  over->_user->_is = parent->_user->_is;
  over->_grid_x1 = parent->_grid_x1;
  over->_grid_x2 = parent->_grid_x2;
  over->_user->_LblInc = 0;
  over->_user->_ptl = 0.0;
  over->_user->_stl = 0.0;
  over->_user->_tmin = max(parent->_tmin,over->_user->_tmin);
  over->_user->_tmax = min(parent->_tmax,over->_user->_tmax);
  over->_ximage.height = (int)((over->_user->_tmax - over->_user->_tmin - 
                          over->_user->_G.srval)
                          * parent->_user->_is * vpixels_per_inch);

  if(over->_ximage.height < parent->_ximage.height)
     {
     over->_height_diff = parent->_ximage.height-over->_ximage.height;
     over->_ximage.height = parent->_ximage.height;
     }
  else
     {
     over->_height_diff = 0;
     }

/*check size and allocate memory*/
  stat = over->checkSize();
  if(stat != PlotSuccess)
     {
     over->_displayed= False;
     if (parent->_statusw) wprocPopMsg(  parent->_statusw, tempstr);
     return ( stat );
     }

/*create the image*/
  stat = over->plot();



  return(stat);

}
