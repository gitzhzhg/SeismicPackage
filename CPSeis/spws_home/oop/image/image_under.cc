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
 *Name   : createUnderlay 
 *Purpose: Generate a image to be used as the background for stacked
 *         trace underlays. If the underlay image is to be created from
 *         a byte or tfile the image->user->mode should be set to
 *         PlotCOLOR. If the underlay image is to be created
 *         from some other type file the image->user->mode should be
 *         set to PlotARRAY and the data should be already read into
 *         the byte or float array of the underlay image.
 *
 *Author : Michael L. Sherrill
 *Date   : 12/93 (C++ version 4/97)
 *
 * Function Definition:
 *
 * int createUnderlay( PlotImage *under,
 *                     PlotImage *over)
 *
 * under         in      Pixmap image to be underlayed by bitmap.
 * over          in      Bitmap image which will underlay the pixmap
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
#include "sl/ximage.hh"

#define resource_err "Not enough memory for the underlay image."
#define bounds_err "Could not match overlay/underlay coordinates.\n" \
                   "Trace sequence numbers (Hdr Wrd 1) may not overlap\n" \
                   "or Z-ranges may not overlap."
#define planes_err "Underlay plane colors have not been allocated."


long PlotImage::createUnderlay( PlotImage *under,
                                PlotImage *over)

{
 float hpixels_per_inch;
 float vpixels_per_inch;
 Display *dpy;
 int screen;
 Boolean out_of_bounds = False;
 char *tempstr;
 float undermin,undermax,overmin,overmax;
 float under_user_tmin, under_user_tmax;
 float over_user_tmin, over_user_tmax;
 float over_user_xmin, over_user_xmax;
 tempstr = "";

  under_user_tmin = min(under->_user->_tmin,under->_user->_tmax);
  under_user_tmax = max(under->_user->_tmin,under->_user->_tmax);
  over_user_tmin  = min(over->_user->_tmin,over->_user->_tmax);
  over_user_tmax  = max(over->_user->_tmin,over->_user->_tmax);


  if(over->_statusw) tempstr= wprocPushMsg(  over->_statusw, "");
 
  if(under->_user->_mode == PlotWONLY || under->_user->_mode == PlotWFILL)
    under->_user->_mode = PlotCOLOR;

  if(over->_displayed)
    {
    if(over->_user->_mode == PlotWONLY || over->_user->_mode == PlotWFILL)
       {
       over_user_xmin = over->_hd[(over->getFirstTraceInImage() - 1) 
                             * over->getNumberOfHeaders() 
                             + over->_coordinate_header - 1];
       over_user_xmax = over->_hd[((over->getFirstTraceInImage() - 1) 
                             + over->getNumberDisplayedTraces()-1) 
                             * over->getNumberOfHeaders() + 
                             over->_coordinate_header - 1];
       }
    else
       {
       over_user_xmin = over->_user->_grid_x1;
       over_user_xmax = over->_user->_grid_x2;  
       }
    }


/* check image area matching -- the range of trace sequence numbers (hwd 1)
   must overlap between the overlay and underlay. Aditionally the trace
   time windows must overlap. Otherwise an error is printed */
  if(over->_displayed)
     {
     if(under_user_tmax <= over_user_tmin ||
        under_user_tmin >= over->_user->_tmax) out_of_bounds = True;
     undermin   = min(under->_user->_grid_x1,under->_user->_grid_x2);
     undermax   = max(under->_user->_grid_x1,under->_user->_grid_x2);
     overmin = min(over_user_xmin,over_user_xmax);
     overmax = max(over_user_xmin,over_user_xmax); 
     if(undermin < overmin && undermax < overmin)out_of_bounds = True;
     if(undermin > overmax && undermax > overmax)out_of_bounds = True;
     }

  if(out_of_bounds)
     {
     strcpy(_errstr,bounds_err);
     under->_displayed= False;
     over->_chain_image = NULL;
     if (over->_statusw) wprocPopMsg(  over->_statusw, tempstr);
     printf("underlay and overlay image regions do not overlap\n");
     return ( ResourceFail );
     }

/*set up underlay image*/
  under->_graphic = over->_graphic;
  under->_can_overlay = True;  


  dpy = XtDisplay(under->_graphic);
  if(over->_displayed) under->_user->_is = over->_is;
  screen = DefaultScreen(XtDisplay(under->_graphic));
  hpixels_per_inch = horizontalPixelsPerInch(dpy, screen);
  vpixels_per_inch = verticalPixelsPerInch(dpy, screen);



/*handle underlay only case*/
  if(!over->_displayed){
    over->_chain_image = under;
    under->_over_image = NULL;
    under->_ximage.height = (int)(under->_user->_is * vpixels_per_inch);
    if(under->_ximage.height < 0) 
       under->_ximage.height = (-under->_ximage.height);
    return(PlotSuccess);
  }



  under->_ximage.height = (int)((under_user_tmax-under_user_tmin) 
                       * over->_user->_is * vpixels_per_inch);
  if(under->_ximage.height < 0) under->_ximage.height = (-under->_ximage.height);

  if(under->_ximage.height != over->_ximage.height)
     {
     under->_height_diff = over->_ximage.height - under->_ximage.height;
     /*under->_ximage.height = over->_ximage.height;*/
     }
  else
     {
     under->_height_diff = 0;
     }

  if(under->_user->_mode == PlotSEMB || under->_user->_mode == PlotCONTOUR)
     under->_trace_delta = 
           (long)((under->_user->_ti*hpixels_per_inch) / under->_user->_nplt);
  else
     under->_trace_delta = (long)(hpixels_per_inch / under->_user->_ti + .5);

  switch(under->_user->_mode)
    {
    case PlotISO:
    case PlotGRID:
    case PlotHEADER:
    case PlotSEMB:
    case PlotCONTOUR:
          under->_ximage.width = (int)(under->_user->_ti * hpixels_per_inch);
          if(under->_ximage.width < 0)
              under->_ximage.width=(-under->_ximage.width);
          if(!under->_ximage.width)under->_ximage.width = 1;
          break;   

    case PlotCOLOR:
          under->_ximage.width  = (int)(under->_user->_nplt 
                                        * under->_trace_delta);
          if(!under->_ximage.width)under->_ximage.width = 1;
          break;
    }

///////////////// new /////////////////
// force _ximage_ptr to use the newly defined height and width
  under->_ximage_ptr->create (&under->_ximage);
///////////////// new /////////////////


  setDestinations(over,under);

  over->_chain_image = under;
  under->_over_image = over;
  return(PlotSuccess);

}




/****************** Function to line up over and under images ************/

void PlotImage::setDestinations( PlotImage *over,
                                 PlotImage *under)
{               
Boolean out_of_bounds = False;
float undermin = 0,undermax = 0,overmin = 0,overmax = 0;
double y_perpix, x_perpix;
float under_user_tmin, under_user_tmax;
float over_user_tmin, over_user_tmax;
double over_tmin, over_tmax;
float over_user_xmin, over_user_xmax;
float leftside;
Boolean decrementing = False;
float over_x1, over_x2;
float under_x1, under_x2;

  under_user_tmin = (under->_zoomed) ? min(under->_tmin,under->_tmax) 
                                : min(under->_user->_tmin,under->_user->_tmax);
  under_user_tmax = (under->_zoomed) ? max(under->_tmin,under->_tmax) 
                                : max(under->_user->_tmin,under->_user->_tmax);
  over_user_tmin  = (over->_zoomed)  ? min(over->_tmin,over->_tmax) 
                                : min(over->_user->_tmin,over->_user->_tmax);
  over_user_tmax  = (over->_zoomed)  ? max(over->_tmin,over->_tmax)
                                : max(over->_user->_tmin,over->_user->_tmax);
  over_tmin       = min(over->_tmin,over->_tmax);
  over_tmax       = max(over->_tmin,over->_tmax);

  if(over->_user->_mode == PlotWONLY || over->_user->_mode == PlotWFILL)
    {
     over_user_xmin = over->_hd[(over->getFirstTraceInImage() - 1) 
                             * over->getNumberOfHeaders()
                             + over->_coordinate_header - 1];
     over_user_xmax = over->_hd[((over->getFirstTraceInImage()- 1)
                             + over->getNumberDisplayedTraces()-1)
                             * over->getNumberOfHeaders()
                             + over->_coordinate_header - 1];
    }
  else
    {
     over_user_xmin = over->_user->_grid_x1;
     over_user_xmax = over->_user->_grid_x2;
    }

  over_x1 = over_user_xmin;
  over_x2 = over_user_xmax;

  if(over->_displayed)
    {
     if(!over->_zoomed)
        {
        if(under_user_tmax <= over_user_tmin ||
           under_user_tmin >= over->_user->_tmax) out_of_bounds = True;
        undermin   = min(under->_user->_grid_x1,under->_user->_grid_x2);
        undermax   = max(under->_user->_grid_x1,under->_user->_grid_x2);
        overmin = min(over_user_xmin,over_user_xmax);
        overmax = max(over_user_xmin,over_user_xmax);
        if(undermin < overmin && undermax < overmin)out_of_bounds = True;
        if(undermin > overmax && undermax > overmax)out_of_bounds = True;
        }
     else
        {
        if(under_user_tmax <= over_user_tmin ||
           under_user_tmin >= over->_user->_tmax) out_of_bounds = True;
        undermin=(under->_zoomed)?min(under->_grid_x1,under->_grid_x2) 
                           :min(under->_user->_grid_x1,under->_user->_grid_x2);
        undermax=(under->_zoomed)?max(under->_grid_x1,under->_grid_x2)
                           :max(under->_user->_grid_x1,under->_user->_grid_x2);
        overmin = min(over_user_xmin,over_user_xmax);
        overmax = max(over_user_xmin,over_user_xmax);
        if(undermin < overmin && undermax < overmin)out_of_bounds = True;
        if(undermin > overmax && undermax > overmax)out_of_bounds = True;
	}
   }

  if(out_of_bounds)return;

 
/*line up x axis*/
   if(!over->_user->getRightToLeft())
     {
     over_x1 = over->_hd[(over->getFirstTraceInImage() - 1) 
                           * over->getNumberOfHeaders()
                           + over->_coordinate_header - 1];
     over_x2 = over->_hd[((over->getFirstTraceInImage() - 1)
                           + over->getNumberDisplayedTraces()-1)
                           * over->getNumberOfHeaders()
                           + over->_coordinate_header - 1];
     }
   else
     {
     over_x2 = over->_hd[(over->getFirstTraceInImage() - 1) 
                          * over->getNumberOfHeaders()
                          + over->_coordinate_header - 1];
     over_x1 = over->_hd[((over->getFirstTraceInImage() - 1)
                          + over->getNumberDisplayedTraces()-1)
                          * over->getNumberOfHeaders() 
                          + over->_coordinate_header - 1];
     }

   if(under->isDisplayed())
     {
     if(!under->_user->getRightToLeft())
      {
       under_x1 = under->_hd[(under->getFirstTraceInImage() - 1) 
                           * under->getNumberOfHeaders()
                           + under->_coordinate_header - 1];
       under_x2 = under->_hd[((under->getFirstTraceInImage() - 1)
                           + under->getNumberDisplayedTraces()-1)
                           * under->getNumberOfHeaders()
                           + under->_coordinate_header - 1];
       }
     else
       {
       under_x2 = under->_hd[(under->getFirstTraceInImage() - 1) 
                          * under->getNumberOfHeaders()
                          + under->_coordinate_header - 1];
       under_x1 = under->_hd[((under->getFirstTraceInImage() - 1)
                          + under->getNumberDisplayedTraces()-1)
                          * under->getNumberOfHeaders() 
                          + under->_coordinate_header - 1];
       }
     }
   else//not displayed yet so use user input for coordinates
     {
     if(!under->_user->getRightToLeft())
       {
       under_x1 = under->_user->_grid_x1;
       under_x2 = under->_user->_grid_x2;
       }
     else
       {
       under_x2 = under->_user->_grid_x1;
       under_x1 = under->_user->_grid_x2;
       } 
     }

  if(over_x1 > over_x2)
    decrementing = True;
  
  if(!decrementing)
    leftside = min(over_x1,under_x1);
  else
    leftside = max(over_x1,under_x1);

  if(under->_user->_mode == PlotARRAY)//not a trace type underlay
    x_perpix = (max(over_x1,over_x2) - min(over_x1,over_x2)) /
               (over->_ximage.width - over->_first_trace_location - 1);  
  else//trace type underlay
    x_perpix =((float)(over->_ntot)) 
               / ((float)(over->_ntot * over->_trace_delta - 1));
 
  over->_dest_x = (int)((over_x1  - leftside) / x_perpix +.5);
  under->_dest_x= (int)((under_x1 - leftside) / x_perpix + .5);

  if(over->_dest_x < 0) over->_dest_x  = -over->_dest_x;
  if(under->_dest_x< 0) under->_dest_x = -under->_dest_x;




/*line up y axis note if matching problems occur later may need to pattern
  the following more like the above x axis alignment*/
  if(under_user_tmin < over_tmin)
     {
     y_perpix = (under_user_tmax - under_user_tmin) / under->_ximage.height;
     if(y_perpix < 0.0) y_perpix = (-y_perpix);
     over->_dest_y = (int)((over_tmin - under_user_tmin) / y_perpix);
     under->_dest_y = 0;
     }
  else if(under_user_tmin > over_tmin)
     {
     y_perpix = over->getYvaluePerPixel();
     if(y_perpix < 0.0) y_perpix = (-y_perpix);
     under->_dest_y = (int)((under_user_tmin - over_tmin) / y_perpix);
     over->_dest_y = 0;
     }
  else if(under_user_tmin == over_tmin)
     {
     over->_dest_y = 0;
     under->_dest_y= 0;
     }

  over->_orig_dest_x = over->_dest_x;
  over->_orig_dest_y = over->_dest_y;
  under->_orig_dest_x = under->_dest_x;
  under->_orig_dest_y = under->_dest_y;
}

