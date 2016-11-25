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
 *Name   : setxy_out and showxy
 *Purpose: Install the mouse readout widgets.
 *         Convert an x y pixel location on an image to data values.
 *
 *Author : Michael L. Sherrill
 *Date   : 11/91 (C++ version 4/97)
 *
 * Function Definition:
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
#include "plot_image.hh"
#include "xy_display.hh"
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>
#include <Xm/Xm.h>
#include <X11/Shell.h>
#include <Xm/Label.h>
#include <Xm/PushB.h>
#include <Xm/DrawingA.h>
#include <math.h>


#define vel_label "Z:"
#define time_label "Y:"
#define depth_label "Depth:"
#define x_label "X:"
#define y_label "Y:"
#define amp_label "Amp:"
#define int_label "Inv:"
#define no_label " "


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void PlotImage::showxy( Widget /*w*/,
                        PlotImage *image,
                        XEvent *event)
{

  XMotionEvent *ev;
  Widget xloc, yloc, zloc;
  long stat;
  long xindex;
  char x_str[20];
  char y_str[20];
  char a_str[20];
  double yvalue;
  double xvalue;



  if (event->type == MotionNotify) {
       Widget w= image->_graphic;
       if (wpMoreEvents( XtDisplay(w), XtWindow(w), MotionNotify) ) return;
  }


  xloc= image->_xydisp->xloc;
  yloc= image->_xydisp->yloc;
  zloc= image->_xydisp->zloc;



  switch(image->_user->_mode)
  {
      case PlotWONLY:
      case PlotWFILL:
      case PlotCOLOR:
      case PlotGRID:
      case PlotHEADER:
                 if(image->_xydisp->mouse_readout_type == MOUSE_AMP)
                    {
                    if (image->_xydisp->override_x_str) 
                       strcpy(x_str, image->_xydisp->override_x_str);
                    else
                       strcpy(x_str, x_label);
                    if (image->_xydisp->override_y_str) 
                       strcpy(y_str, image->_xydisp->override_y_str);
                    else if(image->_user->_depth) 
                       strcpy(y_str, depth_label);
                    else 
                       strcpy(y_str, time_label);
                    if(image->_user->_mode == PlotGRID || 
                       image->_user->_mode == PlotHEADER)
                      strcpy(a_str, no_label);
                    else 
                      strcpy(a_str, amp_label);
                    }
                 else if(image->_xydisp->mouse_readout_type == MOUSE_VEL)
                    {
                    if (image->_xydisp->override_x_str) 
                       strcpy(x_str, image->_xydisp->override_x_str);
                    else
                       strcpy(x_str, x_label);
                    if (image->_xydisp->override_y_str) 
                       strcpy(y_str, image->_xydisp->override_y_str);
                    else if(image->_user->_depth)
                       strcpy(y_str, depth_label);
                    else
                       strcpy(y_str, time_label);
                    strcpy(a_str, vel_label);          
                    }
                 else
		    {
                    if(image->_xydisp->override_x_str) 
                       strcpy(x_str, image->_xydisp->override_x_str);
                    else
                       strcpy(x_str, x_label);
                    if(image->_xydisp->override_y_str) 
                       strcpy(y_str, image->_xydisp->override_y_str);
                    else if(image->_user->_depth)
                       strcpy(y_str, depth_label);
                    else
                       strcpy(y_str, time_label);
                    strcpy(a_str, image->_aux_label);
                    }
      break;

      case PlotCONTOUR:
      case PlotSEMB:
                 if(image->_xydisp->override_x_str) 
                    strcpy(x_str, image->_xydisp->override_x_str);
                 else
                    strcpy( x_str, vel_label);
                 if(image->_xydisp->override_y_str) 
                    strcpy(y_str, image->_xydisp->override_y_str);
                 else if(image->_user->_depth)
                    strcpy(y_str, depth_label);
                 else
                    strcpy(y_str, time_label);
                 strcpy(a_str, int_label);
      break;
      case PlotISO:
                 if(image->_xydisp->override_x_str) 
                    strcpy(x_str, image->_xydisp->override_x_str);
                 else
                    strcpy(x_str, x_label);
                 if(image->_xydisp->override_y_str) 
                    strcpy(y_str, image->_xydisp->override_y_str);
                 else if(image->_user->_depth)
                    strcpy(y_str, depth_label);
                 else
                    strcpy(y_str, time_label);
                 strcpy(a_str, vel_label);
      break;
  }


  switch (event->type) {


    case EnterNotify :
                  if(image->_xydisp->mouse_readout_type == MOUSE_AUX) {
                    if (zloc) wprocVAShowMsg( zloc,"%s %6.2f",
                                              a_str, image->_aux_value);
                  }
    break;          
 

    case LeaveNotify :
                  XCrossingEvent *lev;
                  lev= (XCrossingEvent*)event;
                  //printf("Leave: detail: %d, NotifyInferior: %d\n", 
                  //                lev->detail, NotifyInferior);
                  if (lev->detail != NotifyInferior) {
                      if (xloc) wprocShowMsg( xloc, x_str );
                      if (yloc) wprocShowMsg( yloc, y_str );
                      if(image->_xydisp->mouse_readout_type != MOUSE_AUX)
                        if (zloc) wprocVAShowMsg( zloc, a_str );
                      else {
                        if (zloc) 
                              wprocVAShowMsg( zloc,"%s %6.2f",
                                          a_str, image->_aux_value);
                      }
                      if (image->_xy_output_function)
                           image->_xy_output_function( 
                                   image->_xydisp->outdata,-1,-1);
                  } // end if lev->detail
    break;



    case MotionNotify :
            if(image->_displayed)
               {
               ev= (XMotionEvent *)event;
               /*convert pixels to data coordinates*/
               stat= image->readoutXY(ev->x,ev->y,&xindex,&xvalue,&yvalue,
                                      &image->_aux_value);
               if(stat) /*in bounds of image*/
                  {
                  if (xloc) wprocVAShowMsg( xloc, "%s %5.3f",x_str, xvalue);
                  if (yloc) wprocVAShowMsg( yloc, "%s %5.3f",y_str, yvalue);
                  if (zloc && (image->_user->_mode != PlotSEMB))
                     {
                     if(image->_xydisp->mouse_readout_type == MOUSE_AMP)
		       {
                       if(image->_user->_mode == PlotGRID || 
                         image-> _user->_mode == PlotHEADER)
                        wprocVAShowMsg(zloc,"%s %s",a_str, "");
                       else 
                       wprocVAShowMsg(zloc,"%s %10.7e",a_str,image->_aux_value);
		       }
                     else if(image->_xydisp->mouse_readout_type == MOUSE_VEL)
		       {
                       wprocVAShowMsg( zloc,"%s %6.2f",a_str,image->_aux_value);
		       }
		     }
                  if (image->_xy_output_function)
                     image->_xy_output_function(image->_xydisp->outdata, 
                                                     ev->x, ev->y);
                  }
               else  /*out of image bounds*/
                  {
                  if (xloc) wprocVAShowMsg( xloc, x_str );
                  if (yloc) wprocVAShowMsg( yloc, y_str );
                  if(image->_xydisp->mouse_readout_type != MOUSE_AUX)
                    {
                    if (zloc) wprocVAShowMsg( zloc, a_str );
                    }
                  else
                    {
                    if (zloc)wprocVAShowMsg( zloc,"%s %6.2f",a_str,
                                            image->_aux_value);
                    }
                  if (image->_xy_output_function)
                      image->_xy_output_function(image->_xydisp->outdata,-1,-1);
                  }
            }
    break;

  } // end switch

}


/*
 * ------------------------------ FUNCTION ----------------------------------
 */
void PlotImage::setXYout(Widget           xloc,
                         Widget           yloc,
                         Widget           zloc,
                         int              newstat )


{



   _xydisp->xloc= xloc;
   _xydisp->yloc= yloc;
   _xydisp->zloc= zloc;

   if ((!xloc)&&(!yloc)&&(!zloc)) newstat= XYOFF;

   if (!_xydisp->init) 
      {
      switch (_xydisp->status) 
         {
         case XYAUTO :
                XtRemoveEventHandler(_graphic,
                                     PointerMotionMask|EnterWindowMask|
                                     LeaveWindowMask,
                                     False, (XtEventHandler)showxy, this);


         break;
 
         case XYNOAUTO :
                XtRemoveEventHandler(_graphic,
                                     PointerMotionMask|EnterWindowMask|
                                     LeaveWindowMask|PointerMotionHintMask,
                                     False, (XtEventHandler)showxy, this);
         break;
         }
      }
   else
      {
      _xydisp->init= False; 
      }






   switch (newstat) 
      {
      case XYAUTO :
                XtAddEventHandler(_graphic,
                                  PointerMotionMask|EnterWindowMask|
                                  LeaveWindowMask,
                                  False, (XtEventHandler)showxy, this );
      break;

      case XYNOAUTO :
                XtAddEventHandler(_graphic,
                                  PointerMotionMask|EnterWindowMask|
                                  LeaveWindowMask|PointerMotionHintMask,
                                  False, (XtEventHandler)showxy, this);
      break;
      }




   _xydisp->status= newstat;

}



