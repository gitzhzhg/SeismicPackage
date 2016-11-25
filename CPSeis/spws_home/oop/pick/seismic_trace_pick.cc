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

//---------------------- seismic_trace_pick.cc -----------------------//
//---------------------- seismic_trace_pick.cc -----------------------//
//---------------------- seismic_trace_pick.cc -----------------------//

//       implementation file for the SeismicTracePick class
//                 derived from the PickBase class
//                        subdirectory pick


//   This class controls picking on traces in a SeisPlot display.
//   It should be constructed when picking is to start, and destroyed
//   when picking is finished.

//   This class creates its own rubberband vector and rubberband
//   data classes.  These two classes are created when the button
//   is pressed, and deleted when the button is released.

//   The drawn rubberband is a straight line defined by the button-down
//   and the button-motion-or-release locations, but with each end point
//   adjusted slightly to fall exactly on a trace.

//   The following virtual functions are available for overriding:
//     verifyAction     to verify whether to use the current action.
//     actionCompleted  to notify when the action is completed.

//   If verifyAction returns FALSE, a bell will be sounded, the
//     rubberband will be drawn with a zero-width line, and the
//     actionCompleted function will not be called.

//   If verifyAction is not overridden, all of the following actions
//   will be permitted:    button1, shft button1, cntl button1,
//                         button2, shft button2, cntl button2.

//   The virtual function actionCompleted must be overridden, in order
//   to do something useful when the action is terminated.


#include "pick/seismic_trace_pick.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_winman.hh"
#include "vect/vect_data.hh"
#include "vect/ll_seis_vect.hh"
#include "cprim.h"
#include "named_constants.h"
#include <string.h>
#include <iostream.h>
#include <assert.h>


//------------------- constructor --------------------------------//
//------------------- constructor --------------------------------//
//------------------- constructor --------------------------------//

SeismicTracePick::SeismicTracePick(SeisPlot           *sp,
                                   SeisVectLinkedList *vectors,
                                   const char * const  picking_mode,
                                   const char * const  help_token,
                                   const char * const  help_fallback,
                                   const char * const  color_name,
                                   Cursor              cursor,
                                   int                 line_width)
        : PickBase(sp, (char*)picking_mode, help_token, help_fallback,
                                          (unsigned int)cursor),
/*
                     _sp          (sp),         // removed 9/4/97
*/
                     _winman      (sp->getSeisWinMan()),   // new 9/4/97
                     _vectors     (vectors),
                     _color_name  (newstr((char*)color_name)),
                     _line_width  (line_width),
                     _rub_vector  (NULL),
                     _rub_data    (NULL)
{
  assert(_winman && _vectors);
  //// cout << "am in SeismicTracePick constructor" << endl;
}


//------------------------ destructor ----------------------------//
//------------------------ destructor ----------------------------//
//------------------------ destructor ----------------------------//

SeismicTracePick::~SeismicTracePick()
{
  //// cout << "am in SeismicTracePick destructor" << endl;
  if(_rub_vector) _vectors->remove(_rub_vector);
  if(_rub_data) delete _rub_data;
  if(_color_name) free((char*)_color_name);
}


//------------------------ pick helpers --------------------------//
//------------------------ pick helpers --------------------------//
//------------------------ pick helpers --------------------------//

void SeismicTracePick::pickPress(float *xdata, float *ydata,
                                        Boolean doit)
{
  _rub_data = new VectData(2, xdata, ydata);
  if(doit)
      {
      _rub_vector = _vectors->add(_rub_data, _color_name,
                                       _line_width, True);
      }
  else
      {
      _rub_vector = _vectors->add(_rub_data, _color_name,
                                                 0, True);
      SeisPlot *sp = _winman->currentSP();
      XBell(XtDisplay(sp->W()), 50);
      }
}


void SeismicTracePick::pickMotion(float *xdata, float *ydata)
{
  _rub_data->replace(0, 2, xdata, ydata);
}


void SeismicTracePick::pickRelease(float *xdata, float *ydata,
                     Boolean doit, int button, Modifier modifier)
{
  _vectors->remove(_rub_vector);
  delete _rub_data;
  _rub_vector  = NULL;
  _rub_data    = NULL;
  if(!doit) return;
  long first_trace, last_trace, direction;
  float first_time, last_time;
  if(xdata[1] >= xdata[0])
     {
     direction   = FORWARD;
     first_trace = NearestInteger(xdata[0]);
     last_trace  = NearestInteger(xdata[1]);
     first_time  = ydata[0];
     last_time   = ydata[1];
     }
  else
     {
     direction   = BACKWARD;
     first_trace = NearestInteger(xdata[1]);
     last_trace  = NearestInteger(xdata[0]);
     first_time  = ydata[1];
     last_time   = ydata[0];
     }
  SeisPlot *sp = _winman->currentSP();
  actionCompleted(sp, button, modifier, direction,
                  first_trace, last_trace, first_time, last_time);
}



//--------------------- verify action ------------------------//
//--------------------- verify action ------------------------//
//--------------------- verify action ------------------------//

      //  Virtual function to override.
      //  Return doit = TRUE  if action is to be used.
      //  Return doit = FALSE if action is to be ignored.
      //  Possible values of button   = 1, 2.
      //  Possible values of modifier = none, shft, cntl.
      //  Possible values of action   = press, motion, release.

      //  The action should not be used to decide whether to doit.
      //  The action is in the argument list only for informational
      //    purposes (for example if you want to display a message
      //    about the action in progress).

Boolean SeismicTracePick::verifyAction(int /*button*/, Modifier /*modifier*/,
                                       Action /*action*/)
{
  return TRUE;
}



//--------------------- get end points ------------------------//
//--------------------- get end points ------------------------//
//--------------------- get end points ------------------------//

void SeismicTracePick::getEndPoints(int x1, int x2, int y1, int y2,
                                float xdata[2], float ydata[2])
{
  SeisPlot *sp = _winman->currentSP();
  xdata[0] = sp->xWC(x1);
  xdata[1] = sp->xWC(x2);
  ydata[0] = sp->yWC(y1);
  ydata[1] = sp->yWC(y2);
  long n   = sp->memoryTraces();
  long first_trace = NearestInteger(xdata[0]);
  long last_trace  = NearestInteger(xdata[1]);
  first_trace = ConstrainValue(first_trace, 1, n);
  last_trace  = ConstrainValue(last_trace , 1, n);
  if(first_trace != last_trace)
     {
     float m = (ydata[1] - ydata[0]) / (xdata[1] - xdata[0]);
     float b = ydata[1] - m * xdata[1];
     xdata[0] = (float)first_trace;
     xdata[1] = (float)last_trace;
     ydata[0] = m * xdata[0] + b;
     ydata[1] = m * xdata[1] + b;
     }
  else
     {
     xdata[0] = (float)first_trace;
     xdata[1] = (float)last_trace;
     }
}



//---------------------- button any -------------------------//
//---------------------- button any -------------------------//
//---------------------- button any -------------------------//

void SeismicTracePick::buttonAny (int x1, int x2, int y1, int y2,
                 int button, Action action, Modifier modifier)
{
  ///// cout << "  x1,x2 = " << x1 << " " << x2 <<
  /////         "  y1,y2 = " << y1 << " " << y2 <<
  /////         "  button,action,modifier = " << button << " " <<
  /////         action << " " << modifier << " " << endl;

  SeisPlot *sp = _winman->currentSP();
  if(sp->memoryTraces() == 0) return;
  if(button != 1 && button != 2) return;
  Boolean doit = verifyAction(button, modifier, action);
  float xdata[2], ydata[2];
  getEndPoints(x1, x2, y1, y2, xdata, ydata);
  switch(action)
     {
     case press:
         pickPress  (xdata, ydata, doit);
         break;
     case motion:
         pickMotion (xdata, ydata);
         break;
     case release:
         pickRelease(xdata, ydata, doit, button, modifier);
         break;
     default:
         assert(FALSE);
     }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
