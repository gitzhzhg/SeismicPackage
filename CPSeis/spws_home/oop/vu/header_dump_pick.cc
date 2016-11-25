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

//---------------------- header_dump_pick.cc -----------------------//
//---------------------- header_dump_pick.cc -----------------------//
//---------------------- header_dump_pick.cc -----------------------//

//      implementation file for the HeaderDumpPick class
//              derived from the PickBase class
//                      subdirectory pick


#include "vu/header_dump_pick.hh"
#include "sp/seis_plot.hh"
#include "vu/header_dump_vectors.hh"
#include "sl/sl_table_view.hh"
#include "vect/vect_data.hh"
#include "cprim.h"
#include "named_constants.h"
#include <iostream.h>
#include <assert.h>

static       char *       picking_mode  = "Mode: Header Dump\n        Picking";
static const char * const help_token    = "HEADER_DUMP";
static const char * const help_fallback =
         "mouse*HEADER_DUMP: button 1:  select  trace(s) \\n\
button 2: unselect trace(s)";
//static int starting = 1;
                 
/////  wproc function:
/////  if(hctx) ctxhMergeHelpLine(hctx, (char*)help_fallback);
/////  Need to use:   getHelpCtx()   since _hctx is private.

/////  PickBase member function:
/////     setHelpFallback(const char * const help_fallback);

//------------------- constructor and destructor -----------------//
//------------------- constructor and destructor -----------------//
//------------------- constructor and destructor -----------------//

HeaderDumpPick::HeaderDumpPick(SeisPlot *sp,
                               HeaderDumpVectors *headvectors,
                               SLTableView *view)
        : PickBase(sp, picking_mode, help_token, help_fallback),
                     _sp          (sp),
                     _headvectors (headvectors),
                     _view        (view),
                     _rub_vector  (NULL),
                     _rub_data    (NULL)
{
  assert(sp && headvectors && view);
  //// cout << "am in HeaderDumpPick constructor" << endl;
 // if(starting)
 //      {
 //      setHelpFallback(help_fallback);
 //      starting=0;
 //      }
}


HeaderDumpPick::~HeaderDumpPick()
{
  //// cout << "am in HeaderDumpPick destructor" << endl;
  if(_rub_vector) _headvectors->remove(_rub_vector);
  if(_rub_data) delete _rub_data;
}


//------------------------ pick helpers --------------------------//
//------------------------ pick helpers --------------------------//
//------------------------ pick helpers --------------------------//

void HeaderDumpPick::pickPress(float *xdata, float *ydata)
{
  _rub_data = new VectData(2, xdata, ydata);
  _rub_vector = _headvectors->add(_rub_data, "green", 2, True,
            Vector::SolidLine, Vector::FilledSquareMarker, 5);
}


void HeaderDumpPick::pickMotion(float *xdata, float *ydata)
{
  _rub_data->replace(0, 2, xdata, ydata);
}


void HeaderDumpPick::pickRelease(float *xdata, Boolean selected)
{
  _headvectors->remove(_rub_vector);
  delete _rub_data;
  _rub_vector = NULL;
  _rub_data   = NULL;
  long column1 = NearestInteger(xdata[0]);
  long column2 = NearestInteger(xdata[1]);
  Boolean changed = _view->selectTrueColumns
                                  (column1, column2, selected);
  if(changed) _headvectors->updateLocVector();
  _headvectors->updateSelVector();
}



//---------------------- button any -------------------------//
//---------------------- button any -------------------------//
//---------------------- button any -------------------------//

void HeaderDumpPick::buttonAny (int x1, int x2, int y1, int y2,
                 int button, Action action, Modifier modifier)
{
  if(modifier != none) return;
  if(button != 1 && button != 2) return;
  ///// cout << "  x1,x2 = " << x1 << " " << x2 <<
  /////         "  y1,y2 = " << y1 << " " << y2 <<
  /////         "  button,action,modifier = " << button << " " <<
  /////         action << " " << modifier << " " << endl;
  float xdata[2], ydata[2];
  xdata[0] = _sp->xWC(x1);
  xdata[1] = _sp->xWC(x2);
  ydata[0] = _sp->yWC(y1);
  ydata[1] = _sp->yWC(y2);
  long column1 = NearestInteger(xdata[0]);
  long column2 = NearestInteger(xdata[1]);
  Boolean selected;

  if(column1 != column2)
     {
     float m = (ydata[1] - ydata[0]) / (xdata[1] - xdata[0]);
     float b = ydata[1] - m * xdata[1];
     xdata[0] = (float)NearestInteger(xdata[0]);
     xdata[1] = (float)NearestInteger(xdata[1]);
     ydata[0] = m * xdata[0] + b;
     ydata[1] = m * xdata[1] + b;
     }
  else
     {
     xdata[0] = (float)NearestInteger(xdata[0]);
     xdata[1] = (float)NearestInteger(xdata[1]);
     }
  switch(action)
     {
     case press:
         pickPress  (xdata, ydata);
         break;
     case motion:
         pickMotion (xdata, ydata);
         break;
     case release:
         selected = (button == 1);
         pickRelease(xdata, selected);
         break;
     default:
         break;
     }
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
