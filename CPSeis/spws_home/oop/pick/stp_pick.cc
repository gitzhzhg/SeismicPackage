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

//-------------------------- stp_pick.cc ---------------------------//
//-------------------------- stp_pick.cc ---------------------------//
//-------------------------- stp_pick.cc ---------------------------//

//          implementation file for the StpPick class
//           derived from the SeismicTracePick class
//                      subdirectory pick


//        This class controls picking on traces in a SeisPlot
//        display.  It is constructed by StpPopupBase when
//        picking is to start, and is destroyed when picking
//        is finished.  The rubberband vector is added to the
//        SeisVectLinkedList specified to the constructor.


#include "pick/stp_pick.hh"
#include "pick/stp_popup_base.hh"
#include "cprim.h"
#include <string.h>
#include <iostream.h>
#include <assert.h>


//------------------- constructor and destructor -----------------//
//------------------- constructor and destructor -----------------//
//------------------- constructor and destructor -----------------//


StpPick::StpPick(class SeisPlot *sp, StpPopupBase  *pop,
                   class SeisVectLinkedList *vectors,
                   const char * const  picking_mode,
                   const char * const  help_token,
                   const char * const  help_fallback,
                   const char * const  rub_color_name,
                   Cursor              cursor,
                   int                 rub_line_width)
        : SeismicTracePick(sp, vectors,
                           picking_mode, help_token, help_fallback,
                           rub_color_name, cursor, rub_line_width),
                     _pop         (pop)
{
  assert(_pop);
  //// cout << "am in StpPick constructor" << endl;
}


StpPick::~StpPick()
{
  //// cout << "am in StpPick destructor" << endl;
}


//---------------------- verify action --------------------//
//---------------------- verify action --------------------//
//---------------------- verify action --------------------//

Boolean StpPick::verifyAction(int button, Modifier modifier,
                                Action action)
{
  return _pop->verifyPickingAction(button, modifier, action);
}



//--------------------- action completed --------------------//
//--------------------- action completed --------------------//
//--------------------- action completed --------------------//

void StpPick::actionCompleted(SeisPlot *sp,
                             int button, Modifier modifier,
                             long direction,
                             long first_trace, long last_trace,
                             float first_time, float last_time)
{
  _pop->holdAllVectors();
  _pop->pickingActionCompleted(sp, button, modifier, direction,
           first_trace, last_trace, first_time, last_time);
  _pop->setChangedToTrue();
  _pop->flushAllVectors();
  _pop->update();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
