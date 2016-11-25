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

//---------------------- seismic_trace_pick.hh ----------------------//
//---------------------- seismic_trace_pick.hh ----------------------//
//---------------------- seismic_trace_pick.hh ----------------------//

//         header file for the SeismicTracePick class
//              derived from the PickBase class
//                     subdirectory pick


#ifndef _SEISMIC_TRACE_PICK_HH_
#define _SEISMIC_TRACE_PICK_HH_

#include "plot/pick_base.hh"


class SeismicTracePick : public PickBase
{

//----------------------- data --------------------------------//
//----------------------- data --------------------------------//
//----------------------- data --------------------------------//

private:

  class SeisWinMan         *_winman;       // new 9/4/97
/*
  class SeisPlot           *_sp;           // removed 9/4/97
*/
  class SeisVectLinkedList *_vectors;
  const char * const        _color_name;
  int                       _line_width;
  class Vector             *_rub_vector;
  class VectData           *_rub_data;

//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//

public:    // constructor and destructor

  SeismicTracePick(class SeisPlot     *sp,
                   SeisVectLinkedList *vectors,
                   const char * const  picking_mode,
                   const char * const  help_token,
                   const char * const  help_fallback,
                   const char * const  color_name,
                   Cursor              cursor,
                   int                 line_width);
 
  virtual ~SeismicTracePick();

private:     // pick helpers

  void pickPress    (float *xdata, float *ydata, Boolean doit);
  void pickMotion   (float *xdata, float *ydata);
  void pickRelease  (float *xdata, float *ydata, Boolean doit,
                     int button, Modifier modifier);

  void getEndPoints (int x1, int x2, int y1, int y2,
                                float xdata[2], float ydata[2]);

protected:     // virtual functions to override

  virtual Boolean verifyAction (int button, Modifier modifier,
                                Action action);

  virtual void actionCompleted (class SeisPlot*,
                                int /*button*/, Modifier /*modifier*/,
                                long /*direction*/,
                                long /*first_trace*/, long /*last_trace*/,
                                float /*first_time*/, float /*last_time*/) {}

private:     // overrides PickBase

  virtual void buttonAny (int x1, int x2, int y1, int y2,
                 int button, Action action, Modifier modifier);

//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//

};

#endif

//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
