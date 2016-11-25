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

//------------------------- stp_pick.hh ---------------------------//
//------------------------- stp_pick.hh ---------------------------//
//------------------------- stp_pick.hh ---------------------------//

//              header file for the StpPick class
//          derived from the SeismicTracePick class
//                     subdirectory pick

                   // used with StpPopupBase

#ifndef _STP_PICK_HH_
#define _STP_PICK_HH_

#include "pick/seismic_trace_pick.hh"


class StpPick : public SeismicTracePick
{
  friend class StpPopupBase;

//----------------------- data --------------------------------//
//----------------------- data --------------------------------//
//----------------------- data --------------------------------//

private:

  class StpPopupBase *_pop;

//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//
//----------------------- functions ----------------------------//

private:    // constructor and destructor

  StpPick(class SeisPlot *sp, StpPopupBase  *pop,
                   class SeisVectLinkedList *vectors,
                   const char * const  picking_mode,
                   const char * const  help_token,
                   const char * const  help_fallback,
                   const char * const  rub_color_name,
                   Cursor              cursor,
                   int                 rub_line_width);
 
  virtual ~StpPick();

private:     // overrides SeismicTracePick

  virtual Boolean verifyAction (int button, Modifier modifier,
                                Action action);

  virtual void actionCompleted (SeisPlot *sp,
                                int button, Modifier modifier,
                                long direction,
                                long first_trace, long last_trace,
                                float first_time, float last_time);

//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//
//---------------------- end of functions ---------------------//

};

#endif

//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
//--------------------------- end ----------------------------//
