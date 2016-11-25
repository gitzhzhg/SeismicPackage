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
#ifndef DO_ABORT_WITH_STATUS_HH
#define DO_ABORT_WITH_STATUS_HH

#include "sp/do_abort.hh"

class SLStatusScale;

class DoAbortWithStatus : public DoAbort {

public:
  DoAbortWithStatus				// constructor
    (Widget w,					//   parent widget
     HelpCtx hctx = 0,				//   context sensitive help
     char *msg = "Click here to Abort");	//   message for button

  static Boolean altUserAbortAndStatus		// user abort and status fcn
    (void *obj,					//   given instance of class
     float percent_done);			//   given pct done of task

  void setNewAction				// set a new action
    (char *msg);				//   status message

  void actionComplete ();			// complete an action

protected:
  SLStatusScale
    *_status_scale;				// status scale object

  Widget
    _widget;					// parent widget

  HelpCtx
    _hctx;					// context sensitive help

};

#endif
