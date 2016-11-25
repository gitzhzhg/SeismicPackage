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
#include "sp/do_abort_with_status.hh"
#include "sl/sl_status_scale.hh"
#include <assert.h>
#include <stdio.h>

DoAbortWithStatus::DoAbortWithStatus (Widget w, HelpCtx hctx, char *msg) :
  DoAbort (w, msg),
  _widget        (w),
  _hctx          (hctx),
  _status_scale  (0)
{
}

void DoAbortWithStatus::setNewAction (char *msg)
{
  assert (!_status_scale);
  
  _status_scale = new SLStatusScale (_widget, "status_scale_pop",
                                      _hctx, msg);
  _status_scale->make ();

  XtVaSetValues(_status_scale->getScale()->topWidget(), 
                XmNdecimalPoints, 2, NULL);

  _status_scale->manage ();

  DoAbort::setNewAction ();
  
}

// percent_done is assumed a fraction from 0.0 to 1.0
Boolean DoAbortWithStatus::altUserAbortAndStatus (void *obj,
  float percent_done)
{
  DoAbortWithStatus *object = (DoAbortWithStatus *)obj;

// update the status and message
  if (object->_status_scale) {
    object->_status_scale->setPercent (percent_done * 100);
  }

  Boolean retval = (object->userAbort());
  return retval;
}

void DoAbortWithStatus::actionComplete ()
{
  DoAbort::actionComplete ();
  _status_scale->unmanage ();
  delete _status_scale, _status_scale = 0;
}
