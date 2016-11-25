#include "fggui/fg_user_abort_gui.hh"
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
#include "sl/sl_base.hh"
#include "geom/field_geometry.hh"

FgUserAbortGui::FgUserAbortGui (FieldGeometry *fg, Widget w) :
  DoAbort (w),
  FgInform  (fg),
  _aborted  (0)
{
  _fg->registerAbortFun (abortFun, this);
}

FgUserAbortGui::~FgUserAbortGui ()
{
  if (_aborted) {
    _fg->ringBell ();
    _fg->showMessage ("action aborted");
    _fg->ringBell ();
  }
  _fg->registerAbortFun (NULL, NULL);
}


int FgUserAbortGui::abortFun (void *abort_data)
{
  FgUserAbortGui *obj = (FgUserAbortGui*) abort_data;
  if (!obj->_aborted) obj->_aborted = obj->userAbort();
  return obj->_aborted;
}


void FgUserAbortGui::sendMessage (FieldGeometry *, char *msg,
  long, long, long, long, long)
{
  if      (!strcmp(msg,"start abort button")) setNewAction ();
  else if (!strcmp(msg,"stop abort button" )) actionComplete ();
}
