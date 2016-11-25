#include "geom/field_geometry.hh"
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
#include "fgmap/fg_shell_msg.hh"
#include "sl/multishell_msg.hh"




FgShellMsg::FgShellMsg(Widget w, FieldGeometry *fg) : 
                       FgInform(fg), _msg(NULL), _w(w) 
{}


void FgShellMsg::showMessage(FieldGeometry*, char *msg)
{
  if (!_msg) {
         _msg= new MultiShellMsg(_w, msg);
  } // end if
  else {
         _msg->updateMsg(msg);
  }

}


void FgShellMsg::returningToEventLoop (FieldGeometry *)
{
  if (_msg) {
      delete _msg;
      _msg= NULL;
  } // end if 
}


