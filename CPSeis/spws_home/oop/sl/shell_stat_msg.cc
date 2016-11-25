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
#include <assert.h>
#include <stdlib.h>
#include "wproc.h"
#include "sl/shell_stat_msg.hh"
#include "sl/sl_shell_container.hh"
#include "sl/shell_watch.hh"


ShellStatMsg::ShellStatMsg(Widget w,char *s, Boolean do_watch) : _sw(NULL)
{
  assert(w);
  assert(s);
  _my_shell= get_toplevel_shell(w);
  _save_str= SLShellContainer::setMsgWidget(_my_shell,s);
  if (do_watch) _sw= new ShellWatch;
  XFlush(XtDisplay(_my_shell));
}

ShellStatMsg::~ShellStatMsg()
{
  // _save_str is freed inside resetMsgWidget
  SLShellContainer::resetMsgWidget(_my_shell,_save_str);
  if (_sw) delete _sw;
  XFlush(XtDisplay(_my_shell));
}


void ShellStatMsg::updateMsg(char *s)
{
  if (_save_str) free(_save_str);
  _save_str= SLShellContainer::setMsgWidget(_my_shell,s);
  XFlush(XtDisplay(_my_shell));
}

void ShellStatMsg::changeCurrentMsg(char *s)
{
  Widget text= SLShellContainer::getMsgArea(_my_shell);
  wprocShowMsg(text,s);
  XFlush(XtDisplay(_my_shell));
}
