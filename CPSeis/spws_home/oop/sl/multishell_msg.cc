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
#include "sl/multishell_msg.hh"
#include "sl/sl_shell_container.hh"
#include "sl/shell_watch.hh"
#include "oprim/ll_charptr.hh"


MultiShellMsg::MultiShellMsg(Widget w,char *s, Boolean do_watch) : 
                _sw(NULL), _w(w)
{
  assert(w);
  assert(s);
  _list= new CharPtrLinkedList();
  SLShellContainer::setAllMsgWidgets(_list,s);
  if (do_watch) _sw= new ShellWatch;
  XFlush(XtDisplay(_w));
}

MultiShellMsg::~MultiShellMsg()
{
  SLShellContainer::resetAllMsgWidgets(_list);
  if (_sw) delete _sw;
  delete _list;
  XFlush(XtDisplay(_w));
}


void MultiShellMsg::updateMsg(char *s)
{
  assert(s);
  char *p=NULL,*q=NULL;
  void *x;
  for(p= _list->top(&x); (p); p=q) {
      q= _list->next(&x);
      _list->remove(p);
      if (p) free(p);
  }
  SLShellContainer::setAllMsgWidgets(_list,s);
  XFlush(XtDisplay(_w));
}
