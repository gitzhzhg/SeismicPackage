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
#ifndef DOABORT_H
#define DOABORT_H

#include <Xm/Xm.h>
#include "sl/sl_base.hh"


class DoAbort : private SLBase {
  private:
     static void dummy(Widget, XtPointer, XEvent*, Boolean*);
  protected:
     Boolean _user_abort;
     Display *_dpy;
     Widget  _box;
     Widget  _parent_shell;
     Window  _box_window;
     char    *_abort_msg;
     Cursor  _kill_cursor;

  public:
     DoAbort(Widget w, char *msg= "Click here to Abort.");
     ~DoAbort();
     Boolean userAbort();
     static  Boolean altUserAbort(void *obj);
     void    setNewAction();
     void    actionComplete();
     void    setAbort()     {_user_abort= True;}
};

#endif
