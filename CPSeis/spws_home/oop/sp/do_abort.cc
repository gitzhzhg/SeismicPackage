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
#include "sp/do_abort.hh"
#include <stdio.h>
#include <X11/cursorfont.h>
#include <Xm/Text.h>
#include "wproc.h"
#include "cprim.h"

static String  defres[]= {
    ".background:      green",
    NULL };


DoAbort::DoAbort(Widget w, char *msg) : 
             _user_abort(False), _box_window(0)
{
  short len;

  _dpy= XtDisplay(w);
  _parent_shell= get_shell_widget(w);
  setDefaultResources( _dpy, "abort_box", defres);

  _kill_cursor= XCreateFontCursor(_dpy, XC_pirate); 
  _box= XtVaCreateWidget( "abort_box", xmTextWidgetClass, get_shell_child(w), 
                                      XmNshadowThickness,  0, NULL);
  setTopWidget(_box);

  _abort_msg= newstr(msg);
  len= strlen( _abort_msg);
  XtVaSetValues(topWidget(), XmNcolumns, len, NULL);
  XSync(_dpy,False);

  XtAddEventHandler(_box, ButtonPress, False, dummy, NULL);
}



DoAbort::~DoAbort()
{
  free(_abort_msg);
  XFreeCursor( _dpy, _kill_cursor);
}

void DoAbort::setNewAction()
{
  Position x,y;
  Position newx, newy;
  Dimension width, height, popw, poph;
  _user_abort= False;

  XtVaGetValues( _parent_shell, XmNx, &x, XmNy, &y,
                                XmNwidth, &width, XmNheight, &height, NULL);
  XtVaGetValues( _box, XmNwidth, &popw, XmNheight, &poph, NULL);

  newx= width/2 - popw/2;
  newy= height/2 - poph/2;


  XtManageChild(_box);
  XSync(_dpy,False);
  _box_window= XtWindow(_box);
  XRaiseWindow(_dpy, _box_window);
  XMoveWindow(_dpy, _box_window, newx, newy);
  show_msg(_box,_abort_msg);
  XSync(_dpy,False);
  XmUpdateDisplay(_box);
  wprocCursorSet(_box, _kill_cursor);
}


void DoAbort::actionComplete()
{
   if (XtIsManaged(_box)) XtUnmanageChild(_box);
   _user_abort= False;
   _box_window= 0;
}


Boolean DoAbort::altUserAbort(void *data)
{
  DoAbort *obj= (DoAbort*)data;
  Boolean retval = (obj->userAbort());
  return retval;
}


Boolean DoAbort::userAbort()
{
  if (!_user_abort && _box_window) {
      if( XEventsQueued(_dpy,QueuedAlready) ) {
           XEvent ev;
           if (XCheckWindowEvent(_dpy, _box_window, ButtonPress, &ev )) {
              puts("DoAbort::userAbort found an event");
              _user_abort= True;
           }
      }
  }
  return (_user_abort);
}

void DoAbort::dummy(Widget, XtPointer, XEvent*, Boolean*) {}
