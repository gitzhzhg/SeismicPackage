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
#include <X11/cursorfont.h>
#include "wproc.h"
#include "sl/sl_shell_container.hh"
#include "sl/shell_watch.hh"


int ShellWatch::_total_watches= 0;

ShellWatch::ShellWatch(Widget w) :_w(w), _multi(False), _num_cursors(1)
{
   assert(w);
   _cur_list[0].watch_cur= XCreateFontCursor( XtDisplay(w), XC_watch);
   _cur_list[0].dpy=  XtDisplay(w);
   if (XtWindow(_w)) {
        define_cursor(w, _cur_list[0].watch_cur);
   }
}


ShellWatch::ShellWatch() : _w(NULL), _multi(True)
{
  void *x;
  Display *dpy;
  int i;
  
  for(dpy= SLShellContainer::topDpy(&x), i=0; (dpy); 
                    dpy= SLShellContainer::nextDpy(&x), i++) {
       _cur_list[i].watch_cur= XCreateFontCursor( dpy, XC_watch);
       _cur_list[i].dpy= dpy;
       SLShellContainer::setClocks(dpy,_cur_list[i].watch_cur);
       XFlush(dpy);
  } // end loop
  _num_cursors=i;
  _total_watches++;

}

ShellWatch::~ShellWatch()
{ 
   if (_multi) {
      if (--_total_watches == 0) {
         for(int i=0; (i<_num_cursors); i++) {
              SLShellContainer::resetClocks(_cur_list[i].dpy);
              XFreeCursor( _cur_list[i].dpy, _cur_list[i].watch_cur);
              XFlush(_cur_list[i].dpy);
         } // end loop
      } // end if (--_total_watches == 0)
   } // end if _multi
   else {
       if (XtWindow(_w)) {
          define_cursor(_w, None);
       }
       XFreeCursor( _cur_list[0].dpy, _cur_list[0].watch_cur);
       XFlush(_cur_list[0].dpy);
   } // end else
}
