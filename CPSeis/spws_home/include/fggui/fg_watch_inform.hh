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

//------------------------ fg_watch_inform.hh ----------------------------//
//------------------------ fg_watch_inform.hh ----------------------------//
//------------------------ fg_watch_inform.hh ----------------------------//

//             header file for the FgWatchInform class
//                derived from the FgInform class
//                derived from the SLDelay class
//                       subdirectory fggui


//   This class puts the watch cursor onto all shells when
//   it gets a message from FieldGeometry that a slow calculation
//   is starting, and removes the watch cursor when the slow
//   calculation is finished.

//   This class also rings the bell if so informed.

//   This class also tells FieldGeometry to let all FgInform
//   objects know when we are returning to the event loop.

//   This class also writes the name of the last file read or saved
//   into the title bar of _app if not NULL.

//   Only one of these objects is needed in an application.


#ifndef _FG_WATCH_INFORM_HH_
#define _FG_WATCH_INFORM_HH_

#include "geom/fg_inform.hh"
#include "sl/sl_delay.hh"
#include <X11/Intrinsic.h>


class FgWatchInform : public FgInform, public SLDelay
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:    // also protected _fg in FgInform.

  Widget               _w;
  class SLApp         *_app;
  class ShellWatch    *_watch1;
  class PickWatch     *_watch2;

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:    // constructor and destructor.

  FgWatchInform(FieldGeometry *fg, Widget w, SLApp *app = NULL);
  virtual ~FgWatchInform();

protected:  // these override FgInform.

  virtual void ringBell           (FieldGeometry *fg);
  virtual void preSlowOperations  (FieldGeometry *fg);
  virtual void sendMessage        (FieldGeometry *fg, char *msg,
                       long i1, long i2, long i3, long i4, long i5);

protected:  // these overrides SLDelay.

  virtual void        update();
  virtual WidgetClass topClass()      { return NULL; }
  virtual Boolean     isWidgetBased() { return FALSE; }

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//

