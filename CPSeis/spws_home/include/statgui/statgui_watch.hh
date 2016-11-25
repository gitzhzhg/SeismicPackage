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

//--------------------- statgui_watch.hh ----------------------------//
//--------------------- statgui_watch.hh ----------------------------//
//--------------------- statgui_watch.hh ----------------------------//

//               header file for the StatguiWatch class
//                derived from the StaticInform class
//                  derived from the SLDelay class
//                       subdirectory statgui


//   This class puts the watch cursor onto all shells when
//   it gets a message that slow operations are beginning, and
//   removes the watch cursor when it gets a message that slow
//   operations are ending, or when returning to the event loop.

//   This class also rings the bell when requested.

//   This class also writes the name of the last static file read
//   or saved into the title bar of _app.

//   This class also writes any received messages into the message
//   area of _app, and blanks them out when returning to event loop.

//   This class also tells StaticManager to let all StaticInform
//   objects know when we are returning to the event loop.

//   Only one of these objects should exist for any given
//   StaticManager object and its associated StaticInform objects.
//   This normally means that only one of these objects should exist
//   in an application.


#ifndef _STATGUI_WATCH_HH_
#define _STATGUI_WATCH_HH_

#include "stat/static_inform.hh"
#include "sl/sl_delay.hh"
#include <X11/Intrinsic.h>


class StatguiWatch : public StaticInform, public SLDelay
{

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:    // also public manager() in StaticInform.

  class SLApp         *_app;
  class ShellWatch    *_watch1;
  class PickWatch     *_watch2;

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:    // constructor and destructor.

           StatguiWatch (StaticManager *manager, SLApp *app);
  virtual ~StatguiWatch ();

protected:  // these override StaticInform.

  virtual void beginSlowOperations ();
  virtual void endSlowOperations   ();
  virtual void ringBell            ();
  virtual void showMessage         (const char *msg);
  virtual void sendMessage         (const char *code, const char *msg,
                                    int, int, int, int, int);

protected:  // these override SLDelay.

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

