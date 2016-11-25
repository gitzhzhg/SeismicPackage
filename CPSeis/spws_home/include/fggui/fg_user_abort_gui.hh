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

//---------------------- fg_user_abort_gui.hh ---------------------------//
//---------------------- fg_user_abort_gui.hh ---------------------------//
//---------------------- fg_user_abort_gui.hh ---------------------------//

//              header file for the FgUserAbortGui class
//                 derived from the DoAbort class
//                       subdirectory fggui

     // this class is designed to allow FieldGeometry to display
     // an abort button, and to react to a button press on it,
     // without knowing about X-windows or this class.
     //
     // this class can also be used by any other class which knows
     // about FieldGeometry but does not know about X-windows.

     // to use this class:
     //   (1) create this object (outside FieldGeometry, by an object
     //         which knows about X-windows) before starting an operation
     //         which might have time-consuming portions.
     //   (2) call fg->startAbortOption()
     //         from within or outside FieldGeometry at any time
     //         to display an abort button.
     //   (3) call fg->stopAbortOption()
     //         from within or outside FieldGeometry at any time
     //         to remove the abort button.
     //   (4) call fg->aborted()
     //         from within or outside FieldGeometry at any time
     //         to learn whether the abort button has been pressed.
     //   (5) delete this object after the time-consuming operation
     //         is either finished or aborted.

     // SLBase *obj is the dialog box, or any SL object within
     // the dialog box, where the abort button will be placed.
     // This SL object must be made when the constructor is called.

     // if the abort button is turned on and off more than once,
     // the abort flag will remain true if an earlier invocation
     // of the abort button was pressed.


#ifndef _FG_USER_ABORT_GUI_HH_
#define _FG_USER_ABORT_GUI_HH_

#include "sp/do_abort.hh"
#include "geom/fg_inform.hh"
#include <assert.h>


class FgUserAbortGui : public DoAbort, public FgInform
{

//----------------------- data ------------------------------//
//----------------------- data ------------------------------//
//----------------------- data ------------------------------//

private:           // also protected _fg in FgInform

  int _aborted;

//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//
//--------------------- functions ---------------------------//

public:

  FgUserAbortGui (class FieldGeometry *fg, Widget w);

  virtual ~FgUserAbortGui ();

  static int abortFun (void *abort_data);

  virtual void sendMessage (class FieldGeometry *, char *msg,
                            long, long, long, long, long);

//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//
//----------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
//---------------------------- end -------------------------------//
