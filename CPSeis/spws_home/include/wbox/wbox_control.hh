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

//----------------------- wbox_control.hh -------------------------//
//----------------------- wbox_control.hh -------------------------//
//----------------------- wbox_control.hh -------------------------//

//              header file for the WboxControl class
//                  not derived from any class
//                       subdirectory wbox


#ifndef _WBOX_CONTROL_HH_
#define _WBOX_CONTROL_HH_


#include "wproc.h"


class WboxControl
{

//--------------------------- typedefs ----------------------------//
//--------------------------- typedefs ----------------------------//
//--------------------------- typedefs ----------------------------//

  typedef const char *WboxHardcopy
                 (void *data, int numlines, const char *default_filename);

//------------------------ data -------------------------------//
//------------------------ data -------------------------------//
//------------------------ data -------------------------------//

private:

  class WboxBox      *_box;           // pointer to external class.
  class WboxHelp     *_help;          // internal class.
  class WboxHard     *_hard;          // internal class.
  class WboxFind     *_find;          // internal class.
  class WboxEvents   *_events;        // internal class.
  class WboxText     *_text;          // internal class.
  class WboxRoll     *_roll;          // internal class.
  class WboxInsert   *_insert;        // internal class.
  class WboxMessages *_messages;      // internal class.

//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//
//-------------------- functions --------------------------------//

public:    // constructor and destructor.

  WboxControl (WboxBox *box);

  virtual ~WboxControl();

public:

  class WboxField  *getActiveFieldPointer     ()  const;

  void  setFocus               (int ident, int index);
  void  sendClientMessageEvent (const char *endkey)      const;
  void  sendImmediateEvent     (const char *endkey);
  void  showMessage            (const char *msg);
  void  maybeShowMessage       (const char *msg);
  void  showWindowboxInfo      ();

  void  registerHardcopy       (WboxHardcopy *hardcopy);
  void  saveTable              (const char *filename = NULL)  const;

  void  updateFields     (const char *endkey,
                          int irow, int icol, int irow2, int icol2);

  void  boxtrap          (const char *charr, const char *endkey,
                          int irow, int icol, int irow2, int icol2);

  void  restorePreviousUserValue (int *ident, int *index, int *istat);

  void  completeCanvas (HelpCtx hctx, Widget toplevel,
                        const char *helpfile, const char *helptitle);

private:

  void  updateActiveField (const char *endkey,
                           int irow, int icol, int irow2, int icol2);

  void  callUserTrap  (int ivar, float fvar, double dvar, const char *cvar,
                       int *nread, char *endkey,
                       int *ident, int *index);
  void  callUserTrap  (char *endkey);

  void  savePreviousValue ();
  int   restorePreviousValue
         (int *ivar, float *fvar, double *dvar, char *cvar);

  void  printDebug    (const char *endkey, const char *msg);

//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//
//---------------------- end of functions -----------------------//

} ;

#endif

//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//
//---------------------------- end --------------------------------//


