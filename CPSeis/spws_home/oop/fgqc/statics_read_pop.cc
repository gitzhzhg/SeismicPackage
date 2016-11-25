
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
//---------------------- statics_read_pop.cc ------------------------------//
//---------------------- statics_read_pop.cc ------------------------------//
//---------------------- statics_read_pop.cc ------------------------------//

//          implementation file for the StaticsReadPop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used to read statics files.


#include "fgqc/statics_read_pop.hh"
#include "fggui/fg_message_gui.hh"
#include "fggui/fg_status_gui.hh"
#include "fgqc/statics_read_gui.hh"
#include "fgqc/statics_file.hh"
#include "sl/sl_file_choice.hh"
#include "sl/slp_file.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_sep.hh"
#include "sl/slp_push.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_toggle.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>

#define notsupported "The given statics file coordinate system is not supported."

//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//

     // these private virtual functions override SLDialog.

Boolean StaticsReadPop::preManageNotify ()
{
  _choice->updateFields ();
  return TRUE;
}

Boolean StaticsReadPop::okNotify ()
{
  _choice->takeAction (this, FP_OK);
  return FALSE;
}

void StaticsReadPop::applyNotify ()
{
  _choice->takeAction (this, FP_APPLY);
}

Boolean StaticsReadPop::cancelNotify ()
{
  callNotifyComplex (FP_CANCEL);
  return TRUE;
}

//-------------------- static update functions ------------------//
//-------------------- static update functions ------------------//
//-------------------- static update functions ------------------//

          // called from OK and Apply pushbuttons.

static long ok_sense_upfun (void *data)
{
  StaticsReadPop *THIS = (StaticsReadPop *)data;
  StaticsFile    *file = THIS->getFile ();
  FileBase::InputStatus status = file->inputStatus ();
  if (status == FileBase::INPUT_VALID) {
    return True;
  }
  else /* if (status != FileBase::INPUT_VALID) */ {
    return False;
  }
}

//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//

StaticsReadPop::StaticsReadPop (SLDelay *slparent, char *name, HelpCtx hctx,
  FieldGeometry *fg, Boolean use_srg):
  SLDialog (slparent, name, hctx, FALSE),
  _fg       (fg),
  _use_srg  (use_srg),
  _file     (0),
  _choice   (0)
{
  constructorHelper ();
}

StaticsReadPop::StaticsReadPop (Widget wparent, char *name, HelpCtx hctx,
  FieldGeometry *fg, Boolean use_srg):
  SLDialog (wparent, name, hctx, FALSE),
  _fg       (fg),
  _use_srg  (use_srg),
  _file     (0),
  _choice   (0)
{
  constructorHelper ();
}

void StaticsReadPop::constructorHelper ()
{
  assert (_fg);
  setTitle ("Read Statics File");
  _file = new StaticsFile (_fg);

/////// populate work area:

  SLSmartForm     *work = workArea();

                _choice = new SLFileChoice  (work, "choice",
                              SLpFile::_INPUT, _file, "Input Statics File...");

  FgMessageGui     *msg  = new FgMessageGui   (work, "msg" , _fg);
  FgStatusGui      *stat = new FgStatusGui    (work, "stat", _fg);
  StaticsReadGui   *act;
  if (_use_srg)
                    act  = new StaticsReadGui (work, "act" , _file);

           //             LEFT     RIGHT    TOP    BOTTOM
  work->attach(_choice  , work   , work ,  work  ,  NULL);
  work->attach(msg      , work   , work , _choice,  NULL);
  if (_use_srg) {
    work->attach(stat   , work   , work ,  msg   ,  NULL);
    work->attach(act    , work   , work ,  stat  ,  work,  0, 10);
  }
  else /* if (!_use_srg) */ {
    work->attach(stat   , work   , work ,  msg   ,  work,  0, 10);
  }

/////// populate bottom area:

  SLpPush *ok    = addBottomOK    ();
  SLpPush *apply = addBottomApply ();
                   addBottomCancel();
                   addBottomHelp  ("STATICS_READ_FILE");

  ok   ->setupSenseFun (ok_sense_upfun, this);
  apply->setupSenseFun (ok_sense_upfun, this);

  update();
}

//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


StaticsReadPop::~StaticsReadPop()
{
  delete _file;
  delete _choice;
}

Widget StaticsReadPop::make (Widget p)
{
  Widget w;
  if (!made()) w = SLDialog::make (p);
  makeChildren();
  return topWidget();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
