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

//---------------------- tp_mute_pop.cc -----------------------//
//---------------------- tp_mute_pop.cc -----------------------//
//---------------------- tp_mute_pop.cc -----------------------//

//            implementation file for the TpMutePop class
//                 derived from the TpPopupBase class
//                       subdirectory pick


#include "pick/tp_mute_pop.hh"
#include "pick/tp_mute_pair.hh"
#include "pick/tp_mute_reference.hh"
#include "pick/tp_mute_file_gui.hh"
#include "pick/tp_mute_info_gui.hh"
#include "pick/tp_mute_show_gui.hh"
#include "pick/tp_mute_auto_gui.hh"
#include "pick/tp_scan_gui.hh"
#include "oprim/file_base.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_file.hh"
#include "cprim.h"
#include <iostream.h>
#include <string.h>
#include <assert.h>




//------------------- misc constants ------------------------//
//------------------- misc constants ------------------------//
//------------------- misc constants ------------------------//


static const long         NUMVECTORS    = 6;
static const float        ZERO_PICK     = ZNIL;
static const float        MISSING_PICK  = MISSING;
static const Boolean      ALLOW_SNAP    = TRUE;
static const Boolean      ALLOW_AUTO    = TRUE;

static const char * const PICKING_MODE  = "Mode: Mute\nPicking";
static const char * const HELP_TOKEN    = "MUTE_PICKING";
static const char * const HELP_FALLBACK =
/*
  "mouse*MUTE_PICKING: BTN#1: Pick mute; BTN#2: nil gather;\\n\
Shift-BTN#2: Pick zero mute; BTN#3: Popup Menu.";
*/
  "mouse*MUTE_PICKING: BTN#1: Pick mute; BTN#2: Pick zero mute;\\n\
Shift-BTN#2: nil gather; BTN#3: Popup Menu.";

static const char * const FILETYPE      = "CPS 3D mute file";
static const char * const EXTENSION     = "mute";
static const Boolean      REQUIRED1     = FALSE;
static const Boolean      REQUIRED2     = FALSE;





//============================================================================
//============================ Set the auto pick mode ========================
//============================================================================

static void toggle_trap(void *data, long/*ident*/, long /*oldvar*/,long newvar)
{
  TpMutePop *pop = (TpMutePop*)data;
  pop->_auto_mute = newvar;
  XtSetSensitive(pop->_auto->W(), (Boolean)newvar);
  XtSetSensitive(pop->_scan->W(), (Boolean)newvar);
}


static long toggle_update(void *data)
{
  TpMutePop *pop = (TpMutePop*)data;
  return pop->_auto_mute;
}





//------------------------- constructor --------------------//
//------------------------- constructor --------------------//
//------------------------- constructor --------------------//


TpMutePop::TpMutePop(SLDelay *slparent, char *name,
                           HelpCtx hctx, SeisPlot *sp)
     : TpPopupBase(slparent, name, hctx, sp,
           NUMVECTORS, ZERO_PICK, MISSING_PICK, ALLOW_SNAP, ALLOW_AUTO,
           PICKING_MODE, HELP_TOKEN, HELP_FALLBACK)
{

  _mute_mode = 1;

  TpMutePair    *pair = new TpMutePair       (_work1, this, sp,
                            FILETYPE, EXTENSION, REQUIRED1, REQUIRED2);

  FileBase *fileb     = new FileBase ("CPS 3D MUTE FILE",
                                      "mute",
                                      FileBase::USE_FOR_INPUT);

  TpMuteReference *mref = new TpMuteReference(_work1, "mute_ref",
                                            SLpFile::_INPUT,
                                            fileb, "Reference File...",
                                            getHelpCtx(), this);

  TpMuteFileGui *gui0 = new TpMuteFileGui    (_work2, pair);

  _auto_tog           = new SLpToggle(_work2, "tp_mute_auto_tog", 0,
                                     "Use automatic picking");

  _auto               = new TpMuteAutoGui    (_work2, this, pair);

  TpMuteInfoGui *gui3 = new TpMuteInfoGui    (_work2, pair);

  TpMuteShowGui *gui4 = new TpMuteShowGui    (_work3, this, pair);

                _scan = new TpScanGui        (_work3, this);

  _work1->attach(pair,      _work1, _work1, _work1,    NULL,   0, 0,  0,  0);
  _work1->attach(mref,      _work1, _work1, pair,    _work1,   0, 0,  0,  0);
  _work2->attach(gui0,      _work2, _work2, _work2,    NULL,   0, 0,  0,  0);
  _work2->attach(gui3,      _work2, _work2, gui0,      NULL,   0, 0,  0,  0);
  _work2->attach(_auto_tog, _work2, _work2, gui3,      NULL,   0, 0,  0,  0);
  _work2->attach(_auto,     _work2, _work2, _auto_tog, _work2, 0, 0,  0,  0);
  _work3->attach(gui4,      _work3, _work3, _work3,    NULL,   0, 0,  0,  0);
  _work3->attach(_scan,     _work3, _work3, gui4,      NULL,   0, 0, 20,  0);

  _pair = pair;      // for base class
  _ref  = mref;
  pair->setNhx(6);
  pair->setNhy(46);
  pair->setNhz(0);

  _auto_tog->setItrap    (toggle_trap  , this);
  _auto_tog->setupIvarFun(toggle_update, this);

}



//----------------------- destructor -------------------------//
//----------------------- destructor -------------------------//
//----------------------- destructor -------------------------//

TpMutePop::~TpMutePop()
{
}


Widget TpMutePop::make(Widget p)
{
  if(made()) return topWidget();

  TpPopupBase::make(p);

  XtSetSensitive(_auto->W(), False);
  XtSetSensitive(_scan->W(), False);

  return topWidget();
}


//===================================================================
//============ External methods to set sensitivity and state ========
//============ of snap  toggles                              ========
//===================================================================
void TpMutePop::setBreakSensitivity(Boolean set)
{
  _scan->setSensitivity(set);
}

void TpMutePop::setSnapSensitivity(Boolean set)
{
  _scan->setSensitivity(set);
}

void TpMutePop::setBreakState(Boolean set)
{
  _scan->setBreakState(set);
}

void TpMutePop::setSnapState(Boolean set)
{
  _scan->setSnapState(set);
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
