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

//---------------------- tp_ref_pop.cc -----------------------//
//---------------------- tp_ref_pop.cc -----------------------//
//---------------------- tp_ref_pop.cc -----------------------//

//            implementation file for the TpRefPop class
//                 derived from the TpPopupBase class
//                       subdirectory pick


#include "pick/tp_ref_pop.hh"
#include "pick/tp_ref_pair.hh"
#include "pick/tp_ref_ground_gui.hh"
#include "pick/tp_ref_info_gui.hh"
#include "pick/tp_ref_auto_gui.hh"
#include "pick/tp_ref_show_gui.hh"
#include "pick/tp_scan_gui.hh"
#include "pick/tp_flatten_gui.hh"
#include "sl/slp_toggle.hh"
#include "sl/sl2_text.hh"
#include "cprim.h"
#include <iostream.h>
#include <string.h>
#include <assert.h>


//------------------- misc constants ------------------------//
//------------------- misc constants ------------------------//
//------------------- misc constants ------------------------//

static const long    NUMVECTORS   = 5;
static const float   ZERO_PICK    = SCRS_ZERO;
static const float   MISSING_PICK = SCRS_MISSING;
static const Boolean ALLOW_SNAP   = TRUE;
static const Boolean ALLOW_AUTO   = TRUE;

static const char * const PICKING_MODE  = "Mode: Refraction\nStatics Picking";
static const char * const HELP_TOKEN    = "SCRS_PICKING";
static const char * const HELP_FALLBACK =
  "mouse*SCRS_PICKING:  BTN#1: Auto;  BTN#2: Delete;  BTN#3: Popup Menu;\\n\
Shift-BTN#1: Manual;  Ctrl-BTN#1: Snap.";


//------------------------- static functions ---------------------//
//------------------------- static functions ---------------------//
//------------------------- static functions ---------------------//

static void toggle_trap(void *data, long/*ident*/, long /*oldvar*/,long newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->updateOverlaysAfterPicking((char)newvar);
}


static long toggle_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getUpdateOverlaysAfterPicking();
}



static void pickhead_trap(void *data, long/*ident*/, long /*oldvar*/,long newvar)
{
  TpRefPop *pop = (TpRefPop*)data;
  TpRefPair *pair = pop->getTpRefPair();
  pair->setPickhead((int)newvar);
}


static long pickhead_update(void *data)
{
  TpRefPop *pop = (TpRefPop*)data;
  TpRefPair *pair = pop->getTpRefPair();
  return (long)pair->getPickhead();
}



//------------------------- constructor --------------------//
//------------------------- constructor --------------------//
//------------------------- constructor --------------------//


TpRefPop::TpRefPop(SLDelay *slparent, char *name,
                           HelpCtx hctx, SeisPlot *sp)
     : TpPopupBase(slparent, name, hctx, sp,
           NUMVECTORS, ZERO_PICK, MISSING_PICK, ALLOW_SNAP, ALLOW_AUTO,
           PICKING_MODE, HELP_TOKEN, HELP_FALLBACK)
{
  TpRefPair        *pair = new TpRefPair      (_work1, this, sp);
  TpRefGroundGui   *gui0 = new TpRefGroundGui (_work2, pair);
  TpRefInfoGui     *gui1 = new TpRefInfoGui   (_work2, pair);
  TpRefAutoGui     *gui2 = new TpRefAutoGui   (_work2, this);
  TpRefShowGui     *gui4 = new TpRefShowGui   (_work3, this, pair);
  TpFlattenGui     *gui5 = new TpFlattenGui   (_work3, this);
                   _scan = new TpScanGui      (_work3, this);

  SLpToggle        *gui6 = new SLpToggle      (_work4,
                      "tp_ref_pop_toggle", 0,
                      "update overlays after each pick");

  SL2Text          *text = new SL2Text        (_work1, "pickhead", 0,
          "Fortran header word containing picks (ms) for creating pickfile:",
                                               SLpText::_LONG, 3);

  _work1->attach(pair, _work1, _work1, _work1,   NULL,  0,  0,  0,  0);
  _work1->attach(text, _work1, _work1,   pair, _work1,  0,  0,  0,  0);
  _work2->attach(gui0, _work2, _work2, _work2,   NULL,  0,  0,  0,  0);
  _work2->attach(gui1, _work2, _work2,   gui0,   gui2,  0,  0,  0,  0);
  _work2->attach(gui2, _work2, _work2,   NULL, _work2,  0,  0,  0,  0);
  _work3->attach(gui4, _work3, _work3, _work3,   NULL,  0,  0,  0,  0);
  _work3->attach(gui5, _work3, _work3,   gui4,   NULL,  0,  0, 20,  0);
  _work3->attach(_scan,_work2, _work2,   gui5, _work3,  0,  0, 20,  0);

  _work4->showEvenSpacing();
  gui6->setItrap    (toggle_trap  , this);
  gui6->setupIvarFun(toggle_update, this);

  text->setItrap    (pickhead_trap  , this);
  text->setupIvarFun(pickhead_update, this);

  _pair = pair;         // for base class
}



//----------------------- destructor -------------------------//
//----------------------- destructor -------------------------//
//----------------------- destructor -------------------------//

TpRefPop::~TpRefPop()
{
}


//===================================================================
//============ External methods to set sensitivity and state ========
//============ of the toggles                                ========
//===================================================================
void TpRefPop::setBreakSensitivity(Boolean set)
{
  _scan->setSensitivity(set);
}

void TpRefPop::setSnapSensitivity(Boolean set)
{
  _scan->setSensitivity(set);
}

void TpRefPop::setBreakState(Boolean set)
{
  _scan->setBreakState(set);
}

void TpRefPop::setSnapState(Boolean set)
{
  _scan->setSnapState(set);
}

//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
