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

//---------------------- tp_gen_pop.cc -----------------------//
//---------------------- tp_gen_pop.cc -----------------------//
//---------------------- tp_gen_pop.cc -----------------------//

//            implementation file for the TpGenPop class
//                 derived from the TpPopupBase class
//                       subdirectory pick


#include "pick/tp_gen_pop.hh"
#include "pick/tp_encoding_desire.hh"
#include "pick/tp_statfile_pair.hh"
#include "pick/tp_gen_info_gui.hh"
#include "pick/tp_gen_auto_gui.hh"
#include "pick/tp_gen_file_gui.hh"
#include "pick/tp_gen_show_gui.hh"
#include "pick/tp_scan_gui.hh"
#include "pick/tp_flatten_gui.hh"
#include "cprim.h"
#include <iostream.h>
#include <string.h>
#include <assert.h>


//------------------- misc constants ------------------------//
//------------------- misc constants ------------------------//
//------------------- misc constants ------------------------//


static const long         NUMVECTORS    = 5;
static const float        ZERO_PICK     = ZNIL;
static const float        MISSING_PICK  = MISSING;
static const Boolean      ALLOW_SNAP    = TRUE;
static const Boolean      ALLOW_AUTO    = TRUE;

static const char * const PICKING_MODE  = "Mode: General\nTrace Picking";
static const char * const HELP_TOKEN    = "GENERAL_PICKING";
static const char * const HELP_FALLBACK =
  "mouse*GENERAL_PICKING: BTN#1: Auto; BTN#2: Nil; BTN#3: Popup Menu;\\n\
Shift-BTN#1: Manual; Shift-BTN#2: Zero; Ctrl-BTN#1: Snap.";

static const char * const FILETYPE      = "CPS static file";
static const char * const EXTENSION     = "pick";
static const Boolean      REQUIRED1     = FALSE;
static const Boolean      REQUIRED2     = FALSE;

static const char * const PROGRAM       = "CBYT General Trace Picking";
static const char * const DEFAULT_TYPE  = "PICK";



//------------------------- constructor --------------------//
//------------------------- constructor --------------------//
//------------------------- constructor --------------------//


TpGenPop::TpGenPop(SLDelay *slparent, char *name,
                           HelpCtx hctx, SeisPlot *sp)
     : TpPopupBase(slparent, name, hctx, sp,
           NUMVECTORS, ZERO_PICK, MISSING_PICK, ALLOW_SNAP, ALLOW_AUTO,
           PICKING_MODE, HELP_TOKEN, HELP_FALLBACK)
{
  TpStatfilePair   *pair = new TpStatfilePair     (_work1, this, sp,
                            FILETYPE, EXTENSION, REQUIRED1, REQUIRED2,
                            PROGRAM, DEFAULT_TYPE);
  TpGenFileGui       *gui0 = new TpGenFileGui     (_work2, pair, FILETYPE);
  TpGenAutoGui       *gui1 = new TpGenAutoGui     (_work2, this, pair);
  TpGenInfoGui       *gui3 = new TpGenInfoGui     (_work3, pair);
  TpGenShowGui       *gui4 = new TpGenShowGui     (_work3, this, pair);
  TpFlattenGui       *gui5 = new TpFlattenGui     (_work3, this);
                     _scan = new TpScanGui        (_work3, this);
  TpEncodingDesire *desire = new TpEncodingDesire (_work1, pair);

  _work1->attach(pair,   _work1, _work1, _work1,   NULL,  0, 0,  0,  0);
  _work1->attach(desire, _work1, _work1,   pair, _work1, 30,30, 10, 10);
  _work2->attach(gui0,   _work2, _work2, _work2,   NULL,  0, 0,  0,  0);
  _work2->attach(gui1,   _work2, _work2,   gui0, _work2,  0, 0, 20,  0);
  _work3->attach(gui3,   _work3, _work3, _work3,   NULL,  0, 0,  0,  0);
  _work3->attach(gui4,   _work3, _work3,   gui3,   NULL,  0, 0,  0,  0);
  _work3->attach(gui5,   _work3, _work3,   gui4,   NULL,  0, 0, 20,  0);
  _work3->attach(_scan,  _work3, _work3,   gui5, _work3,  0, 0, 20,  0);

  _pair = pair;      // for base class
}



//----------------------- destructor -------------------------//
//----------------------- destructor -------------------------//
//----------------------- destructor -------------------------//

TpGenPop::~TpGenPop()
{
}

//===================================================================
//============ External methods to set sensitivity and state ========
//============ of the toggles                                ========
//===================================================================
void TpGenPop::setBreakSensitivity(Boolean set)
{
  _scan->setSensitivity(set);
}

void TpGenPop::setSnapSensitivity(Boolean set)
{
  _scan->setSensitivity(set);
}

void TpGenPop::setBreakState(Boolean set)
{
  _scan->setBreakState(set);
}

void TpGenPop::setSnapState(Boolean set)
{
  _scan->setSnapState(set);
}


















//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
