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

//---------------------- tp_sisc_pop.cc -----------------------//
//---------------------- tp_sisc_pop.cc -----------------------//
//---------------------- tp_sisc_pop.cc -----------------------//

//            implementation file for the TpSiscPop class
//                 derived from the TpPopupBase class
//                       subdirectory pick


#include "pick/tp_sisc_pop.hh"
#include "pick/tp_encoding_desire.hh"
#include "pick/tp_sisc_pair.hh"
#include "pick/tp_vectors.hh"
#include "pick/tp_fish_file_gui.hh"
#include "pick/tp_sisc_show_gui.hh"
#include "pick/tp_flatten_gui.hh"
#include "cprim.h"
#include <iostream.h>
#include <string.h>
#include <assert.h>


//------------------- misc constants ------------------------//
//------------------- misc constants ------------------------//
//------------------- misc constants ------------------------//


static const long    NUMVECTORS   = 3;
static const float   ZERO_PICK    = ZNIL;
static const float   MISSING_PICK = MISSING;
static const Boolean ALLOW_SNAP   = FALSE;
static const Boolean ALLOW_AUTO   = TRUE;

static const char * const PICKING_MODE  = "Mode: SISC\nCorrelation Picking";
static const char * const HELP_TOKEN    = "SISC_PICKING";
static const char * const HELP_FALLBACK =
  "mouse*SISC_PICKING: BTN#1: Auto;  BTN#2: Nil;  BTN#3: Popup Menu;\\n\
Shift-BTN#1: Manual;  Shift-BTN#2: Zero.";

static const char * const FILETYPE      = "SISC increment file";
static const char * const EXTENSION     = "inc";
static const Boolean      REQUIRED1     = FALSE;
static const Boolean      REQUIRED2     = FALSE;

static const char * const PROGRAM       = "CBYT SISC Correlation Picking";
static const char * const DEFAULT_TYPE  = "RESID";



//------------------------- constructor --------------------//
//------------------------- constructor --------------------//
//------------------------- constructor --------------------//


TpSiscPop::TpSiscPop(SLDelay *slparent, char *name,
                           HelpCtx hctx, SeisPlot *sp)
     : TpPopupBase(slparent, name, hctx, sp,
           NUMVECTORS, ZERO_PICK, MISSING_PICK, ALLOW_SNAP, ALLOW_AUTO,
           PICKING_MODE, HELP_TOKEN, HELP_FALLBACK)
{
  TpSiscPair       *pair   = new TpSiscPair       (_work1, this, sp,
                              FILETYPE, EXTENSION, REQUIRED1, REQUIRED2,
                              PROGRAM, DEFAULT_TYPE);
  TpFishFileGui    *gui0   = new TpFishFileGui    (_work2, pair, FILETYPE);
  TpSiscShowGui    *gui4   = new TpSiscShowGui    (_work3, this);
  TpEncodingDesire *desire = new TpEncodingDesire (_work1, pair);

  _work1->attach(pair,   _work1, _work1, _work1,   NULL,  0, 0,  0,  0);
  _work1->attach(desire, _work1, _work1,   pair, _work1, 30,30, 10, 10);
  _work2->attach(gui0,   _work2, _work2, _work2, _work2,  0, 0,  0,  0);
  _work3->attach(gui4,   _work3, _work3, _work3,   NULL,  0, 0,  0,  0);

  _pair = pair;   // for base class
}



//----------------------- destructor -------------------------//
//----------------------- destructor -------------------------//
//----------------------- destructor -------------------------//

TpSiscPop::~TpSiscPop()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
