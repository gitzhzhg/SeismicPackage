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

//---------------------- statgui_nils2.cc ------------------------//
//---------------------- statgui_nils2.cc ------------------------//
//---------------------- statgui_nils2.cc ------------------------//

//         implementation file for the StatguiNils2 class
//               derived from the SLSmartForm class
//                      subdirectory statgui

#include "statgui/statgui_nils2.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/sl_sep.hh"
#include "sl/radio_list.hh"
#include "sl/slp_radio.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include <stdio.h>
#include <string.h>


enum { CONST ,  //  Replace Nils with Specified Value.
       RANGE ,  //  Replace Range of Values With Nils.
       TERP  ,  //  Replace Nils with Interpolated Values.
       XDIR  ,  //  Interpolate Nils in X dir (no extrapolation).
       YDIR  ,  //  Interpolate Nils in Y dir (no extrapolation).
       NEARBY   //  Interpolate Nils only from Nearby Values.
     };


//----------------------- set values ---------------------------//
//----------------------- set values ---------------------------//
//----------------------- set values ---------------------------//


void StatguiNils2::setReplaceConstant  (float value)
{
  _rconstant = value;
}


void StatguiNils2::setRange1           (float value)
{
  _range1 = value;
}


void StatguiNils2::setRange2           (float value)
{
  _range2 = value;
}


void StatguiNils2::setIxdist          (int   value)
{
  _ixdist = value;
}


void StatguiNils2::setIydist          (int   value)
{
  _iydist = value;
}


void StatguiNils2::setRequire         (int  value)
{
  _require = value;
}



//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//


#define V_TRAP(trim_trap, setReplaceConstant, float2)    \
static void trim_trap(void *data, long /*ident*/,        \
                 float2 /*oldvar*/, float2 newvar)       \
{                                                        \
  StatguiNils2 *gui = (StatguiNils2*)data;               \
  gui->setReplaceConstant(newvar);                       \
}

V_TRAP(rconstant_trap, setReplaceConstant  , float)
V_TRAP(   range1_trap, setRange1           , float)
V_TRAP(   range2_trap, setRange2           , float)
V_TRAP(   ixdist_trap, setIxdist           , long )
V_TRAP(   iydist_trap, setIydist           , long )
V_TRAP(  require_trap, setRequire          , long )



//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//


#define V_UPFUN(trim_upfun, getTrimPercentage, float2)   \
static float2 trim_upfun(void *data)                     \
{                                                        \
  StatguiNils2 *gui = (StatguiNils2*)data;               \
  return gui->getTrimPercentage();                       \
}

V_UPFUN(rconstant_upfun, getReplaceConstant  , float)
V_UPFUN(   range1_upfun, getRange1           , float)
V_UPFUN(   range2_upfun, getRange2           , float)
V_UPFUN(   ixdist_upfun, getIxdist           , long )
V_UPFUN(   iydist_upfun, getIydist           , long )
V_UPFUN(  require_upfun, getRequire          , long )



//----------------- static sense update functions ----------------//
//----------------- static sense update functions ----------------//
//----------------- static sense update functions ----------------//


static long live_sense_upfun(void *data)
{
  StatguiNils2      *gui = (StatguiNils2*)data;
  StaticDataset *dataset = gui->manager()->activeDataset();
  if(dataset->isLocked()) return FALSE;
  if(dataset->numLiveValues() == 0) return FALSE;
  return TRUE;
}


static long nil_sense_upfun(void *data)
{
  StatguiNils2      *gui = (StatguiNils2*)data;
  StaticDataset *dataset = gui->manager()->activeDataset();
  if(dataset->isLocked()) return FALSE;
  if(dataset->numNilValues () == 0) return FALSE;
  return TRUE;
}


static long both_sense_upfun(void *data)
{
  StatguiNils2      *gui = (StatguiNils2*)data;
  StaticDataset *dataset = gui->manager()->activeDataset();
  if(dataset->isLocked()) return FALSE;
  if(dataset->numLiveValues() == 0) return FALSE;
  if(dataset->numNilValues () == 0) return FALSE;
  return TRUE;
}



static long nil_const_sense_upfun(void *data)
{
  StatguiNils2      *gui = (StatguiNils2*)data;
  StaticDataset *dataset = gui->manager()->activeDataset();
  if(dataset->isLocked()) return FALSE;
  if(dataset->numNilValues () == 0) return FALSE;
  if(gui->getChoice() != CONST) return FALSE;
  return TRUE;
}


static long live_range_sense_upfun(void *data)
{
  StatguiNils2      *gui = (StatguiNils2*)data;
  StaticDataset *dataset = gui->manager()->activeDataset();
  if(dataset->isLocked()) return FALSE;
  if(dataset->numLiveValues () == 0) return FALSE;
  if(gui->getChoice() != RANGE) return FALSE;
  return TRUE;
}


static long both_nearby_sense_upfun(void *data)
{
  StatguiNils2      *gui = (StatguiNils2*)data;
  StaticDataset *dataset = gui->manager()->activeDataset();
  if(dataset->isLocked()) return FALSE;
  if(dataset->numNilValues () == 0) return FALSE;
  if(dataset->numLiveValues () == 0) return FALSE;
  if(gui->getChoice() != NEARBY) return FALSE;
  return TRUE;
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        ".borderWidth:        2",
        "*background:         gray80",
        ".sep.separatorType:  SINGLE_LINE",
        ".sep.height:         6",
            NULL };



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


StatguiNils2::StatguiNils2(SLDelay *slparent, StaticManager *manager)
       : SLSmartForm(slparent, "statgui_transform"),
               _manager      (manager),
               _choice       (CONST),
               _rconstant    (0.0),
               _range1       (0.0),
               _range2       (0.0),
               _ixdist       (1),
               _iydist       (1),
               _require      (TRUE)
{
  assert(_manager);
  setFallbackResources(defres);

/////////////////////////// radio buttons:

  RadioList *radio = new RadioList();

  SLpRadio *radio1 = radio->addRadio(this, "r1", CONST);
  SLpRadio *radio2 = radio->addRadio(this, "r2", RANGE);
  SLpRadio *radio3 = radio->addRadio(this, "r3", TERP);
  SLpRadio *radio4 = radio->addRadio(this, "r4", XDIR);
  SLpRadio *radio5 = radio->addRadio(this, "r5", YDIR);
  SLpRadio *radio6 = radio->addRadio(this, "r6", NEARBY);

  radio->setLabel(CONST , "Replace Nils with Specified Value");
  radio->setLabel(RANGE , "Replace Range of Values With Nils");
  radio->setLabel(TERP  , "Replace Nils with Interpolated Values");
  radio->setLabel(XDIR  , "Interpolate Nils in X dir (no extrapolation)");
  radio->setLabel(YDIR  , "Interpolate Nils in Y dir (no extrapolation)");
  radio->setLabel(NEARBY, "Interpolate Nils only from Nearby Values");

  radio->setupIvarPoint(&_choice);  // do not need trap or update function.
  
  radio->setupSenseFun(CONST , nil_sense_upfun , this);
  radio->setupSenseFun(RANGE , live_sense_upfun, this);
  radio->setupSenseFun(TERP  , both_sense_upfun, this);
  radio->setupSenseFun(XDIR  , both_sense_upfun, this);
  radio->setupSenseFun(YDIR  , both_sense_upfun, this);
  radio->setupSenseFun(NEARBY, both_sense_upfun, this);

/////////////////////////// separator widgets:

  SLSep     *sep1 = new SLSep       (this, "sep");
  SLSep     *sep2 = new SLSep       (this, "sep");
  SLSep     *sep3 = new SLSep       (this, "sep");

/////////////////////////// value widgets:

  SL2Text   *t0      = new SL2Text   (this, "t0", 0,
                                            "value to replace nils with:",
                                             SLpText::_FLOAT, 8, 3);
  SL2Text   *t01     = new SL2Text   (this, "t01", 0,
                                    "smallest of range to replace with nil:",
                                             SLpText::_FLOAT, 8, 3);
  SL2Text   *t02     = new SL2Text   (this, "t02", 0,
                                    "largest of range to replace with nil:",
                                             SLpText::_FLOAT, 8, 3);
  SL2Text   *t51     = new SL2Text   (this, "t51", 0,
                                    "search distance (#GP) in X direction:",
                                             SLpText::_LONG, 3, 0);
  SL2Text   *t52     = new SL2Text   (this, "t52", 0,
                                    "search distance (#GP) in Y direction:",
                                             SLpText::_LONG, 3, 0);
  SLpToggle *t53     = new SLpToggle (this, "t53", 0,
                                    "require non-nil values on both sides");

  t0 ->setFtrap  (rconstant_trap, this);
  t01->setFtrap  (   range1_trap, this);
  t02->setFtrap  (   range2_trap, this);
  t51->setItrap  (   ixdist_trap, this);
  t52->setItrap  (   iydist_trap, this);
  t53->setItrap  (  require_trap, this);

  t0 ->setupFvarFun  (rconstant_upfun, this);
  t01->setupFvarFun  (   range1_upfun, this);
  t02->setupFvarFun  (   range2_upfun, this);
  t51->setupIvarFun  (   ixdist_upfun, this);
  t52->setupIvarFun  (   iydist_upfun, this);
  t53->setupIvarFun  (  require_upfun, this);

  t0 ->setupSenseFun  (  nil_const_sense_upfun, this);
  t01->setupSenseFun  ( live_range_sense_upfun, this);
  t02->setupSenseFun  ( live_range_sense_upfun, this);
  t51->setupSenseFun  (both_nearby_sense_upfun, this);
  t52->setupSenseFun  (both_nearby_sense_upfun, this);
  t53->setupSenseFun  (both_nearby_sense_upfun, this);

/////////////////////////// attachments:

         /////       LEFT     RIGHT    TOP      BOTTOM

  attach(radio1    , this  ,  NULL   , this   ,  NULL ,  2, 0,10);
  attach(t0        , NULL  ,  this   , radio1 ,  NULL ,  0, 2, 0);
  attach(sep1      , this  ,  this   , t0     ,  NULL ,  0, 0,10);
  attach(radio2    , this  ,  NULL   , sep1   ,  NULL ,  2, 0,10);
  attach(t01       , NULL  ,  this   , radio2 ,  NULL ,  0, 2, 0);
  attach(t02       , NULL  ,  this   , t01    ,  NULL ,  0, 2, 0);
  attach(sep2      , this  ,  this   , t02    ,  NULL ,  0, 0,10);
  attach(radio3    , this  ,  NULL   , sep2   ,  NULL ,  2, 0,10);
  attach(radio4    , this  ,  NULL   , radio3 ,  NULL ,  2, 0, 0);
  attach(radio5    , this  ,  NULL   , radio4 ,  NULL ,  2, 0, 0);
  attach(sep3      , this  ,  this   , radio5 ,  NULL ,  0, 0,10);
  attach(radio6    , this  ,  NULL   , sep3   ,  NULL ,  2, 0,10);
  attach(t51       , NULL  ,  this   , radio6 ,  NULL ,  0, 2, 0);
  attach(t52       , NULL  ,  this   , t51    ,  NULL ,  0, 2, 0);
  attach(t53       , NULL  ,  this   , t52    ,  this ,  0, 2, 0,8);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


StatguiNils2::~StatguiNils2()
{
}



//------------------------- take action ----------------------------//
//------------------------- take action ----------------------------//
//------------------------- take action ----------------------------//

        // public.

void StatguiNils2::takeAction()
{
  StaticDataset *dataset = manager()->activeDataset();
  switch(_choice)
      {
      case CONST : dataset->replaceNilsWithSpecifiedValue(this, _rconstant);
                   break;
      case RANGE : dataset->replaceRangeOfValuesWithNils
                                                   (this, _range1, _range2);
                   break;
      case TERP  : dataset->replaceNilsWithTerpValues(this);
                   break;
      case XDIR  : dataset->interpNilsInXdirection(this);
                   break;
      case YDIR  : dataset->interpNilsInYdirection(this);
                   break;
      case NEARBY: dataset->interpNilsFromNearby
                                    (this, _ixdist, _iydist, (int)_require);
                   break;
      default: assert(FALSE);
      }
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
