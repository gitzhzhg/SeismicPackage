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

//-------------------------- statgui_nils.cc --------------------------//
//-------------------------- statgui_nils.cc --------------------------//
//-------------------------- statgui_nils.cc --------------------------//

//            implementation file for the StatguiNils class
//                 derived from the SLSmartForm class
//                       subdirectory statgui


#include "statgui/statgui_nils.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_row.hh"
#include "sl/sl_sep.hh"
#include "sl/slp_push.hh"
#include "sl/slp_option.hh"
#include "sl/slp_toggle.hh"
#include "sl/sl2_text.hh"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


//--------------------------- set values ---------------------------//
//--------------------------- set values ---------------------------//
//--------------------------- set values ---------------------------//


void StatguiNils::setReplaceConstant  (float value)
{
  _rconstant = value;
}


void StatguiNils::setRange1           (float value)
{
  _range1 = value;
}


void StatguiNils::setRange2           (float value)
{
  _range2 = value;
}


void StatguiNils::setIxdist          (int   value)
{
  _ixdist = value;
}


void StatguiNils::setIydist          (int   value)
{
  _iydist = value;
}


void StatguiNils::setRequire         (int  value)
{
  _require = value;
}



//-------------------- get active data object ------------------------//
//-------------------- get active data object ------------------------//
//-------------------- get active data object ------------------------//

      // public.

StaticDataset *StatguiNils::activeDataset()  const
{
  return manager()->activeDataset();
}



//-------------------------- cancel undo ----------------------------//
//-------------------------- cancel undo ----------------------------//
//-------------------------- cancel undo ----------------------------//

      // public.
      // call this function when unmanaging the dialog box.
      // this function deletes all temporary files which might
      //   have been saved by this dialog box.

void StatguiNils::cancelUndo()
{
  manager()->maybeDeleteUndoFiles(_doer4);
  manager()->maybeDeleteUndoFiles(_doer44);
  manager()->maybeDeleteUndoFiles(_doer5);
  manager()->maybeDeleteUndoFiles(_doer5x);
  manager()->maybeDeleteUndoFiles(_doer5y);
  manager()->maybeDeleteUndoFiles(_doer5n);
}



//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//


#define V_TRAP(trim_trap, setReplaceConstant, float2)    \
static void trim_trap(void *data, long /*ident*/,        \
                 float2 /*oldvar*/, float2 newvar)       \
{                                                        \
  StatguiNils *gui = (StatguiNils*)data;                 \
  gui->setReplaceConstant(newvar);                       \
}

V_TRAP(rconstant_trap, setReplaceConstant  , float)
V_TRAP(   range1_trap, setRange1           , float)
V_TRAP(   range2_trap, setRange2           , float)
V_TRAP(   ixdist_trap, setIxdist           , long )
V_TRAP(   iydist_trap, setIydist           , long )
V_TRAP(  require_trap, setRequire          , long )



#define P_TRAP(p1_trap, getDoer1, replaceNilsWithTerpValues)   \
static void p1_trap(void *data, long /*ident*/)                \
{                                                              \
  StatguiNils   *gui     = (StatguiNils*)data;                 \
  StaticDataset *dataset = gui->activeDataset();               \
  void          *doer    = gui->getDoer1();                    \
  dataset->replaceNilsWithTerpValues(doer);                    \
}

  P_TRAP(p5_trap , getDoer5 , replaceNilsWithTerpValues)
  P_TRAP(p5x_trap, getDoer5x, interpNilsInXdirection)
  P_TRAP(p5y_trap, getDoer5y, interpNilsInYdirection)



static void p4_trap(void *data, long /*ident*/)
{
  StatguiNils       *gui = (StatguiNils*)data;
  StaticDataset *dataset = gui->activeDataset();
  void             *doer = gui->getDoer4();
  float         constant = gui->getReplaceConstant();
  dataset->replaceNilsWithSpecifiedValue(doer, constant);
}


static void p44_trap(void *data, long /*ident*/)
{
  StatguiNils       *gui = (StatguiNils*)data;
  StaticDataset *dataset = gui->activeDataset();
  void             *doer = gui->getDoer44();
  float           range1 = gui->getRange1();
  float           range2 = gui->getRange2();
  dataset->replaceRangeOfValuesWithNils(doer, range1, range2);
}


static void p5n_trap(void *data, long /*ident*/)
{
  StatguiNils       *gui = (StatguiNils*)data;
  StaticDataset *dataset = gui->activeDataset();
  void             *doer = gui->getDoer5n();
  int             ixdist = gui->getIxdist();
  int             iydist = gui->getIydist();
  int            require = gui->getRequire();
  dataset->interpNilsFromNearby(doer, ixdist, iydist, require);
}



#define U_TRAP(u1_trap, getDoer1)                        \
static void u1_trap(void *data, long /*ident*/)          \
{                                                        \
  StatguiNils       *gui = (StatguiNils*)data;           \
  StaticDataset *dataset = gui->activeDataset();         \
  void             *doer = gui->getDoer1();              \
  dataset->maybeReadUndoFile(doer);                      \
}

U_TRAP(u4_trap , getDoer4)
U_TRAP(u44_trap, getDoer44)
U_TRAP(u5_trap , getDoer5)
U_TRAP(u5x_trap, getDoer5x)
U_TRAP(u5y_trap, getDoer5y)
U_TRAP(u5n_trap, getDoer5n)




//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//


#define V_UPFUN(trim_upfun, getTrimPercentage, float2)   \
static float2 trim_upfun(void *data)                     \
{                                                        \
  StatguiNils *gui = (StatguiNils*)data;                 \
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
  StatguiNils       *gui = (StatguiNils*)data;
  StaticDataset *dataset = gui->activeDataset();
  if(dataset->isLocked()) return FALSE;
  if(dataset->numLiveValues() == 0) return FALSE;
  return TRUE;
}


static long nil_sense_upfun(void *data)
{
  StatguiNils       *gui = (StatguiNils*)data;
  StaticDataset *dataset = gui->activeDataset();
  if(dataset->isLocked()) return FALSE;
  if(dataset->numNilValues () == 0) return FALSE;
  return TRUE;
}


static long both_sense_upfun(void *data)
{
  StatguiNils       *gui = (StatguiNils*)data;
  StaticDataset *dataset = gui->activeDataset();
  if(dataset->isLocked()) return FALSE;
  if(dataset->numLiveValues() == 0) return FALSE;
  if(dataset->numNilValues () == 0) return FALSE;
  return TRUE;
}


#define U_SENSE_UPFUN(u1_sense_upfun, getDoer1)              \
static long u1_sense_upfun(void *data)                       \
{                                                            \
  StatguiNils       *gui = (StatguiNils*)data;               \
  StaticDataset *dataset = gui->activeDataset();             \
  void             *doer = gui->getDoer1();                  \
  return (dataset->allowReadDeleteUndoFile(doer) &&          \
            !dataset->isLocked());                           \
}


U_SENSE_UPFUN(u4_sense_upfun , getDoer4)
U_SENSE_UPFUN(u44_sense_upfun, getDoer44)
U_SENSE_UPFUN(u5_sense_upfun , getDoer5)
U_SENSE_UPFUN(u5x_sense_upfun, getDoer5x)
U_SENSE_UPFUN(u5y_sense_upfun, getDoer5y)
U_SENSE_UPFUN(u5n_sense_upfun, getDoer5n)



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        ".borderWidth:        2",
        "*background:         gray80",
        ".sep.separatorType:  SINGLE_LINE",
        ".sep.height:         6",
            NULL };



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


StatguiNils::StatguiNils(SLDelay *slparent, StaticManager *manager)
            : SLSmartForm  (slparent, "statgui_nils"),
                        _manager      (manager),
                        _doer4        (NULL),
                        _doer44       (NULL),
                        _doer5        (NULL),
                        _doer5x       (NULL),
                        _doer5y       (NULL),
                        _doer5n       (NULL),
                        _rconstant    (0.0),
                        _range1       (0.0),
                        _range2       (0.0),
                        _ixdist       (1),
                        _iydist       (1),
                        _require      (TRUE)
{
  assert(_manager);
  setFallbackResources(defres);

/////////////////////////// separator widgets:

  SLSep     *sep1 = new SLSep       (this, "sep");
  SLSep     *sep2 = new SLSep       (this, "sep");
  SLSep     *sep3 = new SLSep       (this, "sep");

/////////////////////////// pushbutton widgets:

  SLpPush *p4 = new SLpPush (this, "Replace Nils with Specified Value");
  SLpPush *p44= new SLpPush (this, "Replace Range of Values With Nils");
  SLpPush *p5 = new SLpPush (this, "Replace Nils with Interpolated Values");
  SLpPush *p5x= new SLpPush (this,
                              "Interpolate Nils in X dir (no extrapolation)");
  SLpPush *p5y= new SLpPush (this,
                              "Interpolate Nils in Y dir (no extrapolation)");
  SLpPush *p5n= new SLpPush (this, "Interpolate Nils only from Nearby Values");

  p4 ->setAtrap       (p4_trap      ,  this);
  p44->setAtrap       (p44_trap     ,  this);
  p5 ->setAtrap       (p5_trap      ,  this);
  p5x->setAtrap       (p5x_trap     ,  this);
  p5y->setAtrap       (p5y_trap     ,  this);
  p5n->setAtrap       (p5n_trap     ,  this);

  p4 ->setupSenseFun  (nil_sense_upfun , this);
  p44->setupSenseFun  (live_sense_upfun, this);
  p5 ->setupSenseFun  (both_sense_upfun, this);
  p5x->setupSenseFun  (both_sense_upfun, this);
  p5y->setupSenseFun  (both_sense_upfun, this);
  p5n->setupSenseFun  (both_sense_upfun, this);

  _doer4  = p4;
  _doer44 = p44;
  _doer5  = p5;
  _doer5x = p5x;
  _doer5y = p5y;
  _doer5n = p5n;

/////////////////////////// undo widgets:

  SLpPush   *u4       = new SLpPush  (this  , "Undo");
  SLpPush   *u44      = new SLpPush  (this  , "Undo");
  SLpPush   *u5       = new SLpPush  (this  , "Undo");
  SLpPush   *u5x      = new SLpPush  (this  , "Undo");
  SLpPush   *u5y      = new SLpPush  (this  , "Undo");
  SLpPush   *u5n      = new SLpPush  (this  , "Undo");

  u4 ->setAtrap       (u4_trap      ,  this);
  u44->setAtrap       (u44_trap     ,  this);
  u5 ->setAtrap       (u5_trap      ,  this);
  u5x->setAtrap       (u5x_trap     ,  this);
  u5y->setAtrap       (u5y_trap     ,  this);
  u5n->setAtrap       (u5n_trap     ,  this);

  u4 ->setupSenseFun  (u4_sense_upfun , this);
  u44->setupSenseFun  (u44_sense_upfun, this);
  u5 ->setupSenseFun  (u5_sense_upfun , this);
  u5x->setupSenseFun  (u5x_sense_upfun, this);
  u5y->setupSenseFun  (u5y_sense_upfun, this);
  u5n->setupSenseFun  (u5n_sense_upfun, this);

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

  t0 ->setupSenseFun  (nil_sense_upfun , this);
  t01->setupSenseFun  (live_sense_upfun, this);
  t02->setupSenseFun  (live_sense_upfun, this);
  t51->setupSenseFun  (both_sense_upfun, this);
  t52->setupSenseFun  (both_sense_upfun, this);
  t53->setupSenseFun  (both_sense_upfun, this);

/////////////////////////// attachments:

         /////       LEFT     RIGHT    TOP      BOTTOM

  attach(p4        , this  ,  NULL   , this   ,  NULL ,  2, 0,10);
  attach(u4        , p4    ,  NULL   , this   ,  NULL ,  0, 0,10);
  attach(t0        , NULL  ,  this   , p4     ,  NULL ,  0, 2, 0);
  attach(sep1      , this  ,  this   , t0     ,  NULL ,  0, 0,10);
  attach(p44       , this  ,  NULL   , sep1   ,  NULL ,  2, 0,10);
  attach(u44       , p44   ,  NULL   , sep1   ,  NULL ,  0, 0,10);
  attach(t01       , NULL  ,  this   , p44    ,  NULL ,  0, 2, 0);
  attach(t02       , NULL  ,  this   , t01    ,  NULL ,  0, 2, 0);
  attach(sep2      , this  ,  this   , t02    ,  NULL ,  0, 0,10);
  attach(p5        , this  ,  NULL   , sep2   ,  NULL ,  2, 0,10);
  attach(u5        , p5    ,  NULL   , sep2   ,  NULL ,  0,20,10);
  attach(p5x       , this  ,  NULL   , p5     ,  NULL ,  2, 0, 0);
  attach(u5x       , p5x   ,  NULL   , p5     ,  NULL ,  0,20, 0);
  attach(p5y       , this  ,  NULL   , p5x    ,  NULL ,  2, 0, 0);
  attach(u5y       , p5y   ,  NULL   , p5x    ,  NULL ,  0,20, 0);
  attach(sep3      , this  ,  this   , p5y    ,  NULL ,  0, 0,10);
  attach(p5n       , this  ,  NULL   , sep3   ,  NULL ,  2, 0,10);
  attach(u5n       , p5n   ,  this   , sep3   ,  NULL ,  0,20,10);
  attach(t51       , NULL  ,  this   , p5n    ,  NULL ,  0, 2, 0);
  attach(t52       , NULL  ,  this   , t51    ,  NULL ,  0, 2, 0);
  attach(t53       , NULL  ,  this   , t52    ,  this ,  0, 2, 0,8);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


StatguiNils::~StatguiNils()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
