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

//-------------------------- statgui_edit.cc --------------------------//
//-------------------------- statgui_edit.cc --------------------------//
//-------------------------- statgui_edit.cc --------------------------//

//            implementation file for the StatguiEdit class
//                 derived from the SLSmartForm class
//                       subdirectory statgui


#include "statgui/statgui_edit.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_row.hh"
#include "sl/sl_sep.hh"
#include "sl/slp_push.hh"
#include "sl/slp_option.hh"
#include "sl/slp_toggle.hh"
#include "sl/sl2_text.hh"
#include "named_constants.h"
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


//--------------------------- set values ---------------------------//
//--------------------------- set values ---------------------------//
//--------------------------- set values ---------------------------//


void StatguiEdit::setXmin             (float value)
{
  _xmin = value;
}


void StatguiEdit::setXmax             (float value)
{
  _xmax = value;
}


void StatguiEdit::setYmin             (float value)
{
  _ymin = value;
}


void StatguiEdit::setYmax             (float value)
{
  _ymax = value;
}


void StatguiEdit::setMultiplyConstant (float value)
{
  _mconstant = value;
}


void StatguiEdit::setAddConstant      (float value)
{
  _aconstant = value;
}


void StatguiEdit::setTrimPercentage   (int   value)
{
  _trim = ConstrainValue(value, 0, 100);
}


void StatguiEdit::setXrun             (float value)
{
  _xrun = MaximumValue(value, 0.0);
}


void StatguiEdit::setYrun             (float value)
{
  _yrun = MaximumValue(value, 0.0);
}


void StatguiEdit::setEndFlag          (int   value)
{
  _endflag = value;
}



//-------------------- get active data object ------------------------//
//-------------------- get active data object ------------------------//
//-------------------- get active data object ------------------------//

      // public.

StaticDataset *StatguiEdit::activeDataset()  const
{
  return manager()->activeDataset();
}



//-------------------------- cancel undo ------------------------//
//-------------------------- cancel undo ------------------------//
//-------------------------- cancel undo ------------------------//

      // public.
      // call this function when unmanaging the dialog box.
      // this function deletes all temporary files which might
      //   have been saved by this dialog box.

void StatguiEdit::cancelUndo()
{
  manager()->maybeDeleteUndoFiles(_doer1);
  manager()->maybeDeleteUndoFiles(_doer2);
  manager()->maybeDeleteUndoFiles(_doer3);
  manager()->maybeDeleteUndoFiles(_doer6);
  manager()->maybeDeleteUndoFiles(_doer7);
  manager()->maybeDeleteUndoFiles(_doer8);
}



//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//


#define V_TRAP(trim_trap, setTrimPercentage, float2)     \
static void trim_trap(void *data, long /*ident*/,        \
                 float2 /*oldvar*/, float2 newvar)       \
{                                                        \
  StatguiEdit *gui = (StatguiEdit*)data;                 \
  gui->setTrimPercentage(newvar);                        \
}

V_TRAP(     xmin_trap, setXmin             , float)
V_TRAP(     xmax_trap, setXmax             , float)
V_TRAP(     ymin_trap, setYmin             , float)
V_TRAP(     ymax_trap, setYmax             , float)
V_TRAP(mconstant_trap, setMultiplyConstant , float)
V_TRAP(aconstant_trap, setAddConstant      , float)
V_TRAP(     trim_trap, setTrimPercentage   , long )
V_TRAP(     xrun_trap, setXrun             , float)
V_TRAP(     yrun_trap, setYrun             , float)
V_TRAP(  endflag_trap, setEndFlag          , long )



static void p1_trap(void *data, long /*ident*/)
{
  StatguiEdit   *gui     = (StatguiEdit*)data;
  StaticDataset *dataset = gui->activeDataset();
  void          *doer    = gui->getDoer1();
  dataset->integrateStaticValues(doer);
}


static void p2_trap(void *data, long /*ident*/)
{
  StatguiEdit       *gui = (StatguiEdit*)data;
  StaticDataset *dataset = gui->activeDataset();
  void             *doer = gui->getDoer2();
  int               trim = gui->getTrimPercentage();
  float             xrun = gui->getXrun();
  float             yrun = gui->getYrun();
  int            endflag = gui->getEndFlag();
  dataset->removeRunningAverage(doer, (int)trim, xrun, yrun, (int)endflag);
}


static void p3_trap(void *data, long /*ident*/)
{
  StatguiEdit       *gui = (StatguiEdit*)data;
  StaticDataset *dataset = gui->activeDataset();
  void             *doer = gui->getDoer3();
  int               trim = gui->getTrimPercentage();
  float             xrun = gui->getXrun();
  float             yrun = gui->getYrun();
  int            endflag = gui->getEndFlag();
  dataset->smoothStaticValues(doer, (int)trim, xrun, yrun, (int)endflag);
}


static void p6_trap(void *data, long /*ident*/)
{
  StatguiEdit       *gui = (StatguiEdit*)data;
  StaticDataset *dataset = gui->activeDataset();
  void             *doer = gui->getDoer6();
  float         constant = gui->getMultiplyConstant();
  dataset->multiplyStaticValuesByConstant(doer, constant);
}


static void p7_trap(void *data, long /*ident*/)
{
  StatguiEdit       *gui = (StatguiEdit*)data;
  StaticDataset *dataset = gui->activeDataset();
  void             *doer = gui->getDoer7();
  float         constant = gui->getAddConstant();
  dataset->addConstantToStaticValues(doer, constant);
}


static void p8_trap(void *data, long /*ident*/)
{
  StatguiEdit       *gui = (StatguiEdit*)data;
  StaticDataset *dataset = gui->activeDataset();
  void             *doer = gui->getDoer8();
  float             xmin = gui->getXmin();
  float             xmax = gui->getXmax();
  float             ymin = gui->getYmin();
  float             ymax = gui->getYmax();
  dataset->gradeStaticValues(doer, xmin, xmax, ymin, ymax);
}


static void p9_trap(void *data, long /*ident*/)
{
  StatguiEdit   *gui     = (StatguiEdit*)data;
  StaticDataset *dataset = gui->activeDataset();
  void          *doer    = gui->getDoer9();
  dataset->randomizeStaticValues(doer);
}



#define U_TRAP(u1_trap, getDoer1)                        \
static void u1_trap(void *data, long /*ident*/)          \
{                                                        \
  StatguiEdit       *gui = (StatguiEdit*)data;           \
  StaticDataset *dataset = gui->activeDataset();         \
  void             *doer = gui->getDoer1();              \
  dataset->maybeReadUndoFile(doer);                      \
}

U_TRAP(u1_trap , getDoer1)
U_TRAP(u2_trap , getDoer2)
U_TRAP(u3_trap , getDoer3)
U_TRAP(u6_trap , getDoer6)
U_TRAP(u7_trap , getDoer7)
U_TRAP(u8_trap , getDoer8)
U_TRAP(u9_trap , getDoer9)




//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//


#define V_UPFUN(trim_upfun, getTrimPercentage, float2)   \
static float2 trim_upfun(void *data)                     \
{                                                        \
  StatguiEdit *gui = (StatguiEdit*)data;                 \
  return gui->getTrimPercentage();                       \
}

V_UPFUN(     xmin_upfun, getXmin             , float)
V_UPFUN(     xmax_upfun, getXmax             , float)
V_UPFUN(     ymin_upfun, getYmin             , float)
V_UPFUN(     ymax_upfun, getYmax             , float)
V_UPFUN(mconstant_upfun, getMultiplyConstant , float)
V_UPFUN(aconstant_upfun, getAddConstant      , float)
V_UPFUN(     trim_upfun, getTrimPercentage   , long )
V_UPFUN(     xrun_upfun, getXrun             , float)
V_UPFUN(     yrun_upfun, getYrun             , float)
V_UPFUN(  endflag_upfun, getEndFlag          , long )



//----------------- static sense update functions ----------------//
//----------------- static sense update functions ----------------//
//----------------- static sense update functions ----------------//


static long live_sense_upfun(void *data)
{
  StatguiEdit       *gui = (StatguiEdit*)data;
  StaticDataset *dataset = gui->activeDataset();
  if(dataset->isLocked()) return FALSE;
  if(dataset->numLiveValues() == 0) return FALSE;
  return TRUE;
}


static long random_sense_upfun(void *data)
{
  StatguiEdit       *gui = (StatguiEdit*)data;
  StaticDataset *dataset = gui->activeDataset();
  if(dataset->isLocked()) return FALSE;
  return TRUE;
}



#define U_SENSE_UPFUN(u1_sense_upfun, getDoer1)              \
static long u1_sense_upfun(void *data)                       \
{                                                            \
  StatguiEdit       *gui = (StatguiEdit*)data;               \
  StaticDataset *dataset = gui->activeDataset();             \
  void             *doer = gui->getDoer1();                  \
  return (dataset->allowReadDeleteUndoFile(doer) &&          \
            !dataset->isLocked());                           \
}


U_SENSE_UPFUN(u1_sense_upfun , getDoer1)
U_SENSE_UPFUN(u2_sense_upfun , getDoer2)
U_SENSE_UPFUN(u3_sense_upfun , getDoer3)
U_SENSE_UPFUN(u6_sense_upfun , getDoer6)
U_SENSE_UPFUN(u7_sense_upfun , getDoer7)
U_SENSE_UPFUN(u8_sense_upfun , getDoer8)
U_SENSE_UPFUN(u9_sense_upfun , getDoer9)



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


StatguiEdit::StatguiEdit(SLDelay *slparent, StaticManager *manager)
            : SLSmartForm  (slparent, "statgui_edit"),
                        _manager      (manager),
                        _doer1        (NULL),
                        _doer2        (NULL),
                        _doer3        (NULL),
                        _doer6        (NULL),
                        _doer7        (NULL),
                        _doer8        (NULL),
                        _doer9        (NULL),
                        _xmin         (0.0),
                        _xmax         (0.0),
                        _ymin         (0.0),
                        _ymax         (0.0),
                        _mconstant    (1.0),
                        _aconstant    (0.0),
                        _trim         (0),
                        _xrun         (0.0),
                        _yrun         (0.0),
                        _endflag      (StaticDataset::ENDFLAG_N)
{
  assert(_manager);
  setFallbackResources(defres);

/////////////////////////// separator widgets:

  SLSep     *sep1 = new SLSep       (this, "sep");
  SLSep     *sep2 = new SLSep       (this, "sep");
  SLSep     *sep3 = new SLSep       (this, "sep");
  SLSep     *sep4 = new SLSep       (this, "sep");

/////////////////////////// pushbutton widgets:

  SLpPush *p1 = new SLpPush (this, "Integrate Static Values");
  SLpPush *p2 = new SLpPush (this, "Remove Running Average");
  SLpPush *p3 = new SLpPush (this, "Smooth Static Values");
  SLpPush *p6 = new SLpPush (this, "Multiply Values by a Constant");
  SLpPush *p7 = new SLpPush (this, "Add a Constant to Static Values");
  SLpPush *p8 = new SLpPush (this, "Grade Static Values");
  SLpPush *p9 = new SLpPush (this, "Set Static Values to Random Numbers");

  p1 ->setAtrap       (p1_trap      ,  this);
  p2 ->setAtrap       (p2_trap      ,  this);
  p3 ->setAtrap       (p3_trap      ,  this);
  p6 ->setAtrap       (p6_trap      ,  this);
  p7 ->setAtrap       (p7_trap      ,  this);
  p8 ->setAtrap       (p8_trap      ,  this);
  p9 ->setAtrap       (p9_trap      ,  this);

  p1 ->setupSenseFun  (  live_sense_upfun, this);
  p2 ->setupSenseFun  (  live_sense_upfun, this);
  p3 ->setupSenseFun  (  live_sense_upfun, this);
  p6 ->setupSenseFun  (  live_sense_upfun, this);
  p7 ->setupSenseFun  (  live_sense_upfun, this);
  p8 ->setupSenseFun  (  live_sense_upfun, this);
  p9 ->setupSenseFun  (random_sense_upfun, this);

  _doer1  = p1;
  _doer2  = p2;
  _doer3  = p3;
  _doer6  = p6;
  _doer7  = p7;
  _doer8  = p8;
  _doer9  = p9;

/////////////////////////// undo widgets:

  SLpPush   *u1       = new SLpPush  (this  , "Undo");
  SLpPush   *u2       = new SLpPush  (this  , "Undo");
  SLpPush   *u3       = new SLpPush  (this  , "Undo");
  SLpPush   *u6       = new SLpPush  (this  , "Undo");
  SLpPush   *u7       = new SLpPush  (this  , "Undo");
  SLpPush   *u8       = new SLpPush  (this  , "Undo");
  SLpPush   *u9       = new SLpPush  (this  , "Undo");

  u1 ->setAtrap       (u1_trap      ,  this);
  u2 ->setAtrap       (u2_trap      ,  this);
  u3 ->setAtrap       (u3_trap      ,  this);
  u6 ->setAtrap       (u6_trap      ,  this);
  u7 ->setAtrap       (u7_trap      ,  this);
  u8 ->setAtrap       (u8_trap      ,  this);
  u9 ->setAtrap       (u9_trap      ,  this);

  u1 ->setupSenseFun  (u1_sense_upfun , this);
  u2 ->setupSenseFun  (u2_sense_upfun , this);
  u3 ->setupSenseFun  (u3_sense_upfun , this);
  u6 ->setupSenseFun  (u6_sense_upfun , this);
  u7 ->setupSenseFun  (u7_sense_upfun , this);
  u8 ->setupSenseFun  (u8_sense_upfun , this);
  u9 ->setupSenseFun  (u9_sense_upfun , this);

/////////////////////////// value widgets:

  SL2Text   *t1      = new SL2Text   (this, "t1", 0,
                                            "minimum X ground position:",
                                             SLpText::_FLOAT, 8, 3);
  SL2Text   *t2      = new SL2Text   (this, "t2", 0,
                                            "maximum X ground position:",
                                             SLpText::_FLOAT, 8, 3);
  SL2Text   *t3      = new SL2Text   (this, "t3", 0,
                                            "minimum Y ground position:",
                                             SLpText::_FLOAT, 8, 3);
  SL2Text   *t4      = new SL2Text   (this, "t4", 0,
                                            "maximum Y ground position:",
                                             SLpText::_FLOAT, 8, 3);
  SL2Text   *t5      = new SL2Text   (this, "t5", 0,
                                            "constant to multiply by:",
                                             SLpText::_FLOAT, 8, 3);
  SL2Text   *t6      = new SL2Text   (this, "t6", 0,
                                            "constant to add:",
                                             SLpText::_FLOAT, 8, 3);
  SL2Text   *t7      = new SL2Text   (this, "t7", 0,
                            "trim percentage (0=mean, 100=median):",
                                             SLpText::_LONG, 3, 0);
  SL2Text   *t8      = new SL2Text   (this, "t8", 0,
                                            "X ground position range:",
                                             SLpText::_FLOAT, 8, 3);
  SL2Text   *t9      = new SL2Text   (this, "t9", 0,
                                            "Y ground position range:",
                                             SLpText::_FLOAT, 8, 3);
  SLpOption *t10     = new SLpOption (this, "t10", 0, "end option:");

  t10->addOption("narrowed end range" , StaticDataset::ENDFLAG_N);
  t10->addOption("truncated end range", StaticDataset::ENDFLAG_T);
  t10->addOption("extended end range" , StaticDataset::ENDFLAG_E);
  t10->addOption("shifted end range"  , StaticDataset::ENDFLAG_S);

  t1 ->setFtrap  (     xmin_trap, this);
  t2 ->setFtrap  (     xmax_trap, this);
  t3 ->setFtrap  (     ymin_trap, this);
  t4 ->setFtrap  (     ymax_trap, this);
  t5 ->setFtrap  (mconstant_trap, this);
  t6 ->setFtrap  (aconstant_trap, this);
  t7 ->setItrap  (     trim_trap, this);
  t8 ->setFtrap  (     xrun_trap, this);
  t9 ->setFtrap  (     yrun_trap, this);
  t10->setItrap  (  endflag_trap, this);

  t1 ->setupFvarFun  (     xmin_upfun, this);
  t2 ->setupFvarFun  (     xmax_upfun, this);
  t3 ->setupFvarFun  (     ymin_upfun, this);
  t4 ->setupFvarFun  (     ymax_upfun, this);
  t5 ->setupFvarFun  (mconstant_upfun, this);
  t6 ->setupFvarFun  (aconstant_upfun, this);
  t7 ->setupIvarFun  (     trim_upfun, this);
  t8 ->setupFvarFun  (     xrun_upfun, this);
  t9 ->setupFvarFun  (     yrun_upfun, this);
  t10->setupIvarFun  (  endflag_upfun, this);

  t1 ->setupSenseFun  (live_sense_upfun, this);
  t2 ->setupSenseFun  (live_sense_upfun, this);
  t3 ->setupSenseFun  (live_sense_upfun, this);
  t4 ->setupSenseFun  (live_sense_upfun, this);
  t5 ->setupSenseFun  (live_sense_upfun, this);
  t6 ->setupSenseFun  (live_sense_upfun, this);
  t7 ->setupSenseFun  (live_sense_upfun, this);
  t8 ->setupSenseFun  (live_sense_upfun, this);
  t9 ->setupSenseFun  (live_sense_upfun, this);
  t10->setupSenseFun  (live_sense_upfun, this);

/////////////////////////// attachments:

         /////       LEFT     RIGHT    TOP      BOTTOM

  attach(p1        , this  ,  NULL   , this   ,  NULL ,  2, 0,10);
  attach(u1        , p1    ,  NULL   , this   ,  NULL ,  0, 0,10);
  attach(sep1      , this  ,  this   , p1     ,  NULL ,  0, 0,10);
  attach(p2        , this  ,  NULL   , sep1   ,  NULL ,  2, 0,10);
  attach(u2        , p2    ,  NULL   , sep1   ,  NULL ,  0, 0,10);
  attach(p3        , this  ,  NULL   , p2     ,  NULL ,  2, 0, 0);
  attach(u3        , p3    ,  NULL   , p2     ,  NULL ,  0, 0, 0);
  attach(t7        , NULL  ,  this   , p3     ,  NULL ,  0, 2, 0);
  attach(t8        , NULL  ,  this   , t7     ,  NULL ,  0, 2, 0);
  attach(t9        , NULL  ,  this   , t8     ,  NULL ,  0, 2, 0);
  attach(t10       , NULL  ,  this   , t9     ,  NULL ,  0, 2, 0);
  attach(sep2      , this  ,  this   , t10    ,  NULL ,  0, 0,10);
  attach(p6        , this  ,  NULL   , sep2   ,  NULL ,  2, 0,10);
  attach(u6        , p6    ,  NULL   , sep2   ,  NULL ,  0, 0,10);
  attach(t5        , NULL  ,  this   , p6     ,  NULL ,  0, 2, 0);
  attach(p7        , this  ,  NULL   , t5     ,  NULL ,  2, 0, 0);
  attach(u7        , p7    ,  NULL   , t5     ,  NULL ,  0, 0, 0);
  attach(t6        , NULL  ,  this   , p7     ,  NULL ,  0, 2, 0);
  attach(sep3      , this  ,  this   , t6     ,  NULL ,  0, 0,10);
  attach(p8        , this  ,  NULL   , sep3   ,  NULL ,  2, 0,10);
  attach(u8        , p8    ,  NULL   , sep3   ,  NULL ,  0, 0,10);
  attach(t1        , NULL  ,  this   , p8     ,  NULL ,  0, 2, 0);
  attach(t2        , NULL  ,  this   , t1     ,  NULL ,  0, 2, 0);
  attach(t3        , NULL  ,  this   , t2     ,  NULL ,  0, 2, 0);
  attach(t4        , NULL  ,  this   , t3     ,  NULL ,  0, 2, 0);
  attach(sep4      , this  ,  this   , t4     ,  NULL ,  0, 0,10);
  attach(p9        , this  ,  NULL   , sep4   ,  NULL ,  2, 0,10);
  attach(u9        , p9    ,  NULL   , sep4   ,  this ,  0, 0,10, 8);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


StatguiEdit::~StatguiEdit()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
