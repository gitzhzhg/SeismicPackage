
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
//---------------------- jd_save_action_gui.cc ------------------------//
//---------------------- jd_save_action_gui.cc ------------------------//
//---------------------- jd_save_action_gui.cc ------------------------//

//         implementation file for the JdSaveActionGui class
//               derived from the SLSmartForm class
//                       subdirectory fggui


#include "fggui/jd_save_action_gui.hh"
#include "geom/jd_file.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_push.hh"
#include "sl/sl_sep.hh"
#include "cprim.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//

static void push1_trap(void *data, long /*ident*/)
{
  JdFile *jd  = (JdFile*)data;
  int rflag = !jd->ldSaveFlag();
  jd->   ldSaveFlagSet(rflag);
  jd->   rpSaveFlagSet(rflag);
  jd->   ppSaveFlagSet(rflag);
  jd->  zt1SaveFlagSet(rflag);
  jd->  zt2SaveFlagSet(rflag);
  jd->  zt3SaveFlagSet(rflag);
  jd->  zt4SaveFlagSet(rflag);
  jd-> gridSaveFlagSet(rflag);
}


#define TOGTRAP(tog1_trap, chainSaveFlagSet)                \
static void tog1_trap(void *data, long /*ident*/,           \
                           long /*oldvar*/, long newvar)    \
{                                                           \
  JdFile *jd  = (JdFile*)data;                              \
  jd->chainSaveFlagSet((int)newvar);                        \
}

TOGTRAP(tog2_trap,    ldSaveFlagSet)
TOGTRAP(tog3_trap,    rpSaveFlagSet)
TOGTRAP(tog4_trap,    ppSaveFlagSet)
TOGTRAP(tog5_trap,   zt1SaveFlagSet)
TOGTRAP(tog6_trap,   zt2SaveFlagSet)
TOGTRAP(tog7_trap,   zt3SaveFlagSet)
TOGTRAP(tog8_trap,   zt4SaveFlagSet)
TOGTRAP(tog9_trap,  gridSaveFlagSet)



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


static long tog1_upfun(void * /*data*/)
{
  return TRUE;
}


#define TOGUPFUN(tog1_upfun, chainSaveFlag)          \
static long tog1_upfun(void *data)                   \
{                                                    \
  JdFile *jd  = (JdFile*)data;                       \
  return jd->chainSaveFlag();                        \
}

TOGUPFUN(tog2_upfun,    ldSaveFlag)
TOGUPFUN(tog3_upfun,    rpSaveFlag)
TOGUPFUN(tog4_upfun,    ppSaveFlag)
TOGUPFUN(tog5_upfun,   zt1SaveFlag)
TOGUPFUN(tog6_upfun,   zt2SaveFlag)
TOGUPFUN(tog7_upfun,   zt3SaveFlag)
TOGUPFUN(tog8_upfun,   zt4SaveFlag)
TOGUPFUN(tog9_upfun,  gridSaveFlag)



//------------------ sense update functions ----------------------//
//------------------ sense update functions ----------------------//
//------------------ sense update functions ----------------------//


static long simple_sensefun(void *data)     
{                                   
  JdFile *jd  = (JdFile*)data;
  return (jd->outputStatus() != FileBase::OUTPUT_INVALID);
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


JdSaveActionGui::JdSaveActionGui(SLDelay *slparent, char *name, JdFile *jd)
       : SLSmartForm(slparent, name, NULL, TRUE)
{
  assert(jd);

  SLpPush  *push1 = new SLpPush   (this, "push1", 0, "save all/none");
  SLDelay  *sep1  = new SLSep     (this, "sep");
  SLDelay  *sep2  = new SLSep     (this, "sep");
  SLDelay  *sep3  = new SLSep     (this, "sep");
  SLDelay  *sep4  = new SLSep     (this, "sep");

  SLpToggle *tog1 = new SLpToggle (this, "tog", 0, "save chaining");
  SLpToggle *tog2 = new SLpToggle (this, "tog", 0, "save LD cards");
  SLpToggle *tog3 = new SLpToggle (this, "tog", 0, "save RP cards");
  SLpToggle *tog4 = new SLpToggle (this, "tog", 0, "save PP cards");
  SLpToggle *tog5 = new SLpToggle (this, "tog", 0, "save ZT1 cards");
  SLpToggle *tog6 = new SLpToggle (this, "tog", 0, "save ZT2 cards");
  SLpToggle *tog7 = new SLpToggle (this, "tog", 0, "save ZT3 cards");
  SLpToggle *tog8 = new SLpToggle (this, "tog", 0, "save ZT4 cards");
  SLpToggle *tog9 = new SLpToggle (this, "tog", 0, "save grid transform");

  push1->setAtrap(push1_trap, this);

//tog1->setItrap(tog1_trap, jd);   // do not need a trap since always TRUE.
  tog2->setItrap(tog2_trap, jd);
  tog3->setItrap(tog3_trap, jd);
  tog4->setItrap(tog4_trap, jd);
  tog5->setItrap(tog5_trap, jd);
  tog6->setItrap(tog6_trap, jd);
  tog7->setItrap(tog7_trap, jd);
  tog8->setItrap(tog8_trap, jd);
  tog9->setItrap(tog9_trap, jd);

  tog1->setupIvarFun(tog1_upfun, jd);     // always TRUE.
  tog2->setupIvarFun(tog2_upfun, jd);
  tog3->setupIvarFun(tog3_upfun, jd);
  tog4->setupIvarFun(tog4_upfun, jd);
  tog5->setupIvarFun(tog5_upfun, jd);
  tog6->setupIvarFun(tog6_upfun, jd);
  tog7->setupIvarFun(tog7_upfun, jd);
  tog8->setupIvarFun(tog8_upfun, jd);
  tog9->setupIvarFun(tog9_upfun, jd);

  push1->setupSenseFun(simple_sensefun, jd);

  tog1->setupSenseFun(simple_sensefun, jd);
  tog2->setupSenseFun(simple_sensefun, jd);
  tog3->setupSenseFun(simple_sensefun, jd);
  tog4->setupSenseFun(simple_sensefun, jd);
  tog5->setupSenseFun(simple_sensefun, jd);
  tog6->setupSenseFun(simple_sensefun, jd);
  tog7->setupSenseFun(simple_sensefun, jd);
  tog8->setupSenseFun(simple_sensefun, jd);
  tog9->setupSenseFun(simple_sensefun, jd);

//                  LEFT   RIGHT   TOP      BOTTOM
  attach(push1    , this , NULL  , this    , NULL, 20, 0, 4);
  attach(sep1     , this , this  , push1   , NULL,  0, 0, 4);
  attach(tog1     , this , NULL  , sep1    , NULL);
  attach(sep2     , this , this  , tog1    , NULL);
  attach(tog2     , this , NULL  , sep2    , NULL);
  attach(tog3     , this , NULL  , tog2    , NULL);
  attach(tog4     , this , NULL  , tog3    , NULL);
  attach(sep3     , this , this  , tog4    , NULL);
  attach(tog5     , this , NULL  , sep3    , NULL);
  attach(tog6     , this , NULL  , tog5    , NULL);
  attach(tog7     , this , NULL  , tog6    , NULL);
  attach(tog8     , this , NULL  , tog7    , NULL);
  attach(sep4     , this , this  , tog8    , NULL);
  attach(tog9     , this , this  , sep4    , this);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


JdSaveActionGui::~JdSaveActionGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

