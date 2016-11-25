
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
//---------------------- jd_read_action_gui.cc ------------------------//
//---------------------- jd_read_action_gui.cc ------------------------//
//---------------------- jd_read_action_gui.cc ------------------------//

//         implementation file for the JdReadActionGui class
//               derived from the SLSmartForm class
//                       subdirectory fggui


#include "fggui/jd_read_action_gui.hh"
#include "geom/jd_file.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_radio.hh"
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
  int rflag = !jd->ldReadFlag();
  jd->   ldReadFlagSet(rflag);
  jd->   rpReadFlagSet(rflag);
  jd->   ppReadFlagSet(rflag);
  jd->  zt1ReadFlagSet(rflag);
  jd->  zt2ReadFlagSet(rflag);
  jd->  zt3ReadFlagSet(rflag);
  jd->  zt4ReadFlagSet(rflag);
  jd-> gridReadFlagSet(rflag);
}


static void push2_trap(void *data, long /*ident*/)
{
  JdFile *jd  = (JdFile*)data;
  int cflag = !jd->ldClearFlag();
  jd->   ldClearFlagSet(cflag);
  jd->   rpClearFlagSet(cflag);
  jd->   ppClearFlagSet(cflag);
  jd->  zt1ClearFlagSet(cflag);
  jd->  zt2ClearFlagSet(cflag);
  jd->  zt3ClearFlagSet(cflag);
  jd->  zt4ClearFlagSet(cflag);
}


#define TOGTRAP(tog1_trap, chainReadFlagSet)                \
static void tog1_trap(void *data, long /*ident*/,           \
                           long /*oldvar*/, long newvar)    \
{                                                           \
  JdFile *jd  = (JdFile*)data;                              \
  jd->chainReadFlagSet((int)newvar);                        \
}

TOGTRAP(tog2_trap,    ldReadFlagSet)
TOGTRAP(tog3_trap,    rpReadFlagSet)
TOGTRAP(tog4_trap,    ppReadFlagSet)
TOGTRAP(tog5_trap,   zt1ReadFlagSet)
TOGTRAP(tog6_trap,   zt2ReadFlagSet)
TOGTRAP(tog7_trap,   zt3ReadFlagSet)
TOGTRAP(tog8_trap,   zt4ReadFlagSet)
TOGTRAP(tog9_trap,  gridReadFlagSet)


            // newvar = 1 is append   (clear flag FALSE).
            // newvar = 2 is replace  (clear flag TRUE).

#define RADTRAP(rad2_trap, ldClearFlagSet)                  \
static void rad2_trap(void *data, long /*ident*/,           \
                           long /*oldvar*/, long newvar)    \
{                                                           \
  JdFile *jd  = (JdFile*)data;                              \
       if(newvar == 1) jd->ldClearFlagSet(FALSE);           \
  else if(newvar == 2) jd->ldClearFlagSet(TRUE);            \
}

RADTRAP(rad2_trap,    ldClearFlagSet)
RADTRAP(rad3_trap,    rpClearFlagSet)
RADTRAP(rad4_trap,    ppClearFlagSet)
RADTRAP(rad5_trap,   zt1ClearFlagSet)
RADTRAP(rad6_trap,   zt2ClearFlagSet)
RADTRAP(rad7_trap,   zt3ClearFlagSet)
RADTRAP(rad8_trap,   zt4ClearFlagSet)



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


#define TOGUPFUN(tog1_upfun, chainReadFlag)          \
static long tog1_upfun(void *data)                   \
{                                                    \
  JdFile *jd  = (JdFile*)data;                       \
  return jd->chainReadFlag();                        \
}

TOGUPFUN(tog2_upfun,    ldReadFlag)
TOGUPFUN(tog3_upfun,    rpReadFlag)
TOGUPFUN(tog4_upfun,    ppReadFlag)
TOGUPFUN(tog5_upfun,   zt1ReadFlag)
TOGUPFUN(tog6_upfun,   zt2ReadFlag)
TOGUPFUN(tog7_upfun,   zt3ReadFlag)
TOGUPFUN(tog8_upfun,   zt4ReadFlag)
TOGUPFUN(tog9_upfun,  gridReadFlag)


            // newvar = 1 is append   (clear flag FALSE).
            // newvar = 2 is replace  (clear flag TRUE).

#define RADUPFUN(rad2_upfun, ldClearFlag)           \
static long rad2_upfun(void *data)                  \
{                                                   \
  JdFile *jd  = (JdFile*)data;                      \
  if(jd->ldClearFlag()) return 2;                   \
  return 1;                                         \
}

RADUPFUN(rad2_upfun,    ldClearFlag)
RADUPFUN(rad3_upfun,    rpClearFlag)
RADUPFUN(rad4_upfun,    ppClearFlag)
RADUPFUN(rad5_upfun,   zt1ClearFlag)
RADUPFUN(rad6_upfun,   zt2ClearFlag)
RADUPFUN(rad7_upfun,   zt3ClearFlag)
RADUPFUN(rad8_upfun,   zt4ClearFlag)



//------------------ sense update functions ----------------------//
//------------------ sense update functions ----------------------//
//------------------ sense update functions ----------------------//


static long simple_sensefun(void *data)     
{                                   
  JdFile *jd  = (JdFile*)data;
  return (jd->inputStatus() == FileBase::INPUT_VALID);
}



static long repsel_sensefun(void *data)     
{                                   
  JdFile *jd  = (JdFile*)data;
  return (jd->inputStatus() == FileBase::INPUT_VALID &&
          !jd->replaceSelectedMatchingLinesOrFlags() &&
          !jd->replaceAnyMatchingLinesOrFlags());
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


JdReadActionGui::JdReadActionGui(SLDelay *slparent, char *name, JdFile *jd)
       : SLSmartForm(slparent, name, NULL, TRUE)
{
  assert(jd);

  SLpPush  *push1 = new SLpPush   (this, "push1", 0, "read all/none");
  SLpPush  *push2 = new SLpPush   (this, "push2", 0, "append/replace");
  SLDelay  *sep1  = new SLSep     (this, "sep");
  SLDelay  *sep2  = new SLSep     (this, "sep");
  SLDelay  *sep3  = new SLSep     (this, "sep");
  SLDelay  *sep4  = new SLSep     (this, "sep");

  SLpToggle *tog1 = new SLpToggle (this, "tog", 0,
                                        "read chaining (with LD cards)");
  SLpToggle *tog2 = new SLpToggle (this, "tog", 0, "read LD cards");
  SLpToggle *tog3 = new SLpToggle (this, "tog", 0, "read RP cards");
  SLpToggle *tog4 = new SLpToggle (this, "tog", 0, "read PP cards");
  SLpToggle *tog5 = new SLpToggle (this, "tog", 0, "read ZT1 cards");
  SLpToggle *tog6 = new SLpToggle (this, "tog", 0, "read ZT2 cards");
  SLpToggle *tog7 = new SLpToggle (this, "tog", 0, "read ZT3 cards");
  SLpToggle *tog8 = new SLpToggle (this, "tog", 0, "read ZT4 cards");
  SLpToggle *tog9 = new SLpToggle (this, "tog", 0, "read grid transform");

  SLpRadio  *app2 = new SLpRadio  (this, "app", 1, "append");
  SLpRadio  *app3 = new SLpRadio  (this, "app", 1, "append");
  SLpRadio  *app4 = new SLpRadio  (this, "app", 1, "append");
  SLpRadio  *app5 = new SLpRadio  (this, "app", 1, "append");
  SLpRadio  *app6 = new SLpRadio  (this, "app", 1, "append");
  SLpRadio  *app7 = new SLpRadio  (this, "app", 1, "append");
  SLpRadio  *app8 = new SLpRadio  (this, "app", 1, "append");

  SLpRadio  *rep2 = new SLpRadio  (this, "rep", 2, "replace");
  SLpRadio  *rep3 = new SLpRadio  (this, "rep", 2, "replace");
  SLpRadio  *rep4 = new SLpRadio  (this, "rep", 2, "replace");
  SLpRadio  *rep5 = new SLpRadio  (this, "rep", 2, "replace");
  SLpRadio  *rep6 = new SLpRadio  (this, "rep", 2, "replace");
  SLpRadio  *rep7 = new SLpRadio  (this, "rep", 2, "replace");
  SLpRadio  *rep8 = new SLpRadio  (this, "rep", 2, "replace");

  push1->setAtrap(push1_trap, jd);
  push2->setAtrap(push2_trap, jd);

  tog1->setItrap(tog2_trap, jd);   // same as tog2.
  tog2->setItrap(tog2_trap, jd);
  tog3->setItrap(tog3_trap, jd);
  tog4->setItrap(tog4_trap, jd);
  tog5->setItrap(tog5_trap, jd);
  tog6->setItrap(tog6_trap, jd);
  tog7->setItrap(tog7_trap, jd);
  tog8->setItrap(tog8_trap, jd);
  tog9->setItrap(tog9_trap, jd);

  tog1->setupIvarFun(tog2_upfun, jd);   // same as tog2.
  tog2->setupIvarFun(tog2_upfun, jd);
  tog3->setupIvarFun(tog3_upfun, jd);
  tog4->setupIvarFun(tog4_upfun, jd);
  tog5->setupIvarFun(tog5_upfun, jd);
  tog6->setupIvarFun(tog6_upfun, jd);
  tog7->setupIvarFun(tog7_upfun, jd);
  tog8->setupIvarFun(tog8_upfun, jd);
  tog9->setupIvarFun(tog9_upfun, jd);

  push1->setupSenseFun(simple_sensefun, jd);
  push2->setupSenseFun(simple_sensefun, jd);

  tog1->setupSenseFun(simple_sensefun, jd);
  tog2->setupSenseFun(simple_sensefun, jd);
  tog3->setupSenseFun(simple_sensefun, jd);
  tog4->setupSenseFun(simple_sensefun, jd);
  tog5->setupSenseFun(simple_sensefun, jd);
  tog6->setupSenseFun(simple_sensefun, jd);
  tog7->setupSenseFun(simple_sensefun, jd);
  tog8->setupSenseFun(simple_sensefun, jd);
  tog9->setupSenseFun(simple_sensefun, jd);

  app2->setItrap(rad2_trap, jd);
  app3->setItrap(rad3_trap, jd);
  app4->setItrap(rad4_trap, jd);
  app5->setItrap(rad5_trap, jd);
  app6->setItrap(rad6_trap, jd);
  app7->setItrap(rad7_trap, jd);
  app8->setItrap(rad8_trap, jd);

  app2->setupIvarFun(rad2_upfun, jd);
  app3->setupIvarFun(rad3_upfun, jd);
  app4->setupIvarFun(rad4_upfun, jd);
  app5->setupIvarFun(rad5_upfun, jd);
  app6->setupIvarFun(rad6_upfun, jd);
  app7->setupIvarFun(rad7_upfun, jd);
  app8->setupIvarFun(rad8_upfun, jd);

  app2->setupSenseFun(repsel_sensefun, jd);
  app3->setupSenseFun(simple_sensefun, jd);
  app4->setupSenseFun(simple_sensefun, jd);
  app5->setupSenseFun(simple_sensefun, jd);
  app6->setupSenseFun(simple_sensefun, jd);
  app7->setupSenseFun(simple_sensefun, jd);
  app8->setupSenseFun(simple_sensefun, jd);

  rep2->setItrap(rad2_trap, jd);
  rep3->setItrap(rad3_trap, jd);
  rep4->setItrap(rad4_trap, jd);
  rep5->setItrap(rad5_trap, jd);
  rep6->setItrap(rad6_trap, jd);
  rep7->setItrap(rad7_trap, jd);
  rep8->setItrap(rad8_trap, jd);

  rep2->setupIvarFun(rad2_upfun, jd);
  rep3->setupIvarFun(rad3_upfun, jd);
  rep4->setupIvarFun(rad4_upfun, jd);
  rep5->setupIvarFun(rad5_upfun, jd);
  rep6->setupIvarFun(rad6_upfun, jd);
  rep7->setupIvarFun(rad7_upfun, jd);
  rep8->setupIvarFun(rad8_upfun, jd);

  rep2->setupSenseFun(repsel_sensefun, jd);
  rep3->setupSenseFun(simple_sensefun, jd);
  rep4->setupSenseFun(simple_sensefun, jd);
  rep5->setupSenseFun(simple_sensefun, jd);
  rep6->setupSenseFun(simple_sensefun, jd);
  rep7->setupSenseFun(simple_sensefun, jd);
  rep8->setupSenseFun(simple_sensefun, jd);

//                  LEFT   RIGHT   TOP      BOTTOM
  attach(push1    , this , NULL  , this    , NULL, 20, 0, 4);
  attach(sep1     , this , this  , push1   , NULL,  0, 0, 4);
  attach(tog1     , this , NULL  , sep1    , NULL);
  attach(sep2     , this , this  , tog1    , NULL);
  attach(tog2     , this , app2  , sep2    , NULL);
  attach(tog3     , this , app3  , tog2    , NULL);
  attach(tog4     , this , app4  , tog3    , NULL);
  attach(sep3     , this , this  , tog4    , NULL);
  attach(tog5     , this , app5  , sep3    , NULL);
  attach(tog6     , this , app6  , tog5    , NULL);
  attach(tog7     , this , app7  , tog6    , NULL);
  attach(tog8     , this , app8  , tog7    , NULL);
  attach(sep4     , this , this  , tog8    , NULL);
  attach(tog9     , this , NULL  , sep4    , this);

  attach(push2    , NULL , this  , this    , NULL,  0, 20, 4);
  attach(app2     , NULL , rep2  , sep2    , NULL);
  attach(app3     , NULL , rep2  , tog2    , NULL);
  attach(app4     , NULL , rep2  , tog3    , NULL);
  attach(app5     , NULL , rep2  , sep3    , NULL);
  attach(app6     , NULL , rep2  , tog5    , NULL);
  attach(app7     , NULL , rep2  , tog6    , NULL);
  attach(app8     , NULL , rep2  , tog7    , NULL);

  attach(rep2     , NULL , this  , sep2    , NULL);
  attach(rep3     , NULL , this  , tog2    , NULL);
  attach(rep4     , NULL , this  , tog3    , NULL);
  attach(rep5     , NULL , this  , sep3    , NULL);
  attach(rep6     , NULL , this  , tog5    , NULL);
  attach(rep7     , NULL , this  , tog6    , NULL);
  attach(rep8     , NULL , this  , tog7    , NULL);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


JdReadActionGui::~JdReadActionGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

