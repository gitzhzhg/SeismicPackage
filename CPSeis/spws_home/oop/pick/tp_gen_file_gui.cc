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

//---------------------- tp_gen_file_gui.cc ------------------------//
//---------------------- tp_gen_file_gui.cc ------------------------//
//---------------------- tp_gen_file_gui.cc ------------------------//

//         implementation file for the TpGenFileGui class
//               derived from the SLSmartForm class
//                        subdirectory pick

                       // use also for CC3D

#include "pick/tp_gen_file_gui.hh"
#include "pick/tp_statfile_pair.hh"
#include "pick/tp_resources.hh"
#include "sl/sl_row.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "cprim.h"
#include "stdio.h"
#include "string.h"


//-------------------------- static traps -----------------------//
//-------------------------- static traps -----------------------//
//-------------------------- static traps -----------------------//

#define TRAP(long2, nhx_trap, setNhx)                 \
static void nhx_trap(void *data, long /*ident*/,          \
                        long2 /*oldvar*/, long2 newvar)   \
{                                                     \
  TpStatfilePair *pair = (TpStatfilePair*)data;       \
  pair->setNhx(newvar);                               \
}

TRAP(char*, stattype_trap, setStattype)
TRAP(long ,      nhx_trap, setNhx     )
TRAP(long ,      nhy_trap, setNhy     )
TRAP(float,       x1_trap, setX1      )
TRAP(float,       y1_trap, setY1      )
TRAP(float,     xinc_trap, setXinc    )
TRAP(float,     yinc_trap, setYinc    )
TRAP(long ,       nx_trap, setNx      )
TRAP(long ,       ny_trap, setNy      )
TRAP(float,     xend_trap, setXend    )
TRAP(float,     yend_trap, setYend    )



//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//

static char *stattype_update(void *data)
{                                  
  TpStatfilePair *pair = (TpStatfilePair*)data;
  return (char*)pair->getStattype();                      
}

#define UPDATE(long2, nhx_update, getNhx)               \
static long2 nhx_update(void *data)                     \
{                                                       \
  TpStatfilePair *pair = (TpStatfilePair*)data;         \
  return pair->getNhx();                                \
}

/////////UPDATE(const char*, stattype_update, getStattype)
UPDATE(long ,  nhx_update, getNhx )
UPDATE(long ,  nhy_update, getNhy )
UPDATE(float,   x1_update, getX1  )
UPDATE(float,   y1_update, getY1  )
UPDATE(float, xinc_update, getXinc)
UPDATE(float, yinc_update, getYinc)
UPDATE(long ,   nx_update, getNx  )
UPDATE(long ,   ny_update, getNy  )
UPDATE(float, xend_update, getXend)
UPDATE(float, yend_update, getYend)


static long sense_update(void *data)
{
  TpStatfilePair *pair = (TpStatfilePair*)data;
  return pair->allowHeaderInput();
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//

static String defres[]= {
    "*fontList: 8x13bold",
    NULL };


TpGenFileGui::TpGenFileGui(SLDelay *slparent, TpStatfilePair *pair,
                        const char * const filetype)
       : SLSmartForm(slparent, "tp_gen_file_gui", NULL, TRUE)
{
  assert(slparent && pair);

  setDefaultResources(TpResources::getDisplay(), "tp_gen_file_gui", defres);

////////  SLpLabel  *label  = new SLpLabel (this, "CPS Static File");
  SLpLabel  *label  = new SLpLabel (this, (char*)filetype);
  SLRow     *row0   = new SLRow    (this, "row0");
  SLRow     *row1   = new SLRow    (this, "row1");
  SLRow     *row2   = new SLRow    (this, "row2");
  SLRow     *row3   = new SLRow    (this, "row3");
  SLRow     *row4   = new SLRow    (this, "row4");
  SLRow     *row5   = new SLRow    (this, "row5");

  SLpLabel *label0 = new SLpLabel (row0, "type:");
  SLpLabel *label1 = new SLpLabel (row1, "header words...");
  SLpLabel *label2 = new SLpLabel (row2, "first bin......");
  SLpLabel *label3 = new SLpLabel (row3, "bin increment..");
  SLpLabel *label4 = new SLpLabel (row4, "number of bins.");
  SLpLabel *label5 = new SLpLabel (row5, "last bin.......");

  SLpText  *t0  = new SLpText  (row0, "text0", 0, SLpText::_CHAR, 8);
  SLpLabel *xy  = new SLpLabel (row0, " X bin   Y bin");

  SLpText  *t1a = new SLpText (row1, "t1a", 0, SLpText::_LONG , 6);
  SLpText  *t1b = new SLpText (row1, "t1b", 0, SLpText::_LONG , 6);
  SLpText  *t2a = new SLpText (row2, "t2a", 0, SLpText::_FLOAT, 6, 3);
  SLpText  *t2b = new SLpText (row2, "t2b", 0, SLpText::_FLOAT, 6, 3);
  SLpText  *t3a = new SLpText (row3, "t3a", 0, SLpText::_FLOAT, 6, 3);
  SLpText  *t3b = new SLpText (row3, "t3b", 0, SLpText::_FLOAT, 6, 3);
  SLpText  *t4a = new SLpText (row4, "t4a", 0, SLpText::_LONG , 6);
  SLpText  *t4b = new SLpText (row4, "t4b", 0, SLpText::_LONG , 6);
  SLpText  *t5a = new SLpText (row5, "t5a", 0, SLpText::_FLOAT, 6, 3);
  SLpText  *t5b = new SLpText (row5, "t5b", 0, SLpText::_FLOAT, 6, 3);

  t0 ->setCtrap      (stattype_trap, pair);
  t1a->setItrap      (     nhx_trap, pair);
  t1b->setItrap      (     nhy_trap, pair);
  t2a->setFtrap      (      x1_trap, pair);
  t2b->setFtrap      (      y1_trap, pair);
  t3a->setFtrap      (    xinc_trap, pair);
  t3b->setFtrap      (    yinc_trap, pair);
  t4a->setItrap      (      nx_trap, pair);
  t4b->setItrap      (      ny_trap, pair);
  t5a->setFtrap      (    xend_trap, pair);
  t5b->setFtrap      (    yend_trap, pair);

  t0 ->setupCvarFun  (stattype_update, pair);
  t1a->setupIvarFun  (     nhx_update, pair);
  t1b->setupIvarFun  (     nhy_update, pair);
  t2a->setupFvarFun  (      x1_update, pair);
  t2b->setupFvarFun  (      y1_update, pair);
  t3a->setupFvarFun  (    xinc_update, pair);
  t3b->setupFvarFun  (    yinc_update, pair);
  t4a->setupIvarFun  (      nx_update, pair);
  t4b->setupIvarFun  (      ny_update, pair);
  t5a->setupFvarFun  (    xend_update, pair);
  t5b->setupFvarFun  (    yend_update, pair);

  t0 ->setupSenseFun (sense_update, pair);
  t1a->setupSenseFun (sense_update, pair);
  t1b->setupSenseFun (sense_update, pair);
  t2a->setupSenseFun (sense_update, pair);
  t2b->setupSenseFun (sense_update, pair);
  t3a->setupSenseFun (sense_update, pair);
  t3b->setupSenseFun (sense_update, pair);
  t4a->setupSenseFun (sense_update, pair);
  t4b->setupSenseFun (sense_update, pair);
  t5a->setupSenseFun (sense_update, pair);
  t5b->setupSenseFun (sense_update, pair);

  attach(label , this , this,   this, NULL, 0, 0, 0, 0);
  attach(row0  , this , this,  label, NULL, 0, 0, 0, 0);
  attach(row1  , this , this,   row0, NULL, 0, 0, 0, 0);
  attach(row2  , this , this,   row1, NULL, 0, 0, 0, 0);
  attach(row3  , this , this,   row2, NULL, 0, 0, 0, 0);
  attach(row4  , this , this,   row3, NULL, 0, 0, 0, 0);
  attach(row5  , this , this,   row4, this, 0, 0, 0, 0);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpGenFileGui::~TpGenFileGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
