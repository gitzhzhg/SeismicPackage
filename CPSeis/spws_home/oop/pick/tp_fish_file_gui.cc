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

//---------------------- tp_fish_file_gui.cc ------------------------//
//---------------------- tp_fish_file_gui.cc ------------------------//
//---------------------- tp_fish_file_gui.cc ------------------------//

//         implementation file for the TpFishFileGui class
//               derived from the SLSmartForm class
//                        subdirectory pick

                       // also used by SISC

#include "pick/tp_fish_file_gui.hh"
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

#define TRAP(long2, nhx_trap, setNhx)                     \
static void nhx_trap(void *data, long /*ident*/,          \
                        long2 /*oldvar*/, long2 newvar)   \
{                                                         \
  TpStatfilePair *pair = (TpStatfilePair*)data;           \
  pair->setNhx(newvar);                                   \
}

TRAP(char*, stattype_trap, setStattype)
TRAP(long ,      nhx_trap, setNhx     )
TRAP(long ,     nhx2_trap, setNhx2    )
TRAP(float,       x1_trap, setX1      )
TRAP(float,     xinc_trap, setXinc    )
TRAP(long ,       nx_trap, setNx      )
TRAP(float,     xend_trap, setXend    )



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

UPDATE(long ,  nhx_update, getNhx )
UPDATE(long , nhx2_update, getNhx2)
UPDATE(float,   x1_update, getX1  )
UPDATE(float, xinc_update, getXinc)
UPDATE(long ,   nx_update, getNx  )
UPDATE(float, xend_update, getXend)


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


TpFishFileGui::TpFishFileGui(SLDelay *slparent, TpStatfilePair *pair,
                       const char * const filetype)
       : SLSmartForm(slparent, "tp_fish_file_gui", NULL, TRUE)
{
  assert(slparent && pair);

  setDefaultResources(TpResources::getDisplay(), "tp_fish_file_gui", defres);

  SLpLabel  *label  = new SLpLabel (this, (char*)filetype);
  SLRow     *row0   = new SLRow    (this, "row0");
  SLRow     *row1   = new SLRow    (this, "row1");
  SLRow     *row2   = new SLRow    (this, "row2");
  SLRow     *row3   = new SLRow    (this, "row3");
  SLRow     *row4   = new SLRow    (this, "row4");
  SLRow     *row5   = new SLRow    (this, "row5");

  SLpLabel *label0 = new SLpLabel (row0, "type:");
  SLpLabel *label1 = new SLpLabel (row1, "header words......");
  SLpLabel *label2 = new SLpLabel (row2, "first gnd position.");
  SLpLabel *label3 = new SLpLabel (row3, "gnd pos increment..");
  SLpLabel *label4 = new SLpLabel (row4, "number of gnd pos..");
  SLpLabel *label5 = new SLpLabel (row5, "last gnd position..");

  SLpText  *t0  = new SLpText  (row0, "t0", 0, SLpText::_CHAR, 8);

  SLpText  *t1a = new SLpText (row1, "t1a", 0, SLpText::_LONG , 2);
  SLpText  *t1b = new SLpText (row1, "t1b", 0, SLpText::_LONG , 2);
  SLpText  *t2a = new SLpText (row2, "t2a", 0, SLpText::_FLOAT, 6, 3);
  SLpText  *t3a = new SLpText (row3, "t3a", 0, SLpText::_FLOAT, 6, 3);
  SLpText  *t4a = new SLpText (row4, "t4a", 0, SLpText::_LONG , 6);
  SLpText  *t5a = new SLpText (row5, "t5a", 0, SLpText::_FLOAT, 6, 3);

  t0 ->setCtrap      (stattype_trap, pair);
  t1a->setItrap      (     nhx_trap, pair);
  t1b->setItrap      (    nhx2_trap, pair);
  t2a->setFtrap      (      x1_trap, pair);
  t3a->setFtrap      (    xinc_trap, pair);
  t4a->setItrap      (      nx_trap, pair);
  t5a->setFtrap      (    xend_trap, pair);

  t0 ->setupCvarFun  (stattype_update, pair);
  t1a->setupIvarFun  (     nhx_update, pair);
  t1b->setupIvarFun  (    nhx2_update, pair);
  t2a->setupFvarFun  (      x1_update, pair);
  t3a->setupFvarFun  (    xinc_update, pair);
  t4a->setupIvarFun  (      nx_update, pair);
  t5a->setupFvarFun  (    xend_update, pair);

  t0 ->setupSenseFun (sense_update, pair);
  t1a->setupSenseFun (sense_update, pair);
  t1b->setupSenseFun (sense_update, pair);
  t2a->setupSenseFun (sense_update, pair);
  t3a->setupSenseFun (sense_update, pair);
  t4a->setupSenseFun (sense_update, pair);
  t5a->setupSenseFun (sense_update, pair);

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


TpFishFileGui::~TpFishFileGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
