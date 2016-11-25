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

//---------------------- tp_mute_file_gui.cc ------------------------//
//---------------------- tp_mute_file_gui.cc ------------------------//
//---------------------- tp_mute_file_gui.cc ------------------------//

//         implementation file for the TpMuteFileGui class
//               derived from the SLSmartForm class
//                        subdirectory pick


#include "pick/tp_mute_file_gui.hh"
#include "cprim.h"
#include "pick/tp_mute_pair.hh"
#include "pick/tp_resources.hh"
#include "sl/sl_row.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "sl/slp_option.hh"
#include "stdio.h"
#include "string.h"


#define SHOT_2D    1
#define SHOT_3D    2
#define CMP_2D     3
#define CMP_3D     4
#define STACK_2D   5
#define STACK_3D   6
#define CUSTOMIZED 7


//-------------------------- static traps -----------------------//
//-------------------------- static traps -----------------------//
//-------------------------- static traps -----------------------//


static void choice_trap(void *data, long /* ident */, long  /* oldvar */,
                        long newvar)
{
  TpMutePair *pair = (TpMutePair*)data;
  long nhx, nhy, nhz;
  switch(newvar)
    {
    case SHOT_2D : nhx = 6; nhy = 46; nhz = 0 ; break;
    case SHOT_3D : nhx = 6; nhy = 33; nhz = 34; break;
    case CMP_2D  : nhx = 6; nhy = 7 ; nhz = 0 ; break;
    case CMP_3D  : nhx = 6; nhy = 7 ; nhz = 8 ; break;
    case STACK_2D: nhx = 7; nhy = 0 ; nhz = 0 ; break;
    case STACK_3D: nhx = 7; nhy = 8 ; nhz = 0 ; break;
    default: return;
    }
  pair->setNhx(nhx);
  pair->setNhy(nhy);
  pair->setNhz(nhz);
}



#define TRAP(nhx_trap, setNhx)                          \
static void nhx_trap(void *data, long /* ident */,      \
                        long /* oldvar */, long newvar) \
{                                                       \
  TpMutePair *pair = (TpMutePair*)data;                 \
  pair->setNhx(newvar);                                 \
}

TRAP(nhx_trap, setNhx)
TRAP(nhy_trap, setNhy)
TRAP(nhz_trap, setNhz)



//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//


static long choice_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  long nhx = pair->getNhx();
  long nhy = pair->getNhy();
  long nhz = pair->getNhz();
  long choice;
  if     (nhx == 6 && nhy == 46 && nhz == 0 ) choice = SHOT_2D;
  else if(nhx == 6 && nhy == 33 && nhz == 34) choice = SHOT_3D;
  else if(nhx == 6 && nhy == 7  && nhz == 0 ) choice = CMP_2D;
  else if(nhx == 6 && nhy == 7  && nhz == 8 ) choice = CMP_3D;
  else if(nhx == 7 && nhy == 0  && nhz == 0 ) choice = STACK_2D;
  else if(nhx == 7 && nhy == 8  && nhz == 0 ) choice = STACK_3D;
  else                                        choice = CUSTOMIZED;
  return choice;
}



#define UPDATE(nhx_update, getNhx)           \
static long nhx_update(void *data)           \
{                                            \
  TpMutePair *pair = (TpMutePair*)data;      \
  return pair->getNhx();                     \
}

UPDATE(nhx_update, getNhx)
UPDATE(nhy_update, getNhy)
UPDATE(nhz_update, getNhz)


static long sense_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  return pair->allowHeaderInput();
}



static char *mode_update(void *data)
{
  static char *mode1 = "offset considered negative\n\
if hdr 47 (recvr sequ gnd pos)\n\
< hdr 46 (source sequ gnd pos)";
  static char *mode2 = "offset considered negative\n\
if hdr 35 (recvr X gnd pos)\n\
< hdr 33 (source X gnd pos)";
  static char *mode3 = "offset header value\n\
used\nwithout modification";

  TpMutePair *pair = (TpMutePair*)data;
  long nhx = pair->getNhx();
  long nhy = pair->getNhy();
  long nhz = pair->getNhz();
  if     (nhx == 6 && nhy == 46 && nhz == 0 ) return mode1;
  else if(nhx == 6 && nhy == 33 && nhz == 34) return mode2;
  return mode3;
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//

static String defres[]= {
    "*fontList: 8x13bold",
    NULL };


TpMuteFileGui::TpMuteFileGui(SLDelay *slparent, TpMutePair *pair)
       : SLSmartForm(slparent, "tp_mute_file_gui", NULL, TRUE)
{
  assert(slparent && pair);

  setDefaultResources(TpResources::getDisplay(), "tpmf_mode", defres);

  SLpLabel  *label  = new SLpLabel (this, "Header Words");
  SLpOption *option = new SLpOption(this, "tpmf_option");

  option->addOption("2D shot profiles"  , SHOT_2D   );
  option->addOption("3D shot profiles"  , SHOT_3D   );
  option->addOption("2D CMP gathers"    , CMP_2D    );
  option->addOption("3D CMP gathers"    , CMP_3D    );
  option->addOption("2D stacked section", STACK_2D  );
  option->addOption("3D stacked dataset", STACK_3D  );
  option->addOption("customized headers", CUSTOMIZED);

  option->setItrap     (choice_trap  , pair);
  option->setupIvarFun (choice_update, pair);
  option->setupSenseFun( sense_update, pair);

/*
  option->setupOptionSenseFun (SHOT_2D , sense_update, pair);
  option->setupOptionSenseFun (SHOT_3D , sense_update, pair);
  option->setupOptionSenseFun (CMP_2D  , sense_update, pair);
  option->setupOptionSenseFun (CMP_3D  , sense_update, pair);
  option->setupOptionSenseFun (STACK_2D, sense_update, pair);
  option->setupOptionSenseFun (STACK_3D, sense_update, pair);
*/
  option->setOptionSense      (CUSTOMIZED, FALSE);

  SLRow     *row1  = new SLRow    (this, "row1");
  SLRow     *row2  = new SLRow    (this, "row2");
  SLRow     *row3  = new SLRow    (this, "row3");
  SLpLabel  *mode  = new SLpLabel (this, "tpmf_mode", 0, "xx\nxx\nxx");

  mode->setupCvarFun(mode_update, pair);

  SLpLabel *label1 = new SLpLabel (row1, "offset .........");
  SLpLabel *label2 = new SLpLabel (row2, "inline bin .....");
  SLpLabel *label3 = new SLpLabel (row3, "crossline bin ..");

  SLpText  *t1 = new SLpText (row1, "t1", 0, SLpText::_LONG , 6);
  SLpText  *t2 = new SLpText (row2, "t2", 0, SLpText::_LONG , 6);
  SLpText  *t3 = new SLpText (row3, "t3", 0, SLpText::_LONG , 6);

  t1->setItrap      ( nhx_trap, pair);
  t2->setItrap      ( nhy_trap, pair);
  t3->setItrap      ( nhz_trap, pair);

  t1->setupIvarFun  ( nhx_update, pair);
  t2->setupIvarFun  ( nhy_update, pair);
  t3->setupIvarFun  ( nhz_update, pair);

  t1->setupSenseFun (sense_update, pair);
  t2->setupSenseFun (sense_update, pair);
  t3->setupSenseFun (sense_update, pair);

  attach(label , this , this,   this, NULL, 0, 0, 0, 0);
  attach(option, this , this,  label, NULL, 0, 0, 0, 0);
  attach(row1  , this , this, option, NULL, 0, 0, 0, 0);
  attach(row2  , this , this,   row1, NULL, 0, 0, 0, 0);
  attach(row3  , this , this,   row2, NULL, 0, 0, 0, 0);
  attach(mode  , this , this,   row3, this, 0, 0, 0, 0);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpMuteFileGui::~TpMuteFileGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
