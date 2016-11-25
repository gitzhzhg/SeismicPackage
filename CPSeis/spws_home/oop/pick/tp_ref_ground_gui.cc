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

//---------------------- tp_ref_ground_gui.cc ------------------------//
//---------------------- tp_ref_ground_gui.cc ------------------------//
//---------------------- tp_ref_ground_gui.cc ------------------------//

//         implementation file for the TpRefGroundGui class
//               derived from the SLSmartForm class
//                        subdirectory pick


#include "pick/tp_ref_ground_gui.hh"
#include "pick/tp_ref_pair.hh"
#include "sl/slp_label.hh"
#include "sl/slp_option.hh"
#include "cprim.h"
#include "stdio.h"
#include "string.h"


//-------------------------- static traps -----------------------//
//-------------------------- static traps -----------------------//
//-------------------------- static traps -----------------------//

static void ground_trap(void *data, long/*ident*/,long/*oldvar*/, long newvar)
{
  TpRefPair *pair = (TpRefPair*)data;
  pair->setTypeGP(newvar);
}



//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//

static long ground_update(void *data)
{
  TpRefPair *pair = (TpRefPair*)data;
  return pair->getTypeGP();
}


static long ground_sense_update(void *data)
{
  TpRefPair *pair = (TpRefPair*)data;
  return !pair->fileIsLoaded();
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TpRefGroundGui::TpRefGroundGui(SLDelay *slparent, TpRefPair *pair)
       : SLSmartForm(slparent, "tp_ref_ground_gui", NULL, TRUE)
{
  assert(slparent && pair);

  SLpLabel  *label  = new SLpLabel (this, "Ground Positions:");
  SLpOption *ground = new SLpOption(this, "ground");

  ground->addOption("sequential (46,47)", SEQU);
  ground->addOption("grid (33,34,35,36)", GRID);

  ground->setItrap      (ground_trap        , pair);
  ground->setupIvarFun  (ground_update      , pair);
  ground->setupSenseFun (ground_sense_update, pair);

  attach(label , this , NULL,   this, NULL, 0, 0, 0, 0);
  attach(ground, NULL , this,  label, this, 0, 0, 0, 0);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpRefGroundGui::~TpRefGroundGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
