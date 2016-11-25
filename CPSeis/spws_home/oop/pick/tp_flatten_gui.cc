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

//---------------------- tp_flatten_gui.cc ------------------------//
//---------------------- tp_flatten_gui.cc ------------------------//
//---------------------- tp_flatten_gui.cc ------------------------//

//         implementation file for the TpFlattenGui class
//               derived from the SLSmartForm class
//                        subdirectory pick


#include "pick/tp_flatten_gui.hh"
#include "pick/tp_popup_base.hh"
#include "sl/slp_push.hh"
#include "sl/slp_text.hh"
#include "cprim.h"
#include "named_constants.h"
#include <string.h>


//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//

void TpFlattenGui::pushTrap(void *data, long /*ident*/)
{
  TpFlattenGui *gui = (TpFlattenGui*)data;
  gui->_pop->flattenToVelocity(gui->_velocity);
}


void TpFlattenGui::textTrap(void *data,long/*ident*/,float/*oldvar*/,
                            float newvar)
{
  TpFlattenGui *gui = (TpFlattenGui*)data;
  gui->_velocity = ConstrainValue(newvar, 100.0, 50000.0);
  gui->_pop->setVelocity(newvar);
}


static void push1_trap(void *data, long /*ident*/)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->flattenPicks();
}


static void push2_trap(void *data, long /*ident*/)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->unflatten();
}


static long push0_sense_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return (pop->haveSeisShift() && pop->haveDisplay());
}


static long push1_sense_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return (pop->pickingInProgress() && pop->haveSeisShift() &&
          pop->haveDisplay());
}


static long push2_sense_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return (pop->haveSeisShift() &&
          pop->haveDisplay() && pop->dataIsShifted());
/*
  return (pop->pickingInProgress() && pop->haveSeisShift() &&
          pop->haveDisplay() && pop->dataIsShifted());
*/
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TpFlattenGui::TpFlattenGui(SLDelay *slparent, TpPopupBase *pop)
       : SLSmartForm(slparent, "tp_flatten_gui", NULL, TRUE),
                 _pop       (pop),
                 _velocity  (5000.0)
{
  assert(slparent && pop);

  SLpPush *push0 = new SLpPush(this, "Flatten to velocity:");
  SLpText *text  = new SLpText(this, "text", 0, SLpText::_FLOAT, 6, 0);
  SLpPush *push1 = new SLpPush(this, "Flatten picks");
  SLpPush *push2 = new SLpPush(this, "Unflatten");

  push0->setAtrap     (pushTrap          , this);
  text ->setFtrap     (textTrap          , this);
  push1->setAtrap     (push1_trap        , pop);
  push2->setAtrap     (push2_trap        , pop);
  text ->setFvar      (_velocity);
  push0->setupSenseFun(push0_sense_update, pop);
  text ->setupSenseFun(push0_sense_update, pop);
  push1->setupSenseFun(push1_sense_update, pop);
  push2->setupSenseFun(push2_sense_update, pop);

  attach(push0, this , NULL, this , NULL);
  attach(text , push0, this, this , NULL);
  attach(push1, this , NULL, push0, this);
  attach(push2, NULL , this, push0, this);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpFlattenGui::~TpFlattenGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
