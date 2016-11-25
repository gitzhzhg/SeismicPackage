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

//---------------------- tp_scan_gui.cc ------------------------//
//---------------------- tp_scan_gui.cc ------------------------//
//---------------------- tp_scan_gui.cc ------------------------//

//         implementation file for the TpScanGui class
//               derived from the SLSmartForm class
//                        subdirectory pick


#include "pick/tp_scan_gui.hh"
#include "pick/tp_popup_base.hh"
#include "sl/slp_label.hh"
#include "sl/slp_toggle.hh"
#include "cprim.h"
#include <string.h>


//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//
//--------------------- static traps -------------------------//

static void togbreak_trap(void *data,long /*ident*/,long /*oldvar*/,long newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setAutoBreak((char)newvar);
}



static void togsnap_trap(void *data,long /*ident*/,long /*oldvar*/,long newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setAutoSnap((char)newvar);
}



static long togbreak_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getAutoBreak();
}



static long togsnap_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getAutoSnap();
}



static long togbreak_sense_update(void *data)
{
long state;

  TpPopupBase *pop = (TpPopupBase*)data;
  if(pop->getAutoMode() == FIRST_BREAK ||
     pop->getAutoMode() == FIRST_BREAK_NO_SNAP ||
     pop->getAutoMode() == FIRST_BREAK_CORR ||
     pop->getAutoMode() == HURST_BREAK ||
     pop->getAutoMode() == HURST_BREAK_NO_SNAP ||
     pop->getAutoMode() == HURST_CORR ||
     pop->getAutoMode() == COMBO ||
     pop->getAutoMode() == COMBO_CORR)
       state = TRUE;
  else
       state = FALSE;

  pop->broadcastScanSensitivityState();

  return state;
}



static long togsnap_sense_update(void *data)
{
long state;

  TpPopupBase *pop = (TpPopupBase*)data;

  if(pop->getSnapChoice() == TP_NONE)
    state = FALSE;
  else
    state = TRUE;

  pop->broadcastScanSensitivityState();

  return state;
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TpScanGui::TpScanGui(SLDelay *slparent, TpPopupBase *pop)
       : SLSmartForm(slparent, "tp_scan_gui", NULL, TRUE)
{
  assert(slparent && pop);

  SLpLabel  *label    = new SLpLabel (this, "Auto Action while Scanning");
            _togbreak = new SLpToggle(this, "pick first breaks");
            _togsnap  = new SLpToggle(this, "snap");

  attach(label   , this, this, this , NULL, 10, 10, 0, 0);
  attach(_togbreak, this, NULL, label, this, 10, 10, 0, 0);
  attach(_togsnap , NULL, this, label, this, 10, 10, 0, 0);

  _togbreak->setItrap     (togbreak_trap        , pop);
  _togsnap ->setItrap     (togsnap_trap         , pop);
  _togbreak->setupIvarFun (togbreak_update      , pop);
  _togsnap ->setupIvarFun (togsnap_update       , pop);
  _togbreak->setupSenseFun(togbreak_sense_update, pop);
  _togsnap ->setupSenseFun(togsnap_sense_update , pop);

}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpScanGui::~TpScanGui()
{
}

//===================================================================
//============ External methods to set sensitivity and state ========
//============ of the toggles                                ========
//===================================================================
void TpScanGui::setBreakSensitivity(Boolean set)
{
  _togbreak->setSensitivity(set);
}

void TpScanGui::setSnapSensitivity(Boolean set)
{
  _togsnap->setSensitivity(set);
}

void TpScanGui::setBreakState(Boolean set)
{
  _togbreak->setToggleValue(set);
}

void TpScanGui::setSnapState(Boolean set)
{
  _togsnap->setToggleValue(set);
}


//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
