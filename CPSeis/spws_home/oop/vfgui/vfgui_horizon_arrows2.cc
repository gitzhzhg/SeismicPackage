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

//---------------------- vfgui_horizon_arrows2.cc ------------------------//
//---------------------- vfgui_horizon_arrows2.cc ------------------------//
//---------------------- vfgui_horizon_arrows2.cc ------------------------//

//       implementation file for the VfguiHorizonArrows2 class
//               derived from the SLSmartForm class
//                       subdirectory vfgui


#include "vfgui/vfgui_horizon_arrows2.hh"
#include "vf/vf_horizons.hh"
#include "sl/sl2_arrows.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>


#define HORIZONS  VfHorizons *horizons = (VfHorizons*)data;


//---------------------- arrows static functions ---------------------//
//---------------------- arrows static functions ---------------------//
//---------------------- arrows static functions ---------------------//


static void arrows_num1_trap(void *data, long newvar)
{
  HORIZONS
  long ihorizon = horizons->getActiveHorizonIndex();
  if(ihorizon == -1) return;
  long npicks = horizons->numPicks(ihorizon);
  if(npicks == 0) return;
  newvar = ConstrainValue(newvar, 1, npicks);
  horizons->setActivePick(ihorizon, newvar - 1);
}


static long arrows_num1_update(void *data)
{
  HORIZONS
  long ihorizon = horizons->getActiveHorizonIndex();
  if(ihorizon == -1) return 0;
  long ipick = horizons->getActivePick(ihorizon);
  if(ipick == -1) return 0;
  return (ipick + 1);
}


static long arrows_num2_update(void *data)
{
  HORIZONS
  long ihorizon = horizons->getActiveHorizonIndex();
  if(ihorizon == -1) return 0;
  long npicks = horizons->numPicks(ihorizon);
  return npicks;
}


static long arrows_sense_update(void *data)
{
  HORIZONS
  long ihorizon = horizons->getActiveHorizonIndex();
  if(ihorizon == -1) return FALSE;
  long npicks = horizons->numPicks(ihorizon);
  return (npicks > 0);
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


VfguiHorizonArrows2::VfguiHorizonArrows2
                           (SLDelay *slparent, VfHorizons *horizons)
       : SLSmartForm(slparent, "vfgui_horizon_arrows2", NULL, FALSE)
{
  assert(horizons);
  showEvenSpacing();

  SL2Arrows *arrows = new SL2Arrows(this, "arrows", "active pick", 6);

  arrows->registerNum1Trap    (arrows_num1_trap   , horizons);
  arrows->registerNum1Update  (arrows_num1_update , horizons);
  arrows->registerNum2Update  (arrows_num2_update , horizons);
  arrows->registerSenseUpdate (arrows_sense_update, horizons);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


VfguiHorizonArrows2::~VfguiHorizonArrows2()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

