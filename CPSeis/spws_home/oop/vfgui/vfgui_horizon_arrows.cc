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

//---------------------- vfgui_horizon_arrows.cc ------------------------//
//---------------------- vfgui_horizon_arrows.cc ------------------------//
//---------------------- vfgui_horizon_arrows.cc ------------------------//

//       implementation file for the VfguiHorizonArrows class
//               derived from the SLSmartForm class
//                       subdirectory vfgui


#include "vfgui/vfgui_horizon_arrows.hh"
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
  long num = horizons->numHorizons();
  if(num == 0) return;
  newvar = ConstrainValue(newvar, 1, num);
  horizons->setActiveHorizonIndex(newvar - 1);
}


static long arrows_num1_update(void *data)
{
  HORIZONS
  long index = horizons->getActiveHorizonIndex();
  if(index == -1) return 0;
  return (index + 1);
}


static long arrows_num2_update(void *data)
{
  HORIZONS
  long num = horizons->numHorizons();
  return num;
}


static long arrows_sense_update(void *data)
{
  HORIZONS
  long num = horizons->numHorizons();
  return (num > 0);
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


VfguiHorizonArrows::VfguiHorizonArrows
                           (SLDelay *slparent, VfHorizons *horizons)
       : SLSmartForm(slparent, "vfgui_horizon_arrows", NULL, FALSE)
{
  assert(horizons);
  showEvenSpacing();

  SL2Arrows *arrows = new SL2Arrows(this, "arrows", "active horizon", 3);

  arrows->registerNum1Trap    (arrows_num1_trap   , horizons);
  arrows->registerNum1Update  (arrows_num1_update , horizons);
  arrows->registerNum2Update  (arrows_num2_update , horizons);
  arrows->registerSenseUpdate (arrows_sense_update, horizons);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


VfguiHorizonArrows::~VfguiHorizonArrows()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

