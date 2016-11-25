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

//---------------------- vfgui_horizon_info.cc ------------------------//
//---------------------- vfgui_horizon_info.cc ------------------------//
//---------------------- vfgui_horizon_info.cc ------------------------//

//         implementation file for the VfguiHorizonInfo class
//               derived from the SLSmartForm class
//                       subdirectory vfgui


#include "vfgui/vfgui_horizon_info.hh"
#include "vfgui/vfgui_horizon_arrows.hh"
#include "vfgui/vfgui_horizon_arrows2.hh"
#include "vf/vf_horizons.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_text.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#define NBUF  111

#define HORIZONS  VfHorizons *horizons = (VfHorizons*)data;



//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


static char *row2_upfun(void *data)
{                                
  HORIZONS
  static char *blank = " ";
  static char buffer[NBUF];
  long active = horizons->getActiveHorizonIndex();
  if(active == -1) return blank;
                                   strcpy(buffer, horizons->getName(active));
  if(horizons->readError (active)) strcat(buffer, "  (read error)");
                                   strcat(buffer, "       ");
                                   strcat(buffer, horizons->getColor(active));
  if(horizons->isSelected(active)) strcat(buffer, "    (selected)");
  return buffer;
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        ".borderWidth:         2",
/*
        "*background:          light sea green",
*/
        "*background:          orange",
            NULL };



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


VfguiHorizonInfo::VfguiHorizonInfo
                         (SLDelay *slparent, VfHorizons *horizons, int two)
       : SLSmartForm(slparent, "vfgui_horizon_info", NULL, FALSE)
{
  setFallbackResources(defres);

  VfguiHorizonArrows  *row1 = new VfguiHorizonArrows (this, horizons);
  SLpText             *row2 = new SLpText            (this, "row2");
  VfguiHorizonArrows2 *row3 = NULL;

  if(two) row3 = new VfguiHorizonArrows2 (this, horizons);

  row2->showLabelAppearance();
  row2->setupCvarFun(row2_upfun, horizons);

         //              LEFT   RIGHT   TOP  BOTTOM
  if(two)
      {
      attach     (row1,  this,  this,  this,  NULL,   4, 4, 4, 4);
      attach     (row2,  this,  this,  row1,  NULL,   4, 4, 4, 4);
      attach     (row3,  this,  this,  row2,  this,   4, 4, 4, 4);
      }
  else
      {
      attach     (row1,  this,  this,  this,  NULL,   4, 4, 4, 4);
      attach     (row2,  this,  this,  row1,  this,   4, 4, 4, 4);
      }
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


VfguiHorizonInfo::~VfguiHorizonInfo()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

