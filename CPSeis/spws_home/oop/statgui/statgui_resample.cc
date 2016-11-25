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

//---------------------- statgui_resample.cc ------------------------//
//---------------------- statgui_resample.cc ------------------------//
//---------------------- statgui_resample.cc ------------------------//

//         implementation file for the StatguiResample class
//               derived from the SLSmartForm class
//                      subdirectory statgui

#include "statgui/statgui_resample.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/sl_sep.hh"
#include "sl/radio_list.hh"
#include "sl/slp_radio.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_option.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include <stdio.h>
#include <string.h>


//----------------------- set values ---------------------------//
//----------------------- set values ---------------------------//
//----------------------- set values ---------------------------//



//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//
//------------------------ static traps ----------------------------//




//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//
//--------------------- static update functions ----------------//




//----------------- static sense update functions ----------------//
//----------------- static sense update functions ----------------//
//----------------- static sense update functions ----------------//


//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


/*
static char *defres[] = {
        ".borderWidth:        2",
        "*background:         gray80",
        ".sep.separatorType:  SINGLE_LINE",
        ".sep.height:         6",
            NULL };
*/



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


StatguiResample::StatguiResample (SLDelay *slparent,
                       StaticManager *manager, StaticDataset *proposed)
       : SLSmartForm(slparent, "statgui_resample"),
              _manager      (manager),
              _proposed     (proposed),
              _interp       (StaticDataset::INTERP_NEAR),
              _extrap       (StaticDataset::EXTRAP_NILS)
{
  assert(_manager && _proposed);
/*
  setFallbackResources(defres);
*/

  SLpOption *interp = new SLpOption   (this, "interp", 0,
                                                "interpolation option:");
  SLpOption *extrap = new SLpOption   (this, "extrap", 0,
                                                "extrapolation option:");

  interp->addOption("use nearest values", StaticDataset::INTERP_NEAR);
  interp->addOption("interpolate among non-nil values",
                                          StaticDataset::INTERP_TERP);

  extrap->addOption("extrapolate with edge value", StaticDataset::EXTRAP_EDGE);
  extrap->addOption("extrapolate with nils"      , StaticDataset::EXTRAP_NILS);
  extrap->addOption("extrapolate with zero"      , StaticDataset::EXTRAP_ZERO);

  interp->setupIvarPoint(&_interp);  // do not need trap or update function.
  extrap->setupIvarPoint(&_extrap);  // do not need trap or update function.
  
         /////       LEFT     RIGHT    TOP      BOTTOM

  attach(interp    , this  ,  this   , this   ,  NULL ,  2, 2,10);
  attach(extrap    , this  ,  this   , interp ,  this ,  2, 2, 0, 8);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


StatguiResample::~StatguiResample()
{
}



//------------------------- take action ----------------------------//
//------------------------- take action ----------------------------//
//------------------------- take action ----------------------------//

        // public.

void StatguiResample::takeAction()
{
  StaticDataset *dataset = manager()->activeDataset();
  _manager->activeDataset()->resampleStaticValues(this,
                                    (int)_interp, (int)_extrap,
                                    _proposed->getX1(),
                                    _proposed->getY1(),
                                    _proposed->getXinc(),
                                    _proposed->getYinc(),
                                    _proposed->getNx(),
                                    _proposed->getNy());
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
