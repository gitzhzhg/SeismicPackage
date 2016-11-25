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

//---------------------- statgui_splice.cc ------------------------//
//---------------------- statgui_splice.cc ------------------------//
//---------------------- statgui_splice.cc ------------------------//

//         implementation file for the StatguiSplice class
//               derived from the SLSmartForm class
//                      subdirectory statgui

#include "statgui/statgui_splice.hh"
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


static const char *defres[] = {
        ".borderWidth:        2",
        "*background:         gray80",
        ".sep.separatorType:  SINGLE_LINE",
        ".sep.height:         6",
            NULL };



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


StatguiSplice::StatguiSplice(SLDelay *slparent, StaticManager *manager)
       : SLSmartForm(slparent, "statgui_splice"),
              _manager      (manager),
              _where        (StaticManager::WHERE_NEWACTIVE),
              _nils         (StaticManager::NILS_KEEP),
              _interp       (StaticManager::INTERP_NEAR),
              _xdist        (10),
              _ydist        (10)
{
  assert(_manager);
  setFallbackResources(defres);

  SLpOption *where  = new SLpOption (this, "where", 0,
                                        "where to put spliced dataset:");
  SLpOption *nils   = new SLpOption (this, "nils", 0,
                                        "before resampling:");
  SLpOption *interp = new SLpOption (this, "interp", 0,
                                        "interpolation while resampling:");

  where ->addOption("replace ACTIVE static dataset",
                                              StaticManager::WHERE_ACTIVE);
  where ->addOption("create NEW static dataset",
                                              StaticManager::WHERE_NEW);
  where ->addOption("create NEW static dataset (and make it ACTIVE)",
                                              StaticManager::WHERE_NEWACTIVE);

  nils  ->addOption("do not get rid of nils", StaticManager::NILS_KEEP);
  nils  ->addOption("interpolate nils in X dir (no extrapolation)",
                                              StaticManager::NILS_XDIR);
  nils  ->addOption("interpolate nils in Y dir (no extrapolation)",
                                              StaticManager::NILS_YDIR);

  interp->addOption("use nearest values", StaticManager::INTERP_NEAR);
  interp->addOption("interpolate among non-nil values",
                                          StaticManager::INTERP_TERP);

  SL2Text *xdist = new SL2Text (this, "xdist", 0,
         "maximum distance (#points) for grading weights in X direction:",
                                                SLpText::_LONG, 3);
  SL2Text *ydist = new SL2Text (this, "ydist", 0,
         "maximum distance (#points) for grading weights in Y direction:",
                                                SLpText::_LONG, 3);

  where ->setupIvarPoint(&_where );  // do not need trap or update function.
  nils  ->setupIvarPoint(&_nils  );  // do not need trap or update function.
  interp->setupIvarPoint(&_interp);  // do not need trap or update function.
  xdist ->setupIvarPoint(&_xdist );  // do not need trap or update function.
  ydist ->setupIvarPoint(&_ydist );  // do not need trap or update function.

         /////       LEFT     RIGHT    TOP      BOTTOM

  attach(where     , this  ,  this   , this   ,  NULL ,  2, 2,10);
  attach(nils      , this  ,  this   , where  ,  NULL ,  2, 2, 0);
  attach(interp    , this  ,  this   , nils   ,  NULL ,  2, 2, 0);
  attach(xdist     , this  ,  NULL   , interp ,  NULL , 22, 2, 7);
  attach(ydist     , this  ,  NULL   , xdist  ,  this , 22, 2, 7, 8);
}



//---------------------------- destructor ---------------------------//
//---------------------------- destructor ---------------------------//
//---------------------------- destructor ---------------------------//


StatguiSplice::~StatguiSplice()
{
}



//------------------------- take action ----------------------------//
//------------------------- take action ----------------------------//
//------------------------- take action ----------------------------//

        // public.

void StatguiSplice::takeAction()
{
  _manager->mergeSelectedDatasets(this, StaticManager::ACTION_AV,
                                        StaticManager::SIZE_ALL,
                                        (int)_where,
                                        (int)_nils,
                                        (int)_interp,
                                        StaticManager::EXTRAP_NILS,
                                        _xdist, _ydist);
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
