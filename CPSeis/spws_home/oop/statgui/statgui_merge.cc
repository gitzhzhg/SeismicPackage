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

//---------------------- statgui_merge.cc ------------------------//
//---------------------- statgui_merge.cc ------------------------//
//---------------------- statgui_merge.cc ------------------------//

//         implementation file for the StatguiMerge class
//               derived from the SLSmartForm class
//                      subdirectory statgui

#include "statgui/statgui_merge.hh"
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


StatguiMerge::StatguiMerge(SLDelay *slparent, StaticManager *manager)
       : SLSmartForm(slparent, "statgui_merge"),
              _manager      (manager),
              _action       (StaticManager::ACTION_AV),
              _size         (StaticManager::SIZE_ALL),
              _where        (StaticManager::WHERE_NEWACTIVE),
              _nils         (StaticManager::NILS_KEEP),
              _interp       (StaticManager::INTERP_NEAR),
              _extrap       (StaticManager::EXTRAP_NILS)
{
  assert(_manager);
  setFallbackResources(defres);

  SLpOption *action = new SLpOption (this, "action", 0,
                                        "action to take:");
  SLpOption *size   = new SLpOption (this, "where", 0,
                                        "size of merged dataset:");
  SLpOption *where  = new SLpOption (this, "where", 0,
                                        "where to put merged dataset:");
  SLpOption *nils   = new SLpOption (this, "nils", 0,
                                        "before resampling:");
  SLpOption *interp = new SLpOption (this, "interp", 0,
                                        "interpolation while resampling:");
  SLpOption *extrap = new SLpOption (this, "extrap", 0,
                                        "extrapolation while resampling:");

  action->addOption("add selected datasets together",
                                             StaticManager::ACTION_ADD);
  action->addOption("average selected datasets together",
                                             StaticManager::ACTION_AV);
  action->addOption("subtract selected dataset from active dataset",
                                             StaticManager::ACTION_SUB);

  size  ->addOption("large enough to include all selected datasets",
                                                   StaticManager::SIZE_ALL);
  size  ->addOption("same size as active dataset", StaticManager::SIZE_ACTIVE);

  where ->addOption("replace ACTIVE static dataset",
                                              StaticManager::WHERE_ACTIVE);
  where ->addOption("create NEW static dataset",
                                              StaticManager::WHERE_NEW);
  where ->addOption("create NEW static dataset (and make it ACTIVE)",
                                              StaticManager::WHERE_NEWACTIVE);

  nils  ->addOption("do not get rid of nils", StaticManager::NILS_KEEP);
  nils  ->addOption("replace nils with zero", StaticManager::NILS_ZERO);
  nils  ->addOption("replace nils with interpolated values",
                                              StaticManager::NILS_TERP);
  nils  ->addOption("interpolate nils in X dir (no extrapolation)",
                                              StaticManager::NILS_XDIR);
  nils  ->addOption("interpolate nils in Y dir (no extrapolation)",
                                              StaticManager::NILS_YDIR);

  interp->addOption("use nearest values", StaticManager::INTERP_NEAR);
  interp->addOption("interpolate among non-nil values",
                                          StaticManager::INTERP_TERP);

  extrap->addOption("extrapolate with edge value", StaticManager::EXTRAP_EDGE);
  extrap->addOption("extrapolate with nils"      , StaticManager::EXTRAP_NILS);
  extrap->addOption("extrapolate with zero"      , StaticManager::EXTRAP_ZERO);

  action->setupIvarPoint(&_action);  // do not need trap or update function.
  size  ->setupIvarPoint(&_size  );  // do not need trap or update function.
  where ->setupIvarPoint(&_where );  // do not need trap or update function.
  nils  ->setupIvarPoint(&_nils  );  // do not need trap or update function.
  interp->setupIvarPoint(&_interp);  // do not need trap or update function.
  extrap->setupIvarPoint(&_extrap);  // do not need trap or update function.

         /////       LEFT     RIGHT    TOP      BOTTOM

  attach(action    , this  ,  this   , this   ,  NULL ,  2, 2,10);
  attach(size      , this  ,  this   , action ,  NULL ,  2, 2, 0);
  attach(where     , this  ,  this   , size   ,  NULL ,  2, 2, 0);
  attach(nils      , this  ,  this   , where  ,  NULL ,  2, 2, 0);
  attach(interp    , this  ,  this   , nils   ,  NULL ,  2, 2, 0);
  attach(extrap    , this  ,  this   , interp ,  this ,  2, 2, 0, 8);
}



//---------------------------- destructor ---------------------------//
//---------------------------- destructor ---------------------------//
//---------------------------- destructor ---------------------------//


StatguiMerge::~StatguiMerge()
{
}



//------------------------- take action ----------------------------//
//------------------------- take action ----------------------------//
//------------------------- take action ----------------------------//

        // public.

void StatguiMerge::takeAction()
{
  _manager->mergeSelectedDatasets(this, (int)_action, (int)_size,
                                        (int)_where, (int)_nils,
                                        (int)_interp, (int)_extrap);
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
