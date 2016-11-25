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

//---------------------- statgui_active.cc ------------------------//
//---------------------- statgui_active.cc ------------------------//
//---------------------- statgui_active.cc ------------------------//

//         implementation file for the StatguiActive class
//               derived from the SLSmartForm class
//                       subdirectory statgui


#include "statgui/statgui_active.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "stat/static_informer.hh"
#include "sl/slp_push.hh"
#include "sl/sl2_text.hh"
#include "sl/sl2_scale.hh"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


static void find_trap(void *data, long /*ident*/)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  manager->informer()->findActiveGroundPosition(dataset);
}


static void xactive_trap
                (void *data, long /*ident*/, float /*oldvar*/, float newvar)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  dataset->setActiveXbin(newvar);
}


static void xscale_trap
                (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  dataset->setActiveIx(newvar - 1);
}


static void yactive_trap
                (void *data, long /*ident*/, float /*oldvar*/, float newvar)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  dataset->setActiveYbin(newvar);
}


static void yscale_trap
                (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  dataset->setActiveIy(newvar - 1);
}



//------------- update functions ----------------------------//
//------------- update functions ----------------------------//
//------------- update functions ----------------------------//


static float xactive_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  return dataset->getActiveXbin();
}


static long xscale_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  return dataset->getActiveIx() + 1;
}


static long xscale_min_update(void* /*data*/)
{
  return 1;
}


static long xscale_max_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  return dataset->getNx();
}


static float yactive_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  return dataset->getActiveYbin();
}


static long yscale_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  return dataset->getActiveIy() + 1;
}


static long yscale_min_update(void* /*data*/)
{
  return 1;
}


static long yscale_max_update(void *data)
{
  StaticManager *manager = (StaticManager*)data;
  StaticDataset *dataset = manager->activeDataset();
  return dataset->getNy();
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        ".borderWidth:        2",
        "*background:         gray80",
            NULL };



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


StatguiActive::StatguiActive(SLDelay *slparent, StaticManager *manager)
       : SLSmartForm(slparent, "statgui_active"),
               _manager        (manager)
{
  assert(slparent && manager);
  setFallbackResources(defres);

  SLpPush   *find    = new SLpPush (this, "find", 0, "F\nI\nN\nD");

  SL2Text   *xactive = new SL2Text (this, "xactive", 0, "ACTIVE XBIN:",
                                                    SLpText::_FLOAT, 8, 3);
  SL2Scale  *xscale  = new SL2Scale(this, "xscale", 0, "",
                                            SLpScale::_HORIZONTAL, FALSE);

  SL2Text   *yactive = new SL2Text (this, "yactive", 0, "ACTIVE YBIN:",
                                                    SLpText::_FLOAT, 8, 3);
  SL2Scale  *yscale  = new SL2Scale(this, "yscale", 0, "",
                                            SLpScale::_HORIZONTAL, FALSE);

//                left     right   top     bottom

  attach(find   , this   , NULL  , this  , this);

  attach(xactive, find   , NULL  , NULL  , yscale);
  attach(xscale , xactive, this  , this  , yscale);

  attach(yactive, find   , NULL  , NULL  , this);
  attach(yscale , yactive, this  , NULL  , this);

  find    ->setAtrap      (find_trap          , manager);

  xactive ->setFtrap      (xactive_trap       , manager);
  xactive ->setupFvarFun  (xactive_update     , manager);
  xscale  ->setItrap      (xscale_trap        , manager);
  xscale  ->setupIvarFun  (xscale_update      , manager);
  xscale  ->setupIminFun  (xscale_min_update  , manager);
  xscale  ->setupImaxFun  (xscale_max_update  , manager);

  yactive ->setFtrap      (yactive_trap       , manager);
  yactive ->setupFvarFun  (yactive_update     , manager);
  yscale  ->setItrap      (yscale_trap        , manager);
  yscale  ->setupIvarFun  (yscale_update      , manager);
  yscale  ->setupIminFun  (yscale_min_update  , manager);
  yscale  ->setupImaxFun  (yscale_max_update  , manager);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


StatguiActive::~StatguiActive()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

