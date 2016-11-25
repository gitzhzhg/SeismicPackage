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

//---------------------- statgui_several.cc ------------------------//
//---------------------- statgui_several.cc ------------------------//
//---------------------- statgui_several.cc ------------------------//

//         implementation file for the StatguiSeveral class
//               derived from the SLSmartForm class
//               derived from the StaticInform class
//                       subdirectory statgui


#include "statgui/statgui_several.hh"
#include "statgui/statbox_several.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/slp_push.hh"
#include "sl/sl2_text.hh"
#include "sl/sl2_scale.hh"
#include "sl/slp_option.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



static int freeze = FALSE;


//---------------- post new active ground position ------------------//
//---------------- post new active ground position ------------------//
//---------------- post new active ground position ------------------//

       // virtual functions overriding StaticInform.
       // called when active bin changes (or when finding active bins).
       // resets chosen indices to active bin indices.
       // then tells table to scroll to chosen indices.

void StatguiSeveral::postNewActiveGroundPosition(StaticDataset *dataset)
{
  findActiveGroundPosition(dataset);
}


void StatguiSeveral::findActiveGroundPosition(StaticDataset *dataset)
{
  if(freeze) return;
  if(dataset->notActive()) return;
  _chosen_xindex = dataset->getActiveIx();
  _chosen_yindex = dataset->getActiveIy();
  _table->scrollTable();
}



//-------------------- freeze and unfreeze chosen indices -------------//
//-------------------- freeze and unfreeze chosen indices -------------//
//-------------------- freeze and unfreeze chosen indices -------------//

        // public static functions.
        // whether to keep the chosen indices from being reset
        //   when the active ground position changes.

void StatguiSeveral::freezeChosenIndices  ()  { freeze = TRUE; }
void StatguiSeveral::unfreezeChosenIndices()  { freeze = FALSE; }



//---------------------- get and set chosen indices -----------------//
//---------------------- get and set chosen indices -----------------//
//---------------------- get and set chosen indices -----------------//


int StatguiSeveral::getChosenXindex()  const
{
  int nx = manager()->activeDataset()->getNx();
  return ConstrainValue(_chosen_xindex, 0, nx - 1);
}


int StatguiSeveral::getChosenYindex()  const
{
  int ny = manager()->activeDataset()->getNy();
  return ConstrainValue(_chosen_yindex, 0, ny - 1);
}


void StatguiSeveral::setChosenXindex(int ix)
{
  _chosen_xindex = ix;
}


void StatguiSeveral::setChosenYindex(int iy)
{
  _chosen_yindex = iy;
}



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


static void ychosen_trap
                (void *data, long /*ident*/, float /*oldvar*/, float newvar)
{
  StatguiSeveral *gui   = (StatguiSeveral*)data;
  StaticDataset    *dataset  = gui->manager()->activeDataset();
  StatboxSeveral *table = gui->getStatboxSeveral();
  int ix, iy;
  switch(gui->getDirection())
      {
      case StatguiSeveral::XDIRECTION: iy = dataset->getNearestIy(newvar);
                                       gui->setChosenYindex(iy); break;
      case StatguiSeveral::YDIRECTION: ix = dataset->getNearestIx(newvar);
                                       gui->setChosenXindex(ix); break;
      }
////////////////  table->scrollTable();
}


static void yscale_trap
                (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  StatguiSeveral *gui   = (StatguiSeveral*)data;
  StatboxSeveral *table = gui->getStatboxSeveral();
  switch(gui->getDirection())
      {
      case StatguiSeveral::XDIRECTION: gui->setChosenYindex(newvar - 1); break;
      case StatguiSeveral::YDIRECTION: gui->setChosenXindex(newvar - 1); break;
      }
////////////////  table->scrollTable();
}



static void option_trap
                (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  StatguiSeveral *gui   = (StatguiSeveral*)data;
  StatboxSeveral *table = gui->getStatboxSeveral();
  gui->setDirection((int)newvar);
  table->scrollTable();
}



//------------- update functions ----------------------------//
//------------- update functions ----------------------------//
//------------- update functions ----------------------------//


static float ychosen_update(void *data)
{
  StatguiSeveral *gui  = (StatguiSeveral*)data;
  StaticDataset    *dataset = gui->manager()->activeDataset();
  int ix, iy;
  switch(gui->getDirection())
      {
      case StatguiSeveral::XDIRECTION: iy = gui->getChosenYindex();
                                       return dataset->getYbin(iy);
      case StatguiSeveral::YDIRECTION: ix = gui->getChosenXindex();
                                       return dataset->getXbin(ix);
      }
  return FNIL;
}


static char *ychosen_label_update(void *data)
{
  static char *buffer1 = "chosen YBIN:";
  static char *buffer2 = "chosen XBIN:";
  static char *buffer3 = "(all YBINs)";
  static char *buffer4 = "(all XBINs)";
  static char *buffer5 = "------------";
  StatguiSeveral *gui = (StatguiSeveral*)data;
  switch(gui->getDirection())
      {
      case StatguiSeveral::XDIRECTION    : return buffer1;
      case StatguiSeveral::YDIRECTION    : return buffer2;
      case StatguiSeveral::XDIRECTION_ALL: return buffer3;
      case StatguiSeveral::YDIRECTION_ALL: return buffer4;
      }
  return buffer5;
}


static long yscale_update(void *data)
{
  StatguiSeveral *gui = (StatguiSeveral*)data;
  switch(gui->getDirection())
      {
      case StatguiSeveral::XDIRECTION: return gui->getChosenYindex() + 1;
      case StatguiSeveral::YDIRECTION: return gui->getChosenXindex() + 1;
      }
  return 0;
}


static long yscale_min_update(void* data)
{
  StatguiSeveral *gui = (StatguiSeveral*)data;
  switch(gui->getDirection())
      {
      case StatguiSeveral::XDIRECTION: return 1;
      case StatguiSeveral::YDIRECTION: return 1;
      }
  return 0;
}


static long yscale_max_update(void *data)
{
  StatguiSeveral *gui  = (StatguiSeveral*)data;
  StaticDataset    *dataset = gui->manager()->activeDataset();
  switch(gui->getDirection())
      {
      case StatguiSeveral::XDIRECTION: return dataset->getNy();
      case StatguiSeveral::YDIRECTION: return dataset->getNx();
      }
  return 0;
}


static long option_update(void *data)
{
  StatguiSeveral *gui = (StatguiSeveral*)data;
  return gui->getDirection();
}


static long sense_update(void *data)
{
  StatguiSeveral *gui = (StatguiSeveral*)data;
  switch(gui->getDirection())
      {
      case StatguiSeveral::XDIRECTION: return TRUE;
      case StatguiSeveral::YDIRECTION: return TRUE;
      }
  return FALSE;
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


StatguiSeveral::StatguiSeveral(SLDelay *slparent, StaticManager *manager)
       : SLSmartForm(slparent, "statgui_several"),
         StaticInform(manager),
                _table                  (NULL),
                _direction              (XDIRECTION),
                _chosen_xindex          (0),
                _chosen_yindex          (0)
{
  assert(slparent && manager);
  setFallbackResources(defres);

  SL2Text   *ychosen = new SL2Text (this, "ychosen", 0, " ",
                                                    SLpText::_FLOAT, 8, 3);
  SL2Scale  *yscale  = new SL2Scale(this, "yscale", 0, "",
                                            SLpScale::_HORIZONTAL, FALSE);

  SLpOption *option  = new SLpOption(this, "slp_option", 0, "");

  option->addOption(
"show inline (X) direction vertically   (for chosen YBIN)",    XDIRECTION);
  option->addOption(
"show crossline (Y) direction vertically   (for chosen XBIN)", YDIRECTION);
  option->addOption(
"show inline (X) direction vertically   (for all YBINs - one after another)",
                                                               XDIRECTION_ALL);
  option->addOption(
"show crossline (Y) direction vertically   (for all XBINs - one after another)",
                                                               YDIRECTION_ALL);

//                left     right   top     bottom

  attach(option , this   , this  , this  , NULL, 40, 40);
  attach(ychosen, this   , NULL  , NULL  , this);
  attach(yscale , ychosen, this  , option, this);

  ychosen ->setFtrap      (ychosen_trap        , this);
  ychosen ->setupFvarFun  (ychosen_update      , this);
  ychosen ->setupLabelFun (ychosen_label_update, this);
  ychosen ->setupSenseFun (sense_update        , this);
  yscale  ->setItrap      (yscale_trap         , this);
  yscale  ->setupIvarFun  (yscale_update       , this);
  yscale  ->setupIminFun  (yscale_min_update   , this);
  yscale  ->setupImaxFun  (yscale_max_update   , this);
  yscale  ->setupSenseFun (sense_update        , this);

  option  ->setItrap      (option_trap         , this);
  option  ->setupIvarFun  (option_update       , this);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


StatguiSeveral::~StatguiSeveral()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

