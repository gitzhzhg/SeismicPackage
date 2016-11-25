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

//---------------------- statgui_values.cc ------------------------//
//---------------------- statgui_values.cc ------------------------//
//---------------------- statgui_values.cc ------------------------//

//         implementation file for the StatguiValues class
//               derived from the SLSmartForm class
//               derived from the StaticInform class
//                       subdirectory statgui


#include "statgui/statgui_values.hh"
#include "statgui/statbox_values.hh"
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


enum { XDIRECTION = 1, YDIRECTION };

static int freeze = FALSE;


//---------------- post new active ground position ------------------//
//---------------- post new active ground position ------------------//
//---------------- post new active ground position ------------------//

       // virtual functions overriding StaticInform.
       // called when active bin changes (or when finding active bins).
       // resets chosen indices to active bin indices.
       // then tells table to scroll to chosen indices.

void StatguiValues::postNewActiveGroundPosition(StaticDataset *dataset)
{
  findActiveGroundPosition(dataset);
}


void StatguiValues::findActiveGroundPosition(StaticDataset *dataset)
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

void StatguiValues::freezeChosenIndices  ()  { freeze = TRUE; }
void StatguiValues::unfreezeChosenIndices()  { freeze = FALSE; }



//---------------------- get and set values -------------------------//
//---------------------- get and set values -------------------------//
//---------------------- get and set values -------------------------//


int StatguiValues::isXdirection()  const
{
  return (_direction == XDIRECTION);
}


int StatguiValues::isYdirection()  const
{
  return (_direction == YDIRECTION);
}


int StatguiValues::getChosenXindex()  const
{
  int nx = manager()->activeDataset()->getNx();
  return ConstrainValue(_chosen_xindex, 0, nx - 1);
}


int StatguiValues::getChosenYindex()  const
{
  int ny = manager()->activeDataset()->getNy();
  return ConstrainValue(_chosen_yindex, 0, ny - 1);
}


void StatguiValues::setChosenXindex(int ix)
{
  int nx = manager()->activeDataset()->getNx();
  _chosen_xindex = ConstrainValue(ix, 0, nx - 1);
}


void StatguiValues::setChosenYindex(int iy)
{
  int ny = manager()->activeDataset()->getNy();
  _chosen_yindex = ConstrainValue(iy, 0, ny - 1);
}



void StatguiValues::stepChosenIndex(int step)   // step is +-1
{
  if(_direction == XDIRECTION) setChosenYindex(_chosen_yindex + step);
  else                         setChosenXindex(_chosen_xindex + step);
}



//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//
//---------------------- traps ------------------------------//


static void ychosen_trap
                (void *data, long /*ident*/, float /*oldvar*/, float newvar)
{
  StatguiValues *gui   = (StatguiValues*)data;
  StaticDataset    *dataset  = gui->manager()->activeDataset();
  StatboxValues *table = gui->getStatboxValues();
  int ix, iy;
  switch(gui->getDirection())
      {
      case XDIRECTION: iy = dataset->getNearestIy(newvar);
                       gui->setChosenYindex(iy); break;
      case YDIRECTION: ix = dataset->getNearestIx(newvar);
                       gui->setChosenXindex(ix); break;
      }
}


static void yscale_trap
                (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  StatguiValues *gui   = (StatguiValues*)data;
  StatboxValues *table = gui->getStatboxValues();
  switch(gui->getDirection())
      {
      case XDIRECTION: gui->setChosenYindex(newvar - 1); break;
      case YDIRECTION: gui->setChosenXindex(newvar - 1); break;
      }
}



static void option_trap
                (void *data, long /*ident*/, long /*oldvar*/, long newvar)
{
  StatguiValues *gui   = (StatguiValues*)data;
  StatboxValues *table = gui->getStatboxValues();
  gui->setDirection((int)newvar);
  table->scrollTable();
}



//------------- update functions ----------------------------//
//------------- update functions ----------------------------//
//------------- update functions ----------------------------//


static float ychosen_update(void *data)
{
  StatguiValues *gui  = (StatguiValues*)data;
  StaticDataset    *dataset = gui->manager()->activeDataset();
  int ix, iy;
  switch(gui->getDirection())
      {
      case XDIRECTION: iy = gui->getChosenYindex();
                       return dataset->getYbin(iy);
      case YDIRECTION: ix = gui->getChosenXindex();
                       return dataset->getXbin(ix);
      }
  return FNIL;
}


static char *ychosen_label_update(void *data)
{
  static char *buffer1 = "first chosen YBIN:";
  static char *buffer2 = "first chosen XBIN:";
  static char *buffer5 = "------------";
  StatguiValues *gui = (StatguiValues*)data;
  switch(gui->getDirection())
      {
      case XDIRECTION: return buffer1;
      case YDIRECTION: return buffer2;
      }
  return buffer5;
}


static long yscale_update(void *data)
{
  StatguiValues *gui = (StatguiValues*)data;
  switch(gui->getDirection())
      {
      case XDIRECTION: return gui->getChosenYindex() + 1;
      case YDIRECTION: return gui->getChosenXindex() + 1;
      }
  return 0;
}


static long yscale_min_update(void* data)
{
  StatguiValues *gui = (StatguiValues*)data;
  switch(gui->getDirection())
      {
      case XDIRECTION: return 1;
      case YDIRECTION: return 1;
      }
  return 0;
}


static long yscale_max_update(void *data)
{
  StatguiValues *gui  = (StatguiValues*)data;
  StaticDataset    *dataset = gui->manager()->activeDataset();
  switch(gui->getDirection())
      {
      case XDIRECTION: return dataset->getNy();
      case YDIRECTION: return dataset->getNx();
      }
  return 0;
}


static long option_update(void *data)
{
  StatguiValues *gui = (StatguiValues*)data;
  return gui->getDirection();
}


static long sense_update(void *data)
{
  StatguiValues *gui = (StatguiValues*)data;
  switch(gui->getDirection())
      {
      case XDIRECTION: return TRUE;
      case YDIRECTION: return TRUE;
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


StatguiValues::StatguiValues(SLDelay *slparent, StaticManager *manager)
       : SLSmartForm(slparent, "statgui_values"),
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

  option->addOption
      ("show inline (X) direction vertically   (for several chosen YBINs)",
                                                              XDIRECTION);
  option->addOption
      ("show crossline (Y) direction vertically   (for several chosen XBINs)",
                                                              YDIRECTION);

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


StatguiValues::~StatguiValues()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

