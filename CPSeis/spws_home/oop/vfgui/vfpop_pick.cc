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

//---------------------- vfpop_pick.cc ------------------------------//
//---------------------- vfpop_pick.cc ------------------------------//
//---------------------- vfpop_pick.cc ------------------------------//

//         implementation file for the VfpopPick class
//                 derived from the SLDialog class
//                 derived from the VfInform class
//                      subdirectory vfgui


#include "vfgui/vfpop_pick.hh"
#include "vfgui/vfgui_pick.hh"
#include "vfgui/vfbox_pick.hh"
#include "vfgui/vfgui_status.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_function.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include <stream.h>
#include <assert.h>


//----------------- virtual functions overriding VfInform ------------//
//----------------- virtual functions overriding VfInform ------------//
//----------------- virtual functions overriding VfInform ------------//

       // private.

void VfpopPick::postNewActiveDataset ()
{
  saveCopy();
}


void VfpopPick::postTotalChanges     (VfDataset *dataset)
{
  if(dataset->notActive()) return;
  saveCopy();
}


void VfpopPick::postRemoveInsertVelocityFunctions
           (VfDataset *dataset, long /*ifun*/, long /*nrem*/, long /*nins*/)
{
  if(dataset->notActive()) return;
  saveCopy();
}


void VfpopPick::postNewActiveVelocityFunction (VfDataset *dataset)
{
  if(dataset->notActive()) return;
  saveCopy();
}


void VfpopPick::postChangeCoords (VfDataset *dataset, long ifun, long nchng)
{
  if(dataset->notActive()) return;
  long active = dataset->getActiveVelocityFunction();
  if(active == -1) return;
  if(active < ifun || active >= ifun + nchng) return;
  _modified = TRUE;
}


void VfpopPick::postNewDefaultTypes (VfDataset *dataset, long ifun, long nchng)
{
  if(dataset->notActive()) return;
  long active = dataset->getActiveVelocityFunction();
  if(active == -1) return;
  if(active < ifun || active >= ifun + nchng) return;
  _modified = TRUE;
}


void VfpopPick::postModifyStrings (VfDataset *dataset, long ifun, long nchng)
{
  if(dataset->notActive()) return;
  long active = dataset->getActiveVelocityFunction();
  if(active == -1) return;
  if(active < ifun || active >= ifun + nchng) return;
  _modified = TRUE;
}


void VfpopPick::postModifyPicks
       (VfDataset *dataset, long ifun, int /*type*/, long /*ipick*/,
                                          long /*nrem*/, long /*nins*/)
{
  if(dataset->notActive()) return;
  long active = dataset->getActiveVelocityFunction();
  if(active == -1) return;
  if(active != ifun) return;
  _modified = TRUE;
}



//---------------- virtual functions overriding SLDialog ---------------//
//---------------- virtual functions overriding SLDialog ---------------//
//---------------- virtual functions overriding SLDialog ---------------//

       // private.

void VfpopPick::postManageNotify()
{
  saveCopy();
}


void VfpopPick::postUnmanageNotify()
{
  deleteCopy();
}


void VfpopPick::resetNotify()
{
  resetCopy();
}



//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//


static long reset_sense_upfun(void *data)
{
  VfpopPick *pop = (VfpopPick*)data;
  return pop->resetIsUseful();
}



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


VfpopPick::VfpopPick(SLDelay *slparent, char *name,
                                VfManager     *manager,
                                ContainerList *clist)
            : SLDialog(slparent, name, NULL, FALSE),
              VfInform(manager),
                   _velfun    (NULL),
                   _modified  (FALSE)
{
  SLSmartForm  *work  = workArea();

  VfguiStatus *status = new VfguiStatus  (work, manager, clist);
  VfguiPick   *gui    = new VfguiPick    (work, "gui", manager);
  VfboxPick   *table  = new VfboxPick    (work, "pick_table", manager);

  work->attach(status , work, work, work  , NULL, 20, 20);
  work->attach(gui    , work, work, status, NULL,  0,  0,  10);
  work->attach(table  , work, work, gui   , work,  0,  0,  10);

  SLpPush *reset = addBottomReset();
                   addBottomRemove();
                   addBottomKeyhelp();
                   addBottomHelp("PICK_OVERVIEW");

  reset->setupSenseFun(reset_sense_upfun, this);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopPick::~VfpopPick()
{
}



//----------------- save and reset and delete copy ---------------------//
//----------------- save and reset and delete copy ---------------------//
//----------------- save and reset and delete copy ---------------------//

      // private.

void VfpopPick::saveCopy()
{
  deleteCopy();
  VfDataset *dataset = manager()->activeDataset();
  long active = dataset->getActiveVelocityFunction();
  if(active == -1) return;
  _velfun = new VfFunction(manager()->utilities());
  dataset->getVelocityFunctionContents(active, _velfun);
}



void VfpopPick::resetCopy()
{
  if(!_velfun) return;
  VfDataset *dataset = manager()->activeDataset();
  long active = dataset->getActiveVelocityFunction();
  if(active == -1) return;
  dataset->setVelocityFunctionContents(active, _velfun);
  _modified = FALSE;
}



void VfpopPick::deleteCopy()
{
  if(_velfun)
      {
      delete _velfun;
      _velfun = NULL;
      }
  _modified = FALSE;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
