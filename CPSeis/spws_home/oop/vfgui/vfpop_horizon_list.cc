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

//------------------- vfpop_horizon_list.cc -----------------------//
//------------------- vfpop_horizon_list.cc -----------------------//
//------------------- vfpop_horizon_list.cc -----------------------//

//       implementation file for the VfpopHorizonList class
//                   derived from the SLDialog class
//                        subdirectory vfgui


#include "vfgui/vfpop_horizon_list.hh"
#include "vfgui/vfbox_horizon_list.hh"
#include "vfgui/vfbox_horizon_picks.hh"
#include "vfgui/vfgui_horizon_info.hh"
#include "vfgui/vfgui_status.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_informer.hh"
#include "vf/vf_horizons.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_quest_pop.hh"
#include "sl/slp_push.hh"
#include <stream.h>
#include <assert.h>



//------------------------ static functions -------------------------//
//------------------------ static functions -------------------------//
//------------------------ static functions -------------------------//


static void yes_fun(void *data)
{
  VfHorizons *horizons = (VfHorizons*)data;
  horizons->deleteActiveHorizon();
}


static void no_fun(void *)
{
}


static const char *question = "Are you sure you want to delete\n\
the active horizon?";


static void del_trap(void *data, long /*ident*/)
{
  VfpopHorizonList *pop      = (VfpopHorizonList*)data;
  VfManager        *manager  = pop->manager();
  VfHorizons       *horizons = pop->horizons();
  long num = horizons->numHorizons();
  if(num == 0)
      {
      manager->informer()->ringBell();
      manager->informer()->showMessage("there are no horizons to delete");
      return;
      }
  new SLQuestPop(pop, "Question", question, FALSE, yes_fun, no_fun, horizons);
}


static long del_sense_upfun(void *data)
{
  VfHorizons *horizons = (VfHorizons*)data;
  long num = horizons->numHorizons();
  return (num > 0);
}



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


VfpopHorizonList::VfpopHorizonList(SLDelay *slparent, char *name,
                                   VfManager        *manager,
                                   VfHorizons       *horizons,
                                   SLShellContainer *trans,
                                   ContainerList    *clist)
            : SLDialog(slparent, name, NULL, FALSE),
                          _manager   (manager),
                          _horizons  (horizons)
{
  assert(manager && horizons);

  SLSmartForm       *work   = workArea();
  VfguiStatus       *status = new VfguiStatus       (work, manager, clist);
  VfguiHorizonInfo  *info   = new VfguiHorizonInfo  (work, horizons, TRUE);
  VfboxHorizonList  *table  = new VfboxHorizonList  (work, horizons);
  VfboxHorizonPicks *picks  = new VfboxHorizonPicks (work, manager, horizons);
  SLpPush            *push  = new SLpPush           (work, "trans");

  push->manageShellWhenPressed(trans);

  work->attach(status , work, work, work   , NULL,   0,   0);
  work->attach(info   , work, work, status , NULL,   0,   0,  10);
  work->attach(table  , work, work, info   , NULL,   0,   0,  10);
  work->attach(picks  , work, work, table  , push,   0,   0,  10, 10);
  work->attach(push   , work, work, NULL   , work, 150, 150,   0);

  SLpPush *del = addBottomPush("Delete Active Horizon");
                 addBottomRemove();
                 addBottomKeyhelp();
                 addBottomHelp("HORIZON_LIST_OVERVIEW");

  del->setAtrap      (del_trap       , this);
  del->setupSenseFun (del_sense_upfun, horizons);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopHorizonList::~VfpopHorizonList()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
