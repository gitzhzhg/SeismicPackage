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

//---------------------- statpop_several.cc --------------------------//
//---------------------- statpop_several.cc --------------------------//
//---------------------- statpop_several.cc --------------------------//

//         implementation file for the StatpopSeveral class
//                 derived from the SLDialog class
//                      subdirectory statgui


#include "statgui/statpop_several.hh"
#include "statgui/statgui_active.hh"
#include "statgui/statgui_status.hh"
#include "statgui/statgui_several.hh"
#include "statgui/statbox_several.hh"
#include "stat/static_dataset.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include "sl/slp_option.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


StatpopSeveral::StatpopSeveral(SLDelay *slparent, char *name,
                                 StaticManager    *manager,
                                 ContainerList    *clist)
         : SLDialog(slparent, name, NULL, FALSE)
{
  assert(manager);
  SLSmartForm  *work  = workArea();

  StatguiStatus    *status = new StatguiStatus    (work, manager, clist);
  StatguiActive    *act    = new StatguiActive    (work, manager);
  StatguiSeveral   *gui    = new StatguiSeveral   (work, manager);
  StatboxSeveral   *table  = new StatboxSeveral   (work, manager);

  gui  ->setStatboxSeveral    (table);
  table->setStatguiSeveral    (gui);

           /////       LEFT  RIGHT  TOP   BOTTOM

  work->attach(status, work, work, work  , NULL, 20, 20,  0);
  work->attach(act   , work, work, status, NULL,  0,  0, 10);
  work->attach(gui   , work, work, act   , NULL,  0,  0, 10);
  work->attach(table , work, work, gui   , work,  0,  0, 10);

  addBottomRemove();
  addBottomKeyhelp();
  addBottomHelp("COMPARE_SEVERAL_DATASETS");

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


StatpopSeveral::~StatpopSeveral()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
