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

//---------------------- vfpop_history.cc ------------------------------//
//---------------------- vfpop_history.cc ------------------------------//
//---------------------- vfpop_history.cc ------------------------------//

//         implementation file for the VfpopHistory class
//                 derived from the SLDialog class
//                 derived from the VfInform class
//                      subdirectory vfgui


#include "vfgui/vfpop_history.hh"
#include "vfgui/vfgui_status.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "sl/sl_history_cards.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include <stream.h>
#include <assert.h>



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


VfpopHistory::VfpopHistory(SLDelay *slparent, char *name,
                                VfManager     *manager,
                                ContainerList *clist)
            : SLDialog(slparent, name, NULL, FALSE),
              VfInform(manager),
                  _table     (NULL)
{
  assert(manager);
  SLSmartForm  *work  = workArea();

  VfguiStatus *status = new VfguiStatus   (work, manager, clist);
               _table = new SLHistoryCards(work);

  work->attach(status , work, work, work  , NULL, 20, 20);
  work->attach(_table , work, work, status, work,  0,  0,  10);

  addBottomRemove();
  addBottomKeyhelp();
  addBottomHelp("HISTORY_OVERVIEW");

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopHistory::~VfpopHistory()
{
}



//--------------------- post manage notify --------------------------//
//--------------------- post manage notify --------------------------//
//--------------------- post manage notify --------------------------//


void VfpopHistory::postManageNotify()
{
  _table->setHistory(manager()->activeDataset()->history());
  _table->showLastHistoryCard();
}



//----------------------- after changes -------------------------//
//----------------------- after changes -------------------------//
//----------------------- after changes -------------------------//


void VfpopHistory::afterChanges()
{
  _table->setHistory(manager()->activeDataset()->history());
  _table->showLastHistoryCard();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
