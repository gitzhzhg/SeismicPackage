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

//---------------------- statpop_history.cc ------------------------------//
//---------------------- statpop_history.cc ------------------------------//
//---------------------- statpop_history.cc ------------------------------//

//         implementation file for the StatpopHistory class
//                 derived from the SLDialog class
//               derived from the StaticInform class
//                      subdirectory statgui


#include "statgui/statpop_history.hh"
#include "statgui/statgui_status.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "named_constants.h"
#include "sl/sl_history_cards.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


StatpopHistory::StatpopHistory(SLDelay *slparent, char *name,
                                StaticManager    *manager,
                                ContainerList    *clist)
            : SLDialog(slparent, name, NULL, FALSE),
              StaticInform(manager),
                  _table     (NULL)
{
  assert(manager);
  SLSmartForm  *work  = workArea();

  StatguiStatus  *status = new StatguiStatus  (work, manager, clist);
                 _table  = new SLHistoryCards (work, NULL, TRUE);

  work->attach(status , work, work, work  , NULL, 20, 20);
  work->attach(_table , work, work, status, work,  0,  0,  10);

  addBottomRemove();
  addBottomKeyhelp();
  addBottomHelp("STATIC_HISTORY");

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


StatpopHistory::~StatpopHistory()
{
}



//--------------------- post manage notify --------------------------//
//--------------------- post manage notify --------------------------//
//--------------------- post manage notify --------------------------//


void StatpopHistory::postManageNotify()
{
  _table->setHistory(manager()->activeDataset()->history());
  _table->showLastHistoryCard();
}



//----------------------- after changes -------------------------//
//----------------------- after changes -------------------------//
//----------------------- after changes -------------------------//


void StatpopHistory::afterChanges()
{
  _table->setHistory(manager()->activeDataset()->history());
  _table->showLastHistoryCard();
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
