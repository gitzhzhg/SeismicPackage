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

//---------------------- statpop_merge.cc ------------------------------//
//---------------------- statpop_merge.cc ------------------------------//
//---------------------- statpop_merge.cc ------------------------------//

//           implementation file for the StatpopMerge class
//                 derived from the SLDialog class
//                       subdirectory statgui


#include "statgui/statpop_merge.hh"
#include "statgui/statbox_list.hh"
#include "statgui/statgui_merge.hh"
#include "statgui/statgui_header.hh"
#include "statgui/statgui_status.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_sep.hh"
#include "sl/slp_push.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_label.hh"
#include <stdio.h>
#include <iostream.h>
#include <assert.h>

#define INFO  \
"All selected datasets will be merged into a single merged dataset\n\
either by adding them together, averaging them, or subtracting them.\n\
The selected datasets will not be changed.\n\
See Help for more information."


//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//

     // these private virtual functions override SLDialog.


void StatpopMerge::postUnmanageNotify()
{
  manager()->maybeDeleteUndoFiles(_gui);
}



Boolean StatpopMerge::okNotify()
{
  StaticDataset *dataset = manager()->activeDataset();
  _gui->takeAction();
  return TRUE;
}



void StatpopMerge::applyNotify()
{
  StaticDataset *dataset = manager()->activeDataset();
  _gui->takeAction();
}



Boolean StatpopMerge::cancelNotify()
{
  return TRUE;
}



void StatpopMerge::undoNotify()
{
  StaticDataset *dataset = manager()->activeDataset();
  dataset->maybeReadUndoFile(_gui);
}



//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//

          // private static functions.
          // called from OK and Apply and Undo pushbuttons.

static long ok_sense_upfun(void* /*data*/)
{
  return TRUE;
}


static long undo_sense_upfun(void *data)
{
  StatpopMerge  *THIS    = (StatpopMerge*)data;
  StaticDataset *dataset = THIS->manager()->activeDataset();
  StatguiMerge  *gui     = THIS->getStatguiMerge();
  return (dataset->allowReadDeleteUndoFile(gui));
}


/*
static long ok_sense_upfun(void *data)
{
  StatpopMerge  *THIS    = (StatpopMerge*)data;
  StaticDataset *dataset = THIS->manager()->activeDataset();
  return (!dataset->isLocked());
}


static long undo_sense_upfun(void *data)
{
  StatpopMerge  *THIS    = (StatpopMerge*)data;
  StaticDataset *dataset = THIS->manager()->activeDataset();
  StatguiMerge  *gui     = THIS->getStatguiMerge();
  return (dataset->allowReadDeleteUndoFile(gui) &&
            !dataset->isLocked());
}
*/



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


StatpopMerge::StatpopMerge(SLDelay *slparent, char *name,
                                StaticManager    *manager,
                                ContainerList    *clist)
            : SLDialog(slparent, name, NULL, FALSE),
                    _manager    (manager),
                    _gui        (NULL)
{
  assert(_manager);

/////// populate work area:

  SLSmartForm     *work = workArea();
  SLpLabel       *label = new SLpLabel      (work, "label", 0, INFO);
  StatguiStatus *status = new StatguiStatus (work, _manager, clist);
                   _gui = new StatguiMerge  (work, _manager);
  StatboxList    *table = new StatboxList   (work, _manager);

  table->setShowOption(StatboxList::SHOW_BINS);

           //           LEFT     RIGHT    TOP    BOTTOM
  work->attach(status , work   , work ,  work  ,  NULL ,  0,  0);
  work->attach(label  , work   , work , status ,  NULL ,  0,  0,   0);
  work->attach(_gui   , work   , work , label  ,  NULL ,  0,  0,   0);
  work->attach(table  , work   , work , _gui   ,  work ,  0,  0,  10);

/////// populate bottom area:

//SLpPush *ok     = addBottomOK    ();
  SLpPush *apply  = addBottomApply ();
  SLpPush *undo   = addBottomUndo  ();
//                  addBottomCancel();
                    addBottomRemove();
                    addBottomHelp  ("MERGE_POPUP");

//ok    ->setupSenseFun          (  ok_sense_upfun, this);
  apply ->setupSenseFun          (  ok_sense_upfun, this);
  undo  ->setupSenseFun          (undo_sense_upfun, this);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


StatpopMerge::~StatpopMerge()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
