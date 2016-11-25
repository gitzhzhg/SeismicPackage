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

//---------------------- statpop_nils2.cc ------------------------------//
//---------------------- statpop_nils2.cc ------------------------------//
//---------------------- statpop_nils2.cc ------------------------------//

//           implementation file for the StatpopNils2 class
//                 derived from the SLDialog class
//                       subdirectory statgui


#include "statgui/statpop_nils2.hh"
#include "statgui/statgui_nils2.hh"
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



//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//

     // these private virtual functions override SLDialog.


void StatpopNils2::postUnmanageNotify()
{
  manager()->maybeDeleteUndoFiles(_gui);
}



Boolean StatpopNils2::okNotify()
{
  StaticDataset *dataset = manager()->activeDataset();
  _gui->takeAction();
  return TRUE;
}



void StatpopNils2::applyNotify()
{
  StaticDataset *dataset = manager()->activeDataset();
  _gui->takeAction();
}



Boolean StatpopNils2::cancelNotify()
{
  return TRUE;
}



void StatpopNils2::undoNotify()
{
  StaticDataset *dataset = manager()->activeDataset();
  dataset->maybeReadUndoFile(_gui);
}



//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//

          // private static functions.
          // called from OK and Apply and Undo pushbuttons.

static long ok_sense_upfun(void *data)
{
  StatpopNils2   *THIS    = (StatpopNils2*)data;
  StaticDataset  *dataset = THIS->manager()->activeDataset();
  return (!dataset->isLocked());
}


static long undo_sense_upfun(void *data)
{
  StatpopNils2   *THIS    = (StatpopNils2*)data;
  StaticDataset  *dataset = THIS->manager()->activeDataset();
  StatguiNils2   *gui     = THIS->getStatguiNils2();
  return (dataset->allowReadDeleteUndoFile(gui) &&
            !dataset->isLocked());
}



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


StatpopNils2::StatpopNils2(SLDelay *slparent, char *name,
                                StaticManager    *manager,
                                ContainerList    *clist)
            : SLDialog(slparent, name, NULL, FALSE),
                    _manager    (manager),
                    _gui        (NULL)
{
  assert(_manager);

/////// populate work area:

  SLSmartForm   *work   = workArea();
  StatguiStatus *status = new StatguiStatus  (work, _manager, clist, FALSE);
                   _gui = new StatguiNils2   (work, _manager);

           //           LEFT     RIGHT    TOP    BOTTOM
  work->attach(status , work   , work ,  work  ,  NULL ,  0,  0);
  work->attach(_gui   , work   , work ,  status,  work ,  0,  0,  10);

/////// populate bottom area:

//SLpPush *ok     = addBottomOK    ();
  SLpPush *apply  = addBottomApply ();
  SLpPush *undo   = addBottomUndo  ();
//                  addBottomCancel();
                    addBottomRemove();
                    addBottomHelp  ("NILS2_POPUP");

//ok    ->setupSenseFun          (  ok_sense_upfun, this);
  apply ->setupSenseFun          (  ok_sense_upfun, this);
  undo  ->setupSenseFun          (undo_sense_upfun, this);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


StatpopNils2::~StatpopNils2()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
