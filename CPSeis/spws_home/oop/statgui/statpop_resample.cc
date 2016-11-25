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

//---------------------- statpop_resample.cc ------------------------------//
//---------------------- statpop_resample.cc ------------------------------//
//---------------------- statpop_resample.cc ------------------------------//

//           implementation file for the StatpopResample class
//                 derived from the SLDialog class
//                       subdirectory statgui


#include "statgui/statpop_resample.hh"
#include "statgui/statgui_resample.hh"
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


#define INFO  "The active dataset will be resampled to a new grid of \
ground positions,\nbased upon the values specified below.\n\
The dataset might be extended or truncated,\n\
or the bin spacing might be increased or decreased.\n\
Static values at a specified ground position will not change,\n\
except for any necessary interpolation or extrapolation.\n\
If the dataset contains nil values, you might wish to replace nils with\n\
interpolated values before doing any resampling."


//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//

     // these private virtual functions override SLDialog.


void StatpopResample::postUnmanageNotify()
{
  manager()->maybeDeleteUndoFiles(_gui);
}



Boolean StatpopResample::okNotify()
{
  StaticDataset *dataset = manager()->activeDataset();
  _gui->takeAction();
  return TRUE;
}



void StatpopResample::applyNotify()
{
  StaticDataset *dataset = manager()->activeDataset();
  _gui->takeAction();
}



Boolean StatpopResample::cancelNotify()
{
  return TRUE;
}



void StatpopResample::undoNotify()
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
  StatpopResample *THIS    = (StatpopResample*)data;
  StaticDataset   *dataset = THIS->manager()->activeDataset();
  return (!dataset->isLocked());
}


static long undo_sense_upfun(void *data)
{
  StatpopResample *THIS    = (StatpopResample*)data;
  StaticDataset   *dataset = THIS->manager()->activeDataset();
  StatguiResample *gui     = THIS->getStatguiResample();
  return (dataset->allowReadDeleteUndoFile(gui) &&
            !dataset->isLocked());
}



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


StatpopResample::StatpopResample(SLDelay *slparent, char *name,
                                StaticManager    *manager,
                                ContainerList    *clist)
            : SLDialog(slparent, name, NULL, FALSE),
                    _manager    (manager),
                    _proposed   (NULL),
                    _gui        (NULL)
{
  assert(_manager);

/////// populate work area:

              _proposed = new StaticDataset();
  SLSmartForm     *work = workArea();
  SLpLabel       *label = new SLpLabel        (work, "label", 0, INFO);
  StatguiStatus *status = new StatguiStatus   (work, _manager, clist);
                   _gui = new StatguiResample (work, _manager, _proposed);

  StatguiHeader *sum1 = new StatguiHeader (work, NULL, _manager,
                                      "Active Dataset in Memory",
                                      StatguiHeader::NOT_EDITABLE,
                                      StatguiHeader::ACTIVE);

  StatguiHeader *sum2 = new StatguiHeader (work, _proposed, _manager,
                                      "Desired Resampled Active Dataset",
                                      StatguiHeader::EDITABLE_LARGE_PRINT,
                                      StatguiHeader::RESAMPLE);

           //           LEFT     RIGHT    TOP    BOTTOM
  work->attach(status , work   , work ,  work  ,  NULL ,  0,  0);
  work->attach(label  , work   , work ,  status,  NULL ,  0,  0,  0);
  work->attach(_gui   , work   , work ,  label ,  NULL , 60, 60,  0);
  work->attach(sum1   , work   , NULL ,  _gui  ,  work ,  0,  0,  0);
  work->attach(sum2   , sum1   , work ,  _gui  ,  work , 10,  0,  0);

/////// populate bottom area:

  SLpPush *apply  = addBottomApply ();
  SLpPush *undo   = addBottomUndo  ();
                    addBottomRemove();
                    addBottomHelp  ("RESAMPLE_POPUP");

  apply->setupSenseFun (  ok_sense_upfun, this);
  undo ->setupSenseFun (undo_sense_upfun, this);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


StatpopResample::~StatpopResample()
{
  delete _proposed;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
