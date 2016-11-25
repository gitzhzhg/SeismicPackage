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

//---------------------- statpop_read.cc ------------------------------//
//---------------------- statpop_read.cc ------------------------------//
//---------------------- statpop_read.cc ------------------------------//

//           implementation file for the StatpopRead class
//                 derived from the SLDialog class
//               derived from the StaticInform class
//                       subdirectory statgui

      // This dialog box is used to read CPS static files.


#include "statgui/statpop_read.hh"
#include "statgui/statgui_status.hh"
#include "statgui/statgui_header.hh"
#include "stat/static_manager.hh"
#include "stat/static_dataset.hh"
#include "stat/static_informer.hh"
#include "stat/static_file_base.hh"
#include "stat/statio_wrapper.hh"
#include "sl/sl_paned_window.hh"
#include "sl/sl_file_choice.hh"
#include "sl/sl_how_read_write.hh"
#include "sl/sl_starting_cards.hh"
#include "sl/slp_file.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_label.hh"
#include "sl/slp_option.hh"
#include "sl/slp_push.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//-------------------- virtual functions overriding SLDialog ---------------//
//-------------------- virtual functions overriding SLDialog ---------------//
//-------------------- virtual functions overriding SLDialog ---------------//

     // private


Boolean StatpopRead::preManageNotify()
{
  _manager->maybeDeleteUndoFiles(this);  // when OK is pushed.
  return TRUE;
}


void StatpopRead::postManageNotify()
{
  _choice->updateFields();
}



void StatpopRead::postUnmanageNotify()
{
  _manager->maybeDeleteUndoFiles(this);
                              // when Cancel is pushed (after Apply).
}


///////// _choice->updateFields() was moved from preManageNotify to
///////// postManageNotify so that the (possibly) time-consuming file
///////// validation will be performed (and maybe showing working messages)
///////// AFTER the popup is displayed rather than BEFORE.

///////////// _manager->maybeDeleteUndoFiles(this) is needed ALSO at
///////////// preManageNotify time since, when the OK button is pushed,
///////////// the undo file has not been saved yet since the dialog box
///////////// is popped down before the time-consuming work takes place.

///// unfortunately, when the OK button is pressed, but a file read error
///// occurs (which causes the dialog box to be popped up again), the
///// undo file gets deleted before there is an opportunity to read it.
///// however, this probably does not matter since the user already
///// decided he didn't want the old data anymore since he chose to read
///// in new data.  whether or not the new data has a read error, the old
///// data is gone as soon as the OK button is pressed.


Boolean StatpopRead::okNotify()
{
  _choice->takeAction(this, FP_OK);
  return FALSE;
}



void StatpopRead::applyNotify()
{
  _choice->takeAction(this, FP_APPLY);
}



Boolean StatpopRead::cancelNotify()
{
  _choice->takeAction(this, FP_CANCEL);
  return TRUE;
}


void StatpopRead::undoNotify()
{
  _manager->activeDataset()->maybeReadUndoFile(this);
}



//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//


static void scan_trap(void *data, long /*ident*/)
{
  StaticFileBase *file     = (StaticFileBase*)data;
  StaticManager  *manager  = file->getStaticManager();
  StatioWrapper  *statio   = file->getStatioWrapper();
  const char     *filename = file->inputFilename();
  StaticInformer *informer = manager->informer();
  char msg[200];
  int err;
  informer->beginSlowOperations();
  informer->showMessage("scanning static file...");
  statio  ->scanForeign(filename, &err, msg);
  if(err != StatioWrapper::STATUS_OK) informer->ringBell();
  informer->showMessage(msg);
  informer->endSlowOperations();
}



static long undo_sense_upfun(void *data)
{
  StatpopRead      *THIS = (StatpopRead*)data;
  StaticDataset *dataset = THIS->getManager()->activeDataset();
  return (dataset->allowReadDeleteUndoFile(THIS));
}



static void desire_trap(void *data, long /*ident*/,
                        long /*oldvar*/, long newvar)
{
  StatpopRead    *THIS = (StatpopRead*)data;
  StaticFileBase *file = THIS->getFileBase();
  file->setReadChoice((int)newvar);
}



static long desire_upfun(void *data)
{ 
  StatpopRead    *THIS = (StatpopRead*)data;
  StaticFileBase *file = THIS->getFileBase();
  return file->getReadChoice();
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        "*choice*Error*background:       red",
        "*choice*Question*background:    goldenrod",
        "*choice.borderWidth:            2",
        "*choice*background:             gray80",
            NULL };



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


StatpopRead::StatpopRead(SLDelay *slparent, char *name,
                                StaticManager *manager,
                                ContainerList *clist)
            : SLDialog(slparent, name, NULL, FALSE),
              StaticInform(manager),
                    _manager      (manager),
                    _statio       (NULL),
                    _file         (NULL),
                    _choice       (NULL)
{
//////////// create non gui objects:

  assert(_manager);
  setFallbackResources(defres);

  _statio = new StatioWrapper(0);

  _file = new StaticFileBase(_manager, this, _statio);

//////////// populate work area:

  SLSmartForm     *work = workArea();
  StatguiStatus *status = new StatguiStatus (work, _manager, clist);

  _choice = new SLFileChoice (work, "choice", SLpFile::_INPUT, _file,
                              "Input Static File...", NULL, FALSE);

//////////// create and populate a container:

  SLDelay      *contain = new SLSmartForm   (work, "contain");

  StatguiHeader   *sum1 = new StatguiHeader (contain, NULL, NULL,
                                             "Static File to Read",
                                           StatguiHeader::EDITABLE_SMALL_PRINT,
                                             StatguiHeader::VALIDATE,
                                             _choice, _statio);

  StatguiHeader   *sum2 = new StatguiHeader (contain, NULL, _manager,
                                             "Active Dataset in Memory",
                                             StatguiHeader::NOT_EDITABLE,
                                             StatguiHeader::ACTIVE);

  SLpPush       *scan = new SLpPush         (contain, "middle", 0,
     "Scan\nFile\n \nusing\n \nHow to Read\nthe File\n\
    \nto find:\n \nfirst bin\nbin increment\nlast bin\n# bins");

  scan->setAtrap(scan_trap, _file);

//////////// create desire option menu:

  SLSmartForm    *row = new SLSmartForm     (work, "row", NULL);

  row ->showEvenSpacing();

  SLpOption *desire = new SLpOption(row, "desire", 0, "");
  desire->addOption("desire", StaticFileBase::READ_ACTIVE,
                         "replace ACTIVE static dataset");
  desire->addOption("desire", StaticFileBase::READ_REF,
                         "replace REFERENCE static dataset");
  desire->addOption("desire", StaticFileBase::READ_NEW,
                         "put data into NEW static dataset");
  desire->addOption("desire", StaticFileBase::READ_NEWACTIVE,
                         "put data into NEW dataset (and make it ACTIVE)");
  desire->addOption("desire", StaticFileBase::READ_NEWREF,
                         "put data into NEW dataset (and make it REFERENCE)");
  desire->addOption("desire", StaticFileBase::READ_NOTHING,
                         "do not read the static file");

  desire->setItrap     (desire_trap , this);
  desire->setupIvarFun (desire_upfun, this);

//////////// create customize label:

  SLDelay  *label = new SLpLabel (work, "label", 0,
                               "Pull Down to Customize Static File Input");

//////////// create customizing tables:

  SLPanedWindow *paned = new SLPanedWindow   (work, "paned");
                         new SLHowReadWrite  (paned, _statio, 1);
                         new SLStartingCards (paned, _statio, _statio, 1);

//////////// do attachments:

           //           LEFT     RIGHT    TOP      BOTTOM
  work->attach(sum1   , contain, NULL   , contain, contain);
  work->attach(scan   , sum1   , sum2   , contain, NULL   , 10, 10, 10);
  work->attach(sum2   , NULL   , contain, contain, contain);

           //           LEFT     RIGHT    TOP      BOTTOM
  work->attach(status , work   , work   ,  work  , NULL, 20, 20);
  work->attach(row    , work   , work   ,  status, NULL,  0,  0, 10);
  work->attach(_choice, work   , work   ,  row   , NULL,  0,  0, 10);
  work->attach(contain, work   , work   , _choice, NULL,  0,  0, 10);
  work->attach(label  , work   , work   , contain, NULL,  0,  0, 10);
  work->attach(paned  , work   , work   , label  , work,  0,  0, 10);

//////////// populate bottom area:

                    addBottomOK    ();
                    addBottomApply ();
  SLpPush *undo   = addBottomUndo  ();
                    addBottomCancel();
                    addBottomHelp  ("READ_STATIC_FILE");

  undo->setupSenseFun (undo_sense_upfun, this);
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


StatpopRead::~StatpopRead()
{
  delete _statio;
  delete _file;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
