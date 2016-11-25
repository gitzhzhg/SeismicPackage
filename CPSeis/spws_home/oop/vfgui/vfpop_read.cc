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

//----------------------------- vfpop_read.cc ---------------------------//
//----------------------------- vfpop_read.cc ---------------------------//
//----------------------------- vfpop_read.cc ---------------------------//

//            implementation file for the VfpopRead class
//                 derived from the SLDialog class
//                 derived from the VfInform class
//                       subdirectory vfgui


#include "vfgui/vfpop_read.hh"
#include "vfgui/vfgui_status.hh"
#include "vfgui/vfgui_read.hh"
#include "vfgui/vfgui_how_read_write.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_read_save.hh"
#include "vf/vf_diskfile.hh"
#include "vf/vf_file_base.hh"
#include "sl/sl_paned_window.hh"
#include "sl/sl_file_choice.hh"
#include "sl/sl_how_read_write.hh"
#include "sl/sl_starting_cards.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include "sl/slp_file.hh"
#include "sl/slp_option.hh"
#include "sl/slp_label.hh"
#include "named_constants.h"
#include <stream.h>
#include <assert.h>



//-------------------- virtual functions overriding SLDialog ---------------//
//-------------------- virtual functions overriding SLDialog ---------------//
//-------------------- virtual functions overriding SLDialog ---------------//

     // private


Boolean VfpopRead::preManageNotify()
{
  _manager->maybeDeleteUndoFiles(_file);
  return TRUE;
}

void VfpopRead::postManageNotify()
{
  _choice->updateFields();
}

///////// _choice->updateFields() was moved from preManageNotify to
///////// postManageNotify so that the time-consuming file
///////// validation will be performed (and showing working messages)
///////// after the popup is displayed rather than before.

///////////// _manager->maybeDeleteUndoFiles(_file) is needed at
///////////// preManageNotify time since, when the OK button is pushed,
///////////// the undo file has not been saved yet since the dialog box
///////////// is popped down before the time-consuming work takes place.



////// SLDialog has XmUpdateDisplay, but it does not work, presumably
////// because XmUpdateDisplay is called before the expose events make it
////// into the event queue.

void VfpopRead::postUnmanageNotify()
{
//sleep(1);                               // makes the next line work.
//XmUpdateDisplay(W());                   // does not work here (unless sleep).
  _manager->maybeDeleteUndoFiles(_file);
//XmUpdateDisplay(W());                   // works here.
}



Boolean VfpopRead::okNotify()
{
  _choice->takeAction(this, FP_OK);
  return FALSE;
}



void VfpopRead::applyNotify()
{
  _choice->takeAction(this, FP_APPLY);
}



void VfpopRead::undoNotify()
{
  _manager->activeDataset()->maybeReadUndoFile(_file);
}



Boolean VfpopRead::cancelNotify()
{
  _choice->takeAction(this, FP_CANCEL);
  return TRUE;
}



//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//


static long undo_sense_upfun(void *data)
{
  VfpopRead *pop = (VfpopRead*)data;
  if(pop->getManager()->activeDataset()
                 ->allowReadDeleteUndoFile(pop->getFileBase())) return TRUE;
  return FALSE;
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        "*choice*Error*background:       red",
        "*choice*Question*background:    goldenrod",
        "*gui.borderWidth:               2",
        "*gui*background:                gray80",
        "*choice.borderWidth:            2",
        "*choice*background:             gray80",
        "*tab1.borderWidth:              5",
        "*tab1*background:               gray80",
/*
        "*work*custom*c1*height:         5",
        "*work*custom*c2*height:         5",
        "*work*custom*c3*height:         5",
        "*work*custom*c4*height:         5",
        "*work*custom*tab1*height:      25",
*/
            NULL };



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


VfpopRead::VfpopRead(SLDelay *slparent, char *name, VfManager *manager,
                                ContainerList *clist)
            : SLDialog(slparent, name, NULL, FALSE),
              VfInform(manager),
                    _manager    (manager),
                    _file       (NULL),
                    _choice     (NULL)
{
//////////// create non gui objects:

  assert(_manager);
  setFallbackResources(defres);

  _file = new VfFileBase ("velocity file", "vel",
                          _manager, FileBase::USE_FOR_INPUT, FALSE);

  VfDiskfile   *diskfile = _file->readsave()->diskfile();

//////////// populate work area:

  SLSmartForm   *work = workArea();
  VfguiStatus *status = new VfguiStatus  (work, _manager, clist);
  VfguiRead      *gui = new VfguiRead    (work, "gui", _manager, _file);

  _choice = new SLFileChoice (work, "choice", SLpFile::_INPUT, _file,
                              "Input Velocity File...", NULL, FALSE);

  _choice->setComplexNotify(this);

  SLDelay       *tab1 = new VfguiHowReadWrite (work, "tab1", diskfile);

/*
//////////// create customize label:

  SLpLabel *label = new SLpLabel (work, "label", 0,
                               "Pull Down to Customize Velocity File Input");
*/

//////////// create customizing tables:

  SLPanedWindow *paned = new SLPanedWindow     (work, "custom");
/*
  SLDelay        *tab1 = new VfguiHowReadWrite (paned, "tab1", diskfile);
*/
                         new SLHowReadWrite    (paned, diskfile, 14);
                         new SLStartingCards   (paned, diskfile, diskfile);

/*
        ////// The containers c2 and c3 are needed to keep the two
        ////// SLDatabox objects from realizing their true size when
        ////// first popped up.

  SLPanedWindow *custom = new SLPanedWindow  (work, "custom");
  SLSmartForm   *c1     = new SLSmartForm    (custom, "c1");
  SLSmartForm   *c2     = new SLSmartForm    (custom, "c2");
  SLSmartForm   *c4     = new SLSmartForm    (custom, "c4");

  SLDelay        *tab1 = new VfguiHowReadWrite (c1, "tab1", diskfile);
  SLDelay        *tab2 = new SLHowReadWrite    (c2, diskfile);
  SLDelay        *tab4 = new SLStartingCards   (c4, diskfile, diskfile);

             //     LEFT  RIGHT  TOP  BOTTOM
  c1->attach(tab1   , c1 , c1 , c1 , c1 ,  0,  0, 10, 10);
  c2->attach(tab2   , c2 , c2 , c2 , c2 ,  0,  0, 10, 10);
  c4->attach(tab4   , c4 , c4 , c4 , c4 ,  0,  0, 10, 10);
*/

//////////// do attachments:

           //           LEFT     RIGHT    TOP    BOTTOM
  work->attach(status , work   , work ,  work  ,  NULL, 20, 20);
  work->attach(gui    , work   , work ,  status,  NULL, 40, 40, 10);
  work->attach(_choice, work   , work ,  gui   ,  NULL,  0,  0, 10);
/*
  work->attach(label  , work   , work , _choice,  NULL,  0,  0, 10);
  work->attach(custom , work   , work ,  label ,  work,  0,  0, 10);
*/
  work->attach(tab1   , work   , work , _choice,  NULL,  0,  0, 10);
  work->attach(paned  , work   , work ,  tab1  ,  work,  0,  0, 10);

//////////// populate bottom area:

                    addBottomOK     ();
                    addBottomApply  ();
  SLpPush *undo   = addBottomUndo   ();
                    addBottomCancel ();
                    addBottomHelp   ("READ_VELOCITY_FILE");

  undo  ->setupSenseFun (undo_sense_upfun, this);
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopRead::~VfpopRead()
{
  delete _file;
}



//-------------------------- notify complex -----------------------//
//-------------------------- notify complex -----------------------//
//-------------------------- notify complex -----------------------//

      // public virtual function overriding SLDelay.
      // called from SLFileChoice with ident == FileBase::INPUT_VALID or
      //                                        FileBase::INPUT_INVALID.

Boolean VfpopRead::notifyComplex(SLDelay* /*sender*/, int /*ident*/)
{
/****
  if(sender == _choice && _file->readChoice() == READ_NOTHING &&
       ident == FileBase::INPUT_VALID) _file->setReadChoice(READ_ADD);
****/
  return TRUE;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
