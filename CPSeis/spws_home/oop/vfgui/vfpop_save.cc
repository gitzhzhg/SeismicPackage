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

//----------------------------- vfpop_save.cc ---------------------------//
//----------------------------- vfpop_save.cc ---------------------------//
//----------------------------- vfpop_save.cc ---------------------------//

//           implementation file for the VfpopSave class
//                 derived from the SLDialog class
//                 derived from the VfInform class
//                       subdirectory vfgui


#include "vfgui/vfpop_save.hh"
#include "vfgui/vfgui_status.hh"
#include "vfgui/vfgui_save.hh"
#include "vf/vf_file_base.hh"
#include "vf/vf_read_save.hh"
#include "vf/vf_diskfile.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "sl/sl_paned_window.hh"
#include "sl/sl_encoding_desire.hh"
#include "sl/sl_how_read_write.hh"
#include "sl/sl_starting_cards.hh"
#include "sl/sl_file_choice.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include "sl/slp_file.hh"
#include "sl/slp_label.hh"
#include "sl/slp_option.hh"
#include "named_constants.h"
#include <stream.h>
#include <assert.h>



//-------------------- virtual functions overriding SLDialog ---------------//
//-------------------- virtual functions overriding SLDialog ---------------//
//-------------------- virtual functions overriding SLDialog ---------------//

     // private


Boolean VfpopSave::preManageNotify()
{
  _choice->updateFields();
  return TRUE;
}



void VfpopSave::postManageNotify()
{
  _file->readsave()->diskfile()->updatePjarFromKernal
                                   (_manager->activeDataset()->kernal(),
                                    _manager->activeDataset()->history(),
                                    TRUE);
}



Boolean VfpopSave::okNotify()
{
  _choice->takeAction(this, FP_OK);
  return FALSE;
}



void VfpopSave::applyNotify()
{
  _choice->takeAction(this, FP_APPLY);
}



Boolean VfpopSave::cancelNotify()
{
  _choice->takeAction(this, FP_CANCEL);
  return TRUE;
}



//---------------- virtual functions overriding VfInform ---------------//
//---------------- virtual functions overriding VfInform ---------------//
//---------------- virtual functions overriding VfInform ---------------//

     // public

void VfpopSave::afterChanges()
{
  _file->readsave()->diskfile()->updatePjarFromKernal
                                   (_manager->activeDataset()->kernal(),
                                    _manager->activeDataset()->history(),
                                    TRUE);
}



//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//



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
        "*work*custom*height:            2",
                       // last item is irrelevant if custom is a paned window.
*/
            NULL };



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


VfpopSave::VfpopSave(SLDelay *slparent, char *name, VfManager *manager,
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

  _file  = new VfFileBase ("velocity file", "vel",
                           _manager, FileBase::USE_FOR_OUTPUT, FALSE);

  VfDiskfile *diskfile = _file->readsave()->diskfile();  // file to write.
  VelioWrapper *velio2 = _file->velio2();                // file to overwrite.

//////////// populate work area:

  SLSmartForm   *work = workArea();
  VfguiStatus *status = new VfguiStatus  (work, _manager, clist);
  VfguiSave      *gui = new VfguiSave    (work, "gui", _manager, _file);

  _choice = new SLFileChoice (work, "choice", SLpFile::_OUTPUT, _file,
                              "Output Velocity File:", NULL, FALSE);

  SLDelay *desire = new SLEncodingDesire  (work, diskfile, "velocity");

//////////// create customize label:
//////////// (not needed if showing all of each windowbox)

  SLDelay  *label = new SLpLabel (work, "label", 0,
                               "Pull Down to Customize Velocity File Output");

//////////// create customizing tables:

/************
///// to show nothing:
  SLSmartForm    *paned = new SLSmartForm       (work, "paned");
  SLPanedWindow    *win = new SLPanedWindow     (paned, "win");
                          new SLHowReadWrite    (win, diskfile);
                          new SLStartingCards   (win, diskfile, velio2);
  paned->attach(win, paned, paned, paned, paned);
************/

///// to show one line of each windowbox:
  SLPanedWindow  *paned = new SLPanedWindow     (work, "paned");
                          new SLHowReadWrite    (paned, diskfile, 1);
                          new SLStartingCards   (paned, diskfile, velio2,1);

/************
///// to show all of each windowbox:
  SLPanedWindow  *paned = new SLPanedWindow     (work, "paned");
                          new SLHowReadWrite    (paned, diskfile, 14);
                          new SLStartingCards   (paned, diskfile, velio2);
************/

//////////// do attachments:

           //           LEFT     RIGHT    TOP    BOTTOM
  work->attach(status , work   , work ,  work  ,  NULL, 20, 20);
  work->attach(gui    , work   , work ,  status,  NULL, 20, 20, 10);
  work->attach(desire , work   , work ,  gui   ,  NULL,  0,  0, 10);
  work->attach(_choice, work   , work ,  desire,  NULL,  0,  0, 10);
  work->attach(label  , work   , work , _choice,  NULL,  0,  0, 10);
  work->attach(paned  , work   , work ,  label ,  work,  0,  0, 10);

//////////// populate bottom area:

                    addBottomOK     ();
                    addBottomApply  ();
                    addBottomCancel ();
                    addBottomHelp   ("SAVE_VELOCITY_FILE");
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopSave::~VfpopSave()
{
  delete _file;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
