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

//------------------------ vfpop_horizon_read.cc ----------------------//
//------------------------ vfpop_horizon_read.cc ----------------------//
//------------------------ vfpop_horizon_read.cc ----------------------//

//        implementation file for the VfpopHorizonRead class
//                 derived from the SLDialog class
//                       subdirectory vfgui


#include "vfgui/vfpop_horizon_read.hh"
#include "vfgui/vfgui_horizon_info.hh"
#include "vfgui/vfgui_status.hh"
/*
#include "vfgui/vfbox_horizon_read_write.hh"
*/
#include "vf/vf_horizons.hh"
#include "vf/vf_horizon_file.hh"
#include "vf/vf_horizonio.hh"
#include "sl/sl_file_choice.hh"
#include "sl/sl_paned_window.hh"
#include "sl/sl_how_read_write.hh"
#include "sl/sl_starting_cards.hh"
#include "sl/sl_smart_form.hh"
#include "sl/slp_push.hh"
#include "sl/slp_file.hh"
#include "sl/slp_label.hh"
#include <stream.h>
#include <assert.h>



#define PROMPT  "Read Horizon File..."
#define LABEL   "The horizon file will be read into a new horizon dataset:"
/*
#define TITLE   "first part of horizon file to read:"
#define MESSAGE "Type code for reading file    (not needed for RMOD files)\
    (press Help for instructions):"
*/


//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//

     // these private virtual functions override SLDialog.


void VfpopHorizonRead::postManageNotify()
{
  _choice->updateFields();
}



Boolean VfpopHorizonRead::okNotify()
{
  _choice->takeAction(this, FP_OK);
  return FALSE;
}



void VfpopHorizonRead::applyNotify()
{
  _choice->takeAction(this, FP_APPLY);
}



Boolean VfpopHorizonRead::cancelNotify()
{
  _choice->takeAction(this, FP_CANCEL);
  return TRUE;
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


VfpopHorizonRead::VfpopHorizonRead
                                 (SLDelay *slparent, char *name,
                                  VfManager        *manager,
                                  VfHorizons       *horizons,
                                  SLShellContainer *trans,
                                  ContainerList    *clist)
            : SLDialog(slparent, name, NULL, FALSE),
                    _choice     (NULL)
{
  assert(manager && horizons);
  setFallbackResources(defres);

/////// populate work area:

     _horizonio = new VfHorizonio    (0);
          _file = new VfHorizonFile  (manager, horizons, _horizonio,
                                      FileBase::USE_FOR_INPUT);

  SLSmartForm       *work = workArea();
  SLpLabel         *label = new SLpLabel         (work, "label", 0, LABEL);
  VfguiStatus     *status = new VfguiStatus      (work, manager, clist);
  VfguiHorizonInfo  *info = new VfguiHorizonInfo (work, horizons);
  SLPanedWindow   *paned  = new SLPanedWindow    (work, "paned");
                  _choice = new SLFileChoice     (work, "choice",
                                                  SLpFile::_INPUT, _file,
                                                  PROMPT, NULL, FALSE);
  SLpPush          *push  = new SLpPush          (work, "trans");

  push->manageShellWhenPressed(trans);

                    new SLHowReadWrite  (paned, _horizonio, 9);
                    new SLStartingCards (paned, _horizonio, _horizonio, 12);

           //           LEFT     RIGHT    TOP    BOTTOM
  work->attach(status , work   , work ,  work  ,  NULL,   0,   0);
  work->attach(info   , work   , work ,  status,  NULL,   0,   0, 10);
  work->attach(label  , work   , work ,  info  ,  NULL,   0,   0, 10);
  work->attach(_choice, work   , work ,  label ,  NULL,   0,   0, 10);
  work->attach(paned  , work   , work , _choice,  push,   0,   0, 10, 10);
  work->attach(push   , work   , work ,  NULL  ,  work, 170, 170,  0);

/////// populate bottom area:

                    addBottomOK     ();
                    addBottomApply  ();
                    addBottomCancel ();
                    addBottomHelp   ("HORIZON_READ_OVERVIEW");
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopHorizonRead::~VfpopHorizonRead()
{
  delete _horizonio;
  delete _file;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
