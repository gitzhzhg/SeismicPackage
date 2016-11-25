
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
//---------------------- jd_save_pop.cc ------------------------------//
//---------------------- jd_save_pop.cc ------------------------------//
//---------------------- jd_save_pop.cc ------------------------------//

//             implementation file for the JdSavePop class
//                 derived from the SLDialog class
//                 derived from the FgInform class
//                       subdirectory fggui

      // This dialog box is used to save JD files.


#include "fggui/jd_save_pop.hh"
#include "fggui/fg_message_gui.hh"
#include "fggui/fg_status_gui.hh"
#include "fggui/fg_summary_gui.hh"
#include "fggui/jd_save_action_gui.hh"
#include "geom/field_geometry.hh"
#include "geom/jd_file.hh"
#include "geom/fg_constants.hh"
#include "geom/geomio_wrapper.hh"
#include "sl/sl_encoding_desire.hh"
#include "sl/sl_paned_window.hh"
#include "sl/sl_how_read_write.hh"
#include "sl/sl_starting_cards.hh"
#include "sl/sl_file_choice.hh"
#include "sl/slp_file.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_sep.hh"
#include "sl/slp_push.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_toggle.hh"
#include "sl/slp_option.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>



//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//

     // these private virtual functions override SLDialog.

Boolean JdSavePop::preManageNotify()
{
  _choice->updateFields();
  return TRUE;
}



Boolean JdSavePop::okNotify()
{
  _choice->takeAction(this, FP_OK);
  return FALSE;
}


void JdSavePop::applyNotify()
{
  _choice->takeAction(this, FP_APPLY);
}


Boolean JdSavePop::cancelNotify()
{
  _choice->takeAction(this, FP_CANCEL);
  return TRUE;
}



//---------------- virtual functions overriding VfInform ---------------//
//---------------- virtual functions overriding VfInform ---------------//
//---------------- virtual functions overriding VfInform ---------------//

     // public

void JdSavePop::finishedChanges(FieldGeometry *fg)
{
  _file->updatePjar();
}



//-------------------- static update functions ------------------//
//-------------------- static update functions ------------------//
//-------------------- static update functions ------------------//

          // called from OK and Apply pushbuttons.

static long ok_sense_upfun(void *data)
{
  JdSavePop             *THIS   = (JdSavePop*)data;
  JdFile                *file   = THIS->getFile();
  FileBase::OutputStatus status = file->outputStatus();
  return (status == FileBase::OUTPUT_CREATE ||
          status == FileBase::OUTPUT_OVERWRITE);
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        "*work.custom*height:      2",
            NULL };



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


JdSavePop::JdSavePop(SLDelay *slparent, char *name, HelpCtx hctx,
                                FieldGeometry *fg,
                                FgControlPop  *dcp,
                                ContainerList *clist)
            : SLDialog(slparent, name, hctx, FALSE),
              FgInform(fg),
                    _file       (NULL),
                    _choice     (NULL)
{
  assert(fg && dcp);
  setFallbackResources(defres);
  setTitle("Save JD File");
  _file = new JdFile(fg, JdFile::USE_FOR_OUTPUT);

  GeomioWrapper *geomio  = _file->geomioOutputParameters();
  GeomioWrapper *geomio2 = _file->geomioOutputOverwrite();

/////// populate work area:

  SLSmartForm     *work = workArea();

                _choice = new SLFileChoice (work, "choice",
                               SLpFile::_OUTPUT, _file, "Output JD File:");
  FgMessageGui    *msg  = new FgMessageGui (work, "msg" , fg);
  FgStatusGui     *stat = new FgStatusGui  (work, "stat", fg, clist);
  SLSmartForm     *form = new SLSmartForm  (work, "form");
  FgSummaryGui    *sum1 = new FgSummaryGui (form, "sum1",
                                  "Data in Memory", fg, NULL, NULL);
  JdSaveActionGui  *act = new JdSaveActionGui(form, "act" , _file);
  FgSummaryGui    *sum2 = new FgSummaryGui (form, "sum2",
                            "JD file to Overwrite", NULL, NULL, _file);

//////////// create desire option menu:

  SLDelay *desire = new SLEncodingDesire (work, geomio, "field geometry");

//////////// create customize label:

  SLDelay  *label = new SLpLabel (work, "label", 0,
                          "Pull Down to Customize Field Geometry File Output");

//////////// create customizing tables:

  SLSmartForm  *custom = new SLSmartForm     (work, "custom");
  SLDelay      *paned  = new SLPanedWindow   (custom, "paned");
                         new SLHowReadWrite  (paned, geomio, 1);
                         new SLStartingCards (paned, geomio, geomio2, 1);

//////////// do attachments:

           //             LEFT   RIGHT    TOP    BOTTOM
  custom->attach(paned , custom, custom, custom, custom, 0, 0, 10);

           //           LEFT     RIGHT    TOP    BOTTOM
  form->attach(sum1   , form   , NULL ,  form  ,  form);
  form->attach(act    , sum1   , sum2 ,  form  ,  form, 10, 10);
  form->attach(sum2   , NULL   , work ,  form  ,  form);

           //           LEFT     RIGHT    TOP    BOTTOM
  work->attach(_choice, work   , work ,  work  ,  NULL);
  work->attach(msg    , work   , work , _choice,  NULL);
  work->attach(stat   , work   , work ,  msg   ,  NULL);
  work->attach(form   , work   , work , stat   ,  NULL,  0,  0);
  work->attach(desire , work   , work , form   ,  NULL, 20,  0, 10);
  work->attach(label  , work   , work , desire ,  NULL,  0,  0, 10);
  work->attach(custom , work   , work , label  ,  work,  0,  0, 10);

/////// populate bottom area:

  SLpPush *ok     = addBottomOK    ();
  SLpPush *apply  = addBottomApply ();
                    addBottomCancel();
  SLpPush *cntrl  = addBottomPush  ("Data Control...");
                    addBottomHelp  ("JD_SAVE_FILE");

  ok    ->setupSenseFun          (ok_sense_upfun, this);
  apply ->setupSenseFun          (ok_sense_upfun, this);
  cntrl ->manageShellWhenPressed ((SLShellContainer*)dcp);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


JdSavePop::~JdSavePop()
{
  delete _file;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
