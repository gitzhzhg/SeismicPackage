
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
//---------------------- survey_save_pop.cc ------------------------------//
//---------------------- survey_save_pop.cc ------------------------------//
//---------------------- survey_save_pop.cc ------------------------------//

//          implementation file for the SurveySavePop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used to save survey files.


#include "fggui/survey_save_pop.hh"
#include "fggui/fg_message_gui.hh"
#include "fggui/fg_status_gui.hh"
#include "fggui/fg_summary_gui.hh"
#include "fggui/survey_table_gui.hh"
#include "fggui/survey_save_gui.hh"
#include "geom/field_geometry.hh"
#include "geom/survey_file.hh"
#include "geom/fg_constants.hh"
#include "sl/sl_file_choice.hh"
#include "sl/slp_file.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_sep.hh"
#include "sl/slp_push.hh"
#include "sl/slp_text.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_toggle.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>



//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//

     // these private virtual functions override SLDialog.


Boolean SurveySavePop::preManageNotify()
{
  _choice->updateFields();
  return TRUE;
}



Boolean SurveySavePop::okNotify()
{
  _choice->takeAction(this, FP_OK);
  return FALSE;
}



void SurveySavePop::applyNotify()
{
  _choice->takeAction(this, FP_APPLY);
}



Boolean SurveySavePop::cancelNotify()
{
  _choice->takeAction(this, FP_CANCEL);
  return TRUE;
}



//-------------------- static update functions ------------------//
//-------------------- static update functions ------------------//
//-------------------- static update functions ------------------//

          // called from OK and Apply pushbuttons.

static long ok_sense_upfun(void *data)
{
  SurveySavePop             *THIS   = (SurveySavePop*)data;
  SurveyFile                *file   = THIS->getFile();
  FileBase::OutputStatus status = file->outputStatus();
  return (status == FileBase::OUTPUT_CREATE ||
          status == FileBase::OUTPUT_OVERWRITE);
}



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


SurveySavePop::SurveySavePop(SLDelay *slparent, char *name, HelpCtx hctx,
                                FieldGeometry *fg,
                                FgControlPop  *dcp,
                                ContainerList *clist)
            : SLDialog(slparent, name, hctx, FALSE),
                    _fg         (fg),
                    _file       (NULL),
                    _choice     (NULL)
{
  assert(_fg && dcp);
  setTitle("Save Survey File");
  _file = new SurveyFile(fg);

/////// populate work area:

  SLSmartForm     *work = workArea();

                _choice = new SLFileChoice (work, "choice",
                               SLpFile::_OUTPUT, _file, "Output Survey File:");
  SurveyTableGui  *tab  = new SurveyTableGui(work, "tab" , _file, FALSE);
  FgMessageGui    *msg  = new FgMessageGui  (work, "msg" , _fg);
  FgStatusGui     *stat = new FgStatusGui   (work, "stat", _fg, clist);
  FgSummaryGui    *sum1 = new FgSummaryGui  (work, "sum1",
                                  "Data in Memory", _fg, NULL, NULL);
  SurveySaveGui   *act  = new SurveySaveGui (work, "act" , _fg, _file);

           //           LEFT     RIGHT    TOP    BOTTOM
  work->attach(_choice, work   , work ,  work  ,  NULL);
  work->attach(tab    , work   , work , _choice,  NULL,  0,  0, 10);
  work->attach(msg    , work   , work ,  tab   ,  NULL);
  work->attach(stat   , work   , work ,  msg   ,  NULL);
  work->attach(sum1   , work   , NULL ,  stat  ,  work);
  work->attach(act    , sum1   , work ,  stat  ,  work, 10);

/////// populate bottom area:

  SLpPush *ok     = addBottomOK    ();
  SLpPush *apply  = addBottomApply ();
                    addBottomCancel();
  SLpPush *cntrl  = addBottomPush  ("Data Control...");
                    addBottomHelp  ("SURVEY_SAVE_FILE");

  ok    ->setupSenseFun          (ok_sense_upfun, this);
  apply ->setupSenseFun          (ok_sense_upfun, this);
  cntrl ->manageShellWhenPressed ((SLShellContainer*)dcp);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


SurveySavePop::~SurveySavePop()
{
  delete _file;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
