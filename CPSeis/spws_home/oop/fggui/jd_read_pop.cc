
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
//---------------------- jd_read_pop.cc ------------------------------//
//---------------------- jd_read_pop.cc ------------------------------//
//---------------------- jd_read_pop.cc ------------------------------//

//             implementation file for the JdReadPop class
//                 derived from the SLDialog class
//                       subdirectory fggui

      // This dialog box is used to read JD files.


#include "fggui/jd_read_pop.hh"
#include "fggui/fg_message_gui.hh"
#include "fggui/fg_status_gui.hh"
#include "fggui/fg_summary_gui.hh"
#include "fggui/jd_read_action_gui.hh"
#include "fggui/jd_read_box.hh"
#include "geom/field_geometry.hh"
#include "geom/jd_file.hh"
#include "geom/fg_constants.hh"
#include "geom/geomio_wrapper.hh"
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


Boolean JdReadPop::preManageNotify()
{
  _choice->updateFields();
  return TRUE;
}



Boolean JdReadPop::okNotify()
{
  _choice->takeAction(this, FP_OK);
  return FALSE;
}



void JdReadPop::applyNotify()
{
  _choice->takeAction(this, FP_APPLY);
}



Boolean JdReadPop::cancelNotify()
{
  _choice->takeAction(this, FP_CANCEL);
  return TRUE;
}



//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//
//------------------------- static functions -----------------------//

          // called from OK and Apply pushbuttons.

static long ok_sense_upfun(void *data)
{
  JdReadPop            *THIS   = (JdReadPop*)data;
  JdFile               *file   = THIS->getFile();
  FileBase::InputStatus status = file->inputStatus();
  return (status == FileBase::INPUT_VALID);
}


static void repsel_trap(void *data, long /*ident*/,
                           long /*oldvar*/, long newvar)
{
  JdReadPop     *THIS = (JdReadPop*)data;
  JdFile        *file = THIS->getFile();
  if     (newvar == 2) file->replaceSelectedMatchingLinesOrFlags(TRUE);
  else if(newvar == 3) file->replaceAnyMatchingLinesOrFlags     (TRUE);
  else                 file->replaceAnyMatchingLinesOrFlags     (FALSE);
}



static long repsel_upfun(void *data)
{
  JdReadPop     *THIS = (JdReadPop*)data;
  JdFile        *file = THIS->getFile();
  if(file->replaceSelectedMatchingLinesOrFlags()) return 2;
  if(file->replaceAnyMatchingLinesOrFlags     ()) return 3;
  return 1;
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[] = {
        "*work.custom*height:      2",
            NULL };


//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


JdReadPop::JdReadPop(SLDelay *slparent, char *name, HelpCtx hctx,
                                FieldGeometry *fg,
                                FgControlPop  *dcp,
                                ContainerList *clist)
            : SLDialog(slparent, name, hctx, FALSE),
                    _fg         (fg),
                    _file       (NULL),
                    _choice     (NULL)
{
  assert(_fg && dcp);
  setFallbackResources(defres);

  setTitle("Read JD File");
  _file = new JdFile(fg, JdFile::USE_FOR_INPUT);

  GeomioWrapper *geomio = _file->geomioInput();

/////// populate work area:

  SLSmartForm     *work = workArea();

                _choice = new SLFileChoice (work, "choice",
                                SLpFile::_INPUT, _file, "Input JD File...");
  FgMessageGui    *msg  = new FgMessageGui (work, "msg" , _fg);
  FgStatusGui     *stat = new FgStatusGui  (work, "stat", _fg, clist);
  SLSmartForm     *form = new SLSmartForm  (work, "form");
  FgSummaryGui    *sum1 = new FgSummaryGui (form, "sum1",
                                 "JD file to Read", NULL, _file, NULL);
  JdReadActionGui  *act = new JdReadActionGui(form, "act" , _file);
  FgSummaryGui    *sum2 = new FgSummaryGui (form, "sum2",
                                  "Data in Memory", _fg, NULL, NULL);

  SLpOption *opt  = new SLpOption (work, "opt", 0, "");
  opt->addOption("one", 1, "normal read operation on LD cards");
  opt->addOption("two", 2,
            "ONLY use LD cards which replace SELECTED matching lines or flags");
  opt->addOption("three", 3,
            "ONLY use LD cards which replace ANY matching lines or flags");
  opt->setItrap    (repsel_trap, this);
  opt->setupIvarFun(repsel_upfun, this);

//////////// create customize label:

  SLDelay  *label = new SLpLabel (work, "label", 0,
                          "Pull Down to Customize Field Geometry File Input");

//////////// create customizing tables:

  SLSmartForm  *custom = new SLSmartForm     (work, "custom");
  SLDelay      *tab1   = new JdReadBox       (custom, _file);
  SLDelay      *paned  = new SLPanedWindow   (custom, "paned");
                         new SLHowReadWrite  (paned, geomio, 1);
                         new SLStartingCards (paned, geomio, geomio, 1);

//////////// do attachments:

           //             LEFT   RIGHT    TOP    BOTTOM
  custom->attach(tab1  , custom, custom, custom, NULL  , 0, 0,  5);
  custom->attach(paned , custom, custom, tab1  , custom, 0, 0, 10);

           //           LEFT     RIGHT    TOP    BOTTOM
  form->attach(sum1   , form   , NULL ,  form  ,  form);
  form->attach(act    , sum1   , sum2 ,  form  ,  form, 10, 10);
  form->attach(sum2   , NULL   , work ,  form  ,  form);

           //           LEFT     RIGHT    TOP    BOTTOM
  work->attach(_choice, work   , work ,  work  ,  NULL);
  work->attach(msg    , work   , work , _choice,  NULL);
  work->attach(stat   , work   , work ,  msg   ,  NULL);
  work->attach(form   , work   , work , stat   ,  NULL,  0,  0);
  work->attach(opt    , work   , work , form   ,  NULL, 20,  0, 10);
  work->attach(label  , work   , work , opt    ,  NULL,  0,  0, 10);
  work->attach(custom , work   , work , label  ,  work,  0,  0, 10);

/////// populate bottom area:

  SLpPush *ok     = addBottomOK    ();
  SLpPush *apply  = addBottomApply ();
                    addBottomCancel();
  SLpPush *cntrl  = addBottomPush  ("Data Control...");
                    addBottomHelp  ("JD_READ_FILE");

  ok    ->setupSenseFun          (ok_sense_upfun, this);
  apply ->setupSenseFun          (ok_sense_upfun, this);
  cntrl ->manageShellWhenPressed ((SLShellContainer*)dcp);

  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


JdReadPop::~JdReadPop()
{
  delete _file;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
