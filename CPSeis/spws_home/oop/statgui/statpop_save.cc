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

//---------------------- statpop_save.cc ------------------------------//
//---------------------- statpop_save.cc ------------------------------//
//---------------------- statpop_save.cc ------------------------------//

//          implementation file for the StatpopSave class
//                 derived from the SLDialog class
//               derived from the StaticInform class
//                       subdirectory statgui

      // This dialog box is used to save CPS static files.


#include "statgui/statpop_save.hh"
#include "statgui/statgui_status.hh"
#include "statgui/statgui_header.hh"
#include "stat/static_file_base.hh"
#include "stat/statio_wrapper.hh"
#include "sl/sl_encoding_desire.hh"
#include "sl/sl_how_read_write.hh"
#include "sl/sl_starting_cards.hh"
#include "sl/sl_file_choice.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_paned_window.hh"
#include "sl/slp_file.hh"
#include "sl/slp_push.hh"
#include "sl/slp_label.hh"
#include "sl/slp_option.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//-------------------- virtual functions overriding SLDialog ---------------//
//-------------------- virtual functions overriding SLDialog ---------------//
//-------------------- virtual functions overriding SLDialog ---------------//

     // private


Boolean StatpopSave::preManageNotify()
{
  _choice->updateFields();
  return TRUE;
}



void StatpopSave::postManageNotify()
{
  _file->updatePjarFromKernal (TRUE);
}



Boolean StatpopSave::okNotify()
{
  _choice->takeAction(this, FP_OK);
  return FALSE;
}



void StatpopSave::applyNotify()
{
  _choice->takeAction(this, FP_APPLY);
}



Boolean StatpopSave::cancelNotify()
{
  _choice->takeAction(this, FP_CANCEL);
  return TRUE;
}



//---------------- virtual functions overriding StaticInform ---------------//
//---------------- virtual functions overriding StaticInform ---------------//
//---------------- virtual functions overriding StaticInform ---------------//

     // public


void StatpopSave::afterChanges()
{
  _file->updatePjarFromKernal (TRUE);
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
        "*choice.borderWidth:            2",
        "*choice*background:             gray80",
            NULL };



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


StatpopSave::StatpopSave(SLDelay *slparent, char *name,
                                StaticManager *manager,
                                ContainerList *clist)
            : SLDialog(slparent, name, NULL, FALSE),
              StaticInform(manager),
                    _manager    (manager),
                    _statio     (NULL),
                    _statio2    (NULL),
                    _file       (NULL),
                    _choice     (NULL)
{
//////////// create non gui objects:

  assert(manager);
  setFallbackResources(defres);

  _statio  = new StatioWrapper(1);  // for writing the file.
  _statio2 = new StatioWrapper(1);  // for validating a file to be overwritten.

  _statio->initializeOutputParameters();
//_statio->setEncoding("oldcps");            // removed 2003-09-23

  _file = new StaticFileBase(manager, NULL, _statio, _statio2);

//////////// populate work area:

  SLSmartForm     *work = workArea();
  StatguiStatus *status = new StatguiStatus (work, manager, clist);

  _choice = new SLFileChoice (work, "choice", SLpFile::_OUTPUT, _file,
                              "Output Static File", NULL, FALSE);

//////////// create and populate a container:

  SLDelay      *contain = new SLSmartForm   (work, "contain");

  StatguiHeader   *sum1 = new StatguiHeader (contain, NULL,  manager,
                                             "Active Dataset in Memory",
                                             StatguiHeader::NOT_EDITABLE,
                                             StatguiHeader::ACTIVE);

  StatguiHeader   *sum2 = new StatguiHeader (contain, NULL, NULL,
                                             "Static File to Overwrite",
                                             StatguiHeader::NOT_EDITABLE,
                                             StatguiHeader::VALIDATE,
                                             NULL, _statio2);

  SLpLabel       *space = new SLpLabel      (contain, " ");

//////////// create desire option menu:

  SLDelay *desire = new SLEncodingDesire (work, _statio, "static");

//////////// create customize label:

  SLDelay  *label = new SLpLabel (work, "label", 0,
                               "Pull Down to Customize Static File Output");

//////////// create customizing tables:

  SLPanedWindow *paned = new SLPanedWindow   (work, "paned");
                         new SLHowReadWrite  (paned, _statio, 1);
                         new SLStartingCards (paned, _statio, _statio2, 1);

//////////// do attachments:

           //           LEFT     RIGHT    TOP      BOTTOM
  work->attach(sum1   , contain, NULL   , contain, contain);
  work->attach(space  , sum1   , sum2   , NULL   , contain, 10, 10, 0, 0);
  work->attach(sum2   , NULL   , contain, contain, contain);

           //           LEFT     RIGHT    TOP      BOTTOM
  work->attach(status , work   , work   ,  work  , NULL, 20, 20);
  work->attach(desire , work   , work   ,  status, NULL,  0,  0, 10);
  work->attach(_choice, work   , work   ,  desire, NULL,  0,  0, 10);
  work->attach(contain, work   , work   , _choice, NULL,  0,  0, 10);
  work->attach(label  , work   , work   , contain, NULL,  0,  0, 10);
  work->attach(paned  , work   , work   , label  , work,  0,  0, 10);

//////////// populate bottom area:

                    addBottomOK    ();
                    addBottomApply ();
                    addBottomCancel();
                    addBottomHelp  ("SAVE_STATIC_FILE");
  update();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


StatpopSave::~StatpopSave()
{
  delete _file;
  delete _statio;
  delete _statio2;
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
