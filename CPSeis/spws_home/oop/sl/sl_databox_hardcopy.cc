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

//---------------------- sl_databox_hardcopy.cc ------------------------//
//---------------------- sl_databox_hardcopy.cc ------------------------//
//---------------------- sl_databox_hardcopy.cc ------------------------//

//        implementation file for the SLDataboxHardcopy class
//                   derived from the SLDialog class
//                           subdirectory sl


#include "sl/sl_databox_hardcopy.hh"
#include "sl/sl_databox.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_push.hh"
#include "sl/slp_option.hh"
#include "sl/slp_label.hh"
#include "wproc.h"
#include "cprim.h"
#include <stream.h>
#include <iostream.h>
#include <assert.h>



//---------------------------- set filename -------------------------//
//---------------------------- set filename -------------------------//
//---------------------------- set filename -------------------------//

     // public.

extern "C"
{
int add_ext(char name[], char ext[], int *istat); // prototype not in wproc.h
}


void SLDataboxHardcopy::setFilename(const char *filename)
{
  strcpy(_filename, filename);
  int istat;
  add_ext(_filename, "hardcopy", &istat);
}



//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//


static void filename_trap(void *data, long /*ident*/,
                          char * /*oldvar*/, char * newvar)
{
  SLDataboxHardcopy *pop = (SLDataboxHardcopy*)data;
  pop->setFilename(newvar);
}



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


static char * filename_upfun(void *data)
{
  SLDataboxHardcopy *pop = (SLDataboxHardcopy*)data;
  return (char*)pop->getFilename();
}



//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//



//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//
//-------------------- overriding virtual functions ---------------//

     // these private virtual functions override SLDialog.

Boolean SLDataboxHardcopy::okNotify()
{
  _databox->saveTable(_filename);
  return TRUE;
}


Boolean SLDataboxHardcopy::cancelNotify()
{
  delayDelete();
  return TRUE;
}



//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//
//--------------------- fallback resources --------------------//


static const char *defres[]= {
      "*background:   goldenrod",
      "*foreground:   brown",
      ".dialogStyle:  DIALOG_FULL_APPLICATION_MODAL",
      NULL };



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


SLDataboxHardcopy::SLDataboxHardcopy(SLDelay *slparent, SLDatabox *databox,
                                     int numlines, const char *filename)
       : SLDialog(slparent, "sl_databox_hardcopy", NULL, TRUE),
                _databox   (databox)
{
  assert(_databox);
  if(filename) strcpy(_filename, filename);
  else         strcpy(_filename, "");
  setTitle("Hardcopy of Table");
  setFallbackResources(defres);

  char label[100];
  sprintf(label, "Hardcopy file will contain about %d lines", numlines);

/////// populate work area:

  SLSmartForm *work = workArea();
  SL2Text     *fn   = new SL2Text  (work, "fn", 0, "filename to save:");
  SLpLabel    *lb   = new SLpLabel (work, "lb", 0, label);

  fn->setCtrap     (filename_trap , this);
  fn->setupCvarFun (filename_upfun, this);

           //      LEFT     RIGHT    TOP    BOTTOM
  work->attach(lb, work   , work ,  work  ,  NULL,  0,  0);
  work->attach(fn, work   , work ,  lb    ,  work,  0,  0, 10);

/////// populate bottom area:

                    addBottomOK     ();
                    addBottomCancel ();
                    addBottomHelp   ("DATABOX_HARDCOPY");

  update();
  makeAndManage();
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


SLDataboxHardcopy::~SLDataboxHardcopy()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
