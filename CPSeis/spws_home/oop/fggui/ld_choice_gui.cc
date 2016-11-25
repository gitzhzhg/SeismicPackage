
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
//---------------------- ld_choice_gui.cc ------------------------------//
//---------------------- ld_choice_gui.cc ------------------------------//
//---------------------- ld_choice_gui.cc ------------------------------//

//          implementation file for the LdChoiceGui class
//                derived from the SLSmartForm class
//                       subdirectory fggui

                // This class is used to choose which
                // seismic lines will be operated on.


#include "fggui/ld_choice_gui.hh"
#include "geom/field_geometry.hh"
#include "sl/radio_list.hh"
#include "sl/slp_radio.hh"
#include "sl/slp_text.hh"
#include "cprim.h"
#include <iostream.h>
#include <assert.h>


//---------------------- update functions ---------------------//
//---------------------- update functions ---------------------//
//---------------------- update functions ---------------------//

static long active_line_fun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->getActiveLineNumber();
}

static long selected_lines_fun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->numSelectedLines();
}

static long all_lines_fun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return fg->numLines();
}


static long active_line_sense_fun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return (fg->getActiveLineIndex() >= 0);
}

static long selected_lines_sense_fun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return (fg->numSelectedLines() > 0);
}

static long all_lines_sense_fun(void *data)
{
  FieldGeometry *fg = (FieldGeometry*)data;
  return (fg->numLines() > 0);
}



//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//
//--------------------- constructor ------------------------//


LdChoiceGui::LdChoiceGui(SLDelay *slparent, char *name, HelpCtx hctx,
                                               FieldGeometry *fg)
            : SLSmartForm(slparent, name, hctx, TRUE),
                   _fg              (fg),
                   _choice          (ACTIVE_LINE)

{
  assert(_fg);

  RadioList *r = new RadioList();

  SLpRadio *r1 = r->addRadio(this, "r1", ACTIVE_LINE   );
  SLpRadio *r2 = r->addRadio(this, "r2", SELECTED_LINES);
  SLpRadio *r3 = r->addRadio(this, "r3", ALL_LINES     );

  r->setLabel(ACTIVE_LINE   , "operate on active line#");
  r->setLabel(SELECTED_LINES, "operate on selected lines:");
  r->setLabel(ALL_LINES     , "operate on all lines:");

  r->setupIvarPoint(&_choice);  // do not need trap or update function.

  r->setupSenseFun(ACTIVE_LINE   ,    active_line_sense_fun, _fg);
  r->setupSenseFun(SELECTED_LINES, selected_lines_sense_fun, _fg);
  r->setupSenseFun(ALL_LINES     ,      all_lines_sense_fun, _fg);

  SLpText *t1 = new SLpText(this, "t1", 0, SLpText::_LONG, 5);
  SLpText *t2 = new SLpText(this, "t2", 0, SLpText::_LONG, 5);
  SLpText *t3 = new SLpText(this, "t3", 0, SLpText::_LONG, 5);

  t1->showLabelAppearance();
  t2->showLabelAppearance();
  t3->showLabelAppearance();

  t1->setupIvarFun(   active_line_fun, _fg);
  t2->setupIvarFun(selected_lines_fun, _fg);
  t3->setupIvarFun(     all_lines_fun, _fg);

  t1->setupSenseFun(   active_line_sense_fun, _fg);
  t2->setupSenseFun(selected_lines_sense_fun, _fg);
  t3->setupSenseFun(     all_lines_sense_fun, _fg);

    //        LEFT  RIGHT  TOP  BOTTOM
  attach(r1,  this,  t1,   this, NULL,  10);
  attach(r2,  this,  t2,   r1,   NULL,  10);
  attach(r3,  this,  t3,   r2,   this,  10);
  attach(t1,  NULL,  this, this, NULL);
  attach(t2,  NULL,  this, r1,   NULL);
  attach(t3,  NULL,  this, r2,   this);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


LdChoiceGui::~LdChoiceGui()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
