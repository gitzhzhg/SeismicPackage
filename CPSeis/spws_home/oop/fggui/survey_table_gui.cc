
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
//---------------------- survey_table_gui.cc -----------------------//
//---------------------- survey_table_gui.cc -----------------------//
//---------------------- survey_table_gui.cc -----------------------//

//          implementation file for the SurveyTableGui class
//                derived from the SLSmartForm class
//                       subdirectory fggui


#include "fggui/survey_table_gui.hh"
#include "geom/survey_file.hh"
#include "sl/slp_label.hh"
#include "sl/slp_text.hh"
#include "wbox.h"
#include "cprim.h"
#include <iostream.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//
//---------------- variable update functions --------------------//


#define UPFUN(text_upfun, index)                    \
static char *text_upfun(void *data)                 \
{                                                   \
  SurveyTableGui *table = (SurveyTableGui*)data;    \
  SurveyFile     *file  = table->getSurveyFile();   \
  if(table->displayingInputFile())                  \
         return (char*)file->inputCard(index);      \
  return (char*)file->outputCard(index);            \
}

UPFUN(text1_upfun, 0)
UPFUN(text2_upfun, 1)
UPFUN(text3_upfun, 2)
UPFUN(text4_upfun, 3)
UPFUN(text5_upfun, 4)



//------------------ sense update functions -----------------//
//------------------ sense update functions -----------------//
//------------------ sense update functions -----------------//

static long label_sensefun(void *data)
{
  SurveyTableGui *table = (SurveyTableGui*)data;
  SurveyFile     *file  = table->getSurveyFile();
  if(table->displayingInputFile())
         return (file->inputStatus() == FileBase::INPUT_VALID);
  return (file->outputStatus() == FileBase::OUTPUT_OVERWRITE);
}



//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//
//-------------------- get any widget from SLDelay --------------//

      // SLDelay does not have to be made

static Widget get_any_widget(SLDelay *gui)
{
  assert(gui);
  if(gui->W      ()) return gui->W      ();
  if(gui->wParent()) return gui->wParent();
  return get_any_widget(gui->slParent());
}



//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//
//---------------------- constructor -----------------------//


SurveyTableGui::SurveyTableGui(SLDelay *slparent, char *name,
                   SurveyFile *file, int displaying_input_file)
           : SLSmartForm(slparent, name, NULL, TRUE),
                 _file                   (file),
                 _displaying_input_file  (displaying_input_file)
{
  assert(_file);

  int num = _file->numCardsRetained();
  assert(num >= 5);

  static char *defres[] = { "*fontList: fixed", NULL };
  Widget w = get_any_widget(slparent);
  setDefRes(XtDisplay(w), "survey_table_gui_text", defres);

  SLpLabel *label = new SLpLabel(this, "label");
  if(_displaying_input_file)
       label->setCvar("First Few Lines in File to be Read");
  else label->setCvar("First Few Lines in File to be Overwritten");
  SLpText *text1 = new SLpText(this, "survey_table_gui_text");
  SLpText *text2 = new SLpText(this, "survey_table_gui_text");
  SLpText *text3 = new SLpText(this, "survey_table_gui_text");
  SLpText *text4 = new SLpText(this, "survey_table_gui_text");
  SLpText *text5 = new SLpText(this, "survey_table_gui_text");
  label->setupSenseFun(label_sensefun, this);
  text1->showLabelAppearance();
  text2->showLabelAppearance();
  text3->showLabelAppearance();
  text4->showLabelAppearance();
  text5->showLabelAppearance();
  text1->setupCvarFun(text1_upfun, this);
  text2->setupCvarFun(text2_upfun, this);
  text3->setupCvarFun(text3_upfun, this);
  text4->setupCvarFun(text4_upfun, this);
  text5->setupCvarFun(text5_upfun, this);
  attach(label, this, this, this , NULL);
  attach(text1, this, this, label, NULL);
  attach(text2, this, this, text1, NULL);
  attach(text3, this, this, text2, NULL);
  attach(text4, this, this, text3, NULL);
  attach(text5, this, this, text4, this);
}


//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//
//------------------------ destructor --------------------------//

SurveyTableGui::~SurveyTableGui()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
