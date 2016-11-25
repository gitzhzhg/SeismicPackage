
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
//---------------------- survey_save_gui.cc ------------------------//
//---------------------- survey_save_gui.cc ------------------------//
//---------------------- survey_save_gui.cc ------------------------//

//         implementation file for the SurveySaveGui class
//               derived from the SLSmartForm class
//                       subdirectory fggui


#include "fggui/survey_save_gui.hh"
#include "geom/survey_file.hh"
#include "geom/field_geometry.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_text.hh"
#include "sl/slp_option.hh"
#include "sl/slp_label.hh"
#include "sl/sl_sep.hh"
#include "cprim.h"
#include <stdlib.h>
#include <stdio.h>
#include <iostream.h>
#include <string.h>
#include <assert.h>



//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//


static void save_trap(void *data, long /*ident*/,
                     long /*oldvar*/, long newvar)
{                                          
  SurveySaveGui *gui  = (SurveySaveGui*)data;
  SurveyFile    *file = gui->getFile();
  file->setSaveOption((int)newvar);
}



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


static long save_upfun(void *data)
{                                
  SurveySaveGui *gui  = (SurveySaveGui*)data;
  SurveyFile    *file = gui->getFile();
  return file->getSaveOption();
}



//------------------ sense update functions ----------------------//
//------------------ sense update functions ----------------------//
//------------------ sense update functions ----------------------//


static long simple_sensefun(void *data)     
{                                   
  SurveySaveGui *gui  = (SurveySaveGui*)data;
  SurveyFile    *file = gui->getFile();
  return (file->outputStatus() != FileBase::OUTPUT_INVALID);
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


SurveySaveGui::SurveySaveGui(SLDelay *slparent, char *name,
                                 FieldGeometry *fg, SurveyFile *file)
       : SLSmartForm(slparent, name, NULL, TRUE),
               _fg           (fg),
               _file         (file)
{
  assert(_fg && _file);

  SLpOption *save = new SLpOption(this, "save", 0, "");
  save->addOption("save", SurveyFile::SAVE_ALL_LINES,
                                     "save all lines");
  save->addOption("save", SurveyFile::SAVE_SELECTED_LINES,
                                     "save selected lines");

  save->setItrap     (save_trap      , this);
  save->setupIvarFun (save_upfun     , this);
  save->setupSenseFun(simple_sensefun, this);

//                  LEFT   RIGHT   TOP      BOTTOM
  attach(save     , this , NULL  , this    , this,  0,  0, 5, 5);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


SurveySaveGui::~SurveySaveGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

