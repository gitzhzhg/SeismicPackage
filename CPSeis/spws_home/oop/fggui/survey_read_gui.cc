
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
//---------------------- survey_read_gui.cc ------------------------//
//---------------------- survey_read_gui.cc ------------------------//
//---------------------- survey_read_gui.cc ------------------------//

//         implementation file for the SurveyReadGui class
//               derived from the SLSmartForm class
//                       subdirectory fggui


#include "fggui/survey_read_gui.hh"
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


#define TRAP(tog_trap, long2, set_function)                \
static void tog_trap(void *data, long /*ident*/,           \
                     long2 /*oldvar*/, long2 newvar)       \
{                                                          \
  SurveyReadGui *gui  = (SurveyReadGui*)data;              \
  SurveyFile    *file = gui->getFile();                    \
  file->set_function;                                      \
}


TRAP(txt1_trap,  long, setDefaultLineNumber   (newvar))
TRAP( add_trap,  long, setAddOption      ((int)newvar))
TRAP(txt2_trap,  long, setLineNumberToAdd     (newvar))
TRAP( rep_trap,  long, setReplaceOption  ((int)newvar))
TRAP(  sm_trap, float, setShotpointMultiply   (newvar))
TRAP(  xm_trap, float, setXcoordMultiply      (newvar))
TRAP(  ym_trap, float, setYcoordMultiply      (newvar))
TRAP(  sa_trap, float, setShotpointAdd        (newvar))
TRAP(  xa_trap, float, setXcoordAdd           (newvar))
TRAP(  ya_trap, float, setYcoordAdd           (newvar))



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


static char *lab_upfun(void *data)
{                                
  static char *buf1 = "survey file contains line numbers";
  static char *buf2 = "survey file does not contain line numbers";
  SurveyReadGui *gui  = (SurveyReadGui*)data;
  SurveyFile    *file = gui->getFile();
  int whether = file->inputFileContainsLineNumbers();
  if(whether) return buf1;
  return buf2;
}


#define UPFUN(tog_upfun, long2, fileContainsLineNumbers)   \
static long2 tog_upfun(void *data)                         \
{                                                          \
  SurveyReadGui *gui  = (SurveyReadGui*)data;              \
  SurveyFile    *file = gui->getFile();                    \
  return file->fileContainsLineNumbers();                  \
}

UPFUN(txt1_upfun,  long, getDefaultLineNumber)
UPFUN( add_upfun,  long, getAddOption)
UPFUN(txt2_upfun,  long, getLineNumberToAdd)
UPFUN( rep_upfun,  long, getReplaceOption)
UPFUN(  sm_upfun, float, getShotpointMultiply)
UPFUN(  xm_upfun, float, getXcoordMultiply)
UPFUN(  ym_upfun, float, getYcoordMultiply)
UPFUN(  sa_upfun, float, getShotpointAdd)
UPFUN(  xa_upfun, float, getXcoordAdd)
UPFUN(  ya_upfun, float, getYcoordAdd)



//------------------ sense update functions ----------------------//
//------------------ sense update functions ----------------------//
//------------------ sense update functions ----------------------//


static long simple_sensefun(void *data)     
{                                   
  SurveyReadGui *gui  = (SurveyReadGui*)data;
  SurveyFile    *file = gui->getFile();
  return (file->inputStatus() == FileBase::INPUT_VALID);
}


static long txt1_sensefun(void *data)     
{                                   
  SurveyReadGui *gui  = (SurveyReadGui*)data;
  SurveyFile    *file = gui->getFile();
  if(file->inputFileContainsLineNumbers()) return FALSE;
  return (file->inputStatus() == FileBase::INPUT_VALID);
}


static long txt2_sensefun(void *data)     
{                                   
  SurveyReadGui *gui  = (SurveyReadGui*)data;
  SurveyFile    *file = gui->getFile();
  if(file->getAddOption() != SurveyFile::ADD_SPECIFIED_NEW_LINE) return FALSE;
  return (file->inputStatus() == FileBase::INPUT_VALID);
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


SurveyReadGui::SurveyReadGui(SLDelay *slparent, char *name,
                                 FieldGeometry *fg, SurveyFile *file)
       : SLSmartForm(slparent, name, NULL, TRUE),
               _fg           (fg),
               _file         (file)
{
  assert(_fg && _file);

  SLpLabel  *lab  = new SLpLabel (this, "lab");

  SL2Text   *txt1 = new SL2Text (this, "txt", 0,
                "line number to use:", SLpText::_LONG, 6);

  SLSep     *sep1 = new SLSep(this, "sep");

  SLpOption *add = new SLpOption(this, "add", 0, "");
  add->addOption("add", SurveyFile::ADD_ALL_NEW_LINES,
                                   "add all new lines");
  add->addOption("add", SurveyFile::ADD_SPECIFIED_NEW_LINE,
                                   "add specified new line");
  add->addOption("add", SurveyFile::SKIP_ALL_NEW_LINES,
                                   "skip all new lines");

  SL2Text   *txt2 = new SL2Text (this, "txt", 0,
                "specified new line to add:", SLpText::_LONG, 6);

  SLSep     *sep2 = new SLSep(this, "sep");

  SLpOption *rep = new SLpOption(this, "rep", 0, "");
  rep->addOption("rep", SurveyFile::REPLACE_ALL_MATCHING_LINES,
                                   "replace all matching lines");
  rep->addOption("rep", SurveyFile::REPLACE_SELECTED_MATCHING_LINES,
                                   "replace selected matching lines");
  rep->addOption("rep", SurveyFile::SKIP_ALL_MATCHING_LINES,
                                   "skip all matching lines");

  SLSep     *sep3 = new SLSep(this, "sep");

  SLpText   *st = new SLpText (this, "txt", 0, SLpText::_CHAR, 11);
  SLpText   *xt = new SLpText (this, "txt", 0, SLpText::_CHAR, 11);
  SLpText   *yt = new SLpText (this, "txt", 0, SLpText::_CHAR, 11);
  st->showLabelAppearance();
  xt->showLabelAppearance();
  yt->showLabelAppearance();
  st->setCvar("shotpoints:");
  xt->setCvar("X coords:");
  yt->setCvar("Y coords:");

  SLpText   *mm = new SLpText (this, "txt", 0, SLpText::_CHAR, 11);
  SLpText   *aa = new SLpText (this, "txt", 0, SLpText::_CHAR, 11);
  mm->showLabelAppearance();
  aa->showLabelAppearance();
  mm->setCvar("multiply by");
  aa->setCvar("then add   ");

  SLpText   *sm = new SLpText (this, "txt", 0, SLpText::_FLOAT, 11, 5);
  SLpText   *xm = new SLpText (this, "txt", 0, SLpText::_FLOAT, 11, 5);
  SLpText   *ym = new SLpText (this, "txt", 0, SLpText::_FLOAT, 11, 5);
  SLpText   *sa = new SLpText (this, "txt", 0, SLpText::_FLOAT, 11, 5);
  SLpText   *xa = new SLpText (this, "txt", 0, SLpText::_FLOAT, 11, 5);
  SLpText   *ya = new SLpText (this, "txt", 0, SLpText::_FLOAT, 11, 5);

  txt1->setItrap(txt1_trap, this);
  add ->setItrap( add_trap, this);
  txt2->setItrap(txt2_trap, this);
  rep ->setItrap( rep_trap, this);
  sm  ->setFtrap(  sm_trap, this);
  xm  ->setFtrap(  xm_trap, this);
  ym  ->setFtrap(  ym_trap, this);
  sa  ->setFtrap(  sa_trap, this);
  xa  ->setFtrap(  xa_trap, this);
  ya  ->setFtrap(  ya_trap, this);

  lab ->setupCvarFun( lab_upfun, this);
  txt1->setupIvarFun(txt1_upfun, this);
  add ->setupIvarFun( add_upfun, this);
  txt2->setupIvarFun(txt2_upfun, this);
  rep ->setupIvarFun( rep_upfun, this);
  sm  ->setupFvarFun(  sm_upfun, this);
  xm  ->setupFvarFun(  xm_upfun, this);
  ym  ->setupFvarFun(  ym_upfun, this);
  sa  ->setupFvarFun(  sa_upfun, this);
  xa  ->setupFvarFun(  xa_upfun, this);
  ya  ->setupFvarFun(  ya_upfun, this);

  lab ->setupSenseFun( simple_sensefun, this);
  txt1->setupSenseFun(   txt1_sensefun, this);
  add ->setupSenseFun( simple_sensefun, this);
  txt2->setupSenseFun(   txt2_sensefun, this);
  rep ->setupSenseFun( simple_sensefun, this);
  mm  ->setupSenseFun( simple_sensefun, this);
  aa  ->setupSenseFun( simple_sensefun, this);
  st  ->setupSenseFun( simple_sensefun, this);
  xt  ->setupSenseFun( simple_sensefun, this);
  yt  ->setupSenseFun( simple_sensefun, this);
  sm  ->setupSenseFun( simple_sensefun, this);
  xm  ->setupSenseFun( simple_sensefun, this);
  ym  ->setupSenseFun( simple_sensefun, this);
  sa  ->setupSenseFun( simple_sensefun, this);
  xa  ->setupSenseFun( simple_sensefun, this);
  ya  ->setupSenseFun( simple_sensefun, this);

     //           mm  aa
     //     st    sm  sa
     //     xt    xm  xa
     //     yt    ym  ya

//                  LEFT   RIGHT   TOP      BOTTOM
  attach(lab      , this , NULL  , this    , NULL,  0,  0, 5);
  attach(txt1     , this , this  , lab     , NULL, 60, 40);
  attach(sep1     , this , this  , txt1    , NULL,  0,  0, 5);
  attach(add      , this , NULL  , sep1    , NULL,  0,  0, 5);
  attach(txt2     , this , this  , add     , NULL, 60, 40);
  attach(sep2     , this , this  , txt2    , NULL,  0,  0, 5);
  attach(rep      , this , NULL  , sep2    , NULL,  0,  0, 5);
  attach(sep3     , this , this  , rep     , NULL,  0,  0, 5);
  attach(mm       , NULL , aa    , sep3    , NULL,  0,  5, 5);
  attach(aa       , NULL , this  , sep3    , NULL,  0,  5, 5);
  attach(st       , this , mm    , mm      , NULL,  5,  5);
  attach(sm       , NULL , aa    , mm      , NULL,  0,  5);
  attach(sa       , NULL , this  , mm      , NULL,  0,  5);
  attach(xt       , this , mm    , sm      , NULL,  5,  5);
  attach(xm       , NULL , aa    , sm      , NULL,  0,  5);
  attach(xa       , NULL , this  , sm      , NULL,  0,  5);
  attach(yt       , this , mm    , xm      , this,  5,  5, 0, 5);
  attach(ym       , NULL , aa    , xm      , this,  0,  5, 0, 5);
  attach(ya       , NULL , this  , xm      , this,  0,  5, 0, 5);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


SurveyReadGui::~SurveyReadGui()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

