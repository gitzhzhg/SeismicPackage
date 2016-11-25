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

//------------------------- vfpop_multiply.cc ---------------------------//
//------------------------- vfpop_multiply.cc ---------------------------//
//------------------------- vfpop_multiply.cc ---------------------------//

//          implementation file for the VfpopMultiply class
//               derived from the VfpopEditBase class
//                       subdirectory vfgui


#include "vfgui/vfpop_multiply.hh"
#include "vf/vf_edit_multiply.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_utilities.hh"
#include "vf/vf_constants.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl_sep.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_push.hh"
#include "sl/slp_option.hh"
#include "sl/slp_label.hh"
#include <stream.h>
#include <assert.h>



//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//


static void choice_trap(void *data, long /*ident*/,
                     long /*oldvar*/, long newvar)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  edit->setChoice((int)newvar);
}


static void type_trap(void *data, long /*ident*/,
                     long /*oldvar*/, long newvar)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  edit->setType((int)newvar-1);
}


static void constant_trap(void *data, long /*ident*/,
                     float /*oldvar*/, float newvar)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  edit->setConstant(newvar);
}


static void time1_trap(void *data, long /*ident*/,
                     float /*oldvar*/, float newvar)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  edit->setMinTime(newvar);
}


static void time2_trap(void *data, long /*ident*/,
                     float /*oldvar*/, float newvar)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  edit->setMaxTime(newvar);
}


static void depth1_trap(void *data, long /*ident*/,
                     float /*oldvar*/, float newvar)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  edit->setMinDepth(newvar);
}


static void depth2_trap(void *data, long /*ident*/,
                     float /*oldvar*/, float newvar)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  edit->setMaxDepth(newvar);
}



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


static long choice_upfun(void *data)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  return edit->getChoice();
}


static long nfun_upfun(void *data)
{
  VfManager *manager = (VfManager*)data;
  return manager->activeDataset()->numVelocityFunctions();
}


static long nsel_upfun(void *data)
{
  VfManager *manager = (VfManager*)data;
  return manager->activeDataset()->numSelectedVelocityFunctions();
}


static long act_upfun(void *data)
{
  VfManager *manager = (VfManager*)data;
  return manager->activeDataset()->getActiveVelocityFunction() + 1;
}


static long type_upfun(void *data)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  return edit->getType()+1;
}


static float constant_upfun(void *data)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  return edit->getConstant();
}


static float time1_upfun(void *data)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  return edit->getMinTime();
}


static float time2_upfun(void *data)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  return edit->getMaxTime();
}


static float depth1_upfun(void *data)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  return edit->getMinDepth();
}


static float depth2_upfun(void *data)
{
  VfEditMultiply *edit = (VfEditMultiply*)data;
  return edit->getMaxDepth();
}


static char *sel_fun(void *data)
{
  static char buffer[88];
  VfManager *manager = (VfManager*)data;
  sprintf(buffer, "choose (%d) SELECTED velocity functions in active dataset",
             manager->activeDataset()->numSelectedVelocityFunctions());
  return buffer;
}


static char *all_fun(void *data)
{
  static char buffer[88];
  VfManager *manager = (VfManager*)data;
  sprintf(buffer, "choose ALL (%d) velocity functions in active dataset",
             manager->activeDataset()->numVelocityFunctions());
  return buffer;
}


static char *act_fun(void *data)
{
  static char buffer[88];
  VfManager *manager = (VfManager*)data;
  sprintf(buffer, "choose ACTIVE velocity function (#%d) in active dataset",
             manager->activeDataset()->getActiveVelocityFunction()+1);
  return buffer;
}



//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//


static long time_sense(void *data)
{
  VfpopMultiply  *pop  = (VfpopMultiply*)data;
  VfEditMultiply *edit = (VfEditMultiply*)pop->edit();
  return pop->manager()->utilities()->abscissaIsTime(edit->getType());
}


static long depth_sense(void *data)
{
  VfpopMultiply  *pop  = (VfpopMultiply*)data;
  VfEditMultiply *edit = (VfEditMultiply*)pop->edit();
  return pop->manager()->utilities()->abscissaIsDepth(edit->getType());
}



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


VfpopMultiply::VfpopMultiply(SLDelay *slparent, char *name,
                   VfManager *manager, ContainerList *clist)
       : VfpopEditBase(slparent, name, manager, clist, "TYPES_OVERVIEW")
{
  _edit = new VfEditMultiply();

  SLSmartForm *row1 = new SLSmartForm(_editor, "row1", NULL);
  SLSmartForm *row2 = new SLSmartForm(_editor, "row2", NULL);
  SLSmartForm *row3 = new SLSmartForm(_editor, "row3", NULL);
  SLSep       *sep  = new SLSep      (_editor, "sep");

  row1->showEvenSpacing();
  row2->showEvenSpacing();
  row3->showEvenSpacing();

  SLpOption *choice = new SLpOption(row1, "choice", 0, "");
  choice->addOption("choice", VfEditMultiply::CHOICE_SELECTED,
              "choose SELECTED velocity functions in active dataset");
  choice->addOption("choice", VfEditMultiply::CHOICE_ALL,
              "choose ALL velocity functions in active dataset");
  choice->addOption("choice", VfEditMultiply::CHOICE_ACTIVE,
              "choose ACTIVE velocity function in active dataset");

  SL2Text *nfun  = new SL2Text (row2, "nfun", 0,
                           "#functions in active dataset:", SLpText::_LONG, 6);
  SL2Text *nsel  = new SL2Text (row2, "nsel", 0,
                           "#selected:", SLpText::_LONG, 6);
  SL2Text *act   = new SL2Text (row2, "act" , 0,
                           "active function:", SLpText::_LONG, 6);

  SLpOption *type = new SLpOption(row3, "type", 0, "type of function:");
  for(int i = FIRSTTYPE; i <= LASTTYPE; i++)
      {
      if(!manager->utilities()->abscissaIsThickness(i))
          type->addOption("type", i+1,
                          (char*)_manager->utilities()->typeDescription(i));
      }

  SL2Text *constant   = new SL2Text (_editor, "xxx" , 0,
               "constant to multiply velocities by:", SLpText::_FLOAT, 8, 8);
  SL2Text *time1   = new SL2Text (_editor, "xxx" , 0,
               "min_max times to operate on:", SLpText::_FLOAT, 8, 3);
  SL2Text *depth1  = new SL2Text (_editor, "xxx" , 0,
               "min_max depths to operate on:", SLpText::_FLOAT, 8, 0);
  SLpText *time2   = new SLpText (_editor, "xxx" , 0,
                                               SLpText::_FLOAT, 8, 3);
  SLpText *depth2  = new SLpText (_editor, "xxx" , 0,
                                                SLpText::_FLOAT, 8, 0);

  nfun->showLabelAppearance();
  nsel->showLabelAppearance();
  act ->showLabelAppearance();

  choice->setupOptionLabelFun
                 (VfEditMultiply::CHOICE_SELECTED, sel_fun, manager);
  choice->setupOptionLabelFun
                 (VfEditMultiply::CHOICE_ALL     , all_fun, manager);
  choice->setupOptionLabelFun
                 (VfEditMultiply::CHOICE_ACTIVE  , act_fun, manager);

  choice   ->setItrap      (   choice_trap , _edit);
  type     ->setItrap      (     type_trap , _edit);
  constant ->setFtrap      ( constant_trap , _edit);
  time1    ->setFtrap      (    time1_trap , _edit);
  time2    ->setFtrap      (    time2_trap , _edit);
  depth1   ->setFtrap      (   depth1_trap , _edit);
  depth2   ->setFtrap      (   depth2_trap , _edit);

  choice   ->setupIvarFun  (    choice_upfun, _edit);
  nfun     ->setupIvarFun  (      nfun_upfun, manager);
  nsel     ->setupIvarFun  (      nsel_upfun, manager);
  act      ->setupIvarFun  (       act_upfun, manager);
  type     ->setupIvarFun  (      type_upfun, _edit);
  constant ->setupFvarFun  ( constant_upfun , _edit);
  time1    ->setupFvarFun  (    time1_upfun , _edit);
  time2    ->setupFvarFun  (    time2_upfun , _edit);
  depth1   ->setupFvarFun  (   depth1_upfun , _edit);
  depth2   ->setupFvarFun  (   depth2_upfun , _edit);

  time1    ->setupSenseFun (    time_sense , this);
  time2    ->setupSenseFun (    time_sense , this);
  depth1   ->setupSenseFun (   depth_sense , this);
  depth2   ->setupSenseFun (   depth_sense , this);


//                           LEFT     RIGHT    TOP      BOTTOM
  _editor->attach(row1    , _editor, _editor, _editor ,  NULL  ,  0,  0, 5);
  _editor->attach(row2    , _editor, _editor,  row1   ,  NULL  ,  0,  0, 5, 5);
  _editor->attach(sep     , _editor, _editor,  row2   ,  NULL  ,  0,  0, 5, 5);
  _editor->attach(row3    , _editor, _editor,  sep    ,  NULL  ,  0,  0, 5, 5);
  _editor->attach(constant, _editor,  NULL  ,  row3   ,  NULL  , 20,  0, 5, 5);
  _editor->attach(time1   , _editor,  NULL  , constant,  NULL  , 20,  0, 5, 5);
  _editor->attach(depth1  , _editor,  NULL  , time1   , _editor, 20,  0, 5, 5);
  _editor->attach(time2   ,  time1 ,  NULL  , constant,  NULL  , 20,  0, 5, 5);
  _editor->attach(depth2  ,  depth1,  NULL  , time1   , _editor, 20,  0, 5, 5);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopMultiply::~VfpopMultiply()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
