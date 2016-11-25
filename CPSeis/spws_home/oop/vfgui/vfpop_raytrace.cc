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

//------------------------- vfpop_raytrace.cc ---------------------------//
//------------------------- vfpop_raytrace.cc ---------------------------//
//------------------------- vfpop_raytrace.cc ---------------------------//

//          implementation file for the VfpopRaytrace class
//               derived from the VfpopEditBase class
//                       subdirectory vfgui


#include "vfgui/vfpop_raytrace.hh"
#include "vf/vf_edit_raytrace.hh"
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
  VfEditRaytrace *edit = (VfEditRaytrace*)data;
  edit->setChoice((int)newvar);
}


static void how_trap(void *data, long /*ident*/,
                     long /*oldvar*/, long newvar)
{
  VfEditRaytrace *edit = (VfEditRaytrace*)data;
  edit->setHow((int)newvar);
}



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


static long choice_upfun(void *data)
{
  VfEditRaytrace *edit = (VfEditRaytrace*)data;
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


static long how_upfun(void *data)
{
  VfEditRaytrace *edit = (VfEditRaytrace*)data;
  return edit->getHow();
}


static long nerr_upfun(void *data)
{
  VfManager *manager = (VfManager*)data;
  return manager->activeDataset()->numVelocityFunctionsWithErrors();
}


static long nray_upfun(void *data)
{
  VfManager *manager = (VfManager*)data;
  return manager->activeDataset()->numRaytracedVelocityFunctions();
}



//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//




//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


VfpopRaytrace::VfpopRaytrace(SLDelay *slparent, char *name,
                   VfManager *manager, ContainerList *clist)
       : VfpopEditBase(slparent, name, manager, clist, "TYPES_OVERVIEW")
{
  _edit = new VfEditRaytrace();

  SLSmartForm *row1 = new SLSmartForm(_editor, "row1", NULL);
  SLSmartForm *row2 = new SLSmartForm(_editor, "row2", NULL);
  SLSmartForm *row3 = new SLSmartForm(_editor, "row3", NULL);
  SLSmartForm *row4 = new SLSmartForm(_editor, "row4", NULL);
  SLSep       *sep  = new SLSep      (_editor, "sep");
  SLpLabel    *lab  = new SLpLabel   (_editor, "lab", 0,
     "(raytracing will take awhile when processing many velocity functions)");

  row1->showEvenSpacing();
  row2->showEvenSpacing();
  row3->showEvenSpacing();
  row4->showEvenSpacing();

  SLpOption *choice = new SLpOption(row1, "choice", 0, "");
  choice->addOption("choice", VfEditRaytrace::CHOICE_SELECTED,
            "process SELECTED velocity functions in active dataset");
  choice->addOption("choice", VfEditRaytrace::CHOICE_ALL,
            "process ALL velocity functions in active dataset");
  choice->addOption("choice", VfEditRaytrace::CHOICE_ACTIVE,
            "process ACTIVE velocity function in active dataset");

  SL2Text *nfun  = new SL2Text (row2, "nfun", 0,
                           "#functions in active dataset:", SLpText::_LONG, 6);
  SL2Text *nsel  = new SL2Text (row2, "nsel", 0,
                           "#selected:", SLpText::_LONG, 6);
  SL2Text *act   = new SL2Text (row2, "act" , 0,
                           "active function:", SLpText::_LONG, 6);

  SLpOption *how = new SLpOption(row3, "how", 0, "");
  how->addOption("how", VfEditRaytrace::HOW_FORWARD,
            "raytrace to get NMO from RMS velocities");
  how->addOption("how", VfEditRaytrace::HOW_INVERSE,
            "inverse raytrace to get RMS from NMO velocities");
  how->addOption("how", VfEditRaytrace::HOW_RESET_NMO,
            "reset NMO velocities equal to RMS velocities");
  how->addOption("how", VfEditRaytrace::HOW_RESET_RMS,
            "reset RMS velocities equal to NMO velocities");

  SL2Text *nerr  = new SL2Text (row4, "nerr", 0,
                          " #functions with errors:", SLpText::_LONG, 6);
  SL2Text *nray  = new SL2Text (row4, "nray", 0,
                          " #functions raytraced:", SLpText::_LONG, 6);

  nfun->showLabelAppearance();
  nsel->showLabelAppearance();
  act ->showLabelAppearance();
  nerr->showLabelAppearance();
  nray->showLabelAppearance();

  choice->setItrap      (choice_trap , _edit);
  how   ->setItrap      (   how_trap , _edit);

  choice->setupIvarFun  (choice_upfun, _edit);
  nfun  ->setupIvarFun  (  nfun_upfun, manager);
  nsel  ->setupIvarFun  (  nsel_upfun, manager);
  act   ->setupIvarFun  (   act_upfun, manager);
  how   ->setupIvarFun  (   how_upfun, _edit);
  nerr  ->setupIvarFun  (  nerr_upfun, manager);
  nray  ->setupIvarFun  (  nray_upfun, manager);

//                         LEFT     RIGHT    TOP      BOTTOM
  _editor->attach(row1  , _editor, _editor, _editor,  NULL  ,  0,  0, 5);
  _editor->attach(row2  , _editor, _editor,  row1  ,  NULL  ,  0,  0, 5, 5);
  _editor->attach(sep   , _editor, _editor,  row2  ,  NULL  ,  0,  0, 5, 5);
  _editor->attach(row3  , _editor, _editor,  sep   ,  NULL  ,  0,  0, 5, 5);
  _editor->attach(row4  , _editor, _editor,  row3  ,  NULL  ,  0,  0, 5, 5);
  _editor->attach(lab   , _editor, _editor,  row4  , _editor,  0,  0, 5, 5);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopRaytrace::~VfpopRaytrace()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
