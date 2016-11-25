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

//------------------------- vfpop_delete.cc ---------------------------//
//------------------------- vfpop_delete.cc ---------------------------//
//------------------------- vfpop_delete.cc ---------------------------//

//          implementation file for the VfpopDelete class
//               derived from the VfpopEditBase class
//                       subdirectory vfgui


#include "vfgui/vfpop_delete.hh"
#include "vf/vf_edit_delete.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "sl/sl_smart_form.hh"
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
  VfEditDelete *edit = (VfEditDelete*)data;
  edit->setChoice((int)newvar);
}



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


static long choice_upfun(void *data)
{
  VfEditDelete *edit = (VfEditDelete*)data;
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



//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//



//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


VfpopDelete::VfpopDelete(SLDelay *slparent, char *name,
                   VfManager *manager, ContainerList *clist)
       : VfpopEditBase(slparent, name, manager, clist, "DELETE_OVERVIEW")
{
  _edit = new VfEditDelete();

  SLSmartForm *row1 = new SLSmartForm(_editor, "row1", NULL);
  SLSmartForm *row2 = new SLSmartForm(_editor, "row2", NULL);

  row1->showEvenSpacing();
  row2->showEvenSpacing();

  SLpOption *choice = new SLpOption(row1, "choice", 0, "");
  choice->addOption("choice", VfEditDelete::CHOICE_SELECTED,
                       "delete SELECTED velocity functions in active dataset");
  choice->addOption("choice", VfEditDelete::CHOICE_ALL,
                       "delete ALL velocity functions in active dataset");
  choice->addOption("choice", VfEditDelete::CHOICE_ACTIVE,
                       "delete ACTIVE velocity function in active dataset");
  choice->addOption("choice", VfEditDelete::CHOICE_EMPTY,
                       "delete EMPTY velocity functions in active dataset");

  SL2Text *nfun  = new SL2Text (row2, "nfun", 0,
                           "#functions in active dataset:", SLpText::_LONG, 6);
  SL2Text *nsel  = new SL2Text (row2, "nsel", 0,
                           "#selected:", SLpText::_LONG, 6);
  SL2Text *act   = new SL2Text (row2, "act" , 0,
                           "active function:", SLpText::_LONG, 6);

  nfun->showLabelAppearance();
  nsel->showLabelAppearance();
  act ->showLabelAppearance();

  choice->setItrap     (choice_trap , _edit);
  choice->setupIvarFun (choice_upfun, _edit);
  nfun  ->setupIvarFun (  nfun_upfun, manager);
  nsel  ->setupIvarFun (  nsel_upfun, manager);
  act   ->setupIvarFun (   act_upfun, manager);

//                         LEFT     RIGHT    TOP      BOTTOM
  _editor->attach(row1  , _editor, _editor, _editor,  NULL  ,  0,  0, 5);
  _editor->attach(row2  , _editor, _editor,  row1  , _editor,  0,  0, 5, 5);
}



//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopDelete::~VfpopDelete()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
