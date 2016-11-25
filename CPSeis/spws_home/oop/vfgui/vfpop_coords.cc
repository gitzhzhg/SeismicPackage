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

//------------------------- vfpop_coords.cc ---------------------------//
//------------------------- vfpop_coords.cc ---------------------------//
//------------------------- vfpop_coords.cc ---------------------------//

//          implementation file for the VfpopCoords class
//               derived from the VfpopEditBase class
//                       subdirectory vfgui


#include "vfgui/vfpop_coords.hh"
#include "vfgui/vfgui_transform.hh"
#include "vf/vf_edit_coords.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "sl/sl_smart_form.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_text.hh"
#include "sl/slp_push.hh"
#include "sl/slp_option.hh"
#include "sl/slp_label.hh"
#include "sl/slp_toggle.hh"
#include "sl/sl_sep.hh"
#include <stream.h>
#include <assert.h>


#define  LABEL   "press Help for instructions"

#define  EDIT     VfEditCoords *edit = (VfEditCoords*)data;
#define  MANAGER  VfManager *manager = (VfManager*)data;


//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//


static void choice_trap(void *data, long /*ident*/,
                     long /*oldvar*/, long newvar)
{
  EDIT
  edit->setChoice((int)newvar);
}


static void dir_trap(void *data, long /*ident*/,
                     long /*oldvar*/, long newvar)
{
  EDIT
  edit->setDirection((int)newvar);
}



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


static long choice_upfun(void *data)
{
  EDIT
  return (long)edit->getChoice();
}


static long dir_upfun(void *data)
{
  EDIT
  return (long)edit->getDirection();
}


static long nfun_upfun(void *data)
{
  MANAGER
  return manager->activeDataset()->numVelocityFunctions();
}


static long nsel_upfun(void *data)
{
  MANAGER
  return manager->activeDataset()->numSelectedVelocityFunctions();
}


static long act_upfun(void *data)
{
  MANAGER
  return manager->activeDataset()->getActiveVelocityFunction() + 1;
}



//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//




//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//
//-------------------------- constructor -----------------------------//


VfpopCoords::VfpopCoords(SLDelay *slparent, char *name,
                   VfManager *manager, ContainerList *clist)
       : VfpopEditBase(slparent, name, manager, clist, "COORDS_OVERVIEW")
{
  _edit = new VfEditCoords();

  GridTransform  *transform = ((VfEditCoords*)_edit)->transform();
  VfguiTransform *gui       = new VfguiTransform(_editor, transform, NULL);


  SLSmartForm *row1 = new SLSmartForm(_editor, "row1", NULL);
  SLSmartForm *row2 = new SLSmartForm(_editor, "row2", NULL);
  SLSep       *sep  = new SLSep      (_editor, "sep");
  SLSep       *sep2 = new SLSep      (_editor, "sep", SLSep::_VERTICAL);

  row1->showEvenSpacing();
  row2->showEvenSpacing();

  SLpOption *choice = new SLpOption(row1, "choice", 0, "");
  choice->addOption("choice", VfEditCoords::CHOICE_SELECTED,
            "reset coords in SELECTED velocity functions in active dataset");
  choice->addOption("choice", VfEditCoords::CHOICE_ALL,
            "reset coords in ALL velocity functions in active dataset");
  choice->addOption("choice", VfEditCoords::CHOICE_ACTIVE,
            "reset coords in ACTIVE velocity function in active dataset");

  SL2Text *nfun  = new SL2Text (row2, "nfun", 0,
                           "#functions in active dataset:", SLpText::_LONG, 6);
  SL2Text *nsel  = new SL2Text (row2, "nsel", 0,
                           "#selected:", SLpText::_LONG, 6);
  SL2Text *act   = new SL2Text (row2, "act" , 0,
                           "active function:", SLpText::_LONG, 6);

  SLpOption *dir = new SLpOption(_editor, "dir", 0, "");
  dir->addOption("dir", VfEditCoords::DIRECTION_FORWARD,
            "do\nforward\ntransformation\n");
  dir->addOption("dir", VfEditCoords::DIRECTION_REVERSE,
            "do\nreverse\ntransformation\n");
  dir->addOption("dir", VfEditCoords::INTEGERIZE,
            "integerize\nthe\ncoordinates\n");
  dir->addOption("dir", VfEditCoords::SWAP_COORDS,
            "swap the\ncoordinates\n");
  dir->addOption("dir", VfEditCoords::SWAP_COORDS_AND_HEADERS,
            "swap the\ncoordinates\nand headers\n");

  SLpLabel  *lab = new SLpLabel(_editor, "lab", 0, LABEL);

  choice->setItrap     (choice_trap , _edit);
  dir   ->setItrap     (   dir_trap , _edit);

  nfun  ->showLabelAppearance();
  nsel  ->showLabelAppearance();
  act   ->showLabelAppearance();

  choice->setupIvarFun (choice_upfun, _edit);
  dir   ->setupIvarFun (   dir_upfun, _edit);
  nfun  ->setupIvarFun (  nfun_upfun, manager);
  nsel  ->setupIvarFun (  nsel_upfun, manager);
  act   ->setupIvarFun (   act_upfun, manager);

//                         LEFT     RIGHT    TOP      BOTTOM
  _editor->attach(row1  , _editor, _editor, _editor,  NULL  ,  0,  0,   5, 5);
  _editor->attach(row2  , _editor, _editor,  row1  ,  NULL  ,  0,  0,   5, 5);
  _editor->attach(sep   , _editor, _editor,  row2  ,  NULL  ,  0,  0,   5, 5);
  _editor->attach(gui   , _editor,  NULL  ,  sep   , _editor,  0,  0,   5, 5);
  _editor->attach(sep2  ,  gui   ,  NULL  ,  sep   , _editor,  0,  0,   0, 0);
  _editor->attach(dir   ,  NULL  , _editor,  sep   ,  NULL  ,  0, 50,  80, 5);
  _editor->attach(lab   ,  NULL  , _editor,  dir   ,  NULL  ,  0, 50,  30, 5);
}


//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//
//------------------------- destructor -------------------------//


VfpopCoords::~VfpopCoords()
{
}



//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
//-------------------------- end -------------------------------//
