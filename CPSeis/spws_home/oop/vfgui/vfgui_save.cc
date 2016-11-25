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

//---------------------- vfgui_save.cc ------------------------//
//---------------------- vfgui_save.cc ------------------------//
//---------------------- vfgui_save.cc ------------------------//

//         implementation file for the VfguiSave class
//               derived from the SLSmartForm class
//                       subdirectory vfgui


#include "vfgui/vfgui_save.hh"
#include "vf/vf_file_base.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"
#include "vf/vf_utilities.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_option.hh"
#include "sl/slp_label.hh"
#include "sl/slp_toggle.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//


static void choice_trap(void *data, long /*ident*/,
                        long /*oldvar*/, long newvar)
{
  VfguiSave *gui = (VfguiSave*)data;
  gui->file()->setSaveChoice((int)newvar);
}


static void type_trap(void *data, long /*ident*/,
                     long /*oldvar*/, long newvar)
{
  VfguiSave *gui = (VfguiSave*)data;
  if(newvar == LASTTYPE+2)
      {
      gui->file()->setType(-1);
      }
  else
      {
      gui->file()->setType((int)newvar-1);
      }
}


static void sort_trap(void *data, long /*ident*/,
                        long /*oldvar*/, long newvar)
{
  VfguiSave *gui = (VfguiSave*)data;
  gui->file()->setSortChoice((int)newvar);
}


static void names_trap(void *data, long /*ident*/,
                        long /*oldvar*/, long newvar)
{
  VfguiSave *gui = (VfguiSave*)data;
  gui->file()->setNamesChoice((int)newvar);
}




//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


static long choice_upfun(void *data)
{                                
  VfguiSave *gui = (VfguiSave*)data;
  return gui->file()->getSaveChoice();
}


static long nfun_upfun(void *data)
{                                
  VfguiSave *gui = (VfguiSave*)data;
  return gui->manager()->activeDataset()->numVelocityFunctions();
}


static long nsel_upfun(void *data)
{                                
  VfguiSave *gui = (VfguiSave*)data;
  return gui->manager()->activeDataset()->numSelectedVelocityFunctions();
}


static long act_upfun(void *data)
{                                
  VfguiSave *gui = (VfguiSave*)data;
  return gui->manager()->activeDataset()->getActiveVelocityFunction()+1;
}


static long type_upfun(void *data)
{                                
  VfguiSave *gui = (VfguiSave*)data;
  if(gui->file()->getType() == -1) return LASTTYPE+2;
  return gui->file()->getType()+1;
}


static long nerr_upfun(void *data)
{                                
  VfguiSave *gui = (VfguiSave*)data;
  return gui->manager()->activeDataset()->numVelocityFunctionsWithErrors();
}


static long nray_upfun(void *data)
{                                
  VfguiSave *gui = (VfguiSave*)data;
  return gui->manager()->activeDataset()->numRaytracedVelocityFunctions();
}


static long sort_upfun(void *data)
{                                
  VfguiSave *gui = (VfguiSave*)data;
  return gui->file()->getSortChoice();
}


static long names_upfun(void *data)
{                                
  VfguiSave *gui = (VfguiSave*)data;
  return gui->file()->getNamesChoice();
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


VfguiSave::VfguiSave(SLDelay *slparent, char *name,
                                 VfManager *manager, VfFileBase *file)
       : SLSmartForm(slparent, name, NULL, FALSE),
               _manager      (manager),
               _file         (file)
{
  assert(_manager && _file);

  SLSmartForm *row1 = new SLSmartForm(this, "row1", NULL);
  SLSmartForm *row2 = new SLSmartForm(this, "row2", NULL);
//SLSmartForm *row3 = new SLSmartForm(this, "row3", NULL);
  SLSmartForm *row4 = new SLSmartForm(this, "row4", NULL);

  row1->showEvenSpacing();
  row2->showEvenSpacing();
//row3->showEvenSpacing();
  row4->showEvenSpacing();

  SLpOption *choice = new SLpOption(row1, "choicexxx", 0, "");
  choice->addOption("choicexxx", SAVE_SELECTED,
                     "save SELECTED velocity functions from active dataset");
  choice->addOption("choicexxx", SAVE_ALL,
                     "save ALL velocity functions from active dataset");
  choice->addOption("choicexxx", SAVE_ACTIVE,
                     "save ACTIVE velocity function from active dataset");

  SL2Text *nfun  = new SL2Text (row2, "nfun", 0,
                          " #functions in active dataset:", SLpText::_LONG, 6);
  SL2Text *nsel  = new SL2Text (row2, "nsel", 0,
                                              "#selected:", SLpText::_LONG, 6);
  SL2Text *act   = new SL2Text (row2, "act" , 0,
                                        "active function:", SLpText::_LONG, 6);

  SLpOption *type = new SLpOption(this, "type", 0,
                                                 "type of functions to save:");
  for(int i = FIRSTTYPE; i <= LASTTYPE; i++)
      {
      type->addOption("type", i+1,
                          (char*)_manager->utilities()->typeDescription(i));
      }
  type->addOption("type", LASTTYPE+2, "save mixed velocity function types");

  SL2Text *nerr  = new SL2Text (row4, "nerr", 0,
                          " #functions with errors:", SLpText::_LONG, 6);
  SL2Text *nray  = new SL2Text (row4, "nray", 0,
                          " #functions raytraced:", SLpText::_LONG, 6);

  SLpToggle *sort  = new SLpToggle (this, "sortxxx", 0,
                 "sort all velocity functions (if necessary) before saving");
  SLpToggle *names = new SLpToggle (this, "namesxxx", 0,
                 "rename all blank velocity function names before saving");

  nfun->showLabelAppearance();
  nsel->showLabelAppearance();
  act ->showLabelAppearance();
  nerr->showLabelAppearance();
  nray->showLabelAppearance();

  choice->setItrap (choice_trap, this);
  type  ->setItrap (  type_trap, this);
  sort  ->setItrap (  sort_trap, this);
  names ->setItrap ( names_trap, this);

  choice->setupIvarFun (choice_upfun, this);
  nfun  ->setupIvarFun (  nfun_upfun, this);
  nsel  ->setupIvarFun (  nsel_upfun, this);
  act   ->setupIvarFun (   act_upfun, this);
  type  ->setupIvarFun (  type_upfun, this);
  nerr  ->setupIvarFun (  nerr_upfun, this);
  nray  ->setupIvarFun (  nray_upfun, this);
  sort  ->setupIvarFun (  sort_upfun, this);
  names ->setupIvarFun ( names_upfun, this);

//                  LEFT   RIGHT   TOP      BOTTOM
  attach(row1     , this , this  , this    , NULL,  0,  0, 5);
  attach(row2     , this , this  , row1    , NULL,  0,  0, 5);
  attach(type     , this , NULL  , row2    , NULL,  0,  0, 5);
  attach(row4     , this , this  , type    , NULL,  0,  0, 5);
  attach(sort     , this , NULL  , row4    , NULL,  0,  0, 5);
  attach(names    , this , NULL  , sort    , this,  0,  0, 0, 5);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


VfguiSave::~VfguiSave()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

