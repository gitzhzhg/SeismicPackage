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

//---------------------- vfgui_read.cc ------------------------//
//---------------------- vfgui_read.cc ------------------------//
//---------------------- vfgui_read.cc ------------------------//

//         implementation file for the VfguiRead class
//               derived from the SLSmartForm class
//                       subdirectory vfgui


#include "vfgui/vfgui_read.hh"
#include "vf/vf_file_base.hh"
#include "vf/vf_manager.hh"
#include "vf/vf_dataset.hh"
#include "vf/vf_constants.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_option.hh"
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
  VfguiRead *gui = (VfguiRead*)data;
  gui->file()->setReadChoice((int)newvar);
}


static void nhx_trap(void *data, long /*ident*/,
                     long /*oldvar*/, long newvar)
{
  VfguiRead *gui = (VfguiRead*)data;
  gui->manager()->activeDataset()->setNhx((int)newvar);
}


static void nhy_trap(void *data, long /*ident*/,
                     long /*oldvar*/, long newvar)
{
  VfguiRead *gui = (VfguiRead*)data;
  gui->manager()->activeDataset()->setNhy((int)newvar);
}


static void order_trap(void *data, long /*ident*/,
                       long /*oldvar*/, long newvar)
{
  VfguiRead *gui = (VfguiRead*)data;
  gui->manager()->activeDataset()->setMoveoutOrder((int)newvar);
}



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


static long choice_upfun(void *data)
{                                
  VfguiRead *gui = (VfguiRead*)data;
  return gui->file()->getReadChoice();
}


static long nfun_upfun(void *data)
{                                
  VfguiRead *gui = (VfguiRead*)data;
  return gui->manager()->activeDataset()->numVelocityFunctions();
}


static long nhx_upfun(void *data)
{                                
  VfguiRead *gui = (VfguiRead*)data;
  return gui->manager()->activeDataset()->getNhx();
}


static long nhy_upfun(void *data)
{                                
  VfguiRead *gui = (VfguiRead*)data;
  return gui->manager()->activeDataset()->getNhy();
}


static long order_upfun(void *data)
{                                
  VfguiRead *gui = (VfguiRead*)data;
  return gui->manager()->activeDataset()->getMoveoutOrder();
}



//------------------------ sense update functions ----------------------//
//------------------------ sense update functions ----------------------//
//------------------------ sense update functions ----------------------//



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


VfguiRead::VfguiRead(SLDelay *slparent, char *name,
                                 VfManager *manager, VfFileBase *file)
       : SLSmartForm(slparent, name, NULL, FALSE),
               _manager      (manager),
               _file         (file)
{
  assert(_manager && _file);

  SLSmartForm *row1 = new SLSmartForm(this, "row1", NULL);
  SLSmartForm *row2 = new SLSmartForm(this, "row2", NULL);

  row1->showEvenSpacing();
  row2->showEvenSpacing();

  SLpOption *choice = new SLpOption(row1, "choicexxx", 0, "");
  choice->addOption("choicexxx", READ_REPLACE,
                          "REPLACE ALL velocity functions in active dataset");
  choice->addOption("choicexxx", READ_ADD,
                          "ADD TO velocity functions in active dataset");
  choice->addOption("choicexxx", READ_NEW,
                          "put data into NEW UNEDITABLE dataset");
  choice->addOption("choicexxx", READ_NOTHING,
                          "do not read the velocity file");

  SL2Text *nfun  = new SL2Text (row2, "nfun" , 0,
                           "#functions in active dataset:", SLpText::_LONG, 6);
  SL2Text *nhx   = new SL2Text (row2, "nhx"  , 0, "nhx:"  , SLpText::_LONG, 2);
  SL2Text *nhy   = new SL2Text (row2, "nhy"  , 0, "nhy:"  , SLpText::_LONG, 2);
  SL2Text *order = new SL2Text (row2, "order", 0, "order:", SLpText::_LONG, 2);

  nfun->showLabelAppearance();

  choice->setItrap (choice_trap, this);
  nhx   ->setItrap (   nhx_trap, this);
  nhy   ->setItrap (   nhy_trap, this);
  order ->setItrap ( order_trap, this);

  choice->setupIvarFun (choice_upfun, this);
  nfun  ->setupIvarFun (  nfun_upfun, this);
  nhx   ->setupIvarFun (   nhx_upfun, this);
  nhy   ->setupIvarFun (   nhy_upfun, this);
  order ->setupIvarFun ( order_upfun, this);

//                  LEFT   RIGHT   TOP      BOTTOM
  attach(row1     , this , this  , this    , NULL,  0,  0, 5);
  attach(row2     , this , this  , row1    , this,  0,  0, 5, 5);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


VfguiRead::~VfguiRead()
{
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

