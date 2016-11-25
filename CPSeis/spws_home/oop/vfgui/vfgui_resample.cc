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

//---------------------- vfgui_resample.cc ------------------------//
//---------------------- vfgui_resample.cc ------------------------//
//---------------------- vfgui_resample.cc ------------------------//

//         implementation file for the VfguiResample class
//               derived from the VfguiEditBase class
//                       subdirectory vfgui


#include "vfgui/vfgui_resample.hh"
#include "vf/vf_edit_resample.hh"
#include "vf/vf_constants.hh"
#include "sl/sl2_text.hh"
#include "sl/slp_option.hh"
#include "sl/slp_label.hh"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>



//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//
//---------------------------- traps -----------------------------//


static void nhx_trap(void *data, long /*ident*/,
                     long /*oldvar*/, long newvar)
{
  VfEditResample *edit = (VfEditResample*)data;
/*
  edit->setNhx((int)newvar);
*/
}



//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//
//------------------------ update functions ----------------------//


static long nhx_upfun(void *data)
{                                
  VfEditResample *edit = (VfEditResample*)data;
/*
  return edit->->getNhx();
*/
  return 77;
}



//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//
//-------------------- sense update functions ----------------------//


static long nhx_sense_upfun(void *data)
{                                
  VfEditResample *edit = (VfEditResample*)data;
  return TRUE;
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


VfguiResample::VfguiResample(SLDelay *slparent, VfUtilities *utilities)
       : VfguiEditBase(slparent)
{
  VfEditResample *edit = new VfEditResample(utilities);
  supplyEditObject(edit);

  SLpLabel *label = new SLpLabel(this, "this gui is just testing junk");

  SLSmartForm *row1 = new SLSmartForm(this, "row1", NULL);
  SLSmartForm *row2 = new SLSmartForm(this, "row2", NULL);

  row1->showEvenSpacing();
  row2->showEvenSpacing();

  SLpOption *read = new SLpOption(row1, "read", 0, "");
  read->addOption("read", READ_REPLACE,
                          "replace all velocity functions in active dataset");
  read->addOption("read", READ_ADD,
                          "add to velocity functions in active dataset");
  read->addOption("read", READ_NEW,
                          "put data into new uneditable dataset");

  SL2Text *nfun  = new SL2Text (row2, "nfun" , 0,
                           "#functions in active dataset:", SLpText::_LONG, 6);
  SL2Text *nhx   = new SL2Text (row2, "nhx"  , 0, "nhx:"  , SLpText::_LONG, 2);
  SL2Text *nhy   = new SL2Text (row2, "nhy"  , 0, "nhy:"  , SLpText::_LONG, 2);
  SL2Text *order = new SL2Text (row2, "order", 0, "order:", SLpText::_LONG, 2);

  nfun->showLabelAppearance();

  nhx  ->setItrap (  nhx_trap, edit);

  nhx  ->setupIvarFun (  nhx_upfun, edit);

//                  LEFT   RIGHT   TOP      BOTTOM
  attach(label    , this , this  , this    , NULL,  0,  0, 5);
  attach(row1     , this , this  , label   , NULL,  0,  0, 5);
  attach(row2     , this , this  , row1    , this,  0,  0, 5, 5);
}



//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


VfguiResample::~VfguiResample()
{
  //////// delete _edit;  // deleted in base class destructor.
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//

