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

//---------------------- tp_sisc_show_gui.cc ------------------------//
//---------------------- tp_sisc_show_gui.cc ------------------------//
//---------------------- tp_sisc_show_gui.cc ------------------------//

//         implementation file for the TpSiscShowGui class
//               derived from the SLSmartForm class
//                        subdirectory pick


#include "pick/tp_sisc_show_gui.hh"
#include "pick/tp_popup_base.hh"
#include "pick/tp_resources.hh"
#include "sl/slp_label.hh"
#include "sl/slp_toggle.hh"
#include "sl/sl_column.hh"
#include "cprim.h"


//------------------- create column 1 --------------------------//
//------------------- create column 1 --------------------------//
//------------------- create column 1 --------------------------//


static void overlays_trap(void *data, long ident, long /*oldvar*/, long newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setShowVector((int)ident, (char)newvar);
}



#define TOG_UPDATE(overlays_update1, TP_CURR)   \
static long overlays_update1(void *data)        \
{                                               \
  TpPopupBase *pop = (TpPopupBase*)data;        \
  return pop->showingVector(TP_CURR);           \
}

TOG_UPDATE(overlays_update1, TP_CURR)
TOG_UPDATE(overlays_update2, TP_ORIG)
TOG_UPDATE(overlays_update3, TP_PREV)



SLDelay *TpSiscShowGui::createColumn1(TpPopupBase *pop)
{
  SLColumn *obj = new SLColumn(this, "overlays");
  _tog1 = new SLpToggle(obj, "show current picks"    , TP_CURR);
  _tog2 = new SLpToggle(obj, "overlay original picks", TP_ORIG);
  _tog3 = new SLpToggle(obj, "show integrated picks" , TP_PREV);

  _tog1->setItrap      (overlays_trap   , pop);
  _tog2->setItrap      (overlays_trap   , pop);
  _tog3->setItrap      (overlays_trap   , pop);

  _tog1->setupIvarFun  (overlays_update1, pop);
  _tog2->setupIvarFun  (overlays_update2, pop);
  _tog3->setupIvarFun  (overlays_update3, pop);
  return obj;
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TpSiscShowGui::TpSiscShowGui(SLDelay *slparent, TpPopupBase *pop)
       : SLSmartForm(slparent, "tp_sisc_show_gui", NULL, TRUE),
                          _tog1    (NULL),
                          _tog2    (NULL),
                          _tog3    (NULL)
{
  assert(slparent && pop);

  SLpLabel *label = new SLpLabel  (this, "Overlay Choices");
  SLDelay  *col1  = createColumn1 (pop);
  attach(label, this, this, this , NULL, 0, 0,  0,  0);
  attach(col1 , this, this, label, this, 0, 0,  0,  0);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpSiscShowGui::~TpSiscShowGui()
{
}



//----------------------------- make ----------------------------//
//----------------------------- make ----------------------------//
//----------------------------- make ----------------------------//


Widget TpSiscShowGui::make(Widget p)
{
  if(!made())
       {
       Widget w = SLSmartForm::make(p);
       TpResources::setForeground(TP_CURR, _tog1);
       TpResources::setForeground(TP_ORIG, _tog2);
       TpResources::setForeground(TP_PREV, _tog3);
       }
  makeChildren();
  return topWidget();
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
