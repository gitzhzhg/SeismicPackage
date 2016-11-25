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

//---------------------- tp_ref_show_gui.cc ------------------------//
//---------------------- tp_ref_show_gui.cc ------------------------//
//---------------------- tp_ref_show_gui.cc ------------------------//

//         implementation file for the TpRefShowGui class
//               derived from the SLSmartForm class
//                        subdirectory pick


#include "pick/tp_ref_show_gui.hh"
#include "pick/tp_ref_pair.hh"
#include "pick/tp_popup_base.hh"
#include "pick/tp_resources.hh"
#include "sl/slp_label.hh"
#include "sl/sl2_text.hh"
#include "sl/sl2_scale.hh"
#include "sl/slp_option.hh"
#include "sl/slp_toggle.hh"
#include "sl/sl_column.hh"
#include "cprim.h"



//------------------- create column 1 --------------------------//
//------------------- create column 1 --------------------------//
//------------------- create column 1 --------------------------//


static void overlays_trap(void *data, long ident, long/*oldvar*/, long newvar)
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
TOG_UPDATE(overlays_update4, TP_NEXT)
TOG_UPDATE(overlays_update5, TP_SEL )



SLDelay *TpRefShowGui::createColumn1(TpPopupBase *pop)
{
  SLColumn *obj = new SLColumn(this, "overlays");
  _tog1 = new SLpToggle(obj, "show current picks"      , TP_CURR);
  _tog2 = new SLpToggle(obj, "overlay original picks"  , TP_ORIG);
  _tog3 = new SLpToggle(obj, "overlay previous profile", TP_PREV);
  _tog4 = new SLpToggle(obj, "overlay next profile"    , TP_NEXT);
  _tog5 = new SLpToggle(obj, "overlay selected profile", TP_SEL );

  _tog1->setItrap     (overlays_trap   , pop);
  _tog2->setItrap     (overlays_trap   , pop);
  _tog3->setItrap     (overlays_trap   , pop);
  _tog4->setItrap     (overlays_trap   , pop);
  _tog5->setItrap     (overlays_trap   , pop);

  _tog1->setupIvarFun (overlays_update1, pop);
  _tog2->setupIvarFun (overlays_update2, pop);
  _tog3->setupIvarFun (overlays_update3, pop);
  _tog4->setupIvarFun (overlays_update4, pop);
  _tog5->setupIvarFun (overlays_update5, pop);
  return obj;
}



//------------------- create row 1 --------------------------//
//------------------- create row 1 --------------------------//
//------------------- create row 1 --------------------------//


static void scale_trap(void *data, long/*ident*/, long/*oldvar*/, long newvar)
{
  TpRefPair *pair = (TpRefPair*)data;
  pair->setSelectedProfile(newvar);
}


static long scale_update(void *data)
{
  TpRefPair *pair = (TpRefPair*)data;
  return pair->getSelectedProfile();
}


static long scale_sense_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return (pop->pickingInProgress() && pop->showingVector(TP_SEL));
}



static long scale_min_update(void *data)
{
  TpRefPair *pair = (TpRefPair*)data;
  return pair->getFirstProfile();
}


static long scale_max_update(void *data)
{
  TpRefPair *pair = (TpRefPair*)data;
  return pair->getLastProfile();
}
 


SLDelay *TpRefShowGui::createRow1(TpPopupBase *pop, TpRefPair *pair)
{
  SLSmartForm       *obj  = new SLSmartForm(this, "selection");

  SL2Scale *scale = new SL2Scale(obj, "scale", 0, "",
                                 SLpScale::_HORIZONTAL, FALSE);
  SL2Text  *itext = new SL2Text (obj, "itext", 0, "selected profile",
                                           SLpText::_LONG, 6);

  obj->attach(scale, obj, obj, obj , NULL);
  obj->attach(itext, obj, obj, scale, obj, 10, 10);

  scale->setItrap     (scale_trap        , pair);
  itext->setItrap     (scale_trap        , pair);
  scale->setupIvarFun (scale_update      , pair);
  itext->setupIvarFun (scale_update      , pair);
  scale->setupSenseFun(scale_sense_update, pop);
  itext->setupSenseFun(scale_sense_update, pop);
  scale->setupIminFun (scale_min_update  , pair);
  scale->setupImaxFun (scale_max_update  , pair);
  return obj;
}



//------------------- create row 2 --------------------------//
//------------------- create row 2 --------------------------//
//------------------- create row 2 --------------------------//


static void show_trap(void *data, long/*ident*/, long/*oldvar*/, long newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setShowOverlays((char)newvar);
}


static long show_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->showingOverlays();
}


SLDelay *TpRefShowGui::createRow2(TpPopupBase *pop)
{
  SLSmartForm *obj  = new SLSmartForm(this, "show");

  SLpToggle *toggle = new SLpToggle(obj, "toggle", 0, "show overlays");

  obj->showEvenSpacing();

  toggle->setItrap     (show_trap  , pop);
  toggle->setupIvarFun (show_update, pop);
  return obj;
}



//------------------- create row 3 --------------------------//
//------------------- create row 3 --------------------------//
//------------------- create row 3 --------------------------//


static void align_trap(void *data, long/*ident*/, long/*oldvar*/, long newvar)
{
  TpRefPair *pair = (TpRefPair*)data;
  pair->setOverlayAlignment(newvar);
}


static long align_update(void *data)
{
  TpRefPair *pair = (TpRefPair*)data;
  return pair->getOverlayAlignment();
}


SLDelay *TpRefShowGui::createRow3(TpRefPair *pair)
{
  SLpOption *obj = new SLpOption(this, "align");

  obj->addOption("align matching channels" , CHANNEL );
  obj->addOption("align matching offsets"  , OFFSET  );
  obj->addOption("align matching receivers", RECEIVER);

  obj->setItrap     (align_trap  , pair);
  obj->setupIvarFun (align_update, pair);
  return obj;
}




//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TpRefShowGui::TpRefShowGui(SLDelay *slparent,
                         TpPopupBase *pop, TpRefPair *pair)
       : SLSmartForm(slparent, "tp_ref_show_gui", NULL, TRUE),
                          _tog1    (NULL),
                          _tog2    (NULL),
                          _tog3    (NULL),
                          _tog4    (NULL),
                          _tog5    (NULL)
{
  assert(slparent && pop && pair);

  SLpLabel *label = new SLpLabel  (this, "Overlay Choices");
  SLDelay  *col1  = createColumn1 (pop);
  SLDelay  *row1  = createRow1    (pop, pair);
  SLDelay  *row2  = createRow2    (pop);
  SLDelay  *row3  = createRow3    (pair);

  attach(label, this, this, this , NULL, 0, 0,  0,  0);
  attach(col1 , this, this, label, NULL, 0, 0,  0,  0);
  attach(row1 , this, this, col1 , NULL, 0, 0,  0,  0);
  attach(row2 , this, this, row1 , NULL, 0, 0,  0,  0);
  attach(row3 , this, this, row2 , this, 0, 0,  0,  0);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpRefShowGui::~TpRefShowGui()
{
}




//----------------------------- make ----------------------------//
//----------------------------- make ----------------------------//
//----------------------------- make ----------------------------//


Widget TpRefShowGui::make(Widget p)
{
  if(!made())
       {
       Widget w = SLSmartForm::make(p);
       TpResources::setForeground(TP_CURR, _tog1);
       TpResources::setForeground(TP_ORIG, _tog2);
       TpResources::setForeground(TP_PREV, _tog3);
       TpResources::setForeground(TP_NEXT, _tog4);
       TpResources::setForeground(TP_SEL , _tog5);
       }
  makeChildren();
  return topWidget();
}





//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
