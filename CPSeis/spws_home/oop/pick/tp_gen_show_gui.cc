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

//---------------------- tp_gen_show_gui.cc ------------------------//
//---------------------- tp_gen_show_gui.cc ------------------------//
//---------------------- tp_gen_show_gui.cc ------------------------//

//         implementation file for the TpGenShowGui class
//               derived from the SLSmartForm class
//                        subdirectory pick


#include "pick/tp_gen_show_gui.hh"
#include "pick/tp_statfile_pair.hh"
#include "pick/tp_popup_base.hh"
#include "pick/tp_resources.hh"
#include "sl/slp_label.hh"
#include "sl/slp_push.hh"
#include "sl/sl2_text.hh"
#include "sl/sl2_scale.hh"
#include "sl/slp_option.hh"
#include "sl/slp_toggle.hh"
#include "sl/sl_column.hh"
#include "cprim.h"
#include "named_constants.h"



//------------------- create column 1 --------------------------//
//------------------- create column 1 --------------------------//
//------------------- create column 1 --------------------------//


static void overlays_trap(void *data, long ident, long/*oldvar*/,long newvar)
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



static char *overlays_label3(void *data)
{
  static char xlabel[] = "overlay previous X bin";
  static char ylabel[] = "overlay previous Y bin";
  TpStatfilePair *pair = (TpStatfilePair*)data;
  if(pair->getSwitch()) return xlabel;
                        return ylabel;
}


static char *overlays_label4(void *data)
{
  static char xlabel[] = "overlay next X bin";
  static char ylabel[] = "overlay next Y bin";
  TpStatfilePair *pair = (TpStatfilePair*)data;
  if(pair->getSwitch()) return xlabel;
                        return ylabel;
}


static char *overlays_label5(void *data)
{
  static char xlabel[] = "overlay selected X bin";
  static char ylabel[] = "overlay selected Y bin";
  TpStatfilePair *pair = (TpStatfilePair*)data;
  if(pair->getSwitch()) return xlabel;
                        return ylabel;
}



SLDelay *TpGenShowGui::createColumn1(TpPopupBase *pop,
                                      TpStatfilePair *pair)
{
  SLColumn *obj = new SLColumn(this, "tpgs_overlays");
  _tog1 = new SLpToggle(obj, "show current picks"    , TP_CURR);
  _tog2 = new SLpToggle(obj, "overlay original picks", TP_ORIG);
  _tog3 = new SLpToggle(obj, "overlay previous bin"  , TP_PREV);
  _tog4 = new SLpToggle(obj, "overlay next bin"      , TP_NEXT);
  _tog5 = new SLpToggle(obj, "overlay selected bin"  , TP_SEL );

  _tog1->setItrap      (overlays_trap   , pop);
  _tog2->setItrap      (overlays_trap   , pop);
  _tog3->setItrap      (overlays_trap   , pop);
  _tog4->setItrap      (overlays_trap   , pop);
  _tog5->setItrap      (overlays_trap   , pop);

  _tog1->setupIvarFun  (overlays_update1, pop);
  _tog2->setupIvarFun  (overlays_update2, pop);
  _tog3->setupIvarFun  (overlays_update3, pop);
  _tog4->setupIvarFun  (overlays_update4, pop);
  _tog5->setupIvarFun  (overlays_update5, pop);

  _tog3->setupLabelFun (overlays_label3 , pair);
  _tog4->setupLabelFun (overlays_label4 , pair);
  _tog5->setupLabelFun (overlays_label5 , pair);
  return obj;
}



//------------------- create row 1 --------------------------//
//------------------- create row 1 --------------------------//
//------------------- create row 1 --------------------------//


static void text_trap(void *data, long/*ident*/, float/*oldvar*/, float newvar)
{
  TpStatfilePair *pair = (TpStatfilePair*)data;
  if(pair->getSwitch()) pair->setXsel(newvar);
  else                  pair->setYsel(newvar);
}


static void scale_trap(void *data, long ident, long oldvar, long newvar)
{
  text_trap(data, ident, (float)oldvar, (float)newvar);
}


static float text_update(void *data)
{
  TpStatfilePair *pair = (TpStatfilePair*)data;
  if(pair->getSwitch()) return pair->getXsel();
                        return pair->getYsel();
}


static char *text_label_update(void *data)
{
  static char xlabel[] = "selected X bin";
  static char ylabel[] = "selected Y bin";
  TpStatfilePair *pair = (TpStatfilePair*)data;
  if(pair->getSwitch()) return xlabel;
                        return ylabel;
}


static long scale_update(void *data)
{
  return NearestInteger(text_update(data));
}


static long scale_sense_update(void *data)      // also for text
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->showingVector(TP_SEL);
}


static long scale_min_update(void *data)
{
  TpStatfilePair *pair = (TpStatfilePair*)data;
  if(pair->getSwitch()) return NearestInteger(pair->getX1());
                        return NearestInteger(pair->getY1());
}


static long scale_max_update(void *data)
{
  TpStatfilePair *pair = (TpStatfilePair*)data;
  if(pair->getSwitch()) return NearestInteger(pair->getXend());
                        return NearestInteger(pair->getYend());
}
 


SLDelay *TpGenShowGui::createRow1(TpPopupBase *pop, TpStatfilePair *pair)
{
  SLSmartForm       *obj  = new SLSmartForm(this, "tpgs_selection");

  SL2Scale *scale = new SL2Scale(obj, "tpgs_scale", 0, "",
                                 SLpScale::_HORIZONTAL, FALSE);
  SL2Text  *ftext = new SL2Text (obj, "tpgs_ftext", 0, "selected bin",
                                           SLpText::_FLOAT, 6);

  obj->attach(scale, obj, obj, obj , NULL);
  obj->attach(ftext, obj, obj, scale, obj, 10, 10);

  scale->setItrap     (scale_trap        , pair);
  ftext->setFtrap     (text_trap         , pair);
  scale->setupIvarFun (scale_update      , pair);
  ftext->setupFvarFun (text_update       , pair);
  ftext->setupLabelFun(text_label_update , pair);
  scale->setupSenseFun(scale_sense_update, pop);
  ftext->setupSenseFun(scale_sense_update, pop);
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


SLDelay *TpGenShowGui::createRow2(TpPopupBase *pop)
{
  SLSmartForm *obj  = new SLSmartForm(this, "tpgs_show");

  SLpToggle *toggle = new SLpToggle(obj, "tpgs_toggle", 0, "show overlays");

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
  TpStatfilePair *pair = (TpStatfilePair*)data;
  pair->setOverlayAlignment(newvar);
}


static long align_update(void *data)
{
  TpStatfilePair *pair = (TpStatfilePair*)data;
  return pair->getOverlayAlignment();
}


static char *align_push1(void *data)
{
  static char xlabel[] = "align matching Y bin";
  static char ylabel[] = "align matching X bin";
  TpStatfilePair *pair = (TpStatfilePair*)data;
  if(pair->getSwitch()) return xlabel;
                        return ylabel;
}


static char *align_push2(void *data)
{
  static char xlabel[] = "align matching Y-X bin";
  static char ylabel[] = "align matching X-Y bin";
  TpStatfilePair *pair = (TpStatfilePair*)data;
  if(pair->getSwitch()) return xlabel;
                        return ylabel;
}


SLDelay *TpGenShowGui::createRow3(TpStatfilePair *pair)
{
  SLpOption *obj = new SLpOption(this, "tpgs_align");

  SLpPush *push1 = obj->addOption("align matching X bin"  , XMATCH);
  SLpPush *push2 = obj->addOption("align matching X-Y bin", XYDIFF);

  obj->setItrap     (align_trap  , pair);
  obj->setupIvarFun (align_update, pair);

  push1->setupLabelFun(align_push1, pair);
  push2->setupLabelFun(align_push2, pair);
  return obj;
}



//------------------- create row 4 --------------------------//
//------------------- create row 4 --------------------------//
//------------------- create row 4 --------------------------//


static void switch_trap(void *data, long/*ident*/, long/*oldvar*/, long newvar)
{
  TpStatfilePair *pair = (TpStatfilePair*)data;
  pair->setSwitch(newvar);
}


static long switch_update(void *data)
{
  TpStatfilePair *pair = (TpStatfilePair*)data;
  return pair->getSwitch();
}


SLDelay *TpGenShowGui::createRow4(TpStatfilePair *pair)
{
  SLpToggle *obj = new SLpToggle(this, "switch X and Y for overlays");

  obj->setItrap     (switch_trap  , pair);
  obj->setupIvarFun (switch_update, pair);
  return obj;
}




//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TpGenShowGui::TpGenShowGui(SLDelay *slparent,
                         TpPopupBase *pop, TpStatfilePair *pair)
       : SLSmartForm(slparent, "tp_gen_show_gui", NULL, TRUE),
                          _tog1    (NULL),
                          _tog2    (NULL),
                          _tog3    (NULL),
                          _tog4    (NULL),
                          _tog5    (NULL)
{
  assert(slparent && pop && pair);

  SLpLabel *label = new SLpLabel  (this, "Overlay Choices");
  SLDelay  *col1  = createColumn1 (pop, pair);
  SLDelay  *row1  = createRow1    (pop, pair);
  SLDelay  *row2  = createRow2    (pop);
  SLDelay  *row3  = createRow3    (pair);
  SLDelay  *row4  = createRow4    (pair);

  attach(label, this, this, this , NULL, 0, 0,  0,  0);
  attach(col1 , this, this, label, NULL, 0, 0,  0,  0);
  attach(row1 , this, this, col1 , NULL, 0, 0,  0,  0);
  attach(row2 , this, this, row1 , NULL, 0, 0,  0,  0);
  attach(row3 , this, this, row2 , NULL, 0, 0,  0,  0);
  attach(row4 , this, this, row3 , this, 0, 0,  0,  0);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpGenShowGui::~TpGenShowGui()
{
}




//----------------------------- make ----------------------------//
//----------------------------- make ----------------------------//
//----------------------------- make ----------------------------//


Widget TpGenShowGui::make(Widget p)
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
