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

//---------------------- tp_mute_show_gui.cc ------------------------//
//---------------------- tp_mute_show_gui.cc ------------------------//
//---------------------- tp_mute_show_gui.cc ------------------------//

//         implementation file for the TpMuteShowGui class
//               derived from the SLSmartForm class
//                        subdirectory pick


#include "pick/tp_mute_show_gui.hh"
#include "cprim.h"
#include "named_constants.h"
#include "pick/tp_mute_pair.hh"
#include "pick/tp_popup_base.hh"
#include "pick/tp_resources.hh"
#include "sl/slp_label.hh"
#include "sl/sl2_text.hh"
#include "sl/sl2_scale.hh"
#include "sl/slp_option.hh"
#include "sl/slp_toggle.hh"
#include "sl/sl_column.hh"



//------------------- create column 1 --------------------------//
//------------------- create column 1 --------------------------//
//------------------- create column 1 --------------------------//


static void overlays_trap(void *data, long ident, long /* oldvar */,
                          long newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setShowVector((int)ident, (Boolean)newvar);
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
TOG_UPDATE(overlays_update6, TP_REF )




SLDelay *TpMuteShowGui::createColumn1(TpPopupBase *pop)
{
  TpResources::setFallbackForeground("tpms_tog1", TP_CURR);
  TpResources::setFallbackForeground("tpms_tog2", TP_ORIG);
  TpResources::setFallbackForeground("tpms_tog3", TP_PREV);
  TpResources::setFallbackForeground("tpms_tog4", TP_NEXT);
  TpResources::setFallbackForeground("tpms_tog5", TP_SEL );
  TpResources::setFallbackForeground("tpms_tog6", TP_REF );
  SLpToggle *tog1, *tog2, *tog3, *tog4, *tog5, *tog6;

  SLColumn *obj = new SLColumn(this, "overlays");
  tog1 = new SLpToggle(obj, "tpms_tog1", TP_CURR, "show current picks"     );
  tog2 = new SLpToggle(obj, "tpms_tog2", TP_ORIG, "overlay original picks" );
  tog3 = new SLpToggle(obj, "tpms_tog3", TP_PREV, "overlay previous gather");
  tog4 = new SLpToggle(obj, "tpms_tog4", TP_NEXT, "overlay next gather"    );
  tog5 = new SLpToggle(obj, "tpms_tog5", TP_SEL , "overlay selected gather");
  tog6 = new SLpToggle(obj, "tpms_tog6", TP_REF , "overlay reference file" );

  tog1->setItrap     (overlays_trap   , pop);
  tog2->setItrap     (overlays_trap   , pop);
  tog3->setItrap     (overlays_trap   , pop);
  tog4->setItrap     (overlays_trap   , pop);
  tog5->setItrap     (overlays_trap   , pop);
  tog6->setItrap     (overlays_trap   , pop);

  tog1->setupIvarFun (overlays_update1, pop);
  tog2->setupIvarFun (overlays_update2, pop);
  tog3->setupIvarFun (overlays_update3, pop);
  tog4->setupIvarFun (overlays_update4, pop);
  tog5->setupIvarFun (overlays_update5, pop);
  tog6->setupIvarFun (overlays_update6, pop);
  tog6->setToggleValue(1L);

  return obj;
}



//------------------- create row 1 --------------------------//
//------------------- create row 1 --------------------------//
//------------------- create row 1 --------------------------//


static void text1_trap(void *data, long /* ident */, float /* oldvar */,
                       float newvar)
{
  TpMutePair *pair = (TpMutePair*)data;
  pair->setYsel(newvar, TRUE);
}
static void text2_trap(void *data, long /* ident */, float /* oldvar */,
                       float newvar)
{
  TpMutePair *pair = (TpMutePair*)data;
  pair->setZsel(newvar, TRUE);
}


static void scale1_trap(void *data, long ident, long oldvar, long newvar)
{
  text1_trap(data, ident, (float)oldvar, (float)newvar);
}
static void scale2_trap(void *data, long ident, long oldvar, long newvar)
{
  text2_trap(data, ident, (float)oldvar, (float)newvar);
}


static float text1_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  return pair->getYsel();
}
static float text2_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  return pair->getZsel();
}


static long scale1_update(void *data)
{
  return NearestInteger(text1_update(data));
}
static long scale2_update(void *data)
{
  return NearestInteger(text2_update(data));
}


static long scale_sense_update(void *data)    // also for text
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return (pop->pickingInProgress() && pop->showingVector(TP_SEL));
}



static long scale1_min_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  return NearestInteger(pair->getYmin());
}
static long scale2_min_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  return NearestInteger(pair->getZmin());
}


static long scale1_max_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  return NearestInteger(pair->getYmax());
}
static long scale2_max_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  return NearestInteger(pair->getZmax());
}
 


SLDelay *TpMuteShowGui::createRow1(TpPopupBase *pop, TpMutePair *pair)
{
  TpResources::setFallbackForeground("tpms_s1", TP_SEL);
  TpResources::setFallbackForeground("tpms_f1", TP_SEL);
  TpResources::setFallbackForeground("tpms_s2", TP_SEL);
  TpResources::setFallbackForeground("tpms_f2", TP_SEL);

  SLSmartForm *obj = new SLSmartForm(this, "tpms_selection");

  SL2Scale *scale1 = new SL2Scale(obj, "tpms_s1", 0, "",
                                 SLpScale::_HORIZONTAL, FALSE);
  SL2Text  *ftext1 = new SL2Text (obj, "tpms_f1", 0, "inline bin",
                                           SLpText::_FLOAT, 6);

  SL2Scale *scale2 = new SL2Scale(obj, "tpms_s2", 0, "",
                                 SLpScale::_HORIZONTAL, FALSE);
  SL2Text  *ftext2 = new SL2Text (obj, "tpms_f2", 0, "crossline bin",
                                           SLpText::_FLOAT, 6);

  obj->attach(scale1, obj, obj, obj   , NULL);
  obj->attach(ftext1, obj, obj, scale1, NULL, 20, 20);
  obj->attach(scale2, obj, obj, ftext1, NULL);
  obj->attach(ftext2, obj, obj, scale2, obj , 15, 15);

  scale1->setItrap     (scale1_trap       , pair);
  scale2->setItrap     (scale2_trap       , pair);
  ftext1->setFtrap     (text1_trap        , pair);
  ftext2->setFtrap     (text2_trap        , pair);
  scale1->setupIvarFun (scale1_update     , pair);
  scale2->setupIvarFun (scale2_update     , pair);
  ftext1->setupFvarFun (text1_update      , pair);
  ftext2->setupFvarFun (text2_update      , pair);
  scale1->setupSenseFun(scale_sense_update, pop);
  scale2->setupSenseFun(scale_sense_update, pop);
  ftext1->setupSenseFun(scale_sense_update, pop);
  ftext2->setupSenseFun(scale_sense_update, pop);
  scale1->setupIminFun (scale1_min_update , pair);
  scale2->setupIminFun (scale2_min_update , pair);
  scale1->setupImaxFun (scale1_max_update , pair);
  scale2->setupImaxFun (scale2_max_update , pair);
  return obj;
}



//------------------- create row 2 --------------------------//
//------------------- create row 2 --------------------------//
//------------------- create row 2 --------------------------//


static void show_trap(void *data, long /* ident */, long /* oldvar */,
                      long newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setShowOverlays((Boolean)newvar);
}


static long show_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->showingOverlays();
}


SLDelay *TpMuteShowGui::createRow2(TpPopupBase *pop)
{
  SLSmartForm *obj  = new SLSmartForm(this, "tpms_show");

  SLpToggle *toggle = new SLpToggle(obj, "tpms_toggle", 0, "show overlays");

  obj->showEvenSpacing();

  toggle->setItrap     (show_trap  , pop);
  toggle->setupIvarFun (show_update, pop);
  return obj;
}



//------------------- create row 3 --------------------------//
//------------------- create row 3 --------------------------//
//------------------- create row 3 --------------------------//

#define SWITCH_FALSE  1
#define SWITCH_TRUE   2

static void switch_trap(void *data, long /* ident */, long /* oldvar */,
                        long newvar)
{
  TpMutePair *pair = (TpMutePair*)data;
  if(newvar == SWITCH_TRUE) pair->setSwitch(TRUE);
  else                      pair->setSwitch(FALSE);
}


static long switch_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  if(pair->getSwitch()) return SWITCH_TRUE;
  return SWITCH_FALSE;
}


SLDelay *TpMuteShowGui::createRow3(TpMutePair *pair)
{
  TpResources::setFallbackForeground("tpms_option", TP_PREV);

  SLpOption *option = new SLpOption(this, "tpms_option");

  option->addOption("prev/next inline bins"   , SWITCH_FALSE);
  option->addOption("prev/next crossline bins", SWITCH_TRUE );

  option->setItrap     (switch_trap  , pair);
  option->setupIvarFun (switch_update, pair);
  ////////return obj;
  return option;
}



//------------------- create row 4 --------------------------//
//------------------- create row 4 --------------------------//
//------------------- create row 4 --------------------------//

#define INTERP_FALSE  1
#define INTERP_TRUE   2

static void interp_trap(void *data, long /* ident */, long /* oldvar */,
                        long newvar)
{
  TpMutePair *pair = (TpMutePair*)data;
  if(newvar == INTERP_TRUE) pair->setInterp(TRUE);
  else                      pair->setInterp(FALSE);
}


static long interp_update(void *data)
{
  TpMutePair *pair = (TpMutePair*)data;
  if(pair->getInterp()) return INTERP_TRUE;
  return INTERP_FALSE;
}


SLDelay *TpMuteShowGui::createRow4(TpMutePair *pair)
{
  SLpOption *option = new SLpOption(this, "tpms_interp");

  option->addOption("interpolate in offset direction", INTERP_FALSE);
  option->addOption("interpolate/extrapolate fully"  , INTERP_TRUE );

  option->setItrap     (interp_trap  , pair);
  option->setupIvarFun (interp_update, pair);
  return option;
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TpMuteShowGui::TpMuteShowGui(SLDelay *slparent,
                         TpPopupBase *pop, TpMutePair *pair)
       : SLSmartForm(slparent, "tp_mute_show_gui", NULL, TRUE)
{
  assert(slparent && pop && pair);

  SLpLabel *label = new SLpLabel  (this, "Overlay Choices");
  SLDelay  *col1  = createColumn1 (pop);
  SLDelay  *row1  = createRow1    (pop, pair);
  SLDelay  *row2  = createRow2    (pop);
  SLDelay  *row3  = createRow3    (pair);
  SLDelay  *row4  = createRow4    (pair);

  attach(label, this, this, this , NULL, 0, 0,  0,  0);
  attach(col1 , this, this, label, NULL, 0, 0,  0,  0);
  attach(row1 , this, this, col1 , NULL, 0, 0,  0,  0);
  attach(row3 , this, this, row1 , NULL, 0, 0,  0,  0);
  attach(row2 , this, this, row3 , NULL, 0, 0,  0,  0);
  attach(row4 , this, this, row2 , this, 0, 0,  0,  0);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpMuteShowGui::~TpMuteShowGui()
{
}




//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
