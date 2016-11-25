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

//---------------------- tp_gen_auto_gui.cc ------------------------//
//---------------------- tp_gen_auto_gui.cc ------------------------//
//---------------------- tp_gen_auto_gui.cc ------------------------//

//         implementation file for the TpGenAutoGui class
//               derived from the SLSmartForm class
//                        subdirectory pick


#include "pick/tp_gen_auto_gui.hh"
#include "pick/tp_statfile_pair.hh"
#include "pick/tp_popup_base.hh"
#include "sl/slp_push.hh"
#include "sl/slp_label.hh"
#include "sl/slp_option.hh"
#include "sl/sl2_text.hh"
#include "cprim.h"

enum { _SHORT_MIN, _SHORT_MAX, _LONG_MIN, _LONG_MAX };



//-------------------------- static traps -----------------------//
//-------------------------- static traps -----------------------//
//-------------------------- static traps -----------------------//

static void pickmode_trap(void *data,long/*ident*/,long/*oldvar*/,long newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setPickMode(newvar);
}


static void automode_trap(void *data,long/*ident*/,long/*oldvar*/,long newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setAutoMode(newvar);
}



static void threshhold_trap(void *data,long/*ident*/,float/*oldvar*/,
                            float newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setThreshhold(newvar);
}



static void sigDiff_trap(void *data,long/*ident*/,float/*oldvar*/,
                            float newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setSigDiff(newvar);
}



static void orf_trap(void *data,long/*ident*/,float/*oldvar*/,
                            float newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setORF(newvar);
}



static void short_long_min_max_trap(void *data,long ident,float/*oldvar*/,
                            float newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;

  switch (ident)
  {
    case _SHORT_MIN:    pop->setShortMin(newvar);       break;
    case _SHORT_MAX:    pop->setShortMax(newvar);       break;
    case  _LONG_MIN:    pop-> setLongMin(newvar);       break;
    case  _LONG_MAX:    pop-> setLongMax(newvar);       break;
    default        :    assert(FALSE);
  }
}



static void poto_trap(void *data,long/*ident*/,float/*oldvar*/,
                            float newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setPoto(newvar);
}



static void outlier_trap(void *data,long/*ident*/,float/*oldvar*/,
                            float newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setOutlier(newvar);
}



static void snap_trap(void *data, long/*ident*/, long/*oldvar*/, long newvar)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  pop->setSnapChoice((int)newvar);
}



//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//
//--------------------- static update functions ------------------//

static long pickmode_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getPickMode();
}



static long automode_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getAutoMode();
}



static float threshhold_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getThreshhold();
}



static float sigDiff_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getSigDiff();
}



static float orf_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getORF();
}



static float shortMin_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getShortMin();
}



static float shortMax_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getShortMax();
}



static float longMin_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getLongMin();
}



static float longMax_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getLongMax();
}



static float poto_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getPoto();
}



static float outlier_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  return pop->getOutlier();
}



static long threshhold_sense_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  if(pop->getAutoMode() == FIRST_BREAK ||
     pop->getAutoMode() == FIRST_BREAK_NO_SNAP ||
     pop->getAutoMode() == FIRST_BREAK_CORR ||
     pop->getAutoMode() == COMBO ||
     pop->getAutoMode() == COMBO_CORR) return TRUE;
  return FALSE;
}



static long sigDiff_sense_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  if(pop->getAutoMode() == HURST_BREAK ||
     pop->getAutoMode() == HURST_BREAK_NO_SNAP ||
     pop->getAutoMode() == HURST_CORR ||
     pop->getAutoMode() == COMBO ||
     pop->getAutoMode() == COMBO_CORR) return TRUE;
  return FALSE;
}



static long orf_sense_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  if(pop->getAutoMode() == FIRST_BREAK_CORR ||
     pop->getAutoMode() == HURST_CORR ||
     pop->getAutoMode() == COMBO_CORR ||
     pop->getAutoMode() ==  PICK_CORR ||
     pop->getAutoMode() == CORRELATE) return TRUE;
  return FALSE;
}



static long short_long_min_max_sense_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  if(pop->getAutoMode() == FIRST_BREAK ||
     pop->getAutoMode() == FIRST_BREAK_NO_SNAP ||
     pop->getAutoMode() == FIRST_BREAK_CORR ||
     pop->getAutoMode() == HURST_BREAK ||
     pop->getAutoMode() == HURST_BREAK_NO_SNAP ||
     pop->getAutoMode() == HURST_CORR ||
     pop->getAutoMode() == COMBO ||
     pop->getAutoMode() == COMBO_CORR) return TRUE;
  return FALSE;
}



static long poto_sense_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  if(pop->getAutoMode() == COMBO ||
     pop->getAutoMode() == COMBO_CORR) return TRUE;
  return FALSE;
}



static long outlier_sense_update(void *data)
{
  TpPopupBase *pop = (TpPopupBase*)data;
  if(pop->getAutoMode() == FIRST_BREAK_CORR ||
     pop->getAutoMode() == HURST_CORR ||
     pop->getAutoMode() == COMBO_CORR ||
     pop->getAutoMode() == CORRELATE) return TRUE;
  return FALSE;
}



long TpGenAutoGui::snapUpdate(void *data)
{
  TpGenAutoGui *gui = (TpGenAutoGui*)data;
  gui->setSnapForeground(FALSE);
  return gui->_pop->getSnapChoice();
}



static char *snap_label3(void *data)
{
  static char xlabel[] = "snap to previous X bin";
  static char ylabel[] = "snap to previous Y bin";
  TpStatfilePair *pair = (TpStatfilePair*)data;
  if(pair->getSwitch()) return xlabel;
                        return ylabel;
}


static char *snap_label4(void *data)
{
  static char xlabel[] = "snap to next X bin";
  static char ylabel[] = "snap to next Y bin";
  TpStatfilePair *pair = (TpStatfilePair*)data;
  if(pair->getSwitch()) return xlabel;
                        return ylabel;
}


static char *snap_label5(void *data)
{
  static char xlabel[] = "snap to selected X bin";
  static char ylabel[] = "snap to selected Y bin";
  TpStatfilePair *pair = (TpStatfilePair*)data;
  if(pair->getSwitch()) return xlabel;
                        return ylabel;
}




//------------------ set snap foreground -----------------------//
//------------------ set snap foreground -----------------------//
//------------------ set snap foreground -----------------------//

void TpGenAutoGui::setSnapForeground(Boolean force)
{
  Widget w = _snap_option->W();
  if(!w) return;
  long snap = _pop->getSnapChoice();
  Pixel pixel;
  if(snap == TP_NONE) pixel = _original_foreground;
  else                pixel = TpResources::getPixel((int)snap);
  if(pixel != _current_foreground || force)
       {
       XtVaSetValues(w, XmNforeground, pixel, NULL);
       _current_foreground = pixel;
       }
}



//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//
//--------------------- constructor -------------------------------//


TpGenAutoGui::TpGenAutoGui(SLDelay *slparent, TpPopupBase *pop,
                                                TpStatfilePair *pair)
       : SLSmartForm(slparent, "tp_gen_auto_gui", NULL, TRUE),
                   _pop                  (pop),
                   _snap_option          (NULL),
                   _push1                (NULL),
                   _push2                (NULL),
                   _push3                (NULL),
                   _push4                (NULL),
                   _push5                (NULL),
                   _push6                (NULL),
                   _original_foreground  (0),
                   _current_foreground   (0)
{
  assert(slparent && pop && pair);

  SLpLabel *label = new SLpLabel  (this, "Automatic Picking Choices");
  SLpOption  *one = new SLpOption (this, "pickmode", 0, "hot spot:");
  SLpOption  *two = new SLpOption (this, "automode", 0, "method:");
  SL2Text  *three = new SL2Text   (this, "threshhold", 0, "threshhold:",
                                SLpText::_FLOAT, 6, 4);
  SL2Text   *five = new SL2Text   (this, "sig_diff", 0, "Hurst sig. diff.:",
                                SLpText::_FLOAT, 6, 4);
  SL2Text    *six = new SL2Text   (this, "orf", 0, "offset ratio factor:",
                                SLpText::_FLOAT, 6, 4);
  SL2Text  *seven = new SL2Text   (this, "short_min", _SHORT_MIN,
                                "short offset min.:", SLpText::_FLOAT, 6, 4);
  SL2Text  *eight = new SL2Text   (this, "short_max", _SHORT_MAX,
                                "short offset max.:", SLpText::_FLOAT, 6, 4);
  SL2Text  *nine  = new SL2Text   (this,  "long_min",  _LONG_MIN,
                                 "long offset min.:", SLpText::_FLOAT, 6, 4);
  SL2Text  *ten   = new SL2Text   (this,  "long_max",  _LONG_MAX,
                                 "long offset max.:", SLpText::_FLOAT, 6, 4);
  SL2Text  *eleven= new SL2Text   (this,  "poto",  0, "% offset thresh.:",
                                 SLpText::_FLOAT, 6, 2);
  SL2Text  *twelve= new SL2Text   (this,  "outlier",  0, "outlier:",
                                 SLpText::_FLOAT, 6, 2);
  SLpOption *four = new SLpOption (this, "snap");

  _snap_option = four;

  one->addOption("peak"             , PEAK    );
  one->addOption("trough"           , TROUGH  );
  one->addOption("pos zero crossing", POSITIVE);
  one->addOption("neg zero crossing", NEGATIVE);

  two->addOption("follow line"            , FOLLOW_LINE        );
  two->addOption("follow slope"           , FOLLOW_SLOPE       );
  two->addOption("follow curve"           , FOLLOW_CURVE       );
  two->addOption("first break"            , FIRST_BREAK        );
  two->addOption("first break no snap"    , FIRST_BREAK_NO_SNAP);
  two->addOption("first break with corr." , FIRST_BREAK_CORR   );
  two->addOption("Hurst 1st break"        , HURST_BREAK        );
  two->addOption("Hurst 1st break no snap", HURST_BREAK_NO_SNAP);
  two->addOption("Hurst with correlation",  HURST_CORR         );
  two->addOption("combo 1st break"        , COMBO              );
  two->addOption("combo with correlation" , COMBO_CORR         );
  two->addOption("pick with correlation"  , PICK_CORR          );
  two->addOption("correlate from non-zero", CORRELATE          );

  _push6 = four->addOption("(snap turned off)"       , TP_NONE);
  _push1 = four->addOption("snap to current picks"   , TP_CURR);
  _push2 = four->addOption("snap to original picks"  , TP_ORIG);
  _push3 = four->addOption("snap to previous profile", TP_PREV);
  _push4 = four->addOption("snap to next profile"    , TP_NEXT);
  _push5 = four->addOption("snap to selected profile", TP_SEL );

  one  ->setItrap     (pickmode_trap  , pop);
  one  ->setupIvarFun (pickmode_update, pop);

  two  ->setItrap     (automode_trap  , pop);
  two  ->setupIvarFun (automode_update, pop);

  three->setFtrap     (threshhold_trap        , pop);
  three->setupFvarFun (threshhold_update      , pop);
  three->setupSenseFun(threshhold_sense_update, pop);

  five ->setFtrap     (sigDiff_trap        , pop);
  five ->setupFvarFun (sigDiff_update      , pop);
  five ->setupSenseFun(sigDiff_sense_update, pop);

  six  ->setFtrap     (orf_trap        , pop);
  six  ->setupFvarFun (orf_update      , pop);
  six  ->setupSenseFun(orf_sense_update, pop);

  seven->setFtrap     (short_long_min_max_trap        , pop);
  seven->setupFvarFun (shortMin_update                , pop);
  seven->setupSenseFun(short_long_min_max_sense_update, pop);

  eight->setFtrap     (short_long_min_max_trap        , pop);
  eight->setupFvarFun (shortMax_update                , pop);
  eight->setupSenseFun(short_long_min_max_sense_update, pop);

  nine ->setFtrap     (short_long_min_max_trap        , pop);
  nine ->setupFvarFun ( longMin_update                , pop);
  nine ->setupSenseFun(short_long_min_max_sense_update, pop);

  ten  ->setFtrap     (short_long_min_max_trap        , pop);
  ten  ->setupFvarFun ( longMax_update                , pop);
  ten  ->setupSenseFun(short_long_min_max_sense_update, pop);

  eleven->setFtrap     (poto_trap                     , pop);
  eleven->setupFvarFun (poto_update                   , pop);
  eleven->setupSenseFun(poto_sense_update             , pop);

  twelve->setFtrap     (outlier_trap                  , pop);
  twelve->setupFvarFun (outlier_update                , pop);
  twelve->setupSenseFun(outlier_sense_update          , pop);

  four ->setItrap     (snap_trap , pop);
  four ->setupIvarFun (snapUpdate, this);

  _push3->setupLabelFun (snap_label3 , pair);
  _push4->setupLabelFun (snap_label4 , pair);
  _push5->setupLabelFun (snap_label5 , pair);

  attach(label , this, this, this  , NULL, 0,  0, 0, 0);
  attach(one   , this, this, label , NULL, 0,  0, 0, 0);
  attach(two   , this, this, one   , NULL, 0,  0, 0, 0);
  attach(three , NULL, this, two   , NULL, 0, 20, 0, 0);
  attach(five  , NULL, this, three , NULL, 0, 20, 0, 0);
  attach(six   , NULL, this, five  , NULL, 0, 20, 0, 0);
  attach(seven , NULL, this, six   , NULL, 0, 20, 0, 0);
  attach(eight , NULL, this, seven , NULL, 0, 20, 0, 0);
  attach(nine  , NULL, this, eight , NULL, 0, 20, 0, 0);
  attach(ten   , NULL, this, nine  , NULL, 0, 20, 0, 0);
  attach(eleven, NULL, this, ten   , NULL, 0, 20, 0, 0);
  attach(twelve, NULL, this, eleven, NULL, 0, 20, 0, 0);
  attach(four  , this, this, twelve, this, 0,  0, 0, 0);
}


//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//
//----------------------- destructor ---------------------------//


TpGenAutoGui::~TpGenAutoGui()
{
}



//----------------------------- make ----------------------------//
//----------------------------- make ----------------------------//
//----------------------------- make ----------------------------//


Widget TpGenAutoGui::make(Widget p)
{
  if(!made())
       {
       Widget w = SLSmartForm::make(p);
       TpResources::setForeground(TP_CURR, _push1);
       TpResources::setForeground(TP_ORIG, _push2);
       TpResources::setForeground(TP_PREV, _push3);
       TpResources::setForeground(TP_NEXT, _push4);
       TpResources::setForeground(TP_SEL , _push5);
       XtVaGetValues(_snap_option->W(),
                          XmNforeground, &_original_foreground, NULL);
       setSnapForeground(TRUE);
       }
  makeChildren();
  return topWidget();
}



//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
//---------------------------- end --------------------------------------//
