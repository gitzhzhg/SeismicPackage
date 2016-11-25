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
// class that creates the color extrapolation gui for color bar builder

#include "color/cbb_col_ext_gui.hh"
#include "color/cbb_col_sys_gui.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_scale_text_arrow.hh"
#include "wproc.h"
#include <Xm/Label.h>

static String defres[] = {
  "*ce_title.labelString:         Color Extrapolation",
  "*cc0.labelString:              R",
  "*cc1.labelString:              G",
  "*cc2.labelString:              B",
/*
  "*increasing.labelString:       Increasing",
  "*decreasing.labelString:       Decreasing",
  "*increasing_cc0.labelString:   ",
  "*decreasing_cc0.labelString:   ",
  "*increasing_cc1.labelString:   ",
  "*decreasing_cc1.labelString:   ",
*/
  "*increasing_cc2.labelString:   Increasing",
  "*decreasing_cc2.labelString:   Decreasing",
  "*cycles.labelString:           Cycles",
  0};

static SLRadio ext_cc0[] = {
  { "increasing_cc0", CBBColExtGui::INCREASING_CC0 },
  { "decreasing_cc0", CBBColExtGui::DECREASING_CC0 }
};

static SLRadio ext_cc1[] = {
  { "increasing_cc1", CBBColExtGui::INCREASING_CC1 },
  { "decreasing_cc1", CBBColExtGui::DECREASING_CC1 }
};

static SLRadio ext_cc2[] = {
  { "increasing_cc2", CBBColExtGui::INCREASING_CC2 },
  { "decreasing_cc2", CBBColExtGui::DECREASING_CC2 }
};

CBBColExtGui::CBBColExtGui (Widget parent, char *name, HelpCtx hctx,
  ColorBarBuilderPop *cbb_pop) :
  SLForm (parent, name, hctx, True),
  _cbb_pop  (cbb_pop),
  _cc0      (0),
  _cc1      (0),
  _cc2      (0),
  _status   (1)
{
// set the default resources
  setDefaultResources (parent, name, defres);
  init ();
}

CBBColExtGui::CBBColExtGui (SLDelay *container, char *name, HelpCtx hctx,
  ColorBarBuilderPop *cbb_pop) :
  SLForm (container, name, hctx, True),
  _cbb_pop  (cbb_pop),
  _cc0      (0),
  _cc1      (0),
  _cc2      (0),
  _status   (1)
{
// set the default resources
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
    make (container->topWidget());
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
  init ();
}

void CBBColExtGui::init ()
{
// color coordinate 0 radio box
  _ext_cc0_box = new SLRadioBox (this, "ext_cc0", getHelpCtx(), ext_cc0,
     XtNumber(ext_cc0), NULL, True, False);

// color coordinate 1 radio box
  _ext_cc1_box = new SLRadioBox (this, "ext_cc1", getHelpCtx(), ext_cc1,
     XtNumber(ext_cc1), NULL, True, False);

// color coordinate 2 radio box
  _ext_cc2_box = new SLRadioBox (this, "ext_cc2", getHelpCtx(), ext_cc2,
     XtNumber(ext_cc2), NULL, True, False);

  CBBColExtSettings *cbb_ces = _cbb_pop->colExtSetter ();
// color coordinate 0 arrow title
  _ext_cc0_arrow = new SLScaleTextArrow (this, "cc0_arrow", getHelpCtx(),
    cbb_ces->CC0CyclesPtr(), True, cbb_ces->CC0Cycles(),
    cbb_ces->CC0CyclesMin(), cbb_ces->CC0CyclesMax(),
    cbb_ces->CC0CyclesInc(), (int)1, (int)3, XmVERTICAL, 0, 75);

// color coordinate 1 arrow title
  _ext_cc1_arrow = new SLScaleTextArrow (this, "cc1_arrow", getHelpCtx(),
    cbb_ces->CC1CyclesPtr(), True, cbb_ces->CC1Cycles(),
    cbb_ces->CC1CyclesMin(), cbb_ces->CC1CyclesMax(),
    cbb_ces->CC1CyclesInc(), (int)1, (int)3, XmVERTICAL, 0, 75);

// color coordinate 2 arrow title
  _ext_cc2_arrow = new SLScaleTextArrow (this, "cc2_arrow", getHelpCtx(),
    cbb_ces->CC2CyclesPtr(), True, cbb_ces->CC2Cycles(),
    cbb_ces->CC2CyclesMin(), cbb_ces->CC2CyclesMax(),
    cbb_ces->CC2CyclesInc(), (int)1, (int)3, XmVERTICAL, 0, 75);

  _status = setState ();

}

CBBColExtGui::~CBBColExtGui ()
{
  if (_ext_cc0_box)   delete _ext_cc0_box,   _ext_cc0_box   = 0;
  if (_ext_cc1_box)   delete _ext_cc1_box,   _ext_cc1_box   = 0;
  if (_ext_cc2_box)   delete _ext_cc2_box,   _ext_cc2_box   = 0;
}

Widget CBBColExtGui::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  SLForm::make (parent);

  _cc0 = XtVaCreateManagedWidget ("cc0",
                         xmLabelWidgetClass,  topWidget(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _ext_cc0_box->W(),
                         XmNleftOffset,       10,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _ext_cc0_box->W(),
                         NULL);

  _cc1 = XtVaCreateManagedWidget ("cc1",
                         xmLabelWidgetClass,  topWidget(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _ext_cc1_box->W(),
                         XmNleftOffset,       10,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _ext_cc1_box->W(),
                         NULL);

  _cc2 = XtVaCreateManagedWidget ("cc2",
                         xmLabelWidgetClass,  topWidget(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _ext_cc2_box->W(),
                         XmNleftOffset,       10,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _ext_cc2_box->W(),
                         NULL);

  Widget cycles = XtVaCreateManagedWidget ("cycles",
                         xmLabelWidgetClass,  topWidget(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _ext_cc2_arrow->W(),
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _ext_cc2_arrow->W(),
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _ext_cc2_arrow->W(),
                         NULL);

  Widget ce_title = XtVaCreateManagedWidget ("ce_title",
                         xmLabelWidgetClass,  topWidget(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _ext_cc0_arrow->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _ext_cc2_box->W(),
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _cc0,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (_ext_cc0_box->W(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _ext_cc0_arrow->W(),
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _ext_cc0_arrow->W(),
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (_ext_cc1_box->W(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _ext_cc1_arrow->W(),
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _ext_cc1_arrow->W(),
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (_ext_cc2_box->W(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _ext_cc2_arrow->W(),
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _ext_cc2_arrow->W(),
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (_ext_cc0_arrow->W(),
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNleftOffset,       1,
                         XmNbottomAttachment, XmATTACH_FORM,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (_ext_cc1_arrow->W(),
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _ext_cc0_arrow->W(),
                         XmNleftOffset,       1,
                         XmNbottomAttachment, XmATTACH_FORM,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (_ext_cc2_arrow->W(),
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _ext_cc1_arrow->W(),
                         XmNleftOffset,       1,
                         XmNbottomAttachment, XmATTACH_FORM,
                         XmNbottomOffset,     1,
                         NULL);

  _status = setState ();

  wprocShowMsg
    (_ext_cc0_box->GetRadioWidget(CBBColExtGui::DECREASING_CC0), " ");
  wprocShowMsg
    (_ext_cc0_box->GetRadioWidget(CBBColExtGui::INCREASING_CC0), " ");
  wprocShowMsg
    (_ext_cc1_box->GetRadioWidget(CBBColExtGui::DECREASING_CC1), " ");
  wprocShowMsg
    (_ext_cc1_box->GetRadioWidget(CBBColExtGui::INCREASING_CC1), " ");

  return topWidget ();

}

Boolean CBBColExtGui::notifyComplex (SLDelay *obj, int ident)
{
  CBBColExtSettings *cbb_ces = _cbb_pop->colExtSetter ();

  if (obj == _ext_cc0_box) {
    if (ident == INCREASING_CC0) {
      cbb_ces->setCC0 (CBBColExtSettings::INCREASING);
    }
    else if (ident == DECREASING_CC0) {
      cbb_ces->setCC0 (CBBColExtSettings::DECREASING);
    }
  }
  else if (obj == _ext_cc1_box) {
    if (ident == INCREASING_CC1) {
      cbb_ces->setCC1 (CBBColExtSettings::INCREASING);
    }
    else if (ident == DECREASING_CC1) {
      cbb_ces->setCC1 (CBBColExtSettings::DECREASING);
    }
  }
  else if (obj == _ext_cc2_box) {
    if (ident == INCREASING_CC2) {
      cbb_ces->setCC2 (CBBColExtSettings::INCREASING);
    }
    else if (ident == DECREASING_CC2) {
      cbb_ces->setCC2 (CBBColExtSettings::DECREASING);
    }
  }
  else if (obj == _ext_cc0_arrow) {
    cbb_ces->setCC0Cycles (_ext_cc0_arrow->getValue());
  }
  else if (obj == _ext_cc1_arrow) {
    cbb_ces->setCC1Cycles (_ext_cc1_arrow->getValue());
  }
  else if (obj == _ext_cc2_arrow) {
    cbb_ces->setCC2Cycles (_ext_cc2_arrow->getValue());
  }
  return True;
}

int CBBColExtGui::setState ()
{
  CBBColSysGui *cbb_csg = _cbb_pop->colSysGui ();
  if (!cbb_csg) return (int)0;
  CBBColExtSettings *cbb_ces = _cbb_pop->colExtSetter ();
  if (!cbb_ces) return (int)0;

  if (cbb_csg->GRAYSelected() && cbb_csg->BHSSelected()) {
    _ext_cc0_box->setSensitivity (True);
    _ext_cc1_box->setSensitivity (True);
    _ext_cc2_box->setSensitivity (True);
    _ext_cc0_arrow->setSensitivity (True);
    _ext_cc1_arrow->setSensitivity (False);
    _ext_cc2_arrow->setSensitivity (False);

    _ext_cc0_arrow->setRange (cbb_ces->CC0CyclesMin(), cbb_ces->CC0CyclesMax());
    _ext_cc0_arrow->setIncrement (cbb_ces->CC0CyclesInc());

    _ext_cc1_arrow->setRange (float(0), float(0));
    _ext_cc2_arrow->setRange (float(0), float(0));

    _ext_cc1_arrow->setIncrement (float(0));
    _ext_cc2_arrow->setIncrement (float(0));

    selectIncreasing (ColorBarBuilderPop::CC1);
    selectIncreasing (ColorBarBuilderPop::CC2);
    setCycles (ColorBarBuilderPop::CC1, (float)0);
    setCycles (ColorBarBuilderPop::CC2, (float)0);
  }
  else {
    _ext_cc0_box->setSensitivity (True);
    _ext_cc1_box->setSensitivity (True);
    _ext_cc2_box->setSensitivity (True);
    _ext_cc0_arrow->setSensitivity (True);
    _ext_cc1_arrow->setSensitivity (True);
    _ext_cc2_arrow->setSensitivity (True);

    _ext_cc0_arrow->setRange (cbb_ces->CC0CyclesMin(), cbb_ces->CC0CyclesMax());
    _ext_cc0_arrow->setIncrement (cbb_ces->CC0CyclesInc());

    if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
      _ext_cc1_arrow->setRange (cbb_ces->CC0CyclesMin(),
         cbb_ces->CC0CyclesMax());
      _ext_cc2_arrow->setRange (cbb_ces->CC0CyclesMin(),
        cbb_ces->CC0CyclesMax());

      _ext_cc1_arrow->setIncrement (cbb_ces->CC0CyclesInc());
      _ext_cc2_arrow->setIncrement (cbb_ces->CC0CyclesInc());
    }
    else {
      _ext_cc1_arrow->setRange (cbb_ces->CC1CyclesMin(),
        cbb_ces->CC1CyclesMax());
      _ext_cc2_arrow->setRange (cbb_ces->CC2CyclesMin(),
        cbb_ces->CC2CyclesMax());

      _ext_cc1_arrow->setIncrement (cbb_ces->CC1CyclesInc());
      _ext_cc2_arrow->setIncrement (cbb_ces->CC2CyclesInc());
    }
  }

  if (_cc0 && _cc1 && _cc2) {
         if (cbb_csg->RGBSelected()) {
      wprocVAShowMsg (_cc0, "R");
      wprocVAShowMsg (_cc1, "G");
      wprocVAShowMsg (_cc2, "B");
    }
    else if (cbb_csg->BHSSelected()) {
      wprocVAShowMsg (_cc0, "B");
      wprocVAShowMsg (_cc1, "H");
      wprocVAShowMsg (_cc2, "S");
    }
  }


       if (cbb_ces->CC0() == CBBColExtSettings::INCREASING) {
    selectIncreasing (ColorBarBuilderPop::CC0);
  }
  else if (cbb_ces->CC0() == CBBColExtSettings::DECREASING) {
    selectDecreasing (ColorBarBuilderPop::CC0);
  }

  if (!cbb_csg->GRAYSelected()) {
         if (cbb_ces->CC1() == CBBColExtSettings::INCREASING) {
      selectIncreasing (ColorBarBuilderPop::CC1);
    }
    else if (cbb_ces->CC1() == CBBColExtSettings::DECREASING) {
      selectDecreasing (ColorBarBuilderPop::CC1);
    }

         if (cbb_ces->CC2() == CBBColExtSettings::INCREASING) {
      selectIncreasing (ColorBarBuilderPop::CC2);
    }
    else if (cbb_ces->CC2() == CBBColExtSettings::DECREASING) {
      selectDecreasing (ColorBarBuilderPop::CC2);
    }
  }

  setCycles (ColorBarBuilderPop::CC0, cbb_ces->CC0Cycles());
  if (!cbb_csg->GRAYSelected()) {
    setCycles (ColorBarBuilderPop::CC1, cbb_ces->CC1Cycles());
    setCycles (ColorBarBuilderPop::CC2, cbb_ces->CC2Cycles());
  }

  return (int)1;
}

void CBBColExtGui::selectIncreasing
  (ColorBarBuilderPop::ColorCoordinate which_cc)
{
  CBBColSysGui *cbb_csg = _cbb_pop->colSysGui ();

  if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
    _ext_cc0_box->SetRadio (INCREASING_CC0);
    _ext_cc1_box->SetRadio (INCREASING_CC1);
    _ext_cc2_box->SetRadio (INCREASING_CC2);
  }
  else {

    switch (which_cc) {
    case ColorBarBuilderPop::CC0:
      _ext_cc0_box->SetRadio (INCREASING_CC0);
      break;
    case ColorBarBuilderPop::CC1:
      _ext_cc1_box->SetRadio (INCREASING_CC1);
      break;
    case ColorBarBuilderPop::CC2:
      _ext_cc2_box->SetRadio (INCREASING_CC2);
      break;
    default:
      break;
    }
  }
}

void CBBColExtGui::selectDecreasing
  (ColorBarBuilderPop::ColorCoordinate which_cc)
{
  CBBColSysGui *cbb_csg = _cbb_pop->colSysGui ();

  if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
    _ext_cc0_box->SetRadio (DECREASING_CC0);
    _ext_cc1_box->SetRadio (DECREASING_CC1);
    _ext_cc2_box->SetRadio (DECREASING_CC2);
  }
  else {

    switch (which_cc) {
    case ColorBarBuilderPop::CC0:
      _ext_cc0_box->SetRadio (DECREASING_CC0);
      break;
    case ColorBarBuilderPop::CC1:
      _ext_cc1_box->SetRadio (DECREASING_CC1);
      break;
    case ColorBarBuilderPop::CC2:
      _ext_cc2_box->SetRadio (DECREASING_CC2);
      break;
    default:
      break;
    }
  }
}

int CBBColExtGui::increasingSelected
  (ColorBarBuilderPop::ColorCoordinate which_cc)
{
  switch (which_cc) {
  case ColorBarBuilderPop::CC0:
    return (int)(_ext_cc0_box->WhichSelected() == INCREASING_CC0);
  case ColorBarBuilderPop::CC1:
    return (int)(_ext_cc1_box->WhichSelected() == INCREASING_CC1);
  case ColorBarBuilderPop::CC2:
    return (int)(_ext_cc2_box->WhichSelected() == INCREASING_CC2);
  default:
    return (int)0;
  }
}

int CBBColExtGui::decreasingSelected
  (ColorBarBuilderPop::ColorCoordinate which_cc)
{
  switch (which_cc) {
  case ColorBarBuilderPop::CC0:
    return (int)(_ext_cc0_box->WhichSelected() == DECREASING_CC0);
  case ColorBarBuilderPop::CC1:
    return (int)(_ext_cc1_box->WhichSelected() == DECREASING_CC1);
  case ColorBarBuilderPop::CC2:
    return (int)(_ext_cc2_box->WhichSelected() == DECREASING_CC2);
  default:
    return (int)0;
  }
}

void CBBColExtGui::setCycles
  (ColorBarBuilderPop::ColorCoordinate which_cc, float cycles)
{
  CBBColSysGui *cbb_csg = _cbb_pop->colSysGui ();

  if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
    _ext_cc0_arrow->setValue (cycles);
    _ext_cc1_arrow->setValue (cycles);
    _ext_cc2_arrow->setValue (cycles);
  }
  else {

    switch (which_cc) {
    case ColorBarBuilderPop::CC0:
      _ext_cc0_arrow->setValue (cycles);
      break;
    case ColorBarBuilderPop::CC1:
      _ext_cc1_arrow->setValue (cycles);
      break;
    case ColorBarBuilderPop::CC2:
      _ext_cc2_arrow->setValue (cycles);
      break;
    default:
      break;
    }
  }
}

float CBBColExtGui::cycles
  (ColorBarBuilderPop::ColorCoordinate which_cc)
{
  switch (which_cc) {
  case ColorBarBuilderPop::CC0:
    return _ext_cc0_arrow->getValue();
  case ColorBarBuilderPop::CC1:
    return _ext_cc1_arrow->getValue();
  case ColorBarBuilderPop::CC2:
    return _ext_cc2_arrow->getValue();
  default:
    return (int)0;
  }
}
