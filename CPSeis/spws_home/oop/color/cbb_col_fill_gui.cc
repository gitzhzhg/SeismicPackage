// class that creates the color fill gui for color bar builder
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

#include "color/cbb_col_fill_gui.hh"
#include "color/cbb_col_sys_gui.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_radio_box.hh"
#include "wproc.h"
#include <Xm/Label.h>

static String defres[] = {
  "*cf_title.labelString:         Color Fill",
  "*cc0.labelString:              R",
  "*cc1.labelString:              G",
  "*cc2.labelString:              B",
/*
  "*constant.labelString:         Constant   ",
  "*linear.labelString:           Linear     ",
  "*logarithmic.labelString:      Logarithmic",
  "*exponential.labelString:      Exponential",
  "*constant_cc0.labelString:     ",
  "*linear_cc0.labelString:       ",
  "*logarithmic_cc0.labelString:  ",
  "*exponential_cc0.labelString:  ",
  "*constant_cc1.labelString:     ",
  "*linear_cc1.labelString:       ",
  "*logarithmic_cc1.labelString:  ",
  "*exponential_cc1.labelString:  ",
*/
  "*constant_cc2.labelString:     Constant",
  "*linear_cc2.labelString:       Linear",
  "*logarithmic_cc2.labelString:  Logarithmic",
  "*exponential_cc2.labelString:  Exponential",
  0};

static SLRadio fill_cc0[] = {
  { "constant_cc0",     CBBColFillGui::CONSTANT_CC0    },
  { "linear_cc0",       CBBColFillGui::LINEAR_CC0      },
  { "logarithmic_cc0",  CBBColFillGui::LOGARITHMIC_CC0 },
  { "exponential_cc0",  CBBColFillGui::EXPONENTIAL_CC0 },
};

static SLRadio fill_cc1[] = {
  { "constant_cc1",     CBBColFillGui::CONSTANT_CC1    },
  { "linear_cc1",       CBBColFillGui::LINEAR_CC1      },
  { "logarithmic_cc1",  CBBColFillGui::LOGARITHMIC_CC1 },
  { "exponential_cc1",  CBBColFillGui::EXPONENTIAL_CC1 },
};

static SLRadio fill_cc2[] = {
  { "constant_cc2",     CBBColFillGui::CONSTANT_CC2    },
  { "linear_cc2",       CBBColFillGui::LINEAR_CC2      },
  { "logarithmic_cc2",  CBBColFillGui::LOGARITHMIC_CC2 },
  { "exponential_cc2",  CBBColFillGui::EXPONENTIAL_CC2 },
};

CBBColFillGui::CBBColFillGui (Widget parent, char *name, HelpCtx hctx,
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

CBBColFillGui::CBBColFillGui (SLDelay *container, char *name, HelpCtx hctx,
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

void CBBColFillGui::init ()
{
// color coordinate 0 radio box
  _fill_cc0_box = new SLRadioBox (this, "fill_cc0", getHelpCtx(), fill_cc0,
     XtNumber(fill_cc0), NULL, True, False);
  _fill_cc0_box->setComplexNotify (this);

// color coordinate 1 radio box
  _fill_cc1_box = new SLRadioBox (this, "fill_cc1", getHelpCtx(), fill_cc1,
     XtNumber(fill_cc1), NULL, True, False);
  _fill_cc1_box->setComplexNotify (this);

// color coordinate 2 radio box
  _fill_cc2_box = new SLRadioBox (this, "fill_cc2", getHelpCtx(), fill_cc2,
     XtNumber(fill_cc2), NULL, True, False);
  _fill_cc2_box->setComplexNotify (this);

  _status = setState ();

}

CBBColFillGui::~CBBColFillGui ()
{
  if (_fill_cc0_box) delete _fill_cc0_box, _fill_cc0_box = 0;
  if (_fill_cc1_box) delete _fill_cc1_box, _fill_cc1_box = 0;
  if (_fill_cc2_box) delete _fill_cc2_box, _fill_cc2_box = 0;
}

Widget CBBColFillGui::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  SLForm::make (parent);

  _cc0 = XtVaCreateManagedWidget ("cc0",
                         xmLabelWidgetClass,  topWidget(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _fill_cc0_box->W(),
                         XmNleftOffset,       10,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _fill_cc0_box->W(),
                         NULL);

  _cc1 = XtVaCreateManagedWidget ("cc1",
                         xmLabelWidgetClass,  topWidget(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _fill_cc1_box->W(),
                         XmNleftOffset,       10,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _fill_cc1_box->W(),
                         NULL);

  _cc2 = XtVaCreateManagedWidget ("cc2",
                         xmLabelWidgetClass,  topWidget(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _fill_cc2_box->W(),
                         XmNleftOffset,       10,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _fill_cc2_box->W(),
                         NULL);

  Widget title = XtVaCreateManagedWidget ("cf_title",
                         xmLabelWidgetClass,  topWidget(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _fill_cc0_box->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _fill_cc2_box->W(),
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _cc0,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (_fill_cc0_box->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _fill_cc2_box->W(),
                         XmNrightAttachment,  XmATTACH_WIDGET,
                         XmNrightWidget,      _fill_cc1_box->W(),
                         XmNrightOffset,      1,
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _fill_cc2_box->W(),
                         NULL);

  XtVaSetValues (_fill_cc1_box->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _fill_cc2_box->W(),
                         XmNrightAttachment,  XmATTACH_WIDGET,
                         XmNrightWidget,      _fill_cc2_box->W(),
                         XmNrightOffset,      1,
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _fill_cc2_box->W(),
                         NULL);

  XtVaSetValues (_fill_cc2_box->W(),
                         XmNrightAttachment,  XmATTACH_FORM,
                         XmNrightOffset,      1,
                         XmNbottomAttachment, XmATTACH_FORM,
                         XmNbottomOffset,     1,
                         NULL);

  _status = setState ();

  wprocShowMsg
    (_fill_cc0_box->GetRadioWidget(CBBColFillGui::CONSTANT_CC0),    " ");
  wprocShowMsg
    (_fill_cc0_box->GetRadioWidget(CBBColFillGui::LINEAR_CC0),      " ");
  wprocShowMsg
    (_fill_cc0_box->GetRadioWidget(CBBColFillGui::LOGARITHMIC_CC0), " ");
  wprocShowMsg
    (_fill_cc0_box->GetRadioWidget(CBBColFillGui::EXPONENTIAL_CC0), " ");
  wprocShowMsg
    (_fill_cc1_box->GetRadioWidget(CBBColFillGui::CONSTANT_CC1),    " ");
  wprocShowMsg
    (_fill_cc1_box->GetRadioWidget(CBBColFillGui::LINEAR_CC1),      " ");
  wprocShowMsg
    (_fill_cc1_box->GetRadioWidget(CBBColFillGui::LOGARITHMIC_CC1), " ");
  wprocShowMsg
    (_fill_cc1_box->GetRadioWidget(CBBColFillGui::EXPONENTIAL_CC1), " ");

  return topWidget ();

}

Boolean CBBColFillGui::notifyComplex (SLDelay *obj, int ident)
{
  CBBColFillSettings *cbb_cfs = _cbb_pop->colFillSetter ();
  CBBColSysGui       *cbb_csg = _cbb_pop->colSysGui ();

  if (obj == _fill_cc0_box) {
    if (ident == CONSTANT_CC0) {
      cbb_cfs->setCC0 (CBBColFillSettings::CONSTANT);
      if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
        if (_fill_cc1_box->WhichSelected() != CONSTANT_CC1)
          _fill_cc1_box->SetRadio (CONSTANT_CC1);
        if (_fill_cc2_box->WhichSelected() != CONSTANT_CC2)
          _fill_cc2_box->SetRadio (CONSTANT_CC2);        
      }
    }
    else if (ident == LINEAR_CC0) {
      cbb_cfs->setCC0 (CBBColFillSettings::LINEAR);
      if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
        if (_fill_cc1_box->WhichSelected() != LINEAR_CC1)
          _fill_cc1_box->SetRadio (LINEAR_CC1);
        if (_fill_cc2_box->WhichSelected() != LINEAR_CC2)
          _fill_cc2_box->SetRadio (LINEAR_CC2);        
      }
    }
    else if (ident == LOGARITHMIC_CC0) {
      cbb_cfs->setCC0 (CBBColFillSettings::LOGARITHMIC);
      if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
        if (_fill_cc1_box->WhichSelected() != LOGARITHMIC_CC1)
          _fill_cc1_box->SetRadio (LOGARITHMIC_CC1);
        if (_fill_cc2_box->WhichSelected() != LOGARITHMIC_CC2)
          _fill_cc2_box->SetRadio (LOGARITHMIC_CC2);        
      }
    }
    else if (ident == EXPONENTIAL_CC0) {
      cbb_cfs->setCC0 (CBBColFillSettings::EXPONENTIAL);
      if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
        if (_fill_cc1_box->WhichSelected() != EXPONENTIAL_CC1)
          _fill_cc1_box->SetRadio (EXPONENTIAL_CC1);
        if (_fill_cc2_box->WhichSelected() != EXPONENTIAL_CC2)
          _fill_cc2_box->SetRadio (EXPONENTIAL_CC2);        
      }
    }
  }
  else if (obj == _fill_cc1_box) {
    if (ident == CONSTANT_CC1) {
      cbb_cfs->setCC1 (CBBColFillSettings::CONSTANT);
      if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
        if (_fill_cc0_box->WhichSelected() != CONSTANT_CC0)
          _fill_cc0_box->SetRadio (CONSTANT_CC0);
        if (_fill_cc2_box->WhichSelected() != CONSTANT_CC2)
          _fill_cc2_box->SetRadio (CONSTANT_CC2);        
      }
    }
    else if (ident == LINEAR_CC1) {
      cbb_cfs->setCC1 (CBBColFillSettings::LINEAR);
      if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
        if (_fill_cc0_box->WhichSelected() != LINEAR_CC0)
          _fill_cc0_box->SetRadio (LINEAR_CC0);
        if (_fill_cc2_box->WhichSelected() != LINEAR_CC2)
          _fill_cc2_box->SetRadio (LINEAR_CC2);        
      }
    }
    else if (ident == LOGARITHMIC_CC1) {
      cbb_cfs->setCC1 (CBBColFillSettings::LOGARITHMIC);
      if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
        if (_fill_cc0_box->WhichSelected() != LOGARITHMIC_CC0)
          _fill_cc0_box->SetRadio (LOGARITHMIC_CC0);
        if (_fill_cc2_box->WhichSelected() != LOGARITHMIC_CC2)
          _fill_cc2_box->SetRadio (LOGARITHMIC_CC2);        
      }
    }
    else if (ident == EXPONENTIAL_CC1) {
      cbb_cfs->setCC1 (CBBColFillSettings::EXPONENTIAL);
      if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
        if (_fill_cc0_box->WhichSelected() != EXPONENTIAL_CC0)
          _fill_cc0_box->SetRadio (EXPONENTIAL_CC0);
        if (_fill_cc2_box->WhichSelected() != EXPONENTIAL_CC2)
          _fill_cc2_box->SetRadio (EXPONENTIAL_CC2);        
      }
    }
  }
  else if (obj == _fill_cc2_box) {
    if (ident == CONSTANT_CC2) {
      cbb_cfs->setCC2 (CBBColFillSettings::CONSTANT);
      if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
        if (_fill_cc0_box->WhichSelected() != CONSTANT_CC0)
          _fill_cc0_box->SetRadio (CONSTANT_CC0);
        if (_fill_cc1_box->WhichSelected() != CONSTANT_CC1)
          _fill_cc1_box->SetRadio (CONSTANT_CC1);        
      }
    }
    else if (ident == LINEAR_CC2) {
      cbb_cfs->setCC2 (CBBColFillSettings::LINEAR);
      if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
        if (_fill_cc0_box->WhichSelected() != LINEAR_CC0)
          _fill_cc0_box->SetRadio (LINEAR_CC0);
        if (_fill_cc1_box->WhichSelected() != LINEAR_CC1)
          _fill_cc1_box->SetRadio (LINEAR_CC1);        
      }
    }
    else if (ident == LOGARITHMIC_CC2) {
      cbb_cfs->setCC2 (CBBColFillSettings::LOGARITHMIC);
      if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
        if (_fill_cc0_box->WhichSelected() != LOGARITHMIC_CC0)
          _fill_cc0_box->SetRadio (LOGARITHMIC_CC0);
        if (_fill_cc1_box->WhichSelected() != LOGARITHMIC_CC1)
          _fill_cc1_box->SetRadio (LOGARITHMIC_CC1);        
      }
    }
    else if (ident == EXPONENTIAL_CC2) {
      cbb_cfs->setCC2 (CBBColFillSettings::EXPONENTIAL);
      if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
        if (_fill_cc0_box->WhichSelected() != EXPONENTIAL_CC0)
          _fill_cc0_box->SetRadio (EXPONENTIAL_CC0);
        if (_fill_cc1_box->WhichSelected() != EXPONENTIAL_CC1)
          _fill_cc1_box->SetRadio (EXPONENTIAL_CC1);        
      }
    }
  }
  return True;
}

int CBBColFillGui::setState ()
{
  CBBColSysGui *cbb_csg = _cbb_pop->colSysGui ();

  if (cbb_csg->GRAYSelected() && cbb_csg->BHSSelected()) {
    _fill_cc0_box->setSensitivity (True);
    _fill_cc1_box->setSensitivity (False);
    _fill_cc2_box->setSensitivity (False);
    selectConstant (ColorBarBuilderPop::CC1);
    selectConstant (ColorBarBuilderPop::CC2);
    
  }
  else {
    _fill_cc0_box->setSensitivity (True);
    _fill_cc1_box->setSensitivity (True);
    _fill_cc2_box->setSensitivity (True);
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

  CBBColFillSettings *cbb_cfs = _cbb_pop->colFillSetter ();

  if (!cbb_cfs) return (int)0;

       if (cbb_cfs->CC0() == CBBColFillSettings::CONSTANT) {
    selectConstant (ColorBarBuilderPop::CC0);
  }
  else if (cbb_cfs->CC0() == CBBColFillSettings::LINEAR) {
    selectLinear (ColorBarBuilderPop::CC0);
  }
  else if (cbb_cfs->CC0() == CBBColFillSettings::LOGARITHMIC) {
    selectLogarithmic (ColorBarBuilderPop::CC0);
  }
  else if (cbb_cfs->CC0() == CBBColFillSettings::EXPONENTIAL) {
    selectExponential (ColorBarBuilderPop::CC0);
  }

  if (!cbb_csg->GRAYSelected()) {

         if (cbb_cfs->CC1() == CBBColFillSettings::CONSTANT) {
      selectConstant (ColorBarBuilderPop::CC1);
    }
    else if (cbb_cfs->CC1() == CBBColFillSettings::LINEAR) {
      selectLinear (ColorBarBuilderPop::CC1);
    }
    else if (cbb_cfs->CC1() == CBBColFillSettings::LOGARITHMIC) {
      selectLogarithmic (ColorBarBuilderPop::CC1);
    }
    else if (cbb_cfs->CC1() == CBBColFillSettings::EXPONENTIAL) {
      selectExponential (ColorBarBuilderPop::CC1);
    }

         if (cbb_cfs->CC2() == CBBColFillSettings::CONSTANT) {
      selectConstant (ColorBarBuilderPop::CC2);
    }
    else if (cbb_cfs->CC2() == CBBColFillSettings::LINEAR) {
      selectLinear (ColorBarBuilderPop::CC2);
    }
    else if (cbb_cfs->CC2() == CBBColFillSettings::LOGARITHMIC) {
      selectLogarithmic (ColorBarBuilderPop::CC2);
    }
    else if (cbb_cfs->CC2() == CBBColFillSettings::EXPONENTIAL) {
      selectExponential (ColorBarBuilderPop::CC2);
    }
  }

  return (int)1;
}

void CBBColFillGui::selectConstant
  (ColorBarBuilderPop::ColorCoordinate which_cc)
{
  CBBColSysGui *cbb_csg = _cbb_pop->colSysGui ();

  if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
    _fill_cc0_box->SetRadio (CONSTANT_CC0);
    _fill_cc1_box->SetRadio (CONSTANT_CC1);
    _fill_cc2_box->SetRadio (CONSTANT_CC2);
  }
  else {

    switch (which_cc) {
    case ColorBarBuilderPop::CC0:
      _fill_cc0_box->SetRadio (CONSTANT_CC0);
      break;
    case ColorBarBuilderPop::CC1:
      _fill_cc1_box->SetRadio (CONSTANT_CC1);
      break;
    case ColorBarBuilderPop::CC2:
      _fill_cc2_box->SetRadio (CONSTANT_CC2);
      break;
    default:
      break;
    }
  }
}

void CBBColFillGui::selectLinear
  (ColorBarBuilderPop::ColorCoordinate which_cc)
{
  CBBColSysGui *cbb_csg = _cbb_pop->colSysGui ();

  if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
    _fill_cc0_box->SetRadio (LINEAR_CC0);
    _fill_cc1_box->SetRadio (LINEAR_CC1);
    _fill_cc2_box->SetRadio (LINEAR_CC2);
  }
  else {

    switch (which_cc) {
    case ColorBarBuilderPop::CC0:
      _fill_cc0_box->SetRadio (LINEAR_CC0);
      break;
    case ColorBarBuilderPop::CC1:
      _fill_cc1_box->SetRadio (LINEAR_CC1);
      break;
    case ColorBarBuilderPop::CC2:
      _fill_cc2_box->SetRadio (LINEAR_CC2);
      break;
    default:
      break;
    }
  }
}

void CBBColFillGui::selectLogarithmic
  (ColorBarBuilderPop::ColorCoordinate which_cc)
{
  CBBColSysGui *cbb_csg = _cbb_pop->colSysGui ();

  if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
    _fill_cc0_box->SetRadio (LOGARITHMIC_CC0);
    _fill_cc1_box->SetRadio (LOGARITHMIC_CC1);
    _fill_cc2_box->SetRadio (LOGARITHMIC_CC2);
  }
  else {

    switch (which_cc) {
    case ColorBarBuilderPop::CC0:
      _fill_cc0_box->SetRadio (LOGARITHMIC_CC0);
      break;
    case ColorBarBuilderPop::CC1:
      _fill_cc1_box->SetRadio (LOGARITHMIC_CC1);
      break;
    case ColorBarBuilderPop::CC2:
      _fill_cc2_box->SetRadio (LOGARITHMIC_CC2);
      break;
    default:
      break;
    }
  }
}

void CBBColFillGui::selectExponential
  (ColorBarBuilderPop::ColorCoordinate which_cc)
{
  CBBColSysGui *cbb_csg = _cbb_pop->colSysGui ();

  if (cbb_csg->GRAYSelected() && cbb_csg->RGBSelected()) {
    _fill_cc0_box->SetRadio (EXPONENTIAL_CC0);
    _fill_cc1_box->SetRadio (EXPONENTIAL_CC1);
    _fill_cc2_box->SetRadio (EXPONENTIAL_CC2);
  }
  else {

    switch (which_cc) {
    case ColorBarBuilderPop::CC0:
      _fill_cc0_box->SetRadio (EXPONENTIAL_CC0);
      break;
    case ColorBarBuilderPop::CC1:
      _fill_cc1_box->SetRadio (EXPONENTIAL_CC1);
      break;
    case ColorBarBuilderPop::CC2:
      _fill_cc2_box->SetRadio (EXPONENTIAL_CC2);
      break;
    default:
      break;
    }
  }
}

int CBBColFillGui::constantSelected
  (ColorBarBuilderPop::ColorCoordinate which_cc)
{
  switch (which_cc) {
  case ColorBarBuilderPop::CC0:
    return (int)(_fill_cc0_box->WhichSelected() == CONSTANT_CC0);
  case ColorBarBuilderPop::CC1:
    return (int)(_fill_cc1_box->WhichSelected() == CONSTANT_CC1);
  case ColorBarBuilderPop::CC2:
    return (int)(_fill_cc2_box->WhichSelected() == CONSTANT_CC2);
  default:
    return (int)0;
  }
}

int CBBColFillGui::linearSelected
  (ColorBarBuilderPop::ColorCoordinate which_cc)
{
  switch (which_cc) {
  case ColorBarBuilderPop::CC0:
    return (int)(_fill_cc0_box->WhichSelected() == LINEAR_CC0);
  case ColorBarBuilderPop::CC1:
    return (int)(_fill_cc1_box->WhichSelected() == LINEAR_CC1);
  case ColorBarBuilderPop::CC2:
    return (int)(_fill_cc2_box->WhichSelected() == LINEAR_CC2);
  default:
    return (int)0;
  }
}

int CBBColFillGui::logarithmicSelected
  (ColorBarBuilderPop::ColorCoordinate which_cc)
{
  switch (which_cc) {
  case ColorBarBuilderPop::CC0:
    return (int)(_fill_cc0_box->WhichSelected() == LOGARITHMIC_CC0);
  case ColorBarBuilderPop::CC1:
    return (int)(_fill_cc1_box->WhichSelected() == LOGARITHMIC_CC1);
  case ColorBarBuilderPop::CC2:
    return (int)(_fill_cc2_box->WhichSelected() == LOGARITHMIC_CC2);
  default:
    return (int)0;
  }
}

int CBBColFillGui::exponentialSelected
  (ColorBarBuilderPop::ColorCoordinate which_cc)
{
  switch (which_cc) {
  case ColorBarBuilderPop::CC0:
    return (int)(_fill_cc0_box->WhichSelected() == EXPONENTIAL_CC0);
  case ColorBarBuilderPop::CC1:
    return (int)(_fill_cc1_box->WhichSelected() == EXPONENTIAL_CC1);
  case ColorBarBuilderPop::CC2:
    return (int)(_fill_cc2_box->WhichSelected() == EXPONENTIAL_CC2);
  default:
    return (int)0;
  }
}
