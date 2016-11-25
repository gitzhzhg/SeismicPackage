#include "curves/gui_linear_fitter.hh"
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
#include "curves/curve_fitter.hh"
#include "curves/ls_linear_fitter.hh"
#include "curves/curve_parameters.hh"
#include "sl/sl_scale_text_arrow.hh"
#include "sl/psuedo_widget.hh"
#include <Xm/Label.h>
#include <math.h>
#include <float.h>

static String defres[] = {
  "*Iequals.labelString:        = ",
  "*plus.labelString:           + ",
  0};

GuiLinearFitter::GuiLinearFitter (Widget parent, char *name,
  HelpCtx hctx, const char *title, int type) :
  GuiCurveFitter (parent, name, hctx, title, type)
{
  init ();
  setDefaultResources (parent, name, defres);
}

GuiLinearFitter::GuiLinearFitter (SLDelay *container, char *name,
  HelpCtx hctx, const char *title, int type) :
  GuiCurveFitter (container, name, hctx, title, type)
{
  init ();
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
}

Widget GuiLinearFitter::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  GuiCurveFitter::make (parent);

  Widget Iequals, plus;

  Iequals = XtVaCreateManagedWidget ("Iequals",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  plus = XtVaCreateManagedWidget ("plus",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

// attach the managed widgets as desired
  XtVaSetValues (_dependtxt[2],
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _top_attach_widget,
		         XmNtopOffset,        5,
                         XmNleftAttachment,   XmATTACH_FORM,
                         NULL);

  XtVaSetValues (Iequals,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _dependtxt[2],
                         NULL);

  XtVaSetValues (_a0_arrow->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       Iequals,
                         NULL);

  XtVaSetValues (plus,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _a0_arrow->W(),
                         NULL);

  XtVaSetValues (_a1_arrow->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       plus,
                         NULL);

  XtVaSetValues (_indeptxt[2],
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _a1_arrow->W(),
                         NULL);

  if (displayErrorStatistics()) {
    XtVaSetValues (_erraveW,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _indeptxt[2],
                         NULL);

    XtVaSetValues (_erravetxt,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _erraveW,
                         NULL);

    XtVaSetValues (_errstdW,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _erravetxt,
                         NULL);

    XtVaSetValues (_errstdtxt,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _errstdW,
                         NULL);
  }

  dataChangedFit ();

  return topWidget ();
}

int GuiLinearFitter::setFitterParameters ()
{
  int err;

  err = GuiCurveFitter::setFitterParameters ();
  if (err != CV::NORMAL) return err;

  float min, max, inc;
  double ai;

  err = _fitter->coefficient (CV::A0, &ai);
  if (err != CV::NORMAL) {
    displayStatus ();
    return err;
  }

  min = (float)_curve_parameters->coefficientMinimum        (CV::A0);
  max = (float)_curve_parameters->coefficientMaximum        (CV::A0);
  inc = (float)_curve_parameters->coefficientRangeIncrement (CV::A0);

  if (_a0_arrow) {
    _a0_arrow->setRange     (min, max);
    _a0_arrow->setIncrement (inc);
    _a0_arrow->setValue     ((float)ai);
  }
 
  err = _fitter->coefficient (CV::A1, &ai);
  if (err != CV::NORMAL) {
    displayStatus ();
    return err;
  }

  min = (float)_curve_parameters->coefficientMinimum        (CV::A1);
  max = (float)_curve_parameters->coefficientMaximum        (CV::A1);
  inc = (float)_curve_parameters->coefficientRangeIncrement (CV::A1);

  if (_a1_arrow) {
    _a1_arrow->setRange     (min, max);
    _a1_arrow->setIncrement (inc);
    _a1_arrow->setValue     ((float)ai);
  }

  return err;
}

void GuiLinearFitter::init ()
{
  _a0_arrow = 0;
  _a1_arrow = 0;
  _a0       = 0;
  _a1       = 0;

  _a0_arrow = new SLScaleTextArrow (this, "a0_arrow", getHelpCtx(),
    &_a0, True, _a0, (float)-1, (float)1, (float).01, (int)7, (int)10,
    XmHORIZONTAL, 0, (Dimension)60);

  _a0_arrow->setComplexNotify (this);


  _a1_arrow = new SLScaleTextArrow (this, "a1_arrow", getHelpCtx(),
    &_a1, True, _a1, (float)0, (float)1, (float).005, (int)7, (int)10,
    XmHORIZONTAL, 0, (Dimension)60);

  _a1_arrow->setComplexNotify (this);
}

Boolean GuiLinearFitter::otherRecognizedObject (SLDelay *obj)
{
  return obj == _a0_arrow || obj == _a1_arrow;
}

int GuiLinearFitter::setOtherGuiValues ()
{
  double a0, a1, tiny;
  int err;

  tiny = FLT_EPSILON * FLT_EPSILON;
  err  = _fitter->coefficient (CV::A0, &a0);

// check if coefficient is trivially small and change to 0.0 as needed
  if (err == CV::NORMAL && fabs(a0) < tiny) {
    a0 = 0;
    if (_a0_arrow) _a0_arrow->setValue ((float)a0);
  }

  err = _fitter->coefficient (CV::A1, &a1);
  if (err == CV::NORMAL && fabs(a1) < tiny) {
    if (_a1_arrow) _a1_arrow->setValue ((float)a1);
  }
  return err;
}

int GuiLinearFitter::setOtherFitterValues ()
{
// use the coefficient on the gui and set the coefficient on the fitter
  int err = _fitter->setCoefficient (CV::A0, (double)_a0);
  if (err == CV::NORMAL) {
    err = _fitter->setCoefficient (CV::A1, (double)_a1);
  }
  return err;
}
