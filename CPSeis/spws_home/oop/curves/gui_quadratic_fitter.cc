#include "curves/gui_quadratic_fitter.hh"
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
#include "curves/ls_quadratic_fitter.hh"
#include "curves/curve_parameters.hh"
#include "sl/sl_scale_text_arrow.hh"
#include "sl/psuedo_widget.hh"
#include <Xm/Label.h>
#include <math.h>
#include <float.h>

static String defres[] = {
  "*squared.labelString:       2",
  "*X2plus.labelString:       + ",
  0};

GuiQuadraticFitter::GuiQuadraticFitter (Widget parent, char *name,
  HelpCtx hctx, const char *title) :
  GuiLinearFitter (parent, name, hctx, title, CV::QUADRATIC)
{
  init ();
  setDefaultResources (parent, name, defres);
}

GuiQuadraticFitter::GuiQuadraticFitter (SLDelay *container, char *name,
  HelpCtx hctx, const char *title) :
  GuiLinearFitter (container, name, hctx, title, CV::QUADRATIC)
{
  init ();
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
}

Widget GuiQuadraticFitter::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  GuiCurveFitter::make (parent);  // grandparent

  Widget Iequals, plus, squared, X2plus;

  Iequals = XtVaCreateManagedWidget ("Iequals",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  plus = XtVaCreateManagedWidget ("plus",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  squared = XtVaCreateManagedWidget ("squared",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  X2plus = XtVaCreateManagedWidget ("X2plus",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  _indeptxt1 = XtVaCreateManagedWidget ("indep2",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

// attach the managed widgets as desired
  XtVaSetValues (_dependtxt[2],
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _top_attach_widget,
                         XmNtopOffset,        15,
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

  XtVaSetValues (_indeptxt1,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _a1_arrow->W(),
                         NULL);

  XtVaSetValues (X2plus,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _indeptxt1,
                         NULL);

  XtVaSetValues (_a2_arrow->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       X2plus,
                         NULL);

  XtVaSetValues (_indeptxt[2],
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _a2_arrow->W(),
                         NULL);

  XtVaSetValues (squared,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNtopOffset,        -7,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _indeptxt[2],
                         NULL);

  if (displayErrorStatistics()) {
    XtVaSetValues (_erraveW,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       squared,
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

int GuiQuadraticFitter::setFitterParameters ()
{
  int err;

  err = GuiLinearFitter::setFitterParameters ();
  if (err != CV::NORMAL) return err;

  float min, max, inc;
  double a2;

  err = _fitter->coefficient (CV::A2, &a2);
  if (err != CV::NORMAL) {
    displayStatus ();
    return err;
  }

  min = (float)_curve_parameters->coefficientMinimum        (CV::A2);
  max = (float)_curve_parameters->coefficientMaximum        (CV::A2);
  inc = (float)_curve_parameters->coefficientRangeIncrement (CV::A2);

  _a2_arrow->setRange     (min, max);
  _a2_arrow->setIncrement (inc);
  _a2_arrow->setValue     ((float)a2);

  return err;
}

void GuiQuadraticFitter::init ()
{
  _indeptxt1 = 0;
  _a2_arrow  = 0;
  _a2        = 0;

  _a2_arrow = new SLScaleTextArrow (this, "a2_arrow", getHelpCtx(),
    &_a2, True, _a2, (float)-1, (float)1, (float).01, (int)7, (int)10,
    XmHORIZONTAL, 0, (Dimension)60);
  
  _a2_arrow->setComplexNotify (this);
}

Boolean GuiQuadraticFitter::otherRecognizedObject (SLDelay *obj)
{
  Boolean retval = GuiLinearFitter::otherRecognizedObject (obj) ||
                   obj == _a2_arrow                               ;

  return retval;
}

int GuiQuadraticFitter::setOtherGuiValues ()
{
  int err;
  err = GuiLinearFitter::setOtherGuiValues ();
  if (err != CV::NORMAL) return err;

  double a2, tiny;

  tiny = FLT_EPSILON * FLT_EPSILON;
  err = _fitter->coefficient (CV::A2,  &a2);

  if (err == CV::NORMAL) {
    if (fabs((double)a2) < tiny) {
// a2 is trivially small change to 0.0
      a2 = 0;
      if (_a2_arrow) _a2_arrow->setValue (a2);
    }
  }
  return err;
}

int GuiQuadraticFitter::setOtherFitterValues ()
{
// use the coefficient on the gui and set the coefficient on the fitter
  int err = GuiLinearFitter::setOtherFitterValues ();
  if (err == CV::NORMAL) {
    err = _fitter->setCoefficient (CV::A2, (double)_a2);
  }
  return err;
}

void GuiQuadraticFitter::display ()
{
  GuiCurveFitter::display ();

  if (_y_is_independent && _indeptxt1) {
    char y_str[3];
    strcpy (y_str, _y_str);
    strcat (y_str, "'"   );
    wprocVAShowMsg (_indeptxt1, "%.2s", y_str);
  }
  else if (_indeptxt1) {
    char x_str[3];
    strcpy (x_str, _x_str);
    strcat (x_str, "'"   );
    wprocVAShowMsg (_indeptxt1, "%.2s", x_str);
  }
}
