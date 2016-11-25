#include "curves/gui_bowers_fitter.hh"
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
#include "curves/gui_power_law_fitter.hh"
#include "curves/weighted_curve_fitter.hh"
#include "curves/curve_parameters.hh"
#include "sl/sl_scale_text_arrow.hh"
#include "sl/psuedo_widget.hh"
#include <Xm/Label.h>
#include <math.h>
#include <float.h>

static String defres[] = {
  "*Ucoef.labelString:      ;    u = ",
  0};

GuiBowersFitter::GuiBowersFitter (Widget parent, char *name,
  HelpCtx hctx, const char *title) :
  GuiPowerLawFitter (parent, name, hctx, title, CV::BOWERS)
{
  init ();
  setDefaultResources (parent, name, defres);
}

GuiBowersFitter::GuiBowersFitter (SLDelay *container, char *name,
  HelpCtx hctx, const char *title) :
  GuiPowerLawFitter (container, name, hctx, title, CV::BOWERS)
{
  init ();
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
}

Widget GuiBowersFitter::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  GuiPowerLawFitter::make (parent);  // parent

  Widget Ucoef;

  Ucoef = XtVaCreateManagedWidget ("Ucoef",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  XtVaSetValues (Ucoef,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
		         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _a1_arrow->W(),
                         NULL);

  XtVaSetValues (_u_arrow->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
		         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       Ucoef,
                         NULL);

  if (displayErrorStatistics()) {
    XtVaSetValues (_erraveW,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[2],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _u_arrow->W(),
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

int GuiBowersFitter::setFitterParameters ()
{
  int err;

  err = GuiPowerLawFitter::setFitterParameters ();
  if (err != CV::NORMAL) return err;

  float min, max, inc;
  double u;

  err = _fitter->coefficient (CV::A2, &u);
  if (err != CV::NORMAL) {
    displayStatus ();
    return err;
  }

  min = (float)_curve_parameters->coefficientMinimum        (CV::A2);
  max = (float)_curve_parameters->coefficientMaximum        (CV::A2);
  inc = (float)_curve_parameters->coefficientRangeIncrement (CV::A2);

  if (_u_arrow) {
    _u_arrow->setRange     (min, max);
    _u_arrow->setIncrement (inc);
    _u_arrow->setValue     ((float)u);
  }

  return err;
    
}

char *GuiBowersFitter::getCString (int type)
{
  assert (type >= CV::A0 &&
    type <= CV::maximumCoefficientIndex(fitter()->type()));

  char *retval = new char[3];
  if (type == CV::A0) {
    strcpy (retval, "c0");
  }
  else if (type == CV::A1) {
    strcpy (retval, "c1");
  }
  else if (type == CV::A2) {
    strcpy (retval, "u");
  }
  return retval;
}

void GuiBowersFitter::init ()
{
  _u_arrow = 0;
  _u       = 0;

  _u_arrow = new SLScaleTextArrow (this, "u_arrow", getHelpCtx(),
    &_u, True, _u, (float)1, (float)50, (float).2, (int)7, (int)10,
    XmHORIZONTAL, 0, (Dimension)60);

  _u_arrow->setComplexNotify (this);

}

Boolean GuiBowersFitter::otherRecognizedObject (SLDelay *obj)
{
  Boolean retval;
  retval = GuiPowerLawFitter::otherRecognizedObject (obj) ||
    obj == _u_arrow;
  return retval;
}

// at some point, may want to set the variable extrema for the unloading
//   factor on _curve_parameters like any other fitter coefficient

int GuiBowersFitter::setOtherFitterValues ()
{
  int err;
  err = GuiPowerLawFitter::setOtherFitterValues ();

  if (err == CV::NORMAL) {
// use the unloading coefficient (u) on the gui and set the coefficient on
//   the fitter
    err = _fitter->setCoefficient (CV::A2, (double)_u);
  }

  return err;
}
