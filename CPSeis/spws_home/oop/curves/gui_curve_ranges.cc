#include "curves/gui_curve_ranges.hh"
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
#include "curves/gui_curve_fitter.hh"
#include "curves/curve_parameters.hh"
#include "sl/slp_text.hh"
#include "sl/psuedo_widget.hh"

#include "cprim.h"     /* for ZNIL */

#include <Xm/RowColumn.h>
#include <Xm/Label.h>

#include <float.h>

static void set_range_trap (void *data, long ident, float oldvar,
  float newvar)
{
  GuiCurveRanges *gui = (GuiCurveRanges *)data;
  if (newvar != oldvar) gui->setRange (ident, newvar);
}

static float min0_upfun (void *data)
{
  GuiCurveRanges *gui = (GuiCurveRanges *)data;
  float min0 = gui->getRange (GuiCurveRanges::MIN0);
  return min0;
}

static float max0_upfun (void *data)
{
  GuiCurveRanges *gui = (GuiCurveRanges *)data;
  float max0 = gui->getRange (GuiCurveRanges::MAX0);
  return max0;
}

static float min1_upfun (void *data)
{
  GuiCurveRanges *gui = (GuiCurveRanges *)data;
  float min1 = gui->getRange (GuiCurveRanges::MIN1);
  return min1;
}

static float max1_upfun (void *data)
{
  GuiCurveRanges *gui = (GuiCurveRanges *)data;
  float max1 = gui->getRange (GuiCurveRanges::MAX1);
  return max1;
}

static float min2_upfun (void *data)
{
  GuiCurveRanges *gui = (GuiCurveRanges *)data;
  float min2 = gui->getRange (GuiCurveRanges::MIN2);
  return min2;
}

static float max2_upfun (void *data)
{
  GuiCurveRanges *gui = (GuiCurveRanges *)data;
  float max2 = gui->getRange (GuiCurveRanges::MAX2);
  return max2;
}

static float min3_upfun (void *data)
{
  GuiCurveRanges *gui = (GuiCurveRanges *)data;
  float min3 = gui->getRange (GuiCurveRanges::MIN3);
  return min3;
}

static float max3_upfun (void *data)
{
  GuiCurveRanges *gui = (GuiCurveRanges *)data;
  float max3 = gui->getRange (GuiCurveRanges::MAX3);
  return max3;
}

static float min4_upfun (void *data)
{
  GuiCurveRanges *gui = (GuiCurveRanges *)data;
  float min4 = gui->getRange (GuiCurveRanges::MIN4);
  return min4;
}

static float max4_upfun (void *data)
{
  GuiCurveRanges *gui = (GuiCurveRanges *)data;
  float max4 = gui->getRange (GuiCurveRanges::MAX4);
  return max4;
}

static String defres[] = {
  "*gui_curve_ranges_lb1.labelString:        Coefficient",
  "*gui_curve_ranges_lb7.labelString:        Minimum    ",
  "*gui_curve_ranges_lb8.labelString:        Maximum    ",
  0
};

GuiCurveRanges::GuiCurveRanges (Widget parent, char *name,
  HelpCtx hctx, GuiCurveFitter *gui_curve_fitter) :
  SLFPopSep (parent, name, FP_DOREMOVE, hctx, True, False),
  _gui_curve_fitter  (gui_curve_fitter)
{
  setDefaultResources (parent, name, defres);
  _min0_txt = 0; _min1_txt = 0;
  _min2_txt = 0; _min3_txt = 0;
  _min4_txt = 0; _max0_txt = 0;
  _max1_txt = 0; _max2_txt = 0;
  _max3_txt = 0; _max4_txt = 0;
}

GuiCurveRanges::GuiCurveRanges (SLDelay *container, char *name,
  HelpCtx hctx, GuiCurveFitter *gui_curve_fitter) :
  SLFPopSep (container, name, FP_DOREMOVE, hctx, True, False),
  _gui_curve_fitter  (gui_curve_fitter)
{
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
  _min0_txt = 0; _min1_txt = 0;
  _min2_txt = 0; _min3_txt = 0;
  _min4_txt = 0; _max0_txt = 0;
  _max1_txt = 0; _max2_txt = 0;
  _max3_txt = 0; _max4_txt = 0;
}

GuiCurveRanges::~GuiCurveRanges ()
{
//Following deletes added by MLS 03/2000. The SLPText widgets were
//not being deleted which caused crashes when the X callbacks
//thought they still existed.
  if(_min0_txt) delete _min0_txt;
  if(_min1_txt) delete _min1_txt;
  if(_min2_txt) delete _min2_txt;
  if(_min3_txt) delete _min3_txt;
  if(_min4_txt) delete _min4_txt;
  if(_max0_txt) delete _max0_txt;
  if(_max1_txt) delete _max1_txt;
  if(_max2_txt) delete _max2_txt;
  if(_max3_txt) delete _max3_txt;
  if(_max4_txt) delete _max4_txt;
}

Widget GuiCurveRanges::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  SLFPopSep::make (parent);

  Widget rc = XtVaCreateManagedWidget ("gui_curve_ranges_rc",
                           xmRowColumnWidgetClass, topWidget(),
                           XmNtopAttachment,    XmATTACH_FORM,
                           XmNleftAttachment,   XmATTACH_FORM,
                           XmNrightAttachment,  XmATTACH_FORM,
                           XmNbottomAttachment, XmATTACH_WIDGET,
                           XmNbottomWidget,     bottomSeparator(),
                           XmNpacking,          XmPACK_COLUMN,
                           XmNnumColumns,       3,
                           XmNentryAlignment,   XmALIGNMENT_END,
                           NULL);

  assert (fitter());
  assert (!(CV::maximumCoefficientIndex(fitter()->type()) > 5));

// Coefficient
  Widget lb1 = XtVaCreateManagedWidget ("gui_curve_ranges_lb1",
                           xmLabelWidgetClass,  rc,
                           NULL);

  XmString label_string;
// x0
  label_string = XmStringCreateSimple
    ((char *)_gui_curve_fitter->getIndependentString(CV::OFFSET_VARIABLE));
  Widget lb2 = XtVaCreateManagedWidget ("gui_curve_ranges_lb2",
                           xmLabelWidgetClass,  rc,
			   XmNlabelString,      label_string,
                           NULL);
  XmStringFree (label_string);

// y0
  label_string = XmStringCreateSimple
    ((char *)_gui_curve_fitter->getDependentString(CV::OFFSET_VARIABLE));
  Widget lb3 = XtVaCreateManagedWidget ("gui_curve_ranges_lb3",
                           xmLabelWidgetClass,  rc,
			   XmNlabelString,      label_string,
                           NULL);
  XmStringFree (label_string);

  if (CV::coefficientCount(fitter()->type()) > 2) {
// c0
    label_string = XmStringCreateSimple (
      ((char *)_gui_curve_fitter->getCString(CV::A0)));
    Widget lb4 = XtVaCreateManagedWidget ("gui_curve_ranges_lb4",
                           xmLabelWidgetClass,  rc,
			   XmNlabelString,      label_string,
                           NULL);
    XmStringFree (label_string);

    if (CV::coefficientCount(fitter()->type()) > 3) {
// c1
      label_string = XmStringCreateSimple (
        ((char *)_gui_curve_fitter->getCString(CV::A1)));
      Widget lb5 = XtVaCreateManagedWidget ("gui_curve_ranges_lb5",
                           xmLabelWidgetClass,  rc,
			   XmNlabelString,      label_string,
                           NULL);
      XmStringFree (label_string);

      if (CV::coefficientCount(fitter()->type()) > 4) {
// c2
        label_string = XmStringCreateSimple (
          ((char *)_gui_curve_fitter->getCString(CV::A2)));
        Widget lb6 = XtVaCreateManagedWidget ("gui_curve_ranges_lb6",
                           xmLabelWidgetClass,  rc,
			   XmNlabelString,      label_string,
                           NULL);
        XmStringFree (label_string);
      }
    }
  }

// minimum
  Widget lb7 = XtVaCreateManagedWidget ("gui_curve_ranges_lb7",
                           xmLabelWidgetClass,  rc,
                           NULL);

// x0 minimum value
  _min0_txt = new SLpText (rc, "gui_curve_ranges_min0", MIN0,
    PrimSupport::_FLOAT, 12, 4);
  _min0_txt->setFtrap     (set_range_trap,  this);
  _min0_txt->setupFvarFun (min0_upfun,      this);
  _min0_txt->setHelpCtx (getHelpCtx());
  _min0_txt->install_help ();

// y0 minimum value
  _min1_txt = new SLpText (rc, "gui_curve_ranges_min1", MIN1,
    PrimSupport::_FLOAT, 12, 4);
  _min1_txt->setFtrap     (set_range_trap,  this);
  _min1_txt->setupFvarFun (min1_upfun,      this);
  _min1_txt->setHelpCtx (getHelpCtx());
  _min1_txt->install_help ();

  if (CV::coefficientCount(fitter()->type()) > 2) {
// c0 minimum value
    _min2_txt = new SLpText (rc, "gui_curve_ranges_min2", MIN2,
      PrimSupport::_FLOAT, 12, 4);
    _min2_txt->setFtrap     (set_range_trap,  this);
    _min2_txt->setupFvarFun (min2_upfun,      this);
    _min2_txt->setHelpCtx (getHelpCtx());
    _min2_txt->install_help ();

    if (CV::coefficientCount(fitter()->type()) > 3) {
// c1 minimum value
      _min3_txt = new SLpText (rc, "gui_curve_ranges_min3", MIN3,
        PrimSupport::_FLOAT, 12, 4);
      _min3_txt->setFtrap     (set_range_trap,  this);
      _min3_txt->setupFvarFun (min3_upfun,      this);
      _min3_txt->setHelpCtx (getHelpCtx());
      _min3_txt->install_help ();

      if (CV::coefficientCount(fitter()->type()) > 4) {
// c2
        _min4_txt = new SLpText (rc, "gui_curve_ranges_min4", MIN4,
          PrimSupport::_FLOAT, 12, 4);
        _min4_txt->setFtrap     (set_range_trap,  this);
        _min4_txt->setupFvarFun (min4_upfun,      this);
        _min4_txt->setHelpCtx (getHelpCtx());
        _min4_txt->install_help ();
      }
    }
  }

// maximum
  Widget lb8 = XtVaCreateManagedWidget ("gui_curve_ranges_lb8",
                           xmLabelWidgetClass,  rc,
                           NULL);

// c0 maximum value
  _max0_txt = new SLpText (rc, "gui_curve_ranges_max0", MAX0,
    PrimSupport::_FLOAT, 12, 4);
  _max0_txt->setFtrap     (set_range_trap,  this);
  _max0_txt->setupFvarFun (max0_upfun,      this);
  _max0_txt->setHelpCtx (getHelpCtx());
  _max0_txt->install_help ();

// c1 maximum value
  _max1_txt = new SLpText (rc, "gui_curve_ranges_max1", MAX1,
    PrimSupport::_FLOAT, 12, 4);
  _max1_txt->setFtrap     (set_range_trap,  this);
  _max1_txt->setupFvarFun (max1_upfun,      this);
  _max1_txt->setHelpCtx (getHelpCtx());
  _max1_txt->install_help ();

  if (CV::coefficientCount(fitter()->type()) > 2) {
// c2 maximum value
    _max2_txt = new SLpText (rc, "gui_curve_ranges_max2", MAX2,
      PrimSupport::_FLOAT, 12, 4);
    _max2_txt->setFtrap     (set_range_trap,  this);
    _max2_txt->setupFvarFun (max2_upfun,      this);
    _max2_txt->setHelpCtx (getHelpCtx());
    _max2_txt->install_help ();

    if (CV::coefficientCount(fitter()->type()) > 3) {
// c3 maximum value
      _max3_txt = new SLpText (rc, "gui_curve_ranges_max3", MAX3,
        PrimSupport::_FLOAT, 12, 4);
      _max3_txt->setFtrap     (set_range_trap,  this);
      _max3_txt->setupFvarFun (max3_upfun,      this);
      _max3_txt->setHelpCtx (getHelpCtx());
      _max3_txt->install_help ();

      if (CV::coefficientCount(fitter()->type()) > 4) {
// c2
        _max4_txt = new SLpText (rc, "gui_curve_ranges_max4", MIN4,
          PrimSupport::_FLOAT, 12, 4);
        _max4_txt->setFtrap     (set_range_trap,  this);
        _max4_txt->setupFvarFun (max4_upfun,      this);
        _max4_txt->setHelpCtx (getHelpCtx());
        _max4_txt->install_help ();
      }
    }
  }
  return topWidget ();
}


void GuiCurveRanges::setRange (long ident, float newvar)
{
  assert (fitter());
  int max_coef = CV::coefficientCount(fitter()->type()) - 1;

  int coef;
  double min_value, max_value;
  CurveParameters *params = _gui_curve_fitter->curveParameters ();
  int steps = params->coefficientRangeSteps () - 1;
  if (steps < 1) steps = 1;
  switch (ident) {
    case MIN0:
      coef = 0;
      min_value = (double)newvar;
      max_value = params->coefficientMaximum (coef) > (double)newvar
        ? params->coefficientMaximum (coef)
        : min_value + steps * FLT_EPSILON; 
      break;
    case MAX0:
      coef = 0;
      max_value = (double)newvar;
      min_value = params->coefficientMinimum (coef) < (double)newvar
        ? params->coefficientMinimum (coef)
        : max_value - steps * FLT_EPSILON; 
      break;
    case MIN1:
      coef = 1;
      min_value = (double)newvar;
      max_value = params->coefficientMaximum (coef) > (double)newvar
        ? params->coefficientMaximum (coef)
        : min_value + steps * FLT_EPSILON; 
      break;
    case MAX1:
      coef = 1;
      max_value = (double)newvar;
      min_value = params->coefficientMinimum (coef) < (double)newvar
        ? params->coefficientMinimum (coef)
        : max_value - steps * FLT_EPSILON; 
      break;
    case MIN2:
      coef = 2;
      if (coef > max_coef) return;
      min_value = (double)newvar;
      max_value = params->coefficientMaximum (coef) > (double)newvar
        ? params->coefficientMaximum (coef)
        : min_value + steps * FLT_EPSILON; 
      break;
    case MAX2:
      coef = 2;
      if (coef > max_coef) return;
      max_value = (double)newvar;
      min_value = params->coefficientMinimum (coef) < (double)newvar
        ? params->coefficientMinimum (coef)
        : max_value - steps * FLT_EPSILON; 
      break;
    case MIN3:
      coef = 3;
      if (coef > max_coef) return;
      min_value = (double)newvar;
      max_value = params->coefficientMaximum (coef) > (double)newvar
        ? params->coefficientMaximum (coef)
        : min_value + steps * FLT_EPSILON; 
      break;
    case MAX3:
      coef = 3;
      if (coef > max_coef) return;
      max_value = (double)newvar;
      min_value = params->coefficientMinimum (coef) < (double)newvar
        ? params->coefficientMinimum (coef)
        : max_value - steps * FLT_EPSILON; 
      break;
    case MIN4:
      coef = 4;
      if (coef > max_coef) return;
      min_value = (double)newvar;
      max_value = params->coefficientMaximum (coef) > (double)newvar
        ? params->coefficientMaximum (coef)
        : min_value + steps * FLT_EPSILON; 
      break;
    case MAX4:
      coef = 4;
      if (coef > max_coef) return;
      max_value = (double)newvar;
      min_value = params->coefficientMinimum (coef) < (double)newvar
        ? params->coefficientMinimum (coef)
        : max_value - steps * FLT_EPSILON; 
      break;
    default:
      assert (0);
      break;
  }
  params->setCoefficientRange (coef, min_value, max_value);
  _gui_curve_fitter->setFitterParameters ();
  _gui_curve_fitter->userChangedFit();  // 05/24/00 req'd for SOLARIS
}

float GuiCurveRanges::getRange (long ident)
{
  static float znil = ZNIL;

  double retval;
  if (!fitter()) return znil;
  int max_coef = CV::coefficientCount(fitter()->type()) - 1;

  CurveParameters *params = _gui_curve_fitter->curveParameters ();
  switch (ident) {
    case MIN0:
      retval = params->coefficientMinimum (0);
      break;
    case MAX0:
      retval = params->coefficientMaximum (0);
      break;
    case MIN1:
      retval = params->coefficientMinimum (1);
      break;
    case MAX1:
      retval = params->coefficientMaximum (1);
      break;
    case MIN2:
      if (max_coef < 2) retval = params->coefficientMinimum (max_coef);
      else              retval = params->coefficientMinimum (2);
      break;
    case MAX2:
      if (max_coef < 2) retval = params->coefficientMaximum (max_coef);
      else              retval = params->coefficientMaximum (2);
      break;
    case MIN3:
      if (max_coef < 3) retval = params->coefficientMinimum (max_coef);
      else              retval = params->coefficientMinimum (3);
      break;
    case MAX3:
      if (max_coef < 3) retval = params->coefficientMaximum (max_coef);
      else              retval = params->coefficientMaximum (3);
      break;
    case MIN4:
      if (max_coef < 4) retval = params->coefficientMinimum (max_coef);
      else              retval = params->coefficientMinimum (4);
      break;
    case MAX4:
      if (max_coef < 4) retval = params->coefficientMaximum (max_coef);
      else              retval = params->coefficientMaximum (4);
      break;
    default:
      assert (0);
      break;
  }
  return (float)retval;
}

CurveFitter *GuiCurveRanges::fitter ()
{
  return (CurveFitter *)_gui_curve_fitter->fitter ();
}
