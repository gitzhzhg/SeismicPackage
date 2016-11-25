// Used to work with a DistributionSlicer object to allow the user to
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
//   interactively change the upper and lower limits being displayed
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <stdio.h>
#include <assert.h>
#include "dp/distr_slicer_pop.hh"
#include "dp/distribution_slicer.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_scale.hh"

static String defres[] = {
  ".distr_slicer_pop.title:    Distribution Slicer",
  "*dsupperlab.labelString:    Upper Limit",
  "*dslowerlab.labelString:    Lower Limit",
  0};

DistrSlicerPop::DistrSlicerPop (Widget             p,
                                char               *name,
                                DistributionSlicer *ds,
                                SeisPlot           *sp,
                                HelpCtx            hctx):

  RGBZPop (p, name, (RGBZGenerator *)ds, (Decoder *)ds, sp, hctx),
  _ds            (ds)
{
  _dsupperscale = 0;
  _dslowerscale = 0;

  if (failed()) return;

  setDefaultResources (_p, name, defres);

  _dsupperscale = new DSUpperLimitScale (this, "dsupperscale", getHelpCtx(),
    0, this);

  if (!_dsupperscale) {
    _error_status = RGBZP_MEMORY_ALLOCATION_ERROR;
    return;
  }

  _dslowerscale = new DSLowerLimitScale (this, "dslowerscale", getHelpCtx(),
    0, this);

  if (!_dslowerscale) {
    _error_status = RGBZP_MEMORY_ALLOCATION_ERROR;
    return;
  }
}

DistrSlicerPop::~DistrSlicerPop ()
{
  if (_dsupperscale) delete _dsupperscale;
  if (_dslowerscale) delete _dslowerscale;
}

Widget DistrSlicerPop::make (Widget p)
{
  if (made()) return topWidget ();
  SLFPopSep::make (p);

  _dsupperscale->make (topWidget());
  _dslowerscale->make (topWidget());

  XtVaSetValues (_dsupperscale->W(),
                   XmNtopAttachment,    XmATTACH_FORM,
                   XmNtopOffset,        5,
                   XmNleftAttachment,   XmATTACH_FORM,
                   XmNleftOffset,       5,
                   XmNrightAttachment,  XmATTACH_FORM,
                   XmNrightOffset,      5,
                   XmNminimum,          _ds->getMinimumUpperBinLimit(),
                   XmNmaximum,          _ds->getMaximumUpperBinLimit(),
                   XmNvalue,            (int)(_ds->getUpperBinLimit()+0.5),
                   XmNdecimalPoints,    0, NULL);

  Widget dsupperlab = XtVaCreateManagedWidget ("dsupperlab",
                        xmLabelWidgetClass, topWidget(),
                        XmNleftAttachment,  XmATTACH_FORM,
                        XmNleftOffset,      5,
                        XmNtopAttachment,   XmATTACH_WIDGET,
                        XmNtopWidget,       _dsupperscale->W(), NULL);

  _dsupperscale->setScaleValue ((int)(_ds->getUpperBinLimit()+0.5));

  XtVaSetValues (_dslowerscale->W(),
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        dsupperlab,
                   XmNleftAttachment,   XmATTACH_FORM,
                   XmNleftOffset,       5,
                   XmNrightAttachment,  XmATTACH_FORM,
                   XmNrightOffset,      5,
                   XmNminimum,          _ds->getMinimumLowerBinLimit(),
                   XmNmaximum,          _ds->getMaximumLowerBinLimit(),
                   XmNvalue,            (int)(_ds->getLowerBinLimit()+0.5),
                   XmNdecimalPoints,    0, NULL);

  Widget dslowerlab = XtVaCreateManagedWidget ("dslowerlab",
                        xmLabelWidgetClass, topWidget(),
                        XmNleftAttachment,  XmATTACH_FORM,
                        XmNleftOffset,      5,
                        XmNtopAttachment,   XmATTACH_WIDGET,
                        XmNtopWidget,       _dslowerscale->W(), NULL);

  _dslowerscale->setScaleValue ((int)(_ds->getLowerBinLimit()+0.5));

  _previous_widget = dslowerlab;

  return RGBZPop::make (p);
}


DSUpperLimitScale::DSUpperLimitScale (SLDelay *contain, char *name,
  HelpCtx hctx, int *valptr, DistrSlicerPop *dsp)
  : SLScaleDrag (contain, name, hctx, valptr),
    _dsp        (dsp)
{
  assert (_dsp);
}

void DSUpperLimitScale::ScaleAction (int /*val*/)
{
}

void DSUpperLimitScale::ScaleActionDrag (int val)
{
  _dsp->_ds->setUpperBinLimit ((float)val);
  setScaleValue ((int)(_dsp->_ds->getUpperBinLimit()+0.5));
  _dsp->_dslowerscale->setScaleValue
    ((int)(_dsp->_ds->getLowerBinLimit()+0.5));
  _dsp->setLUT ();
}

void DSUpperLimitScale::ScaleActionVC (int val)
{
  _dsp->_ds->setUpperBinLimit ((float)val);
  setScaleValue ((int)(_dsp->_ds->getUpperBinLimit()+0.5));
  _dsp->_dslowerscale->setScaleValue
    ((int)(_dsp->_ds->getLowerBinLimit()+0.5));
  _dsp->setLUT ();
}


DSLowerLimitScale::DSLowerLimitScale (SLDelay *contain, char *name,
  HelpCtx hctx, int *valptr, DistrSlicerPop *dsp)
  : SLScaleDrag (contain, name, hctx, valptr),
    _dsp        (dsp)
{
  assert (_dsp);
}

void DSLowerLimitScale::ScaleAction (int /*val*/)
{
}

void DSLowerLimitScale::ScaleActionDrag (int val)
{
  _dsp->_ds->setLowerBinLimit ((float)val);
  setScaleValue ((int)(_dsp->_ds->getLowerBinLimit()+0.5));
  _dsp->_dsupperscale->setScaleValue
    ((int)(_dsp->_ds->getUpperBinLimit()+0.5));
  _dsp->setLUT ();
}

void DSLowerLimitScale::ScaleActionVC (int val)
{
  _dsp->_ds->setLowerBinLimit ((float)val);
  setScaleValue ((int)(_dsp->_ds->getLowerBinLimit()+0.5));
  _dsp->_dsupperscale->setScaleValue
    ((int)(_dsp->_ds->getUpperBinLimit()+0.5));
  _dsp->setLUT ();
}
