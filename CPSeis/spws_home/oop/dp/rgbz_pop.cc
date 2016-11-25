// A base class for real-time RGBZ generator/decoder popups that also provides
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
//   the setting of color intensity and compression
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <stdio.h>
#include <assert.h>
#include "dp/rgbz_pop.hh"
#include "dp/rgbz_generator.hh"
#include "dp/decoder.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_color.hh"

static String defres[] = {
  ".width:                       185",
  "*intensitylab.labelString:    Intensity",
  "*compressionlab.labelString:  Compression",
  0};

#define ParentClass SLFPopSep

RGBZPop::RGBZPop (Widget p, char *name, RGBZGenerator *rgbzg, Decoder *decoder,
  SeisPlot *sp, HelpCtx hctx) :
  SLFPopSep (p, name, FP_DOOK|FP_DOHELP, hctx, True, False),
  _rgbzg         (rgbzg),
  _decoder       (decoder),
  _sp            (sp),
  _first_time    (True),
  _p             (p),
  _error_status  (RGBZP_SUCCESSFUL)
{
  _intensityscale = 0;
  _compressionscale = 0;
  _sc = 0;

  if (!_rgbzg || !_decoder || !_sp) {
    _error_status = RGBZP_BAD_INPUTS;
    return;
  }

  setDefaultResources (_p, name, defres);

  _intensityscale = new RGBZIntensityScale (this, "intensityscale",
    getHelpCtx(), 0, this);

  if (!_intensityscale) {
    _error_status = RGBZP_MEMORY_ALLOCATION_ERROR;
    return;
  }

  _compressionscale = new RGBZCompressionScale (this, "compressionscale",
    getHelpCtx(), 0, this);

  if (!_compressionscale) {
    _error_status = RGBZP_MEMORY_ALLOCATION_ERROR;
    return;
  }

  _sc = new SeisColor (_sp);
  if (!_sc) {
    _error_status = RGBZP_MEMORY_ALLOCATION_ERROR;
    return;
  }
}

RGBZPop::~RGBZPop ()
{
  if (_intensityscale)   delete _intensityscale;
  if (_compressionscale) delete _compressionscale;
  if (_sc) {
    resetSeisPlotLUT ();
    delete _sc;
  }
}

int RGBZPop::initialize ()
{
// remember the color amplitudes for _sp for later restoration
  _color_ampmin = _sp->minColorAmp ();
  _color_ampmax = _sp->maxColorAmp ();

  if (!getSeisPlotLUT())   return 0;
  if (!initializeHelper()) return 0;

// invoke the Color LUT results
  setLUT ();

  DoAction ();

  return 1;
}

// get the color look-up-table but do not get the previous Z's
int RGBZPop::getSeisPlotLUT ()
{
  int num_colors = _sc->getNumColors ();
  if (!_rgbzg->setSize(num_colors)) {
    _error_status = _rgbzg->errorStatus ();
    return 0;
  }
  float red, green, blue, attribute;
  _sc->getAColor (0, &red, &green, &blue, &attribute);
  _rgbzg->setOne (0, red, green, blue, (float)0);  // clear out old Z's
  for (int k2 = 1; k2 < num_colors; k2++) {
    _sc->getAColor (k2, &red, &green, &blue, &attribute);
    _rgbzg->setNext (red, green, blue, (float)0);  // clear out old Z's
  }
  if (!_rgbzg->prepareUse()) {
    _error_status = _rgbzg->errorStatus ();
    return 0;
  }
  return 1;
}

void RGBZPop::setLUT ()
{
  int num_colors = _decoder->getDecoderSize ();
  _sc->setNumColors (num_colors);
  float red, green, blue, attribute;
  _rgbzg->getOneCode (0, &red, &green, &blue, &attribute);
  _sc->setAColor (0, red, green, blue, attribute);
  for (int k2 = 1; k2 < num_colors; k2++) {
    _rgbzg->getNextCode (&red, &green, &blue, &attribute);
    _sc->setAColor (k2, red, green, blue, attribute);
  }
  _sp->setMinColorAmp (_decoder->getMinimumCode());
  _sp->setMaxColorAmp (_decoder->getMaximumCode());
  _sc->prepareColors ();
}

void RGBZPop::resetSeisPlotLUT ()
{
  int num_colors = _rgbzg->getSize ();
  _sc->setNumColors (num_colors);
  float red, green, blue, attribute;
  _rgbzg->getOne (0, &red, &green, &blue, &attribute);
  _sc->setAColor (0, red, green, blue, attribute);
  for (int k2 = 1; k2 < num_colors; k2++) {
    _rgbzg->getNext (&red, &green, &blue, &attribute);
    _sc->setAColor (k2, red, green, blue, attribute);
  }

  _sp->setMinColorAmp (_color_ampmin);
  _sp->setMaxColorAmp (_color_ampmax);
  _sc->prepareColors ();
}

int RGBZPop::notifyColor (SeisColor *sc, Boolean reploted)
{
  assert (sc);
  if (reploted) {} // talk to M. Sherril about handling a replotted SeisPlot
  if (!changeSeisPlotLUT(sc)) {
    _error_status = _rgbzg->errorStatus ();
    return 0;
  }
  setLUT ();
  return 1;
}

float RGBZPop::getMinimumCode ()
{
  return (float)_decoder->getMinimumCode ();
}

float RGBZPop::getMaximumCode ()
{
  return (float)_decoder->getMaximumCode ();
}

int RGBZPop::failed ()
{
  return (int) (_error_status != RGBZP_SUCCESSFUL);
}

Widget RGBZPop::make (Widget /*p*/)
{
// it is assumed that derived class checks to see if made and makes SLFPopSep

  _intensityscale  ->make (topWidget());
  _compressionscale->make (topWidget());

  XtVaSetValues (_intensityscale->W(),
                   XmNtopAttachment,    XmATTACH_WIDGET,
		   XmNtopWidget,        _previous_widget, // frm derived class
                   XmNleftAttachment,   XmATTACH_FORM,
                   XmNleftOffset,       5,
                   XmNrightAttachment,  XmATTACH_FORM,
                   XmNrightOffset,      5,
                   XmNminimum,          1,
                   XmNmaximum,          20,
                   XmNvalue,            10,
                   XmNdecimalPoints,    1,
                   NULL);

  Widget intensitylab = XtVaCreateManagedWidget ("intensitylab",
                          xmLabelWidgetClass, topWidget(),
                          XmNleftAttachment,  XmATTACH_FORM,
                          XmNleftOffset,      5,
                          XmNtopAttachment,   XmATTACH_WIDGET,
                          XmNtopWidget,       _intensityscale->W(),
                          NULL);

  XtVaSetValues (_compressionscale->W(),
                   XmNtopAttachment,    XmATTACH_WIDGET,
                   XmNtopWidget,        intensitylab,
                   XmNleftAttachment,   XmATTACH_FORM,
                   XmNleftOffset,       5,
                   XmNrightAttachment,  XmATTACH_FORM,
                   XmNrightOffset,      5,
                   XmNminimum,          -100,
                   XmNmaximum,          100,
                   XmNvalue,            0,
                   XmNdecimalPoints,    0,
                   NULL);

  Widget compressionlab = XtVaCreateManagedWidget ("compressionlab",
                            xmLabelWidgetClass, topWidget(),
                            XmNleftAttachment,  XmATTACH_FORM,
                            XmNleftOffset,      5,
                            XmNtopAttachment,   XmATTACH_WIDGET,
                            XmNtopWidget,       _compressionscale->W(),
                            NULL);

  Widget bottomphantomlab = XtVaCreateManagedWidget ("",
                            xmLabelWidgetClass,  topWidget(),
                            XmNtopAttachment,    XmATTACH_WIDGET,
                            XmNtopWidget,        compressionlab,
                            XmNtopOffset,        3,
                            XmNbottomAttachment, XmATTACH_WIDGET,
                            XmNbottomWidget,     bottomSeparator(),
                            XmNbottomOffset,     2,
                            NULL);

  return topWidget ();
}

void RGBZPop::manage ()
{
  XtManageChild (topWidget());
}

void RGBZPop::DoAction ()
{
  ParentClass::DoAction ();
  _first_time = False;
}

int RGBZPop::changeSeisPlotLUT (SeisColor *sc)
{
  int num_colors = sc->getNumColors ();
  if (!_rgbzg->setSize(num_colors)) {
    _error_status = _rgbzg->errorStatus ();
    return 0;
  }
  float red, green, blue, attribute;
  sc->getAColor (0, &red, &green, &blue, &attribute);
  _rgbzg->setOne (0, red, green, blue, attribute);
  for (int k2 = 1; k2 < num_colors; k2++) {
    sc->getAColor (k2, &red, &green, &blue, &attribute);
    _rgbzg->setNext (red, green, blue, attribute);
  }
  if (!_rgbzg->prepareUse()) {
    _error_status = _rgbzg->errorStatus ();
    return 0;
  }
  return 1;
}

RGBZIntensityScale::RGBZIntensityScale (SLDelay *contain, char *name, HelpCtx
  hctx, int *valptr, RGBZPop *rgbz_pop) :
  SLScaleDrag (contain, name, hctx, valptr),
  _rgbz_pop   (rgbz_pop)
{
  assert (_rgbz_pop);
}

void RGBZIntensityScale::ScaleAction (int /*val*/)
{
}

void RGBZIntensityScale::ScaleActionDrag (int val)
{
  _rgbz_pop->_rgbzg->setIntensity ((long)val);
  _rgbz_pop->setLUT ();
}

void RGBZIntensityScale::ScaleActionVC (int val)
{
  _rgbz_pop->_rgbzg->setIntensity ((long)val);
  _rgbz_pop->setLUT ();
}

RGBZCompressionScale::RGBZCompressionScale (SLDelay *contain, char *name,
  HelpCtx hctx, int *valptr, RGBZPop *rgbz_pop) :
  SLScaleDrag (contain, name, hctx, valptr),
  _rgbz_pop   (rgbz_pop)
{
  assert (_rgbz_pop);
}

void RGBZCompressionScale::ScaleAction (int /*val*/)
{
}

void RGBZCompressionScale::ScaleActionDrag (int val)
{
  _rgbz_pop->_rgbzg->setCompression ((long)val);
  _rgbz_pop->setLUT ();
}

void RGBZCompressionScale::ScaleActionVC (int val)
{
  _rgbz_pop->_rgbzg->setCompression ((long)val);
  _rgbz_pop->setLUT ();
}
