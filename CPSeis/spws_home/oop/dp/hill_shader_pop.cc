// Used to work with a HillShader object to allow the user to interactively
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
//   change the illumination azimuth in real time and allow setting intensity
#include <Xm/Label.h>
#include <Xm/Frame.h>
#include <Xm/Separator.h>
#include <stdio.h>
#include <assert.h>
#include "sl/sl_radio_box.hh"
#include "dp/hill_shader_pop.hh"
#include "dp/hill_shader.hh"
#include "sp/seis_plot.hh"
#include "sp/seis_color.hh"
#include "sp/seis_scale.hh"
#include "dp/grid_error_handler.hh"


static String  defres[]= {
  ".hsp_popup.title:                  Hill Shader",
  "*hillshaderazmlab.labelString:     Illumination Azimuth",
  "*hillshaderelvlab.labelString:     Illumination Elevation",
  "*colorattribHS.labelString:        Color Attribute",
  "*colorattribContours.labelString:  Color Attribute Contours",
  "*BWattribHS.labelString:           B&W Attribute",
  "*BWattribContours.labelString:     B&W Attribute Contours",
  "*BWstructHS.labelString:           B&W Structure",
  0};


#define ParentClass SLFPopSep


HillShaderPop::HillShaderPop (Widget            p,
                              char              *name,
                              HillShader        *hs,
                              SeisPlot          *sp,
                              HelpCtx           hctx):

  RGBZPop (p, name, (RGBZGenerator *)hs, (Decoder *)hs, sp, hctx),
  _hs            (hs)
{
  _hillshaderazimuth   = 0;
  _hillshaderelevation = 0;

  if (failed()) {
    GridErrorHandler *geh = new GridErrorHandler (_p, "Hill Shader Pop Error",
      _error_status);
    return;
  }

  static SLRadio types[] =
  {
      {"colorattribHS",        HillShader::COLOR_ATTRIBUTE},
      {"colorattribContours",  HillShader::COLOR_ATTRIBUTE_CONTOURS},
      {"BWattribHS",           HillShader::B_W_ATTRIBUTE},
      {"BWattribContours",     HillShader::B_W_ATTRIBUTE_CONTOURS},
      {"BWstructHS",           HillShader::B_W_STRUCTURE},
  };

  setDefaultResources (_p, name, defres);

  _hillshaderazimuth =
          new HillShaderAzimuth (this, "hillshaderazimuth", getHelpCtx(),
                               0, this);
  if (!_hillshaderazimuth) {
    _error_status = RGBZP_MEMORY_ALLOCATION_ERROR;
    GridErrorHandler *geh = new GridErrorHandler (_p, "Hill Shader Pop Error",
      _error_status);
    return;
  }

  _hillshaderelevation =
          new HillShaderElevation (this, "hillshaderelevation", getHelpCtx(),
                               0, this);
  if (!_hillshaderelevation) {
    _error_status = RGBZP_MEMORY_ALLOCATION_ERROR;
    GridErrorHandler *geh = new GridErrorHandler (_p, "Hill Shader Pop Error",
      _error_status);
    return;
  }

  _typerbox = new SLRadioBox (this, "typerbox", getHelpCtx(),
    types, XtNumber(types), NULL, True, False);
  _typerbox->setAltChoiceAction ((SLRadioButtonfunc)typeAction, this);

}

HillShaderPop::~HillShaderPop()
{
  if (_hillshaderazimuth)   delete _hillshaderazimuth;
  if (_hillshaderelevation) delete _hillshaderelevation;
}

Widget HillShaderPop::make(Widget p)
{
  if (made()) return topWidget();
  SLFPopSep::make(p);

  _hillshaderazimuth->make(topWidget());

  XtVaSetValues (_hillshaderazimuth->W(),
                              XmNtopAttachment,    XmATTACH_FORM,
                              XmNtopOffset,        5,
                              XmNleftAttachment,   XmATTACH_FORM,
                              XmNleftOffset,       5, 
                              XmNrightAttachment,  XmATTACH_FORM,
                              XmNrightOffset,      5,
                              XmNminimum,          -180,
                              XmNmaximum,          180,
                              XmNvalue,            0,
                              XmNdecimalPoints,    0,
                              NULL);

  Widget hillshaderazmlab = XtVaCreateManagedWidget ("hillshaderazmlab",
                              xmLabelWidgetClass,  topWidget(),
                              XmNleftAttachment,   XmATTACH_FORM,
                              XmNleftOffset,       5,
                              XmNtopAttachment,    XmATTACH_WIDGET,
                              XmNtopWidget,        _hillshaderazimuth->W(),
                              NULL);

  _hillshaderelevation->make(topWidget());

  XtVaSetValues (_hillshaderelevation->W(),
                              XmNtopAttachment,    XmATTACH_WIDGET,
                              XmNtopWidget,        hillshaderazmlab,
                              XmNtopOffset,        8,
                              XmNleftAttachment,   XmATTACH_FORM,
                              XmNleftOffset,       5, 
                              XmNrightAttachment,  XmATTACH_FORM,
                              XmNrightOffset,      5,
                              XmNminimum,          0,
                              XmNmaximum,          90,
                              XmNvalue,            45,
                              XmNdecimalPoints,    0,
                              NULL);

  Widget hillshaderelvlab = XtVaCreateManagedWidget ("hillshaderelvlab",
                              xmLabelWidgetClass,  topWidget(),
                              XmNleftAttachment,   XmATTACH_FORM,
                              XmNleftOffset,       5,
                              XmNtopAttachment,    XmATTACH_WIDGET,
                              XmNtopWidget,        _hillshaderelevation->W(),
                              NULL);

  _typerbox->SetRadio ((long)_hs->type());
  adjustSensitivity ();

  XtVaSetValues (_typerbox->W(),
                              XmNtopAttachment,    XmATTACH_WIDGET,
                              XmNtopWidget,        hillshaderelvlab,
                              XmNtopOffset,        8,
                              XmNleftAttachment,   XmATTACH_FORM,
                              XmNleftOffset,       5, 
                              XmNrightAttachment,  XmATTACH_FORM,
                              XmNrightOffset,      5,
                              NULL);

  _previous_widget = _typerbox->W();

  return RGBZPop::make (p);
}

int HillShaderPop::getSeisPlotLUT ()
{
  if (!(changeSeisPlotLUT (_sc))) {
    GridErrorHandler *geh = new GridErrorHandler (_p, "Hill Shader Pop Error",
      _error_status);
    return 0;
  }
  return 1;
}

int HillShaderPop::initializeHelper ()
{
// initialize for 0 deg azimuth
  if (!(_hs->setAzimuth (0))) {
    _error_status = _hs->errorStatus ();
    GridErrorHandler *geh = new GridErrorHandler (_p, "Hill Shader Pop Error",
      _error_status);
    return 0;
  }

// initialize for 45 deg elevation
  if (!(_hs->setElevation (45))) {
    _error_status = _hs->errorStatus ();
    GridErrorHandler *geh = new GridErrorHandler (_p, "Hill Shader Pop Error",
      _error_status);
    return 0;
  }

  _hs->setColorAttribute ();

  return 1;
}

void HillShaderPop::typeAction (void *data, long which)
{
  HillShaderPop *obj = (HillShaderPop *)data;
  obj->_hs->setType ((HillShader::Type)which);
  obj->adjustSensitivity ();
  obj->setLUT ();
}

void HillShaderPop::adjustSensitivity ()
{
  if (_hs->type() == HillShader::COLOR_ATTRIBUTE_CONTOURS ||
      _hs->type() == HillShader::B_W_ATTRIBUTE_CONTOURS     ) {
    XtSetSensitive (_hillshaderazimuth->W(),   False);
    XtSetSensitive (_hillshaderelevation->W(), False);
  }
  else {
    XtSetSensitive (_hillshaderazimuth->W(),   True);
    XtSetSensitive (_hillshaderelevation->W(), True);
  }
}




HillShaderAzimuth::HillShaderAzimuth (SLDelay *contain, char *name,
  HelpCtx hctx, int *valptr, HillShaderPop *hsp)
  : SLScaleDrag (contain, name, hctx, valptr),
    _hsp        (hsp)
{
  assert (_hsp);
}

void HillShaderAzimuth::ScaleAction (int /*val*/)
{
}

void HillShaderAzimuth::ScaleActionDrag (int val)
{
  _hsp->_hs->setAzimuth ((float)val);
  _hsp->setLUT ();
}

void HillShaderAzimuth::ScaleActionVC (int val)
{
  _hsp->_hs->setAzimuth ((float)val);
  _hsp->setLUT ();
}





HillShaderElevation::HillShaderElevation (SLDelay *contain, char *name,
  HelpCtx hctx, int *valptr, HillShaderPop *hsp)
  : SLScaleDrag (contain, name, hctx, valptr),
    _hsp        (hsp)
{
  assert (_hsp);
}

void HillShaderElevation::ScaleAction (int /*val*/)
{
}

void HillShaderElevation::ScaleActionDrag (int val)
{
  _hsp->_hs->setElevation ((float)val);
  _hsp->setLUT ();
}

void HillShaderElevation::ScaleActionVC (int val)
{
  _hsp->_hs->setElevation ((float)val);
  _hsp->setLUT ();
}
