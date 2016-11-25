// class that creates the color bar builder menu
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
#include "color/color_selector_pop.hh"
#include "color/cbb_color_set.hh"
#include "color/cbb_color_picker.hh"
#include "color/cbb_rgb_set.hh"
#include "color/rgb_to_bhs.hh"
#include "color/color_descriptor.hh"
#include "color/amplitude_processor.hh"
#include "sl/sl_form.hh"
#include "sl/psuedo_widget.hh"
#include "sl/sl_scale_text_arrow.hh"
#include "sl/sl_centering_form.hh"

#include <Xm/Label.h>
#include <math.h>
#include <assert.h>

static String defres[] = {
  "*csp_title.labelString:          Color Selector",
  "*csp_title.fontList:             -*-*-bold-r-*-*-*-180-*-*-p-*-*-*",
  "*hue.labelString:                Hue:  ",
  "*brightness.labelString:         Brightness:  ",
  "*saturation.labelString:         Saturation",
  "*red.labelString:                Red",
  "*green.labelString:              Green",
  "*blue.labelString:               Blue",
  "*active.labelString:             Active Color",
  0};

ColorSelectorPop::ColorSelectorPop (Widget parent, char *name,
  HelpCtx hctx, ColorBarBuilderPop *cbb, int ncols) :
  SLFPopSep (parent, name, FP_DOALL, hctx, False, False, True, ncols),
  _cbb           (cbb),
  _been_managed  (False),
  _status        (1)
{
  setDefaultResources (parent, name, defres);
  init (ncols);
}

ColorSelectorPop::ColorSelectorPop (SLDelay *container, char *name,
  HelpCtx hctx, ColorBarBuilderPop *cbb, int ncols) :
  SLFPopSep (container, name, FP_DOALL, hctx, False, False, True, ncols),
  _cbb           (cbb),
  _been_managed  (False),
  _status        (1)
{
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
    make (container->topWidget());
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
  init (ncols);
}

ColorSelectorPop::~ColorSelectorPop ()
{
// deactivate SL objects to prevent calls after the following deletes
  if (_active)     _active    ->deactivate ();
  if (_hue)        _hue       ->deactivate ();
  if (_brightness) _brightness->deactivate ();

// non SL objects must be deleted to prevent memory leaks
  if (_active_rgb)     delete _active_rgb,     _active_rgb     = 0;
  if (_hue_rgb)        delete _hue_rgb,        _hue_rgb        = 0;
  if (_brightness_rgb) delete _brightness_rgb, _brightness_rgb = 0;

  if (_active_col)     delete _active_col,     _active_col     = 0;
  if (_hue_col)        delete _hue_col,        _hue_col        = 0;
  if (_brightness_col) delete _brightness_col, _brightness_col = 0;

  if (_active_amp)     delete _active_amp,     _active_amp     = 0;
  if (_hue_amp)        delete _hue_amp,        _hue_amp        = 0;
  if (_brightness_amp) delete _brightness_amp, _brightness_amp = 0;

  if (_rgb_to_bhs)     delete _rgb_to_bhs,     _rgb_to_bhs     = 0;
}

Widget ColorSelectorPop::make (Widget parent)
{
  Arg args[10];
  int nargs;

  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  SLFPopSep::make (parent);

  // There are other potential problems because there is an assumed 33
  //   colors beyond ColorBarBuilder::_max_levels assumed available in
  //   SeisCBBPop. SLShellContainer never checks for more than _max_levels!
  
  if (!(_active_col = new ColorDescriptor (W(), 1))) _status = 0;
  if (!(_active_amp = new AmplitudeProcessor ())) {
    _status = 0;
  }
  else {
    _active_amp->setLevels (1);
    _active_amp->setCellsPerLevel (1);
    _active_rgb = new CBBRGBSet (_active_amp);
  }

  _active = new CBBColorSet (_active_cf, "active_color", getHelpCtx());

  XtVaSetValues (_active->W(),
                         XmNwidth,            (Dimension)100,
                         XmNheight,           (Dimension)100,
                         NULL);

  _active->initialize (_active_col, _active_rgb);
  
  if (!(_hue_col = new ColorDescriptor (W(), _ncols/2))) _status = 0;
  if (!(_hue_amp = new AmplitudeProcessor ())) {
    _status = 0;
  }
  else {
    _hue_amp->setLevels (_ncols/2);
    _hue_amp->setCellsPerLevel (1);
    _hue_rgb = new CBBRGBSet (_hue_amp);
  }

  _hue = new CBBColorSet (this, "hue_set", getHelpCtx());
  _hue->initialize (_hue_col, _hue_rgb);

  _hue->setComplexNotify (this);
  
  if (!(_brightness_col = new ColorDescriptor (W(), (_ncols-1)/2)))
    _status = 0;
  if (!(_brightness_amp = new AmplitudeProcessor ())) {
    _status = 0;
  }
  else {
    _brightness_amp->setLevels ((_ncols-1)/2);
    _brightness_amp->setCellsPerLevel (1);
    _brightness_rgb = new CBBRGBSet (_brightness_amp);
  }

  _brightness = new CBBColorSet (this, "brightness_set", getHelpCtx());
  _brightness->initialize (_brightness_col, _brightness_rgb);

  _brightness->setComplexNotify (this);

  initRGBs ();
  
  Widget title = XtVaCreateManagedWidget ("csp_title",
                         xmLabelWidgetClass,  _title_cf->topWidget(),
                         NULL);
  
  Widget hue = XtVaCreateManagedWidget ("hue",
                         xmLabelWidgetClass,  topWidget(),
                         NULL);
			 
  Widget brightness = XtVaCreateManagedWidget ("brightness",
                         xmLabelWidgetClass,  topWidget(),
                         NULL);
  
  Widget active = XtVaCreateManagedWidget ("active",
                         xmLabelWidgetClass,  _active_cf->topWidget(),
                         NULL);


  nargs = 0;
  XtSetArg (args[nargs], XmNtopAttachment,   XmATTACH_FORM); nargs++;
  XtSetArg (args[nargs], XmNtopOffset,       5            ); nargs++;
  XtSetArg (args[nargs], XmNleftAttachment,  XmATTACH_FORM); nargs++;
  XtSetArg (args[nargs], XmNrightAttachment, XmATTACH_FORM); nargs++;
  XtSetValues (_title_cf->W(), args, nargs);
/*
  XtVaSetValues (_title_cf->W(),
                         XmNtopAttachment,    XmATTACH_FORM,
                         XmNtopOffset,        5,
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNrightAttachment,  XmATTACH_FORM,
                         NULL);
*/

  XtVaSetValues (_saturation_arrow->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _title_cf->W(),
                         XmNtopOffset,        10,
                         XmNrightAttachment,  XmATTACH_FORM,
                         XmNrightOffset,      5,
                         NULL);

  XtVaSetValues (_brightness->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _saturation_arrow->W(),
                         XmNtopOffset,        10,
                         XmNrightAttachment,  XmATTACH_WIDGET,
                         XmNrightWidget,      _saturation_arrow->W(),
                         XmNrightOffset,      10,
                         XmNwidth,            (Dimension)250,
                         XmNheight,           (Dimension)50,
                         NULL);

 XtVaSetValues (brightness,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _brightness->W(),
                         XmNrightAttachment,  XmATTACH_WIDGET,
                         XmNrightWidget,      _brightness->W(),
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _brightness->W(),
                         NULL);

  XtVaSetValues (_hue->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _saturation_arrow->W(),
                         XmNrightAttachment,  XmATTACH_WIDGET,
                         XmNrightWidget,      _saturation_arrow->W(),
                         XmNrightOffset,      10,
                         XmNwidth,            (Dimension)250,
                         XmNheight,           (Dimension)50,
                         NULL);

  XtVaSetValues (hue,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _hue->W(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       brightness,
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _hue->W(),
                         NULL);

  XtVaSetValues (_red_arrow->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _brightness->W(),
                         XmNtopOffset,        10,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       brightness,
                         NULL);

  XtVaSetValues (_green_arrow->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _red_arrow->W(),
                         XmNtopOffset,        5,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       brightness,
                         NULL);

  XtVaSetValues (_blue_arrow->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _green_arrow->W(),
                         XmNtopOffset,        5,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       brightness,
                         NULL);

  Widget phantomB = XtVaCreateManagedWidget ("",
                         xmLabelWidgetClass,  topWidget(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _blue_arrow->W(),
                         XmNtopOffset,        5,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     bottomSeparator(),
                         NULL);
  
  XtVaSetValues (_active_cf->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _red_arrow->W(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _brightness->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _saturation_arrow->W(),
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _blue_arrow->W(),
                         NULL);

  XtVaSetValues (active,
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _active->W(),
                         XmNtopOffset,        5,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _active->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _active->W(),
                         NULL);
			 
  defaultButtonOK (False);
  defaultButton (FP_APPLY, True);
  
  return topWidget ();
}

Boolean ColorSelectorPop::notifyComplex (SLDelay *obj, int ident)
{
  if (obj == _saturation_arrow && _active && _hue && _brightness) {
// the saturation value has changed
    float brightness, hue, saturation;
    _rgb_to_bhs->setRGB (_h_red, _h_green, _h_blue);
    _rgb_to_bhs->BHS (&brightness, &hue, &saturation);
    _rgb_to_bhs->setBHS (brightness, hue, _h_saturation);
    _rgb_to_bhs->RGB (&_h_red, &_h_green, &_h_blue);
    initHueColors ();
    _hue->recolor ();
  }
  else if (obj == _red_arrow && _active && _hue && _brightness) {
// the active red value has changed
    initActiveColor ();
    _active->recolor ();
  }
  else if (obj == _green_arrow && _active && _hue && _brightness) {
// the active green value has changed
    initActiveColor ();
    _active->recolor ();
  }
  else if (obj == _blue_arrow && _active && _hue && _brightness) {
// the active blue value has changed
    initActiveColor ();
    _active->recolor ();
  }
  else if (obj == _hue                             &&
    ident == CBBColorSet::ESTABLISHED_SECOND_INDEX &&
    _active && _hue                                  ) {
// a new active color was selected from the hue color set
    float attribute;
    _hue_rgb->getOne (_hue->secondIndex(), &_a_red, &_a_green, &_a_blue,
      &attribute);
    initActiveColor ();
    _active->recolor ();
  }
  else if (obj == _brightness                      &&
    ident == CBBColorSet::ESTABLISHED_SECOND_INDEX &&
    _active && _brightness                           ) {
// a new active color was selected from the brightness color set
    float attribute;
    _brightness_rgb->getOne (_brightness->secondIndex(), &_a_red,
      &_a_green, &_a_blue, &attribute);
    initActiveColor ();
    _active->recolor ();
  }
  return True;
}

void ColorSelectorPop::DoAction ()
{
// reinitialize all of the the color sets
  initColors ();

  initActiveColor ();
  _active->recolor ();

  initHueColors ();
  _hue->recolor ();

  initBrightnessColors ();
  _brightness->recolor ();
}

void ColorSelectorPop::managing ()
{
  if (!_been_managed) {
    _hue->picker()->enable ();
    _brightness->picker()->enable ();

    _been_managed = True;
  }

  _hue_col->allocateColorCells ();
  _brightness_col->allocateColorCells ();
  _active_col->allocateColorCells ();

  _hue->manage ();
  _brightness->manage ();
  _active->manage ();

}

void ColorSelectorPop::activeColor (float *red, float *green, float *blue)
{
  *red   = _a_red;
  *green = _a_green;
  *blue  = _a_blue;
}

void ColorSelectorPop::setActiveColor (float red, float green, float blue)
{
  _a_red   = red;
  _a_green = green;
  _a_blue  = blue;
  DoAction ();
}

void ColorSelectorPop::init (int ncols)
{
  _active_col = 0;
  _active_amp = 0;
  _active_rgb = 0;

  _hue_col = 0;
  _hue_amp = 0;
  _hue_rgb = 0;

  _brightness_col = 0;
  _brightness_amp = 0;
  _brightness_rgb = 0;

  _active     = 0;
  _hue        = 0;
  _brightness = 0;

  _rgb_to_bhs = 0;

  _saturation_arrow = 0;
  _red_arrow        = 0;
  _green_arrow      = 0;
  _blue_arrow       = 0;

  _title_cf  = 0;
  _active_cf = 0;

  _pi = (float)atan2 ((double)0,(double)(-1));

  assert (ncols > 4);
  _ncols = ncols;

  _rgb_to_bhs = new RGBToBHS ();
  _a_red   = 1;
  _a_green = 0;
  _a_blue  = 0;

  initColors ();

  _saturation_arrow = new SLScaleTextArrow (this, "saturation_arrow",
    getHelpCtx(), &_h_saturation, True, _h_saturation, (float)0, (float)1,
    (float).005, (int)3, (int)5, XmHORIZONTAL, "saturation",
    (Dimension)100);

  _saturation_arrow->setComplexNotify (this);

  _red_arrow = new SLScaleTextArrow (this, "red_arrow", getHelpCtx(),
    &_a_red, True, _a_red, (float)0, (float)1, (float).005, (int)3,
    (int)5, XmHORIZONTAL, "red", (Dimension)100);

  _red_arrow->setComplexNotify (this);

  _green_arrow = new SLScaleTextArrow (this, "green_arrow", getHelpCtx(),
    &_a_green, True, _a_green, (float)0, (float)1, (float).005, (int)3,
    (int)5, XmHORIZONTAL, "green", (Dimension)100);

  _green_arrow->setComplexNotify (this);

  _blue_arrow = new SLScaleTextArrow (this, "blue_arrow", getHelpCtx(),
    &_a_blue, True, _a_blue, (float)0, (float)1, (float).005, (int)3,
    (int)5, XmHORIZONTAL, "blue", (Dimension)100);

  _blue_arrow->setComplexNotify (this);

  _title_cf = new SLCenteringForm (this, "title_cf", getHelpCtx());

  _active_cf = new SLCenteringForm (this, "active_cf", getHelpCtx());

}

void ColorSelectorPop::initActiveColor ()
{
  if (!_active_rgb) return;
// set the active color on the active CBBRGBSet
  _active_rgb->setOne (0, _a_red, _a_green, _a_blue,
    _active_rgb->getAttribute(0));
  if (_red_arrow  ) _red_arrow  ->setValue (_a_red  );
  if (_green_arrow) _green_arrow->setValue (_a_green);
  if (_blue_arrow ) _blue_arrow ->setValue (_a_blue );
}

void ColorSelectorPop::initHueColors ()
{
  if (!_hue_rgb) return;
  if (_hue_col->numColors() < 2) return;

// set the hue colors on the hue CBBRGBSet
  float red, green, blue, brightness, hue, saturation, del_hue, dist;
  float min_dist = 0;
  int k2, min_k2;

// determine the brightness and saturation of the hue color
  _rgb_to_bhs->setRGB (_h_red, _h_green, _h_blue);
  _rgb_to_bhs->BHS (&brightness, &hue, &saturation);

// determine hue step
  del_hue = (float)2 * _pi / (float)(_hue_col->numColors());

// initialize loop
  hue = (float)0;

// loop through the hues
// remember which loop through hue was closest to the hue color
  for (k2 = 0; k2 < _hue_col->numColors(); k2++) {
    _rgb_to_bhs->setBHS (brightness, hue, saturation);

    dist = _rgb_to_bhs->distance (_h_red, _h_green, _h_blue);
    if (k2 == 0 || dist < min_dist) {
      min_dist = dist;
      min_k2 = k2;
    }

    _rgb_to_bhs->RGB (&red, &green, &blue);
    _hue_rgb->setOne (k2, red, green, blue, _hue_rgb->getAttribute(k2));
    hue += del_hue;
  }

// replace the loop through hue that was closest to the hue color
//   with the hue color
  _hue_rgb->setOne (min_k2, _h_red, _h_green, _h_blue,
    _hue_rgb->getAttribute(min_k2));
}

void ColorSelectorPop::initBrightnessColors ()
{
  if (!_brightness_rgb) return;
  if (_brightness_col->numColors() < 2) return;

// set the brightness colors on the brightness CBBRGBSet
  float red, green, blue, brightness, hue, saturation, del_brightness, dist;
  float min_dist = 0;
  int k2, min_k2;

// determine the hue and saturation of the brightness color
  _rgb_to_bhs->setRGB (_b_red, _b_green, _b_blue);
  _rgb_to_bhs->BHS (&brightness, &hue, &saturation);

// determine brightness step
  del_brightness = (float).998 / (float)(_brightness_col->numColors()-1);

// initialize loop
  brightness = (float).001;

// loop through the brightnesses
// remember which loop through brightness was closest to the brightness
//   color
  for (k2 = 0; k2 < _brightness_col->numColors(); k2++) {
    _rgb_to_bhs->setBHS (brightness, hue, saturation);

    dist = _rgb_to_bhs->distance (_b_red, _b_green, _b_blue);
    if (k2 == 0 || dist < min_dist) {
      min_dist = dist;
      min_k2 = k2;
    }

    _rgb_to_bhs->RGB (&red, &green, &blue);
    _brightness_rgb->setOne (k2, red, green, blue,
      _brightness_rgb->getAttribute(k2));
    brightness += del_brightness;
  }

// replace the loop through brightness that was closest to the brightness
//   color with the brightness color
  _brightness_rgb->setOne (min_k2, _b_red, _b_green, _b_blue,
    _brightness_rgb->getAttribute(min_k2));
}

void ColorSelectorPop::initColors ()
{
// make the current color the active color
  _rgb_to_bhs->setRGB (_a_red, _a_green, _a_blue);

// define the hue saturation to be the saturation of the active color
  float brightness, hue;
  _rgb_to_bhs->BHS (&brightness, &hue, &_h_saturation);
  if (_saturation_arrow) _saturation_arrow->setValue (_h_saturation);

// initialize the hue color and the brightness color to be the active color
  _h_red = _a_red;
  _h_green = _a_green;
  _h_blue  = _a_blue;

  _b_red   = _a_red;
  _b_green = _a_green;
  _b_blue  = _a_blue;
}

void ColorSelectorPop::initRGBs ()
{
  _active_rgb->setSize (1);
  initActiveColor ();
  _active_rgb->initializeLUTs (_active_col);
  _active_rgb->prepareUse ();

  _hue_rgb->setSize (_ncols/2);
  initHueColors ();
  _hue_rgb->initializeLUTs (_hue_col);
  _hue_rgb->prepareUse ();

  _brightness_rgb->setSize ((_ncols-1)/2);
  initBrightnessColors ();
  _brightness_rgb->initializeLUTs (_brightness_col);
  _brightness_rgb->prepareUse ();
}
