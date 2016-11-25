#include "cube/cube_amplitude_gui.hh"
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
#include "cube/cube_master.hh"
#include "cube/cube_display.hh"
#include "cube/cube.hh"
#include "cube/cube_plot_error.hh"
#include "cube/cube_amp_params.hh"
#include "sl/radio_list.hh"
#include "sl/slp_radio.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_push_box.hh"
#include "sl/shell_watch.hh"
#include "sl/error_handler.hh"
#include "plot_image.hh"
#include <Xm/Xm.h>
#include <Xm/Label.h>

enum {
  AMP, STP, /* STPS, */ NRM, STC, STAC, STA
};

enum {
  ALLCUBESNORM = PlotImage::FILENORM+1, AMPNORM, PANELSNORM
};

static String defres[] = {
  "*stp.labelString:          Scale to Panel",
/*"*stps.labelString:         Scale to All Panels", */
  "*stp.labelString:          Scale to Panel",
  "*norm.labelString:         Normalize",
  "*stc.labelString:          Scale to Cube",
  "*stac.labelString:         Scale to All Cubes",
  "*sta.labelString:          Scale to Amplitude->",
  "*amp.labelString:          ",
  "*amp.value:                1.0",
  "*stc.set:                  True",
  0
};

CubeAmplitudeGui::CubeAmplitudeGui (Widget p, char *name, HelpCtx hctx,
  CubeDisplay *cube_display) :
  SLFPopSep (p, name, FP_DOALL, hctx, False, False),
  _cube_display    (cube_display),
  _dont_plot_yet   (False),
  _wiggle_trace    (True),
  _norm_state      (PlotImage::FILENORM)
{
  static SLText Scale_text[]  = {
    {"amp", "range:0.0 *   , default:1.00 ", NULL, SLType_float, AMP},
  };

  Scale_text[0].target=&_amp;

  setDefaultResources (p, name, defres);

  _norm_list = new RadioList ();

  _stp  = new SLpRadio (this, "stp",  PlotImage::PANELNORM,    0, _norm_list);
/*_stps = new SLpRadio (this, "stps", PANELSNORM,   0, _norm_list); */
  _norm = new SLpRadio (this, "norm", PlotImage::NORM,         0, _norm_list);
  _stc  = new SLpRadio (this, "stc",  PlotImage::FILENORM,     0, _norm_list);
  _stac = new SLpRadio (this, "stac", ALLCUBESNORM, 0, _norm_list);
  _sta  = new SLpRadio (this, "sta",  AMPNORM,      0, _norm_list);

  _norm_list->setupIvarPoint (&_norm_state);

  _ampui = new SLTextBox (this, "ampui", getHelpCtx(), Scale_text,
    XtNumber(Scale_text), False);

  _ampui->setAltLosingAction ((SLTextfunc)ampuiLosingFocusAction, this);
  _ampui->setAltFocusAction  ((SLTextfunc)ampuiFocusAction, this);

  setParams ();

  
}

CubeAmplitudeGui::~CubeAmplitudeGui ()
{
  delete _norm_list;
  //delete _stp;
  //delete _norm;
  //delete _stc;
  //delete _stac;
  //delete _sta;
  //delete _ampui;
}

Widget CubeAmplitudeGui::make (Widget p)
{
  if (made()) return topWidget ();
  SLFPopSep::make (p);
  p = wParent ();

//  defaultButtonOK (False);
  defaultButton (FP_APPLY, True);

  XtVaSetValues (_sta->W(),
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNleftOffset,       10,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     bottomSeparator(),
                         XmNbottomOffset,     10,
                         NULL);

 XtVaSetValues (_ampui->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _sta->W(),
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _sta->W(),
                         NULL);

  Widget phantomlabR = XtVaCreateManagedWidget (
                         "",                 xmLabelWidgetClass, 
                         topWidget(),
                         XmNleftAttachment,  XmATTACH_WIDGET,
                         XmNleftWidget,      _ampui->W(),
                         XmNleftOffset,      5,
                         XmNrightAttachment, XmATTACH_FORM,
                         XmNrightOffset,     5,
                         NULL);

  XtVaSetValues (_stac->W(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _sta->W(),
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _sta->W(),
                         XmNbottomOffset,     5,
                         NULL);

  XtVaSetValues (_stc->W(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _sta->W(),
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _stac->W(),
                         XmNbottomOffset,     5,
                         NULL);

  XtVaSetValues (_norm->W(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _sta->W(),
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _stc->W(),
                         XmNbottomOffset,     5,
                         NULL);
/*
  XtVaSetValues (_stps->W(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _sta->W(),
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _norm->W(),
                         XmNbottomOffset,     5,
                         NULL);
*/
  XtVaSetValues (_stp->W(),
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _sta->W(),
                         XmNbottomAttachment, XmATTACH_WIDGET,
/*                       XmNbottomWidget,     _stps->W(),              */
                         XmNbottomWidget,     _norm->W(),
                         XmNbottomOffset,     5,
                         NULL);

  Widget phantomlabT = XtVaCreateManagedWidget (
                         "",                  xmLabelWidgetClass, 
                         topWidget(),
                         XmNtopAttachment,    XmATTACH_FORM,
                         XmNtopOffset,        5,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _stp->W(),
                         XmNbottomOffset,     25,
                         NULL);

  return topWidget ();
}

void CubeAmplitudeGui::managing ()
{
  if (!_wiggle_trace) {
    variableDensitySensitivity ();
  }
  else {
    wiggleTraceSensitivity ();
  }
}

void CubeAmplitudeGui::setParams ()
{
// apply user's inputs
  CubeAmpParams *cube_amp_params = new CubeAmpParams ();

  cube_amp_params->setExternalAmp  ((double)_amp);
  cube_amp_params->setNormType ((int)_norm_state);
  if(_norm_state == ALLCUBESNORM)//Must make sure all cubes get replotted
    _cube_display->allCubesAcceptAndDelete (cube_amp_params);
  else//Scaling requested is for the current displayed cube only
    _cube_display->currentCubeAcceptAndDelete (cube_amp_params);
}

void CubeAmplitudeGui::DoAction() 
{
  setParams ();
  if (_dont_plot_yet && (whichButton() == FP_OK || whichButton() == FP_APPLY))
  {
    _dont_plot_yet = False;
    return;
  }

  ShellWatch sw;
  CubeMaster *cube_master = CubeMaster::instance();
  CubeDisplay *cube_display;
  Cube *cube;
  void *x;
  Boolean status;
  for (cube_display = cube_master->top(&x); cube_display;
    cube_display = cube_master->next(&x)) {
    cube = cube_display->currentDisplayedCube ();
    status = plotIfOk (cube);
    if (!status) {
      CubePlotError error(W(), cube);
    }
  }
}

int CubeAmplitudeGui::plotIfOk (Cube *cube)
{
  if (cube->currentLine() != Cube::NoLinePlotted) {
//  return cube->plotIfNecessary (); // too picky
    return cube->plot ();
  }
  else {
    return (int)1;
  }
}

void CubeAmplitudeGui::ampuiFocusAction (void *data, long /* ident */)
{
  CubeAmplitudeGui *obj = (CubeAmplitudeGui *)data;
  obj->_amp_at_focus = obj->_amp;
}

void CubeAmplitudeGui::ampuiLosingFocusAction (void *data, long /* ident */)
{
  CubeAmplitudeGui *obj = (CubeAmplitudeGui *)data;
  if (obj->_amp != obj->_amp_at_focus) {
// make sure amplitude rescaling is set
    obj->_norm_state = AMPNORM;    
  }
}

void CubeAmplitudeGui::variableDensitySensitivity ()
{
  if (_norm_state != PlotImage::FILENORM     &&
      _norm_state !=            ALLCUBESNORM &&
      _norm_state !=            AMPNORM        ) {
    _norm_state = PlotImage::FILENORM;
    if (_stp->W()) _stc->updateEverything ();
  }
  if (_stp->W()) {
    XtSetSensitive (_stp ->W(), False);
    XtSetSensitive (_norm->W(), False);
  }
  _wiggle_trace = False;
}

void CubeAmplitudeGui::wiggleTraceSensitivity ()
{
  if (_stp->W()) {
    XtSetSensitive (_stp ->W(), True);
    XtSetSensitive (_norm->W(), True);
  }
  _wiggle_trace = True;
}



