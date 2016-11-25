#include "cube/cube_section_gui.hh"
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
#include "cube/cube_amplitude_gui.hh"
#include "cube/cube.hh"
#include "cube/cube_params.hh"
#include "cube/cube_plot_error.hh"
#include "sl/sl_radio_box.hh"
#include "sl/sl_text_box.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_tog_box.hh"
#include "sl/shell_watch.hh"
#include "sl/error_handler.hh"
#include "plot_image.hh"
#include <Xm/Xm.h>
#include <Xm/Label.h>

enum {
  LNSCALE, XLSCALE, RLSCALE, TMSCALE, CT, TMIN, TMAX, 
  INLINE_RIGHT_TO_LEFT,   CROSSLINE_RIGHT_TO_LEFT,   TIMESLICE_RIGHT_TO_LEFT,
  RANDOMLINE_RIGHT_TO_LEFT,
  INLINE_INVERT_VERTICAL, CROSSLINE_INVERT_VERTICAL, TIMESLICE_INVERT_VERTICAL,
  RANDOMLINE_INVERT_VERTICAL
};

enum{NEGATIVE_FILL};

static String defres[] = {
  "_popup.title:              Modify Plot Parameters",
  "*gs.labelString:           Variable Density",
  "*wonly.labelString:        Wiggle Only",
  "*wfill.labelString:        Variable Area Pos",
  "*wfilln.labelString:       Variable Area Neg",
  "*lnscllab.labelString:     Crosslines / Inch:",
  "*xlscllab.labelString:     Lines / Inch:",
  "*rlscllab.labelString:     Random Line / Inch:",
  "*tmscllab.labelString:     Inches / Second:",
  "*Inline_rtol.labelString:  Inline",
  "*Crossline_rtol.labelString: Crossline",
  "*Timeslice_rtol.labelString: TimeSlice",
  "*Randomline_rtol.labelString: Random Line",
  "*Inline_invert.labelString:  Inline",
  "*Crossline_invert.labelString: Crossline",
  "*Timeslice_invert.labelString: TimeSlice",
  "*Randomline_invert.labelString: Random Line",
  "*ctlab.labelString:        Ct:",
  "*tminlab.labelString:      Starting Time:",
  "*tmaxlab.labelSTring:      Ending Time:",
  "*lnscale.value:            20.0",
  "*xlscale.value:            20.0",
  "*rlscale.value:            20.0",
  "*tmscale.value:            2.0",
  "*ct.value:                 4.0",
  "*wfill.set:                True",
  0
};

static SLRadio rads[]  = {
  { "gs",    PlotImage::PlotGS    },
  { "wonly", PlotImage::PlotWONLY },
  { "wfill", PlotImage::PlotWFILL },
  { "wfilln",NEGATIVE_FILL },
};

CubeSectionGui::CubeSectionGui (Widget p, char *name, HelpCtx hctx,
  CubeDisplay *cube_display, CubeAmplitudeGui *cube_amp) :
  SLFPopSep (p, name, FP_DOALL, hctx, False, False),
  _cube_display        (cube_display),
  _cube_amp            (cube_amp),
  _extrema_label       (0),
  _time_range_now_set  (False),
  _dont_plot_yet       (False)

{

  static SLText Cube_scale_text[] = {
    {"lnscale", "range:0.1 *   , default:20.00 ", NULL, SLType_float, LNSCALE},
    {"xlscale", "range:0.1 *   , default:20.00 ", NULL, SLType_float, XLSCALE},
    {"rlscale", "range:0.1 *   , default:20.00 ", NULL, SLType_float, RLSCALE},
    {"tmscale", "range:0.0001 *, default:2.0000", NULL, SLType_float, TMSCALE},
    {"ct",      NULL,                             NULL, SLType_float, CT},
    {"tmin",    "range:0.0 *",                    NULL, SLType_float, TMIN},
    {"tmax",    "range:0.0001 *",                 NULL, SLType_float, TMAX},
  };

  Cube_scale_text[0].target=&_lnscale; 
  Cube_scale_text[1].target=&_xlscale;
  Cube_scale_text[2].target=&_rlscale; 
  Cube_scale_text[3].target=&_tmscale; 
  Cube_scale_text[4].target=&_ct; 
  Cube_scale_text[5].target=&_tmin; 
  Cube_scale_text[6].target=&_tmax; 


static SLTog right_to_left[]      = { 
   {"Inline_rtol",    NULL, INLINE_RIGHT_TO_LEFT },
   {"Crossline_rtol", NULL, CROSSLINE_RIGHT_TO_LEFT },
   {"Timeslice_rtol", NULL, TIMESLICE_RIGHT_TO_LEFT },
   {"Randomline_rtol",NULL, RANDOMLINE_RIGHT_TO_LEFT}
};
right_to_left[0].target= &_inline_right_to_left;
right_to_left[1].target= &_crossline_right_to_left;
right_to_left[2].target= &_timeslice_right_to_left;
right_to_left[3].target= &_randomline_right_to_left;

static SLTog invert_vertical_toggles[]      = { 
   {"Inline_invert",    NULL, INLINE_INVERT_VERTICAL },
   {"Crossline_invert", NULL, CROSSLINE_INVERT_VERTICAL },
   {"Timeslice_invert", NULL, TIMESLICE_INVERT_VERTICAL },
   {"Randomline_invert",NULL, RANDOMLINE_INVERT_VERTICAL}
};
invert_vertical_toggles[0].target= &_inline_invert_vertical;
invert_vertical_toggles[1].target= &_crossline_invert_vertical;
invert_vertical_toggles[2].target= &_timeslice_invert_vertical;
invert_vertical_toggles[3].target= &_randomline_invert_vertical;





  setDefaultResources (p, name, defres);

  _ptype = new SLRadioBox (this, "ptype", getHelpCtx(), rads, XtNumber(rads),
    NULL, True);
  _ptype->setComplexNotify (this);

  _cube_scale = new SLTextBox (this, "cube_scale", getHelpCtx(),
                     Cube_scale_text, XtNumber(Cube_scale_text));

  _right_to_left_toggles = new SLTogBox(this,"right_to_left",getHelpCtx(),
                     right_to_left, XtNumber(right_to_left),True);                               
  _invert_vertical_toggles = new SLTogBox(this,"invert_vertical_toggles",
                 getHelpCtx(), invert_vertical_toggles, 
                 XtNumber(invert_vertical_toggles),True);  

  setParams ();
}

CubeSectionGui::~CubeSectionGui ()
{
  delete _ptype;
  delete _cube_scale;
}

Widget CubeSectionGui::make (Widget p)
{
  if (made()) return topWidget ();
  SLFPopSep::make (p);
  p = wParent ();

  Widget phantomlabB = XtVaCreateManagedWidget (
                               "",                  xmLabelWidgetClass, 
                               topWidget(),
                               NULL);

  XtVaSetValues (_ptype->W(),
                               XmNtopAttachment,    XmATTACH_FORM,
                               XmNtopOffset,        10,
                               XmNleftAttachment,   XmATTACH_FORM,
                               XmNleftOffset,       10,
                               NULL);

 XtVaSetValues (_cube_scale->W(),
                               XmNtopAttachment,    XmATTACH_FORM,
                               XmNtopOffset,        10,
                               XmNleftAttachment,   XmATTACH_WIDGET,
                               XmNleftWidget,       _ptype->W(),
                               XmNleftOffset,       20,
                               NULL);

  _extrema_label = XtVaCreateManagedWidget (
                               "",                  xmLabelWidgetClass, 
                               topWidget(),
                               NULL);
  

  XtVaSetValues (_extrema_label,
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _cube_scale->W(),
                               XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,       _cube_scale->W(),
                               NULL);

  XtVaSetValues (phantomlabB,
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _extrema_label,
                               XmNtopOffset,        5,
                               XmNbottomAttachment, XmATTACH_WIDGET,
                               XmNbottomWidget,     bottomSeparator(),
                               XmNbottomOffset,     25,
                               NULL);

  Widget right_to_left_label = XtVaCreateManagedWidget (
                               "Right To Left",    xmLabelWidgetClass, 
                               topWidget(),
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _ptype->W(),
                               XmNtopOffset,        10,
                               XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,       _ptype->W(),
                               XmNleftOffset,       0,
                               NULL);
  
  XtVaSetValues (_right_to_left_toggles->W(),
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        right_to_left_label,
                               XmNtopOffset,        2,
                               XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,       right_to_left_label,
                               XmNleftOffset,       0,
                               NULL);
  Widget invert_vertical_label = XtVaCreateManagedWidget (
                               "Invert Vertical Axis",  xmLabelWidgetClass, 
                               topWidget(),
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _right_to_left_toggles->W(),
                               XmNtopOffset,        10,
                               XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,       _right_to_left_toggles->W(),
                               XmNleftOffset,       0,
                               NULL);

  XtVaSetValues (_invert_vertical_toggles->W(),
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        invert_vertical_label,
                               XmNtopOffset,        2,
                               XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,       invert_vertical_label,
                               XmNleftOffset,       0,
                               NULL);

  Widget phantomlabR = XtVaCreateManagedWidget (
                               "",                  xmLabelWidgetClass, 
                               topWidget(),
                               XmNleftAttachment,   XmATTACH_FORM,
                               XmNleftOffset,       1,
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,      _invert_vertical_toggles->W(),
                               XmNbottomAttachment, XmATTACH_WIDGET,
                               XmNbottomWidget,    bottomSeparator(),
                               XmNbottomOffset,     25,
                               NULL);

  setTimeRange ();
  setUnits ();

  return topWidget ();
}

Boolean CubeSectionGui::notifyComplex (SLDelay * /*obj*/, int /*ident*/)
{
/*
  if (obj == _ptype) {
    checkCubeAmplitudeGui ();
  }
 */
  return True;
}

void CubeSectionGui::checkCubeAmplitudeGui ()
{
  if (_plot_type == PlotImage::PlotGS) {
    _cube_amp->variableDensitySensitivity ();
  }
  else if (_plot_type == PlotImage::PlotWONLY ||
           _plot_type == PlotImage::PlotWFILL ||
           _plot_type == NEGATIVE_FILL          ) {
    _cube_amp->wiggleTraceSensitivity ();
  }
}

void CubeSectionGui::setUnits()
{
  if (made()) {
/*  if (_cube_display->unitsMetric()) { See Trey for units help
      wprocShowMsg (_cube_scale->LabW(LNSCALE), "crosslines / cm: " );
      wprocShowMsg (_cube_scale->LabW(XLSCALE), "lines / cm: "      );
      wprocShowMsg (_cube_scale->LabW(TMSCALE), "cm / sec: "        );
    }
    else { */
      wprocShowMsg (_cube_scale->LabW(LNSCALE), "Crosslines / inch: " );
      wprocShowMsg (_cube_scale->LabW(XLSCALE), "Lines / inch: "      );
      wprocShowMsg (_cube_scale->LabW(RLSCALE), "Random Lines / inch:");
      wprocShowMsg (_cube_scale->LabW(TMSCALE), "Inches / sec: "      );
/*  } */
    wprocShowMsg (_cube_scale->LabW(CT),   "Ct: " );
    wprocShowMsg (_cube_scale->LabW(TMIN), "Starting time: " );
    wprocShowMsg (_cube_scale->LabW(TMAX), "Ending time: "   );
  }
}

void CubeSectionGui::setParams ()
{
  setTimeRange ();
  _plot_type = _ptype->WhichSelected();
  
  _cube_params = new CubeParams ();
  if(_plot_type == NEGATIVE_FILL)
    {
    _plot_type = PlotImage::PlotWFILL;
    _cube_params->setNegativeFill(True);
    }
  else
    {
    _cube_params->setNegativeFill(False);
    }
  _cube_params->setPlotType ((int)_plot_type);
  _cube_params->setLinesPerInch  (_xlscale);
  _cube_params->setXlinesPerInch (_lnscale);
  _cube_params->setRlinesPerInch (_rlscale);
  _cube_params->setIS            (_tmscale);
  _cube_params->setCT            (_ct);
  _cube_params->setTmin          (_tmin);
  _cube_params->setTmax          (_tmax);
  _cube_params->setRightToLeft   (Cube::InLine,   _inline_right_to_left);
  _cube_params->setRightToLeft   (Cube::CrossLine,_crossline_right_to_left);
  _cube_params->setRightToLeft   (Cube::TimeSlice,_timeslice_right_to_left);
  _cube_params->setRightToLeft   (Cube::RandomLine,_randomline_right_to_left);
  _cube_params->setInvertVertical(Cube::InLine,   _inline_invert_vertical);
  _cube_params->setInvertVertical(Cube::CrossLine,_crossline_invert_vertical);
  _cube_params->setInvertVertical(Cube::TimeSlice,_timeslice_invert_vertical);
  _cube_params->setInvertVertical(Cube::RandomLine,_randomline_invert_vertical);

  _cube_display->allCubesAcceptAndDelete(_cube_params);
}

Boolean CubeSectionGui::ValidInput ()
{
  Boolean stat = True;

  if (made()) {
    if (stat) stat = _cube_scale->validate ();
  }

  return stat;
}

void CubeSectionGui::DoAction() 
{ 
  setParams ();
  checkCubeAmplitudeGui ();
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
      manage();
      CubePlotError error(W(), cube);
    }
  }
}

void CubeSectionGui::setTimeRange ()
{
  Cube *cube = _cube_display->currentDisplayedCube ();

  if (cube) {
    if (cube->validCubeFile()) {
      _mint = cube->minTmin ();
      _maxt = cube->maxTmax ();
    }
    else {
      _mint = 0.0;
      _maxt = 1.0;
    }
  }
  else {
    _mint = 0.0;
    _maxt = 1.0;
  }

  if (!_time_range_now_set) {
    if (cube) {
      if (cube->validCubeFile()) _time_range_now_set = True;
    }
    _tmin = _mint;
    _tmax = _maxt;
  }

  if (cube) {
    if (!_time_range_now_set || cube->validCubeFile()) {
      if (_tmin < _mint) _tmin = _mint;
      if (_tmax > _maxt) _tmax = _maxt;
    }
  }

  if (_cube_scale) {
    _cube_scale->SetRange (TMIN, _mint, _maxt, _mint, 3);
    _cube_scale->SetRange (TMAX, _mint, _maxt, _maxt, 3);
    _cube_scale->SetValue (TMIN, _tmin);
    _cube_scale->SetValue (TMAX, _tmax);
  }
  if (_extrema_label) wprocVAShowMsg (_extrema_label,
    "Minimum time:  %5.3f;\nMaximum time:  %5.3f.", _mint, _maxt);
}

int CubeSectionGui::plotIfOk (Cube *cube)
{
  if (cube->currentLine() != Cube::NoLinePlotted) {
    return cube->plotIfNecessary ();
  }
  else {
    return (int)1;
  }
}
