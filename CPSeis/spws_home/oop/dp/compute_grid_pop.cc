// Base class for installing a picking class for selecting grids to manipulate
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
//   and creating a popup menu to be used with a ComputeGrid object

#include <Xm/Label.h>
#include <Xm/Text.h>
#include "sl/sl_tog_box.hh"
#include "dp/compute_grid_pop.hh"
#include "dp/compute_grid.hh"
#include "dp/compute_grid_constants.hh"
#include "sp/seis_plot.hh"
#include "sl/sl_push_box.hh"
#include "sl/sl_prim.hh"
#include "dp/grid_error_handler.hh"
#include "sl/sl_scale_text_arrow.hh"

static String defres[]= {
    "*variablelab.labelString:  Variable",
    "*gridlab.labelString:      Plot",
    "*AvariablePB.labelString:  A",
    "*BvariablePB.labelString:  B",
    "*BvariablePB.background:   red",
    "*compressPB.labelString:   Compress",
    "*_Agridlab.labelString:    None",
    "*_Bgridlab.labelString:    None",
    "*_messagelab.labelString:  None",
    "*resultlab.labelString:    Result:",
    "*Ap0PB.labelString:        A",
    "*Bp0PB.labelString:        B",
    "*AmBPB.labelString:        A - B",
    "*ApBPB.labelString:        A + B",
    "*AdBPB.labelString:        A / B",
    "*AtBPB.labelString:        A * B",
    "*rescaletog.labelString:   Rescale",
    0};

static char *picking_mode = "Mode: Pick a plot";
static const char * const help_token = "COMP";
static char *comphelp = "mouse*COMP:  BTN#1: Select a plot as a variable, \
BTN#2: None, BTN#3: None";
static char *none = "None                                    ";
static char *select_variable = "Select a variable to assign a plot to.\
                    ";
static char *select_grid = "Click on a plot to assign to selected variable.";
static char *compress_ok = "Plots have been selected, compress or get \
result.";
static char *compress_no = "Plots have been selected, get result.";
static char *compressed = "Plots have been compressed, get fast result.";
static char *compress_okS = "Plots have been selected, compress or \
get scaled result.";
static char *compress_noS = "Plots have been selected, get scaled result.";
static char *compressedS = "Plots have been compressed, get fast scaled \
result.";
static char *result_there = "Indicated result is displayed.";
static char *working = "...working...";
static char *grid_gone = "Previously selected plot(s) not now available.";
static char *out_of_dateC = "Display is out of date until result \
uncompressed.";
static char *out_of_date = "Display is out of date until compress or get \
result again.";

#define ParentClass SLFPopSep

enum {SELECT_NONE, SELECT_A, SELECT_B, COMPRESS_GRIDS};

ComputeGridPop::ComputeGridPop (Widget            p,
                                char              *name,
                                ComputeGrid       *cg,
                                HelpCtx           hctx)
               : SLFPopSep (p, name, FP_DOREMOVE|FP_DOHELP, hctx, True, False),
                 _cg                     (cg),
                 _select_inputs_button   (-1),
                 _error_status           (CGP_SUCCESSFUL)
{
  static SLPush select_inputs[] = {
    { "AvariablePB", SELECT_A },
    { "BvariablePB", SELECT_B },
    { "compressPB",  COMPRESS_GRIDS },
  };

  static SLPush compute_results[] = {
    { "Ap0PB", A_ONLY },
    { "Bp0PB", B_ONLY },
    { "AmBPB", A_MINUS_B },
    { "ApBPB", A_PLUS_B },
    { "AdBPB", A_DIVIDED_BY_B },
    { "AtBPB", A_TIMES_B },
  };

  static SLTog rescale_result[] = {
    { "rescaletog", NULL, RESCALE_RESULT },
  };

  setDefaultResources (p, name, defres);

  _select_inputs_box = new SLPushBox (this, "select_inputs_box", hctx,
    select_inputs, XtNumber(select_inputs));
  _select_inputs_box->setAltPushAction ((SLPushButtonfunc)selectInputs,
    this);

  _compute_results_box = new SLPushBox (this, "compute_results_box", hctx,
    compute_results, XtNumber(compute_results));
  _compute_results_box->setAltPushAction ((SLPushButtonfunc)computeResult,
    this);


  _rescale_result = new SLTogBox (this, "rescale_result", hctx, rescale_result,
     XtNumber(rescale_result), True, False, False);
  _rescale_result->setAltChoiceAction((SLToggleButtonfunc)rescaleResult, this);

  _cg->setPercentOfB (50);
  _proportion_result = new SLScaleTextArrow (this, "proportion_result", hctx,
    _cg->percentOfBLoc(), True, _cg->percentOfB(), 0, 100, 1, 0, 3,
    XmHORIZONTAL, "Percent of B", 100, False);
  _proportion_result->setComplexNotify (this);

  DoAction();     
}

ComputeGridPop::~ComputeGridPop ()
{
}

Widget ComputeGridPop::make (Widget p)
{
  if ( made() ) return topWidget();
  SLFPopSep::make(p);

  Widget variablelab = XtVaCreateManagedWidget (
                           "variablelab", xmLabelWidgetClass, topWidget(),
                            XmNleftAttachment,   XmATTACH_FORM,
                            XmNleftOffset,       20,
                            XmNtopAttachment,    XmATTACH_FORM,
                            XmNtopOffset,        10, NULL);

  Widget gridlab = XtVaCreateManagedWidget (
                           "gridlab", xmLabelWidgetClass, topWidget(),
                            XmNleftAttachment,   XmATTACH_WIDGET,
                            XmNleftWidget,       variablelab,
                            XmNleftOffset,       50,
                            XmNtopAttachment,    XmATTACH_FORM,
                            XmNtopOffset,        10, NULL);

  XtVaSetValues (_select_inputs_box->W(),
                            XmNleftAttachment,   XmATTACH_FORM,
                            XmNleftOffset,       10,
                            XmNtopAttachment,    XmATTACH_WIDGET,
                            XmNtopWidget,        variablelab,
                            XmNtopOffset,        5, NULL);

  XtVaGetValues (_select_inputs_box->pushW(SELECT_A),
                          XmNforeground,        &_foreground_no_hilite_color,
                          XmNbackground,        &_background_no_hilite_color,
                          XmNtopShadowColor,    &_topshadow_no_hilite_color,
                          XmNbottomShadowColor, &_bottomshadow_no_hilite_color,
                          NULL);

  XtVaGetValues (_select_inputs_box->pushW(SELECT_B),
                          XmNforeground,        &_foreground_hilite_color,
                          XmNbackground,        &_background_hilite_color,
                          NULL);

  setWidgetInActive (_select_inputs_box->pushW(SELECT_B));

  _Agridlab = XtVaCreateManagedWidget (
                           "_Agridlab", xmTextWidgetClass, topWidget(),
                            XmNtopAttachment,          XmATTACH_WIDGET,
                            XmNtopWidget,              gridlab,
                            XmNtopOffset,              10,
                            XmNleftAttachment,         XmATTACH_WIDGET,
                            XmNleftWidget,             _select_inputs_box->W(),
                            XmNleftOffset,             5,
                            XmNrightAttachment,        XmATTACH_FORM,
                            XmNeditable,               False,
                            XmNcolumns,                30,
                            XmNborderWidth,            1,
                            XmNcursorPositionVisible,  False,
                            XmNshadowThickness,        0, NULL);

  _Bgridlab = XtVaCreateManagedWidget (
                           "_Bgridlab", xmTextWidgetClass, topWidget(),
                            XmNleftAttachment,         XmATTACH_WIDGET,
                            XmNleftWidget,             _select_inputs_box->W(),
                            XmNleftOffset,             5,
                            XmNrightAttachment,        XmATTACH_FORM,
                            XmNtopAttachment,          XmATTACH_WIDGET,
                            XmNtopWidget,              _Agridlab,
                            XmNtopOffset,              5,
                            XmNeditable,               False,
                            XmNcolumns,                30,
                            XmNborderWidth,            1,
                            XmNcursorPositionVisible,  False,
                            XmNshadowThickness,        0, NULL);

  _messagelab = XtVaCreateManagedWidget (
                           "_messagelab", xmTextWidgetClass, topWidget(),
                            XmNleftAttachment,         XmATTACH_FORM,
                            XmNleftOffset,             15,
                            XmNrightAttachment,        XmATTACH_FORM,
                            XmNtopAttachment,          XmATTACH_WIDGET,
                            XmNtopWidget,              _select_inputs_box->W(),
                            XmNtopOffset,              10,
                            XmNeditable,               False,
                            XmNcolumns,                55,
                            XmNborderWidth,            1,
                            XmNcursorPositionVisible,  False,
                            XmNshadowThickness,        0, NULL);

  Widget resultlab = XtVaCreateManagedWidget (
                           "resultlab", xmLabelWidgetClass, topWidget(),
                            XmNleftAttachment,   XmATTACH_FORM,
                            XmNleftOffset,       15,
                            XmNtopAttachment,    XmATTACH_WIDGET,
                            XmNtopWidget,        _messagelab,
                            XmNborderWidth,      0,
                            XmNtopOffset,        10, NULL);
                             
  XtVaSetValues (_compute_results_box->W(),
                            XmNleftAttachment,   XmATTACH_WIDGET,
                            XmNleftWidget,       resultlab,
                            XmNleftOffset,       5,
                            XmNtopAttachment,    XmATTACH_WIDGET,
                            XmNtopWidget,        _messagelab,
                            XmNtopOffset,        10, NULL);

  XtVaSetValues (_compute_results_box->topWidget(),
                            XmNorientation,      XmHORIZONTAL, 0);

  XtVaSetValues (_rescale_result->W(),
                            XmNleftAttachment,   XmATTACH_FORM,
                            XmNleftOffset,       15,
                            XmNtopAttachment,    XmATTACH_WIDGET,
                            XmNtopWidget,        _compute_results_box->W(),
                            XmNtopOffset,        10, NULL);

  XtVaSetValues (_proportion_result->W(),
                            XmNleftAttachment,   XmATTACH_FORM,
                            XmNleftOffset,       15,
                            XmNtopAttachment,    XmATTACH_WIDGET,
                            XmNtopWidget,        _rescale_result->W(),
                            XmNtopOffset,        10, NULL);

  Widget bottomphantomlab = XtVaCreateManagedWidget (
                            "", xmLabelWidgetClass, topWidget(),
                            XmNtopAttachment,    XmATTACH_WIDGET,
                            XmNtopWidget,        _proportion_result->W(),
                            XmNtopOffset,        5,
                            XmNbottomAttachment, XmATTACH_WIDGET,
                            XmNbottomWidget,     bottomSeparator(),
                            XmNbottomOffset,     5, NULL);

  return topWidget();

}

void ComputeGridPop::manage()
{
  setDisplay ();
  XtManageChild(topWidget()); 
}

int ComputeGridPop::failed ()
{
  return (int)(_error_status != CGP_SUCCESSFUL);
}

Boolean ComputeGridPop::notifyComplex (SLDelay *obj, int /* ident */)
{
  if (obj == _proportion_result) {
// SL scale arrow text modified to change percent of B
    if (_compute_results_button > -1) {
      _cg->changeProportion ();
    }
  }
  return True;
}

// display the pop up to reflect the current state of use 
void ComputeGridPop::setDisplay ()
{
  int message_selected = 0;
  if (_cg->gridValid(0))  // SELECT_A
    wprocShowMsg (_Agridlab, gridname(0));
  else
    wprocShowMsg (_Agridlab, none);

  if (_cg->gridValid(1))  // SELECT_B
    wprocShowMsg (_Bgridlab, gridname(1));
  else
    wprocShowMsg (_Bgridlab, none);

  setWidgetInActive (_select_inputs_box->pushW(SELECT_A));
  setWidgetInActive (_select_inputs_box->pushW(SELECT_B));
  if (_select_inputs_button > -1) {
    setWidgetActive (_select_inputs_box->pushW(_select_inputs_button));
    wprocShowMsg (_messagelab, select_grid);
    message_selected = 1;
  }
  else if (_cg->resultIsDisplayed() && _cg->inputsOutOfDate()) {
    wprocShowMsg (_messagelab, grid_gone);
    message_selected = 1;
  }

  if (_cg->compressionStateIsSet())
    setWidgetActive (_select_inputs_box->pushW(COMPRESS_GRIDS));
  else
    setWidgetInActive (_select_inputs_box->pushW(COMPRESS_GRIDS));

  setWidgetInActive (_compute_results_box->pushW(A_ONLY));
  setWidgetInActive (_compute_results_box->pushW(B_ONLY));
  setWidgetInActive (_compute_results_box->pushW(A_MINUS_B));
  setWidgetInActive (_compute_results_box->pushW(A_PLUS_B));
  setWidgetInActive (_compute_results_box->pushW(A_DIVIDED_BY_B));
  setWidgetInActive (_compute_results_box->pushW(A_TIMES_B));

  if (_cg->enoughGridsAreValid()) {
    if (_cg->resultIsDisplayed()) {
      if (!message_selected) {
        if (!_cg->inputsOutOfDate()) {
          wprocShowMsg (_messagelab, result_there);
        }
        else {
          if (_cg->compressionStateIsSet()) {
            wprocShowMsg (_messagelab, out_of_dateC);
          }
          else {
            wprocShowMsg (_messagelab, out_of_date);
          }
        }
        message_selected = 1;
      }
      if (_compute_results_button > -1)
        setWidgetActive (_compute_results_box->pushW(_compute_results_button));
    }
    else if (!message_selected) {
      if (_cg->rescaleResultStateIsSet()) {
        if (_cg->compressionStateIsSet()) {
          wprocShowMsg (_messagelab, compressedS);
        }
        else if (compressIsOK()) {
          wprocShowMsg (_messagelab, compress_okS);
        }
        else {
          wprocShowMsg (_messagelab, compress_noS);
        }
      }
      else {
        if (_cg->compressionStateIsSet()) {
          wprocShowMsg (_messagelab, compressed);
        }
        else if (compressIsOK()) {
          wprocShowMsg (_messagelab, compress_ok);
        }
        else {
          wprocShowMsg (_messagelab, compress_no);
        }
      }
      message_selected = 1;
    }
    XtSetSensitive (_select_inputs_box->pushW(COMPRESS_GRIDS), compressIsOK());
    XtSetSensitive (_compute_results_box->pushW(A_ONLY),         True);
    XtSetSensitive (_compute_results_box->pushW(B_ONLY),         True);
    XtSetSensitive (_compute_results_box->pushW(A_MINUS_B),      True);
    XtSetSensitive (_compute_results_box->pushW(A_PLUS_B),       True);
    XtSetSensitive (_compute_results_box->pushW(A_DIVIDED_BY_B), True);
    XtSetSensitive (_compute_results_box->pushW(A_MINUS_B),      True);
    XtSetSensitive (_compute_results_box->pushW(A_TIMES_B),      True);
    XtSetSensitive (_proportion_result->W(),                     True);
  }
  else if (_cg->compressionStateIsSet()) {
    if (_cg->resultIsDisplayed()) {
      if (!message_selected) {
        wprocShowMsg (_messagelab, out_of_dateC);
        message_selected = 1;
      }
      if (_compute_results_button > -1)
        setWidgetActive (_compute_results_box->pushW(_compute_results_button));
    }
    else if (!message_selected) {
      if (_cg->rescaleResultStateIsSet()) {
        wprocShowMsg (_messagelab, compressedS);
      }
      else {
        wprocShowMsg (_messagelab, compressed);
      }
      message_selected = 1;
    }
    XtSetSensitive (_select_inputs_box->pushW(COMPRESS_GRIDS),   False);
    XtSetSensitive (_compute_results_box->pushW(A_ONLY),         True);
    XtSetSensitive (_compute_results_box->pushW(B_ONLY),         True);
    XtSetSensitive (_compute_results_box->pushW(A_MINUS_B),      True);
    XtSetSensitive (_compute_results_box->pushW(A_PLUS_B),       True);
    XtSetSensitive (_compute_results_box->pushW(A_DIVIDED_BY_B), True);
    XtSetSensitive (_compute_results_box->pushW(A_MINUS_B),      True);
    XtSetSensitive (_compute_results_box->pushW(A_TIMES_B),      True);
    XtSetSensitive (_proportion_result->W(),                     True);
  }
  else {
    if (!message_selected) {
      wprocShowMsg (_messagelab, select_variable);
      message_selected = 1;
    }
    XtSetSensitive (_select_inputs_box->pushW(COMPRESS_GRIDS),   False);
    if (_cg->gridValid(0)) {
      XtSetSensitive (_compute_results_box->pushW(A_ONLY),       True);
      XtSetSensitive (_compute_results_box->pushW(B_ONLY),       False);
    }
    else if (_cg->gridValid(1)) {
      XtSetSensitive (_compute_results_box->pushW(A_ONLY),       False);
      XtSetSensitive (_compute_results_box->pushW(B_ONLY),       True);
    }
    else {
      XtSetSensitive (_compute_results_box->pushW(A_ONLY),       False);
      XtSetSensitive (_compute_results_box->pushW(B_ONLY),       False);
    }
    XtSetSensitive (_compute_results_box->pushW(A_MINUS_B),      False);
    XtSetSensitive (_compute_results_box->pushW(A_PLUS_B),       False);
    XtSetSensitive (_compute_results_box->pushW(A_DIVIDED_BY_B), False);
    XtSetSensitive (_compute_results_box->pushW(A_MINUS_B),      False);
    XtSetSensitive (_compute_results_box->pushW(A_TIMES_B),      False);
    XtSetSensitive (_proportion_result->W(),                     False);
  }
}

// respond to a button push on the select inputs box
void ComputeGridPop::selectInputs (void *data, long button)
{
  ComputeGridPop *obj = (ComputeGridPop *)data;
  if (button == obj->_select_inputs_button) {
    obj->_select_inputs_button = -1;
    obj->removePicking ();
  }
  else if (button != COMPRESS_GRIDS) {
    obj->removePicking ();
    obj->_select_inputs_button = button;
    obj->installPicking ();
  }
  else {
    obj->compressionButtonPushed ();
  }
  obj->setDisplay ();
}

// toggle the current state of data compression
void ComputeGridPop::compressionButtonPushed ()
{
// watch cursor needed here
    wprocShowMsg (_messagelab, working);
  if (_cg->compressionStateIsSet()) {
    if (!_cg->setCompressionState (DONT_COMPRESS_INPUTS)) {
      new GridErrorHandler (topWidget(), "Compute Grid Error",
        _cg->errorStatus());
// clear watch cursor needed here
      return;
    }
  }
  else {
    if (!_cg->setCompressionState(COMPRESS_INPUTS)) {
      new GridErrorHandler (topWidget(), "Compute Grid Error",
        _cg->errorStatus());
// clear watch cursor needed here
      return;
    }
  }
// clear watch cursor needed here
}

// respond to an option selection on the compute results box
void ComputeGridPop::computeResult (void *data, long button)
{
  ComputeGridPop *obj = (ComputeGridPop *)data;
  obj->_compute_results_button = button;
// need a watch cursor here!
  wprocShowMsg (obj->_messagelab, working);
  if (!obj->_cg->setResultState(obj->_compute_results_button)) {
    new GridErrorHandler (obj->topWidget(), "Compute Grid Error",
      obj->_cg->errorStatus());
    return;
  }
// clear watch cursor needed here
  obj->setDisplay ();
}

// respond to an action on the rescale result toggle button
void ComputeGridPop::rescaleResult (void *data, long /*which*/)
{
  ComputeGridPop *obj = (ComputeGridPop *)data;
// need a watch cursor here!
  wprocShowMsg (obj->_messagelab, working);
  if (obj->_cg->rescaleResultStateIsSet()) {
    if (!obj->_cg->setRescaleResultState(DONT_RESCALE_RESULT)) {
      new GridErrorHandler (obj->topWidget(), "Compute Grid Error",
        obj->_cg->errorStatus());
      return;
    }
  }
  else {
    if (!obj->_cg->setRescaleResultState (RESCALE_RESULT)) {
      new GridErrorHandler (obj->topWidget(), "Compute Grid Error",
        obj->_cg->errorStatus());
      return;
    }
  }
// clear watch cursor needed here
  obj->setDisplay ();
}

void ComputeGridPop::setWidgetActive (Widget w)
{
  XtVaSetValues (w,        XmNforeground,       _foreground_hilite_color,
                           XmNbackground,       _background_hilite_color,
                           NULL);
}

void ComputeGridPop::setWidgetInActive (Widget w)
{

  XtVaSetValues (w,        XmNforeground,        _foreground_no_hilite_color,
                           XmNbackground,        _background_no_hilite_color,
                           XmNtopShadowColor,    _topshadow_no_hilite_color,
                           XmNbottomShadowColor, _bottomshadow_no_hilite_color,
                           NULL);
}


//******************** ComputeGrid PickBase ********************************

ComputeGridPicker::ComputeGridPicker (SeisPlot *sp, ComputeGridPop *cgp)
                           : PickBase (sp, picking_mode, 
                                       help_token, comphelp,
                                       XC_tcross, allow, True),
                             _cgp(cgp), _sp(sp)
{
}

ComputeGridPicker::~ComputeGridPicker ()
{
}

void ComputeGridPicker::buttonAny (int /*ev_x1*/, int /*ev_x2*/,
  int /*ev_y1*/, int /*ev_y2*/, int button, Action action,
  Modifier /*modifier*/)
{
  if (!_sp->imageIsDisplayed())return;

  if (button == 1 && action == press) {
    _cgp->selectGrid (_sp);
    _cgp->removePicking ();
    _cgp->setDisplay ();
  }
  PrimSupport::updateEverything();
}
