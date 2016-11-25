#include "cube/cube_annotation_gui.hh"
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
#include "cube/cube_anno_params.hh"
#include "cube/cube.hh"
#include "cube/cube_plot_error.hh"
#include "sl/sl_text_box.hh"
#include "sl/shell_watch.hh"
#include "sl/psuedo_widget.hh"
#include <Xm/Xm.h>
#include <Xm/Label.h>

enum {
  PLOTTITLE, STARTLINE, STARTXLINE, PRIMHORILINE, FIRSTLHWD, FIRSTXHWD,
  LINEINCR, XLINEINCR, SECHORILINE, SECONDLHWD, SECONDXHWD
};

static String defres[] = {
  "*titleL.labelString:       Plot Title:  ",
  "*slineL.labelString:       Starting Line:  ",
  "*sxlineL.labelString:      Starting Crossline:  ",
  "*phlineL.labelString:      Prim Horizon Lines:  ",
  "*flhwdL.labelString:       First Line Header:  ",
  "*fxhwdL.labelString:       First Crossline Header:  ",
  "*lincrL.labelString:       Line Increment:  ",
  "*xincrL.labelString:       Crossline Increment:  ",
  "*shlineL.labelString:      Sec Horizon Lines:  ",
  "*slhwdL.labelString:       Second Line Header:  ",
  "*sxhwdL.labelString:       Second Crossline Header:  ",
  "*sline.value:              1",
  "*sxline.value:             1",
  "*phline.value:             1.0",
  "*flhwd.value:              1",
  "*fxhwd.value:              1",
  "*lincr.value:              20",
  "*xincr.value:              20",
  "*shline.value:             .2",
  "*slhwd.value:              1",
  "*sxhwd.value:              1",
  0
};

CubeAnnotationGui::CubeAnnotationGui (SLDelay *contain, char *name,
  HelpCtx hctx, CubeDisplay *cube_display) :
  SLFPopSep (contain, name, FP_DOALL, hctx, False, False),
  _titlew         (0),
  _cube_display   (cube_display),
  _dont_plot_yet  (False)
{
  setDefaultResources ((contain->pW())->display(), name, defres);
  constructorHelper ();
}

CubeAnnotationGui::CubeAnnotationGui (Widget p, char *name, HelpCtx hctx,
  CubeDisplay *cube_display) :
  SLFPopSep (p, name, FP_DOALL, hctx, False, False),
  _titlew         (0),
  _cube_display   (cube_display),
  _dont_plot_yet  (False)
{
  setDefaultResources (p, name, defres);
  constructorHelper ();
}

void CubeAnnotationGui::constructorHelper ()
{
  static SLText Input_text[]  = {
    {"sline",  NULL,                           NULL, SLType_float, STARTLINE},
    {"sxline", NULL,                           NULL, SLType_float, STARTXLINE},
    {"phline", "range:0.001 *, default:1.000", NULL, SLType_float, PRIMHORILINE},
    {"flhwd",  "range:1 9999999, default:1",   NULL, SLType_int,   FIRSTLHWD},
    {"fxhwd",  "range:0 9999999, default:1",   NULL, SLType_int,   FIRSTXHWD},
  };

  Input_text[0].target=&_start_line;
  Input_text[1].target=&_start_xline;
  Input_text[2].target=&_prim_hori_line_incr;
  Input_text[3].target=&_first_line_hwrd;
  Input_text[4].target=&_first_xline_hwrd;

  static SLText Input_text1[]  = {
    {"lincr",  NULL,                           NULL, SLType_float, LINEINCR},
    {"xincr",  NULL,                           NULL, SLType_float, XLINEINCR},
    {"shline", "range:0.001 *, default:1.000", NULL, SLType_float, SECHORILINE},
    {"slhwd",  "range:1 9999999, default:1",   NULL, SLType_int,   SECONDLHWD},
    {"sxhwd",  "range:0 9999999, default:1",   NULL, SLType_int,   SECONDXHWD},
  };

  Input_text1[0].target=&_line_incr;
  Input_text1[1].target=&_xline_incr;
  Input_text1[2].target=&_sec_hori_line_incr;
  Input_text1[3].target=&_secnd_line_hwrd;
  Input_text1[4].target=&_secnd_xline_hwrd;

  _input_text = new SLTextBox (this, "input_text", getHelpCtx(), Input_text,
    XtNumber(Input_text));

  _input_text1 = new SLTextBox (this, "input_text1", getHelpCtx(), Input_text1,
    XtNumber(Input_text1));

  setComplexNotify (this);

  _plot_title = _pw_topw->childTextDef ("title");

  setParams ();
}

CubeAnnotationGui::~CubeAnnotationGui ()
{
  if (_plot_title)  free   (_plot_title);
  if (_input_text)  delete _input_text;
  if (_input_text1) delete _input_text1;
}

Widget CubeAnnotationGui::make (Widget p)
{
  if (made()) return topWidget ();
  SLFPopSep::make (p);
  p = p ? p :  wParent ();

  Widget lab = XtVaCreateManagedWidget ("titleL",
                         xmLabelWidgetClass,  topWidget(),
                         XmNtopAttachment,    XmATTACH_FORM,
                         XmNtopOffset,        10,
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNleftOffset,       25,
                         NULL);

  _titlew = XtVaCreateManagedWidget ("title",
                         xmTextWidgetClass,   topWidget(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        lab,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       lab,
                         XmNrightAttachment,  XmATTACH_FORM,
                         XmNrightOffset,      25,
                         NULL);

  XtVaSetValues (_input_text->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        lab,
                         XmNtopOffset,        15,
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNleftOffset,       10,
                         NULL);

  Widget phantomlabB = XtVaCreateManagedWidget (
                         "",                  xmLabelWidgetClass, 
                         topWidget(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _input_text->W(),
                         XmNtopOffset,        5,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     bottomSeparator(),
                         XmNbottomOffset,     25,
                         NULL);

  XtVaSetValues (_input_text1->W(),
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        lab,
                         XmNtopOffset,        15,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _input_text->W(),
                         XmNleftOffset,       5,
                         NULL);

  Widget phantomlabR = XtVaCreateManagedWidget (
                         "",                 xmLabelWidgetClass, 
                         topWidget(),
                         XmNleftAttachment,  XmATTACH_WIDGET,
                         XmNleftWidget,      _input_text1->W(),
                         XmNleftOffset,      5,
                         XmNrightAttachment, XmATTACH_FORM,
                         XmNrightOffset,     5,
                         NULL);

  return topWidget ();
}

void CubeAnnotationGui::setParams ()
{
// apply user's inputs
  CubeAnnoParams *cube_anno_params = new CubeAnnoParams ();

  if (_titlew) {
    char *tmp = wprocGetMsg (_titlew);
    cube_anno_params->setPlotLabel (tmp);
    free (tmp);
  }
  cube_anno_params->setLineLabeling  ((long)_start_line, (long)_line_incr);
  cube_anno_params->setXLineLabeling ((long)_start_xline, (long)_xline_incr);
  cube_anno_params->setTimingLines   (_prim_hori_line_incr,
    _sec_hori_line_incr);
  cube_anno_params->setLineHeaders   (_first_line_hwrd,  _secnd_line_hwrd);
  cube_anno_params->setXLineHeaders  (_first_xline_hwrd, _secnd_xline_hwrd);

  _cube_display->allCubesAcceptAndDelete (cube_anno_params);
}

void CubeAnnotationGui::setLineHeader (int first_line_hwrd)
{
  _first_line_hwrd  = first_line_hwrd;
  _secnd_xline_hwrd = first_line_hwrd;
  _input_text ->SetValue ((int)FIRSTLHWD,  (long)_first_line_hwrd);
  _input_text1->SetValue ((int)SECONDXHWD, (long)_secnd_xline_hwrd);
}

void CubeAnnotationGui::setCrosslineHeader (int first_xline_hwrd)
{
  _first_xline_hwrd = first_xline_hwrd;
  _secnd_line_hwrd  = first_xline_hwrd;
  _input_text ->SetValue ((int)FIRSTXHWD,  (long)_first_xline_hwrd);
  _input_text1->SetValue ((int)SECONDLHWD, (long)_secnd_line_hwrd);
}

Boolean CubeAnnotationGui::notifyComplex (SLDelay *obj, int ident)
{

// in the future, write & use snap functions to "fix" the following variables
//   to valid inputs.  For now assume the cube is integer indices.  These
//   snap functions should probably be on Cube... See Trey 
  if (obj == _input_text) {
    Cube *cube = _cube_display->currentDisplayedCube ();
    switch (ident) {
      case STARTLINE: {
        _start_line = _start_line <= (float)cube->totalLines() ? _start_line :
          (float)(cube->totalLines());
        _start_line = _start_line >= (float)1 ? _start_line : (float)1;
        _start_line = (float)((int)(_start_line + (float).5));
        _input_text->SetValue (STARTLINE, _start_line);
      }
      break;
      case STARTXLINE: {
        _start_xline = _start_xline <= (float)cube->totalCrossLines() ?
          _start_xline : (float)cube->totalCrossLines();
        _start_xline = _start_xline > (float)1 ? _start_xline : (float)1;
        _start_xline = (float)((int)(_start_xline + (float).5));
        _input_text->SetValue (STARTXLINE, _start_xline);
      }
      break;
      case PRIMHORILINE: {
        _prim_hori_line_incr *= (float)1000;
        _prim_hori_line_incr = (float)((int)(_prim_hori_line_incr + (float).5));
        _prim_hori_line_incr /= (float)1000;
        _sec_hori_line_incr = _prim_hori_line_incr >= _sec_hori_line_incr ?
	  _sec_hori_line_incr : _prim_hori_line_incr;
        _input_text->SetValue  (PRIMHORILINE, _prim_hori_line_incr);
        _input_text1->SetValue (SECHORILINE,  _sec_hori_line_incr);
      }
      break;

      default:
      break;
    }
  }
  else if (obj == _input_text1) {
    switch (ident) {
      case LINEINCR: {
	_line_incr = _line_incr >= (float)1 ? (float)((int)(_line_incr +
          (float).5)) : (float)1;
        _input_text1->SetValue (LINEINCR, _line_incr);
      }
      break;
      case XLINEINCR: {
	_xline_incr = _xline_incr >= (float)1 ? (float)((int)(_xline_incr +
          (float).5)) : (float)1;
        _input_text1->SetValue (XLINEINCR, _xline_incr);
      }
      break;
      case SECHORILINE: {
        _sec_hori_line_incr *= (float)1000;
        _sec_hori_line_incr = (float)((int)(_sec_hori_line_incr + (float).5));
        _sec_hori_line_incr /= (float)1000;
        _prim_hori_line_incr = _sec_hori_line_incr <= _prim_hori_line_incr ?
	  _prim_hori_line_incr : _sec_hori_line_incr;
        _input_text1->SetValue (SECHORILINE,  _sec_hori_line_incr);
        _input_text->SetValue  (PRIMHORILINE, _prim_hori_line_incr);
      }
      break;

      default:
      break;
    }
  }
  return True;
}

Boolean CubeAnnotationGui::ValidInput ()
{
  Boolean stat = True;

  if (made()) {
    if (stat) stat = _input_text->validate ();
  }

  return stat;
}

void CubeAnnotationGui::DoAction() 
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

int CubeAnnotationGui::plotIfOk (Cube *cube)
{
  if (cube->currentLine() != Cube::NoLinePlotted) {
    return cube->plotIfNecessary ();
  }
  else {
    return (int)1;
  }
}
