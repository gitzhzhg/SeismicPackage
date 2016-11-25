#include "cube/cube_table_guis.hh"
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
#include "cube/cube_tables.hh"
#include "cube/cube.hh"
#include "sl/radio_list.hh"
#include "sl/slp_radio.hh"
#include <Xm/Xm.h>
#include <Xm/Label.h>

enum {
  LINE=1000, CROSSLINE, HORIZON_SLICE, LINE_RB, CROSSLINE_RB, HORIZON_SLICE_RB
};

static String defres[] = {
  "*line.labelString:                Line",
  "*crossline.labelString:           Crossline",
  "*horizon_slice.labelString:       Horizon Slice",
  "*line_rb.labelString:             Line",
  "*crossline_rb.labelString:        Crossline",
  "*horizon_slice_rb.labelString:    Horizon Slice",
  "*line_rb.set:                     True",
  0
};

CubeTableShowGui::CubeTableShowGui (Widget p, char *name, HelpCtx hctx) :
  SLFPopSep (p, name, FP_DOREMOVE | FP_DOHELP, hctx, False, False)
{
  setDefaultResources (p, name, defres);
  _table = new CubeTableShow (p, name);
}

CubeTableShowGui::CubeTableShowGui (SLDelay *slp, char *name, HelpCtx hctx) :
  SLFPopSep (slp, name, FP_DOREMOVE | FP_DOHELP, hctx, False, False)
{
  setDefaultResources (slp->W(), name, defres);
  _table = new CubeTableShow (slp, name);
}

CubeTableShowGui::~CubeTableShowGui ()
{
  delete _table;
}

Widget CubeTableShowGui::make (Widget p)
{
  if (made()) return topWidget ();
  SLFPopSep::make (p);
  p = wParent ();

  _table->make (topWidget());

  XtVaSetValues (_table->W(),
                         XmNtopAttachment,    XmATTACH_FORM,
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNrightAttachment,  XmATTACH_FORM,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     bottomSeparator(),
                         XmNbottomOffset,     5,
                         NULL);

  return topWidget ();
}






CubeTableSelectGui::CubeTableSelectGui (Widget p, char *name, HelpCtx hctx)
  : SLFPopSep (p, name, FP_DOALL, hctx, False, False),
  _section_state  (LINE_RB)
{
  setDefaultResources (p, name, defres);
  _table = new CubeTableSelect (p, name, this);
  constructorHelper ();
}

CubeTableSelectGui::CubeTableSelectGui (SLDelay *slp, char *name, HelpCtx hctx)
  : SLFPopSep (slp, name, FP_DOALL, hctx, False, False),
  _section_state  (LINE_RB)
{
  setDefaultResources (slp->W(), name, defres);
  _table = new CubeTableSelect (slp, name, this);
  constructorHelper ();
}

CubeTableSelectGui::~CubeTableSelectGui ()
{
  delete _table;
  delete _section_list;
  delete _line_rb;
  delete _crossline_rb;
  delete _horizon_slice_rb;
}

Widget CubeTableSelectGui::make (Widget p)
{
  if (made()) return topWidget ();
  SLFPopSep::make (p);
  p = wParent ();

  _table->make (topWidget());

  _line = XtVaCreateManagedWidget (
                         "line",              xmLabelWidgetClass,
                         topWidget(),
                         XmNborderWidth,      1,
                         NULL);

  _crossline = XtVaCreateManagedWidget (
                         "crossline",         xmLabelWidgetClass,
                         topWidget(),
                         XmNborderWidth,      1,
                         NULL);

  _horizon_slice = XtVaCreateManagedWidget (
                         "horizon_slice",     xmLabelWidgetClass,
                         topWidget(),
                         XmNborderWidth,      1,
                         NULL);

  XtVaSetValues (_table->W(),
                         XmNtopAttachment,    XmATTACH_FORM,
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNrightAttachment,  XmATTACH_FORM,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _line,
                         XmNbottomOffset,     10,
                         NULL);

  XtVaSetValues (_line,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _line_rb->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _line_rb->W(),
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _line_rb->W(),
                         XmNbottomOffset,     5,
                         NULL);

  XtVaSetValues (_crossline,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _line,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _crossline_rb->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _crossline_rb->W(),
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _line,
                         NULL);

  XtVaSetValues (_horizon_slice,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _line,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _horizon_slice_rb->W(),
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _horizon_slice_rb->W(),
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _line,
                         NULL);

  XtVaSetValues (_line_rb->W(),
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNleftOffset,       20,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     bottomSeparator(),
                         XmNbottomOffset,     5,
                         NULL);

  XtVaSetValues (_crossline_rb->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _line_rb->W(),
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _line_rb->W(),
                         XmNleftOffset,       10,
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _line_rb->W(),
                         NULL);

  XtVaSetValues (_horizon_slice_rb->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _line_rb->W(),
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _crossline_rb->W(),
                         XmNleftOffset,       10,
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _line_rb->W(),
                         NULL);

  displaySectionNumbers ();

  return topWidget ();
}

void CubeTableSelectGui::displaySectionNumbers (Cube *cube)
{
  if (cube) {
    if (_line) wprocVAShowMsg (_line, "%f", (float)cube->inlineSlice());
    if (_crossline) wprocVAShowMsg (_crossline, "%f",
      (float)cube->crosslineSlice());
    if (_horizon_slice) wprocVAShowMsg (_horizon_slice, "%f",
      cube->timeSlice());
  }
  else /* if (!cube) */ {
    if (_line) wprocVAShowMsg (_line, "%s", "      ");
    if (_crossline) wprocVAShowMsg (_crossline, "%s", "         ");
    if (_horizon_slice) wprocVAShowMsg (_horizon_slice, "%s",
      "               ");
  }
}

void CubeTableSelectGui::DoAction ()
{
// In the future, this call will call another object passing along which
//   section on which cube the user has selected... to be used in for example
//   difference plots, dip filtering,...  That object will likely come in
//   through the constructor
}

void CubeTableSelectGui::constructorHelper ()
{
  _section_list = new RadioList ();

  _line_rb = 
   new SLpRadio (this, "line_rb",          LINE_RB,          0, _section_list);
  _crossline_rb =
   new SLpRadio (this, "crossline_rb",     CROSSLINE_RB,     0, _section_list);
  _horizon_slice_rb =
   new SLpRadio (this, "horizon_slice_rb", HORIZON_SLICE_RB, 0, _section_list);

  _section_list->setupIvarPoint (&_section_state);
}
