#include "curves/gui_slow_shale_fitter.hh"
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
#include "sl/sl_scale_text_arrow.hh"
#include "sl/psuedo_widget.hh"
#include <Xm/Label.h>
#include <math.h>
#include <float.h>

static String defres[] = {
  "*equalsLn.labelString:    = ln (",
  "*dividedBy.labelString:       / ",
  "*parenSemi.labelString:     );  ",
  0};

GuiSlowShaleFitter::GuiSlowShaleFitter (Widget parent, char *name,
  HelpCtx hctx, const char *title) :
  GuiPowerLawFitter (parent, name, hctx, title, CV::SLOW_SHALE)
{
  setDefaultResources (parent, name, defres);
}

GuiSlowShaleFitter::GuiSlowShaleFitter (SLDelay *container, char *name,
  HelpCtx hctx, const char *title) :
  GuiPowerLawFitter (container, name, hctx, title, CV::SLOW_SHALE)
{
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
}

Widget GuiSlowShaleFitter::coordinateDefinitionLine ()
{
  Widget equaG, equalsLn, dividedBy, parenSemi, Xequals, Xminus;

  char *str = new char[strlen(CV::getCurveName(_type))+3];
  strcpy (str, CV::getCurveName(_type));
  strcat (str, ": ");

  XmString label_string = XmStringCreateSimple (str);
  equaG = XtVaCreateManagedWidget ("equaG",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
			 XmNlabelString,      label_string,
                         NULL);
  XmStringFree (label_string);
  delete [] str;

  equalsLn = XtVaCreateManagedWidget ("equalsLn",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  dividedBy = XtVaCreateManagedWidget ("dividedBy",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  parenSemi = XtVaCreateManagedWidget ("parenSemi",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  Xequals = XtVaCreateManagedWidget ("Xequals",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  Xminus = XtVaCreateManagedWidget ("Xminus",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

// attach managed widgets as desired
  XtVaSetValues (equaG,
                         XmNtopAttachment,    XmATTACH_WIDGET,
                         XmNtopWidget,        _titleW,
                         XmNtopOffset,        5,
                         XmNleftAttachment,   XmATTACH_FORM,
                         NULL);

  XtVaSetValues (_dependtxt[1],
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        equaG,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       equaG,
                         NULL);

  XtVaSetValues (equalsLn,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _dependtxt[1],
                         NULL);

  XtVaSetValues (_y0_arrow->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       equalsLn,
                         NULL);

  XtVaSetValues (dividedBy,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _y0_arrow->W(),
                         NULL);

  XtVaSetValues (_dependtxt[0],
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       dividedBy,
                         NULL);

  XtVaSetValues (parenSemi,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _dependtxt[0],
                         NULL);

  XtVaSetValues (_indeptxt[1],
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       parenSemi,
                         NULL);

  XtVaSetValues (Xequals,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _indeptxt[1],
                         NULL);

  XtVaSetValues (_indeptxt[0],
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       Xequals,
                         NULL);

  XtVaSetValues (Xminus,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _indeptxt[0],
                         NULL);

  XtVaSetValues (_x0_arrow->W(),
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        _dependtxt[1],
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       Xminus,
                         NULL);

  return _y0_arrow->W();
}
