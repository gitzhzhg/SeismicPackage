// class that creates the color readout gui for a color bar builder
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

#include "color/cbb_col_ro_gui.hh"
#include "color/color_bar_builder_pop.hh"
#include "color/cbb_col_set_gui.hh"
#include "sl/slp_text.hh"
#include "sl/psuedo_widget.hh"
#include "wproc.h"
#include <Xm/Label.h>
#include <Xm/Separator.h>
#include <float.h>

static String defres[] = {
  "*red_label.labelString:           Red   ",
  "*green_label.labelString:        Green  ",
  "*blue_label.labelString:         Blue   ",
  "*attribute_label.labelString:   Attribute",
  0};

CBBColROGui::CBBColROGui (Widget parent, char *name, HelpCtx hctx,
  ColorBarBuilderPop *cbb_pop) :
  SLForm (parent, name, hctx, True),
  _enabled  (False),
  _cbb_pop  (cbb_pop)
{
  setDefaultResources (parent, name, defres);
}

CBBColROGui::CBBColROGui (SLDelay *container, char *name, HelpCtx hctx,
  ColorBarBuilderPop *cbb_pop) :
  SLForm (container, name, hctx, True),
  _enabled  (False),
  _cbb_pop  (cbb_pop)
{
  if (container->made()) {
    setDefaultResources (container->topWidget(), name, defres);
    make (container->topWidget());
  }
  else {
    setDefaultResources (container->pW()->display(), name, defres);
  }
}

CBBColROGui::~CBBColROGui ()
{
}

Widget CBBColROGui::make (Widget parent)
{
  if (made()) return topWidget ();

  parent = parent ? parent : wParent ();

  SLForm::make (parent);

// create the managed widgets necessary

  Widget red_label = XtVaCreateManagedWidget ("red_label",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  Widget red_uline = XtVaCreateManagedWidget ("red_uline",
                         xmSeparatorWidgetClass, topWidget(),
                         NULL);

  SLpText *red_text = new SLpText (this, "red_text", 0, PrimSupport::_FLOAT,
    (long)5, (long)3);
  red_text->showLabelAppearance ();
  _red_widget = red_text->W();

  Widget red_bar = XtVaCreateManagedWidget ("red_bar",
                         xmSeparatorWidgetClass, topWidget(),
                         XmNorientation,      XmVERTICAL,
                         NULL);

  Widget green_label = XtVaCreateManagedWidget ("green_label",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  Widget green_uline = XtVaCreateManagedWidget ("green_uline",
                         xmSeparatorWidgetClass, topWidget(),
                         NULL);

  SLpText *green_text = new SLpText (this, "green_text", 0, PrimSupport::_FLOAT,
    (long)5, (long)3);
  green_text->showLabelAppearance ();
  _green_widget = green_text->W();

  Widget green_bar = XtVaCreateManagedWidget ("green_bar",
                         xmSeparatorWidgetClass, topWidget(),
                         XmNorientation,      XmVERTICAL,
                         NULL);

  Widget blue_label = XtVaCreateManagedWidget ("blue_label",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  Widget blue_uline = XtVaCreateManagedWidget ("blue_uline",
                         xmSeparatorWidgetClass, topWidget(),
                         NULL);

  SLpText *blue_text = new SLpText (this, "blue_text", 0, PrimSupport::_FLOAT,
    (long)5, (long)3);
  blue_text->showLabelAppearance ();
  _blue_widget = blue_text->W();

  Widget blue_bar = XtVaCreateManagedWidget ("blue_bar",
                         xmSeparatorWidgetClass, topWidget(),
                         XmNorientation,      XmVERTICAL,
                         NULL);

  Widget attribute_label = XtVaCreateManagedWidget ("attribute_label",
                         xmLabelWidgetClass,  topWidget(),
                         XmNborderWidth,      0,
                         NULL);

  Widget attribute_uline = XtVaCreateManagedWidget ("attribute_uline",
                         xmSeparatorWidgetClass, topWidget(),
                         NULL);

  SLpText *attribute_text = new SLpText (this, "attribute_text", 0,
    PrimSupport::_FLOAT, (long)10, (long)3);
  attribute_text->showLabelAppearance ();
  _attribute_widget = attribute_text->W();
                         
// clear the displayed values
  clear ();
                         
// attach the managed widgets as desired
  XtVaSetValues (red_label,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _red_widget,
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _red_widget,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     red_uline,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (red_uline,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _red_widget,
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _red_widget,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _red_widget,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (_red_widget,
                         XmNleftAttachment,   XmATTACH_FORM,
                         XmNleftOffset,       1,
                         XmNbottomAttachment, XmATTACH_FORM,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (red_bar,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        red_label,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _red_widget,
                         XmNleftOffset,       1,
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _red_widget,
                         NULL);

  XtVaSetValues (green_label,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _green_widget,
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _green_widget,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     green_uline,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (green_uline,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _green_widget,
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _green_widget,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _green_widget,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (_green_widget,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       red_bar,
                         XmNleftOffset,       1,
                         XmNbottomAttachment, XmATTACH_FORM,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (green_bar,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        green_label,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _green_widget,
                         XmNleftOffset,       1,
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _green_widget,
                         NULL);

  XtVaSetValues (blue_label,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _blue_widget,
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _blue_widget,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     blue_uline,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (blue_uline,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _blue_widget,
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _blue_widget,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _blue_widget,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (_blue_widget,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       green_bar,
                         XmNleftOffset,       1,
                         XmNbottomAttachment, XmATTACH_FORM,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (blue_bar,
                         XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                         XmNtopWidget,        blue_label,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       _blue_widget,
                         XmNleftOffset,       1,
                         XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                         XmNbottomWidget,     _blue_widget,
                         NULL);

  XtVaSetValues (attribute_label,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _attribute_widget,
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _attribute_widget,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     attribute_uline,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (attribute_uline,
                         XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                         XmNleftWidget,       _attribute_widget,
                         XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                         XmNrightWidget,      _attribute_widget,
                         XmNbottomAttachment, XmATTACH_WIDGET,
                         XmNbottomWidget,     _attribute_widget,
                         XmNbottomOffset,     1,
                         NULL);

  XtVaSetValues (_attribute_widget,
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       blue_bar,
                         XmNleftOffset,       1,
                         XmNbottomAttachment, XmATTACH_FORM,
                         XmNbottomOffset,     1,
                         NULL);


  Widget phantomR = XtVaCreateManagedWidget ("",
                         xmLabelWidgetClass,  topWidget(),
                         XmNleftAttachment,   XmATTACH_WIDGET,
                         XmNleftWidget,       attribute_uline,
                         XmNrightAttachment,  XmATTACH_FORM,
                         XmNrightOffset,      1,
                         NULL);

  return topWidget ();
}

void CBBColROGui::display (float red, float green, float blue,
  float attribute)
{
  if (red   < (float)FLT_EPSILON) red   = (float)0;
  if (green < (float)FLT_EPSILON) green = (float)0;
  if (blue  < (float)FLT_EPSILON) blue  = (float)0;
  if (_red_widget)       wprocVAShowMsg (_red_widget,       "%4.3g", red);
  if (_green_widget)     wprocVAShowMsg (_green_widget,     "%4.3g", green);
  if (_blue_widget)      wprocVAShowMsg (_blue_widget,      "%4.3g", blue);
  if (_attribute_widget) wprocVAShowMsg (_attribute_widget, "%10.4g",
    attribute);
}

void CBBColROGui::clear ()
{
  if (_red_widget)       wprocVAShowMsg (_red_widget,       " ");
  if (_green_widget)     wprocVAShowMsg (_green_widget,     " ");
  if (_blue_widget)      wprocVAShowMsg (_blue_widget,      " ");
  if (_attribute_widget) wprocVAShowMsg (_attribute_widget, " ");
}

void CBBColROGui::setShowRGB (Boolean enable)
{
  if (!enable && _enabled) {
    XtRemoveEventHandler (_cbb_pop->_col_set_gui->drawingArea(),
      PointerMotionMask|EnterWindowMask|LeaveWindowMask,
      False, (XtEventHandler)showRGB, this);
    _enabled = False;
  }
  else if (enable && !_enabled) {
    XtAddEventHandler (_cbb_pop->_col_set_gui->drawingArea(),
      PointerMotionMask|EnterWindowMask|LeaveWindowMask,
      False, (XtEventHandler)showRGB, this);
    _enabled = True;
  }
}

void CBBColROGui::showRGB (Widget /* widget */, CBBColROGui *col_ro,
  XEvent *event)
{
  if (!col_ro->_enabled) return;

  XMotionEvent *motion_event;

  if (event->type == MotionNotify) {
  // ignore this event and skip to the next if there are more of this type
    Widget graphic = col_ro->_cbb_pop->_col_set_gui->drawingArea ();
    if (wpMoreEvents (XtDisplay(graphic), XtWindow(graphic), MotionNotify))
      return;
  }
  
  if (event->type == EnterNotify) {
// prepare to do the read out
  }
  else if (event->type == LeaveNotify) {
// clean up after the read out
    col_ro->clear ();
  }
  else if (event->type == MotionNotify) {
// get the RGB-attribute information
    motion_event = (XMotionEvent *)event;
    float red, green, blue, attribute;
    col_ro->_cbb_pop->_col_set_gui->RGB (motion_event->x,
      motion_event->y, &red, &green, &blue, &attribute);
    col_ro->display (red, green, blue, attribute);
  }
}
