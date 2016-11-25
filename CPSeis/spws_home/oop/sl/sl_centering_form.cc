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
// sl_centering_form.cc

#include "sl/sl_centering_form.hh"
#include "sl/sl_smart_form.hh"

SLCenteringForm::SLCenteringForm (Widget parent, char *name, HelpCtx hctx,
  Boolean make_now) :
  SLDelay (parent, name, hctx, False),
  _sl_form   (0)
{
  supportUnmadeDefaults(parent); 
  _sl_form = new SLSmartForm (this, "bb_form", getHelpCtx());
  if (make_now) 
    make (parent);
}

SLCenteringForm::SLCenteringForm (SLDelay *container, char *name,
  HelpCtx hctx, Boolean make_now) :
  SLDelay (container, name, hctx, False),
  _sl_form   (0)
{
  supportUnmadeDefaults(container->pW()); 
  _sl_form = new SLSmartForm (this, "bb_form", getHelpCtx());
  if ((container->made())&&(make_now)) make (container->topWidget());
}

Widget SLCenteringForm::make (Widget parent)
{
  if (made())
    return topWidget ();

  SLDelay::make (parent);

  Widget widget = XtVaCreateManagedWidget(_name,
                      xmBulletinBoardWidgetClass,
                      makeFrameIfNeeded(wParent()),
                      XmNshadowThickness,  (Dimension)0,
                      XmNmarginHeight,     (Dimension)0,
                      XmNmarginWidth,      (Dimension)0,
                      NULL);

  setTopWidget (widget);
  _sl_form->make (widget);
  setParentOfChildren (_sl_form->W());
  makeChildren();

  XtVaSetValues (_sl_form->W(),
                      XmNx,                (Position)0,
                      XmNy,                (Position)0,
                      NULL);

  XtAddEventHandler (get_shell_child(topWidget()), StructureNotifyMask,
    False, (XtEventHandler)mapCallback, (XtPointer)this);

  XtAddEventHandler (W(), StructureNotifyMask, False,
    (XtEventHandler)structCallback, (XtPointer)this);

  XtAddEventHandler (_sl_form->W(), StructureNotifyMask, False,
    (XtEventHandler)structCallback, (XtPointer)this);

  return topWidget ();

}

void SLCenteringForm::showEvenSpacing ()
{
  _sl_form->showEvenSpacing ();
}

void SLCenteringForm::attach (SLDelay *gui, SLDelay *left, SLDelay *right,
  SLDelay *top, SLDelay *bottom, int oleft, int oright, int otop,
  int obottom)
{
  _sl_form->attach (gui, left, right, top, bottom, oleft, oright, otop,
    obottom);
}

void SLCenteringForm::mapCallback (Widget widget, XtPointer OBJdata,
  XEvent *event)
{
  if (event->type == MapNotify) {
    SLCenteringForm *obj = (SLCenteringForm *)OBJdata;
    obj->centerForm ();
    XtRemoveEventHandler (widget, StructureNotifyMask, False,
      (XtEventHandler)mapCallback, OBJdata);
  }
}

void SLCenteringForm::structCallback (Widget /* widget */,
  XtPointer OBJdata, XEvent *event)
{
  if (event->type == ConfigureNotify) {
    SLCenteringForm *obj = (SLCenteringForm *)OBJdata;
    obj->centerForm ();
  }
}

void SLCenteringForm::centerForm ()
{
  if (_sl_form) {

    Dimension bb_width, bb_height, form_width, form_height;
    Position x_pos, y_pos;

    XtVaGetValues (W(),
                      XmNwidth,            &bb_width,
                      XmNheight,           &bb_height,
                      NULL);

    XtVaGetValues (_sl_form->W(),
                      XmNwidth,            &form_width,
                      XmNheight,           &form_height,
                      NULL);

    if (form_width < bb_width) {
      x_pos = (Position)((float)bb_width / (float)2
        - (float)form_width / (float)2 + (float).5);

      XtVaSetValues (_sl_form->W(),
                      XmNx,     x_pos,
                      NULL);
    }
    else if (form_width > bb_width) {
      XtVaSetValues (W(),
                      XmNwidth,            (int)form_width,
                      NULL);
    }
    if (form_height < bb_height) {
      y_pos = (Position)((float)bb_height / (float)2
        - (float)form_height / (float)2 + (float).5);

      XtVaSetValues (_sl_form->W(),
                      XmNy,                y_pos,
                      NULL);
    }
    else if (form_height > bb_height) {
      XtVaSetValues (W(),
                      XmNheight,           (int)form_height,
                      NULL);
    }
  }
}
