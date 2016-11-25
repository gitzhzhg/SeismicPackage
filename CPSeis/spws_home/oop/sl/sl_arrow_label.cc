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
// sl_arrow_label.cc

#include "sl/sl_arrow_label.hh"
#include "sl/psuedo_widget.hh"
#include "wproc.h"
#include <Xm/Label.h>
#include <Xm/Form.h>
#include <Xm/ArrowB.h>
#include <stdio.h>
#include <stdlib.h>

static String  res[] = {
  ".Tarrow.arrowDirection:          ARROW_UP",
  ".Barrow.arrowDirection:          ARROW_DOWN",
  ".XmArrowButton.width:            20",
  ".XmArrowButton.height:           20",
  "*XmArrowButton.shadowThickness:  0",
  NULL };

SLArrowLabel::SLArrowLabel (Widget p, char *name, HelpCtx hctx, int *valptr,
  Boolean dowrap, int value, int low, int high, int increment,
  Boolean  make_now) : 
  SLDelay (p, name, hctx, False),
  _valptr       (valptr),
  _dowrap       (dowrap),
  _value        (value),
  _low          (low),
  _high         (high),
  _increment    (increment),
  _been_mapped  (False)
{
  setDefaultResources (p, name, res);
  if (make_now) make(p);
  supportUnmadeDefaults (p);
}

SLArrowLabel::SLArrowLabel (SLDelay *contain, char *name, HelpCtx hctx,
  int *valptr, Boolean dowrap, int value, int low, int high, int increment,
  Boolean make_if_can) :
  SLDelay (contain, name, hctx, False),
  _valptr       (valptr),
  _dowrap       (dowrap),
  _value        (value),
  _low          (low),
  _high         (high),
  _increment    (increment),
  _been_mapped  (False)
{
  setDefaultResources (contain->pW()->display(), name, res);
  supportUnmadeDefaults (contain);
  if ((contain->made())&&(make_if_can)) make(contain->topWidget());
}

Widget SLArrowLabel::make (Widget p)
{
  if (made()) return topWidget ();
  SLDelay::make(p);

  Widget w;
  w = XtVaCreateManagedWidget        (instanceName(),
                                      topClass(),
                                      makeFrameIfNeeded (p),
                                      NULL);
  setTopWidget (w);

  _Tarrow = XtVaCreateManagedWidget  ("Tarrow",
                                      xmArrowButtonWidgetClass,
                                      w, 
                                      XmNtopAttachment,    XmATTACH_FORM,
                                      XmNleftAttachment,   XmATTACH_FORM,
                                      XmNleftOffset,       (int)0,
                                      NULL);

  _Barrow = XtVaCreateManagedWidget  ("Barrow",
                                      xmArrowButtonWidgetClass,
                                      w,
                                      XmNleftAttachment,   XmATTACH_FORM,
                                      XmNleftOffset,       (int)0,
                                      XmNbottomAttachment, XmATTACH_FORM,
                                      NULL);

  _label = XtVaCreateManagedWidget   ("Value",
                                      xmLabelWidgetClass,
                                      w,
                                      XmNtopAttachment,    XmATTACH_WIDGET,
                                      XmNtopWidget,        _Tarrow,
                                      XmNbottomAttachment, XmATTACH_WIDGET,
                                      XmNbottomWidget,     _Barrow,
                                      XmNrightAttachment,  XmATTACH_FORM,
                                      XmNleftAttachment,   XmATTACH_FORM,
                                      NULL);

  if (*_valptr < _low)  *_valptr = _low;
  if (*_valptr > _high) *_valptr = _high;
  setValue (*_valptr);

  XtAddCallback (_Tarrow, XmNactivateCallback,
    (XtCallbackProc)ArrowPressCallback, (XtPointer)this );

  XtAddCallback (_Barrow, XmNactivateCallback,
    (XtCallbackProc)ArrowPressCallback, (XtPointer)this );

  XtAddCallback (get_shell_child(w), XmNmapCallback,
    (XtCallbackProc)mapCallback, (XtPointer)this);

  return topWidget();
}

void SLArrowLabel::setValue (int value)
{
  if ((value >= _low) && (value <= _high)) {
    _value = value;
    *_valptr = value;
    if (made()) wprocVAShowMsg (_label, "%d", value);
  }
  else {
    printf ("SLArrowLabel::setValue: value passed - %d, is out of range\n",
      value); 
    printf ("SLArrowlabel::setValue: current range is %d - %d\n",
      _low, _high); 
  }
}

void SLArrowLabel::setRange (int low, int high)
{
  if (low > high) {
    _low = high;
    _high = low;
  }
  if (low == high) {
    _low = low;
    _high = low + 1;
  }
  else /* if (low < high) */ {
    _low = low;
    _high = high;
  }
  if (_value < _low)  setValue (_low);
  if (_value > _high) setValue (_high);
}

void SLArrowLabel::ArrowPress (Widget widget, XtPointer /* OBJdata */,
  XmArrowButtonCallbackStruct * /* CBdata */)
{
  int test_value;
  if (widget == _Barrow) {
    test_value = _value - _increment;
    if (_dowrap) {
      if         (test_value <  _low)    _value = _high;
      else /* if (test_value >= _low) */ _value = test_value;
    }
    else /* if (!_dowrap) */ {
      if      (test_value >= _low) _value = test_value;
      else if (_value     >  _low) _value = _low;
    }
  } 
  else if  (widget == _Tarrow) {
    test_value = _value + _increment;
    if (_dowrap) {
      if         (test_value > _high)     _value = _low;
      else /* if (test_value <= _high) */ _value = test_value;
    }
    else /* if (!_dowrap) */ {
      if      (test_value <= _high) _value = test_value;
      else if (_value     <  _high) _value = _high;
    }
  } 
  callNotifyComplex(_value);
  setValue (_value);
}

SLArrowLabel::~SLArrowLabel()
{
}

void SLArrowLabel::centerGraphics ()
{
  if (_been_mapped) {

// if either of the arrows are smaller than the label, then adjust the
//   offset to center the arrows

    int offset;
    Dimension l_width, ta_width, ba_width;

    XtVaGetValues (_label,
                              XmNwidth,            &l_width,
                              NULL);

    XtVaGetValues (_Tarrow,
                              XmNwidth,            &ta_width,
                              NULL);

    XtVaGetValues (_Barrow,
                              XmNwidth,            &ba_width,
                              NULL);

    if (ta_width < l_width) {
      offset = (int)(((float)l_width / (float)2 - (float)ta_width
        / (float)2) / (float)l_width * (float)100 + (float).5);
      XtVaSetValues (_Tarrow,
                              XmNleftOffset,       offset,
                              NULL);
    }

    if (ba_width < l_width) {
      offset = (int)(((float)l_width / (float)2 - (float)ba_width
        / (float)2) / (float)l_width * (float)100 + (float).5);
      XtVaSetValues (_Barrow,
                              XmNleftOffset,       offset,
                              NULL);
    }
  }
  _been_mapped = True;
}

void SLArrowLabel::ArrowPressCallback (Widget widget, XtPointer OBJdata,
  XmArrowButtonCallbackStruct *CBdata)
{
  SLArrowLabel *object = (SLArrowLabel *)OBJdata;
  object->ArrowPress (widget, OBJdata, CBdata);
}

void SLArrowLabel::mapCallback (Widget widget, XtPointer OBJdata,
  XtPointer /* CBdata */)
{
  SLArrowLabel *obj = (SLArrowLabel *)OBJdata;
  obj->centerGraphics ();
  XtRemoveCallback (widget, XmNmapCallback, (XtCallbackProc)mapCallback,
    OBJdata);
}





SLArrowTitle::SLArrowTitle (Widget parent, char *name, HelpCtx hctx,
  int *valptr, Boolean dowrap, int value, int low, int high, int increment,
  Boolean  make_now) : 
  SLDelay (parent, name, hctx, False),
  _name         (name),
  _valptr       (valptr),
  _dowrap       (dowrap),
  _value        (value),
  _low          (low),
  _high         (high),
  _increment    (increment),
  _title        (0),
  _arrow_label  (0),
  _been_mapped  (False)
{

  supportUnmadeDefaults (parent);
  _arrow_label = new SLArrowLabel (this, _name, getHelpCtx(), _valptr,
    _dowrap, _value, _low, _high, _increment, False);
  if (make_now) make(parent);
}

SLArrowTitle::SLArrowTitle (SLDelay *contain, char *name, HelpCtx hctx,
  int *valptr, Boolean dowrap, int value, int low, int high, int increment,
  Boolean make_if_can) :
  SLDelay (contain, name, hctx, False),
  _name         (name),
  _valptr       (valptr),
  _dowrap       (dowrap),
  _value        (value),
  _low          (low),
  _high         (high),
  _increment    (increment),
  _title        (0),
  _arrow_label  (0),
  _been_mapped  (False)
{

  supportUnmadeDefaults (contain);
  _arrow_label = new SLArrowLabel (this, _name, getHelpCtx(), _valptr,
    _dowrap, _value, _low, _high, _increment, False);
  if ((contain->made())&&(make_if_can)) make(contain->topWidget());
}

Widget SLArrowTitle::make (Widget parent)
{
  if (made()) return topWidget ();
  SLDelay::make(parent);

  Widget w;
  w = XtVaCreateManagedWidget        (instanceName(),
                                      topClass(),
                                      makeFrameIfNeeded (parent),
                                      NULL);
  setTopWidget (w);

  makeChildren ();

  XtVaSetValues                      (_arrow_label->W(),
                                      XmNtopAttachment,     XmATTACH_FORM,
                                      XmNleftAttachment,    XmATTACH_FORM,
                                      XmNleftOffset,        (int)0,
                                      NULL);

  _title = XtVaCreateManagedWidget   (_name,
                                      xmLabelWidgetClass,
                                      w,
                                      XmNtopAttachment,     XmATTACH_WIDGET,
                                      XmNtopWidget,         _arrow_label->W(),
                                      XmNleftAttachment,    XmATTACH_FORM,
                                      XmNleftOffset,        (int)0,
                                      NULL);

  Widget phantomBW = XtVaCreateManagedWidget ("",
                                       xmLabelWidgetClass,
                                       w,
                                       XmNtopAttachment,    XmATTACH_WIDGET,
                                       XmNtopWidget,        _title,
                                       XmNbottomAttachment, XmATTACH_FORM,
                                       NULL);


  XtAddCallback (get_shell_child(w), XmNmapCallback,
    (XtCallbackProc)mapCallback, (XtPointer)this);

  return topWidget ();

}

SLArrowTitle::~SLArrowTitle ()
{
  unattach(topWidget());
}

void SLArrowTitle::mapCallback (Widget widget, XtPointer OBJdata,
  XtPointer /* CBdata */)
{
  SLArrowTitle *obj = (SLArrowTitle *)OBJdata;
  obj->centerGraphics ();
  XtRemoveCallback (widget, XmNmapCallback, (XtCallbackProc)mapCallback,
    OBJdata);
}

void SLArrowTitle::centerGraphics ()
{
  if (!_been_mapped) {

// if either of the arrows are smaller than the label, then adjust the
//   offset to center the arrows

    int offset;
    Dimension t_width, a_width;

    XtVaGetValues (_arrow_label->W(),
                                       XmNwidth,            &a_width,
                                       NULL);

    XtVaGetValues (_title,
                                       XmNwidth,            &t_width,
                                       NULL);

    if (a_width < t_width) {
      offset = (int)(((float)t_width / (float)2 - (float)a_width
        / (float)2) / (float)t_width * (float)100 + (float).5);
      XtVaSetValues (_arrow_label->W(),
                                       XmNleftOffset,       offset,
                                       NULL);
    }
    else if (a_width > t_width) {
      offset = (int)(((float)a_width / (float)2 - (float)a_width
        / (float)2) / (float)a_width * (float)100 + (float).5);
      XtVaSetValues (_title,
                                       XmNleftOffset,       offset,
                                       NULL);
    }

  }
  _been_mapped = True;
}





SLArrowFPLabel::SLArrowFPLabel (Widget p, char *name, HelpCtx hctx,
  float *valptr, Boolean dowrap, float value, float low, float high,
  float increment, Boolean  make_now) : 
  SLDelay (p, name, hctx, False),
  _valptr       (valptr),
  _dowrap       (dowrap),
  _value        (value),
  _low          (low),
  _high         (high),
  _increment    (increment),
  _been_mapped  (False)
{
  setDefaultResources (p, name, res);
  if (make_now) make(p);
  supportUnmadeDefaults (p);
}

SLArrowFPLabel::SLArrowFPLabel (SLDelay *contain, char *name, HelpCtx hctx,
  float *valptr, Boolean dowrap, float value, float low, float high,
  float increment,
  Boolean make_if_can) :
  SLDelay (contain, name, hctx, False),
  _valptr       (valptr),
  _dowrap       (dowrap),
  _value        (value),
  _low          (low),
  _high         (high),
  _increment    (increment),
  _been_mapped  (False)
{
  setDefaultResources (contain->pW()->display(), name, res);
  supportUnmadeDefaults (contain);
  if ((contain->made())&&(make_if_can)) make(contain->topWidget());
}

Widget SLArrowFPLabel::make (Widget p)
{
  if (made()) return topWidget ();
  SLDelay::make(p);

  Widget w;
  w = XtVaCreateManagedWidget        (instanceName(),
                                      topClass(),
                                      makeFrameIfNeeded (p),
                                      NULL);
  setTopWidget (w);

  _Tarrow = XtVaCreateManagedWidget  ("Tarrow",
                                      xmArrowButtonWidgetClass,
                                      w, 
                                      XmNtopAttachment,    XmATTACH_FORM,
                                      XmNleftAttachment,   XmATTACH_FORM,
                                      XmNleftOffset,       (int)0,
                                      NULL);

  _Barrow = XtVaCreateManagedWidget  ("Barrow",
                                      xmArrowButtonWidgetClass,
                                      w,
                                      XmNleftAttachment,   XmATTACH_FORM,
                                      XmNleftOffset,       (int)0,
                                      XmNbottomAttachment, XmATTACH_FORM,
                                      NULL);

  _label = XtVaCreateManagedWidget   ("Value",
                                      xmLabelWidgetClass,
                                      w,
                                      XmNtopAttachment,    XmATTACH_WIDGET,
                                      XmNtopWidget,        _Tarrow,
                                      XmNbottomAttachment, XmATTACH_WIDGET,
                                      XmNbottomWidget,     _Barrow,
                                      XmNrightAttachment,  XmATTACH_FORM,
                                      XmNleftAttachment,   XmATTACH_FORM,
                                      NULL);

  if (*_valptr < _low)  *_valptr = _low;
  if (*_valptr > _high) *_valptr = _high;
  setValue (*_valptr);

  XtAddCallback (_Tarrow, XmNactivateCallback,
    (XtCallbackProc)ArrowPressCallback, (XtPointer)this );

  XtAddCallback (_Barrow, XmNactivateCallback,
    (XtCallbackProc)ArrowPressCallback, (XtPointer)this );

  XtAddCallback (get_shell_child(w), XmNmapCallback,
    (XtCallbackProc)mapCallback, (XtPointer)this);

  return topWidget();
}

void SLArrowFPLabel::setValue (float value)
{
  if ((value >= _low) && (value <= _high)) {
    _value = value;
    *_valptr = value;
    if (made()) wprocVAShowMsg (_label, "%g", value);
  }
  else {
    printf ("SLArrowFPLabel::setValue: value passed - %g, is out of range\n",
      value); 
    printf ("SLArrowFPLabel::setValue: current range is %g - %g\n",
      _low, _high); 
  }
}

void SLArrowFPLabel::setRange (float low, float high)
{
  if (low > high) {
    _low = high;
    _high = low;
  }
  if (low == high) {
    _low = low;
    _high = low + 1;
  }
  else /* if (low < high) */ {
    _low = low;
    _high = high;
  }
  if (_value < _low)  setValue (_low);
  if (_value > _high) setValue (_high);
}

void SLArrowFPLabel::ArrowPress (Widget widget, XtPointer /* OBJdata */,
  XmArrowButtonCallbackStruct * /* CBdata */)
{
  float test_value;
  if (widget == _Barrow) {
    test_value = _value - _increment;
    if (_dowrap) {
      if         (test_value <  _low)    _value = _high;
      else /* if (test_value >= _low) */ _value = test_value;
    }
    else /* if (!_dowrap) */ {
      if      (test_value >= _low) _value = test_value;
      else if (_value     >  _low) _value = _low;
    }
  } 
  else if  (widget == _Tarrow) {
    test_value = _value + _increment;
    if (_dowrap) {
      if         (test_value > _high)     _value = _low;
      else /* if (test_value <= _high) */ _value = test_value;
    }
    else /* if (!_dowrap) */ {
      if      (test_value <= _high) _value = test_value;
      else if (_value     <  _high) _value = _high;
    }
  } 
  callNotifyComplex ();
  setValue (_value);
}

SLArrowFPLabel::~SLArrowFPLabel()
{
}

void SLArrowFPLabel::centerGraphics ()
{
  if (_been_mapped) {

// if either of the arrows are smaller than the label, then adjust the
//   offset to center the arrows

    int offset;
    Dimension l_width, ta_width, ba_width;

    XtVaGetValues (_label,
                              XmNwidth,            &l_width,
                              NULL);

    XtVaGetValues (_Tarrow,
                              XmNwidth,            &ta_width,
                              NULL);

    XtVaGetValues (_Barrow,
                              XmNwidth,            &ba_width,
                              NULL);

    if (ta_width < l_width) {
      offset = (int)(((float)l_width / (float)2 - (float)ta_width
        / (float)2) / (float)l_width * (float)100 + (float).5);
      XtVaSetValues (_Tarrow,
                              XmNleftOffset,       offset,
                              NULL);
    }

    if (ba_width < l_width) {
      offset = (int)(((float)l_width / (float)2 - (float)ba_width
        / (float)2) / (float)l_width * (float)100 + (float).5);
      XtVaSetValues (_Barrow,
                              XmNleftOffset,       offset,
                              NULL);
    }
  }
  _been_mapped = True;
}

void SLArrowFPLabel::ArrowPressCallback (Widget widget, XtPointer OBJdata,
  XmArrowButtonCallbackStruct *CBdata)
{
  SLArrowFPLabel *object = (SLArrowFPLabel *)OBJdata;
  object->ArrowPress (widget, OBJdata, CBdata);
}

void SLArrowFPLabel::mapCallback (Widget widget, XtPointer OBJdata,
  XtPointer /* CBdata */)
{
  SLArrowFPLabel *obj = (SLArrowFPLabel *)OBJdata;
  obj->centerGraphics ();
  XtRemoveCallback (widget, XmNmapCallback, (XtCallbackProc)mapCallback,
    OBJdata);
}





SLArrowFPTitle::SLArrowFPTitle (Widget parent, char *name, HelpCtx hctx,
  float *valptr, Boolean dowrap, float value, float low, float high,
  float increment, Boolean  make_now) : 
  SLDelay (parent, name, hctx, False),
  _name         (name),
  _valptr       (valptr),
  _dowrap       (dowrap),
  _value        (value),
  _low          (low),
  _high         (high),
  _increment    (increment),
  _title        (0),
  _arrow_label  (0),
  _been_mapped  (False)
{

  supportUnmadeDefaults (parent);
  _arrow_label = new SLArrowFPLabel (this, _name, getHelpCtx(), _valptr,
    _dowrap, _value, _low, _high, _increment, False);
  if (make_now) make(parent);
}

SLArrowFPTitle::SLArrowFPTitle (SLDelay *contain, char *name, HelpCtx hctx,
  float *valptr, Boolean dowrap, float value, float low, float high,
  float increment, Boolean make_if_can) :
  SLDelay (contain, name, hctx, False),
  _name         (name),
  _valptr       (valptr),
  _dowrap       (dowrap),
  _value        (value),
  _low          (low),
  _high         (high),
  _increment    (increment),
  _title        (0),
  _arrow_label  (0),
  _been_mapped  (False)
{

  supportUnmadeDefaults (contain);
  _arrow_label = new SLArrowFPLabel (this, _name, getHelpCtx(), _valptr,
    _dowrap, _value, _low, _high, _increment, False);
  if ((contain->made())&&(make_if_can)) make(contain->topWidget());
}

Widget SLArrowFPTitle::make (Widget parent)
{
  if (made()) return topWidget ();
  SLDelay::make(parent);

  Widget w;
  w = XtVaCreateManagedWidget        (instanceName(),
                                      topClass(),
                                      makeFrameIfNeeded (parent),
                                      NULL);
  setTopWidget (w);

  makeChildren ();

  XtVaSetValues                      (_arrow_label->W(),
                                      XmNtopAttachment,     XmATTACH_FORM,
                                      XmNleftAttachment,    XmATTACH_FORM,
                                      XmNleftOffset,        (int)0,
                                      NULL);

  _title = XtVaCreateManagedWidget   (_name,
                                      xmLabelWidgetClass,
                                      w,
                                      XmNtopAttachment,     XmATTACH_WIDGET,
                                      XmNtopWidget,         _arrow_label->W(),
                                      XmNleftAttachment,    XmATTACH_FORM,
                                      XmNleftOffset,        (int)0,
                                      NULL);

  Widget phantomBW = XtVaCreateManagedWidget ("",
                                       xmLabelWidgetClass,
                                       w,
                                       XmNtopAttachment,    XmATTACH_WIDGET,
                                       XmNtopWidget,        _title,
                                       XmNbottomAttachment, XmATTACH_FORM,
                                       NULL);


  XtAddCallback (get_shell_child(w), XmNmapCallback,
    (XtCallbackProc)mapCallback, (XtPointer)this);

  return topWidget ();

}

SLArrowFPTitle::~SLArrowFPTitle ()
{
}

void SLArrowFPTitle::mapCallback (Widget widget, XtPointer OBJdata,
  XtPointer /* CBdata */)
{
  SLArrowFPTitle *obj = (SLArrowFPTitle *)OBJdata;
  obj->centerGraphics ();
  XtRemoveCallback (widget, XmNmapCallback, (XtCallbackProc)mapCallback,
    OBJdata);
}

void SLArrowFPTitle::centerGraphics ()
{
  if (!_been_mapped) {

// if either of the arrows are smaller than the label, then adjust the
//   offset to center the arrows

    int offset;
    Dimension t_width, a_width;

    XtVaGetValues (_arrow_label->W(),
                                       XmNwidth,            &a_width,
                                       NULL);

    XtVaGetValues (_title,
                                       XmNwidth,            &t_width,
                                       NULL);

    if (a_width < t_width) {
      offset = (int)(((float)t_width / (float)2 - (float)a_width
        / (float)2) / (float)t_width * (float)100 + (float).5);
      XtVaSetValues (_arrow_label->W(),
                                       XmNleftOffset,       offset,
                                       NULL);
    }
    else if (a_width > t_width) {
      offset = (int)(((float)a_width / (float)2 - (float)a_width
        / (float)2) / (float)a_width * (float)100 + (float).5);
      XtVaSetValues (_title,
                                       XmNleftOffset,       offset,
                                       NULL);
    }

  }
  _been_mapped = True;
}
