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
// sl_scale_text_arrow.cc

#include "sl/sl_scale_text_arrow.hh"
#include "sl/slp_text.hh"
#include "sl/psuedo_widget.hh"
#include <Xm/Form.h>
#include <Xm/ArrowB.h>
#include <Xm/Label.h>
#include <stdio.h>
#include <stdlib.h>
#include <float.h>

SLScaleTextArrow::SLScaleTextArrow (Widget parent, char *name, HelpCtx hctx,
  float *valptr, Boolean dowrap, float value, float low, float high,
  float increment, int precision, int columns, unsigned char orientation,
  char *label, Dimension height, Boolean  make_now) : 
  SLForm (parent, name, hctx, False),
  _valptr       (valptr),
  _dowrap       (dowrap),
  _value        (value),
  _low          (low),
  _high         (high),
  _increment    (increment),
  _precision    (precision),
  _columns      (columns),
  _orientation  (orientation),
  _label        (label),
  _height       (height),
  _been_mapped  (False),
  _old_value    (-1)
{
  if (make_now) make (parent);
  init ();
}

SLScaleTextArrow::SLScaleTextArrow (SLDelay *contain, char *name,
  HelpCtx hctx, float *valptr, Boolean dowrap, float value, float low,
  float high, float increment, int precision, int columns,
  unsigned char orientation, char *label, Dimension height, Boolean  make_now) : 
  SLForm (contain, name, hctx, False),
  _valptr       (valptr),
  _dowrap       (dowrap),
  _value        (value),
  _low          (low),
  _high         (high),
  _increment    (increment),
  _precision    (precision),
  _columns      (columns),
  _orientation  (orientation),
  _label        (label),
  _height       (height),
  _been_mapped  (False)
{
  if ((contain->made())&&(make_now)) make(contain->topWidget());
  init ();
}

SLScaleTextArrow::~SLScaleTextArrow ()
{
}

void SLScaleTextArrow::init ()
{
  _slsta_scale = 0;
  _slsta_text  = 0;

  _slsta_scale = new SLSTAScale (this, "slstascale", getHelpCtx(), this);
  _slsta_text  = new SLSTAText  (this, "slstatext",  getHelpCtx(), this);

}

Widget SLScaleTextArrow::make (Widget parent)
{
  if (made()) return topWidget ();

  SLForm::make (parent);

  if (_orientation == XmHORIZONTAL) {
    XtVaSetValues (_slsta_scale->W(),
                               XmNleftAttachment,   XmATTACH_FORM,
                               XmNbottomAttachment, XmATTACH_FORM,
                               NULL);
 
    XtVaSetValues (_slsta_text->W(),
                               XmNleftAttachment,   XmATTACH_FORM,
                               XmNbottomAttachment, XmATTACH_WIDGET,
                               XmNbottomWidget,     _slsta_scale->W(),
                               NULL);
  }
  else {
    XtVaSetValues (_slsta_scale->W(),
                               XmNtopAttachment,    XmATTACH_FORM,
                               XmNrightAttachment,  XmATTACH_FORM,
                               NULL);
 
    XtVaSetValues (_slsta_text->W(),
                               XmNtopAttachment,    XmATTACH_FORM,
                               XmNrightAttachment,  XmATTACH_WIDGET,
                               XmNrightWidget,      _slsta_scale->W(),
                               NULL);
  }

  if (*_valptr < _low)  *_valptr = _low;
  if (*_valptr > _high) *_valptr = _high;
  setValue (*_valptr);

  XtAddCallback (_slsta_scale->_Tarrow, XmNactivateCallback,
    (XtCallbackProc)ArrowPressCallback, (XtPointer)this );

  XtAddCallback (_slsta_scale->_Barrow, XmNactivateCallback,
    (XtCallbackProc)ArrowPressCallback, (XtPointer)this );

  XtAddCallback (_slsta_scale->_scale, XmNvalueChangedCallback,
    (XtCallbackProc)ValueChangedCallback, (XtPointer)this );

  XtAddCallback (_slsta_scale->_scale, XmNdragCallback,
    (XtCallbackProc)DragCallback, (XtPointer)this );


  XtAddEventHandler (get_shell_child(topWidget()), StructureNotifyMask,
    False, (XtEventHandler)mapCallback, (XtPointer)this);

  return topWidget();
}

void SLScaleTextArrow::initValue (float value)
{
  _value = value;
}

void SLScaleTextArrow::setValue (float value)
{
  if ((value >= _low) && (value <= _high)) {
    _value = value;
  }
  else if (value < _low) {
    _value = _low;
  }
  else /* if (value > _high) */ {
    _value = _high;
  }
  *_valptr = _value;
  setValueAsIs ();
}

void SLScaleTextArrow::setRange (float low, float high)
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
  setValue (_value);
}

Widget SLScaleTextArrow::topW ()
{
  if (_slsta_scale->_Tarrow)
    return _slsta_scale->_Tarrow;
  else
    return (Widget)0;
}

Widget SLScaleTextArrow::bottomW ()
{
  if (_slsta_scale->_Barrow)
    return _slsta_scale->_Barrow;
  else
    return (Widget)0;
}

Widget SLScaleTextArrow::textW ()
{
  if (_slsta_text->_text)
    return _slsta_text->_text->W();
  else
    return (Widget)0;
}

Widget SLScaleTextArrow::scaleW ()
{
  if (_slsta_scale->_scale)
    return _slsta_scale->_scale;
  else
    return (Widget)0;
}

void SLScaleTextArrow::valueTrap (void *object, long /* ident */,
  float oldvar, float newvar)
{
  SLScaleTextArrow *obj = (SLScaleTextArrow *)object;
  if (obj->_slsta_text) {
    obj->setValue (newvar);
    if (!obj->callNotifyComplex(VALUE_INPUT)) {
      obj->setValue (oldvar);
    }
  }
}

void SLScaleTextArrow::ArrowPress (Widget widget, XtPointer /* OBJdata */,
  XmArrowButtonCallbackStruct * /* CBdata */)
{
  float test_value, old_value;
  old_value = _value;
  if (_orientation == XmHORIZONTAL) {
    if (widget == _slsta_scale->_Barrow && _slsta_scale->_Barrow) {
// the right arrow button was pressed
      test_value = _value + _increment;
      dontLetValueGrowTooBig (test_value);
    } 
    else if (widget == _slsta_scale->_Tarrow && _slsta_scale->_Tarrow) {
// the left arrow button was pressed
      test_value = _value - _increment;
      dontLetValueShrinkTooSmall (test_value);
    }
  }
  else {
    if (widget == _slsta_scale->_Barrow && _slsta_scale->_Barrow) {
// the bottom arrow button was pressed
      test_value = _value - _increment;
      dontLetValueShrinkTooSmall (test_value);
    } 
    else if  (widget == _slsta_scale->_Tarrow && _slsta_scale->_Tarrow) {
// the top arrow button was pressed
      test_value = _value + _increment;
      dontLetValueGrowTooBig (test_value);
    }
  }
  setValue (_value);
  if (!callNotifyComplex(VALUE_CHANGED)) {
    setValue (old_value);
  }
}

void SLScaleTextArrow::ValueChanged (Widget widget, XtPointer /* OBJdata */,
  XmScaleCallbackStruct *CBdata)
{
  if (widget == _slsta_scale->_scale && _slsta_scale->_scale) {
// the following test on the callback structure event was to accomodate a
//   sudden
//   change in INTELSOL that resulted in callbacks occuring when our code
//   calls XtVaSetValue to set the scalebar value.  when these callbacks
//   are made, the callback structure event is nil, however, when these
//   callbacks occur as a result of gui actions the structure event is
//   non-nil.
    if (CBdata->event) {
// the slider was incremented
      float old_value = _value;
      setValueByScale ();
      if (!callNotifyComplex(VALUE_CHANGED)) {
        setValue (old_value);
      }
    }
  }
}

void SLScaleTextArrow::Drag (Widget widget, XtPointer /* OBJdata */,
  XmScaleCallbackStruct * /* CBdata */)
{
  if (widget == _slsta_scale->_scale && _slsta_scale->_scale) {
// the slider was drug
    float old_value = _value;
    setValueByScaleDrag ();
    if (!callNotifyComplex(SLIDER_DRUG)) {
      setValue (old_value);
    }
  }
}

void SLScaleTextArrow::setValueAsIs ()
{
  if (made()) {
    if (_slsta_text->_text) _slsta_text->_text->setFvar (_value);

    int minimum, maximum, value;

    if (_slsta_scale->_scale) {
      XtVaGetValues (_slsta_scale->_scale,
                                      XmNminimum,          &minimum,
                                      XmNmaximum,          &maximum,
                                      NULL);

      if (_dowrap) {
        value = (int)((_value - _low) / (_high - _low) 
          * (float)(maximum - minimum - 2) + minimum + 1);
        if (value > maximum - 1) value = maximum - 1;
        if (value < minimum + 1) value = minimum + 1;
      }
      else {
        value = (int)((_value - _low) / (_high - _low) 
          * (float)(maximum - minimum) + minimum);
        if (value > maximum) value = maximum;
        if (value < minimum) value = minimum;
      }

      XtVaSetValues (_slsta_scale->_scale,
                                      XmNvalue,            value,
                                      NULL);
    }
  }
}

void SLScaleTextArrow::setValueByScale ()
{
  if (made()) {
    int minimum, maximum, value;

    if (_slsta_scale->_scale) {
/*
      Dimension sw, w, sh, h;
      unsigned char ori, rp;
      XtVaGetValues (_slsta_scale->_scale,
		                      XmNorientation,      &ori,
		                      XmNscaleWidth,       &sw,
		                      XmNwidth,            &w,
		                      XmNscaleHeight,      &sh,
		                      XmNheight,           &h,
		                      XmNresizePolicy,      &rp,
		                      NULL);
      switch (rp) {
      case XmRESIZE_NONE :
	printf ("scale resize policy is NONE\n");
	break;
      case XmRESIZE_ANY :
	printf ("scale resize policy is ANY\n");
	break;
      case XmRESIZE_GROW :
	printf ("scale resize policy is GROW\n");
	break;
      default:
	printf ("scale resize policy not recognized(%d)\n", rp);
	break;
      }

      if (ori == XmHORIZONTAL) {
	printf ("scale width = %d\n width = %d\n\n", sw, w);
      }
      else {
	printf ("scale height = %d\n height = %d\n\n", sh, h);
      }
*/

      XtVaGetValues (_slsta_scale->_scale,
                                      XmNminimum,          &minimum,
                                      XmNmaximum,          &maximum,
                                      XmNvalue,            &value,
                                      NULL);
      if (value == _old_value) value++;

      Boolean changed_value = False;
      if (_dowrap) {
        if (value == minimum) {
          value = maximum - 1;
          changed_value = True;
        }
        if (value == maximum) {
          value = minimum + 1;
          changed_value = True;
        }

        _value = (float)(value - minimum - 1) / (float)(maximum - minimum - 2)
          * (_high - _low) + _low;
	_old_value = value;
      }
      else {
        _value = (float)(value - minimum) / (float)(maximum - minimum)
          * (_high - _low) + _low;
	_old_value = value;
      }

      if (_value > _high) _value = _high;
      if (_value < _low)  _value = _low;
      *_valptr = _value;

      if (_slsta_text->_text) _slsta_text->_text->setFvar (_value);
      if (changed_value) {
        XtVaSetValues (_slsta_scale->_scale,
                                      XmNvalue,            value,
                                      NULL);
      }
    }
  }
}

void SLScaleTextArrow::setValueByScaleDrag ()
{
  if (made()) {
    int minimum, maximum, value;

    if (_slsta_scale->_scale) {
      XtVaGetValues (_slsta_scale->_scale,
                                      XmNminimum,          &minimum,
                                      XmNmaximum,          &maximum,
                                      XmNvalue,            &value,
                                      NULL);
      Boolean changed_value = False;
      if (_dowrap) {
        if (value == minimum) {
          value = minimum + 1;
          changed_value = True;
        }
        if (value == maximum) {
          value = maximum - 1;
          changed_value = True;
        }

        _value = (float)(value - minimum - 1) / (float)(maximum - minimum - 2)
          * (_high - _low) + _low;
	_old_value = value;
      }
      else {
        _value = (float)(value - minimum) / (float)(maximum - minimum)
          * (_high - _low) + _low;
	_old_value = value;
      }

      if (_value > _high) _value = _high;
      if (_value < _low)  _value = _low;
      *_valptr = _value;

      if (_slsta_text->_text) _slsta_text->_text->setFvar (_value);
      if (changed_value) {
        XtVaSetValues (_slsta_scale->_scale,
                                      XmNvalue,            value,
                                      NULL);
      }
    }
  }
}

void SLScaleTextArrow::dontLetValueGrowTooBig (float test_value)
{
  if (_dowrap) {
    if (test_value > _high) {
      if         (test_value - _high <= FLT_EPSILON)    _value = _high;
      else /* if (test_value - _high >  FLT_EPSILON) */ _value = _low;
    }
    else /* if (test_value <= _high) */ _value = test_value;
  }
  else /* if (!_dowrap) */ {
    if         (test_value <= _high)    _value = test_value;
    else /* if (test_value >  _high) */ _value = _high;
  }
}

void SLScaleTextArrow::dontLetValueShrinkTooSmall (float test_value)
{
  if (_dowrap) {
    if (test_value <  _low) {
      if         (_low - test_value <= FLT_EPSILON)    _value = _low;
      else /* if (_low - test_value >  FLT_EPSILON) */ _value = _high;
    }
    else /* if (test_value >= _low) */ _value = test_value;
  }
  else /* if (!_dowrap) */ {
    if         (test_value >= _low)    _value = test_value;
    else /* if (test_value <  _low) */ _value = _low;
  }
}

void SLScaleTextArrow::ArrowPressCallback (Widget widget, XtPointer OBJdata,
  XmArrowButtonCallbackStruct *CBdata)
{
  SLScaleTextArrow *object = (SLScaleTextArrow *)OBJdata;
  object->ArrowPress (widget, OBJdata, CBdata);
}

void SLScaleTextArrow::ValueChangedCallback (Widget widget, XtPointer OBJdata,
  XmScaleCallbackStruct *CBdata)
{
  SLScaleTextArrow *object = (SLScaleTextArrow *)OBJdata;
  object->ValueChanged (widget, OBJdata, CBdata);
}

void SLScaleTextArrow::DragCallback (Widget widget, XtPointer OBJdata,
  XmScaleCallbackStruct *CBdata)
{
  SLScaleTextArrow *object = (SLScaleTextArrow *)OBJdata;
  object->Drag (widget, OBJdata, CBdata);
}

void SLScaleTextArrow::mapCallback (Widget widget, XtPointer OBJdata,
  XEvent * /*event */)
{
  SLScaleTextArrow *obj = (SLScaleTextArrow *)OBJdata;
  obj->centerText ();
  XtRemoveEventHandler (widget, StructureNotifyMask, False,
    (XtEventHandler)mapCallback, OBJdata);
}

void SLScaleTextArrow::centerText ()
{
  if (!_been_mapped && _slsta_text && _slsta_scale) {

    int offset;
    Dimension form_dimen, scale_dimen, text_dimen;

    if (_orientation == XmHORIZONTAL) {
      XtVaGetValues (topWidget(),
                              XmNwidth,            &form_dimen,
                              NULL);

      XtVaGetValues (_slsta_scale->W(),
                              XmNwidth,            &scale_dimen,
                              NULL);

      XtVaGetValues (_slsta_text->W(),
                              XmNwidth,            &text_dimen,
                              NULL);
      if (text_dimen < scale_dimen) {

        offset = (int)(((float)form_dimen / (float)2 - (float)text_dimen
          / (float)2) + (float).5);

        XtVaSetValues (_slsta_text->W(),
                              XmNleftOffset,       offset,
                              NULL);
      }
      else if (text_dimen > scale_dimen) {

        offset = (int)(((float)form_dimen / (float)2 - (float)scale_dimen
          / (float)2) + (float).5);

        XtVaSetValues (_slsta_scale->W(),
                              XmNleftOffset,       offset,
                              NULL);
      }
    }
    else {
      XtVaGetValues (topWidget(),
                              XmNheight,           &form_dimen,
                              NULL);

      XtVaGetValues (_slsta_scale->W(),
                              XmNheight,           &scale_dimen,
                              NULL);

      XtVaGetValues (_slsta_text->W(),
                              XmNheight,           &text_dimen,
                              NULL);
      if (text_dimen < scale_dimen) {

        offset = (int)(((float)form_dimen / (float)2 - (float)text_dimen
          / (float)2) + (float).5);

        XtVaSetValues (_slsta_text->W(),
                              XmNtopOffset,        offset,
                              NULL);
      }
      else if (text_dimen > scale_dimen) {

        offset = (int)(((float)form_dimen / (float)2 - (float)scale_dimen
          / (float)2) + (float).5);

        XtVaSetValues (_slsta_scale->W(),
                              XmNtopOffset,        offset,
                              NULL);
      }
    }
  }
  _been_mapped = True;
}

SLSTAText::SLSTAText (Widget parent, char *name, HelpCtx hctx,
  SLScaleTextArrow *slsta, Boolean  make_now) : 
  SLForm (parent, name, hctx, False),
  _slsta   (slsta)
{
  if (make_now) make (parent);
  init ();
}

SLSTAText::SLSTAText (SLDelay *contain, char *name, HelpCtx hctx,
  SLScaleTextArrow *slsta, Boolean  make_now) : 
  SLForm (contain, name, hctx, False),
  _slsta   (slsta)
{
  if ((contain->made())&&(make_now)) make (contain->topWidget());
  init ();
}

void SLSTAText::init ()
{
  _text = 0 ;

  _text = new SLpText (this, "text", 0, PrimSupport::_FLOAT,
    (long)_slsta->_columns, (long)_slsta->_precision);

  _text->showNormalAppearance ();

  _text->setFtrap (SLScaleTextArrow::valueTrap, _slsta);

}

Widget SLSTAText::make (Widget parent)
{
  if (made()) return topWidget ();

  SLForm::make (parent);

  XtVaSetValues (_text->W(),
                               XmNtopAttachment,    XmATTACH_FORM,
                               XmNrightAttachment,  XmATTACH_FORM,
                               NULL);

  if (_slsta->_label) {

    Widget label = XtVaCreateManagedWidget (_slsta->_label,
                               xmLabelWidgetClass,
                               topWidget(),
                               XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,        _text->W(),
                               XmNrightAttachment,  XmATTACH_WIDGET,
                               XmNrightWidget,      _text->W(),
                               XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                               XmNbottomWidget,     _text->W(),
                               NULL);
  }
  return topWidget();
}

static String  res[] = {
  ".XmArrowButton.width:            20",
  ".XmArrowButton.height:           20",
  "*XmArrowButton.shadowThickness:  0",
  "*Vscale.decimalPoints:           0",
  "*Vscale.width:                   20",
  "*Vscale.showValue:               False",
  "*Vscale.minimum:                 0",
  "*Vscale.maximum:                 100",
  NULL };

SLSTAScale::SLSTAScale (Widget parent, char *name, HelpCtx hctx,
  SLScaleTextArrow *slsta, Boolean  make_now) : 
  SLForm (parent, name, hctx, False),
  _slsta   (slsta),
  _Tarrow  (0),
  _Barrow  (0),
  _scale   (0)
{
  setDefaultResources (parent, name, res);
  if (make_now) make (parent);
  supportUnmadeDefaults (parent);
}

SLSTAScale::SLSTAScale (SLDelay *contain, char *name, HelpCtx hctx,
  SLScaleTextArrow *slsta, Boolean  make_now) : 
  SLForm (contain, name, hctx, False),
  _slsta   (slsta),
  _Tarrow  (0),
  _Barrow  (0),
  _scale   (0)
{
  setDefaultResources (contain->pW()->display(), name, res);
  supportUnmadeDefaults (contain);
  if ((contain->made())&&(make_now)) make (contain->topWidget());
}

Widget SLSTAScale::make (Widget parent)
{
  if (made()) return topWidget ();

  SLForm::make (parent);

  _Tarrow = XtVaCreateManagedWidget ("Tarrow",
                               xmArrowButtonWidgetClass,
                               topWidget(),
                               NULL);

  _Barrow = XtVaCreateManagedWidget ("Barrow",
                               xmArrowButtonWidgetClass,
                               topWidget(),
                               NULL);

  _scale = XtVaCreateManagedWidget ("Vscale",
                               xmScaleWidgetClass,
                               topWidget(),
			       
                               NULL);

  if (_slsta->_orientation == XmHORIZONTAL) {
    // for a horizontal scale
    XtVaSetValues (_Tarrow,
                               XmNtopAttachment,    XmATTACH_FORM,
                               XmNleftAttachment,   XmATTACH_FORM,
                               XmNarrowDirection,   XmARROW_LEFT,
                               NULL);

    XtVaSetValues (_Barrow,
                               XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,        _Tarrow,
                               XmNleftAttachment,   XmATTACH_WIDGET,
                               XmNleftWidget,       _scale,
                               XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                               XmNbottomWidget,     _Tarrow,
                               XmNarrowDirection,   XmARROW_RIGHT,
                               NULL);

    Dimension w = (Dimension)(0.75*_slsta->_height);
    XtVaSetValues (_scale,                              
                               XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,        _Tarrow,
                               XmNleftAttachment,   XmATTACH_WIDGET,
                               XmNleftWidget,       _Tarrow,
                               XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                               XmNbottomWidget,     _Tarrow,
                               XmNorientation,      _slsta->_orientation,
		               XmNwidth,            w,
                               XmNscaleWidth,       _slsta->_height,
                               NULL);
/*
    XtVaSetValues (_scale,                              
                               XmNtopAttachment,    XmATTACH_OPPOSITE_WIDGET,
                               XmNtopWidget,        _Tarrow,
                               XmNleftAttachment,   XmATTACH_WIDGET,
                               XmNleftWidget,       _Tarrow,
                               XmNbottomAttachment, XmATTACH_OPPOSITE_WIDGET,
                               XmNbottomWidget,     _Tarrow,
                               XmNorientation,      _slsta->_orientation,
                               XmNscaleWidth,       _slsta->_height,
                               NULL);
*/

/*
    Dimension sw;
    unsigned char rp;
    XtVaGetValues (_scale,
		               XmNscaleWidth,       &sw,
		               XmNwidth,            &w,
		               XmNresizePolicy,      &rp,
		               NULL);
    switch (rp) {
    case XmRESIZE_NONE :
      printf ("scale resize policy is NONE\n");
      break;
    case XmRESIZE_ANY :
      printf ("scale resize policy is ANY\n");
      break;
    case XmRESIZE_GROW :
      printf ("scale resize policy is GROW\n");
      break;
    default:
      printf ("scale resize policy not recognized(%d)\n", rp);
      break;
    }
    printf ("scale width = %d\n width = %d\n\n", sw, w);
*/
  }
  else {
    // for a vertical scale
    XtVaSetValues (_Tarrow,
                               XmNtopAttachment,    XmATTACH_FORM,
                               XmNrightAttachment,  XmATTACH_FORM,
                               XmNarrowDirection,   XmARROW_UP,
                               NULL);
 
    XtVaSetValues (_Barrow,
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _scale,
                               XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,       _Tarrow,
                               XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                               XmNrightWidget,      _Tarrow,
                               XmNarrowDirection,   XmARROW_DOWN,
                               NULL);

    XtVaSetValues (_scale,                              
                               XmNtopAttachment,    XmATTACH_WIDGET,
                               XmNtopWidget,        _Tarrow,
                               XmNleftAttachment,   XmATTACH_OPPOSITE_WIDGET,
                               XmNleftWidget,       _Tarrow,
                               XmNrightAttachment,  XmATTACH_OPPOSITE_WIDGET,
                               XmNrightWidget,      _Tarrow,
                               XmNorientation,      _slsta->_orientation,
                               XmNscaleHeight,      _slsta->_height,
                               NULL);
/*
    Dimension sh, h;
    unsigned char rp;
    XtVaGetValues (_scale,
		               XmNscaleHeight,      &sh,
		               XmNheight,           &h,
		               XmNresizePolicy,      &rp,
		               NULL);

    switch (rp) {
    case XmRESIZE_NONE :
      printf ("scale resize policy is NONE\n");
      break;
    case XmRESIZE_ANY :
      printf ("scale resize policy is ANY\n");
      break;
    case XmRESIZE_GROW :
      printf ("scale resize policy is GROW\n");
      break;
    default:
      printf ("scale resize policy not recognized(%d)\n", rp);
      break;
    }
    printf ("scale height = %d\n height = %d\n\n", sh, h);
*/
  }

  return topWidget ();
}
