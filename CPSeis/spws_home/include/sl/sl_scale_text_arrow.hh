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
// sl_scale_text_arrow.hh

#ifndef SL_SCALE_TEXT_ARROW_HH
#define SL_SCALE_TEXT_ARROW_HH

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include <Xm/Scale.h>
#include "wproc.h"
#include "sl/sl_form.hh"

#define SLSTA_VALUE  0.0
#define SLSTA_LOW    0.0
#define SLSTA_HIGH   1.0
#define SLSTA_INCR   0.01
#define SLSTA_PREC   3
#define SLSTA_NCOL   6
#define SLSTA_HGHT   100

class SLpText;
class SLScaleTextArrow;

///////////////////////////////////////////////////////////////////////////////

class SLSTAText : public SLForm {

friend class SLScaleTextArrow;

public:
  SLSTAText					// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     SLScaleTextArrow *slsta = NULL,		//   given associated object
     Boolean make_now = True);			//   make it now flag

  SLSTAText					// constructor
    (SLDelay *contain,				//   SL Delay container
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     SLScaleTextArrow *slsta = NULL,		//   given associated object
     Boolean make_now = True);			//   make it now flag

  virtual Widget make				// make function
    (Widget parent = NULL);			//   given parent widget

private:
  void init ();					// constructor helper

  SLScaleTextArrow
    *_slsta;					// given scale text arrow obj

  SLpText
    *_text;					// text value widget
};

///////////////////////////////////////////////////////////////////////////////

class SLSTAScale : public SLForm {

friend class SLScaleTextArrow;

public:
  SLSTAScale					// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     SLScaleTextArrow *slsta = NULL,		//   given associated object
     Boolean make_now = True);			//   make it now flag

  SLSTAScale					// constructor
    (SLDelay *contain,				//   SL Delay container
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     SLScaleTextArrow *slsta = NULL,		//   given associated object
     Boolean make_now = True);			//   make it now flag

  virtual Widget make				// make function
    (Widget parent = NULL);			//   given parent widget

private:
  SLScaleTextArrow
    *_slsta;					// given scale text arrow obj

  Widget
    _Tarrow,					// top (left) arrow widget
    _Barrow,					// bottom (right) arrow widget
    _scale;					// scale bar widget
};

///////////////////////////////////////////////////////////////////////////////

class SLScaleTextArrow : public SLForm {

friend class SLSTAText;
friend class SLSTAScale;

public:

  enum {
    VALUE_INPUT = 1,                            // indicates value was input
    ARROW_PRESSED,				// flags arrow being pressed
    VALUE_CHANGED,				// indicates value was changed
    SLIDER_DRUG					// flags slidder being drug
  };

  SLScaleTextArrow				// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     float *valptr = NULL,			//   given address of value
     Boolean dowrap = True,			//   flag for wrapping value
     float value = SLSTA_VALUE,			//   given current value
     float low = SLSTA_LOW,			//   given lowest value
     float high = SLSTA_HIGH,			//   given highest value
     float increment = SLSTA_INCR,		//   given increment for value
     int precision = SLSTA_PREC,		//   given # of decimal places
     int column = SLSTA_NCOL,			//   given # of text columns
     unsigned char orientation = XmHORIZONTAL,	//   scale orientation
     char *label = 0,				//   text label
     Dimension height = SLSTA_HGHT,		//   height of scale
     Boolean make_now = True);			//   make it now flag

  SLScaleTextArrow				// constructor
    (SLDelay *contain,				//   SL Delay container
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     float *valptr = NULL,			//   given address of value
     Boolean dowrap = True,			//   flag for wrapping value
     float value = SLSTA_VALUE,			//   given current value
     float low = SLSTA_LOW,			//   given lowest value
     float high = SLSTA_HIGH,			//   given highest value
     float increment = SLSTA_INCR,		//   given increment for value
     int precision = SLSTA_PREC,		//   given # of decimal places
     int column = SLSTA_NCOL,			//   given # of text columns
     unsigned char orientation = XmHORIZONTAL,	//   scale orientation
     char *label = 0,				//   text label
     Dimension height = SLSTA_HGHT,		//   height of scale
     Boolean make_now = True);			//   make it now flag

  virtual ~SLScaleTextArrow ();			// destructor

  virtual Widget make				// make function
    (Widget parent = NULL);			//   given parent widget

  void initValue				// init internal value
    (float value);				//   given the value

  void setValue					// set internal value
    (float value);				//   given the value

  float getValue ()				// get the internal value
    {return _value;}

  void setRange					// set the range of values
    (float low,					//   given the low value
     float high);				//   given the high value

  void setIncrement				// set the value increment
    (float increment)				//   given the increment
    {_increment = increment;}

  Widget topW ();				// return top arrow widget

  Widget bottomW ();				// return bottom arrow widget

  Widget textW ();				// return value widget

  Widget scaleW ();				// return scale bar widget

  void setWrap					// set the wrap value flag
    (Boolean dowrap)				//   given the wrap flag
    {_dowrap = dowrap;}

  virtual WidgetClass topClass ()		// return the top widget class
    {return xmFormWidgetClass;}

  static void valueTrap				// general callback
    (void *object,				//   data object
     long ident,				//   data value identifier
     float oldvar,				//   old data value
     float newvar);				//   new data value

protected:
  void ArrowPress				// call when arrow is pressed
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to the object
     XmArrowButtonCallbackStruct* CBdata);	//   pointer to the data

  void ValueChanged				// call when scale increments
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to the object
     XmScaleCallbackStruct* CBdata);		//   pointer to the data

  void Drag					// call when scale drags
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to the object
     XmScaleCallbackStruct* CBdata);		//   pointer to the data

  void setValueAsIs ();				// set text by loc value

  void setValueByScale ();			// set text using scale value

  void setValueByScaleDrag ();			// set text using scale value

  void dontLetValueGrowTooBig			// limit incremented value
    (float test_value);				//   given new incremented vlu

  void dontLetValueShrinkTooSmall		// limit decremented value
    (float test_value);				//   given new decremented vlu

  SLSTAScale
    *_slsta_scale;				// scale object

  SLSTAText
    *_slsta_text;				// text object

  Dimension
    _height;					// height of scale bar

  Boolean
    _dowrap,					// wrap value flag
    _been_mapped;				// True after form mapped

  unsigned char
    _orientation;				// scale bar orientation

  char
    *_label;					// text label

  float
    _value,					// internal value
    *_valptr,					// where value is stored
    _high,					// highest value
    _low,					// lowest value
    _increment;					// value increment

  int
    _old_value,					// previous internal scl value
    _precision,					// # of decimal points
    _columns;					// # of columns in text

private:
  void init ();					// constructor helper

  static void ArrowPressCallback		// arrow press call back fcn
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to object
     XmArrowButtonCallbackStruct* CBdata);	//   pointer to data

  static void ValueChangedCallback		// value changed call back fcn
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to object
     XmScaleCallbackStruct* CBdata);		//   pointer to the data

  static void DragCallback			// drag call back fcn
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to object
     XmScaleCallbackStruct* CBdata);		//   pointer to the data

  static void mapCallback			// call back fcn when mapped
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to object
     XEvent *event);				//   pointer to the event

  void centerText ();				// centers text with form

};

#endif
