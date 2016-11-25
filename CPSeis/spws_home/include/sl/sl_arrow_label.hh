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
// sl_arrow_label.hh

#ifndef SL_ARROW_LABEL_HH
#define SL_ARROW_LABEL_HH

#include <Xm/Xm.h>
#include <Xm/Form.h>
#include "wproc.h"
#include "sl/sl_delay.hh"

class SLArrowLabel : public SLDelay {

public:
  enum {SLAL_VALUE = 1,				// default current value
        SLAL_LOW   = 1,				// default lowest value
        SLAL_HIGH  = 7,				// default highest value
        SLAL_INCR  = 2				// default increment for value
  };

  SLArrowLabel					// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     int *valptr = NULL,			//   given address of value
     Boolean dowrap = True,			//   flag for wrapping value
     int value = SLAL_VALUE,			//   given current value
     int low = SLAL_LOW,			//   given lowest value
     int high = SLAL_HIGH,			//   given highest value
     int increment = SLAL_INCR,			//   given increment for value
     Boolean make_now = True);			//   make it now flag

  SLArrowLabel					// constructor
    (SLDelay *contain,				//   SL Delay container
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     int *valptr = NULL,			//   given address of value
     Boolean dowrap = True,			//   flag for wrapping value
     int value = SLAL_VALUE,			//   given current value
     int low = SLAL_LOW,			//   given lowest value
     int high = SLAL_HIGH,			//   given highest value
     int increment = SLAL_INCR,			//   given increment for value
     Boolean make_if_can = True);		//   make if can flag

  virtual Widget make				// make function
    (Widget parent = NULL);			//   given parent widget

  void setValue					// set internal value
    (int value);				//   given the value

  int getValue ()				// get the internal value
    {return _value;}

  void setRange					// set the range of values
    (int low,					//   given the low value
     int high);					//   given the high value

  void setIncrement				// set the value increment
    (int increment)				//   given the increment
    {_increment = increment;}

  Widget topW ()				// return top arrow widget
    {return _Tarrow;}

  Widget bottomW ()				// return bottom arrow widget
    {return _Barrow;}

  Widget labelW ()				// return value widget
    {return _label;}

  void setWrap					// set the wrap value flag
    (Boolean dowrap)				//   given the wrap flag
    {_dowrap = dowrap;}

  virtual WidgetClass topClass ()		// return the top widget class
    {return xmFormWidgetClass;}

protected:
  void ArrowPress				// call when arrow is pressed
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to the object
     XmArrowButtonCallbackStruct* CBdata);	//   pointer to the data

  virtual ~SLArrowLabel ();			// destructor

  Widget
    _Tarrow,					// top arrow widget
    _Barrow,					// bottom arrow widget
    _label;					// value label

  Boolean
    _dowrap;					// wrap value flag

  int
    _value,					// internal value
    *_valptr,					// where value is stored
    _high,					// highest value
    _low,					// lowest value
    _increment;					// value increment

private:
  void centerGraphics ();			// called when mapping

  static void ArrowPressCallback		// arrow press call back fcn
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to object
     XmArrowButtonCallbackStruct* CBdata);	//   pointer to data

  static void mapCallback			// when mappin-call back fcn
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to object
     XtPointer CBdata);				//   pointer to data

  Boolean
    _been_mapped;				// true after mapped

};





class SLArrowTitle : public SLDelay {

public:
  SLArrowTitle					// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     int *valptr = NULL,			//   given address of value
     Boolean dowrap = True,			//   flag for wrapping value
     int value = SLArrowLabel::SLAL_VALUE,	//   given current value
     int low = SLArrowLabel::SLAL_LOW,		//   given lowest value
     int high = SLArrowLabel::SLAL_HIGH,	//   given highest value
     int increment = SLArrowLabel::SLAL_INCR,	//   given increment for value
     Boolean make_now = True);			//   make it now flag

  SLArrowTitle					// constructor
    (SLDelay *contain,				//   SL Delay container
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     int *valptr = NULL,			//   given address of value
     Boolean dowrap = True,			//   flag for wrapping value
     int value = SLArrowLabel::SLAL_VALUE,	//   given current value
     int low = SLArrowLabel::SLAL_LOW,		//   given lowest value
     int high = SLArrowLabel::SLAL_HIGH,	//   given highest value
     int increment = SLArrowLabel::SLAL_INCR,	//   given increment for value
     Boolean make_if_can = True);		//   make if can flag

  virtual Widget make				// make function
    (Widget parent = NULL);			//   given parent widget

  SLArrowLabel *arrowLabel ()			// return arrow label pntr
    { return _arrow_label; }

  virtual WidgetClass topClass ()		// return the top widget class
    {return xmFormWidgetClass;}

  virtual Boolean isContainer ()		// this class contains somethin
    {return True;}

protected:
  virtual ~SLArrowTitle ();			// destructor

  Widget
    _title;					// title label widget

  SLArrowLabel
    *_arrow_label;				// arrow label pntr

  char
    *_name;					// name of title label

  Boolean
    _dowrap;					// wrap value flag

  int
    _value,					// internal value
    *_valptr,					// where value is stored
    _high,					// highest value
    _low,					// lowest value
    _increment;					// value increment

private:
  void centerGraphics ();			// called when managing

  static void mapCallback			// when mappin-call back fcn
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to object
     XtPointer CBdata);				//   pointer to data

  Boolean
    _been_mapped;				// true after mapped


};

#define SLAL_VALUE_FP  0.0			// default current value
#define SLAL_LOW_FP    0.0			// default lowest value
#define SLAL_HIGH_FP   1.0			// default highest value
#define SLAL_INCR_FP   0.01			// default increment for value

class SLArrowFPLabel : public SLDelay {

public:
  SLArrowFPLabel				// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     float *valptr = NULL,			//   given address of value
     Boolean dowrap = True,			//   flag for wrapping value
     float value = SLAL_VALUE_FP,		//   given current value
     float low = SLAL_LOW_FP,			//   given lowest value
     float high = SLAL_HIGH_FP,			//   given highest value
     float increment = SLAL_INCR_FP,		//   given increment for value
     Boolean make_now = True);			//   make it now flag

  SLArrowFPLabel				// constructor
    (SLDelay *contain,				//   SL Delay container
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     float *valptr = NULL,			//   given address of value
     Boolean dowrap = True,			//   flag for wrapping value
     float value = SLAL_VALUE_FP,		//   given current value
     float low = SLAL_LOW_FP,			//   given lowest value
     float high = SLAL_HIGH_FP,			//   given highest value
     float increment = SLAL_INCR_FP,		//   given increment for value
     Boolean make_if_can = True);		//   make if can flag

  virtual Widget make				// make function
    (Widget parent = NULL);			//   given parent widget

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

  Widget topW ()				// return top arrow widget
    {return _Tarrow;}

  Widget bottomW ()				// return bottom arrow widget
    {return _Barrow;}

  Widget labelW ()				// return value widget
    {return _label;}

  void setWrap					// set the wrap value flag
    (Boolean dowrap)				//   given the wrap flag
    {_dowrap = dowrap;}

  virtual WidgetClass topClass ()		// return the top widget class
    {return xmFormWidgetClass;}

protected:
  void ArrowPress				// call when arrow is pressed
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to the object
     XmArrowButtonCallbackStruct* CBdata);	//   pointer to the data

  virtual ~SLArrowFPLabel ();			// destructor

  Widget
    _Tarrow,					// top arrow widget
    _Barrow,					// bottom arrow widget
    _label;					// value label

  Boolean
    _dowrap;					// wrap value flag

  float
    _value,					// internal value
    *_valptr,					// where value is stored
    _high,					// highest value
    _low,					// lowest value
    _increment;					// value increment

private:
  void centerGraphics ();			// called when mapping

  static void ArrowPressCallback		// arrow press call back fcn
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to object
     XmArrowButtonCallbackStruct* CBdata);	//   pointer to data

  static void mapCallback			// when mappin-call back fcn
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to object
     XtPointer CBdata);				//   pointer to data

  Boolean
    _been_mapped;				// true after mapped

};





class SLArrowFPTitle : public SLDelay {

public:
  SLArrowFPTitle				// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     float *valptr = NULL,			//   given address of value
     Boolean dowrap = True,			//   flag for wrapping value
     float value = SLAL_VALUE_FP,		//   given current value
     float low = SLAL_LOW_FP,			//   given lowest value
     float high = SLAL_HIGH_FP,			//   given highest value
     float increment = SLAL_INCR_FP,		//   given increment for value
     Boolean make_now = True);			//   make it now flag

  SLArrowFPTitle				// constructor
    (SLDelay *contain,				//   SL Delay container
     char *name,				//   name given
     HelpCtx hctx = NULL,			//   context sensitive help
     float *valptr = NULL,			//   given address of value
     Boolean dowrap = True,			//   flag for wrapping value
     float value = SLAL_VALUE_FP,		//   given current value
     float low = SLAL_LOW_FP,			//   given lowest value
     float high = SLAL_HIGH_FP,			//   given highest value
     float increment = SLAL_INCR_FP,		//   given increment for value
     Boolean make_if_can = True);		//   make if can flag

  virtual Widget make				// make function
    (Widget parent = NULL);			//   given parent widget

  SLArrowFPLabel *arrowLabel ()			// return arrow label pntr
    { return _arrow_label; }

  virtual WidgetClass topClass ()		// return the top widget class
    {return xmFormWidgetClass;}

  virtual Boolean isContainer ()		// this class contains somethin
    {return True;}

protected:
  virtual ~SLArrowFPTitle ();			// destructor

  Widget
    _title;					// title label widget

  SLArrowFPLabel
    *_arrow_label;				// arrow label pntr

  char
    *_name;					// name of title label

  Boolean
    _dowrap;					// wrap value flag

  float
    _value,					// internal value
    *_valptr,					// where value is stored
    _high,					// highest value
    _low,					// lowest value
    _increment;					// value increment

private:
  void centerGraphics ();			// called when managing

  static void mapCallback			// when mappin-call back fcn
    (Widget widget,				//   widget
     XtPointer OBJdata,				//   pointer to object
     XtPointer CBdata);				//   pointer to data

  Boolean
    _been_mapped;				// true after mapped


};

#endif
