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
// class that creates the attribute range gui for a color bar builder
#ifndef CBB_ATTR_RNG_GUI_HH
#define CBB_ATTR_RNG_GUI_HH

#include "sl/sl_form.hh"

class ColorBarBuilderPop;
class SLScaleTextArrow;
class SLPushBox;

class CBBAttrRngGui : public SLForm {

public:

  enum {
    CBB_INSERT,					// ident to do insertion
    CBB_INTERPOLATE,				// ident to do interpolation
    CBB_EXTRAPOLATE,				// ident to do extrapolation
    CBB_EXTRACT					// ident to do extraction
  };

  CBBAttrRngGui					// constructor
    (Widget parent,				//   parent widget
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  CBBAttrRngGui					// constructor
    (SLDelay *container,			//   container
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  ~CBBAttrRngGui ();				// destructor

  void init ();					// constructor helper

  virtual Widget make				// make function
    (Widget parent = 0);			//   parent widget

  virtual Boolean notifyComplex			// action notify function
    (SLDelay *obj,				//   SL object acted upon
     int ident);				//   and which element therein

  float from ()					// return beginning attr value
    { return _from; }

  float to ()					// return ending attr value
    { return _to; }

  void setFrom					// set beginning attr value
    (float value);				//   given beg attrib value

  void setFrom ();				// set beginning attr value

  void setTo					// set ending attr value
    (float value);				//   given ending attrib value

  void setTo ();				// set ending attr value

  void redoAttributeRange ();			// redefine the attrib range

private:
  ColorBarBuilderPop
    *_cbb_pop;					// color bar builder popup

  SLScaleTextArrow
    *_from_arrow,				// beginning attr value chngr
    *_to_arrow;					// ending attrib value chngr

  SLPushBox
    *_fill_box;					// how to fill from-to

  float
    _attr_min,					// minimum attribute value
    _attr_max,					// maximum attribute value
    _from,					// beginning attribute value
    _to;					// ending attribute value

};

#endif
