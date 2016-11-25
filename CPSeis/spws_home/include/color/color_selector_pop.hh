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
// class that creates the color selector menu
#ifndef COLOR_SELECTOR_POP_HH
#define COLOR_SELECTOR_POP_HH

#include "sl/sl_form_pop.hh"
#include "wproc.h"
#include <Xm/Xm.h>

class ColorBarBuilderPop;
class CBBColorSet;
class CBBRGBSet;
class AmplitudeProcessor;
class ColorDescriptor;
class SLCenteringForm;
class SLScaleTextArrow;
class RGBToBHS;

class ColorSelectorPop :  public SLFPopSep {

public:
  ColorSelectorPop				// constructor
    (Widget parent,				//   parent widget
     char *name,				//   name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb,			//   color bar builder
     int ncols);				//   # of colors required

  ColorSelectorPop				// constructor
    (SLDelay *container,			//   container
     char *name,				//   name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb,			//   color bar builder
     int ncols);				//   # of colors required

  virtual ~ColorSelectorPop ();			// destructor

  virtual Widget make				// make function
    (Widget parent = 0);			//   parent widget

  virtual Boolean notifyComplex			// action notify function
    (SLDelay *obj,				//   SL object acted upon
     int ident);				//   and which element therein

  virtual void DoAction ();			// do when OK or APPLY pushed

  virtual void managing ();			// called when being managed

  void activeColor				// return active color
    (float *red,				//   active red
     float *green,				//   active green
     float *blue);				//   active blue

  void setActiveColor				// sets active color
    (float red,					//   active red
     float green,				//   active green
     float blue);				//   active blue

private:
  void init					// constructor helper
    (int ncols);				//   # of colors required

  void initActiveColor ();			// inits active color

  void initHueColors ();			// inits hue colors

  void initBrightnessColors ();			// inits brightness colors

  void initColors ();				// inits reference colors

  void initRGBs ();				// inits RGB objects

  ColorBarBuilderPop
    *_cbb;					// color bar builder popup

  CBBColorSet
    *_active,					// Color selector active color
    *_hue,					// col swath of varying hues 
    *_brightness;				// col swath of varying brites

  CBBRGBSet
    *_active_rgb,				// active RGBs
    *_hue_rgb,					// hue RGBs
    *_brightness_rgb;				// brightness RGBs

  AmplitudeProcessor
    *_active_amp,				// active color descriptor
    *_hue_amp,					// hue color descriptor
    *_brightness_amp;				// brightness color descriptor

  RGBToBHS
    *_rgb_to_bhs;				// obj to cnvrt rgb to bhs

  ColorDescriptor
    *_active_col,				// active color descriptor
    *_hue_col,					// hue color descriptor
    *_brightness_col;				// brightness color descriptor

  SLScaleTextArrow
    *_saturation_arrow,				// amt of sat in active color
    *_red_arrow,				// amt of red in active color
    *_green_arrow,				// amt of grn in active color
    *_blue_arrow;				// amt of blu in active color

  SLCenteringForm
    *_title_cf,					// title centering form
    *_active_cf;				// active color centering form

  Boolean
    _been_managed;				// False until managed

  float
    _pi,					// dimensionless value pi
    _h_saturation,				// hue set saturation
    _h_red,					// hue set red
    _h_green,					// hue set green
    _h_blue,					// hue set blue
    _b_red,					// brightness set red
    _b_green,					// brightness set green
    _b_blue,					// brightness set blue
    _a_red,					// active red
    _a_green,					// active green
    _a_blue;					// active blue

  int
    _ncols,					// total # of colors to use
    _status;					// status flag

};

#endif
