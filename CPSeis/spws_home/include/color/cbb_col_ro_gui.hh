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
// class that creates the color readout gui for a color bar builder
#ifndef CBB_COL_RO_GUI_HH
#define CBB_COL_RO_GUI_HH

#include "sl/sl_form.hh"

class ColorBarBuilderPop;

class CBBColROGui : public SLForm {

public:
  CBBColROGui					// constructor
    (Widget parent,				//   parent widget
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  CBBColROGui					// constructor
    (SLDelay *container,			//   container
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  virtual ~CBBColROGui ();			// destructor

  virtual Widget make				// make function
    (Widget parent = 0);			//   parent widget

  void display					// display the readout values
    (float red = 0,				//   given the red value
     float green = 0,				//   given the green value
     float blue = 0,				//   given the blue value
     float attribute = 0);			//   given the attribute value

  void clear ();				// blank the readout values

  void setShowRGB				// set the RGB readout given
    (Boolean enable);				//   whether or not to enable

  static void showRGB				// function to call on event
    (Widget widget,				//   widget where motion is
     CBBColROGui *col_ro,			//   given read out object
     XEvent *event);				//   given X-event

private:
  ColorBarBuilderPop
    *_cbb_pop;					// color bar builder popup

  Widget
    _red_widget,				// red value label
    _green_widget,				// green value label
    _blue_widget,				// blue value label
    _attribute_widget;				// attribute value label

  Boolean
    _enabled;					// flag

};

#endif
