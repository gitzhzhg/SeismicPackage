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
// class that creates the color levels gui for a color bar builder
#ifndef CBB_LEVELS_GUI_HH
#define CBB_LEVELS_GUI_HH

#include "sl/sl_form.hh"

class ColorBarBuilderPop;
class SLScaleTextArrow;
class SLPushBox;

class CBBLevelsGui : public SLForm {

public:
  enum {
    SELECTOR,					// ident to pop up selector
    CONTROLLER					// ident to pop up controller
  };

  CBBLevelsGui					// constructor
    (Widget parent,				//   parent widget
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  CBBLevelsGui					// constructor
    (SLDelay *container,			//   container
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  ~CBBLevelsGui ();				// destructor

  virtual Widget make				// make function
    (Widget parent = 0);			//   parent widget

  virtual Boolean notifyComplex			// action notify function
    (SLDelay *obj,				//   SL object acted upon
     int ident);				//   and which element therein

  int levels ()					// return # of color levels
    { return (int)_levels; }

  void setLevels				// set # of color levels
    (int levels = -1);				//   given # of color levels

private:
  void init ();					// initialize object

  ColorBarBuilderPop
    *_cbb_pop;					// color bar builder popup

  SLScaleTextArrow
    *_levels_arrow;				// levels changer tool

  SLPushBox
    *_parms_box;				// push buttons

  float
    _levels;					// # of color levels

};

#endif
