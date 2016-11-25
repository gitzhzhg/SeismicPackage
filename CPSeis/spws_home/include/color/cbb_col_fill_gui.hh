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
// class that creates the color fill gui for a color bar builder
#ifndef CBB_COL_FILL_GUI_HH
#define CBB_COL_FILL_GUI_HH

#include "sl/sl_form.hh"
#include "color/color_bar_builder_pop.hh"

class SLRadioBox;

class CBBColFillGui : public SLForm {

public:

  enum {
    CONSTANT_CC0,				// ident to CC0 fill as a cnst
    LINEAR_CC0,					// ident to CC0 fill linearly
    LOGARITHMIC_CC0,				// ident to CC0 fill logrthly
    EXPONENTIAL_CC0,				// ident to CC0 fill expnntly
    CONSTANT_CC1,				// ident to CC1 fill as a cnst
    LINEAR_CC1,					// ident to CC1 fill linearly
    LOGARITHMIC_CC1,				// ident to CC1 fill logrthly
    EXPONENTIAL_CC1,				// ident to CC1 fill expnntly
    CONSTANT_CC2,				// ident to CC2 fill as a cnst
    LINEAR_CC2,					// ident to CC2 fill linearly
    LOGARITHMIC_CC2,				// ident to CC2 fill logrthly
    EXPONENTIAL_CC2				// ident to CC2 fill expnntly
  };

  CBBColFillGui					// constructor
    (Widget parent,				//   parent widget
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  CBBColFillGui					// constructor
    (SLDelay *container,			//   container
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  ~CBBColFillGui ();				// destructor

  void init ();					// constructor helper

  virtual Widget make				// make function
    (Widget parent = 0);			//   parent widget

  virtual Boolean notifyComplex			// action notify function
    (SLDelay *obj,				//   SL object acted upon
     int ident);				//   and which element therein

  int setState ();				// set state of the

  void selectConstant				// select constant type for
    (ColorBarBuilderPop::ColorCoordinate
     which_cc);					//   given color coordinate

  void selectLinear				// select linear type for
    (ColorBarBuilderPop::ColorCoordinate
     which_cc);					//   given color coordinate

  void selectLogarithmic			// select logarithmic type for
    (ColorBarBuilderPop::ColorCoordinate
     which_cc);					//   given color coordinate

  void selectExponential			// select exponential type for
    (ColorBarBuilderPop::ColorCoordinate
     which_cc);					//   given color coordinate

  int constantSelected				// rtn 0 if CONSTANT not seltd
    (ColorBarBuilderPop::ColorCoordinate
     which_cc);					//   given color coordinate

  int linearSelected				// rtn 0 if LINEAR not selctd
    (ColorBarBuilderPop::ColorCoordinate
     which_cc);					//   given color coordinate

  int logarithmicSelected			// rtn 0 LOGARITHMIC not seltd
    (ColorBarBuilderPop::ColorCoordinate
     which_cc);					//   given color coordinate

  int exponentialSelected			// rtn 0 EXPONENTIAL not seltd
    (ColorBarBuilderPop::ColorCoordinate
     which_cc);					//   given color coordinate

  int status ()					// return status flag
    { return _status; }

private:
  ColorBarBuilderPop
    *_cbb_pop;					// color bar builder popup

  SLRadioBox
    *_fill_cc0_box,				// color coord 0 radio box
    *_fill_cc1_box,				// color coord 1 radio box
    *_fill_cc2_box;				// color coord 2 radio box

  Widget
    _cc0,					// 0th color coordinate label
    _cc1,					// 1st color coordinate label
    _cc2;					// 2nd color coordinate label

  int
    _status;					// status flag

};

class CBBColFillSettings {

public:
  enum FillType {
    CONSTANT,					// constant color fill set
    LINEAR,					// linear color fill set
    LOGARITHMIC,				// logarithmic color fill set
    EXPONENTIAL					// exponential color fill set
  };

  CBBColFillSettings ()				// constructor
    :_cc0 (LINEAR), _cc1 (LINEAR), _cc2 (LINEAR) {}

  void setCC0					// set 0th col coord fill type
    (FillType which_type)			//   which fill type to use
    { _cc0 = which_type; }

  void setCC1					// set 1st col coord fill type
    (FillType which_type)			//   which fill type to use
    { _cc1 = which_type; }

  void setCC2					// set 2nd col coord fill type
    (FillType which_type)			//   which fill type to use
    { _cc2 = which_type; }

  FillType CC0 ()				// rtn 0th col coord fill type
    { return _cc0; }

  FillType CC1 ()				// rtn 1st col coord fill type
    { return _cc1; }

  FillType CC2 ()				// rtn 2nd col coord fill type
    { return _cc2; }

private:
  FillType
    _cc0,					// 0th color coordinate
    _cc1,					// 1st color coordinate
    _cc2;					// 2nd color coordinate

};

#endif
