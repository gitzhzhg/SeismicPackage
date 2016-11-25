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
// class that creates the color extrapolation gui for a color bar builder
#ifndef CBB_COL_EXT_GUI_HH
#define CBB_COL_EXT_GUI_HH

#include "sl/sl_form.hh"
#include "color/color_bar_builder_pop.hh"

class SLScaleTextArrow;
class SLRadioBox;

class CBBColExtGui : public SLForm {

public:

  enum {
    INCREASING_CC0,				// ident to incr extrap CC0
    DECREASING_CC0,				// ident to decr extrap CC0
    INCREASING_CC1,				// ident to incr extrap CC1
    DECREASING_CC1,				// ident to decr extrap CC1
    INCREASING_CC2,				// ident to incr extrap CC2
    DECREASING_CC2				// ident to decr extrap CC2
  };

  CBBColExtGui					// constructor
    (Widget parent,				//   parent widget
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  CBBColExtGui					// constructor
    (SLDelay *container,			//   containter
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     ColorBarBuilderPop *cbb_pop);		//   CBB pop up

  ~CBBColExtGui ();				// destructor

  void init ();					// constructor helper

  virtual Widget make				// make function
    (Widget parent = 0);			//   parent widget

  virtual Boolean notifyComplex			// action notify function
    (SLDelay *obj,				//   SL object acted upon
     int ident);				//   and which element therein

  int setState ();				// set state of the

  void selectIncreasing				// select increasing extr for
    (ColorBarBuilderPop::ColorCoordinate
     which_cc);					//   given color coordinate

  void selectDecreasing				// select decreasing extr for
    (ColorBarBuilderPop::ColorCoordinate
     which_cc);					//   given color coordinate

  int increasingSelected			// rtn 0 if INCREASING not sel
    (ColorBarBuilderPop::ColorCoordinate
     which_cc);					//   given color coordinate

  int decreasingSelected			// rtn 0 if DECREASING not sel
    (ColorBarBuilderPop::ColorCoordinate
     which_cc);					//   given color coordinate

  void setCycles				// select increasing extr for
    (ColorBarBuilderPop::ColorCoordinate
     which_cc,					//   given color coordinate
     float cycles);				//   given # of cycles

  float cycles					// return # of cycles
    (ColorBarBuilderPop::ColorCoordinate
     which_cc);					//   given color coordinate

  int status ()					// return status flag
    { return _status; }

private:
  ColorBarBuilderPop
    *_cbb_pop;					// color bar builder popup

  SLRadioBox
    *_ext_cc0_box,				// extr dir coord 0 radio box
    *_ext_cc1_box,				// extr dir coord 1 radio box
    *_ext_cc2_box;				// extr dir coord 2 radio box

  SLScaleTextArrow
    *_ext_cc0_arrow,				// extr cycles for col coord 0
    *_ext_cc1_arrow,				// extr cycles for col coord 1
    *_ext_cc2_arrow;				// extr cycles for col coord 2

  Widget
    _cc0,					// 0th color coordinate label
    _cc1,					// 1st color coordinate label
    _cc2;					// 2nd color coordinate label

  int
    _status;					// status flag

};

class CBBColExtSettings {

public:
  enum ExtrapDir {
    INCREASING,					// extrapolation increases
    DECREASING					// extrapolation decreases
  };

  CBBColExtSettings ()				// constructor
    :_cc0 (INCREASING),     _cc1 (INCREASING),     _cc2 (INCREASING),
     _cc0_cycles (1),       _cc1_cycles (1),       _cc2_cycles (1),
     _cc0_cycles_min (0),   _cc1_cycles_min (0),   _cc2_cycles_min (0),
     _cc0_cycles_max (3),   _cc1_cycles_max (3),   _cc2_cycles_max (3),
     _cc0_cycles_inc (.25), _cc1_cycles_inc (.25), _cc2_cycles_inc (.25) {}

  void setCC0					// set 0th col coord extr dir
    (ExtrapDir which_dir)			//   which extr dir to use
    { _cc0 = which_dir; }

  void setCC1					// set 1st col coord extr dir
    (ExtrapDir which_dir)			//   which extr dir to use
    { _cc1 = which_dir; }

  void setCC2					// set 2nd col coord extr dir
    (ExtrapDir which_dir)			//   which extr dir to use
    { _cc2 = which_dir; }

  ExtrapDir CC0 ()				// rtn 0th col coord extr dir
    { return _cc0; }

  ExtrapDir CC1 ()				// rtn 1st col coord extr dir
    { return _cc1; }

  ExtrapDir CC2 ()				// rtn 2nd col coord extr dir
    { return _cc2; }

  void setCC0Cycles				// set 0th col coord # cycles
    (float cycles)				//   # of cycles to use
    { _cc0_cycles = cycles; }

  void setCC1Cycles				// set 1st col coord # cycles
    (float cycles)				//   # of cycles to use
    { _cc1_cycles = cycles; }

  void setCC2Cycles				// set 2nd col coord # cycles
    (float cycles)				//   # of cycles to use
    { _cc2_cycles = cycles; }

  float CC0Cycles ()				// rtn 0th col coord # cycles
    { return _cc0_cycles; }

  float CC1Cycles ()				// rtn 1st col coord # cycles
    { return _cc1_cycles; }

  float CC2Cycles ()				// rtn 2nd col coord # cycles
    { return _cc2_cycles; }

  float *CC0CyclesPtr ()			// rtn 0th col crd # cycl ptr
    { return &_cc0_cycles; }

  float *CC1CyclesPtr ()				// rtn 1st col crd # cycl ptr
    { return &_cc1_cycles; }

  float *CC2CyclesPtr ()				// rtn 2nd col crd # cycl ptr
    { return &_cc2_cycles; }

  void setCC0CyclesMin				// set 0th col coord cycle min
    (float cycles)				//   # of cycles to use
    { _cc0_cycles_min = cycles; }

  void setCC1CyclesMin				// set 1st col coord cycle min
    (float cycles)				//   # of cycles to use
    { _cc1_cycles_min = cycles; }

  void setCC2CyclesMin				// set 2nd col coord cycle min
    (float cycles)				//   # of cycles to use
    { _cc2_cycles_min = cycles; }

  float CC0CyclesMin ()				// rtn 0th col coord cycle min
    { return _cc0_cycles_min; }

  float CC1CyclesMin ()				// rtn 1st col coord cycle min
    { return _cc1_cycles_min; }

  float CC2CyclesMin ()				// rtn 2nd col coord cycle min
    { return _cc2_cycles_min; }

  void setCC0CyclesMax				// set 0th col coord cycle max
    (float cycles)				//   # of cycles to use
    { _cc0_cycles_max = cycles; }

  void setCC1CyclesMax				// set 1st col coord cycle max
    (float cycles)				//   # of cycles to use
    { _cc1_cycles_max = cycles; }

  void setCC2CyclesMax				// set 2nd col coord cycle max
    (float cycles)				//   # of cycles to use
    { _cc2_cycles_max = cycles; }

  float CC0CyclesMax ()				// rtn 0th col coord cycle max
    { return _cc0_cycles_max; }

  float CC1CyclesMax ()				// rtn 1st col coord cycle max
    { return _cc1_cycles_max; }

  float CC2CyclesMax ()				// rtn 2nd col coord cycle max
    { return _cc2_cycles_max; }

  void setCC0CyclesInc				// set 0th col coord cycle inc
    (float cycles)				//   # of cycles to use
    { _cc0_cycles_inc = cycles; }

  void setCC1CyclesInc				// set 1st col coord cycle inc
    (float cycles)				//   # of cycles to use
    { _cc1_cycles_inc = cycles; }

  void setCC2CyclesInc				// set 2nd col coord cycle inc
    (float cycles)				//   # of cycles to use
    { _cc2_cycles_inc = cycles; }

  float CC0CyclesInc ()				// rtn 0th col coord cycle inc
    { return _cc0_cycles_inc; }

  float CC1CyclesInc ()				// rtn 1st col coord cycle inc
    { return _cc1_cycles_inc; }

  float CC2CyclesInc ()				// rtn 2nd col coord cycle inc
    { return _cc2_cycles_inc; }

private:
  ExtrapDir
    _cc0,					// for 0th color coordinate
    _cc1,					// for 1st color coordinate
    _cc2;					// for 2nd color coordinate

  float
    _cc0_cycles,				// for 0th color coordinate
    _cc1_cycles,				// for 1st color coordinate
    _cc2_cycles,				// for 2nd color coordinate
    _cc0_cycles_min,				// for 0th color coordinate
    _cc1_cycles_min,				// for 1st color coordinate
    _cc2_cycles_min,				// for 2nd color coordinate
    _cc0_cycles_max,				// for 0th color coordinate
    _cc1_cycles_max,				// for 1st color coordinate
    _cc2_cycles_max,				// for 2nd color coordinate
    _cc0_cycles_inc,				// for 0th color coordinate
    _cc1_cycles_inc,				// for 1st color coordinate
    _cc2_cycles_inc;				// for 2nd color coordinate

};

#endif
