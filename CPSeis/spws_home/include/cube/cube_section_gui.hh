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
#ifndef CUBE_SECTION_GUI_HH
#define CUBE_SECTON_GUI_HH

#include "sl/sl_form_pop.hh"

class CubeDisplay;
class CubeAmplitudeGui;
class SLRadioBox;
class SLTextBox;
class SLTogBox;
class CubeParams;
class Cube;

class CubeSectionGui : public SLFPopSep {

public:
  CubeSectionGui				// constructor
    (Widget p,					//   parent widget
     char *name,				//   name of the popup
     HelpCtx hctx,				//   context sensitive help obj
     CubeDisplay *cube_display,			//   cube display object
     CubeAmplitudeGui *cube_amp);		//   cube amplitude Gui

  ~CubeSectionGui ();				// destructor

  virtual Widget make				// make function
    (Widget p);					//   parent widget

  CubeParams  *_cube_params;

  void setUnits ();				// establish english/mks

  void setParams ();				// set params w/o plotting

  void dontPlotYet				// set for plot on 1st OK/APPLY
    (Boolean dont_plot_yet = True)		//   given how to set the flag
    {_dont_plot_yet = dont_plot_yet;}

  Boolean notifyComplex				// notify complex
    (SLDelay *obj,				//   object
     int ident);				//   identifier

  void checkCubeAmplitudeGui ();		// check state of cube amp obj

  CubeAmplitudeGui *cubeAmplitudeGui ()		// return the cube amp gui obj
    { return _cube_amp; }

  float getTmin()    {return _tmin;}
  float getTmax()    {return _tmax;}
  float getIS()      {return _tmscale;}
  float getCT()      {return _ct;}
  long  getPlotType(){return _plot_type;}
  float getRandomLinesPerInch()      {return _rlscale;}  
  long  getRandomLineRightToLeft()   {return _randomline_right_to_left;}
  long  getRandomLineInvertVertical(){return _randomline_invert_vertical;}

protected:
  virtual Boolean ValidInput ();		// check user input

  virtual void DoAction ();			// do after OK or APPLY

  void setTimeRange ();				// set time defaults & range

private:
  int plotIfOk					// returns true if succesful
    (Cube *cube);				//   given cube to plot if OK

  CubeDisplay
    *_cube_display;				// given cube display obj

  CubeAmplitudeGui
    *_cube_amp;					// given cube amplitude Gui ob

  SLRadioBox
    *_ptype;					// plot type radio box

  SLTextBox
    *_cube_scale,				// plot scale text box
    *_cube_tmin,				// cube minimum time box
    *_cube_tmax;				// plot maximum time box

  SLTogBox
    *_right_to_left_toggles,                    //plot displays right to left
    *_invert_vertical_toggles;                  //plot an inverted vertical axis


  Boolean
    _time_range_now_set,			// True if a cube has been read
    _dont_plot_yet;				// flag to plot on 1st OK/APPLY

  Widget
    _extrema_label;				// label showing max time

  float
    _lnscale,					// crosslines/inch scale
    _xlscale,					// lines/inch scale
    _rlscale,                                   // randomlines / inch scale
    _tmscale,					// inches/sec scale
    _tmin,					// starting time in seconds
    _tmax,					// ending time in seconds
    _mint,					// minimum time in seconds
    _maxt,					// maximum time in seconds
    _ct;					// channels/trace scale

  long
    _plot_type,
    _inline_right_to_left,
    _crossline_right_to_left,
    _timeslice_right_to_left,
    _randomline_right_to_left,
    _inline_invert_vertical,
    _crossline_invert_vertical,
    _timeslice_invert_vertical,
    _randomline_invert_vertical;
};

#endif
