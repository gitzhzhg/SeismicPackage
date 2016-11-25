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
#ifndef CUBE_AMPLITUDE_GUI_HH
#define CUBE_AMPLITUDE_GUI_HH

#include "sl/sl_form_pop.hh"
#include "cube/cube_inform.hh"

class SLpRadio;
class RadioList;
class SLTextBox;
class Cube;
class CubeDisplay;

class CubeAmplitudeGui : public SLFPopSep {

public:
  CubeAmplitudeGui				// constructor
    (Widget p,					//   parent widget
     char *name,				//   name of the popup
     HelpCtx hctx,				//   context sensitive help obj
     CubeDisplay *cube_display);		//   cube display

  ~CubeAmplitudeGui ();				// destructor

  virtual Widget make				// make function
    (Widget p);					//   parent widget

  virtual void managing ();			// call when managing

  void setParams ();				// set params w/o plot

  void dontPlotYet				// set for plot on 1st OK/APPLY
    (Boolean dont_plot_yet = True)		//   given how to set the flag
    {_dont_plot_yet = dont_plot_yet;}

  void variableDensitySensitivity ();		// set sensitivity for var den

  void wiggleTraceSensitivity ();		// set sensitivity for wiggles


protected:
  virtual void DoAction ();			// do after OK or APPLY

private:
  int plotIfOk					// returns true if succesful
    (Cube *cube);				//   given cube to plot if OK

  static void ampuiFocusAction			// called when ampui focused on
    (void *data,				//   object data
     long ident);				//   parameter identifier

  static void ampuiLosingFocusAction		// called when ampui focus lost
    (void *data,				//   object data
     long ident);				//   parameter identifier

  CubeDisplay
    *_cube_display;				// cube display object

  RadioList
    *_norm_list;				// normalization type rlist

  SLpRadio
    *_stp,					// amp scale to panel max
/*  *_stps,					// amp scale to all panels mx*/
    *_norm,					// amp scale to each trace
    *_stc,					// amp scale to file max
    *_stac,					// amp scale to all cubes max
    *_sta;					// amp scale to given amp

  SLTextBox
    *_ampui;					// amplitude value text box

  Boolean
    _wiggle_trace,				// flag for wiggle trace
    _dont_plot_yet;				// flag to plot on 1st OK/APPLY

  long
    _norm_state;				// state of normalization

  float
    _amp,					// global amplitude value
    _amp_at_focus;				// global amplitude value

};

#endif
