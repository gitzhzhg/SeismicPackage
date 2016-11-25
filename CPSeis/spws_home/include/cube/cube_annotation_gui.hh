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
#ifndef CUBE_ANNOTATION_GUI_HH
#define CUBE_ANNOTATION_GUI_HH

#include "sl/sl_form_pop.hh"

class CubeDisplay;
class SLTextBox;
class Cube;

class CubeAnnotationGui : public SLFPopSep {

public:
  CubeAnnotationGui				// constructor
    (SLDelay *contain,				//   SL Delay container
     char *name,				//   name of the popup
     HelpCtx hctx,				//   context sensitive help obj
     CubeDisplay *cube_display);		//   cube display object

  CubeAnnotationGui				// constructor
    (Widget p,					//   parent widget
     char *name,				//   name of the popup
     HelpCtx hctx,				//   context sensitive help obj
     CubeDisplay *cube_display);		//   cube display object

  void constructorHelper ();			// constructor helper function

  virtual ~CubeAnnotationGui ();		// destructor

  virtual Widget make				// make function
    (Widget p);					//   parent widget

  void setParams ();				// set params

  void setLineHeader				// set the line coord hdr wrd
    (int first_line_hwrd);			//   given a hdr wrd

  void setCrosslineHeader			// set the xline coord hdr wrd
    (int first_xline_hwrd);			//   given a hdr wrd

  virtual Boolean notifyComplex			// sl delay call back
    (SLDelay *sl_delay,				//   given sl object
     int ident);				//   given identifier

  void dontPlotYet				// set for plot on 1st OK/APPLY
    (Boolean dont_plot_yet = True)		//   given how to set the flag
    {_dont_plot_yet = dont_plot_yet;}

  float getStartingLine(){return _start_line;}
  float getPrimaryHorizontalIncrement(){return _prim_hori_line_incr;}
  float getSecondayHorizontalIncrement(){return _sec_hori_line_incr;}
  float getPrimaryLineHeader(){return _first_line_hwrd;}	
  float getPrimaryCrossLineHeader(){return _first_xline_hwrd;}
  float getCrossLineIncrement(){return _xline_incr;}





protected:
  virtual Boolean ValidInput ();		// check user input

  virtual void DoAction ();			// do after OK or APPLY

private:
  int plotIfOk					// returns true if succesful
    (Cube *cube);				//   given cube to plot if OK

  CubeDisplay
    *_cube_display;				// given cube display obj

  SLTextBox
    *_input_text,				// left annotation text box
    *_input_text1;				// right annotation text box

  Widget
    _titlew;					// plot title widget

  Boolean
    _dont_plot_yet;				// flag to plot on 1st OK/APPLY

  char
    *_plot_title;				// plot title

  int
    _first_line_hwrd,				// first line header word
    _secnd_line_hwrd,				// second line header word
    _first_xline_hwrd,				// first crossline header word
    _secnd_xline_hwrd;				// second crossline header word

  float
    _start_line,				// anno start on lines
    _line_incr,					// anno increment on lines
    _start_xline,				// anno start on crosslines
    _xline_incr,				// anno increment on crosslines
    _prim_hori_line_incr,			// primary horizon line incr
    _sec_hori_line_incr;			// secondary horizon line incr

};

#endif
