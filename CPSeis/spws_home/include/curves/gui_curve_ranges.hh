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
#ifndef GUI_CURVE_RANGES_HH
#define GUI_CURVE_RANGES_HH

#include "sl/sl_form_pop.hh"

class GuiCurveFitter;
class CurveFitter;
class SLpText;

class GuiCurveRanges : public SLFPopSep
{

public:
  enum {
    MIN0 = 0,					// minimum of coefficient x0
    MAX0,					// maximum of coefficient x0
    MIN1,					// minimum of coefficient y0
    MAX1,					// maximum of coefficient y0
    MIN2,					// minimum of coefficient c0
    MAX2,					// maximum of coefficient c0
    MIN3,					// minimum of coefficient c1
    MAX3,					// maximum of coefficient c1
    MIN4,					// minimum of coefficient c2
    MAX4					// maximum of coefficient c2
  };

  GuiCurveRanges				// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     GuiCurveFitter *gui_curve_fitter);		//   gui curve fitter

  GuiCurveRanges				// constructor
    (SLDelay *container,			//   SL Delay container
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     GuiCurveFitter *gui_curve_fitter);		//   gui curve fitter

  virtual ~GuiCurveRanges ();			// destructor

  virtual Widget make				// make function
    (Widget parent = NULL);			//   given parent widget

  void setRange					// set a coefficient range vlu
    (long ident,				//   given a coef extrema id
     float newvar);				//   given an extrema value

  float getRange				// get a coefficient range vlu
    (long ident);				//   given a coef extrema id

  CurveFitter *fitter ();			// return the curve fitter

  GuiCurveFitter *guiCurveFitter ()		// return the gui curve fitter
    { return _gui_curve_fitter; }  

private:
  GuiCurveFitter
    *_gui_curve_fitter;				// gui curve fitter object

  SLpText
    *_min0_txt,					// text gui for minimum of x0
    *_max0_txt,					// text gui for maximum of x0
    *_min1_txt,					// text gui for minimum of y0
    *_max1_txt,					// text gui for maximum of y0
    *_min2_txt,					// text gui for minimum of c0
    *_max2_txt,					// text gui for maximum of c0
    *_min3_txt,					// text gui for minimum of c1
    *_max3_txt,					// text gui for maximum of c1
    *_min4_txt,					// text gui for minimum of c2
    *_max4_txt;					// text gui for maximum of c2

};

#endif
