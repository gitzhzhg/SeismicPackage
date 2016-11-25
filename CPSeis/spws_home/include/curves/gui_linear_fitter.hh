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
#ifndef GUI_LINEAR_FITTER_HH
#define GUI_LINEAR_FITTER_HH

#include "curves/gui_curve_fitter.hh"
#include <Xm/Xm.h>

class LSLinearFitter;
class SLScaleTextArrow;

class GuiLinearFitter : public GuiCurveFitter
{

public:
  GuiLinearFitter				// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     const char *title,				//   gui curve fit title
     int type);					//   gui curve fit type

  GuiLinearFitter				// constructor
    (SLDelay *container,			//   SL Delay container
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     const char *title,				//   gui curve fit title
     int type);					//   gui curve fit type

  virtual Widget make				// make function
    (Widget parent = NULL);			//   given parent widget

  virtual int setFitterParameters ();		// set the fitter parameters

protected:
  void init ();					// constructor helper

  virtual Boolean otherRecognizedObject		// rtn True if object is known
    (SLDelay *obj);				//   SL object acted upon

  virtual int setOtherGuiValues ();		// extend dataChangedFit

  virtual int setOtherFitterValues ();		// extend userChangedFit

  SLScaleTextArrow
    *_a0_arrow,					// a0 coefficient i/f
    *_a1_arrow;					// a1 coefficient i/f

  float
    _a0,					// a0 coefficient
    _a1;					// a1 coefficient

};

#endif
