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
#ifndef GUI_BOWERS_FITTER_HH
#define GUI_BOWERS_FITTER_HH

#include "curves/gui_power_law_fitter.hh"

class SLScaleTextArrow;

class GuiBowersFitter : public GuiPowerLawFitter
{

public:
  GuiBowersFitter				// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     const char *title);			//   gui curve fitter title

  GuiBowersFitter				// constructor
    (SLDelay *contain,				//   SL Delay container
     char *name,				//   widget name
     HelpCtx hctx,				//   context sensitive help
     const char *title);			//   gui curve fitter title

  virtual Widget make				// make function
    (Widget parent = NULL);			//   given parent widget

  virtual int setFitterParameters ();		// set the fitter parameters

  virtual char *getCString			// return the coefficient str
    (int type);					//   given coefficient type

protected:
  void virtual init ();				// constructor helper

  virtual Boolean otherRecognizedObject		// rtns 0 if obj not recognizd
    (SLDelay *obj);				//   given object

  virtual int setOtherFitterValues ();		// set other fitter values

  virtual int displayErrorStatistics ()		// rtrns 0 if errors not dispd
    { return 0; }

  SLScaleTextArrow
    *_u_arrow;					// unloading factor GUI

  float
    _u;						// unloading factor

};

#endif
