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
#ifndef BANDw_OUTPUTS_HH
#define BANDW_OUTPUTS_HH

#include "sl/sl_form_pop.hh"

class AppInputs;
class SLpText;

class AppOutputs : public SLFPopSep {

public:
  AppOutputs					// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   name of gui
     const unsigned long buttons,		//   buttons to use
     const HelpCtx hctx);			//   context sensitive help

  AppOutputs					// constructor
    (SLDelay *container,			//   SLDelay container object
     char *name,				//   name of gui
     const unsigned long buttons,		//   buttons to use
     const HelpCtx hctx);			//   context sensitive help

  virtual ~AppOutputs ();			// destructor

  virtual Widget make				// make function
    (Widget parent = 0);			//   widget parent of gui

  void updateOutputBoxs ();			// update the outputs

  void setInputs				// set up the inputs object
    (AppInputs *inputs);			//  given the inputs object

private:
  void init ();					// initialize object

  AppInputs
    *_inputs;					// input object

  SLpText
    *_box1,					// box 1
    *_box2,					// box 2
    *_box3,
    *_box4,
    *_units1,					// units 1
    *_units2,					// units 2
    *_units3,
    *_units4;
};

#endif
