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
#ifndef BANDW_INPUTS_HH
#define BANDW_INPUTS_HH

#include "sl/sl_form_pop.hh"

class AppOutputs;
class SLTextBox;
class SLRadioBox;
class SLpText;

class AppInputs : public SLFPopSep {

public:
  AppInputs					// constructor
    (Widget parent,				//   Widget parent
     char *name,				//   name of gui
     const unsigned long buttons,		//   buttons to use
     const HelpCtx hctx);			//   context sensitive help

  AppInputs					// constructor
    (SLDelay *container,			//   SLDelay container object
     char *name,				//   name of gui
     const unsigned long buttons,		//   buttons to use
     const HelpCtx hctx);			//   context sensitive help

  Boolean notifyComplex				// notify complex callback
    (SLDelay *sender,				//   message sender
     int ident);				//   sender identification

  virtual ~AppInputs ();			// destructor

  virtual Widget make				// make function
    (Widget parent = 0);			//   widget parent of gui

  void setOutputs				// set the Output object
    (AppOutputs *outputs);			//   given the Output object

  float getBox					// get the box value
    (int which_box);				//   given which box

  const char *getUnits				// get the box units
    (int which_box);				//   given which box

  int getButton ();                             // get the radio button number
     
  float getMaxGOR (float pres, float temp,      // get the maximum GOR value
     float api, float gg);

protected:
  virtual void DoAction ();			// do action

private:
  void init ();					// initialize object

  AppOutputs
    *_outputs;					// outputs object

  SLTextBox
    *_box1,    					// Pressure
    *_box2,                                     // Temperature
    *_box3,                                     // Salinity
    *_box4,                                     // Oil API
    *_box5,                                     // Gas Gravity
    *_box6;	                                // GOR    

  SLpText
      *_box7;                                   // Maximum GOR

  SLRadioBox
    *_rbox;	                                // radio box

  Widget
    _units7;                                    // units7 label 	

  char  *_units[7];				// units of boxes
  int    _inited;				// whether or not initialized	
  float  _boxes[6];				// values in boxes

};

#endif

