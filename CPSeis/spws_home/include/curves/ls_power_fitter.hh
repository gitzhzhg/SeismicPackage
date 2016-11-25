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
// y' = y - y0
// x' = x - x0
// y' = x'**a1

#ifndef LS_POWER_FITTER_HH
#define LS_POWER_FITTER_HH

#include "curves/ls_gain_fitter.hh"

class LSPowerFitter : public LSGainFitter {

public:
  LSPowerFitter ();				// constructor

  virtual ~LSPowerFitter ();			// destructor

protected:
  virtual int independentVariableModifier	// indepndnt variable modifier
    (float x,					//   independent value
     double *modified_x);			//   modified independent valu

  virtual int independentVariableUnmodifier	// indepndnt var inverse mod
    (float x,					//   independent value
     double *unmodified_x);			//   unmodified independent vl

  virtual int dependentVariableModifier		// dependent variable modifier
    (float y,					//   dependent value
     double *modified_y);			//   modified dependent value

  virtual int dependentVariableUnmodifier	// dependent var inverse mod
    (float y,					//   dependent value
     double *unmodified_y);			//   unmodified dependent valu

};

#endif
