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
// y' = log (y0 / y)  (porosity)
// x' = x - x0        (velocity)
// y' = a0 * x'**a1

#ifndef SLOW_SHALE_FITTER_HH
#define SLOW_SHALE_FITTER_HH

#include "curves/ls_power_law_fitter.hh"

class SlowShaleFitter : public LSPowerLawFitter {

public:
  SlowShaleFitter ();				// constructor

  virtual ~SlowShaleFitter ();			// destructor

  virtual int coefficientRangeDefault		// return def coef range
     (int index,				//   given index of coefficnt
     double *min,				//   rtnd minimum coef value
     double *max);				//   rtnd maximum coef value

protected:
  virtual int convertOffsetToInternal		// cnvrt to internal offset
    (int index,					//   given index of offset
     double external_coefficient,		//   givn external offset
     double *internal_coefficient);		//   rtnd internal offset

  virtual int dependentVariableModifier		// dependent variable modifier
    (float y,					//   porosity
     double *modified_y);			//   modified porosity

  virtual int dependentVariableUnmodifier	// dependent var inverse mod
    (float y,					//   porosity
     double *unmodified_y);			//   unmodified porosity

private:
  double
    _ymax;					// maximum porosity in data

  int
    _ymax_initialized;				// 0 if _ymax not initialized
};

#endif
