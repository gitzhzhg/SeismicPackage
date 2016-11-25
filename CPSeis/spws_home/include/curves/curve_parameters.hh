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
#ifndef CURVE_PARAMETERS_HH
#define CURVE_PARAMETERS_HH

#include <assert.h>

#define DEF_STEPS 100

class CurveFitter;

class CurveParameters {

public:
  CurveParameters ();				// constructor

  ~CurveParameters ();				// destructor

  int setFitter					// set the fitter
    (CurveFitter *fitter);			//   given curve fitter

  int getCoefficientRanges ();			// get coef rng vlus frm fittr

  void setCoefficientRange			// set coefficient range
    (int index,					//   given coefficient index
     double min,				//   unscaled coefficient min
     double max);				//   unscaled coefficient max

  double coefficientMinimum			// rtn coefficient minimum
    (int index);				//   given coefficient index

  double coefficientMaximum			// rtn coefficient maximum
    (int index);				//   given coefficient index

  void setCoefficientRangeSteps			// set # of coef range steps
    (int steps = DEF_STEPS)			//   given # of steps in rng
    { assert (steps > 0); _steps = steps; }

  int coefficientRangeSteps ()			// rtn # of coef range steps
    { return _steps; }

  double coefficientRangeIncrement		// rtn coefficient range inc
    (int index);				//   given coefficient index

private:
  void initMinsAndMaxs ();			// init _mins and _maxs arrays

  CurveFitter
    *_fitter;					// curve fitter object

  int
    *_user,					// flags = 1 if user spcd rngs
    _steps;					// # of steps in range

  double
    *_mins,					// unscaled coefficient mins
    *_maxs;					// unscaled coefficient maxs

};

#endif
