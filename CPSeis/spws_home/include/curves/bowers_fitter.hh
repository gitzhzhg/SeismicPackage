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
// v' = v - v0
// s' = s - s0
// v' = a0 * s'**a1

#ifndef BOWERS_FITTER_HH
#define BOWERS_FITTER_HH

#include "curves/ls_power_law_fitter.hh"

class BowersFitter : public LSPowerLawFitter {

public:
  BowersFitter ();				// constructor

  virtual ~BowersFitter ();			// destructor

  virtual int setCoefficient			// set an unscaled coefficient
    (int index,					//   given index of coefficnt
     double coefficient);			//   given unscld xtrnl coef

  virtual int coefficient			// return an unscaled coeffcnt
    (int index,					//   given index of coefficnt
     double *coef);				//   rtnd unscaled coefficient

  virtual int coefficientRangeDefault		// return def coef range 
    (int index,					//   given index of coefficnt
     double *min,				//   rtnd minimum coef value
     double *max);				//   rtnd maximum coef value

  virtual FixedCurveConstants *curveConstants ();// return fixed curve consts

protected:
  virtual double internalCoefficient		// rtn internal coefficient
    (int index);				//   given index of coefficnt

private:
  int
    _u_set;					//   given index of coefficnt

};

#endif
