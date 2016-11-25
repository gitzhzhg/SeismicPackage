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
// e**y' = a0 * x'**a1

#ifndef LS_LOGARITHMIC_FITTER_HH
#define LS_LOGARITHMIC_FITTER_HH

#include "curves/ls_linear_fitter.hh"

class LSLogarithmicFitter : public LSLinearFitter {

public:
  LSLogarithmicFitter ();			// constructor

  virtual ~LSLogarithmicFitter ();		// destructor

  virtual int convertCoefficientToInternal	// cnvrt to unscld intrnl coef
    (int index,					//   given index of coefficnt
     double external_coefficient,		//   givn unscaled extrnl coef
     double *internal_coefficient);		//   rtnd unscaled intrnl coef

  virtual int convertCoefficientFromInternal	// cnvrt frm unscld ntrnl coef
    (int index,					//   given index of coefficnt
     double internal_coefficient,		//   gvn ntrnl unscld coef vlu
     double *external_coefficient);		//   rtnd xtrnl unscld coef vl

protected:
  virtual int independentVariableModifier	// indepndnt variable modifier
    (float x,					//   independent value
     double *modified_x);			//   modified independent valu

  virtual int independentVariableUnmodifier	// indepndnt var inverse mod
    (float x,					//   independent value
     double *unmodified_x);			//   unmodified independent vl

  virtual void limitCoefficientRange		// return limited coef range
    (int index,					//   given index of coefficnt
     double *min,				//   rtnd minimum coef value
     double *max);				//   rtnd maximum coef value

};

#endif
