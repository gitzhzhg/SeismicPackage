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
// y' = a1 * x'

#ifndef LS_GAIN_FITTER_HH
#define LS_GAIN_FITTER_HH

#include "curves/ls_linear_fitter.hh"

class LSGainFitter : public LSLinearFitter {

public:
  LSGainFitter					// constructor
    (int type = CV::GAIN);			//   given curve type

  virtual ~LSGainFitter ();			// destructor

  virtual int computeSums			// compute sums for fitting
    (float *xs,					//   independent array
     float *ys,					//   dependent array
     int count);				//   # of data vlus to remove

  virtual int setCoefficient			// set an unscaled coefficient
    (int index,					//   given index of coefficnt
     double coefficient);			//   given unscld xtrnl coef

  virtual int doFit ();				// find slope and offset

  virtual int computeErrorStatistics ();	// find error statistics

  virtual int coefficient			// return an unscaled coeffcnt
    (int index,					//   given index of coefficnt
     double *coef);				//   rtnd unscaled coefficient

protected:
  virtual double internalCoefficient		// rtn internal coefficient
    (int index);				//   given index of coefficnt

  virtual void redoCoefficientValues ();	// redo linear coefficients

  virtual int coefficientIndex			// rtn coefficient array index
    (int index);				//   given external index

  double
    _ang_sum;					// sum of angles of scaled x,y

};

#endif
