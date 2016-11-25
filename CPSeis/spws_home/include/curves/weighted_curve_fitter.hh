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
#ifndef WEIGHTED_CURVE_FITTER_HH
#define WEIGHTED_CURVE_FITTER_HH

#include "curves/curve_fitter.hh"

class WeightedCurveFitter : public CurveFitter {

public:
  WeightedCurveFitter				// constructor
    (int type = CV::LINEAR,			// type of fitter
     int logarithmic = 0);                      // is fitter logarithmic

  virtual ~WeightedCurveFitter ();		// destructor

  int doit					// replace arrays
    (float *xs,					//   independent array
     float *ys,					//   dependent array
     int count,					//   new array size
     int *counts = 0);				//   # of times to count x,y

  int remove					// remove data from sums
    (float *xs,					//   independent array
     float *ys,					//   dependent array
     int count,					//   new array size
     int *counts = 0);				//   # of times x,y counted

  int insert					// insert data to sums
    (float *xs,					//   independent array
     float *ys,					//   dependent array
     int count,					//   new array size
     int *counts = 0);				//   # of times to count x,y
  
  int isLogarithmic()                           // used in some table types
    {return _logarithmic;}

  void setLogarithmic(int set)
    {_logarithmic = set;}




private:
  void adjustRunArrays				// expand run arrays if nec
    (int count,					//   new array size
     int *counts);				//   # of times to count x,y

  int
    _max_run,					// max run of an x,y pair
    _runs_grtr_thn_1_exist,			// flag indicatng runs>1 exist
    _zero_runs_exist,				// flag indicatng runs=0 exist
    _logarithmic;                               // flag indicating logarithmic

  float
    *_x_run,					// array of x's
    *_y_run;					// array of y's

};

#endif
