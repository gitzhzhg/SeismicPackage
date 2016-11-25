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
#ifndef FIXED_CURVE_CONSTANTS_HH
#define FIXED_CURVE_CONSTANTS_HH

class CurveFitter;

class FixedCurveConstants {

public:
  FixedCurveConstants				// constructor
    (int curve_type);				//   given curve type

  virtual ~FixedCurveConstants ();		// destructor

  int curveType ()				// rtn curve type
    { return _curve_type; }

  int constantCount ()				// rtn # of fixed constants
    { return _indices_used; }

  int setConstant				// set the constant info
    (int coef_index,				//   which fitter coefficient
     double coef);				//   coefficient value

  int getConstant				// return constant info
    (int index,					//   which fixed constant
     int *coef_index,				//   which fitter coefficient
     double *coef);				//   coefficient value

  int reinstateConstants			// reinstate constants
    (CurveFitter *curve_fitter);		//   given a curve fitter

private:
  int
    _curve_type,				// curve type
    _indices_used,				// # of fixed constants set
    *_coef_indices;				// coefficient indices on fitr

  double
    *_coefficients;				// values of coefficients
};

#endif
