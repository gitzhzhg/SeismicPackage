#include "curves/fixed_curve_constants.hh"
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
#include "curves/curve_constants.hh"
#include "curves/curve_fitter.hh"
#include <assert.h>

FixedCurveConstants::FixedCurveConstants (int curve_type) :
  _curve_type              (curve_type),
  _indices_used            (0)
{
  _coef_indices = new int   [CV::coefficientCount(_curve_type)];
  _coefficients = new double[CV::coefficientCount(_curve_type)];
}

FixedCurveConstants::~FixedCurveConstants ()
{
  delete [] _coef_indices;
  delete [] _coefficients;
}

int FixedCurveConstants::setConstant (int coef_index, double coef)
{
  int retval, k2, index = -1;
  if (_indices_used) {
// has this coefficient already been stored?
    for (k2 = 0; index != -1, k2 < _indices_used; k2++) {
      if (coef_index == _coef_indices[k2]) index = k2;
    }
  }

  if (index > -1) {
// redefine the coefficient already stored
    _coefficients[index] = coef;
    retval = CV::NORMAL;
  }
  else {
// define the new coefficient
    if (_indices_used < CV::coefficientCount(_curve_type)) {
      _coef_indices[_indices_used  ] = coef_index;
      _coefficients[_indices_used++] = coef      ;
      retval = CV::NORMAL;
    }
    else {
      retval = CV::BAD_DATA_FOUND;
    }
  }
  return retval;
}

int FixedCurveConstants::getConstant (int index, int *coef_index,
  double *coef)
{
  if (index < 0 || index > _indices_used) return CV::BAD_DATA_FOUND;
  *coef_index = _coef_indices[index];
  *coef       = _coefficients[index];
  return CV::NORMAL;
}

int FixedCurveConstants::reinstateConstants (CurveFitter *curve_fitter)
{
  if (!curve_fitter) return CV::TOO_LITTLE_DATA;

  int retval, k2;
  for (k2 = 0; k2 < _indices_used; k2++) {
    retval = curve_fitter->setCoefficient (_coef_indices[k2],
      _coefficients[k2]);
    if (retval != CV::NORMAL) return retval;
  }
  return retval;
}
