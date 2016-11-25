#include "curves/ls_quadratic_fitter.hh"
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
#include <math.h>
#include <float.h>
#include <assert.h>

LSQuadraticFitter::LSQuadraticFitter () :
  WeightedCurveFitter (CV::QUADRATIC)
{
}

LSQuadraticFitter::~LSQuadraticFitter ()
{
}

// from sums of scaled data, compute scaled coefficients
int LSQuadraticFitter::doFit ()
{
  double det;
  det = _count   * (_sx2_sum * _sx4_sum - _sx3_sum * _sx3_sum)
      - _sx_sum  * (_sx_sum  * _sx4_sum - _sx3_sum * _sx2_sum)
      + _sx2_sum * (_sx_sum  * _sx3_sum - _sx2_sum * _sx2_sum);

  if (fabs(det) < FLT_EPSILON*FLT_EPSILON) {
    return _status = CV::DIVIDE_BY_ZERO;
  }

  _sas[0] = ( _sy_sum   * (_sx2_sum * _sx4_sum - _sx3_sum * _sx3_sum)
            + _sxy_sum  * (_sx2_sum * _sx3_sum - _sx_sum  * _sx4_sum)
            + _sx2y_sum * (_sx_sum  * _sx3_sum - _sx2_sum * _sx2_sum))
          / det;

  _sas[1] = ( _sy_sum   * (_sx3_sum * _sx2_sum - _sx_sum  * _sx4_sum)
            + _sxy_sum  * (_count   * _sx4_sum - _sx2_sum * _sx2_sum)
            + _sx2y_sum * (_sx2_sum * _sx_sum  - _count   * _sx3_sum))
          / det;

  _sas[2] = ( _sy_sum   * (_sx_sum  * _sx3_sum - _sx2_sum * _sx2_sum)
            + _sxy_sum  * (_sx_sum  * _sx2_sum - _count   * _sx3_sum)
            + _sx2y_sum * (_count   * _sx2_sum - _sx_sum  * _sx_sum ))
          / det;

  return _status = CV::NORMAL;
}

// from sums of scaled data and scaled coefficients, find the error statistics
//   in terms of unscaled values
int LSQuadraticFitter::computeErrorStatistics ()
{
// clearly, the error statistics are very dependent on any
//   scaling done on x or y
  double y_sum, y2_sum, x_sum, x2_sum, xy_sum, x2y_sum, x3_sum, x4_sum;
  double y2_gain, x2_gain, xy_gain, x2y_gain, x3_gain, x4_gain, a2, a1, a0;
  double err_var;

  y_sum    = _sy_sum / _y_gain - _y_bias / _y_gain * _count;

  y2_gain  = _y_gain * _y_gain;
  y2_sum   = _sy2_sum / y2_gain
           - 2 * _y_bias / y2_gain * _sy_sum
           + _y_bias * _y_bias / y2_gain * _count;

  x_sum    = _sx_sum / _x_gain - _x_bias / _x_gain * _count;

  x2_gain  = _x_gain * _x_gain;
  x2_sum   = _sx2_sum / x2_gain
           - 2 * _x_bias / x2_gain * _sx_sum
           + _x_bias * _x_bias / x2_gain * _count;

  xy_gain  = _y_gain * _x_gain;
  xy_sum   = _sxy_sum / xy_gain
           - _y_bias / xy_gain * _sx_sum
           - _x_bias / xy_gain * _sy_sum
           + _y_bias * _x_bias / xy_gain * _count;

  x2y_gain = _y_gain * _x_gain * _x_gain;
  x2y_sum  = _sx2y_sum / x2y_gain
           - 2 * _x_bias / x2y_gain * _sxy_sum
           - _y_bias / x2y_gain * _sx2_sum
           + 2 * _y_bias * _x_bias / x2y_gain * _sx_sum
           + _x_bias * _x_bias / x2y_gain * _sy_sum
           - _y_bias * _x_bias * _x_bias / x2y_gain * _count;

  x3_gain  = _x_gain * _x_gain * _x_gain;
  x3_sum   = _sx3_sum / x3_gain
           - 3 * _x_bias / x3_gain * _sx2_sum
           + 3 * _x_bias * _x_bias / x3_gain * _sx_sum
           - _x_bias * _x_bias * _x_bias / x3_gain * _count;

  x4_gain  = _x_gain * _x_gain * _x_gain * _x_gain;
  x4_sum   = _sx4_sum / x4_gain
           - 4 * _x_bias / x4_gain * _sx3_sum
           + 6 * _x_bias * _x_bias / x4_gain * _sx2_sum
           - 4 * _x_bias * _x_bias * _x_bias / x4_gain * _sx_sum
           + _x_bias * _x_bias * _x_bias * _x_bias / x4_gain * _count;

  a2 = internalCoefficient (CV::A2);

  a1 = internalCoefficient (CV::A1);

  a0 = internalCoefficient (CV::A0);

  if (_count < 1) {
    return _status = CV::TOO_LITTLE_DATA;
  }
  _err_ave = y_sum / _count
           - a2 * x2_sum / _count
           - a1 * x_sum  / _count
           - a0;


  if (_count < 2) {
    _err_std = 0;
  }
  else {
    err_var  = (y2_sum
             - 2 * a2 * x2y_sum
             - 2 * a1 * xy_sum
             - 2 * a0 * y_sum
             + a2 * a2 * x4_sum
             + 2 * a2 * a1 * x3_sum
             + (2 * a2 * a0 + a1 * a1) * x2_sum
             + 2 * a1 * a0 * x_sum
             + a0 * a0 * _count
             - _count * _err_ave * _err_ave)
             / (_count - 1);

    _err_std = sqrt (err_var);
  }
  return _status = CV::NORMAL;
}

int LSQuadraticFitter::coefficientRangeDefault (int index,
  double *min, double *max)
{
  int err;

  if (index != CV::A2) {
    err = WeightedCurveFitter::coefficientRangeDefault (index, min, max);
  }
  else if (index == CV::A2) {
// Let the range be +/- 5% of optimal
    double tiny = FLT_EPSILON * FLT_EPSILON;
    double coef;
    err = coefficient (index, &coef);
    if (err == CV::NORMAL) {
      if (fabs(coef) < tiny) coef = 1; 
      *max = coef + 0.5 * fabs(coef);
      *min = coef - 0.5 * fabs(coef);
    }
  }
  return _status = err;
}

double LSQuadraticFitter::internalCoefficient (int index)
{
  assert (index >= CV::A0 && index <= CV::A2);

  double retval;
  if (index == CV::A2) {
    retval = _as[2] = _sas[2] * _x_gain * _x_gain / _y_gain;
  }
  else if (index == CV::A1) {
    retval = _as[1]
      = (_sas[1] * _x_gain + 2 * _sas[2] * _x_gain * _x_bias) / _y_gain;
  }
  else if (index == CV::A0) {
    retval = _as[0]
      = (_sas[2] * _x_bias * _x_bias + _sas[1] * _x_bias + _sas[0]
      - _y_bias) / _y_gain;
  }
  return retval;
}

void LSQuadraticFitter::initializeSums ()
{
  _sx_sum   = 0;
  _sy_sum   = 0;
  _sx2_sum  = 0;
  _sxy_sum  = 0;
  _sy2_sum  = 0;
  _sx2y_sum = 0;
  _sx3_sum  = 0;
  _sx4_sum  = 0;
}

void LSQuadraticFitter::incrementSums (double x, double y)
{
  _sx_sum   += x;
  _sy_sum   += y;
  _sx2_sum  += x * x;
  _sxy_sum  += x * y;
  _sy2_sum  += y * y;
  _sx2y_sum += x * x * y;
  _sx3_sum  += x * x * x;
  _sx4_sum  += x * x * x * x;
}

void LSQuadraticFitter::decrementSums (double x, double y)
{
  _sx_sum   -= x;
  _sy_sum   -= y;
  _sx2_sum  -= x * x;
  _sxy_sum  -= x * y;
  _sy2_sum  -= y * y;
  _sx2y_sum -= x * x * y;
  _sx3_sum  -= x * x * x;
  _sx4_sum  -= x * x * x * x;
}

// from the stored unscaled coefficients, compute the scaled coefficients
void LSQuadraticFitter::redoCoefficientValues ()
{
  _sas[2] = _as[2] * _y_gain / _x_gain / _x_gain;
  _sas[1] = (_as[1] * _y_gain - 2 * _sas[2] * _x_gain * _x_bias) / _x_gain;
  _sas[0] = _as[0] * _y_gain - _sas[2] * _x_bias * _x_bias - _sas[1] * _x_bias
    +_y_bias;
}
