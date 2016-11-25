#include "curves/ls_gain_fitter.hh"
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
#include <stdlib.h>
#include <assert.h>
#include <float.h>

LSGainFitter::LSGainFitter (int type) :
  LSLinearFitter (type)
{
}

LSGainFitter::~LSGainFitter ()
{
}

// From scratch scale the data and compute sums of the scaled data
int LSGainFitter::computeSums (float *xs, float *ys, int count)
{
  if (!xs || !ys || count <= 0) return _status = CV::NO_VALID_DATA_FOUND;

  int k2;
  double x, y;

// if (x0,y0) has not been set, then let them be zero for now
  if (!_x0_set) _x0 = 0;
  if (!_y0_set) _y0 = 0;

// find good data count
  int bad_data_count = 0;
  _count = 0;
  for (k2 = 0; k2 < count; k2++) {
    if (independentVariableModifier(xs[k2],&x) == CV::NORMAL &&
          dependentVariableModifier(ys[k2],&y) == CV::NORMAL   ) {
      _count++;
    }
    else {
      bad_data_count++;
    }
  }

  if (!_count) return _status = CV::NO_VALID_DATA_FOUND;

// no data scaling is allowed
  _x_gain = 1;
  _x_bias = 0;
  _y_gain = 1;
  _y_bias = 0;

// initialize offsets if necessary
  if (!_x0_set) {
//  _x0 = (-_x_bias) / _x_gain;
    _x0 = 0;
    _x0_set = 1;
  }
  if (!_y0_set) {
//  _y0 = (-_y_bias) / _y_gain;
    _y0 = 0;
    _y0_set = 1;
  }

// compute the sums of the scaled data
  initializeSums ();
  for (k2 = 0; k2 < _count; k2++) {
    if (independentVariableModifier(xs[k2],&x) == CV::NORMAL &&
          dependentVariableModifier(ys[k2],&y) == CV::NORMAL   ) {
      x = _x_gain * x + _x_bias;
      y = _y_gain * y + _y_bias;
      incrementSums (x, y);
    }
  }

  if (bad_data_count) return _status = CV::BAD_DATA_FOUND;
  else                return _status = CV::NORMAL;
}

// specify an unscaled coefficient directly from the user and store for use
//   immediately while later redoing the scaled coefficients. convert to
//   internal requirements as necessary.
int LSGainFitter::setCoefficient (int index, double value)
{
// if index == CV::A0, they really meant CV::A1, because there is no CV::A0
  if (index == CV::A0) index = CV::A1;
  int retval = LSLinearFitter::setCoefficient (index, value);
  return retval;
}

// from sums of scaled data, compute scaled coefficients
int LSGainFitter::doFit ()
{
  if (fabs(_sx2_sum) < FLT_EPSILON*FLT_EPSILON) {
    return _status = CV::DIVIDE_BY_ZERO;
  }

  _sas[0] = _sxy_sum  / _sx2_sum;

  return _status = CV::NORMAL;
}

// from sums of scaled data and scaled coefficients, find the GAIN error
//   statistics In terms of unscaled values
int LSGainFitter::computeErrorStatistics ()
{
// clearly, the error statistics are very dependent on any
//   scaling done on x or y
  double y_sum, y2_sum, x_sum, x2_sum, xy_sum;
  double y2_gain, x2_gain, xy_gain, a1, err_var;

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

  a1 = internalCoefficient (CV::A1);

  if (_count < 1) {
    return _status = CV::TOO_LITTLE_DATA;
  }

  _err_ave = y_sum / _count
           - a1 * x_sum  / _count;

  if (_count < 2) {
    _err_std = 0;
  }
  else {
    err_var  = (y2_sum
             - 2 * a1 * xy_sum
             + a1 * a1 * x2_sum
             - _count * _err_ave * _err_ave)
             / (_count - 1);

    _err_std = sqrt (err_var);
  }
  return _status = CV::NORMAL;
}

int LSGainFitter::coefficient (int index, double *coef)
{
// if index == CV::A0, they really meant CV::A1, because there is no CV::A0
  if (index == CV::A0) index = CV::A1;
  int retval = LSLinearFitter::coefficient (index, coef);
  return retval;
}

double LSGainFitter::internalCoefficient (int index)
{
  double retval;
  if (index == CV::A1) {
    retval = _as[0] = _sas[0] * _x_gain / _y_gain;
  }
  else {
    assert (0); // invalid coefficient index
  }
  return retval;
}

// from the stored unscaled coefficients, compute the scaled coefficients
void LSGainFitter::redoCoefficientValues ()
{
  _sas[0] = _as[0] * _y_gain / _x_gain;
}

int LSGainFitter::coefficientIndex (int index)
{
  int retval;
  if      (index == CV::A0) retval = 0; // there is only ONE coefficient a1
  else if (index == CV::A1) retval = 0;
  else                      assert (0);  // invalid coefficient index
  return retval;
}
