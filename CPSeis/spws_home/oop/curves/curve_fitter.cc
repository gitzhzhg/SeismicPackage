#include "curves/curve_fitter.hh"
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
#include "curves/fixed_curve_constants.hh"
#include "named_constants.h"
#include <stdlib.h>
#include <math.h>
#include <float.h>
#include <assert.h>

/*
#define PI (4. * atan (1))
*/

#define NUM_COLUMNS 5
#define DATA_TYPE   "Curve Data"
/*
#define CODES       "AB"
#define COLUMNS     "14"
#define CONVERT     "11"
*/




TableData::TableData ()
/*
    : SeveralFloatArrays (NUM_COLUMNS, DATA_TYPE, CODES, COLUMNS, CONVERT,
                           (const char *) NULL, 1.0e-5F, 0)
*/
    : SeveralFloatArrays (NUM_COLUMNS, DATA_TYPE, 1.0e-5F)
{
}



CurveFitter::CurveFitter (int type) :
  _type       (type),
  _as         (0),
  _sas        (0),
  _x0_set     (0),
  _y0_set     (0),
  _status     (CV::NORMAL),
  _x_bias     (0),
  _y_bias     (0),
  _x_gain     (1),
  _y_gain     (1)
{
  int num_coefficients = CV::coefficientCount (_type);
  _as  = (double *)malloc (sizeof(double)*(size_t)num_coefficients);
  _sas = (double *)malloc (sizeof(double)*(size_t)num_coefficients);
  assert (_as && _sas);
  _data = NULL;
}

CurveFitter::~CurveFitter ()
{
  if (_as)  free (_as),  _as  = 0;
  if (_sas) free (_sas), _sas = 0;
}

int CurveFitter::doit (float *xs, float *ys, int count)
{
  if(_type == CV::TABLE)
    {
    for(long i = 0; i < count; i++)
      {
      _data->setOrAppendValue(1, i, xs[i]);
      _data->setOrAppendValue(2, i, ys[i]);  
      }
    return CV::NORMAL;
    }

  int compute_sums_err = computeSums (xs, ys, count); 
  if (compute_sums_err == CV::NO_VALID_DATA_FOUND) {
    return _status = compute_sums_err;
  }

  int do_fit_err = doFit ();
  if (do_fit_err == CV::TOO_LITTLE_DATA) return _status = do_fit_err;

  int stat_err = computeErrorStatistics ();
  int meas_err = computeOtherMeasures ();

  if (do_fit_err            != CV::NORMAL) return _status = do_fit_err;
  else if (compute_sums_err != CV::NORMAL) return _status = compute_sums_err;
  else if (stat_err         != CV::NORMAL) return _status = stat_err;
  else if (meas_err         != CV::NORMAL) return _status = meas_err;
  else                                     return _status = CV::NORMAL;
}

// scale the given data and adjust the current scaled sums down
//   caution:  this will potentially nullify the goals of the scaling done
//   to the data
// if invalid data is being removed, this function doesn't remove any
//   data and returns CV::NORMAL
// if too much data is removed, this function returns an error
int CurveFitter::remove (float *xs, float *ys, int count)
{
  if (!xs || !ys || count <= 0) return _status = CV::NORMAL;
  
// adjust the current sums of the scaled data down
  double x, y;
  int k2;

  for (k2 = 0; k2 < count && _count; k2++) {
    if (independentVariableModifier(xs[k2],&x) == CV::NORMAL &&
          dependentVariableModifier(ys[k2],&y) == CV::NORMAL   ) {
      x = _x_gain * x + _x_bias;
      y = _y_gain * y + _y_bias;
      decrementSums (x, y);
      _count--;
    }
  }
  if (!_count) return _status = CV::NO_VALID_DATA_FOUND;
  int do_fit_err = doFit ();
  if (do_fit_err == CV::TOO_LITTLE_DATA) return _status = do_fit_err;

  int stat_err = computeErrorStatistics ();
  int meas_err = computeOtherMeasures ();

  if (do_fit_err    != CV::NORMAL) return _status = do_fit_err;
  else if (stat_err != CV::NORMAL) return _status = stat_err;
  else if (meas_err != CV::NORMAL) return _status = meas_err;
  else                             return _status = CV::NORMAL;
}

// scale the given data and adjust the current scaled sums up
//   caution:  this will potentially nullify the goals of the scaling done
//   to the data
// if invalid data is being inserted, this function doesn't insert any and
//   returns CV::NORMAL
// if too little data is available, this function returns an error
int CurveFitter::insert (float *xs, float *ys, int count)
{
  if (!xs || ! ys || count <= 0) return _status = CV::NORMAL;

// adjust the current sums of the scaled data up
  double x, y;
  int k2;
  int bad_data_count = 0;

  for (k2 = 0; k2 < count; k2++) {
    if (independentVariableModifier(xs[k2],&x) == CV::NORMAL &&
          dependentVariableModifier(ys[k2],&y) == CV::NORMAL   ) {
      x = _x_gain * x + _x_bias;
      y = _y_gain * y + _y_bias;
      incrementSums (x, y);
      _count++;
    }
    else {
      bad_data_count++;
    }
  }

  if (!_count) return _status = CV::NO_VALID_DATA_FOUND;
  int do_fit_err = doFit ();
  if (do_fit_err == CV::TOO_LITTLE_DATA) return _status = do_fit_err;

  int stat_err = computeErrorStatistics ();
  int meas_err = computeOtherMeasures ();

  if (do_fit_err    != CV::NORMAL) return _status = do_fit_err;
  else if (stat_err != CV::NORMAL) return _status = stat_err;
  else if (meas_err != CV::NORMAL) return _status = meas_err;
  else if (bad_data_count)         return _status = CV::BAD_DATA_FOUND;
  else                             return _status = CV::NORMAL;
}

// From scratch scale the data and compute sums of the scaled data
int CurveFitter::computeSums (float *xs, float *ys, int count)
{
  if (!xs || !ys || count <= 0) return _status = CV::NO_VALID_DATA_FOUND;

  int k2;
  double x_sum, y_sum, max_x, min_x, max_y, min_y;
  double x, y;

// if (x0,y0) has not been set, then let them be zero for now
  if (!_x0_set) _x0 = 0;
  if (!_y0_set) _y0 = 0;

// find average and extrema of unscaled data
  int bad_data_count = 0;
  _count = 0;
  for (k2 = 0; k2 < count; k2++) {
    if (independentVariableModifier(xs[k2],&x) == CV::NORMAL &&
          dependentVariableModifier(ys[k2],&y) == CV::NORMAL   ) {
      if (!_count) {
        max_x = x;
        min_x = x;
        max_y = y;
        min_y = y;
        x_sum = x;
        y_sum = y;
      }
      else {
        if (x > max_x) max_x = x;
        if (x < min_x) min_x = x;
        if (y > max_y) max_y = y;
        if (y < min_y) min_y = y;
        x_sum += x;
        y_sum += y;
      }
      _count++;
    }
    else {
      bad_data_count++;
    }
  }

  if (!_count) return _status = CV::NO_VALID_DATA_FOUND;

// scale the internal sums so data making up the sums will average to
//   0.0 and maximum absolute value will be 1.0
  double tiny = FLT_EPSILON * FLT_EPSILON;
  double ave = x_sum / (double)_count;
  if (fabs(max_x-ave) < fabs(min_x-ave)) {
    if (fabs(min_x-ave) < tiny) _x_gain = 1;
    else                        _x_gain = (double)1 / fabs(min_x-ave);
  }
  else {
    if (fabs(max_x-ave) < tiny) _x_gain = 1;
    else                        _x_gain = 1 / fabs(max_x-ave);
  }
  _x_bias = -_x_gain * ave;

  ave = y_sum / (double)_count;
  if (fabs(max_y-ave) < fabs(min_y-ave)) {
    if (fabs(min_y-ave) < tiny) _y_gain = 1;
    else                        _y_gain = 1 / fabs(min_y-ave);
  }
  else {
    if (fabs(max_y-ave) < tiny) _y_gain = 1;
    else                        _y_gain = 1 / fabs(max_y-ave);
  }
  _y_bias = -_y_gain * ave;

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
  for (k2 = 0; k2 < count; k2++) {
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
int CurveFitter::setCoefficient (int index, double value)
{
  int err;
  if (index == CV::X0) {
    err = convertOffsetToInternal (index, value, &value);
    if (err == CV::NORMAL) {
      _x0 = value;
      _x0_set = 1;
    }
  }
  else if (index == CV::Y0) {
    err = convertOffsetToInternal (index, value, &value);
    if (err == CV::NORMAL) {
      _y0 = value;
      _y0_set = 1;
    }
  }
  else if (index >= CV::A0 && index <= CV::maximumCoefficientIndex(_type)) {
    err = convertCoefficientToInternal (index, value, &value);
    if (err == CV::NORMAL) _as[coefficientIndex(index)] = value;
  }
  else {
    assert (0); // invalid coefficient index
  }
  return _status = err;
}

// from the unscaled user specified coefficients, recompute the scaled
//   coefficients and store them
int CurveFitter::redoCoefficients ()
{
  redoCoefficientValues ();
  int stat_err = computeErrorStatistics ();
  int meas_err = computeOtherMeasures ();
  if (stat_err      != CV::NORMAL) return _status = stat_err;
  else if (meas_err != CV::NORMAL) return _status = meas_err;
  else                             return _status = CV::NORMAL;
}

int CurveFitter::computeOtherMeasures ()
{
  return _status = CV::NORMAL;
}

int CurveFitter::coefficient (int index, double *coef)
{
  int err;
  if (index == CV::X0) {
    if (!_x0_set) {
//    setCoefficient (CV::X0, (-_x_bias)/_x_gain);
      setCoefficient (CV::X0, 0);
    }
    err = convertOffsetFromInternal (index, _x0, coef);
  }
  else if (index == CV::Y0) {
    if (!_y0_set) {
//    setCoefficient (CV::Y0, (-_y_bias)/_y_gain);
      setCoefficient (CV::Y0, 0);
    }
    err = convertOffsetFromInternal (index, _y0, coef);
  }
  else if (index >= CV::A0 && index <= CV::maximumCoefficientIndex(_type)) {
    double dcoef = internalCoefficient (index);
    err = convertCoefficientFromInternal (index, dcoef, coef);
  }
  else {
    assert (0); // invalid coefficient index
  }
  return _status = err;
}

int CurveFitter::coefficientRangeDefault (int index, double *min,
  double *max)
{
  assert (index > -1);

  double mn, mx;
  int err;

// make the offset ranges equal to the minimum and maximum of the data 
  if (index == CV::X0) {
    mx = ( 1 - _x_bias) / _x_gain;
    mn = (-1 - _x_bias) / _x_gain;
    if (!_x0_set) {
//    err = setCoefficient (CV::X0, (-_x_bias)/_x_gain);
      err = setCoefficient (CV::X0, 0);
    }
    else {
      err = CV::NORMAL;
    }
    if (mx < _x0) mx = _x0;
    if (mn > _x0) mn = _x0;
  }
  else if (index == CV::Y0) {
    mx = ( 1 - _y_bias) / _y_gain;
    mn = (-1 - _y_bias) / _y_gain;
    if (!_y0_set) {
//    err = setCoefficient (CV::Y0, (-_y_bias)/_y_gain);
      err = setCoefficient (CV::Y0, 0);
    }
    else {
      err = CV::NORMAL;
    }
    if (mx < _y0) mx = _y0;
    if (mn > _y0) mn = _y0;
  }
  else if (index >= CV::A0 && index <= CV::A1) {
    double tiny = FLT_EPSILON * FLT_EPSILON;
    double coef;
    err = coefficient (index, &coef);
    if (err == CV::NORMAL) {
      if (fabs(coef) < tiny) coef = 1;
      if (index == CV::A0) {
// zeroth order polynomial coefficient or offsets assumed
        mx = coef + 1.5 * fabs(coef);
        mn = coef - 1.5 * fabs(coef);
      }
      else if (index == CV::A1) {
// first order polynomial coefficient assumed
        mx = coef + 2.5 * fabs(coef);
        mn = coef - 2.5 * fabs(coef);
      }
    }
  }
  else {
    assert (0); // invalid coefficient index
  }

  *min = mn;
  *max = mx;

  limitCoefficientRange (index, min, max);

  return _status = err;
}

int CurveFitter::convertOffsetToInternal (int /* index */,
  double external_offset, double *internal_offset)
{
  *internal_offset = external_offset;
  return _status = CV::NORMAL;
}

int CurveFitter::convertOffsetFromInternal (int /* index */,
  double internal_offset, double *external_offset)
{
  *external_offset = internal_offset;
  return _status = CV::NORMAL;
}

int CurveFitter::convertCoefficientToInternal (int /* index */,
  double external_coefficient, double *internal_coefficient)
{
  *internal_coefficient = external_coefficient;
  return _status = CV::NORMAL;
}

int CurveFitter::convertCoefficientFromInternal (int /* index */,
  double internal_coefficient, double *external_coefficient)
{
  *external_coefficient = internal_coefficient;
  return _status = CV::NORMAL;
}

FixedCurveConstants *CurveFitter::curveConstants ()
{
  FixedCurveConstants *retval = new FixedCurveConstants (_type);
  double constant;
  coefficient (CV::X0, &constant);
  int err = retval->setConstant (CV::X0, constant);
  if (err != CV::NORMAL) {
    delete retval, retval = 0;
  }
  else {
    coefficient (CV::Y0, &constant);
    err = retval->setConstant (CV::Y0, constant);
    if (err != CV::NORMAL) {
      delete retval, retval = 0;
    }
  }
  return retval;
}

// data is modified in unscaled form
int CurveFitter::independentVariableModifier (float x, double *dx)
{
  *dx = (double)x - _x0;
  return _status = CV::NORMAL;
}

// data is unmodified in unscaled form
int CurveFitter::independentVariableUnmodifier (float x, double *dx)
{
  *dx = (double)x + _x0;
  return _status = CV::NORMAL;
}

// data is modified in unscaled form
int CurveFitter::dependentVariableModifier (float y, double *dy)
{
  *dy = (double)y - _y0;
  return _status = CV::NORMAL;
}

// data is unmodified in unscaled form
int CurveFitter::dependentVariableUnmodifier (float y, double *dy)
{
  *dy = (double)y + _y0;
  return _status = CV::NORMAL;
}

int CurveFitter::coefficientIndex (int index)
{
  int retval;
  if      (index == CV::A0) retval = 0;
  else if (index == CV::A1) retval = 1;
  else if (index == CV::A2) retval = 2;
  else                      assert (0);  // invalid coefficient index
  return retval;
}
