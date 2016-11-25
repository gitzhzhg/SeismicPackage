#include "curves/ls_power_law_fitter.hh"
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

#define MOST 0.9999999

LSPowerLawFitter::LSPowerLawFitter (int type) :
  LSLinearFitter (type),
  _xmin              (0),
  _ymin              (0),
  _xmin_initialized  (0),
  _ymin_initialized  (0)
{
}

LSPowerLawFitter::~LSPowerLawFitter ()
{
}

int LSPowerLawFitter::coefficientRangeDefault (int index, double *min,
  double *max)
{
  int err;
  if (index >= CV::A0 && index <= CV::A1) {
    err = CurveFitter::coefficientRangeDefault (index, min, max);
  }
  else if (index == CV::X0) {
// choose defaults so as to produce x' > 0
    double mod_xmin_m_x0 = -_x_bias / _x_gain;
    err = independentVariableUnmodifier ((float)mod_xmin_m_x0, &_xmin);
    if (err == CV::NORMAL) {
      double test_max = _xmin * MOST ;
      double prev_x0 = _x0;
      while (setCoefficient(CV::X0,test_max) != CV::NORMAL) {
	test_max *= MOST;
      }
      *max = test_max;
      *min = 0;
      _xmin_initialized = 1;
// if the previously set x0 is no longer valid, set it in the center
      if (setCoefficient(CV::X0,prev_x0) != CV::NORMAL) {
        err = setCoefficient (CV::X0, (*max+*min)/2);
      }
    }
  }
  else if (index == CV::Y0) {
// choose defaults so as to produce y' > 0
    double mod_ymin_m_y0 = -_y_bias / _y_gain;
    err = dependentVariableUnmodifier ((float)mod_ymin_m_y0, &_ymin);
    if (err == CV::NORMAL) {
      double test_max = _ymin * MOST;
      double prev_y0 = _y0;
      while (setCoefficient(CV::Y0,test_max) != CV::NORMAL) {
	test_max *= MOST;
      }
      *max = test_max;
      *min = 0;
      _ymin_initialized = 1;
// if the previously set y0 is no longer valid, set it in the center
      if (setCoefficient(CV::Y0,prev_y0) != CV::NORMAL) {
        err = setCoefficient (CV::Y0, (*max+*min)/2);
      }
    }
  }
  else {
    assert (0); // invalid coefficient index
  }
  return _status = err;
}

int LSPowerLawFitter::convertOffsetToInternal (int index,
  double external_offset, double *internal_offset)
{
  if (index == CV::X0) {
    if (external_offset >= _xmin && _xmin_initialized) {
      _status = CV::BAD_OFFSET;
    }
    else {
      _status = CV::NORMAL;
      *internal_offset = external_offset;
    }
  }
  else {
    if (external_offset >= _ymin && _ymin_initialized) {
      _status = CV::BAD_OFFSET;
    }
    else {
      _status = CV::NORMAL;
      *internal_offset = external_offset;
    }
  }
  return _status;
}

// given an unscaled coefficient in the appropriate curve's system, do 
//   conversion necessary to take it to the internal (linear) system
int LSPowerLawFitter::convertCoefficientToInternal (int index,
  double external_coefficient, double *internal_coefficient)
{
  if (index == CV::A0) {
    if (external_coefficient <= 0) return _status = CV::BAD_COEFFICIENT;
    *internal_coefficient = log (external_coefficient);
  }
  else {
    *internal_coefficient = external_coefficient;
  }
  return _status = CV::NORMAL;
}

// given an unscaled internal (linear) coefficient, do conversion necessary
//   to take it from the internal system to the appropriate curve's system
int LSPowerLawFitter::convertCoefficientFromInternal (int index,
  double internal_coefficient, double *external_coefficient)
{
  if (index == CV::A0) {
    *external_coefficient = exp (internal_coefficient);
  }
  else {
    *external_coefficient = internal_coefficient;
  }
  return _status = CV::NORMAL;
}

int LSPowerLawFitter::independentVariableModifier (float x,
  double *modified_x)
{
  double x_translated = (double)x - _x0;
  if (x_translated <= 0) return _status = CV::BAD_DATA_FOUND;

  *modified_x = log (x_translated);
  return _status = CV::NORMAL;
}

int LSPowerLawFitter::independentVariableUnmodifier (float x,
  double *unmodified_x)
{
  *unmodified_x = exp ((double)x) + _x0;
  return _status = CV::NORMAL;
}

int LSPowerLawFitter::dependentVariableModifier (float y,
  double *modified_y)
{
  double y_translated = (double)y - _y0;
  if (y_translated <= 0) return _status = CV::BAD_DATA_FOUND;

  *modified_y = log (y_translated);
  return _status = CV::NORMAL;
}

int LSPowerLawFitter::dependentVariableUnmodifier (float y,
  double *unmodified_y)
{
  *unmodified_y = exp ((double)y) + _y0;
  return _status = CV::NORMAL;
}

void LSPowerLawFitter::limitCoefficientRange (int index, double *min,
  double *max)
{
  if (index == CV::A0) {
    double tiny = FLT_EPSILON * FLT_EPSILON;
    double tinyp = 1.5 * tiny;
    if (*min < tiny) *min = tinyp;
    if (*max < tiny) *max = tinyp;
  }
}
