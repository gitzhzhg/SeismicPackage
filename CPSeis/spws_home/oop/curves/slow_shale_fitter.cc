#include "curves/slow_shale_fitter.hh"
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
#include "curves/fixed_curve_constants.hh"
#include <math.h>
#include <float.h>
#include <assert.h>

SlowShaleFitter::SlowShaleFitter () :
  LSPowerLawFitter (CV::SLOW_SHALE),
  _ymax              (0),
  _ymax_initialized  (0)
{
}

SlowShaleFitter::~SlowShaleFitter ()
{
}

int SlowShaleFitter::coefficientRangeDefault (int index, double *min,
  double *max)
{
  int err;
  if (index == CV::X0 || (index >= CV::A0 && index <= CV::A1)) {
    err = LSPowerLawFitter::coefficientRangeDefault (index, min, max);
  }
  else if (index == CV::Y0) {
// y0 (p0) must be constrained for all y (p) such that
//   y0 > y
    double mod_log_log_y0_by_y = (1 - _y_bias) / _y_gain;
    err = dependentVariableUnmodifier ((float)mod_log_log_y0_by_y, &_ymax);
    if (err == CV::NORMAL) {
      double tiny = FLT_EPSILON * FLT_EPSILON;
// make sure that the range is valid!!!
      double test_min = _ymax + tiny;
      double prev_y0 = _y0;
      while (setCoefficient(CV::Y0,test_min) != CV::NORMAL) {
        test_min += tiny;
      }
      *min = test_min;
      *max = 100 * _ymax; // 100 is an arbitray multiplying factor
      *max = (*max >= prev_y0) ? *max : prev_y0; // at least big as _y0
      _ymax_initialized = 1;
// if the previously set y0 is no longer valid, set it in the center
      if (setCoefficient(CV::Y0,prev_y0) != CV::NORMAL) {
        setCoefficient (CV::Y0, (*max+*min)/2);
      }
    }
  }
  return _status = err;
}

int SlowShaleFitter::convertOffsetToInternal (int index,
  double external_offset, double *internal_offset)
{
  if (index == CV::X0) {
    LSPowerLawFitter::convertOffsetToInternal (index, external_offset,
      internal_offset);
  }
  else {
    if (external_offset <= 0 || (_y0 <= _ymax && _ymax_initialized)) {
      _status = CV::BAD_OFFSET;
    }
    else {
      *internal_offset = external_offset;
      _status = CV::NORMAL;
    }
  }
  return _status;
}

// y is porosity
int SlowShaleFitter::dependentVariableModifier (float y,
  double *modified_y)
{
  if (y <= 0) return _status = CV::BAD_DATA_FOUND;

  double y_translated = log (_y0 / (double)y);
  if (y_translated <= 0) return _status = CV::BAD_DATA_FOUND;

  *modified_y = log (y_translated);
  return _status;
}

// y is porosity
int SlowShaleFitter::dependentVariableUnmodifier (float y,
  double *unmodified_y)
{
  *unmodified_y = _y0 / exp (exp((double)y));
  return _status = CV::NORMAL;
}
