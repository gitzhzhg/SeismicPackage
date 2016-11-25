// base_float_array.cc:  implementation file for the BaseFloatArray class
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

#include "dp/base_float_array.hh"
#include <math.h>

#define EPSILON 1e-35

BaseFloatArray::BaseFloatArray (long size) :
  _size              (size),
  _not_defined       (-FLT_MAX),
  _set_minimum       (0),
  _set_maximum       (FLT_MAX),
  _rescale_gain      (1),
  _rescale_bias      (0),
  _index             (0),
  _array             (0),
  _error_status      (BFA_SUCCESSFUL)
{
  if (_size < 1) {
    _error_status = BFA_BAD_INPUTS;
    return;
  }

  _array = new float[_size];
  if (!_array) {
    _error_status = BFA_MEMORY_ALLOCATION_ERROR;
    return;
  }

  if (!fillAll(_set_minimum)) return;
}

BaseFloatArray::~BaseFloatArray ()
{
  if (_array) delete [] _array;
}

int BaseFloatArray::setRange (float not_defined, float set_minimum,
  float set_maximum)
{
  if (set_minimum > set_maximum      ||
      (not_defined <= set_maximum &&
       not_defined >= set_minimum   )  ) {
    _error_status = BFA_SET_RANGE_ERROR;
    return 0;
  }
  _not_defined = not_defined;  // must not change in the middle of processing
  _set_maximum = set_maximum;
  _set_minimum = set_minimum;

  return 1;
}

// override this fill member function if partial fills are required
//   complete fills and partial fills
int BaseFloatArray::fill (float value)
{
  return fillAll (value);
}

// utility to reset the value for undefined
int BaseFloatArray::resetUndefined (float new_not_defined)
{
// did the undefined value change
  if (new_not_defined == _not_defined) return 1;

// is the change valid
  if (findMinimum() > new_not_defined || findMaximum() < new_not_defined) {
    long k2;
    for (k2 = 0; k2 < _size; k2++)
      if (_array[k2] == _not_defined) _array[k2] = new_not_defined;
    _not_defined = new_not_defined;
    return 1;
  }
  else {
    _error_status = BFA_SET_RANGE_ERROR;
    return 0; // could not use new value
  }
}

// utility to rescale this grid
int BaseFloatArray::setRescaleParameters (float gain, float bias)
{
// determine if the rescaling will work with the current undefined value
  float test_max, test_min;
  float test_value1 = gain * findMinimum() + bias;
  float test_value2 = gain * findMaximum() + bias;
  if (test_value2 > test_value1) {
    test_max = test_value2;
    test_min = test_value1;
  }
  else {
    test_max = test_value1;
    test_min = test_value2;
  }
  if (test_min > _not_defined || test_max < _not_defined) {
    _rescale_gain = gain;
    _rescale_bias = bias;
    return 1; // if the new gain and bias are used, they will not corrupt data
  }
  else {
    _error_status = BFA_SET_RANGE_ERROR;
    return 0; // the new gain and bias were rejected, they'd corrupt the data
  }
}

int BaseFloatArray::computeRescaleParameters (float std_dev)
{
  float denominator;
  if (std_dev == 0)
    denominator = findMaximum() - findMinimum();
  else
   denominator=findStatisticalMaximum(std_dev)-findStatisticalMinimum(std_dev);

  if (fabs((double)denominator) < EPSILON) {
    _error_status = BFA_SET_RANGE_ERROR;
    return 0;
  }
  _rescale_gain = (_set_maximum - _set_minimum) / denominator;
  _rescale_bias = _set_minimum - _rescale_gain * findMinimum();
  return 1;
}

int BaseFloatArray::rescale ()
{
  if (!doRescale()) return 0;
  _extrema_ready = 0;
  _statistics_ready = 0;
  return 1;
}

int BaseFloatArray::rescaleWithClip ()
{
  if (!doRescaleWithClip()) return 0;

// if necessary reset the extrema ready flag
  if (_extrema_ready)
    if (fabs ((double)findMinimum() - (double)_set_minimum) > EPSILON ||
        fabs ((double)findMaximum() - (double)_set_maximum) > EPSILON   ) {
      _extrema_ready = 0;
      _statistics_ready = 0;
    }
  _error_status = BFA_SUCCESSFUL;
  return 1;
}

float BaseFloatArray::findMinimum ()
{
  if (!_extrema_ready)
    if (!findExtrema ()) return 0;
  return _actual_minimum;
}

float BaseFloatArray::findMaximum ()
{
  if (!_extrema_ready)
    if (!findExtrema ()) return 0;
  return _actual_maximum;
}

// result will be greater than or equal to actual minimum by definition
float BaseFloatArray::findStatisticalMinimum (float std_dev)
{
  float retval;
  if (std_dev < 0) std_dev = -std_dev;  // negative std dev's are invalid
  if (!_statistics_ready) findStatistics ();
  retval = _actual_average - std_dev * _actual_std_dev;
  if (retval < findMinimum())
    return _actual_minimum;            // result may not be below actual min
  else
    return retval;
}

// result will be less than or equal to actual maximum by definition
float BaseFloatArray::findStatisticalMaximum (float std_dev)
{
  float retval;
  if (std_dev < 0) std_dev = -std_dev;  // negative std dev's are invalid
  if (!_statistics_ready) findStatistics ();
  retval = _actual_average + std_dev * _actual_std_dev;
  if (retval > findMaximum())
    return _actual_maximum;            // result may not be above actual max
  else
    return retval;
}

// 0 => value is invalid (i.e. it lies on the wrong side of the _not_defined
//      value or it lies in between in "no man's land")
// 1 => value is defined and lies between current actual extrema
// 2 => value is defined and is less than current actual minumum
// 3 => value is defined and is greater than current actual maximum
// 4 => value is defined but _extrema_ready flag is false
// 5 => value is defined but _array_not_defined flag is true
// 6 => value is equal to _not_defined value
int BaseFloatArray::valueIsValid (float value) const
{
  if (value == _not_defined)
    return (int)6;

  if (_not_defined < _set_minimum) {

    if (value < _not_defined || (value < _set_minimum && value > _not_defined))
      return (int)0;
  }
  else /* if (_not_defined > _set_maximum) */ {

    if (value > _not_defined || (value > _set_maximum && value < _not_defined))
      return (int)0;
  }
  if (!_extrema_ready)
    return (int)4;

  if (_array_not_defined)
    return (int)5;

  if (value >= _actual_minimum && value <= _actual_maximum)
    return (int)1;

  if (value < _actual_minimum)
    return (int)2;
/*
  if (value > _actual_maximum) */
    return (int)3;

}

void BaseFloatArray::resetExtremaReadyFlag ()
{
  _extrema_ready = 0;
  _statistics_ready = 0;
}

int BaseFloatArray::failed ()
{
  return (int)(_error_status != BFA_SUCCESSFUL);
}

int BaseFloatArray::findExtrema ()
{
  int have_first_value = 0;
  long k2;
  for (k2 = 0; k2 < _size; k2++) {
    if (_array[k2] != _not_defined) {
      if (have_first_value) {
        if (_actual_minimum > _array[k2]) _actual_minimum = _array[k2];
        if (_actual_maximum < _array[k2]) _actual_maximum = _array[k2];
      }
      else {
        _actual_minimum = _array[k2];
        _actual_maximum = _array[k2];
        have_first_value = 1;
      }
    }
  }

  if (!have_first_value) {                        // no data defined yet
    _actual_minimum = _not_defined;
    _actual_maximum = _not_defined;
    _array_not_defined = 1;
  }

  else /* if (have_first_value) */ {
    if (_not_defined < _set_minimum) {
      if (!(_actual_minimum > _not_defined)) {   // bad data got stored
        _error_status = BFA_INVALID_ARRAY;
        return 0;
      }
    }
    else /* if (_not_defined > _set_maximum) */ {
      if (!(_actual_maximum < _not_defined)) {   // bad data got stored
        _error_status = BFA_INVALID_ARRAY;
        return 0;
      }
    }
    _array_not_defined = 0;
  }
  _extrema_ready = 1;

  return 1;
}

void BaseFloatArray::findStatistics ()
{
  double sum = 0;
  double sqr_sum = 0;
  long k2, count = 0;
  for (k2 = 0; k2 < _size; k2++) {
    if (_array[k2] != _not_defined) {
      sum += (double)_array[k2];
      sqr_sum += (double)(_array[k2] * _array[k2]);
      count++;
    }
  }

  if (!count) {                        // no data defined yet
    _actual_average = _not_defined;
    _actual_std_dev = 0;
    _array_not_defined = 1;
  }

  else /* if (count) */ {
    sum /= (double)count;
    _actual_average = (float)sum;
    _actual_std_dev = (float)(sqrt ((sqr_sum-((double)count*sum*sum))
                                    /(double)(count-1)));
    _array_not_defined = 0;
  }
  _statistics_ready = 1;
}

int BaseFloatArray::fillAll (float value)
{
// check for validity of value
  if (!valueIsValid(value)) {
    _error_status = BFA_SET_RANGE_ERROR;
    return 0;
  }

  long k2;
  for (k2 = 0; k2 < _size; k2++)
    _array[k2] = value;

  _actual_minimum = value;
  _actual_maximum = value;
  _actual_average = value;
  _actual_std_dev = 0;
  if (value != _not_defined) {
    _extrema_ready = 1;
    _statistics_ready = 1;
    _array_not_defined = 0;
  }
  else /* if (value == _not_defined) */ {
    _extrema_ready = 0;
    _statistics_ready = 0;
    _array_not_defined = 1;
  }
  return 1;
}
