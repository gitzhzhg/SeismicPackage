// output_lut.cc: implementation file for the OutputLUT object
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

// in the future you may want the ability to take an InputLUT of an InputLUT of
//   an .... and let that be the resultant OutputLUT.  consider a linked list
//   of InputLUT's in that case.  similarly, you may want to do arithmetic with
//   other OutputLUT's or allow a linked list of OutputLUT's as above.

#include "dp/output_lut.hh"
#include "dp/input_lut.hh"
#include <math.h>

#define EPSILON 1e-35

OutputLUT::OutputLUT (int num_bins) :
  BaseFloatArray ((long)num_bins),
  _error_status    (OL_SUCCESSFUL)
{
}

OutputLUT::~OutputLUT ()
{
}

OutputLUT &OutputLUT::operator+=(const InputLUT *LUT)
{
  int altered_first_value = 0;
  int k2, size = (int)_size;
  for (k2 = 0; k2 < size; k2++) {
    if (_array[k2] != _not_defined) {
      if (LUT->getCode(k2) == LUT->undefinedByte())
        _array[k2] = _not_defined;
      else
        _array[k2] += LUT->getValue((int)LUT->getCode(k2));
      altered_first_value = 1;
    }
  }
  if (altered_first_value) {
    _extrema_ready = 0;
    _statistics_ready = 0;
  }
  return *this;
}

OutputLUT &OutputLUT::operator+=(float value)
{
  int altered_first_value = 0;
  int k2, size = (int)_size;
  for (k2 = 0; k2 < size; k2++) {
    if (_array[k2] != _not_defined) {
      _array[k2] += value;
      altered_first_value = 1;
    }
  }
  if (altered_first_value) {
    _extrema_ready = 0;
    _statistics_ready = 0;
  }
  return *this;
}

OutputLUT &OutputLUT::operator-=(const InputLUT *LUT)
{
  int altered_first_value = 0;
  int k2, size = (int)_size;
  for (k2 = 0; k2 < size; k2++) {
    if (_array[k2] != _not_defined) {
      if (LUT->getCode(k2) == LUT->undefinedByte())
        _array[k2] = _not_defined;
      else
        _array[k2] -= LUT->getValue((int)LUT->getCode(k2));
      altered_first_value = 1;
    }
  }
  if (altered_first_value) {
    _extrema_ready = 0;
    _statistics_ready = 0;
  }
  return *this;
}

OutputLUT &OutputLUT::operator-=(float value)
{
  int altered_first_value;
  if (value == _not_defined) {
    _error_status = OL_BAD_INPUTS;
    return *this;
  }
  else if (value == 0)
    return *this;
  else {
    int k2, size = (int)_size;
    altered_first_value = 0;
    for (k2 = 0; k2 < size; k2++) {
      if (_array[k2] != _not_defined) {
        _array[k2] -= value;
        altered_first_value = 1;
      }
    }
  }
  if (altered_first_value) {
    _extrema_ready = 0;
    _statistics_ready = 0;
  }
  return *this;
}

OutputLUT &OutputLUT::operator*=(const InputLUT *LUT)
{
  int altered_first_value = 0;
  int k2, size = (int)_size;
  for (k2 = 0; k2 < size; k2++) {
    if (_array[k2] != _not_defined) {
      if (LUT->getCode(k2) == LUT->undefinedByte())
        _array[k2] = _not_defined;
      else
        _array[k2] *= LUT->getValue((int)LUT->getCode(k2));
      altered_first_value = 1;
    }
  }
  if (altered_first_value) {
    _extrema_ready = 0;
    _statistics_ready = 0;
  }
  return *this;
}

OutputLUT &OutputLUT::operator*=(float value)
{
  int altered_first_value;
  if (value == _not_defined) {
    _error_status = OL_BAD_INPUTS;
    return *this;
  }
  else if (value == 1) // can't do value = 0 because of _not_defined's
    return *this;
  else {
    int k2, size = (int)_size;
    altered_first_value = 0;
    for (k2 = 0; k2 < size; k2++) {
      if (_array[k2] != _not_defined) {
        _array[k2] *= value;
        altered_first_value = 1;
      }
    }
  }
  if (altered_first_value) {
    _extrema_ready = 0;
    _statistics_ready = 0;
  }
  return *this;
}

OutputLUT &OutputLUT::operator/=(const InputLUT *LUT)
{
  int altered_first_value = 0;
  int k2, size = (int)_size;
  for (k2 = 0; k2 < size; k2++) {
    if (_array[k2] != _not_defined) {
      if (LUT->getCode(k2) == LUT->undefinedByte() ||
        fabs((double)LUT->getValue((int)LUT->getCode(k2))) < EPSILON)
        _array[k2] = _not_defined;
      else
        _array[k2] /= LUT->getValue((int)LUT->getCode(k2));
      altered_first_value = 1;
    }
  }
  if (altered_first_value) {
    _extrema_ready = 0;
    _statistics_ready = 0;
  }
  return *this;
}

OutputLUT &OutputLUT::operator/=(float value)
{
  int altered_first_value;
  if (value == _not_defined || fabs((double)value) < EPSILON) {
    _error_status = OL_BAD_INPUTS;
    return *this;
  }
  else if (value == 1)
    return *this;
  else {
    int k2, size = (int)_size;
    altered_first_value = 0;
    for (k2 = 0; k2 < size; k2++) {
      if (_array[k2] != _not_defined) {
        _array[k2] /= value;
        altered_first_value = 1;
      }
    }
  }
  if (altered_first_value) {
    _extrema_ready = 0;
    _statistics_ready = 0;
  }
  return *this;
}

int OutputLUT::failed ()
{
  return (int)(_error_status != OL_SUCCESSFUL || BaseFloatArray::failed());
}

GridErrorCodes OutputLUT::errorStatus ()
{
  if (BaseFloatArray::failed())
    return BaseFloatArray::errorStatus ();
  else
    return _error_status;
}

int OutputLUT::doRescale ()
{
  int k2, size = (int)_size;
  for (k2 = 0; k2 < size; k2++) {
    if (_array[k2] != _not_defined) {
      _array[k2] *= _rescale_gain;
      _array[k2] += _rescale_bias;
    }
  }
  return 1;
}

int OutputLUT::doRescaleWithClip ()
{
  int k2, size = (int)_size;
  for (k2 = 0; k2 < size; k2++) {
    if (_array[k2] != _not_defined) {
      _array[k2] *= _rescale_gain;
      _array[k2] += _rescale_bias;
      if (_array[k2] > _set_maximum)
        _array[k2] = _set_maximum;
      else if (_array[k2] < _set_minimum)
        _array[k2] = _set_minimum;
    }
  }
  return 1;
}
