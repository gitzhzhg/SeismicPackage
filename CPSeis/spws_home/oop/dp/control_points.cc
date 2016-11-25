// control_points.cc:  Implementation file for class ControlPoints
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
#include <assert.h>
#include <stdlib.h>
#include "dp/control_points.hh"

ControlPoints::ControlPoints (long count, int num_attribs):
  _count          (count),
  _num_attribs    (num_attribs)
{
// set pointers to NULL
  _control_points = 0;
  _minimums       = 0;
  _maximums       = 0;
  _min_limits     = 0;
  _max_limits     = 0;
  _offsets        = 0;
  _increments     = 0;
  _get_indexes    = 0;
  _set_indexes    = 0;
  _ptr            = 0;
  _from_ptr       = 0;
  _to_ptr         = 0;

  if (!(count > 0 && num_attribs > 0)) {
    _error_status = CP_BAD_INPUTS;
    return;
  }
  _array_size = _num_attribs * _count;
  _control_points = (float *) calloc ((size_t)_array_size, sizeof(float));
  _minimums       = new float[_num_attribs];
  _maximums       = new float[_num_attribs];
  _min_limits     = new float[_num_attribs];
  _max_limits     = new float[_num_attribs];
  _offsets        = new long [_num_attribs];
  _increments     = new long [_num_attribs];
  _get_indexes    = new long [_num_attribs];
  _set_indexes    = new long [_num_attribs];
  if (!
    (_control_points &&
     _minimums       &&
     _maximums       &&
     _min_limits     &&
     _max_limits     &&
     _offsets        &&
     _get_indexes    &&
     _set_indexes    &&
     _increments       )) {
    _error_status = CP_MEMORY_ALLOCATION_ERROR;
    return;
  }

  for (int k2 = 0; k2 < _num_attribs; k2++) {
    _minimums[k2] = 0;
    _maximums[k2] = 0;
    _min_limits[k2] = 1;
    _max_limits[k2] = 0;
    _offsets[k2]     = (long) k2;
    _increments[k2]  = _num_attribs;
    _get_indexes[k2] = 0;
    _set_indexes[k2] = 0;
  }
  _extrema_ready = 0;
  _error_status = CP_SUCCESSFUL;
}

ControlPoints::~ControlPoints ()
{
  if (_control_points) free (_control_points);
  if (_minimums) delete [] _minimums;
  if (_maximums) delete [] _maximums;
  if (_min_limits) delete [] _min_limits;
  if (_max_limits) delete [] _max_limits;
  if (_offsets) delete [] _offsets;
  if (_increments) delete [] _increments;
  if (_get_indexes) delete [] _get_indexes;
  if (_set_indexes) delete [] _set_indexes;
  if (_ptr) delete [] _ptr;
  if (_from_ptr) delete [] _from_ptr;
  if (_to_ptr) delete [] _to_ptr;
}

int ControlPoints::findExtrema ()
{
  int k2;
  long k3;
  _ptr = new float*[_num_attribs];
  if (!_ptr) {
    _error_status = CP_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  for (k2 = 0; k2 < _num_attribs; k2++) {
    _ptr[k2] = _control_points + _offsets[k2];
    _minimums[k2] = *_ptr[k2];
    _maximums[k2] = *_ptr[k2];
  }
  for (k3 = 1; k3 < _count; k3++)
    for (k2 = 0; k2 < _num_attribs; k2++) {
      _ptr[k2] += _increments[k2];
      if (*_ptr[k2] < _minimums[k2]) _minimums[k2] = *_ptr[k2];
      if (*_ptr[k2] > _maximums[k2]) _maximums[k2] = *_ptr[k2];
    }

  _extrema_ready = 1;
  delete [] _ptr, _ptr = 0;
  _error_status = CP_SUCCESSFUL;
  return 1;
}

float ControlPoints::getMinimum (int attribute_index)
{
  int index_in;
  if (attribute_index < 0) index_in = 0;
  else if (attribute_index >= _num_attribs) index_in = _num_attribs - 1;
  else index_in = attribute_index;

  if (!_extrema_ready)
    if (!(findExtrema ())) return (float)0;
  return _minimums[index_in];
}

float ControlPoints::getMaximum (int attribute_index)
{
  int index_in;
  if (attribute_index < 0) index_in = 0;
  else if (attribute_index >= _num_attribs) index_in = _num_attribs - 1;
  else index_in = attribute_index;

  if (!_extrema_ready)
    if (!(findExtrema ())) return (float)0;
  return _maximums[index_in];
}

void ControlPoints::setExtremaLimits (int attribute_index, float minimum,
  float maximum)
{
  int index_in;
  if (attribute_index < 0) index_in = 0;
  else if (attribute_index >= _num_attribs) index_in = _num_attribs - 1;
  else index_in = attribute_index;

  if (maximum >= minimum) {
    _min_limits[index_in] = minimum;
    _max_limits[index_in] = maximum;
  }
  else {
    _min_limits[index_in] = maximum;
    _max_limits[index_in] = minimum;
  }
}

float ControlPoints::getMinimumLimit (int attribute_index)
{
  int index_in;
  if (attribute_index < 0) index_in = 0;
  else if (attribute_index >= _num_attribs) index_in = _num_attribs - 1;
  else index_in = attribute_index;

  return _min_limits[index_in];
}

float ControlPoints::getMaximumLimit (int attribute_index)
{
  int index_in;
  if (attribute_index < 0) index_in = 0;
  else if (attribute_index >= _num_attribs) index_in = _num_attribs - 1;
  else index_in = attribute_index;

  return _max_limits[index_in];
}

int ControlPoints::imposeLimits ()
{
  int k2;
  long k3, new_count = 0;
  _from_ptr = new float*[_num_attribs];
  _to_ptr   = new float*[_num_attribs];
  if (!(_from_ptr && _to_ptr)) {
    _error_status = CP_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  for (k2 = 0; k2 < _num_attribs; k2++) {
    _from_ptr[k2] = _control_points + _offsets[k2];
    _to_ptr[k2]   = _control_points + _offsets[k2];
  }
  
  int include_point;
  for (k3 = 0; k3 < _count; k3++) {
    include_point = 1;
    for (k2 = 0; k2 < _num_attribs && include_point; k2++)
      include_point = *_from_ptr[k2] >= _min_limits[k2] &&
                      *_from_ptr[k2] <= _max_limits[k2]   ;
    if (include_point) {
      if (new_count == 0) {
        for (k2 = 0; k2 < _num_attribs; k2++) {
          _minimums[k2] = *_from_ptr[k2];
          _maximums[k2] = *_from_ptr[k2];
          *_to_ptr[k2] = *_from_ptr[k2];
          _to_ptr[k2] += _increments[k2];
	}
      }
      else {
        for (k2 = 0; k2 < _num_attribs; k2++) {
          if (*_from_ptr[k2] < _minimums[k2]) _minimums[k2] = *_from_ptr[k2];
          if (*_from_ptr[k2] > _maximums[k2]) _maximums[k2] = *_from_ptr[k2];
          *_to_ptr[k2] = *_from_ptr[k2];
          _to_ptr[k2] += _increments[k2];
        }
      }
      new_count++;
    }
    for (k2 = 0; k2 < _num_attribs; k2++)
      _from_ptr[k2] += _increments[k2];
  }
  long new_array_size = new_count * _num_attribs;
  float *new_control_points = (float *) realloc (_control_points,
    (size_t)new_array_size*sizeof(float));
  if (!new_control_points) {
    _error_status = CP_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  _control_points = new_control_points;
  _extrema_ready = 1;
  delete [] _to_ptr, _to_ptr = 0;
  delete [] _from_ptr, _from_ptr = 0;
  _count = new_count;
  _array_size = new_array_size;
  _error_status = CP_SUCCESSFUL;
  return 1;
}

float ControlPoints::get (int attribute_index, long spatial_index)
{
  int index_in;
  if (attribute_index < 0) index_in = 0;
  else if (attribute_index >= _num_attribs) index_in = _num_attribs - 1;
  else index_in = attribute_index;

  long s_index_in;
  if (spatial_index < 0) s_index_in = 0;
  else if (spatial_index >= _count) s_index_in = _count - 1;
  else s_index_in = spatial_index;

  _get_indexes[index_in] = _offsets[index_in] + s_index_in
    * _increments[index_in];
  return _control_points[_get_indexes[index_in]];
}

// get the next control point attribute.  dangerous in that no checking done
// WARNING:  NO ERROR CHECKING for performance reasons
float ControlPoints::getNext (int attribute_index)
{
  _get_indexes[attribute_index] += _increments[attribute_index];
  return _control_points[_get_indexes[attribute_index]];
}

void ControlPoints::set (int attribute_index, long spatial_index, float value)
{
  int index_in;
  if (attribute_index < 0) index_in = 0;
  else if (attribute_index >= _num_attribs) index_in = _num_attribs - 1;
  else index_in = attribute_index;

  long s_index_in;
  if (spatial_index < 0) s_index_in = 0;
  else if (spatial_index >= _count) s_index_in = _count - 1;
  else s_index_in = spatial_index;

  _set_indexes[index_in] = _offsets[index_in] + s_index_in
    * _increments[index_in];
  _control_points[_set_indexes[index_in]] = value;
}

// set the next control point attribute.  dangerous in that not checking done
// WARNING:  NO ERROR CHECKING for performance reasons
void ControlPoints::setNext (int attribute_index, float value)
{
  _set_indexes[attribute_index] += _increments[attribute_index];
  _control_points[_set_indexes[attribute_index]] = value;
}

int ControlPoints::failed ()
{
  return (int) (_error_status != CP_SUCCESSFUL);
}
