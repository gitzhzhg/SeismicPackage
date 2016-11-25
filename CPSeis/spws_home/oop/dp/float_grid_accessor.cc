// float_grid_accessor.cc:  Implementation file for FloatGridAccessor class
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

#include "dp/float_grid_accessor.hh"
#include "dp/float_grid.hh"
#include <assert.h>

// The assumed plane has its origin in the lower,left corner with positive
//   plane as the coordinates go upward & right. This will be the
//   case when minimums are entered less than maximums.  Otherwise,
//   coordinates can have negative bin sizes and in that case, minimums will
//   have to be entered greater than maximums where appropriate.
//   Note:  this pertains only to the coordinate system convention and
//   presupposes nothing about the organization of the plane as it is stored.

FloatGridAccessor::FloatGridAccessor () :
  _grid          (0),
  _x_bin_size    (0),
  _y_bin_size    (0),
  _error_status  (FA_SUCCESSFUL)
{
}

FloatGridAccessor::~FloatGridAccessor ()
{
}

int FloatGridAccessor::specifyData (FloatGrid *grid)
{
  if (!grid) {
    _error_status = FA_BAD_INPUTS;
    return 0;
  }
  _grid = grid;
  return 1;
}

// set the X-extrema given the X-coordinates of opposite edges
int FloatGridAccessor::setXBinData (float x_min_grid_edge,
  float x_max_grid_edge)
{
  if (!_grid) {
    _error_status = FA_INITIALIZATION_ERROR;
    return 0;
  }

  _x_bin_min = x_min_grid_edge;
  _x_bin_max = x_max_grid_edge;
  if (_grid->getNumXBins() < 1) {
    _x_bin_size = 0.0;
    if (_x_bin_min != _x_bin_max) {
      _error_status = FA_BAD_INPUTS;
      return 0;
    }
  }
  else {
    _x_bin_size = (_x_bin_max - _x_bin_min) / (float)_grid->getNumXBins();
    if (_x_bin_min == _x_bin_max) {
      _error_status = FA_BAD_INPUTS;
      return 0;
    }
  }
  return 1;
}

// set the Y-extrema given the Y-coordinates of opposite edges
int FloatGridAccessor::setYBinData (float y_min_grid_edge,
  float y_max_grid_edge)
{
  if (!_grid) {
    _error_status = FA_INITIALIZATION_ERROR;
    return 0;
  }

  _y_bin_min = y_min_grid_edge;
  _y_bin_max = y_max_grid_edge;
  if (_grid->getNumYBins() < 1) {
    _y_bin_size = 0.0;
    if (_y_bin_min != _y_bin_max) {
      _error_status = FA_BAD_INPUTS;
      return 0;
    }
  }
  else {
    _y_bin_size = (_y_bin_max - _y_bin_min) / (float)_grid->getNumYBins();
    if (_y_bin_min == _y_bin_max) {
      _error_status = FA_BAD_INPUTS;
      return 0;
    }
  }
  return 1;
}

// round to the nearest bin for the given X-coordinate
int FloatGridAccessor::getGridX (float x)
{
  int xin;
  if (_x_bin_size != 0.0) {
    if (x == _x_bin_max) {
      xin = _grid->getNumXBins() - 1;
    }
    else {
      xin = (int) ((x - _x_bin_min) / _x_bin_size);
    }
  }
  else {
    xin = 0;
  }
  return xin;
}

// round to the nearest bin for the given Y-coordinate
int FloatGridAccessor::getGridY (float y)
{
  int yin;
  if (_y_bin_size != 0.0) {
    if (y == _y_bin_max) {
      yin = _grid->getNumYBins() - 1;
    }
    else {
      yin = (int) ((y - _y_bin_min) / _y_bin_size);
    }
  }
  else {
    yin = 0;
  }
  return yin;
}

// return the length in # of bins represented by the given X-range
float FloatGridAccessor::getGridXLength (float x1, float x0)
{
  if (_x_bin_size != 0.0) {
    float retval = (x1 - x0) / _x_bin_size;
    if (retval < 0) return -retval;
    return retval;
  }
  else return (float)0;
}

// return the length in # of bins represented by the given Y-range
float FloatGridAccessor::getGridYLength (float y1, float y0)
{
  if (_y_bin_size != 0.0) {
    float retval = (y1 - y0) / _y_bin_size;
    if (retval < 0) return -retval;
    return retval;
  }
  else return (float)0;
}

// return the X-coordinate at the center of the bin
float FloatGridAccessor::getX (int x)
{
  float retval = (float)x * _x_bin_size + _x_bin_min + _x_bin_size / 2;
  return retval;
}

// return the Y-coordinate at the center of the bin
float FloatGridAccessor::getY (int y)
{
  float retval = (float)y * _y_bin_size + _y_bin_min + _y_bin_size / 2;
  return retval;
}

int FloatGridAccessor::failed ()
{
  return (int) (_error_status != FA_SUCCESSFUL);
}
