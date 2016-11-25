// uchar_grid_accessor.cc:  Implementation file for UCharGridAccessor class
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

#include "dp/uchar_grid_accessor.hh"
#include "dp/uchar_grid.hh"
#include <assert.h>

// The assumed cube has its origin in the lower,left,back corner with positive
//   space as the coordinates go upward, right, & frontward. This will be the
//   case when minimums are entered less than maximums.  Otherwise,
//   coordinates can have negative bin sizes and in that case, minimums will
//   have to be entered greater than maximums where appropriate.
//   Note:  this pertains only to the coordinate system convention and
//   presupposes nothing about the organization of the cube as it is stored.

UCharGridAccessor::UCharGridAccessor ():
  _grid             (0),
  _first_xyz_point  (1),
  _x_bin_size       (0),
  _y_bin_size       (0),
  _z_bin_size       (0),
  _error_status     (UCA_SUCCESSFUL)
{
}

int UCharGridAccessor::specifyData (UCharGrid *grid)
{
  if (!grid) {
    _error_status = UCA_BAD_INPUTS;
    return 0;
  }
  _grid = grid;
  return 1;
}

// set the X-extrema given the X-coordinates of opposite edges
int UCharGridAccessor::setXBinData (float x_min_grid_edge,
  float x_max_grid_edge)
{
  if (!_grid) {
    _error_status = UCA_INITIALIZATION_ERROR;
    return 0;
  }

  _x_bin_min = x_min_grid_edge;
  _x_bin_max = x_max_grid_edge;
  if (_grid->getNumXBins() < 1) {
    _x_bin_size = 0.0;
    if (_x_bin_min != _x_bin_max) {
      _error_status = UCA_BAD_INPUTS;
      return 0;
    }
  }
  else {
    _x_bin_size = (_x_bin_max - _x_bin_min) / (float)_grid->getNumXBins();
    if (_x_bin_min == _x_bin_max) {
      _error_status = UCA_BAD_INPUTS;
      return 0;
    }
  }
  return 1;
}

// set the Y-extrema given the Y-coordinate of opposite edges
int UCharGridAccessor::setYBinData (float y_min_grid_edge,
  float y_max_grid_edge)
{
  if (!_grid) {
    _error_status = UCA_INITIALIZATION_ERROR;
    return 0;
  }

  _y_bin_min = y_min_grid_edge;
  _y_bin_max = y_max_grid_edge;
  if (_grid->getNumYBins() < 1) {
    _y_bin_size = 0.0;
    if (_y_bin_min != _y_bin_max) {
      _error_status = UCA_BAD_INPUTS;
      return 0;
    }
  }
  else {
    _y_bin_size = (y_max_grid_edge - y_min_grid_edge)
      / (float)(_grid->getNumYBins() - 1);
    _y_bin_min = y_min_grid_edge - _y_bin_size / 2.0;
    if (y_min_grid_edge == y_max_grid_edge) {
      _error_status = UCA_BAD_INPUTS;
      return 0;
    }
  }
  return 1;
}

// set the Z-extrema given the Z-coordinates of opposite edges
int UCharGridAccessor::setZBinData (float z_min_grid_edge,
  float z_max_grid_edge)
{
  if (!_grid) {
    _error_status = UCA_INITIALIZATION_ERROR;
    return 0;
  }

  _z_bin_min = z_min_grid_edge;
  _z_bin_max = z_max_grid_edge;
  if (_grid->getNumZBins() < 1) {
    _z_bin_size = 0.0;
    if (_z_bin_min != _z_bin_max) {
      _error_status = UCA_BAD_INPUTS;
      return 0;
    }
  }
  else {
    _z_bin_size = (_z_bin_max - _z_bin_min) / (float)_grid->getNumZBins();
    if (_z_bin_min == _z_bin_max) {
      _error_status = UCA_BAD_INPUTS;
      return 0;
    }
  }
  return 1;
}

// round to the nearest bin for the given X-coordinate
int UCharGridAccessor::getGridX (float x)
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
int UCharGridAccessor::getGridY (float y)
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

// round to the nearest bin for the given Z-coordinate
int UCharGridAccessor::getGridZ (float z)
{
  int zin;
  if (_z_bin_size != 0.0) {
    if (z == _z_bin_max) {
      zin = _grid->getNumZBins() - 1;
    }
    else {
      zin = (int) ((z - _z_bin_min) / _z_bin_size); 
    }
  }
  else {
    zin = 0;
  }
  return zin;
}

// return the X-coordinate at the center of the bin
float UCharGridAccessor::getX (int x)
{
  float retval = (float)x * _x_bin_size + _x_bin_min + _x_bin_size / 2;
  return retval;
}

// return the Y-coordinate at the center of the bin
float UCharGridAccessor::getY (int y)
{
  float retval = (float)y * _y_bin_size + _y_bin_min + _y_bin_size / 2;
  return retval;
}

// return the Z-coordinate at the center of the bin
float UCharGridAccessor::getZ (int z)
{
  float retval = (float)z * _z_bin_size + _z_bin_min + _z_bin_size / 2;
  return retval;
}

int UCharGridAccessor::accumulateXYZ (float value, float x, float y, float z)
{
  if (!_grid) {
    _error_status = UCA_INITIALIZATION_ERROR;
    return 0;
  }
  watchXYZ (x, y, z);
  _grid->accumulateXYZ (value, getGridX(x), getGridY(y), getGridZ(z));
  return 1;
}

int UCharGridAccessor::insertXYZ (float value, float x, float y, float z)
{
  if (!_grid) {
    _error_status = UCA_INITIALIZATION_ERROR;
    return 0;
  }
  watchXYZ (x, y, z);
  _grid->insertXYZ (value, getGridX(x), getGridY(y), getGridZ(z));
  return 1;
}

int UCharGridAccessor::columnIndexAtX (float x)
{
  if (!_grid) {
    _error_status = UCA_INITIALIZATION_ERROR;
    return -1;
  }
  else
    return _grid->columnIndexAtX (getGridX(x));
}

int UCharGridAccessor::rowIndexAtY (float y)
{
  if (!_grid) {
    _error_status = UCA_INITIALIZATION_ERROR;
    return -1;
  }
  else
    return _grid->rowIndexAtY (getGridY(y));
}

int UCharGridAccessor::planeIndexAtZ (float z)
{
  if (!_grid) {
    _error_status = UCA_INITIALIZATION_ERROR;
    return -1;
  }
  else
    return _grid->planeIndexAtZ (getGridZ(z));
}

int UCharGridAccessor::failed ()
{
  return (int) (_error_status != UCA_SUCCESSFUL);
}


void UCharGridAccessor::watchXYZ (float x, float y, float z)
{
  if (_first_xyz_point) {
    _x_min = x;
    _x_max = x;
    _y_min = y;
    _y_max = y;
    _z_min = z;
    _z_max = z;
    _first_xyz_point = 0;
  }
  else {
    if (x < _x_min) _x_min = x;
    if (x > _x_max) _x_max = x;
    if (y < _y_min) _y_min = y;
    if (y > _y_max) _y_max = y;
    if (z < _z_min) _z_min = z;
    if (z > _z_max) _z_max = z;
  }
}
