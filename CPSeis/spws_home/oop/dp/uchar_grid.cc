// uchar_grid.cc:  Implementation file for class UCharGrid
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
#include <limits.h>
#include <float.h>
#include <math.h>
#include "dp/uchar_grid.hh"
#include "sp/do_abort.hh"

#define EPSILON 1e-35


// The X-bins, Y-bins, and Z-bins of the grid are organized as follows:
//   The (x_bin,y_bin,z_bin) = (0,0,0) of the grid is in the top left corner
//   of the front plane.
//   Progression through the grid goes first from top to bottom (y) and
//   then from left to right (x) and then from front to back (z).
// The Xs, Ys, and Zs of the grid are as follows:
//   The (x,y,z) = (0,0,0) of the grid is in the lower left corner of the
//   back plane
// Note the XYZ-bin coordinate system reflects the way that the data is stored.
//   The XYZ coordinate system reflects the normal RH cartesian coordinate
//   system.
// Y-varying most rapidly pointing down was selected for historical reasons.
//   X-varying second most rapidly pointing to the right was also inherited.
//   Z-varying third most rapidly pointing from front to back was chosen so
//   that in the case that this class is used to store multi-attribute data
//   for analysis, the Y-variable could be used for the attribute dimension,
//   (varying most rapidly) and then the Z-X plane could be treated analogously
//   to the previous X-Y plane.  (i.e. with Y being used for the attribute
//   dimenision, now Z is like the previous X in direction and order and
//   similarly, X is like the previous Y in direction and order.) 

UCharGrid::UCharGrid (int num_x_bins, int num_y_bins, int num_z_bins,
  DoAbort *do_abort):
     _num_x_bins        (num_x_bins),
     _num_y_bins        (num_y_bins),
     _num_z_bins        (num_z_bins),
     _do_abort          (do_abort)
{
// set array pointers to NULL
  _grid = 0;

  if (!(num_x_bins > 0 && num_y_bins > 0 && num_z_bins > 0)) {
    _error_status = UCG_SIZE_TOO_SMALL;
    return;
  }

  _grid = new unsigned char[_num_x_bins*_num_y_bins*_num_z_bins];
  if (!_grid) {
    _error_status = UCG_MEMORY_ALLOCATION_ERROR;
    return;
  }

  _sub_start_x = 0;
  _sub_start_y = 0;
  _sub_start_z = 0;
  _sub_x_bins = _num_x_bins;
  _sub_y_bins = _num_y_bins;
  _sub_z_bins = _num_z_bins;
  _not_defined = 255;
  _set_minimum = 0;
  _set_maximum = 254;
  _actual_minimum = _set_minimum;
  _actual_maximum = _set_minimum;
  _actual_average = _set_minimum;
  _actual_std_dev = _set_minimum;

  _extrema_ready = 0;
  if (!(fill ((float)_set_minimum))) return;
  _extrema_ready = 1;
  _statistics_ready = 1;
  _grid_not_defined = 0;
  _x_first_insert = 0;
  _x_last_insert  = 0;
  _y_first_insert = 0;
  _y_last_insert  = 0;
  _z_first_insert = 0;
  _z_last_insert  = 0;
  _multi_insert = 0;
  _error_status = UCG_SUCCESSFUL;
}

// copy constructor
UCharGrid::UCharGrid (const UCharGrid &from)
{
// copy the from object to this object
  copyHelper (from);
}

UCharGrid::~UCharGrid ()
{
  if (_grid) delete [] _grid, _grid = 0;
}

void UCharGrid::setInsertSize (int x_insert_size, int y_insert_size,
  int z_insert_size)
{
  int x_ins_size = x_insert_size < 1 ? 1 : x_insert_size;
  int y_ins_size = y_insert_size < 1 ? 1 : y_insert_size;
  int z_ins_size = z_insert_size < 1 ? 1 : z_insert_size;
      x_ins_size = x_insert_size > _num_x_bins ? _num_x_bins : x_insert_size;
      y_ins_size = y_insert_size > _num_y_bins ? _num_y_bins : y_insert_size;
      z_ins_size = z_insert_size > _num_z_bins ? _num_z_bins : z_insert_size;
  _x_first_insert = x_ins_size / 2;
  _x_last_insert  = x_ins_size - _x_first_insert - 1;
  _y_first_insert = y_ins_size / 2;
  _y_last_insert  = y_ins_size - _y_first_insert - 1;
  _z_first_insert = z_ins_size / 2;
  _z_last_insert  = z_ins_size - _z_first_insert - 1;
  if (x_insert_size > 1 || y_insert_size > 1 || z_insert_size > 1)
    _multi_insert = 1;
  else
    _multi_insert = 0;
}

int UCharGrid::setRange (unsigned char not_defined, unsigned char set_minimum,
  unsigned char set_maximum)
{
  if (!(set_minimum <= set_maximum  &&
        (not_defined > set_maximum  ||
        not_defined < set_minimum)    )){
    _error_status = UCG_SET_RANGE_ERROR;
    return 0;
  }

  _not_defined = not_defined;  // DO NOT CHANGE in the middle of processing
  _set_maximum = set_maximum;
  _set_minimum = set_minimum;

  _error_status = UCG_SUCCESSFUL;
  return 1;
}

int UCharGrid::setSubSize (int sub_x_bins, int sub_y_bins, int sub_z_bins)
{
  if (!(sub_x_bins > 0 &&
        sub_y_bins > 0 &&
        sub_z_bins > 0   )) {
    _error_status = UCG_SIZE_TOO_SMALL;
    return 0;
  }

  _sub_x_bins = sub_x_bins;
  _sub_y_bins = sub_y_bins;
  _sub_z_bins = sub_z_bins;

  _error_status = UCG_SUCCESSFUL;
  return 1;
}

// set the starting x coordinate and starting y coordinate
int UCharGrid::setSubStart (int sub_start_x, int sub_start_y, int sub_start_z)
{
  if (!
    (sub_start_x >= 0           &&
     sub_start_y >= 0           &&
     sub_start_z >= 0           &&
     sub_start_x <  _num_x_bins &&
     sub_start_y <  _num_y_bins &&
     sub_start_z <  _num_z_bins   )) {
    _error_status = UCG_SIZE_TOO_SMALL;
    return 0;
  }

  _sub_start_x = sub_start_x;
  _sub_start_y = sub_start_y;
  _sub_start_z = sub_start_z;

  _error_status = UCG_SUCCESSFUL;
  return 1;
}

// set the ending x,y,z coordinates
int UCharGrid::setSubEnd (int sub_end_x, int sub_end_y, int sub_end_z)
{
  if (!
    (sub_end_x >= 0           &&
     sub_end_y >= 0           &&
     sub_end_z >= 0           &&
     sub_end_x <  _num_x_bins &&
     sub_end_y <  _num_y_bins &&
     sub_end_z <  _num_z_bins   )) {
    _error_status = UCG_SIZE_TOO_SMALL;
    return 0;
  }

// make sure that the ending coordinates are larger than the starting coords
  int temp;
  if (sub_end_x < _sub_start_x) {
    temp = _sub_start_x;
    _sub_start_x = sub_end_x;
    sub_end_x = temp;
  }
  if (sub_end_y < _sub_start_y) {
    temp = _sub_start_y;
    _sub_start_y = sub_end_y;
    sub_end_y = temp;
  }
  if (sub_end_z < _sub_start_z) {
    temp = _sub_start_z;
    _sub_start_z = sub_end_z;
    sub_end_z = temp;
  }

// do not store the end points, use the starting points with this input to
//   determine the subregion size
  return setSubSize (sub_end_x-_sub_start_x+1, sub_end_y-_sub_start_y+1,
    sub_end_z-_sub_start_z+1);
}

// set the starting x bin (column) and starting y bin (row) and
//   starting z bin (plane)
int UCharGrid::setSubBinStart (int sub_start_x_bin, int sub_start_y_bin,
  int sub_start_z_bin)
{
// translate the starting sub (row,col,plane) to reflect the (x,y,z)
//   organization
  int sub_start_y = _num_y_bins - _sub_y_bins - sub_start_y_bin;
  int sub_start_z = _num_z_bins - _sub_z_bins - sub_start_z_bin;
// Store the starting point in (x,y,z), not (row,col,plane) because
//   if the sub region is modified, the (row,col,plane) origin should
//   change, not the (x,y,z) origin.
  return setSubStart (sub_start_x_bin, sub_start_y, sub_start_z);
}

// get the starting row index of the sub region
int UCharGrid::getSubYBinStart () const
{
  return _num_y_bins - _sub_y_bins - _sub_start_y;
}

// get the starting plane index of the sub region
int UCharGrid::getSubZBinStart () const
{
  return _num_z_bins - _sub_z_bins - _sub_start_z;
}

int UCharGrid::move (UCharGrid *to) const
{

// if this is to then do nothing
  if (this == to) {
    to->_error_status = UCG_SUCCESSFUL;
    return 1;
  }

// check for compatibility between this and to
  if (!
    (_not_defined == to->getUndefined() &&
     _set_minimum >= to->getMinimum()   &&
     _set_maximum <= to->getMaximum()     )) {
    to->_error_status = UCG_SET_RANGE_ERROR;
    return 0;
  }

// ONLY equate to this SUB area
  if (!(process (&UCharGrid::valueAtIndex, to))) return 0;

  to->_error_status = UCG_SUCCESSFUL;
  return 1;
}

int UCharGrid::fill (float value)
{

// check validity of value
  if (!
         ((_not_defined < _set_minimum && value >= _not_defined) ||
          (_not_defined > _set_maximum && value <= _not_defined) &&
          valueIsValid (value + 0.5)                               )) {
    _error_status = UCG_SET_RANGE_ERROR;
    return 0;
  }

// fill the value
  if (!(process (&UCharGrid::constantValue, value+0.5))) return 0;

// adjust internal values
  if (completelyUsed() && completelyCoveredBy(this)) {
    _actual_minimum = (unsigned char) (value + 0.5);
    _actual_maximum = (unsigned char) (value + 0.5);
    _actual_average = value;
    _actual_std_dev = 0;
    if (value != _not_defined) {
      _extrema_ready = 1;
      _statistics_ready = 1;
      _grid_not_defined = 0;
    }
    else /* if (value == _not_defined) */ {
      _extrema_ready = 0;
      _statistics_ready = 0;
      _grid_not_defined = 1;
    }
  }

// only a partial fill was done, adjust actual extrema if appropriate
  else if (_extrema_ready && !_grid_not_defined) {
    if (value != _not_defined) {
      if (value < _actual_minimum)
        _actual_minimum = (unsigned char)value;
      if (value > _actual_maximum)
        _actual_maximum = (unsigned char)value;
    }
  }

  _error_status = UCG_SUCCESSFUL;
  return 1;
}

// determine how many undefined grids you have in the entire grid
long UCharGrid::countUndefined ()
{
  long retval = 0;
  for (long k2 = 0; k2 < getArraySize(); k2++) {
    if (_grid[k2] == _not_defined) retval++;
    if (_do_abort) {
      if (!(k2 % (long)2000)) {
	if (_do_abort->userAbort()) {
          _error_status = UCG_USER_ABORTED;
          return (long)0;
        }
      }
    }
  }
  return retval;
}

// utility to reset the value for undefined
int UCharGrid::resetUndefined (unsigned char new_not_defined)
{
// did the undefined value change
  if (new_not_defined == _not_defined) {
    _error_status = UCG_SUCCESSFUL;
    return 1;
  }

// is the change valid
  if (findMinimum() > new_not_defined || findMaximum() < new_not_defined) {
    for (long k2 = 0; k2 < getArraySize(); k2++) {
      if (_grid[k2] == _not_defined)
        _grid[k2] = new_not_defined;
      if (_do_abort) {
        if (!(k2 % (long)2000)) {
	  if (_do_abort->userAbort()) {
            _error_status = UCG_USER_ABORTED;
            return 0;
          }
        }
      }
    }
    _not_defined = new_not_defined;
    _error_status = UCG_SUCCESSFUL;
    return 1;
  }
  else {
    _error_status = UCG_SET_RANGE_ERROR;
    return 0; // could not use new value
  }
}

unsigned char UCharGrid::findMinimum ()
{
  if (!_extrema_ready)
    if (!findExtrema ()) return 0;
  return _actual_minimum;
}

unsigned char UCharGrid::findMaximum ()
{
  if (!_extrema_ready)
    if (!findExtrema ()) return 0;
  return _actual_maximum;
}

// result will be greater than or equal to actual minimum by definition
float UCharGrid::findStatisticalMinimum (float std_dev)
{
  float retval;
  if (std_dev < 0) std_dev = -std_dev;  // negative std dev's are invalid
  if (!_statistics_ready) findStatistics ();
  retval = _actual_average - std_dev * _actual_std_dev;
  if (retval < (float)findMinimum())
    return (float)_actual_minimum;      // result will not be below actual min
  else
    return retval;
}

// result will be less than or equal to actual maximum by definition
float UCharGrid::findStatisticalMaximum (float std_dev)
{
  float retval;
  if (std_dev < 0) std_dev = -std_dev;  // negative std dev's are invalid
  if (!_statistics_ready) findStatistics ();
  retval = _actual_average + std_dev * _actual_std_dev;
  if (retval > (float)findMaximum())
    return (float)_actual_maximum;      // result will not be above actual max
  else
    return retval;
}

// Find the X value for a given index.  The index must be a physically
//   accessible bin, otherwise it is clipped to be so.
int UCharGrid::getX (long index) const
{
// clip index

  long index_in;
  if (index < 0) index_in = 0;
  else if (index >= getArraySize()) index_in = getArraySize() - 1;
  else index_in = index;

  long z = index_in / (long)_num_y_bins / (long)_num_x_bins;
  int retval = (int) ((index_in - z * (long)_num_x_bins * (long)_num_y_bins)
    / (long)_num_y_bins);
  return retval;
}

// Find the X offset given an offset.
int UCharGrid::getXOffset (long offset) const
{
  int x0, y0, z0;
  if (offset < 0) {
    x0 = _num_x_bins - 1;
    y0 = _num_y_bins - 1;
    z0 = _num_z_bins - 1;
  }
  else /* if (offset >= 0) */ {
    x0 = 0;
    y0 = 0;
    z0 = 0;
  }
  int x = getX (getIndex (x0, y0, z0) + offset);
  return x - x0;
}

// Find the Y value for a given index.  The index must be a physically
//   accessible bin, otherwise it is clipped to be so.
int UCharGrid::getY (long index) const
{
// clip index

  long index_in;
  if (index < 0) index_in = 0;
  else if (index >= getArraySize()) index_in = getArraySize() - 1;
  else index_in = index;

  long z = index_in / (long)_num_y_bins / (long)_num_x_bins;
  long x = (index_in - z * (long)_num_x_bins * (long)_num_y_bins)
    / (long)_num_y_bins;
  int retval = (int) (index_in - z * (long)_num_x_bins * (long)_num_y_bins
    - x * (long)_num_y_bins);
  retval = _num_y_bins - 1 - retval;  // negate Y due to (x,y,z) organization
  return retval;
}

// Find the Y offset given an offset.
int UCharGrid::getYOffset (long offset) const
{
  int x0, y0, z0;
  if (offset < 0) {
    x0 = _num_x_bins - 1;
    y0 = _num_y_bins - 1;
    z0 = _num_z_bins - 1;
  }
  else /* if (offset >= 0) */ {
    x0 = 0;
    y0 = 0;
    z0 = 0;
  }
  int y = getY (getIndex (x0, y0, z0) + offset);
  return y0 - y;  // negate Y due to (x,y,z) organization
}

// Find the Z value for a given index.  The index must be a physically
//   accessible bin, otherwise it is clipped to be so.
int UCharGrid::getZ (long index) const
{
// clip index

  long index_in;
  if (index < 0) index_in = 0;
  else if (index >= getArraySize()) index_in = getArraySize() - 1;
  else index_in = index;

  int retval = (int) (index_in / (long)_num_y_bins / (long)_num_x_bins);
  retval = _num_z_bins - 1 - retval;  // negate Z due to (x,y,z) organization
  return retval;
}

// Find the Z offset given an offset.
int UCharGrid::getZOffset (long offset) const
{
  int x0, y0, z0;
  if (offset < 0) {
    x0 = _num_x_bins - 1;
    y0 = _num_y_bins - 1;
    z0 = _num_z_bins - 1;
  }
  else /* if (offset >= 0) */ {
    x0 = 0;
    y0 = 0;
    z0 = 0;
  }
  int z = getZ (getIndex (x0, y0, z0) + offset);
  return z0 - z;  // negate Z due to (x,y,z) organization
}

// Find an array index for a given X,Y,Z triplet.  The X,Y,Z triplet
//   must be a physically accessible bin, otherwise, it is clipped to be so.
long UCharGrid::getIndex (int x, int y, int z) const
{

// clip invalid data

  int xin;
  if (x < 0) xin = 0;
  else if (x >= _num_x_bins) xin = _num_x_bins - 1;
  else xin = x;

  int yin;
  if (y < 0) yin = 0;
  else if (y >= _num_y_bins) yin = _num_y_bins - 1;
  else yin = y;

  int zin;
  if (z < 0) zin = 0;
  else if (z >= _num_z_bins) zin = _num_z_bins - 1;
  else zin = z;

  int y_n = _num_y_bins - 1 - yin;  // negate Y due to organization
  int z_n = _num_z_bins - 1 - zin;  // negate Z due to organization
  long retval = (long)z_n * (long)_num_x_bins * (long)_num_y_bins +
    (long)xin * (long)_num_y_bins + (long)y_n;
  return retval;
}

long UCharGrid::getOffset (int x_offset, int y_offset, int z_offset) const
{
  long retval = (long)(-z_offset) * (long)_num_x_bins * (long)_num_y_bins
    + (long)x_offset * (long)_num_y_bins - (long)y_offset; // note -Z & -Y
  return retval;
}

int UCharGrid::columnIndexAtX (int x) const
{

// clip invalid data

  int xin;
  if (x < 0) xin = 0;
  else if (x >= _num_x_bins) xin = _num_x_bins - 1;
  else xin = x;

  return xin;
}

int UCharGrid::rowIndexAtY (int y) const
{

// clip invalid data

  int yin;
  if (y < 0) yin = 0;
  else if (y >= _num_y_bins) yin = _num_y_bins - 1;
  else yin = y;

  return _num_y_bins - 1 - yin;  // negate Y due to organization
}

int UCharGrid::planeIndexAtZ (int z) const
{

// clip invalid data

  int zin;
  if (z < 0) zin = 0;
  else if (z >= _num_z_bins) zin = _num_z_bins - 1;
  else zin = z;

  return _num_z_bins - 1 - zin;  // negate Z due to organization
}

int UCharGrid::valueIsInRange (float value) const
{
  if (value <= -1 || value >= 256)
    return (int)0;
  else
    return (int)1;
}

// 0 => value is invalid (i.e. it lies on the wrong side of the _not_defined
//      value or lies outside the valid range for an unsigned char
// 1 => value is defined and lies between current actual extrema
// 2 => value is defined and is less than current actual minumum
// 3 => value is defined and is greater than current actual maximum
// 4 => value is defined but _extrema_ready flag is false
// 5 => value is defined but _grid_not_defined flag is true
// 6 => value is equal to _not_defined value
int UCharGrid::valueIsValid (float value) const
{
  if (!valueIsInRange(value)) return 0;

  unsigned char uc_value = (unsigned char)(value + 0.5);

  if (uc_value == _not_defined)
    return (int)6;

  if (_not_defined < _set_minimum) {

    if (uc_value < _not_defined)
      return (int)0;
  }
  else /* if (_not_defined > _set_maximum) */ {

    if (uc_value > _not_defined)
      return (int)0;
  }
  if (!_extrema_ready)
    return (int)4;

  if (_grid_not_defined)
    return (int)5;

  if (uc_value >= _actual_minimum && uc_value <= _actual_maximum)
    return (int)1;

  if (uc_value < _actual_minimum)
    return (int)2;
/*
  if (uc_value > _actual_maximum) */
    return (int)3;

}

void UCharGrid::resetExtremaReadyFlag ()
{
  _extrema_ready = 0;
  _statistics_ready = 0;
}

// using this without resetting _extrema_ready flag will lead to corrupted grid
int UCharGrid::accumulateXYZ (float value, int x, int y, int z)
{
  unsigned char uc_value = (unsigned char) (value + 0.5);
  if (_multi_insert) {
// using getIndex is not efficient but boundary checking is done
    long tlF_index = getIndex (x-_x_first_insert, y+_y_last_insert,
      z+_z_last_insert);
    long blF_index = getIndex (x-_x_first_insert, y-_y_first_insert,
      z+_z_last_insert);
    long trF_index = getIndex (x+_x_last_insert,  y+_y_last_insert,
      z+_z_last_insert);
    long tlB_index = getIndex (x-_x_first_insert, y+_y_last_insert,
      z-_z_first_insert);
    long index_y_rng = blF_index - tlF_index;// indx cnting begins at top-left
    long index_x_rng = trF_index - tlF_index;// indx cnting begins at left
    long i_index, e_index;
    long num_xy_bins = (long)_num_x_bins * (long)_num_y_bins;

// proceed from Front to Back because indx cnting begins at top-left-Front
    long count = 0;
    for (long b_index = tlF_index; b_index <= tlB_index; b_index+=num_xy_bins){
      i_index = b_index + index_x_rng;
      for (long mindex = b_index; mindex <= i_index; mindex+=_num_y_bins) {
         e_index = mindex + index_y_rng;
         for (long index = mindex; index <= e_index; index++) {
           _grid[index] += uc_value;
           if (_do_abort) {
             count++;
             if (!(count % (long)2000)) {
	       if (_do_abort->userAbort()) {
                 _error_status = UCG_USER_ABORTED;
                 return 0;
               }
             }
           }
         }
      }
    }
  }
  else
    _grid[getIndex(x,y,z)] += uc_value;
  return 1;
}

// using this without resetting _extrema_ready flag will lead to corrupted grid
int UCharGrid::accumulateIndex (long index, float value)
{
  unsigned char uc_value = (unsigned char) (value + 0.5);
  if (_multi_insert) {
    int x = getX (index);
    int y = getY (index);
    int z = getZ (index);
// using getIndex is not efficient but boundary checking is done
    long tlF_index = getIndex (x-_x_first_insert, y+_y_last_insert,
      z+_z_last_insert);
    long blF_index = getIndex (x-_x_first_insert, y-_y_first_insert,
      z+_z_last_insert);
    long trF_index = getIndex (x+_x_last_insert,  y+_y_last_insert,
      z+_z_last_insert);
    long tlB_index = getIndex (x-_x_first_insert, y+_y_last_insert,
      z-_z_first_insert);
    long index_y_rng = blF_index - tlF_index;// indx cnting begins at top-left
    long index_x_rng = trF_index - tlF_index;// indx cnting begins at left
    long i_index, e_index;
    long num_xy_bins = (long)_num_x_bins * (long)_num_y_bins;

// proceed from Front to Back because indx cnting begins at top-left-Front
    long count = 0;
    for (long b_index = tlF_index; b_index <= tlB_index; b_index+=num_xy_bins){
      i_index = b_index + index_x_rng;
      for (long mindex = b_index; mindex <= i_index; mindex+=_num_y_bins) {
         e_index = mindex + index_y_rng;
         for (long index = mindex; index <= e_index; index++) {
           _grid[index] += uc_value;
           if (_do_abort) {
             count++;
             if (!(count % (long)2000)) {
	       if (_do_abort->userAbort()) {
                 _error_status = UCG_USER_ABORTED;
                 return 0;
               }
             }
           }
         }
      }
    }
  }
  else
    _grid[index] += uc_value;
  return 1;
}

// using this without resetting _extrema_ready flag will lead to corrupted grid
int UCharGrid::insertXYZ (float value, int x, int y, int z)
{
  unsigned char uc_value = (unsigned char) (value + 0.5);
  if (_multi_insert) {
// using getIndex is not efficient but boundary checking is done
    long tlF_index = getIndex (x-_x_first_insert, y+_y_last_insert,
      z+_z_last_insert);
    long blF_index = getIndex (x-_x_first_insert, y-_y_first_insert,
      z+_z_last_insert);
    long trF_index = getIndex (x+_x_last_insert,  y+_y_last_insert,
      z+_z_last_insert);
    long tlB_index = getIndex (x-_x_first_insert, y+_y_last_insert,
      z-_z_first_insert);
    long index_y_rng = blF_index - tlF_index;// indx cnting begins at top-left
    long index_x_rng = trF_index - tlF_index;// indx cnting begins at left
    long i_index, e_index;
    long num_xy_bins = (long)_num_x_bins * (long)_num_y_bins;

// proceed from Front to Back because indx cnting begins at top-left-Front
    long count = 0;
    for (long b_index = tlF_index; b_index <= tlB_index; b_index+=num_xy_bins){
      i_index = b_index + index_x_rng;
      for (long mindex = b_index; mindex <= i_index; mindex+=_num_y_bins) {
         e_index = mindex + index_y_rng;
         for (long index = mindex; index <= e_index; index++) {
           _grid[index] = uc_value;
           if (_do_abort) {
             count++;
             if (!(count % (long)2000)) {
	       if (_do_abort->userAbort()) {
                 _error_status = UCG_USER_ABORTED;
                 return 0;
               }
             }
           }
         }
      }
    }
  }
  else
    _grid[getIndex(x,y,z)] = uc_value;
  return 1;
}

// using this without resetting _extrema_ready flag will lead to corrupted grid
int UCharGrid::insertIndex (long index, float value)
{
  unsigned char uc_value = (unsigned char) (value + 0.5);
  if (_multi_insert) {
    int x = getX (index);
    int y = getY (index);
    int z = getZ (index);
// using getIndex is not efficient but boundary checking is done
    long tlF_index = getIndex (x-_x_first_insert, y+_y_last_insert,
      z+_z_last_insert);
    long blF_index = getIndex (x-_x_first_insert, y-_y_first_insert,
      z+_z_last_insert);
    long trF_index = getIndex (x+_x_last_insert,  y+_y_last_insert,
      z+_z_last_insert);
    long tlB_index = getIndex (x-_x_first_insert, y+_y_last_insert,
      z-_z_first_insert);
    long index_y_rng = blF_index - tlF_index;// indx cnting begins at top-left
    long index_x_rng = trF_index - tlF_index;// indx cnting begins at left
    long i_index, e_index;
    long num_xy_bins = (long)_num_x_bins * (long)_num_y_bins;

// proceed from Front to Back because indx cnting begins at top-left-Front
    long count = 0;
    for (long b_index = tlF_index; b_index <= tlB_index; b_index+=num_xy_bins){
      i_index = b_index + index_x_rng;
      for (long mindex = b_index; mindex <= i_index; mindex+=_num_y_bins) {
        e_index = mindex + index_y_rng;
        for (long index = mindex; index <= e_index; index++) {
          _grid[index] = uc_value;
          if (_do_abort) {
            count++;
            if (!(count % (long)2000)) {
	      if (_do_abort->userAbort()) {
                _error_status = UCG_USER_ABORTED;
                return 0;
              }
            }
          }
        }
      }
    }
  }
  else
    _grid[index] = uc_value;
  return 1;
}

// overloaded assignment operator:  *this = value
UCharGrid &UCharGrid::operator=(float value)
{
  fill (value);  // assign constant value to this
  return *this;
}

// overloaded self sum operator:  *this += value
UCharGrid &UCharGrid::operator+=(float value)
{
  if (value == _not_defined) {
    _error_status = UCG_UNDEFINED_ARITHMETIC;
    return *this;
  }

  if (!(process (&UCharGrid::addValue, value))) return *this;
  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = UCG_SUCCESSFUL;
  return *this;
}

// overloaded self product operator:  *this *= value
UCharGrid &UCharGrid::operator*=(float value)
{
  if (value == _not_defined) {
    _error_status = UCG_UNDEFINED_ARITHMETIC;
    return *this;
  }

  if (!(process (&UCharGrid::multiplyValue, value))) return *this;
  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = UCG_SUCCESSFUL;
  return *this;
}

// overloaded self quotient operator:  *this /= value
UCharGrid &UCharGrid::operator/=(float value)
{
  if (value == _not_defined || value == 0.0) {
    _error_status = UCG_UNDEFINED_ARITHMETIC;
    return *this;
  }

  if (!(process (&UCharGrid::divideValue, value))) return *this;

  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = UCG_SUCCESSFUL;
  return *this;
}

// overloaded self difference operator: *this -= value
UCharGrid &UCharGrid::operator-=(float value)
{
  if (value == _not_defined) {
    _error_status = UCG_UNDEFINED_ARITHMETIC;
    return *this;
  }
  if (!(process (&UCharGrid::subtractValue, value))) return *this;

  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = UCG_SUCCESSFUL;
  return *this;
}

int UCharGrid::failed ()
{
  return (int) (_error_status != UCG_SUCCESSFUL);
}

int UCharGrid::findExtrema ()
{
  int have_first_value = 0;
  for (long k2 = 0; k2 < getArraySize(); k2++) {
    if (_grid[k2] != _not_defined) {
      if (have_first_value) {
        if (_actual_minimum > _grid[k2]) _actual_minimum = _grid[k2];
        if (_actual_maximum < _grid[k2]) _actual_maximum = _grid[k2];
      }
      else {
        _actual_minimum = _grid[k2];
        _actual_maximum = _grid[k2];
        have_first_value = 1;
      }
    }
    if (_do_abort) {
      if (!(k2 % (long)2000)) {
	if (_do_abort->userAbort()) {
          _error_status = UCG_USER_ABORTED;
          return 0;
        }
      }
    }
  }

  if (!have_first_value) {                        // no grid data defined yet
    _actual_minimum = _not_defined;
    _actual_maximum = _not_defined;
    _grid_not_defined = 1;
  }

  else /* if (have_first_value) */ {
    if (_not_defined < _set_minimum) {
      if (!(_actual_minimum > _not_defined)) {   // bad grid data got stored
        _error_status = UCG_INVALID_GRID;
        return 0;
      }
    }
    else /* if (_not_defined > _set_maximum) */ {
      if (!(_actual_maximum < _not_defined)) {   // bad grid data got stored
        _error_status = UCG_INVALID_GRID;
        return 0;
      }
    }
    _grid_not_defined = 0;
  }
  _extrema_ready = 1;

  _error_status = UCG_SUCCESSFUL;
  return 1;
}

int UCharGrid::findStatistics ()
{
  double sum = 0;
  double sqr_sum = 0;
  long count = getArraySize ();
  for (long k2 = 0; k2 < count; k2++) {
    if (_grid[k2] != _not_defined) {
      sum += (double)_grid[k2];
      sqr_sum += (double)_grid[k2] * (double)_grid[k2];
    }
    if (_do_abort) {
      if (!(k2 % (long)2000)) {
	if (_do_abort->userAbort()) {
          _error_status = UCG_USER_ABORTED;
          return 0;
        }
      }
    }
  }

  if (sum == 0) {                        // no grid data defined yet
    _actual_average = (float)_not_defined;
    _actual_std_dev = (float)0;
    _grid_not_defined = 1;
  }

  else /* if (sum > 0) */ {
    sum /= (double)count;
    _actual_average = (float)sum;
    _actual_std_dev = (float)(sqrt ((sqr_sum-((double)count*sum*sum))
                                    /(double)(count-1)));
    _grid_not_defined = 0;
  }
  _statistics_ready = 1;
  return 1;
}

// process this with a constant
int UCharGrid::process (Function3 function, UCharGrid *to) const
{
// check suitability and compatibility
  if (!to) return 0;

  if (!(to->getUndefined() == _not_defined)) {
    to->_error_status = UCG_SET_RANGE_ERROR;
    return 0;
  }

  long num_xy_bins     = (long)_num_x_bins * (long)_num_y_bins;
  long sub_start_y_bin = (long)getSubYBinStart ();
  long sub_start_x_bin = (long)_sub_start_x;
  long sub_start_z_bin = (long)getSubZBinStart ();
  long xyz_index_offset
    = sub_start_z_bin * num_xy_bins
    + sub_start_x_bin * (long)_num_y_bins
    + sub_start_y_bin;
  if (!(xyz_index_offset >= 0)) {  // make sure you start on this grid
    to->_error_status = UCG_INVALID_SUB_REGION;
    return 0;
  }

  if (!(to->setSubSize(_sub_x_bins, _sub_y_bins, _sub_z_bins))) return 0;

  long to_num_xy_bins     = (long)to->_num_x_bins * (long)to->_num_y_bins;
  long to_sub_start_y_bin = (long)to->getSubYBinStart ();
  long to_sub_start_x_bin = (long)to->_sub_start_x;
  long to_sub_start_z_bin = (long)to->getSubZBinStart ();
  long to_xyz_index_offset
    = to_sub_start_z_bin * to_num_xy_bins
    + to_sub_start_x_bin * (long)to->_num_y_bins
    + to_sub_start_y_bin;
  if (!(to_xyz_index_offset >= 0)) {  // make sure you start on this grid
    to->_error_status = UCG_INVALID_SUB_REGION;
    return 0;
  }

// make sure you stop on this grid
  long max_index = (long)(_num_z_bins - 1) * num_xy_bins
    + (long)(_num_x_bins - 1) * (long)_num_y_bins
    + (long)(_num_y_bins - 1);

  long sub_max_index = (long)(sub_start_z_bin + _sub_z_bins - 1) * num_xy_bins
    + (long)(sub_start_x_bin + _sub_x_bins - 1) * (long)_num_y_bins
    + (long)(sub_start_y_bin + _sub_y_bins - 1);
  if (!(sub_max_index <= max_index)) {
    to->_error_status = UCG_INVALID_SUB_REGION;
    return 0;
  }

// make sure you stop on this grid
  long to_max_index = (long)(to->_num_z_bins - 1) * to_num_xy_bins
    + (long)(to->_num_x_bins - 1) * (long)to->_num_y_bins
    + (long)(to->_num_y_bins - 1);

  long to_sub_max_index = (long)(to_sub_start_z_bin + to->_sub_z_bins - 1)
    * to_num_xy_bins
    + (long)(to_sub_start_x_bin + to->_sub_x_bins - 1) * (long)to->_num_y_bins
    + (long)(to_sub_start_y_bin + to->_sub_y_bins - 1);
  if (!(to_sub_max_index <= to_max_index)) {
    to->_error_status = UCG_INVALID_SUB_REGION;
    return 0;
  }

  long index, xy_index_offset, to_index, to_xy_index_offset;
  int k2, k3, k4;
  for (k2 = 0; k2 < _sub_z_bins; k2++) {
    xy_index_offset = xyz_index_offset;
    to_xy_index_offset = to_xyz_index_offset;
    for (k3 = 0; k3 < _sub_x_bins; k3++) {
      index = xy_index_offset;
      to_index = to_xy_index_offset;
      for (k4 = 0; k4 < _sub_y_bins; k4++) {
        to->_grid[to_index] = (this->*function) (index);
        index++;
        to_index++;
        if (to->_do_abort) {
          if (!(to_index % (long)2000)) {
	    if (to->_do_abort->userAbort()) {
              to->_error_status = UCG_USER_ABORTED;
              return 0;
            }
          }
        }
      }
      xy_index_offset    += (long)_num_y_bins;
      to_xy_index_offset += (long)to->_num_y_bins;
    }
    xyz_index_offset    += num_xy_bins;
    to_xyz_index_offset += to_num_xy_bins;
  }

  to->_error_status = UCG_SUCCESSFUL;
  return 1;
}

// process this with a constant
int UCharGrid::process (Function4 function, float value)
{
  long num_xy_bins     = (long)_num_x_bins * (long)_num_y_bins;
  long sub_start_y_bin = (long)getSubYBinStart ();
  long sub_start_x_bin = (long)_sub_start_x;
  long sub_start_z_bin = (long)getSubZBinStart ();
  long xyz_index_offset
    = sub_start_z_bin * num_xy_bins
    + sub_start_x_bin * (long)_num_y_bins
    + sub_start_y_bin;
  if (!(xyz_index_offset >= 0)) {  // make sure you start on this grid
    _error_status = UCG_INVALID_SUB_REGION;
    return 0;
  }

// make sure you stop on this grid
  long max_index = (long)(_num_z_bins - 1) * num_xy_bins
    + (long)(_num_x_bins - 1) * (long)_num_y_bins
    + (long)(_num_y_bins - 1);

  long sub_max_index = (long)(sub_start_z_bin + _sub_z_bins - 1) * num_xy_bins
    + (long)(sub_start_x_bin + _sub_x_bins - 1) * (long)_num_y_bins
    + (long)(sub_start_y_bin + _sub_y_bins - 1);
  if (!(sub_max_index <= max_index)) {
    _error_status = UCG_INVALID_SUB_REGION;
    return 0;
  }

  long index, xy_index_offset;
  int k2, k3, k4;
  for (k2 = 0; k2 < _sub_z_bins; k2++) {
    xy_index_offset = xyz_index_offset;
    for (k3 = 0; k3 < _sub_x_bins; k3++) {
      index = xy_index_offset;
      for (k4 = 0; k4 < _sub_y_bins; k4++) {
        _grid[index] = (this->*function) (index, value);
        index++;
        if (_do_abort) {
          if (!(index % (long)2000)) {
	    if (_do_abort->userAbort()) {
              _error_status = UCG_USER_ABORTED;
              return 0;
            }
          }
        }
      }
      xy_index_offset += (long)_num_y_bins;
    }
    xyz_index_offset += num_xy_bins;
  }

  _error_status = UCG_SUCCESSFUL;
  return 1;
}

unsigned char UCharGrid::valueAtIndex (long index) const
{
  return _grid[index];
}

// this value needs to be validated at a higher level
unsigned char UCharGrid::constantValue (long /* index */, float value) const
{
  return (unsigned char) (value);
}

unsigned char UCharGrid::divideValue (long index, float value) const
{
  float retval;
  if (_grid[index] == _not_defined || (unsigned char)value == _not_defined ||
    fabs((double)value) < EPSILON)
    return _not_defined;
  else if (_grid[index] == value)
    return (unsigned char)1;
  else {
    retval = (float)_grid[index] / value + 0.5;
    if (valueIsInRange (retval))
      return (unsigned char) retval;
    else
      return _not_defined;
  }
}

unsigned char UCharGrid::multiplyValue (long index, float value) const
{
  float retval;
  if (_grid[index] == _not_defined || (unsigned char)value == _not_defined)
    return _not_defined;
  else {
    retval = (float)_grid[index] * value + 0.5;
    if (valueIsInRange (retval))
      return (unsigned char) retval;
    else
      return _not_defined;
  }
}

unsigned char UCharGrid::addValue (long index, float value) const
{
  float retval;
  if (_grid[index] == _not_defined || (unsigned char)value == _not_defined)
    return _not_defined;
  else {
    retval = (float)_grid[index] + value + 0.5;
    if (valueIsInRange (retval))
      return (unsigned char) retval;
    else
      return _not_defined;
  }
}

unsigned char UCharGrid::subtractValue (long index, float value) const
{
  float retval;
  if (_grid[index] == _not_defined || (unsigned char)value == _not_defined)
    return _not_defined;
  else {
    retval = (float)_grid[index] - value + 0.5;
    if (valueIsInRange (retval))
      return (unsigned char) retval;
    else
      return _not_defined;
  }
}

int UCharGrid::completelyUsed () const
{
  int retval =
    _sub_x_bins == _num_x_bins &&
    _sub_y_bins == _num_y_bins &&
    _sub_z_bins == _num_z_bins   ;
  return retval;
}

int UCharGrid::completelyCoveredBy (const UCharGrid *by) const
{
  int retval =
    by->_sub_x_bins == _num_x_bins &&
    by->_sub_y_bins == _num_y_bins &&
    by->_sub_z_bins == _num_z_bins   ;
  return retval;
}

int UCharGrid::copyHelper (const UCharGrid &from)
{
// check for copying self
  if (&from == this) return 1;

// copy constants
  _extrema_ready     = from._extrema_ready;
  _statistics_ready  = from._statistics_ready;
  _num_x_bins        = from._num_x_bins;
  _num_y_bins        = from._num_y_bins;
  _num_z_bins        = from._num_z_bins;
  _do_abort          = from._do_abort;
  _sub_start_x       = from._sub_start_x;
  _sub_start_y       = from._sub_start_y;
  _sub_start_z       = from._sub_start_z;
  _sub_x_bins        = from._sub_x_bins;
  _sub_y_bins        = from._sub_y_bins;
  _sub_z_bins        = from._sub_z_bins;
  _actual_minimum    = from._actual_minimum;
  _actual_maximum    = from._actual_maximum;
  _actual_average    = from._actual_average;
  _actual_std_dev    = from._actual_std_dev;
  _not_defined       = from._not_defined;
  _set_minimum       = from._set_minimum;
  _set_maximum       = from._set_maximum;
  _grid_not_defined  = from._grid_not_defined;
  _x_first_insert    = from._x_first_insert;
  _x_last_insert     = from._x_last_insert;
  _y_first_insert    = from._y_first_insert;
  _y_last_insert     = from._y_last_insert;
  _z_first_insert    = from._z_first_insert;
  _z_last_insert     = from._z_last_insert;
  _multi_insert      = from._multi_insert;
  _error_status      = from._error_status;
  if (_error_status != UCG_SUCCESSFUL) return 0;

// set array pointers to NULL
  _grid = 0;

// copy variable length arrays
  _grid = new unsigned char[from.getArraySize()];
  if (!_grid) {
    _error_status = UCG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  memcpy (_grid, from._grid, (size_t)from.getArraySize());
//  long k2;
//  for (k2 = 0; k2 < from.getArraySize(); k2++)
//    _grid[k2] = from._grid[k2];

  _error_status = UCG_SUCCESSFUL;
  return 1;
}
