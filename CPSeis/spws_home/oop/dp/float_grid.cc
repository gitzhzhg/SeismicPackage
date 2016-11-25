// float_grid.cc:  Implementation file for class FloatGrid
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
#include "dp/float_grid.hh"
#include "sp/do_abort.hh"

#define EPSILON 1e-35

// The X-bins and Y-bins of the grid are organized as follows:
//   The (x_bin,y_bin) = (0,0) of the grid is in the top left corner.
//   Progression through the grid goes first from top to bottom (y) and
//   then from left to right (x).
// The Xs and Ys of the grid are as follows:
//   The (x,y) = (0,0) of the grid is in the lower left corner

FloatGrid::FloatGrid (int num_x_bins, int num_y_bins, DoAbort *do_abort):
     _num_x_bins        (num_x_bins),
     _num_y_bins        (num_y_bins),
     _do_abort          (do_abort)
{
// set array pointers to NULL
  _grid = 0;
  _pattern_offsets = 0;
  _pattern_weights = 0;
  _col1 = 0;
  _col2 = 0;
  _across1 = 0;
  _across2 = 0;
  _awts1 = 0;
  _awts2 = 0;
  _down1 = 0;
  _down2 = 0;
  _dwts1 = 0;
  _dwts2 = 0;

  if (!(num_x_bins > 0 && num_y_bins > 0)) {
    _error_status = FG_SIZE_TOO_SMALL;
    return;
  }

  _grid = new float[_num_x_bins*_num_y_bins];
  if (!_grid) {
    _error_status = FG_MEMORY_ALLOCATION_ERROR;
    return;
  }

  _sub_start_x = 0;
  _sub_start_y = 0;
  _sub_x_bins = _num_x_bins;
  _sub_y_bins = _num_y_bins;
  _not_defined = -FLT_MAX;
  _set_minimum = 0;
  _set_maximum = FLT_MAX;
  _actual_minimum = _set_minimum;
  _actual_maximum = _set_minimum;
  _actual_average = _set_minimum;
  _actual_std_dev = 0;
  _pattern_length = 0;
  _correl_x_reach = 0;
  _correl_y_reach = 0;

  _extrema_ready = 0;
  if (!(fill (_set_minimum))) return;
  _extrema_ready = 1;
  _statistics_ready = 1;
  _grid_not_defined = 0;
  _rescale_gain = 1;
  _rescale_bias = 0;
  _x_insert_size = 1;
  _y_insert_size = 1;
  _x_first_insert = 0;
  _x_last_insert  = 0;
  _y_first_insert = 0;
  _y_last_insert  = 0;
  _multi_insert = 0;
  _error_status = FG_SUCCESSFUL;
}

// copy constructor
FloatGrid::FloatGrid (const FloatGrid &from)
{
// copy the from object to this object
  copyHelper (from);
}

FloatGrid::~FloatGrid ()
{
  if (_grid) delete [] _grid, _grid = 0;
  if (_pattern_offsets) delete [] _pattern_offsets, _pattern_offsets = 0;
  if (_pattern_weights) delete [] _pattern_weights, _pattern_weights = 0;

  if (_col1) delete [] _col1, _col1 = 0;
  if (_col2) delete [] _col2, _col2 = 0;
  if (_across1) delete [] _across1, _across1 = 0;
  if (_across2) delete [] _across2, _across2 = 0;
  if (_awts1) delete [] _awts1, _awts1 = 0;
  if (_awts2) delete [] _awts2, _awts2 = 0;
  if (_down1) delete [] _down1, _down1 = 0;
  if (_down2) delete [] _down2, _down2 = 0;
  if (_dwts1) delete [] _dwts1, _dwts1 = 0;
  if (_dwts2) delete [] _dwts2, _dwts2 = 0;
}

void FloatGrid::setInsertSize (int x_insert_size, int y_insert_size)
{
  _x_insert_size = x_insert_size < 1 ? 1 : x_insert_size;
  _y_insert_size = y_insert_size < 1 ? 1 : y_insert_size;
  _x_first_insert = _x_insert_size / 2;
  _x_last_insert  = _x_insert_size - _x_first_insert - 1;
  _y_first_insert = _y_insert_size / 2;
  _y_last_insert  = _y_insert_size - _y_first_insert - 1;
  if (_x_insert_size > 1 || _y_insert_size > 1)
    _multi_insert = 1;
  else
    _multi_insert = 0;
}

int FloatGrid::setRange (float not_defined, float set_minimum,
  float set_maximum)
{
  if (!(set_minimum <= set_maximum  &&
        (not_defined > set_maximum  ||
        not_defined < set_minimum)    )){
    _error_status = FG_SET_RANGE_ERROR;
    return 0;
  }

  _not_defined = not_defined;  // DO NOT CHANGE in the middle of processing
  _set_maximum = set_maximum;
  _set_minimum = set_minimum;

  _error_status = FG_SUCCESSFUL;
  return 1;
}

int FloatGrid::setSubSize (int sub_x_bins, int sub_y_bins)
{
  if (!(sub_x_bins > 0 &&
        sub_y_bins > 0   )) {
    _error_status = FG_SIZE_TOO_SMALL;
    return 0;
  }

  _sub_x_bins = sub_x_bins;
  _sub_y_bins = sub_y_bins;

  _error_status = FG_SUCCESSFUL;
  return 1;
}

// set the starting x coordinate and starting y coordinate
int FloatGrid::setSubStart (int sub_start_x, int sub_start_y)
{
  if (!
    (sub_start_x >= 0           &&
     sub_start_y >= 0           &&
     sub_start_x <  _num_x_bins &&
     sub_start_y <  _num_y_bins   )) {
    _error_status = FG_SIZE_TOO_SMALL;
    return 0;
  }

  _sub_start_x = sub_start_x;
  _sub_start_y = sub_start_y;

  _error_status = FG_SUCCESSFUL;
  return 1;
}

// set the ending x coordinate and ending y coordinate
int FloatGrid::setSubEnd (int sub_end_x, int sub_end_y)
{
  if (!
    (sub_end_x >= 0           &&
     sub_end_y >= 0           &&
     sub_end_x <  _num_x_bins &&
     sub_end_y <  _num_y_bins   )) {
    _error_status = FG_SIZE_TOO_SMALL;
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

// do not store the end points, use the starting points with this input to
//   determine the subregion size
  return setSubSize (sub_end_x-_sub_start_x+1, sub_end_y-_sub_start_y+1);
}

// set the starting x bin (column) and starting y bin (row)
int FloatGrid::setSubBinStart (int sub_start_x_bin, int sub_start_y_bin)
{
// translate the starting sub (row,col) to reflect the (x,y) organization
  int sub_start_y = _num_y_bins - _sub_y_bins - sub_start_y_bin;
// Store the starting point in (x,y), not (row,col) because if the sub region
//   is modified, the (row,col) origin should change, not the (x,y) origin.
  return setSubStart (sub_start_x_bin, sub_start_y);
}

// get the starting row index of the sub region
int FloatGrid::getSubYBinStart () const
{
  return _num_y_bins - _sub_y_bins - _sub_start_y;
}

// get the starting column index of the subregion including the insert
int FloatGrid::getSubXBinInsertStart () const
{
  int starting_column = _sub_start_x - _x_first_insert;
  if   (starting_column < 0) return 0;
  else                       return starting_column;
}

// get the starting row index of the subregion including the insert
int FloatGrid::getSubYBinInsertStart () const
{
  int starting_row = _num_y_bins - _sub_y_bins - (_sub_start_y + _y_first_insert);
  if   (starting_row < 0) return 0;
  else                    return starting_row;
}

// get the number of X-bins including the insert
int FloatGrid::getSubXBinsInsert () const
{
  int starting_column = getSubXBinInsertStart ();
  int ending_column   = getSubXBinInsertEnd ();
  return ending_column - starting_column + 1;
}

// get the number of sub Y-bins including the insert
int FloatGrid::getSubYBinsInsert () const
{
  int starting_row = getSubYBinInsertStart ();
  int ending_row   = getSubYBinInsertEnd ();
  return ending_row - starting_row + 1;
}

// get the ending column index of the sub region including the insert
int FloatGrid::getSubXBinInsertEnd () const
{
  int ending_column = _sub_start_x + _sub_x_bins + _x_last_insert - 1;
  if   (ending_column >= _num_x_bins) return _num_x_bins - 1;
  else                                return ending_column;
}

// get the ending row index of the subregion including the insert
int FloatGrid::getSubYBinInsertEnd () const
{
  int ending_row = _num_y_bins - (_sub_start_y - _y_last_insert) - 1;
  if   (ending_row >= _num_y_bins) return _num_y_bins - 1;
  else                             return ending_row;
}
// move from this to to point by point
int FloatGrid::move (FloatGrid *to) const
{

// if this is to then do nothing
  if (this == to) {
    to->_error_status = FG_SUCCESSFUL;
    return 1;
  }

// check for compatibility between this and to
  if (!
    (_not_defined == to->getUndefined() &&
     _set_minimum >= to->getMinimum()   &&
     _set_maximum <= to->getMaximum()     )) {
    to->_error_status = FG_SET_RANGE_ERROR;
    return 0;
  }

// ONLY equate to this SUB area
  if (!(process (&FloatGrid::valueAtIndex, to))) return 0;

  to->_error_status = FG_SUCCESSFUL;
  return 1;
}

// move from this inserting to to
int FloatGrid::moveWithInsert (FloatGrid *to) const
{

// if this is to then do nothing
  if (this == to) {
    to->_error_status = FG_SUCCESSFUL;
    return 1;
  }

// check for compatibility between this and to
  if (!
    (_not_defined == to->getUndefined() &&
     _set_minimum >= to->getMinimum()   &&
     _set_maximum <= to->getMaximum()     )) {
    to->_error_status = FG_SET_RANGE_ERROR;
    return 0;
  }

// ONLY insert to this SUB area
//  if (!(inserter (FloatGrid::insertIndex, to))) return 0;
  if (!(inserter (to))) return 0;

  to->_error_status = FG_SUCCESSFUL;
  return 1;
}

int FloatGrid::fill (float value)
{

// check validity of value
  if (!
         ((_not_defined < _set_minimum && value >= _not_defined) ||
          (_not_defined > _set_maximum && value <= _not_defined) &&
          valueIsValid (value)                                     )) {
    _error_status = FG_SET_RANGE_ERROR;
    return 0;
  }

// fill the value
  if (!(process (&FloatGrid::constantValue, value))) return 0;

// adjust internal values
  if (completelyUsed() && completelyCoveredBy(this)) {
    _actual_minimum = value;
    _actual_maximum = value;
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
        _actual_minimum = value;
      if (value > _actual_maximum)
        _actual_maximum = value;
    }
  }

  _error_status = FG_SUCCESSFUL;
  return 1;
}

int FloatGrid::fillOutside (float value)
{
// if subregion is equal to the entire region there is no work to do
  if (completelyUsed()) return 1;

  int region_changed = 0;

// remember incoming subregion
  int sub_start_x = getSubXBinStart ();
  int sub_start_y = getSubYBinStart ();
  int sub_x_bins  = getSubXBins ();
  int sub_y_bins  = getSubYBins ();
  int sub_end_x   = sub_start_x + sub_x_bins - 1;
  int sub_end_y   = sub_start_y + sub_y_bins - 1;

// fill all of bottom
  if (sub_start_y > 0) {
    if (!setSubStart(0,0)) return 0;
    if (!setSubEnd(_num_x_bins-1,sub_start_y-1)) return 0;
    if (!fill(value)) return 0;
    region_changed = 1;
  }
// fill all of top
  if (sub_end_y < _num_y_bins-1) {
    if (!setSubStart(0,sub_start_y+1)) return 0;
    if (!setSubEnd(_num_x_bins-1,_num_y_bins-1)) return 0;
    if (!fill(value)) return 0;
    region_changed = 1;
  }
// fill left slot
  if (sub_start_x > 0) {
    if (!setSubStart(0,sub_start_y)) return 0;
    if (!setSubEnd(sub_start_x-1,sub_end_y)) return 0;
    if (!fill(value)) return 0;
    region_changed = 1;
  }
// fill right slot
  if (sub_end_x < _num_x_bins-1) {
    if (!setSubStart(sub_end_x+1,sub_start_y)) return 0;
    if (!setSubEnd(_num_x_bins-1,sub_end_y)) return 0;
    if (!fill(value)) return 0;
    region_changed = 1;
  }

// set the subregion back to where it was
  if (region_changed) {
    if (!setSubStart(sub_start_x,sub_start_y)) return 0;
    return setSubSize (sub_x_bins, sub_y_bins);
  }
  return 1;
}

int FloatGrid::divide (FloatGrid *by)
{
  if (!(by)) {
    _error_status = FG_INVALID_GRID;
    return 0;
  }


// if this is by then fill with ones and return
  if (this == by) {
    if (!(fill (1))) return 0;
    _extrema_ready = 0;
    _statistics_ready = 0;
    _error_status = FG_SUCCESSFUL;
    return 1;
  }

  if (!(by->process (&FloatGrid::divideIndex, this))) {
    _error_status = by->_error_status;
    return 0;
  }
  _extrema_ready = 0;
  _statistics_ready = 0;

  _error_status = FG_SUCCESSFUL;
  return 1;
}

int FloatGrid::divide (FloatGrid *by, FloatGrid *to)
{
  if (!(by && to)) {
    _error_status = FG_INVALID_GRID;
    return 0;
  }

  if (this == to) {
    if (!(divide (by))) return 0;
    _error_status = FG_SUCCESSFUL;
    return 1;
  }

  if (this == by) {
    if (!(to->fill(1))) {
      _error_status = to->_error_status;
      return 0;
    }
    to->_extrema_ready = 0;
    to->_statistics_ready = 0;
    _error_status = FG_SUCCESSFUL;
    return 1;
  }

// enforce the assumption that this and by have the same sized subregions
  if (!
    (_sub_x_bins == by->_sub_x_bins &&
     _sub_y_bins == by->_sub_y_bins   )) {
    _error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

  if (!(process (&FloatGrid::valueAtIndex, to))) return 0; // copy this to to
  if (!(by->process (&FloatGrid::divideIndex, to))) {      // divide to by by
    _error_status = by->_error_status;
    return 0;
  }
  to->_extrema_ready = 0;                                 //   & store in to
  to->_statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
  return 1;
}

// utility to resample from this grid to a different grid
int FloatGrid::resample (FloatGrid *to, int NN_only)
{
  if (!(to)) {
    _error_status = FG_INVALID_GRID;
    return 0;
  }

// if this is to then do nothing
  if (this == to) {
    _error_status = FG_SUCCESSFUL;
    return 1;
  }
  
// set range on output grid to be equal to this grid
  if (!(to->setRange (_not_defined, _set_minimum, _set_maximum))) {
    _error_status = to->_error_status;
    return 0;
  }

  int num_x_bins_in   = _num_x_bins;
  int num_y_bins_in   = _num_y_bins;
  float *in           = _grid;
  int num_x_bins_out  = to->getNumXBins();
  int num_y_bins_out  = to->getNumYBins();
  float *out          = to->getArray();

// if this is same size as to then simply copy
  long k2;
  if (num_x_bins_in == num_x_bins_out &&
      num_y_bins_in == num_y_bins_out   ) {
    for (k2 = 0; k2 < getArraySize(); k2++) {
      out[k2] = in[k2];
      if (_do_abort) {
        if (!(k2 % (long)2000)) {
	  if (_do_abort->userAbort()) {
            _error_status = FG_USER_ABORTED;
            return 0;
          }
        }
      }
    }
    _error_status = FG_SUCCESSFUL;
    return 1;
  }

// create an _across1 array that is a set of indexes based on the input grid
//   do this for NN resampling
  _across1 = new int[num_x_bins_out];
  if (!(_across1)) {
    _error_status = FG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  float step_size = (float) num_x_bins_in / (float) num_x_bins_out;
  float total = step_size / 2;
  for (k2 = 0; k2 < num_x_bins_out; k2++) {
    _across1[k2] = (int) total;
    total += step_size;
  }

// make the _across1 array a set of differences
  int prevm1 = _across1[0];
  int prev;
  for (k2 = 1; k2 < num_x_bins_out; k2++) { // don't increment the first time
    prev = _across1[k2];
    _across1[k2] -= prevm1;
    prevm1 = prev;
  }

// create a _down1 array that is a set of indexes based on the input grid
  _down1 = new int[num_y_bins_out];
  if (!(_down1)) {
    _error_status = FG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  step_size = (float) num_y_bins_in / (float) num_y_bins_out;
  total = step_size / 2;
  for (k2 = 0; k2 < num_y_bins_out; k2++) {
    _down1[k2] = (int) total;
    total += step_size;
    prevm1 = prev;
  }

// make the _down1 array a set of differences
  prevm1 = _down1[0];
  for (k2 = 1; k2 < num_y_bins_out; k2++) { // don't increment the first time
    prev = _down1[k2];
    _down1[k2] -= prevm1;
    prevm1 = prev;
  } 

// do NN resampling
  int k3;
  long indexO = 0, offset1I = 0, index1I;
  _col1 = new float[num_y_bins_out];
  if (!(_col1)) {
    _error_status = FG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  for (k2 = 0; k2 < num_x_bins_out; k2 ++) {
    if (k2 == 0 || _across1[k2] > 0) { // only do this when necessary
      offset1I += _across1[k2] * num_y_bins_in;
      index1I = offset1I;
      for (k3 = 0; k3 < num_y_bins_out; k3 ++) {
        index1I += _down1[k3];
        _col1[k3] = in[index1I];
      }
    }
    for (k3 = 0; k3 < num_y_bins_out; k3++) {
      out[indexO] = _col1[k3];
      indexO++;
      if (_do_abort) {
        if (!(indexO % (long)2000)) {
          if (_do_abort->userAbort()) {
            _error_status = FG_USER_ABORTED;
            return 0;
          }
        }
      }
    }
  }
  delete [] _col1, _col1 = 0;

  if (NN_only) {
    delete [] _across1, _across1 = 0;
    delete [] _down1, _down1 = 0;
    _error_status = FG_SUCCESSFUL;
    return 1;
  }

// redo the across and down arrays for BL resampling
// create an _across1 array that is a set of indexes based on the input grid
  _awts1 = new float[num_x_bins_out];
  _awts2 = new float[num_x_bins_out];
  if (!(_awts1 && _awts2)) {
    _error_status = FG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  total = 0;
  if (num_x_bins_out < 2)
    step_size = 0;
  else
    step_size = (float)(num_x_bins_in - 1) / (float)(num_x_bins_out - 1);
  for (k2 = 0; k2 < num_x_bins_out; k2++) {
    _across1[k2] = (int) total;
    _awts2[k2]   = total - (float) _across1[k2];
    _awts1[k2]   = 1 - _awts2[k2];
    total += step_size;
  }

// create an _across2 array that is the _across1 array + 1 except at end
  _across2 = new int[num_x_bins_out];
  if (!(_across2)) {
    _error_status = FG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  for (k2 = 0; k2 < num_x_bins_out; k2++) {
    if (_across1[k2] == num_x_bins_in-1)
      _across2[k2] = _across1[k2];
    else
      _across2[k2] = _across1[k2] + 1;
  }

// make the _across1 array a set of differences
  prevm1 = _across1[0];
  for (k2 = 1; k2 < num_x_bins_out; k2++) { // don't increment the first time
    prev = _across1[k2];
    _across1[k2] -= prevm1;
    prevm1 = prev;
  }

// make the _across2 array a set of differences
  prevm1 = _across1[0];  // you want to increment the first time
  for (k2 = 0; k2 < num_x_bins_out; k2++) {
    prev = _across2[k2];
    _across2[k2] -= prevm1;
    prevm1 = prev;
  }

// create a _down1 array that is a set of indexes based on the input grid
  _dwts1 = new float[num_y_bins_out];
  _dwts2 = new float[num_y_bins_out];
  if (!(_dwts1 && _dwts2)) {
    _error_status = FG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }


  total = 0;
  if (num_y_bins_out < 2)
    step_size = 0;
  else
    step_size = (float)(num_y_bins_in - 1) / (float)(num_y_bins_out - 1);
  for (k2 = 0; k2 < num_y_bins_out; k2++) {
    _down1[k2] = (int) total;
    _dwts2[k2]   = total - (float) _down1[k2];
    _dwts1[k2]   = 1 - _dwts2[k2];
    total += step_size;
  }

// create a _down2 array that is the _down1 array + 1 except at end
  _down2 = new int[num_y_bins_out];
  if (!_down2) {
    _error_status = FG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  for (k2 = 0; k2 < num_y_bins_out; k2++) {
    if (_down1[k2] == num_y_bins_in-1)
      _down2[k2] = _down1[k2];
    else
      _down2[k2] = _down1[k2] + 1;
  }

// make the _down1 array a set of differences
  prevm1 = _down1[0];
  for (k2 = 1; k2 < num_y_bins_out; k2++) { // don't increment the first time
    prev = _down1[k2];
    _down1[k2] -= prevm1;
    prevm1 = prev;
  } 

// make the _down2 array a set of differences
  prevm1 = _down1[0];  // do increment the first time
  for (k2 = 0; k2 < num_y_bins_out; k2++) {
    prev = _down2[k2];
    _down2[k2] -= prevm1;
    prevm1 = prev;
  } 

// do BL resampling where data is defined and be careful not to mix
//   undefined data with defined data
  _col1 = new float[num_y_bins_out];
  _col2 = new float[num_y_bins_out];
  if (!(_col1 && _col2)) {
    _error_status = FG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  indexO = 0, offset1I = 0, index1I;
  long offset2I = 0, index2I;
  long offset3I = 0, index3I;
  long offset4I = 0, index4I;
  float value1I, value2I, value3I, value4I;
  for (k2 = 0; k2 < num_x_bins_out; k2++) {
    if (k2 == 0 || _across1[k2] > 0) {  // only do this when necessary
      offset1I += _across1[k2] * num_y_bins_in;
      index1I = offset1I;
      offset2I += _across1[k2] * num_y_bins_in;
      index2I = offset2I;
      offset3I += _across2[k2] * num_y_bins_in;
      index3I = offset3I;
      offset4I += _across2[k2] * num_y_bins_in;
      index4I = offset4I;
      for (k3 = 0; k3 < num_y_bins_out; k3++) {
        index1I += _down1[k3];
        index2I += _down2[k3];
        if (in[index1I] == _not_defined)
          _col1[k3] = in[index2I]; // set to opposite value
        else if (in[index2I] == _not_defined)
          _col1[k3] = in[index1I]; // set to opposite value
        else {
          value1I = in[index1I];  // set to nominal value
          value2I = in[index2I];  // set to nominal value
          _col1[k3] = _dwts1[k3] * value1I + _dwts2[k3] * value2I;
	}

        index3I += _down1[k3];
        index4I += _down2[k3];
        if (in[index3I] == _not_defined)
          _col2[k3] = in[index4I];  // set to opposite value
        else if (in[index4I] == _not_defined)
          _col2[k3] = in[index3I];  // set to opposite value
        else {
          value3I = in[index3I];  // set to nominal value
          value4I = in[index4I];  // set to nominal value
          _col2[k3] = _dwts1[k3] * value3I + _dwts2[k3] * value4I;
        }
      }
    }
    for (k3 = 0; k3 < num_y_bins_out; k3++) {
      if (out[indexO] != _not_defined) {
        if (_col1[k3] == _not_defined)
          out[indexO] = _col2[k3];  // set to opposite value
        else if (_col2[k3] == _not_defined)
          out[indexO] = _col1[k3];  // set to opposite value
        else     // compute the nominal value
          out[indexO] = _awts1[k2] * _col1[k3] + _awts2[k2] * _col2[k3];
      }
      indexO++;
      if (_do_abort) {
        if (!(indexO % (long)2000)) {
          if (_do_abort->userAbort()) {
            _error_status = FG_USER_ABORTED;
            return 0;
          }
        }
      }
    }
  }
  delete [] _col1, _col1 = 0;
  delete [] _col2, _col2 = 0;
  delete [] _across1, _across1 = 0;
  delete [] _across2, _across2 = 0;
  delete [] _awts1, _awts1 = 0;
  delete [] _awts2, _awts2 = 0;
  delete [] _down1, _down1 = 0;
  delete [] _down2, _down2 = 0;
  delete [] _dwts1, _dwts1 = 0;
  delete [] _dwts2, _dwts2 = 0;

  _error_status = FG_SUCCESSFUL;
  return 1;
}

// determine how many undefined grids you have in the entire grid
long FloatGrid::countUndefined ()
{
  long retval = 0;
  for (long k2 = 0; k2 < getArraySize(); k2++) {
    if (_grid[k2] == _not_defined) retval++;
    if (_do_abort) {
      if (!(k2 % (long)2000)) {
        if (_do_abort->userAbort()) {
          _error_status = FG_USER_ABORTED;
          return 0;
        }
      }
    }
  }
  return retval;
}

// utility to convert this grid in place
int FloatGrid::convert ()
{
// ensure that some data is defined
  if (_grid_not_defined) {
    _error_status = FG_INVALID_GRID;
    return 0;
  }
    
// check to see if there is any work to do
  if (fabs ((double)findMinimum() - (double)_set_minimum) < EPSILON &&
      fabs ((double)findMaximum() - (double)_set_maximum) < EPSILON   ) {
    _error_status = FG_SUCCESSFUL;
    return 1;
  }

  float gain, bias;
// determine the gain and bias
  if ((findMaximum() - findMinimum()) < EPSILON) {
    if (findMinimum() > EPSILON)
      gain = (_set_maximum - _set_minimum) / findMinimum();
    else
      gain = 0;
  }
  else
    gain = (_set_maximum - _set_minimum) / (findMaximum() - findMinimum());
  bias = _set_minimum - gain * findMinimum();

// do the conversion
  for (long k2 = 0; k2 < getArraySize(); k2++) {
    if (_grid[k2] != _not_defined)
      _grid[k2] = gain * _grid[k2] + bias;
    if (_do_abort) {
      if (!(k2 % (long)2000)) {
        if (_do_abort->userAbort()) {
          _error_status = FG_USER_ABORTED;
          return 0;
        }
      }
    }
  }
  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
  return 1;
}

// utility to reset the value for undefined
int FloatGrid::resetUndefined (float new_not_defined)
{
// did the undefined value change
  if (new_not_defined == _not_defined) {
    _error_status = FG_SUCCESSFUL;
    return 1;
  }

// do it if the change valid
  if (findMinimum() > new_not_defined || findMaximum() < new_not_defined) {
    for (long k2 = 0; k2 < getArraySize(); k2++) {
      if (_grid[k2] == _not_defined)
        _grid[k2] = new_not_defined;
      if (_do_abort) {
        if (!(k2 % (long)2000)) {
          if (_do_abort->userAbort()) {
            _error_status = FG_USER_ABORTED;
            return 0;
          }
        }
      }
    }
    _not_defined = new_not_defined;
    _error_status = FG_SUCCESSFUL;
    return 1;
  }
  else {
    _error_status = FG_SET_RANGE_ERROR;
    return 0; // could not use new value
  }
}

// utility to rescale this grid
int FloatGrid::setRescaleParameters (float gain, float bias)
{
  _rescale_gain = gain;
  _rescale_bias = bias;

// indicate if the rescaling will interfere with the undefined data
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
    _error_status = FG_SUCCESSFUL;
    return 1; // if the new gain and bias are used, they will not corrupt data
  }
  else {
    _error_status = FG_SET_RANGE_ERROR;
    return 0; // if the new gain and bias are used, they will corrupt the data
  }
}

int FloatGrid::rescale ()
{
  if (!(process (&FloatGrid::rescaleIndex, this))) return 0;
  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
  return 1;
}

int FloatGrid::rescaleWithClip ()
{
  if (!(process (&FloatGrid::rescaleIndexWithClip, this))) return 0;

// if necessary reset the extrema ready flag
  if (_extrema_ready)
    if (fabs ((double)findMinimum() - (double)_set_minimum) > EPSILON ||
        fabs ((double)findMaximum() - (double)_set_maximum) > EPSILON   ) {
      _extrema_ready = 0;
      _statistics_ready = 0;
    }
  _error_status = FG_SUCCESSFUL;
  return 1;
}

float FloatGrid::findMinimum ()
{
  if (!_extrema_ready)
    if (!findExtrema ()) return 0;
  return _actual_minimum;
}

float FloatGrid::findMaximum ()
{
  if (!_extrema_ready)
    if (!findExtrema ()) return 0;
  return _actual_maximum;
}

// result will be greater than or equal to actual minimum by definition
float FloatGrid::findStatisticalMinimum (float std_dev)
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
float FloatGrid::findStatisticalMaximum (float std_dev)
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

// Find the X value for a given index.  The index must be a physically
//   accessible bin, otherwise it is clipped to be so.
int FloatGrid::getX (long index) const
{
// clip index

  long index_in;
  if (index < 0) index_in = 0;
  else if (index >= getArraySize()) index_in = getArraySize() - 1;
  else index_in = index;

  int retval = (int) (index_in / (long)_num_y_bins);
  return retval;
}

// Find the X offset given an offset.
int FloatGrid::getXOffset (long offset) const
{
  int x0, y0;
  if (offset < 0) {
    x0 = _num_x_bins - 1;
    y0 = _num_y_bins - 1;
  }
  else /* if (offset >= 0) */ {
    x0 = 0;
    y0 = 0;
  }
  int x = getX (getIndex (x0, y0) + offset);
  return x - x0;
}

// Find the Y value for a given index.  The index must be a physically
//   accessible bin, otherwise it is clipped to be so.
int FloatGrid::getY (long index) const
{
// clip index

  long index_in;
  if (index < 0) index_in = 0;
  else if (index >= getArraySize()) index_in = getArraySize() - 1;
  else index_in = index;

  long x = index_in / (long)_num_y_bins;
  int retval = (int) (index_in - x * (long)_num_y_bins);
  retval = _num_y_bins - 1 - retval;  // negate Y due to (x,y) organization
  return retval;
}

// Find the Y offset given an offset.
int FloatGrid::getYOffset (long offset) const
{
  int x0, y0;
  if (offset < 0) {
    x0 = _num_x_bins - 1;
    y0 = _num_y_bins - 1;
  }
  else /* if (offset >= 0) */ {
    x0 = 0;
    y0 = 0;
  }
  int y = getY (getIndex (x0, y0) + offset);
  return y0 - y;  // negate Y due to (x,y) organization
}

// Find an array index for a given X,Y bin pair.  The X,Y bin pair must be a
//   physically accessible bin, otherwise, it is clipped to be so.
long FloatGrid::getIndex (int x, int y) const
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

  int y_n = _num_y_bins - 1 - yin;  // negate Y due to organization
  long retval = (long)xin * (long)_num_y_bins + (long)y_n;
  return retval;
}

long FloatGrid::getOffset (int x_offset, int y_offset) const
{
  long retval = (long)x_offset * (long)_num_y_bins - (long)y_offset; // note -Y
  return retval;
}

// 0 => value is invalid (i.e. it lies on the wrong side of the _not_defined
//      value or it lies in between in "no man's land")
// 1 => value is defined and lies between current actual extrema
// 2 => value is defined and is less than current actual minumum
// 3 => value is defined and is greater than current actual maximum
// 4 => value is defined but _extrema_ready flag is false
// 5 => value is defined but _grid_not_defined flag is true
// 6 => value is equal to _not_defined value
int FloatGrid::valueIsValid (float value) const
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

  if (_grid_not_defined)
    return (int)5;

  if (value >= _actual_minimum && value <= _actual_maximum)
    return (int)1;

  if (value < _actual_minimum)
    return (int)2;
/*
  if (value > _actual_maximum) */
    return (int)3;

}

void FloatGrid::resetExtremaReadyFlag ()
{
  _extrema_ready = 0;
  _statistics_ready = 0;
}

// using this without resetting _extrema_ready flag will lead to corrupted grid
void FloatGrid::accumulateXY (int x, int y, float value)
{
  if (_multi_insert) {
// using getIndex is not efficient but boundary checking is done
    long tl_index = getIndex (x-_x_first_insert, y+_y_first_insert);
    long bl_index = getIndex (x-_x_first_insert, y-_y_last_insert);
    long tr_index = getIndex (x+_x_last_insert,  y+_y_first_insert);
    long index_range = bl_index - tl_index;// index counting begins at top-left
    long e_index;
    for (long b_index = tl_index; b_index <= tr_index; b_index+=_num_y_bins) {
      e_index = b_index + index_range;
      for (long index = b_index; index <= e_index; index++) {
        _grid[index] += value;
      }
    }
  }
  else
    _grid[getIndex(x,y)] += value;
}

// using this without resetting _extrema_ready flag will lead to corrupted grid
void FloatGrid::accumulateIndex (long index, float value)
{
  if (_multi_insert) {
    int x = getX (index);
    int y = getY (index);
// using getIndex is not efficient but boundary checking is done
    long tl_index = getIndex (x-_x_first_insert, y+_y_first_insert);
    long bl_index = getIndex (x-_x_first_insert, y-_y_last_insert);
    long tr_index = getIndex (x+_x_last_insert,  y+_y_first_insert);
    long index_range = bl_index - tl_index;// index counting begins at top-left
    long e_index;
    for (long b_index = tl_index; b_index <= tr_index; b_index+=_num_y_bins) {
      e_index = b_index + index_range;
      for (long index = b_index; index <= e_index; index++)
        _grid[index] += value;
    }
  }
  else
    _grid[index] += value;
}

// using this without resetting _extrema_ready flag will lead to corrupted grid
void FloatGrid::insertXY (int x, int y, float value)
{
  if (_multi_insert) {
// using getIndex is not efficient but boundary checking is done
    long tl_index = getIndex (x-_x_first_insert, y+_y_first_insert);
    long bl_index = getIndex (x-_x_first_insert, y-_y_last_insert);
    long tr_index = getIndex (x+_x_last_insert,  y+_y_first_insert);
    long index_range = bl_index - tl_index;// index counting begins at top-left
    long e_index;
    for (long b_index = tl_index; b_index <= tr_index; b_index+=_num_y_bins) {
      e_index = b_index + index_range;
      for (long index = b_index; index <= e_index; index++)
        _grid[index] = value;
    }
  }
  else
    _grid[getIndex(x,y)] = value;
}
// using this without resetting _extrema_ready flag will lead to corrupted grid
void FloatGrid::insertIndex (long index, float value)
{
  if (_multi_insert) {
    int x = getX (index);
    int y = getY (index);
// using getIndex is not efficient but boundary checking is done
    long tl_index = getIndex (x-_x_first_insert, y+_y_first_insert);
    long bl_index = getIndex (x-_x_first_insert, y-_y_last_insert);
    long tr_index = getIndex (x+_x_last_insert,  y+_y_first_insert);
    long index_range = bl_index - tl_index;// index counting begins at top-left
    long e_index;
    for (long b_index = tl_index; b_index <= tr_index; b_index+=_num_y_bins) {
      e_index = b_index + index_range;
      for (long index = b_index; index <= e_index; index++)
        _grid[index] = value;
    }
  }
  else
    _grid[index] = value;
}

int FloatGrid::initializeSearchGridPattern (int max_hits, int pattern_length)
{
  _max_hits = max_hits;
  if (!_pattern_offsets || _pattern_length != pattern_length) {
    _pattern_length = pattern_length;
    if (_pattern_offsets)
      delete [] _pattern_offsets, _pattern_offsets = 0;
    if (_pattern_weights)
      delete [] _pattern_weights, _pattern_weights = 0;
    if (_pattern_length > 0) {
      _pattern_offsets = new long[_pattern_length];
      if (!(_pattern_offsets)) {
        _error_status = FG_MEMORY_ALLOCATION_ERROR;
        return 0;
      }
      _pattern_weights = new float[_pattern_length];
      if (!(_pattern_weights)) {
        _error_status = FG_MEMORY_ALLOCATION_ERROR;
        return 0;
      }
    }
  }

  int k2;
  for (k2 = 0; k2 < _pattern_length; k2++) {
    _pattern_offsets[k2] = 0;
    _pattern_weights[k2] = 1;
  }

  _error_status = FG_SUCCESSFUL;
  return 1;
}

int FloatGrid::setSearchGridPatternOffset (int index, int x_offset,
  int y_offset)
{
  if (!_pattern_offsets) {
    _error_status = FG_INVALID_GRID;
    return 0;
  }

  int index_in;
  if (index < 0) index_in = 0;
  else if (index >= _pattern_length) index_in = _pattern_length - 1;
  else index_in = index;

  _pattern_offsets[index_in] = getOffset (x_offset, y_offset);

  _error_status = FG_SUCCESSFUL;
  return 1;
}

int FloatGrid::setSearchGridPatternWeight (int index, float weight)
{
  if (!_pattern_weights) {
    _error_status = FG_INVALID_GRID;
    return 0;
  }

  int index_in;
  if (index < 0) index_in = 0;
  else if (index >= _pattern_length) index_in = _pattern_length - 1;
  else index_in = index;

  float weight_in;
  if ((!fabs ((double)weight) > 0)) weight_in = 0.000000001;
  else weight_in = weight;


  _pattern_weights[index_in] = weight_in;

  _error_status = FG_SUCCESSFUL;
  return 1;
}

// searchGrid function (does not return anything)
int FloatGrid::searchGrid (FloatGrid *to) const
{
  if (!(to && _pattern_offsets && _pattern_weights)) {
    to->_error_status = FG_INVALID_GRID;
    return 0;
  }

  if (!(process (&FloatGrid::searchGridIndex, to))) return 0;

  to->_error_status = FG_SUCCESSFUL;
  return 1;
}

// searchGrid function (returns a reference to a constructed grid)
FloatGrid searchGrid (const FloatGrid &operand)
{
  return FloatGrid (&FloatGrid::searchGridIndex, operand);
}

// convolve function (does not return anything)
int FloatGrid::convolve (FloatGrid *to) const
{
  if (!(to && _pattern_offsets && _pattern_weights)) {
    to->_error_status = FG_INVALID_GRID;
    return 0;
  }

  if (!(process (&FloatGrid::convolveIndex, to))) return 0;

  to->_error_status = FG_SUCCESSFUL;
  return 1;
}

// conv (convolve) function (returns a reference to a constructed grid)
FloatGrid conv (const FloatGrid &operand)
{
  return FloatGrid (&FloatGrid::convolveIndex, operand);
}

// correlation function (does not return anything)
int FloatGrid::correlate (FloatGrid *to) const
{
  if (!(to && (_correl_x_reach > 0) && (_correl_y_reach > 0))) {
    to->_error_status = FG_INVALID_GRID;
    return 0;
  }

  if (!(process (&FloatGrid::correlateIndex, to))) return 0;

  to->_error_status = FG_SUCCESSFUL;
  return 1;
}

// correlation function (returns a reference to a constructed grid)
FloatGrid correl (const FloatGrid &operand)
{
  return FloatGrid (&FloatGrid::correlateIndex, operand);
}

// overloaded assignment operator:  *this = float_grid_right
FloatGrid &FloatGrid::operator=(const FloatGrid &right)
{
  if (&right != this) {  // check for self-assignment

    if (right.completelyUsed() && completelyCoveredBy(&right)) {
      _not_defined = right._not_defined;
      _set_maximum = right._set_maximum;
      _set_minimum = right._set_minimum;
      _actual_minimum = right._actual_minimum;
      _actual_maximum = right._actual_maximum;
      _actual_average = right._actual_average;
      _actual_std_dev = right._actual_std_dev;
      _extrema_ready = right._extrema_ready;
      _statistics_ready = right._statistics_ready;
    }

    else {
// with programming misuse, this case can lead to a corrupted lhs grid, but
//   check as best as can
      if (_not_defined < _set_minimum) {
        if (right._extrema_ready) {
          if (!(right._actual_minimum >= _not_defined)) {
            _error_status = FG_SET_RANGE_ERROR;
            return *this;
          }
	}
        else {
          if (!(right._set_minimum >= _not_defined)) {
            _error_status = FG_SET_RANGE_ERROR;
            return *this;
          }
	}
      }
      else /* if (_not_defined > _set_maximum) */ {
        if (right._extrema_ready) {
          if (!(right._actual_maximum <= _not_defined)) {
            _error_status = FG_SET_RANGE_ERROR;
            return *this;
          }
	}
        else {
          if (!(right._set_maximum <= _not_defined)) {
            _error_status = FG_SET_RANGE_ERROR;
            return *this;
          }
	}
      }
        
      _extrema_ready = 0; // at least require the extrema to be checked
      _statistics_ready = 0; // at least require the statistics to be checked
    }

// ONLY equate to the right SUB area
    if (!(right.process (&FloatGrid::valueAtIndex, this))) return *this;

/* Don't even try to equate to the right pattern arrays because the start X,Y
   may not accomodate the patterns

// equate to the right pattern arrays
    if (!(initializeSearchGridPattern (right._max_hits,
      right._pattern_length))) return *this;

    for (int k2 = 0; k2 < _pattern_length; k2++) {
      _pattern_offsets[k2] =
        getOffset (right.getXOffset(right._pattern_offsets[k2]),
                   right.getYOffset(right._pattern_offsets[k2]));
      _pattern_weights[k2] = right._pattern_weights[k2];
    }
*/

  }

  _error_status = FG_SUCCESSFUL;
  return *this;          // enables x = y = z;
}

// overloaded assignment operator:  *this = value
FloatGrid &FloatGrid::operator=(float value)
{
  fill (value);  // assign constant value to this
  return *this;
}

// overloaded summation operator:  *this + float_grid_right
FloatGrid FloatGrid::operator+(const FloatGrid &right) const
{
  return FloatGrid (&FloatGrid::addIndex, *this, right);
}

// overloaded summation operator:  *this + value
FloatGrid FloatGrid::operator+(float value) const
{
  return FloatGrid (&FloatGrid::addIndex, *this, value);
}

// overloaded summation operator:  value + float_grid_right
FloatGrid operator+(float value, const FloatGrid &right)
{
  return FloatGrid (&FloatGrid::addIndex, right, value);
}

// overloaded multiplication operator:  *this * float_grid_right
FloatGrid FloatGrid::operator*(const FloatGrid &right) const
{
  return FloatGrid (&FloatGrid::multiplyIndex, *this, right);
}

// overloaded multiplication operator:  *this * value
FloatGrid FloatGrid::operator*(float value) const
{
  return FloatGrid (&FloatGrid::multiplyIndex, *this, value);
}

// overloaded multiplication operator:  value * float_grid_right
FloatGrid operator*(float value, const FloatGrid &right)
{
  return FloatGrid (&FloatGrid::multiplyIndex, right, value);
}

// overloaded division operator: *this / float_grid_right
FloatGrid FloatGrid::operator/(const FloatGrid &right) const
{
// can't make use of *this == &right because of _not_defined's
  return FloatGrid (&FloatGrid::divideIndex, *this, right);
}

// overloaded subtraction operator:  *this - float_grid_right
FloatGrid FloatGrid::operator-(const FloatGrid &right) const
{
// can't make use of *this == &right because of _not_defined's
  return FloatGrid (&FloatGrid::subtractIndex, *this, right);
}

// overloaded self sum operator
FloatGrid &FloatGrid::operator+=(const FloatGrid &right)
{
  if (!(right.process (&FloatGrid::addIndex, this))) return *this;
  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
  return *this;
}

// overloaded self sum operator with a constant
FloatGrid &FloatGrid::operator+=(float value)
{
  if (value == _not_defined) {
    _error_status = FG_INVALID_VALUE;
    return *this;
  }
  else if (!process(&FloatGrid::addIndex,value))
    return *this;

  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
  return *this;
}

// overloaded self product operator
FloatGrid &FloatGrid::operator*=(const FloatGrid &right)
{
  if (!(right.process (&FloatGrid::multiplyIndex, this))) return *this;
  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
  return *this;
}

// overloaded self product operator with a constant
FloatGrid &FloatGrid::operator*=(float value)
{
  if (value == _not_defined) {
    _error_status = FG_INVALID_VALUE;
    return *this;
  }
  else if (value == 1.0) // can't use value = 0 because of _not_defined's
    return *this;
  else
    if (!process(&FloatGrid::multiplyIndex,value)) return *this;

  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
  return *this;
}

// overloaded self quotient operator
FloatGrid &FloatGrid::operator/=(const FloatGrid &right)
{
// can't make use of *this == &right because of _not_defined's
  if (!(right.process (&FloatGrid::divideIndex, this))) return *this;
  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
  return *this;
}

// overloaded self quotient operator with a constant
FloatGrid &FloatGrid::operator/=(float value)
{
  if (value == _not_defined) {
    _error_status = FG_INVALID_VALUE;
    return *this;
  }
  else if (value == 1.0)
    return *this;
  else if (fabs((double)value) < EPSILON) {
    _error_status = FG_ZERO_DENOMINATOR;
    return *this;
  }
  else if (!process(&FloatGrid::divideValue,value))
    return *this;

  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
  return *this;
}

// overloaded self difference operator
FloatGrid &FloatGrid::operator-=(const FloatGrid &right)
{
// can't check make use of *this == &right because of _not_defined's
  if (!(right.process (&FloatGrid::subtractIndex, this))) return *this;
  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
  return *this;
}

// overloaded self difference operator with a constant
FloatGrid &FloatGrid::operator-=(float value)
{
  if (value == _not_defined) {
    _error_status = FG_INVALID_VALUE;
    return *this;
  }
  else if (value == 0.0)
    return *this;
  else if (!process(&FloatGrid::subtractValue,value))
    return *this;

  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
  return *this;
}

// overloaded sqrt function
FloatGrid sqrt (const FloatGrid &operand)
{
  return FloatGrid (&FloatGrid::sqrtIndex, operand);
}

int FloatGrid::failed ()
{
  return (int) (_error_status != FG_SUCCESSFUL);
}

// process constructor for processes having one argument
FloatGrid::FloatGrid (Function1 function, const FloatGrid &left,
  const FloatGrid &right)
{
// copy the left object to this object
  if (!(copyHelper (left))) return;

// process this and the right object and store in this object
  if (!(right.process (function, this))) return;
  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
}

// process constructor for processes having one argument and one operand
FloatGrid::FloatGrid (Function1 function, const FloatGrid &operand)
{
// copy the operand object to this object
  if (!(copyHelper (operand))) return;

// process this object and store in this object
  if (!(operand.process (function, this))) return;
  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
}

// process constructor for processes having two arguments
FloatGrid::FloatGrid (Function2 function, const FloatGrid &left,
  const FloatGrid &right)
{
// copy the left object to this object
  if (!(copyHelper (left))) return;

// process this and the right object and store in this object
  if (!(right.process (function, this))) return;
  _extrema_ready = 0;
  _statistics_ready = 0;
  _error_status = FG_SUCCESSFUL;
}

// process constructor for processes having two arguments and a constant
FloatGrid::FloatGrid (Function2 function, const FloatGrid &left, float value)
{
// copy the left object to this object
  if (!(copyHelper (left))) return;

// process this with the constant and store in this object
  if (!(process (function, value))) return;
  _extrema_ready = 0;
  _statistics_ready = 0;
}

int FloatGrid::findExtrema ()
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
          _error_status = FG_USER_ABORTED;
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
        _error_status = FG_INVALID_GRID;
        return 0;
      }
    }
    else /* if (_not_defined > _set_maximum) */ {
      if (!(_actual_maximum < _not_defined)) {   // bad grid data got stored
        _error_status = FG_INVALID_GRID;
        return 0;
      }
    }
    _grid_not_defined = 0;
  }
  _extrema_ready = 1;

  _error_status = FG_SUCCESSFUL;
  return 1;
}

int FloatGrid::findStatistics ()
{
  double sum = 0;
  double sqr_sum = 0;
  long count = 0;
  long size = getArraySize ();
  for (long k2 = 0; k2 < size; k2++) {
    if (_grid[k2] != _not_defined) {
      sum += (double)_grid[k2];
      sqr_sum += (double)(_grid[k2] * _grid[k2]);
      count++;
    }
    if (_do_abort) {
      if (!(k2 % (long)2000)) {
        if (_do_abort->userAbort()) {
          _error_status = FG_USER_ABORTED;
          return 0;
        }
      }
    }
  }

  if (sum == 0) {                        // no grid data defined yet
    _actual_average = _not_defined;
    _actual_std_dev = 0;
    _grid_not_defined = 1;
  }

  else /* if (sum != 0) */ {
    sum /= (double)count;
    _actual_average = (float)sum;
    _actual_std_dev = (float)(sqrt ((sqr_sum-((double)count*sum*sum))
                                    /(double)(count-1)));
    _grid_not_defined = 0;
  }
  _statistics_ready = 1;
  return 1;
}

// process this and store in to using only the index of this
int FloatGrid::process (Function1 function, FloatGrid *to) const
{
// check suitability and compatibility
  if (!to) return 0;

  if (!(to->getUndefined() == _not_defined)) {
    to->_error_status = FG_SET_RANGE_ERROR;
    return 0;
  }

  long sub_start_y_bin = (long) getSubYBinStart ();
  long sub_start_x_bin = (long) _sub_start_x;
  long index_offset = sub_start_x_bin * (long)_num_y_bins + sub_start_y_bin;
  if (!(index_offset >= 0)) {  // make sure you start on this grid
    to->_error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

  if (!(to->setSubSize (_sub_x_bins, _sub_y_bins))) return 0;

  long to_sub_start_y_bin = (long) to->getSubYBinStart ();
  long to_sub_start_x_bin = (long) to->_sub_start_x;
  long to_index_offset  = to_sub_start_x_bin * (long)to->_num_y_bins
    + to_sub_start_y_bin;
  if (!(to_index_offset >= 0)) {  // make sure you start on the to grid
    to->_error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

// make sure you stop on this grid
  long max_index = (long)(_num_x_bins - 1) * (long)_num_y_bins
    + (long)(_num_y_bins - 1);
  long sub_max_index = (long)(sub_start_x_bin + _sub_x_bins - 1)
    * (long)_num_y_bins + (long)(sub_start_y_bin + _sub_y_bins - 1);
  if (!(sub_max_index <= max_index)) {
    to->_error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

// make sure you stop on the to grid
  long to_max_index = (long)(to->_num_x_bins - 1) * (long)to->_num_y_bins
    + (long)(to->_num_y_bins - 1);
  long to_sub_max_index = (long)(to_sub_start_x_bin + to->_sub_x_bins - 1)
    * (long)to->_num_y_bins + (long)(to_sub_start_y_bin
    + to->_sub_y_bins - 1);
  if (!(to_sub_max_index <= to_max_index)) {
    to->_error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

  long index, to_index;
  int k2, k3;
  for (k2 = 0; k2 < _sub_x_bins; k2++) {
    index = index_offset;
    to_index = to_index_offset;
    for (k3 = 0; k3 < _sub_y_bins; k3++) {
      to->_grid[to_index] = (this->*function) (index);
      index++;
      to_index++;
      if (to->_do_abort) {
        if (!(to_index % (long)2000)) {
          if (to->_do_abort->userAbort()) {
            to->_error_status = FG_USER_ABORTED;
            return 0;
          }
        }
      }
    }
    index_offset    += (long)_num_y_bins;
    to_index_offset += (long)to->_num_y_bins;
  }

  to->_error_status = FG_SUCCESSFUL;
  return 1;
}

// process this and store in to using the index of this and the values in to
int FloatGrid::process (Function2 function, FloatGrid *to) const
{
// check suitability and compatibility
  if (!to) return 0;

  if (!(to->getUndefined() == _not_defined)) {
    to->_error_status = FG_SET_RANGE_ERROR;
    return 0;
  }

  long sub_start_y_bin = (long) getSubYBinStart ();
  long sub_start_x_bin = (long) _sub_start_x;
  long index_offset = sub_start_x_bin * (long)_num_y_bins + sub_start_y_bin;
  if (!(index_offset >= 0)) {  // make sure you start on this grid
    to->_error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

  if (!(to->setSubSize (_sub_x_bins, _sub_y_bins))) return 0;

  long to_sub_start_y_bin = (long) to->getSubYBinStart ();
  long to_sub_start_x_bin = (long) to->_sub_start_x;
  long to_index_offset  = to_sub_start_x_bin * (long)to->_num_y_bins
    + to_sub_start_y_bin;
  if (!(to_index_offset >= 0)) {  // make sure you start on the to grid
    to->_error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

// make sure you stop on this grid
  long max_index = (long)(_num_x_bins - 1) * (long)_num_y_bins
    + (long)(_num_y_bins - 1);
  long sub_max_index = (long)(sub_start_x_bin + _sub_x_bins - 1)
    * (long)_num_y_bins + (long)(sub_start_y_bin + _sub_y_bins - 1);
  if (!(sub_max_index <= max_index)) {
    to->_error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

// make sure you stop on the to grid
  long to_max_index = (long)(to->_num_x_bins - 1) * (long)to->_num_y_bins
    + (long)(to->_num_y_bins - 1);
  long to_sub_max_index = (long)(to_sub_start_x_bin + to->_sub_x_bins - 1)
    * (long)to->_num_y_bins + (long)(to_sub_start_y_bin
    + to->_sub_y_bins - 1);
  if (!(to_sub_max_index <= to_max_index)) {
    to->_error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

  long index, to_index;
  int k2, k3;
  for (k2 = 0; k2 < _sub_x_bins; k2++) {
    index = index_offset;
    to_index = to_index_offset;
    for (k3 = 0; k3 < _sub_y_bins; k3++) {
      to->_grid[to_index] = (this->*function) (index, to->_grid[to_index]);
      index++;
      to_index++;
      if (to->_do_abort) {
        if (!(to_index % (long)2000)) {
          if (to->_do_abort->userAbort()) {
            to->_error_status = FG_USER_ABORTED;
            return 0;
          }
        }
      }
    }
    index_offset    += (long)_num_y_bins;
    to_index_offset += (long)to->_num_y_bins;
  }

  to->_error_status = FG_SUCCESSFUL;
  return 1;
}

// perform an insert process on to using the index and values of this
//int FloatGrid::inserter (Function5 function, FloatGrid *to) const
int FloatGrid::inserter (FloatGrid *to) const
{
// check suitability and compatibility
  if (!to) return 0;

  if (!(to->getUndefined() == _not_defined)) {
    to->_error_status = FG_SET_RANGE_ERROR;
    return 0;
  }

  long sub_start_y_bin = (long) getSubYBinStart ();
  long sub_start_x_bin = (long) _sub_start_x;
  long index_offset = sub_start_x_bin * (long)_num_y_bins + sub_start_y_bin;
  if (!(index_offset >= 0)) {  // make sure you start on this grid
    to->_error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

  if (!(to->setSubSize (_sub_x_bins, _sub_y_bins))) return 0;

  long to_sub_start_y_bin = (long) to->getSubYBinStart ();
  long to_sub_start_x_bin = (long) to->_sub_start_x;
  long to_index_offset  = to_sub_start_x_bin * (long)to->_num_y_bins
    + to_sub_start_y_bin;
  if (!(to_index_offset >= 0)) {  // make sure you start on the to grid
    to->_error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

// make sure you stop on this grid
  long max_index = (long)(_num_x_bins - 1) * (long)_num_y_bins
    + (long)(_num_y_bins - 1);
  long sub_max_index = (long)(sub_start_x_bin + _sub_x_bins - 1)
    * (long)_num_y_bins + (long)(sub_start_y_bin + _sub_y_bins - 1);
  if (!(sub_max_index <= max_index)) {
    to->_error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

// make sure you stop on the to grid
  long to_max_index = (long)(to->_num_x_bins - 1) * (long)to->_num_y_bins
    + (long)(to->_num_y_bins - 1);
  long to_sub_max_index = (long)(to_sub_start_x_bin + to->_sub_x_bins - 1)
    * (long)to->_num_y_bins + (long)(to_sub_start_y_bin
    + to->_sub_y_bins - 1);
  if (!(to_sub_max_index <= to_max_index)) {
    to->_error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

  long index, to_index;
  int k2, k3;
  for (k2 = 0; k2 < _sub_x_bins; k2++) {
    index = index_offset;
    to_index = to_index_offset;
    for (k3 = 0; k3 < _sub_y_bins; k3++) {
//      (to->*function) (to_index, _grid[index]);
      to->insertIndex (to_index, _grid[index]);
      index++;
      to_index++;
      if (to->_do_abort) {
        if (!(to_index % (long)2000)) {
          if (to->_do_abort->userAbort()) {
            to->_error_status = FG_USER_ABORTED;
            return 0;
          }
        }
      }
    }
    index_offset    += (long)_num_y_bins;
    to_index_offset += (long)to->_num_y_bins;
  }

  to->_error_status = FG_SUCCESSFUL;
  return 1;
}

// process this with a constant
int FloatGrid::process (Function2 function, float value)
{
  long sub_start_y_bin = (long) getSubYBinStart ();
  long sub_start_x_bin = (long) _sub_start_x;
  long index_offset = sub_start_x_bin * (long)_num_y_bins + sub_start_y_bin;
  if (!(index_offset >= 0)) {  // make sure you start on this grid
    _error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

// make sure you stop on this grid
  long max_index = (long)(_num_x_bins - 1) * (long)_num_y_bins
    + (long)(_num_y_bins - 1);
  long sub_max_index = (long)(sub_start_x_bin + _sub_x_bins - 1)
    * (long)_num_y_bins + (long)(sub_start_y_bin + _sub_y_bins - 1);
  if (!(sub_max_index <= max_index)) {
    _error_status = FG_INVALID_SUB_REGION;
    return 0;
  }

  long index;
  int k2, k3;
  for (k2 = 0; k2 < _sub_x_bins; k2++) {
    index = index_offset;
    for (k3 = 0; k3 < _sub_y_bins; k3++) {
      _grid[index] = (this->*function) (index, value);
      index++;
      if (_do_abort) {
        if (!(index % (long)2000)) {
          if (_do_abort->userAbort()) {
            _error_status = FG_USER_ABORTED;
            return 0;
          }
        }
      }
    }
    index_offset += (long)_num_y_bins;
  }

  _error_status = FG_SUCCESSFUL;
  return 1;
}

float FloatGrid::searchGridIndex (long index) const
{
  if (_grid[index] != _not_defined)
    return _grid[index]; // if data value already exists here do nothing

  int k2, num_hits = 0, weights_equal = 1;
  float numerator = 0;
  float denomenator = 0;
  float *gp = _grid + index;
  float value, last_hit_weight = 0;

// ignore the data point at the location in this algorithm
  for (k2 = 1; (k2 < _pattern_length) && ((num_hits < _max_hits) ||
    (weights_equal)); k2++) {
    value = *(gp+_pattern_offsets[k2]);
    if (value != _not_defined) {
      last_hit_weight = _pattern_weights[k2];
      numerator += last_hit_weight * value;
      denomenator += last_hit_weight;
      num_hits++;
    }

// by keeping track of this, all equally weighted data points will be
//   used in the gridding process
    if (k2+1 < _pattern_length)
      weights_equal = fabs ((double)last_hit_weight -
        (double)_pattern_weights[k2+1]) < EPSILON;
    else
      weights_equal = 0;
  }

  if (num_hits == 0)
    return _not_defined;
  else {
    value = numerator / denomenator;
    return value;
  }
}

float FloatGrid::convolveIndex (long index) const
{
// a more efficient algorithm would move the grid around the kernel
//   rather than the kernel around the grid

  if (_grid[index] == _not_defined)
    return _not_defined;   // do not convolve on top of an undefined data value

  int k2;
  float numerator = 0;
  float denomenator = 0;
  float *gp = _grid + index;
  float value;
  int num_hits = 0;

  for (k2 = 0; k2 < _pattern_length; k2++) {
    value = *(gp+_pattern_offsets[k2]);
    if (value != _not_defined) {
      numerator += _pattern_weights[k2] * value;
      denomenator += fabs ((double)_pattern_weights[k2]);
      num_hits++;
    }
  }

  if (num_hits == 0)
    return _not_defined;
  else {
    value = numerator / denomenator;
    return value;
  }
}

float FloatGrid::correlateIndex (long /*index*/) const
{
// compute local correlation coeficient oriented orthogonally to the
//   direction of most rapid change (i.e. along gradient vector).  This should
//   have the largest correlation coeficient.
// determine the local gradient at given index.
// rotate grid data about the index point to a direction perpendicular to
//   the gradient and output into a local array using bi-linear interpolation
// compute a correlation coeficient on the local array
  return 0;
}

int FloatGrid::initializeCorrelatorGradient ()
{
// Use pattern arrays to store kernels
// Initialize local gradient kernels.  Use the same sized gradient kernels
//   as the correlation kernel.  Weight kernel coeficients less the
//   farther they are from the given index
  return 0;
}

float FloatGrid::valueAtIndex (long index) const
{
  return _grid[index];
}

// this value needs to be validated at a higher level
float FloatGrid::constantValue (long /* index */, float value) const
{
  return value;
}

// the value 1 needs to be validated at a higher level
float FloatGrid::unity (long /* index */) const
{
  return (float)1;
}

// the value 0 needs to be validated at a higher level
float FloatGrid::zero (long /* index */) const
{
  return (float)0;
}

float FloatGrid::sqrtIndex (long index) const
{
  if (_grid[index] == _not_defined || _grid[index] < 0)
    return _not_defined;
  else
    return (float) ((double) sqrt ((double) _grid[index]));
}

float FloatGrid::divideIndex (long index, float value) const
{
  if (_grid[index] == _not_defined || value == _not_defined ||
    fabs((double)_grid[index]) < EPSILON)
    return _not_defined;
  else if (_grid[index] == value)
    return (float)1;
  else
    return (float) (value / _grid[index]);
}

float FloatGrid::divideValue (long index, float value) const
{
  if (_grid[index] == _not_defined)
    return _not_defined;
  else if (_grid[index] == value)
    return (float)1;
  else
    return (float) (_grid[index] / value);
}

float FloatGrid::multiplyIndex (long index, float value) const
{
  if (_grid[index] == _not_defined || value == _not_defined)
    return _not_defined;
  else
    return (float) (value * _grid[index]);
}

float FloatGrid::addIndex (long index, float value) const
{
  if (_grid[index] == _not_defined || value == _not_defined)
    return _not_defined;
  else
    return (float) (value + _grid[index]);
}

float FloatGrid::subtractIndex (long index, float value) const
{
  if (_grid[index] == _not_defined || value == _not_defined)
    return _not_defined;
  else
    return (float) (value - _grid[index]);
}

float FloatGrid::subtractValue (long index, float value) const
{
  if (_grid[index] == _not_defined)
    return _not_defined;
  else
    return (float) (_grid[index] - value);
}

float FloatGrid::rescaleIndex (long index) const
{
  if (_grid[index] == _not_defined)
    return _not_defined;
  else
    return (float) (_rescale_gain * _grid[index] + _rescale_bias);
}

float FloatGrid::rescaleIndexWithClip (long index) const
{
  if (_grid[index] == _not_defined)
    return _not_defined;
  else {
    float retval = (float) (_rescale_gain * _grid[index] + _rescale_bias);
    if (retval > _set_maximum)
      return _set_maximum;
    else if (retval < _set_minimum)
      return _set_minimum;
    else
      return retval;
  }
}

int FloatGrid::completelyUsed () const
{
  int retval =
    _sub_x_bins == _num_x_bins &&
    _sub_y_bins == _num_y_bins   ;
  return retval;
}

int FloatGrid::completelyCoveredBy (const FloatGrid *by) const
{
  int retval =
    by->_sub_x_bins == _num_x_bins &&
    by->_sub_y_bins == _num_y_bins   ;
  return retval;
}

int FloatGrid::copyHelper (const FloatGrid &from)
{
// check for copying self
  if (&from == this) return 1;

// copy constants
  _extrema_ready     = from._extrema_ready;
  _statistics_ready  = from._statistics_ready;
  _max_hits          = from._max_hits;
  _pattern_length    = from._pattern_length;
  _correl_x_reach    = from._correl_x_reach;
  _correl_y_reach    = from._correl_y_reach;
  _num_x_bins        = from._num_x_bins;
  _num_y_bins        = from._num_y_bins;
  _do_abort          = from._do_abort;
  _sub_start_x       = from._sub_start_x;
  _sub_start_y       = from._sub_start_y;
  _sub_x_bins        = from._sub_x_bins;
  _sub_y_bins        = from._sub_y_bins;
  _actual_minimum    = from._actual_minimum;
  _actual_maximum    = from._actual_maximum;
  _actual_average    = from._actual_average;
  _actual_std_dev    = from._actual_std_dev;
  _not_defined       = from._not_defined;
  _set_minimum       = from._set_minimum;
  _set_maximum       = from._set_maximum;
  _grid_not_defined  = from._grid_not_defined;
  _rescale_gain      = from._rescale_gain;
  _rescale_bias      = from._rescale_bias;
  _x_insert_size     = from._x_insert_size;
  _y_insert_size     = from._y_insert_size;
  _x_first_insert    = from._x_first_insert;
  _x_last_insert     = from._x_last_insert;
  _y_first_insert    = from._y_first_insert;
  _y_last_insert     = from._y_last_insert;
  _multi_insert      = from._multi_insert;
  _error_status      = from._error_status;
  if (_error_status != FG_SUCCESSFUL) return 0;

// set array pointers to NULL
  _grid = 0;
  _pattern_offsets = 0;
  _pattern_weights = 0;
  _col1 = 0;
  _col2 = 0;
  _across1 = 0;
  _across2 = 0;
  _awts1 = 0;
  _awts2 = 0;
  _down1 = 0;
  _down2 = 0;
  _dwts1 = 0;
  _dwts2 = 0;

// copy variable length arrays
  long k2;
  _grid = new float[from.getArraySize()];
  if (!_grid) {
    _error_status = FG_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  memcpy (_grid, from._grid, (size_t)from.getArraySize()*sizeof(float));

  if (_pattern_length > 0) {
    if (from._pattern_offsets) {
      _pattern_offsets = new long[_pattern_length];
      if (!_pattern_offsets) {
        _error_status = FG_MEMORY_ALLOCATION_ERROR;
        return 0;
      }
      for (k2 = 0; k2 < _pattern_length; k2++)
        _pattern_offsets[k2] = from._pattern_offsets[k2];
    }
    if (from._pattern_weights) {
      _pattern_weights = new float[_pattern_length];
      if (!_pattern_weights) {
        _error_status = FG_MEMORY_ALLOCATION_ERROR;
        return 0;
      }
      for (k2 = 0; k2 < _pattern_length; k2++)
        _pattern_weights[k2] = from._pattern_weights[k2];
    }
  }
  else {
    _pattern_offsets = 0;
    _pattern_weights = 0;
  }

  _error_status = FG_SUCCESSFUL;
  return 1;
}
