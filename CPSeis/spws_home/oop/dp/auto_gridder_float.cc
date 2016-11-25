// auto_gridder_float.cc:  Implementation file for AutoGridderFloat class
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
#include <stdio.h>
#include <limits.h>
#include <math.h>
#include "dp/auto_gridder_float.hh"
#include "dp/gridder_float.hh"
#include "dp/float_grid_accessor.hh"
#include "dp/float_grid.hh"
#include "dp/control_points.hh"

#define EPSILON 1.e-35

int x=0, y=1, a=2;

// constructor
AutoGridderFloat::AutoGridderFloat (int min_reach, int dont_resample,
  int dont_scale_a, int max_hits, int dont_smooth, int which_dimension):
  _min_reach             (min_reach),
  _dont_resample         (dont_resample),
  _dont_scale_a          (dont_scale_a),
  _max_hits              (max_hits),
  _dont_smooth           (dont_smooth),
  _which_dimension       (which_dimension),
  _error_status          (AGF_SUCCESSFUL)
{
// set pointers to NULL
  _array_x = 0;
  _array_y = 0;
  _array = 0;
  _small_grid = 0;
  _small_not_unique = 0;
  _large_grid = 0;
  _large_not_unique = 0;
  _gdr = 0;

// check for programming errors
  if (!(min_reach  >= 0 &&
        max_hits   >  0   ))
    _error_status = AGF_BAD_INPUTS;
}

// destructor
AutoGridderFloat::~AutoGridderFloat ()
{
  if (_array_x) delete [] _array_x, _array_x = 0;
  if (_array_y) delete [] _array_y, _array_y = 0;
  if (_array) delete [] _array, _array = 0;
  if (_small_grid && !_small_not_unique) delete _small_grid, _small_grid = 0;
  if (_large_grid && !_large_not_unique) delete _large_grid, _large_grid = 0;
  if (_gdr) delete _gdr, _gdr = 0;
}

// initialize grid calculations using the extrema of the given control
//   point object.  it is assumed that these control points support the
//   entire grid.

// analyze bin sizes and reaches using the entire set of control points
//   to be used for the initial gridding.  Also return the X- and Y-
//   reach distances required to collect subsets of the control-points
//   that will support a specific partial grid analysis.  By gathering
//   control points that reach these distances beyond a region of
//   interest, a gridded subwindow superscribing the region of interest
//   will be supported by the control-points.

// note that that the actual X- and Y-extrema found in the control points
//   completely determine the relationship between the (X,Y)'s and the
//   (X-bin,Y-bin)'s

int AutoGridderFloat::initializeCalculations (ControlPoints *cps,
  FloatGridAccessor *gridA, float *x_distance, float *y_distance)
{
  if (!(cps && gridA->getData() && x_distance && y_distance)) {
    _error_status = AGF_BAD_INPUTS;
    return 0;
  }

  FloatGrid *grid = gridA->getData ();
  _num_x_bins        = grid->getNumXBins ();
  _num_y_bins        = grid->getNumYBins ();

  _x_min_grid_center = gridA->getX ((int)0);
  _x_max_grid_center = gridA->getX (_num_x_bins-(int)1);
  _y_min_grid_center = gridA->getY ((int)0);
  _y_max_grid_center = gridA->getY (_num_y_bins-(int)1);

  _x_bin_size        = gridA->getXBinSize ();
  _y_bin_size        = gridA->getYBinSize ();

  if ((fabs((double)_x_bin_size) < EPSILON) ||
      (fabs((double)_y_bin_size) < EPSILON)   ) {
    _error_status = AGF_ZERO_DENOMINATOR;
    return 0;
  }

// compute the nominal bin minimums
  _x_bin_min = _x_min_grid_center - _x_bin_size / 2.0;
  _y_bin_min = _y_min_grid_center - _y_bin_size / 2.0;

  _x_to_y_ratio = (float)(fabs((double)(_x_bin_size/_y_bin_size)));
  if (_x_to_y_ratio < (float)EPSILON) {
    _error_status = AGF_ZERO_DENOMINATOR;
    return 0;
  }

// determine the bin reaches necessary to support the given control points
  int prev_dont_resample;
  if (_dont_resample) {
// temporarily assume that resampling will occur
    prev_dont_resample = _dont_resample;
    _dont_resample = 0;
  }
  else
    prev_dont_resample = 0;
// always determine reach distances as though resampling is to occur
  if (!(analyzeBinReach (cps))) return 0;
  *x_distance = _anal_x_distance;
  *y_distance = _anal_y_distance;
  if (prev_dont_resample)
    _dont_resample = prev_dont_resample; // set resampling flag as was

  return 1;
}

// utility to be used only after initializeCalculations and before use of
//    analyze
int AutoGridderFloat::setPointSize (FloatGrid *grid, float num_x_bins,
  float num_y_bins)
{
  int num_x_grid_bins, num_y_grid_bins;
  float x_bin_size, y_bin_size;

  if (_x_bin_size < 0)
    x_bin_size = -_x_bin_size;
  else
    x_bin_size = _x_bin_size;

  if (_y_bin_size < 0)
    y_bin_size = -_y_bin_size;
  else
    y_bin_size = _y_bin_size;

  if ((x_bin_size < EPSILON) ||
      (y_bin_size < EPSILON)   ) {
    _error_status = AGF_ZERO_DENOMINATOR;
    return 0;
    }

  if (_which_dimension != Y_ONLY)
    num_x_grid_bins = (int)(num_x_bins / x_bin_size + 0.5);
  else
    num_x_grid_bins = (int)(num_x_bins + 0.5);

  if (_which_dimension != X_ONLY)
    num_y_grid_bins = (int)(num_y_bins / y_bin_size + 0.5);
  else
    num_y_grid_bins = (int)(num_y_bins + 0.5);

  grid->setInsertSize (num_x_grid_bins, num_y_grid_bins);

  return 1;
}

// utility to initialize insertion only

// no *x_distance and *y_distance are given because it is assumed that the
//   given x_bin_size and y_bin_size is in fact these distances
int AutoGridderFloat::initializeInsert (FloatGrid *grid,
  float x_min_grid_center, float x_max_grid_center, float y_min_grid_center,
  float y_max_grid_center, float x_bin_size, float y_bin_size)
{
  _x_min_grid_center = x_min_grid_center;
  _x_max_grid_center = x_max_grid_center;
  _y_min_grid_center = y_min_grid_center;
  _y_max_grid_center = y_max_grid_center;

  _num_x_bins        = grid->getNumXBins ();
  _num_y_bins        = grid->getNumYBins ();

// compute the nominal bin sizes
  _x_bin_size = (_x_max_grid_center - _x_min_grid_center)
    / (float) (_num_x_bins - 1);
  _y_bin_size = (_y_max_grid_center - _y_min_grid_center)
    / (float) (_num_y_bins - 1);
  if ((fabs((double)_x_bin_size) < EPSILON) ||
      (fabs((double)_y_bin_size) < EPSILON)   ) {
    _error_status = AGF_ZERO_DENOMINATOR;
    return 0;
  }

// compute the nominal bin minimums
  _x_bin_min = _x_min_grid_center - _x_bin_size / 2.0;
  _y_bin_min = _y_min_grid_center - _y_bin_size / 2.0;

  _anal_x_bin_size = x_bin_size;
  _anal_y_bin_size = y_bin_size;

  return 1;
}

// utility to determine the average bin size for a set of control points
int AutoGridderFloat::findAverageBinSizes (ControlPoints *cps)
{

// 2d data is easiest
  if (_which_dimension != BOTH_X_AND_Y) return findSingleAveBinSize (cps);

// the following simplistic solution was adopted for expedience
//   it assumes full random distribution of points
  float x_bin_range =   (cps->getMaximum(x) - cps->getMinimum(x))
                      / (_x_max_grid_center - _x_min_grid_center)
                    * (float)_num_x_bins;
  if (x_bin_range < 0) x_bin_range *= (float)-1;
  float y_bin_range =   (cps->getMaximum(y) - cps->getMinimum(y))
                      / (_y_max_grid_center - _y_min_grid_center)
                      * (float)_num_y_bins;
  if (y_bin_range < 0) y_bin_range *= (float)-1;

  float denominator, x_reduction_factor, y_reduction_factor;

// a fudge factor of pi/2 seems to tend toward an unbiased result!!!???
#define PI_BY_2 1.572963268
  denominator = (float) ((double) sqrt ((double)cps->getCount())
    * PI_BY_2);
  if (denominator < EPSILON) {
    _error_status = AGF_ZERO_DENOMINATOR;
    return 0;
  }
  x_reduction_factor = x_bin_range / denominator;
  y_reduction_factor = y_bin_range / denominator;

  if (x_reduction_factor < 1)
    _anal_x_bin_size = _x_bin_size;
  else
    _anal_x_bin_size = _x_bin_size * x_reduction_factor;

  if (y_reduction_factor < 1)
    _anal_y_bin_size = _y_bin_size;
  else
    _anal_y_bin_size = _y_bin_size * y_reduction_factor;

  if ((fabs((double)_anal_x_bin_size) < EPSILON) ||
      (fabs((double)_anal_y_bin_size) < EPSILON)   ) {
    _error_status = AGF_ZERO_DENOMINATOR;
    return 0;
  }

  int test = 0;
  if (test == 0) return 1;

// the following more involved solution was rejected due to unresolved details
//   in the future you might want to replace this with a clustering compression
//   algorithm that reduces the number of control points down to a few hundred
//   and then look at the average delta-x's and delta-y's

  int x=0, y=1;
  if (!(cps && cps->getCount() > 0)) {
    _error_status = AGF_BAD_INPUTS;
    return 0;
  }
  _array_x = new int[cps->getCount()];
  _array_y = new int[cps->getCount()];
  if (!(_array_x && _array_y)) {
    _error_status = AGF_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  float gain_x = x_bin_range / (cps->getMaximum(x) - cps->getMinimum(x));
  float bias_x = -gain_x * cps->getMinimum(x) + 0.5;
  float gain_y = y_bin_range / (cps->getMinimum(y) - cps->getMaximum(y));
  float bias_y = -gain_y * cps->getMinimum(y) + 0.5;

// store the first control points in the _arrays
  _array_x[0] = (int) (gain_x * cps->get (x, 0) + bias_x);
  _array_y[0] = (int) (gain_y * cps->get (y, 0) + bias_y);
  for (long k2 = 1; k2 < cps->getCount(); k2++){// loop through rest cntl pnts
    _array_x[k2] = (int) (gain_x * cps->getNext(x) + bias_x);
    _array_y[k2] = (int) (gain_y * cps->getNext(y) + bias_y);
  }

  double threshold_pcnt = 0.80;
  float ave_diff = findAverageDifference (cps->getCount(), _array_x, _array_y,
    threshold_pcnt*(double)y_bin_range);
  if (failed()) return 0;
  if (ave_diff == 1.0)
    _anal_x_bin_size = _x_bin_size;
  else {
    if (fabs ((double)gain_x) < EPSILON) {
      _error_status = AGF_ZERO_DENOMINATOR;
      return 0;
    }
    _anal_x_bin_size = ave_diff / gain_x;
  }

  ave_diff = findAverageDifference (cps->getCount(), _array_y, _array_x,
    threshold_pcnt*(double)x_bin_range);
  if (failed()) return 0;
  if (ave_diff == 1.0)
    _anal_y_bin_size = _y_bin_size;
  else {
    if (fabs ((double)gain_y) < EPSILON) {
      _error_status = AGF_ZERO_DENOMINATOR;
      return 0;
    }
    _anal_y_bin_size = ave_diff / gain_y;
  }

  delete [] _array_x, _array_x = 0;
  delete [] _array_y, _array_y = 0;

  return 1;
}

float AutoGridderFloat::findAverageDifference (long count, int *array1,
  int *array2, double threshold2)
{
// sort array1 in DESCENDING order
  sortIArrays (0, count-1, array1, array2);

// sort all tieing X's with the Y's in DESCENDING order
  sortJArrays (count, array1, array2);

// compute a series of averages for array1 values.  Use as a
//   demarcation between averages, the point where array2 values
//   make a "significant" jump and array1 values are at least different
//   from the first value in the group.
  _array = new float[count];
  if (!_array) {
    _error_status = AGF_MEMORY_ALLOCATION_ERROR;
    return (float)0;
  }
  long k2;
  for (k2 = 1; k2 < count; k2++)
    _array[k2] = 0;
  long array_count = 0;
  long average_count = 0;
  int diff1, threshold1 = 1;
  double ddiff2;
  int array1_1 = array1[0];
  long k3;
  for (k3 = 1; k3 < count; k3++) {
     diff1 = array1_1 - array1[k3];
    ddiff2 = (double)array2[k3-1] - (double)array2[k3];
    _array[array_count] += (float) array1[k3-1];
    average_count++;
    if (diff1 >= threshold1 && fabs(ddiff2) >= threshold2) {
      _array[array_count] /= (float) average_count;
      average_count = 0;
      array_count++;
      array1_1 = array1[k3];
    }
  }

// take care of the last _array element.  Note:  array_count is zero-relative
  _array[array_count] += (float) array1[count-1];
  average_count++;
  _array[array_count] /= (float) average_count;

  if (array_count < 1) {
    delete [] _array, _array = 0;
    return 1.0;
  }
// average the differences in the list
  float diff = 0.;
  for (k2 = 1; k2 <= array_count; k2++)
    diff += _array[k2-1] - _array[k2];
  diff /= (float) array_count;

  delete [] _array, _array = 0;

// should the average difference >= 1.05 return
//   the average, otherwise return 1.0
  if (diff >= 1.05)
    return diff;
  else
    return 1.0;
}

// analyze bin sizes and reaches for a set of control points
int AutoGridderFloat::analyzeBinReach (ControlPoints *cps)
{
// check for programming errors
  if (!cps) {
    _error_status = AGF_BAD_INPUTS;
    return 0;
  }

  if (_dont_resample) {

// set the bin reaches so as to equalize the area examined
    _anal_x_bin_size = _x_bin_size;
    _anal_y_bin_size = _y_bin_size;
    if (_x_to_y_ratio >= (float)1) {
      _anal_x_bin_reach = _min_reach;
      if (_which_dimension != X_ONLY)
        _anal_y_bin_reach = (int) ((float)_min_reach * _x_to_y_ratio
                          + (float)0.5);
      else
        _anal_y_bin_reach = _anal_x_bin_reach;
    }
    else /* if (_x_to_y_ratio) < (float)1) */ {
      _anal_y_bin_reach = _min_reach;
      if (_which_dimension != Y_ONLY)
        _anal_x_bin_reach = (int) ((float)_min_reach / _x_to_y_ratio
                          + (float)0.5);
      else
        _anal_x_bin_reach = _anal_y_bin_reach;
    }
  }
  else /* if (!_dont_resample) */ {

// set the bin sizes so as to minimize the grid array size
    if (!(findAverageBinSizes (cps))) return 0;

    float anal_x_to_y_ratio;
    if (_which_dimension != BOTH_X_AND_Y)
      anal_x_to_y_ratio = 1;
    else
      anal_x_to_y_ratio =
        (float)fabs((double)(_anal_x_bin_size/_anal_y_bin_size));

    if (anal_x_to_y_ratio < (float)EPSILON) {
      _error_status = AGF_ZERO_DENOMINATOR;
      return 0;
    }

// set the bin reaches so as to equalize the area examined
    if (anal_x_to_y_ratio >= (float)1) {
      _anal_x_bin_reach = _min_reach;
      _anal_y_bin_reach = (int)((float)_min_reach * anal_x_to_y_ratio + 0.5);
    }
    else /* if (anal_x_to_y_ratio < 1) */ {
      _anal_y_bin_reach = _min_reach;
      _anal_x_bin_reach =  (int) ((float)_min_reach / anal_x_to_y_ratio + 0.5);
    }
  }

  if ((fabs((double)_anal_x_bin_size) < EPSILON) ||
      (fabs((double)_anal_y_bin_size) < EPSILON)   ) {
    _error_status = AGF_ZERO_DENOMINATOR;
    return 0;
  }

  _x_resampling_required = (((int) (_x_bin_size      * 10000. + 0.5)) !=
                            ((int) (_anal_x_bin_size * 10000. + 0.5))   );
  _y_resampling_required = (((int) (_y_bin_size      * 10000. + 0.5)) !=
                            ((int) (_anal_y_bin_size * 10000. + 0.5))   );

// By gathering control points that reach these distances beyond a region of
//   interest, a gridded subwindow superscribing the region of interest
//   will be supported by the control-points.
  _anal_x_distance = (float)(fabs((double)_anal_x_bin_size))
                   * (float) _anal_x_bin_reach;
  _anal_y_distance = (float)(fabs((double)_anal_y_bin_size))
                   * (float) _anal_y_bin_reach;

// determine the number of cols and rows that the reach distance covers
//   in output grid coordinates
  _out_x_bin_reach = (int) (_anal_x_distance
                   / (float)(fabs((double)_x_bin_size)) + (float)0.5);
  _out_y_bin_reach = (int) (_anal_y_distance
                   / (float)(fabs((double)_y_bin_size)) + (float)0.5);

  return 1;
}

// it is assumed that the grid is organized as follows:
//   the first element is the top left corner of the grid.
//   progression through the grid goes first from top to bottom and
//   then from left to right.
// contrariwise, it is assumed that the grid dimension's origin is at
//   the bottom left corner of the grid.  Increasing X goes from left
//   to right while increasing Y goes from bottom to top.
// it is critical to note that Y is reversed in the two coordinate systems

// grid over a given set of control points
int AutoGridderFloat::analyze (ControlPoints *cps, FloatGrid *grid)
{
// check for programming errors
  if (!(cps && grid)) {
    _error_status = AGF_BAD_INPUTS;
    return 0;
  }

// find the nominal grid bounds for the control-point data
  int tmp;
  int dmnx = (int) ((cps->getMinimum (x) - _x_bin_min) / _x_bin_size);
  int dmxx = (int) ((cps->getMaximum (x) - _x_bin_min) / _x_bin_size);
  if (dmnx > dmxx) {
    tmp = dmnx;
    dmnx = dmxx;
    dmxx = tmp;
  }

  int dmny = (int) ((cps->getMinimum (y) - _y_bin_min) / _y_bin_size);
  int dmxy = (int) ((cps->getMaximum (y) - _y_bin_min) / _y_bin_size);
  if (dmny > dmxy) {
    tmp = dmny;
    dmny = dmxy;
    dmxy = tmp;
  }

  if (dmnx < 0) dmnx = 0;
  if (dmxx >= _num_x_bins) dmxx = _num_x_bins - 1;
  if (dmny < 0) dmny = 0;
  if (dmxy >= _num_y_bins) dmxy = _num_y_bins - 1;

  float x_min_bound_center = _x_bin_min + ((float)dmnx + 0.5) * _x_bin_size;
  float x_max_bound_center = _x_bin_min + ((float)dmxx + 0.5) * _x_bin_size;
  float y_min_bound_center = _y_bin_min + ((float)dmny + 0.5) * _y_bin_size;
  float y_max_bound_center = _y_bin_min + ((float)dmxy + 0.5) * _y_bin_size;

  int output_num_x_bins = dmxx - dmnx + (int)1;
  if (output_num_x_bins < (int)1) return (int)1;

  int output_num_y_bins = dmxy - dmny + (int)1;
  if (output_num_y_bins < (int)1) return (int)1;

// determine the number of bins to grid
  int anal_num_x_bins, anal_num_y_bins;
  if (!(analyzeBinReach (cps))) return 0;
  if (_x_resampling_required)
    anal_num_x_bins = (int)((x_max_bound_center - x_min_bound_center)
                    / _anal_x_bin_size + (float)1.5);
  else
    anal_num_x_bins = output_num_x_bins;

  if (_y_resampling_required)
    anal_num_y_bins = (int)((y_max_bound_center - y_min_bound_center)
                    / _anal_y_bin_size + (float)1.5);
  else
    anal_num_y_bins = output_num_y_bins;

// compute the bin minimums to properly grid the control-point data
  float anal_x_bin_min = x_min_bound_center - _anal_x_bin_size / 2.0;
  float anal_y_bin_min = y_min_bound_center - _anal_y_bin_size / 2.0;

// check to see if its necessary to resample the gridded control-point
//   data to fit into the output grid
  int resampling_required = _x_resampling_required || _y_resampling_required;

// check to see if the given control-point data fails to span the entire
//   output grid range (i.e. is only a subset gridded?)
  int full_x_size = dmnx == 0 && dmxx == _num_x_bins-1;
  int full_y_size = dmny == 0 && dmxy == _num_y_bins-1;
  int full_size = full_x_size && full_y_size;

// based on resampling required and whether or not only a subset of the the
//   output grid is being modified, set up the pointers
  if      (!full_size && resampling_required)     {
    
    _small_grid = new FloatGrid (anal_num_x_bins, anal_num_y_bins);
    if (!_small_grid) {
      _error_status = AGF_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_small_grid->failed()) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }
    if (!(_small_grid->setRange (grid->getUndefined(), grid->getMinimum(),
      grid->getMaximum()))) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }
    _large_grid = new FloatGrid (output_num_x_bins, output_num_y_bins);
    if (!_large_grid) {
      _error_status = AGF_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_large_grid->failed()) {
      _error_status = _large_grid->errorStatus ();
      return 0;
    }
  }
  else if (full_size  && resampling_required)     {
    _small_grid = new FloatGrid (anal_num_x_bins, anal_num_y_bins);
    if (!_small_grid) {
      _error_status = AGF_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_small_grid->failed()) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }
    if (!(_small_grid->setRange (grid->getUndefined(), grid->getMinimum(),
      grid->getMaximum()))) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }
    _large_grid = grid;
    _large_not_unique = 1;
  }
  else if (!full_size && !resampling_required)    {
    _small_grid = new FloatGrid (anal_num_x_bins, anal_num_y_bins);
    if (!_small_grid) {
      _error_status = AGF_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_small_grid->failed()) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }
    _small_grid->setInsertSize (grid->getXInsertSize(),
      grid->getYInsertSize());

    if (!(_small_grid->setRange (grid->getUndefined(), grid->getMinimum(),
      grid->getMaximum()))) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }
    _large_grid = _small_grid;
    _large_not_unique = 1;
  }
  else /* (full_size  && !resampling_required) */ {
    _small_grid = grid;
    _small_not_unique = 1;
  }

  if (_gdr) delete _gdr, _gdr = 0;
  _gdr = new GridderFloat ();
  if (!_gdr) {
    _error_status = AGF_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  if (!(_gdr->setReach (_anal_x_bin_reach, _anal_y_bin_reach))) {
    _error_status = _gdr->errorStatus ();
    return 0;
  }
  _gdr->setOrigin  (anal_x_bin_min, anal_y_bin_min);
  _gdr->setBinSize (_anal_x_bin_size, _anal_y_bin_size);
  if (!(_gdr->setMaxHits (_max_hits))) {
    _error_status = _gdr->errorStatus ();
    return 0;
  }
  if (!(_gdr->doGridding (cps, _small_grid))) {
    _error_status = _gdr->errorStatus ();
    return 0;
  }

  delete _gdr, _gdr = 0;

// convert the small grid to have the desired range of values if full coverage
  if (!_dont_scale_a && full_size)
    if (!(_small_grid->convert())) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }

  if (resampling_required)
// resample from the small grid to the large grid
    if (!(_small_grid->resample (_large_grid))) {
      _error_status = _large_grid->errorStatus ();
      return 0;
    }

// copy the resampled and gridded control-point data into the output grid -
//   take special care to shave off the grid border that probably will not be
//   supported by the given control points.  Return the modified region of
//   interest
  int first_x, first_y, imnx, imny;
  if (full_size) {
    first_x = 0;
    _column_count = _num_x_bins;
    first_y = 0;
    _row_count = _num_y_bins;
  }
  else if (_which_dimension == X_ONLY) {

    if (full_x_size) {
      imnx = 0;
      first_x = 0;
      _column_count = _num_x_bins;
    }

    else {

// take care of the starting column case
      imnx = 0;  // use the first index
      _column_count = output_num_x_bins; // partial solution
      first_x = dmnx;  // where this index occurs in the final grid
    }

// take care of the starting and ending row case
    imny = 0;
    first_y = dmny;
    _row_count = 1;

    if (_column_count > 0) {
      if (!(_large_grid->setSubSize (_column_count, _row_count))) {
        _error_status = _large_grid->errorStatus ();
        return 0;
      }
      if (!(_large_grid->setSubStart (imnx, imny))) {
        _error_status = _large_grid->errorStatus ();
        return 0;
      }
      if (!(grid->setSubStart (first_x, first_y))) {
        _error_status = grid->errorStatus ();
        return 0;
      }
      if (!(_large_grid->moveWithInsert (grid))) {
        _error_status = grid->errorStatus ();
        return 0;
      }
    }
  }
  else if (_which_dimension == Y_ONLY) {

// take care of the starting and ending column case
    imnx = 0;
    first_x = dmnx;
    _column_count = 1;

    if (full_y_size) {
      imny = 0;
      first_y = dmny;
      _row_count = _num_y_bins;
    }
    else {

// take care of the starting row case
      imny = 0;  // use the first index
      _row_count = output_num_y_bins; // partial solution
      first_y = dmny;  // where this index occurs in the final grid
    }

    if (_row_count > 0) {
      if (!(_large_grid->setSubSize (_column_count, _row_count))) {
        _error_status = _large_grid->errorStatus ();
        return 0;
      }
      if (!(_large_grid->setSubStart (imnx, imny))) {
        _error_status = _large_grid->errorStatus ();
        return 0;
      }
      if (!(grid->setSubStart (first_x, first_y))) {
        _error_status = grid->errorStatus ();
        return 0;
      }
      if (!(_large_grid->moveWithInsert (grid))) {
        _error_status = grid->errorStatus ();
        return 0;
      }
    }
  }
  else { // case that the full size was not gridded

// it is assumed that there are no control points beyond the known boundaries

// take care of the starting column case

// Following is a more liberal gridding test when not enough data is there
//  if (dmnx >= _out_x_bin_reach              &&  // this is a switch for only
//      output_num_x_bins >= _out_x_bin_reach   ) //  doing part of the columns

    if (dmnx >= _out_x_bin_reach)                 // this is a switch for only
                                                  //  doing part of the columns
    {
// not on the near-X boundary

// the following is the first index adequately supported by the control points
//   in the large grid just processed
      imnx = _out_x_bin_reach;
// the following is the first part of the solution to determine the number
//   of columns that are adequately supported by the control points
      _column_count = output_num_x_bins - _out_x_bin_reach; // partial solution
// the following is the first index adequately supported by the control points
//   in the final grid to return
      first_x = dmnx + _out_x_bin_reach;
    }
    else {  // note what the switch is for doing the entire set of columns

// on the near-X boundary

      imnx = 0;  // use the first index
      _column_count = output_num_x_bins; // partial solution
      first_x = dmnx;  // where this index occurs in the final grid
    }

// take care of the ending column case

// Following is a more liberal gridding test when not enough data is there
//  if (dmxx < _num_x_bins-_out_x_bin_reach-1 &&  // this is a switch for doing
//      _column_count >= _out_x_bin_reach       ) //   part of the columns

    if (dmxx < _num_x_bins-_out_x_bin_reach-1)    // this is a switch for doing
                                                  //   part of the columns

// not on the far-X boundary

// the following completes the solution to determine the number
//   of columns that are adequately supported by the control points
      _column_count -= _out_x_bin_reach;

// take care of the starting row case

// Following is a more liberal gridding test when not enough data is there
//  if (dmny >= _out_y_bin_reach              &&  // this is a switch for only
//      output_num_y_bins >= _out_y_bin_reach   ) //   doing part of the rows

    if (dmny >= _out_y_bin_reach)                 // this is a switch for only
                                                  //   doing part of the rows
    {
// not on the near-Y boundary

// the following is the first index adequately supported by the control points
//   in the large grid just processed
      imny = _out_y_bin_reach;
// the following is the first part of the solution to determine the number
//   of rows that are adequately supported by the control points
      _row_count = output_num_y_bins - _out_y_bin_reach; // partial solution
// the following is the first index adequately supported by the control points
//   in the final grid to return
      first_y = dmny + _out_y_bin_reach;
    }
    else {  // note what the switch for doing the entire set of rows

// on the near-Y boundary

      imny = 0;  // use the first index
      _row_count = output_num_y_bins; // partial solution
      first_y = dmny;  // where this index occurs in the final grid
    }

// take care of the ending row case

// Following is a more liberal gridding test when not enough data is there
//  if (dmxy < _num_y_bins-_out_y_bin_reach-1 &&  // this is a switch for doing
//      _row_count >= _out_y_bin_reach          ) //   part of the rows

    if (dmxy < _num_y_bins-_out_y_bin_reach-1)    // this is a switch for doing
                                                  //   part of the rows

// not on the far-Y boundary

// the following completes the solution to determine the number
//   of rows that are adequately supported by the control points
      _row_count -= _out_y_bin_reach;

    if (_column_count > 0 && _row_count > 0) {
      if (!(_large_grid->setSubSize (_column_count, _row_count))) {
        _error_status = _large_grid->errorStatus ();
        return 0;
      }
      if (!(_large_grid->setSubStart (imnx, imny))) {
        _error_status = _large_grid->errorStatus ();
        return 0;
      }
      if (!(grid->setSubStart (first_x, first_y))) {
        _error_status = grid->errorStatus ();
        return 0;
      }
      if (!(_large_grid->move (grid))) {
        _error_status = grid->errorStatus ();
        return 0;
      }
    }
  }

  _first_column = grid->getSubXBinInsertStart ();
  _first_row    = grid->getSubYBinInsertStart ();

// require the extrema to be redetermined for the incoming grid
  grid->resetExtremaReadyFlag ();

// reinsert the given control points if not converting the values already
  if ((_dont_scale_a || !full_size) && _dont_smooth)
    if (!(insertControlPoints (cps, grid))) return 0;

// based on resampling required and whether or not only a subset of the the
//   output grid is being modified, delete temporary grid objects
  if      (!full_size &&  resampling_required)     {
    delete _large_grid, _large_grid = 0;
    delete _small_grid, _small_grid = 0;
  }
  else if ((full_size &&  resampling_required) ||
           (!full_size && !resampling_required)   ) {
    delete _small_grid, _small_grid = 0;
  }

  if (_small_not_unique) {
    _small_grid = 0;
    _small_not_unique = 0;
  }
  if (_large_not_unique) {
    _large_grid = 0;
    _large_not_unique = 0;
  }

  return 1;
}

// it is assumed that the grid is organized as follows:
//   the first element is the top left corner of the grid.
//   progression through the grid goes first from top to bottom and
//   then from left to right.
// contrariwise, it is assumed that the grid dimension's origin is at
//   the bottom left corner of the grid.  Increasing X goes from left
//   to right while increasing Y goes from bottom to top.
// it is critical to note that Y is reversed in the two coordinate systems

// insert over a given set of control points
int AutoGridderFloat::insert (ControlPoints *cps, FloatGrid *grid)
{
// check for programming errors
  if (!(cps && grid)) {
    _error_status = AGF_BAD_INPUTS;
    return 0;
  }

// find the nominal grid bounds for the control-point data
  int tmp;
  int dmnx = (int) ((cps->getMinimum (x) - _x_bin_min) / _x_bin_size);
  int dmxx = (int) ((cps->getMaximum (x) - _x_bin_min) / _x_bin_size);
  if (dmnx > dmxx) {
    tmp = dmnx;
    dmnx = dmxx;
    dmxx = tmp;
  }

  int dmny = (int) ((cps->getMinimum (y) - _y_bin_min) / _y_bin_size);
  int dmxy = (int) ((cps->getMaximum (y) - _y_bin_min) / _y_bin_size);
  if (dmny > dmxy) {
    tmp = dmny;
    dmny = dmxy;
    dmxy = tmp;
  }

  if (dmnx < 0) dmnx = 0;
  if (dmxx >= _num_x_bins) dmxx = _num_x_bins - 1;
  if (dmny < 0) dmny = 0;
  if (dmxy >= _num_y_bins) dmxy = _num_y_bins - 1;

  float x_min_bound_center = _x_bin_min + ((float)dmnx + 0.5) * _x_bin_size;
  float x_max_bound_center = _x_bin_min + ((float)dmxx + 0.5) * _x_bin_size;
  float y_min_bound_center = _y_bin_min + ((float)dmny + 0.5) * _y_bin_size;
  float y_max_bound_center = _y_bin_min + ((float)dmxy + 0.5) * _y_bin_size;

  int output_num_x_bins = dmxx - dmnx + 1;
  if (output_num_x_bins < 1) return 1;

  int output_num_y_bins = dmxy - dmny + 1;
  if (output_num_y_bins < 1) return 1;

// determine the number of bins to insert
  int anal_num_x_bins, anal_num_y_bins;

  _x_resampling_required = fabs ((double)(_anal_x_bin_size - _x_bin_size))
    > 0.5 / (double)_num_x_bins;
  if (_x_resampling_required)
    anal_num_x_bins = (int) ((x_max_bound_center - x_min_bound_center)
      / _anal_x_bin_size + 1.5);
  else
    anal_num_x_bins = output_num_x_bins;

  _y_resampling_required = fabs ((double)(_anal_y_bin_size - _y_bin_size))
    > 0.5 / (double)_num_y_bins;
  if (_y_resampling_required)
    anal_num_y_bins = (int) ((y_max_bound_center - y_min_bound_center)
      / _anal_y_bin_size + 1.5);
  else
    anal_num_y_bins = output_num_y_bins;

// compute the bin minimums to properly insert the control-point data
  float anal_x_bin_min = x_min_bound_center - _anal_x_bin_size / 2.0;
  float anal_y_bin_min = y_min_bound_center - _anal_y_bin_size / 2.0;

// check to see if its necessary to resample the gridded control-point
//   data to fit into the output grid
  int resampling_required = _x_resampling_required || _y_resampling_required;

// check to see if the given control-point data fails to span the entire
//   output grid range (i.e. is only a subset gridded?)
  int full_x_size = dmnx == 0 && dmxx == _num_x_bins-1;
  int full_y_size = dmny == 0 && dmxy == _num_y_bins-1;
  int full_size = full_x_size && full_y_size;

// based on resampling required and whether or not only a subset of the the
//   output grid is being modified, set up the pointers
  if      (!full_size && resampling_required)     {
    
    _small_grid = new FloatGrid (anal_num_x_bins, anal_num_y_bins);
    if (!_small_grid) {
      _error_status = AGF_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_small_grid->failed()) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }
    if (!(_small_grid->setRange (grid->getUndefined(), grid->getMinimum(),
      grid->getMaximum()))) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }
    _large_grid = new FloatGrid (output_num_x_bins, output_num_y_bins);
    if (!_large_grid) {
      _error_status = AGF_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_large_grid->failed()) {
      _error_status = _large_grid->errorStatus ();
      return 0;
    }
  }
  else if (full_size  && resampling_required)     {
    _small_grid = new FloatGrid (anal_num_x_bins, anal_num_y_bins);
    if (!_small_grid) {
      _error_status = AGF_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_small_grid->failed()) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }
    if (!(_small_grid->setRange (grid->getUndefined(), grid->getMinimum(),
      grid->getMaximum()))) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }
    _large_grid = grid;
    _large_not_unique = 1;
  }
  else if (!full_size && !resampling_required)    {
    _small_grid = new FloatGrid (anal_num_x_bins, anal_num_y_bins);
    if (!_small_grid) {
      _error_status = AGF_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_small_grid->failed()) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }
    if (!(_small_grid->setRange (grid->getUndefined(), grid->getMinimum(),
      grid->getMaximum()))) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }
    _large_grid = _small_grid;
    _large_not_unique = 1;
  }
  else /* (full_size  && !resampling_required) */ {
    _small_grid = grid;
    _small_not_unique = 1;
  }

  if (_gdr) delete _gdr, _gdr = 0;
  _gdr = new GridderFloat ();
  if (!_gdr) {
    _error_status = AGF_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  _gdr->setOrigin  (anal_x_bin_min, anal_y_bin_min);
  _gdr->setBinSize (_anal_x_bin_size, _anal_y_bin_size);
  if (!(_gdr->doInserting (cps, _small_grid))) {
    _error_status = _gdr->errorStatus ();
    return 0;
  }

  delete _gdr, _gdr = 0;

// convert the small grid to have the desired range of values if full coverage
  if (!_dont_scale_a && full_size)
    if (!(_small_grid->convert())) {
      _error_status = _small_grid->errorStatus ();
      return 0;
    }

  if (resampling_required) {
// resample from the small grid to the large grid
    int NN_only = 1;
    if (!(_small_grid->resample (_large_grid, NN_only))) {
      _error_status = _large_grid->errorStatus ();
      return 0;
    }
  }

// copy the resampled and gridded control-point data into the output grid -
  int first_x, first_y;
  if (full_size) {
    first_x = 0;
    _column_count = _num_x_bins;
    first_y = 0;
    _row_count = _num_y_bins;
  }
  else if (_which_dimension == X_ONLY) {
    if (full_x_size) {
      first_x = 0;
      _column_count = _num_x_bins;
    }
    else {
      first_x = dmnx;
      _column_count = output_num_x_bins;
    }
    first_y = 0;
    _row_count = 1;
    if (_column_count > 0) {
      if (!(_large_grid->setSubSize (_column_count, _row_count))) {
        _error_status = _large_grid->errorStatus ();
        return 0;
      }
      if (!(_large_grid->setSubStart (0, 0))) {
        _error_status = _large_grid->errorStatus ();
        return 0;
      }
      if (!(grid->setSubStart (first_x, first_y))) {
        _error_status = grid->errorStatus ();
        return 0;
      }
      if (!(_large_grid->moveWithInsert (grid))) {
        _error_status = grid->errorStatus ();
        return 0;
      }
    }
  }
  if (_which_dimension == Y_ONLY) {
    first_x = 0;
    _column_count = 1;
    if (full_y_size) {
      first_y = 0;
      _row_count = _num_y_bins;
    }
    else {
      first_y = dmny;
      _row_count = output_num_y_bins;
    }
    if (_row_count > 0) {
      if (!(_large_grid->setSubSize (_column_count, _row_count))) {
        _error_status = _large_grid->errorStatus ();
        return 0;
      }
      if (!(_large_grid->setSubStart (0, 0))) {
        _error_status = _large_grid->errorStatus ();
        return 0;
      }
      if (!(grid->setSubStart (first_x, first_y))) {
        _error_status = grid->errorStatus ();
        return 0;
      }
      if (!(_large_grid->moveWithInsert (grid))) {
        _error_status = grid->errorStatus ();
        return 0;
      }
    }
  }
  else { // case that the full size was not inserted
    first_x = dmnx;          // where this index occurs in the final grid
    _column_count = output_num_x_bins;
    first_y = dmny;             // where this index occurs in the final grid
    _row_count = output_num_y_bins;

    if (_column_count > 0 && _row_count > 0) {
      if (!(_large_grid->setSubSize (_column_count, _row_count))) {
        _error_status = _large_grid->errorStatus ();
        return 0;
      }
      if (!(_large_grid->setSubStart (0, 0))) {
        _error_status = _large_grid->errorStatus ();
        return 0;
      }
      if (!(grid->setSubStart (first_x, first_y))) {
        _error_status = grid->errorStatus ();
        return 0;
      }
      if (!(_large_grid->move (grid))) {
        _error_status = grid->errorStatus ();
        return 0;
      }
    }
  }

  _first_column = grid->getSubXBinInsertStart ();
  _first_row    = grid->getSubYBinInsertStart ();

// require the extrema to be redetermined for the incoming grid
  grid->resetExtremaReadyFlag ();

// based on resampling required and whether or not only a subset of the the
//   output grid is being modified, delete temporary grid objects
  if      (!full_size &&  resampling_required)     {
    delete _large_grid, _large_grid = 0;
    delete _small_grid, _small_grid = 0;
  }
  else if ((full_size &&  resampling_required) ||
           (!full_size && !resampling_required)   )
    delete _small_grid, _small_grid = 0;

  if (_small_not_unique) {
    _small_grid = 0;
    _small_not_unique = 0;
  }
  if (_large_not_unique) {
    _large_grid = 0;
    _large_not_unique = 0;
  }

  return 1;
}

// insert a set of control points into a grid
int AutoGridderFloat::insertControlPoints (ControlPoints *cps,
  FloatGrid *grid)
{
  if (_gdr) delete _gdr, _gdr = 0;
  _gdr = new GridderFloat ();
  if (!_gdr) {
    _error_status = AGF_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  _gdr->setOrigin   (_x_bin_min,  _y_bin_min);
  _gdr->setBinSize  (_x_bin_size, _y_bin_size);
  if (!(_gdr->doInserting (cps, grid))) {
    _error_status = _gdr->errorStatus ();
    return 0;
  }
  delete _gdr, _gdr = 0;

  return 1;
}

int AutoGridderFloat::firstColumn ()
{
  return _first_column;
}

int AutoGridderFloat::columnCount ()
{
  return _column_count;
}

int AutoGridderFloat::firstRow ()
{
  return _first_row;
}

int AutoGridderFloat::rowCount ()
{
  return _row_count;
}

int AutoGridderFloat::failed ()
{
  return (int) (_error_status != AGF_SUCCESSFUL);
}

// descendingly sort integer array2 where array1 elements are equal
void AutoGridderFloat::sortJArrays (long count, int *array1, int *array2)
{
  long top;
  int still_equal;
  long k2 = 0, k3;
  while (k2 < count) {
    still_equal = 1;
    for (k3 = k2+1; (k3 < count) && still_equal; k3++)
      still_equal = array1[k2] == array1[k3];
    if (still_equal)
      top = k3 - 1;
    else
      top = k3 - 2;
    if (top > k2)
      sortIArrays (k2, top, array2, array1);
    k2 = top + 1;
  }
}

// descendingly sort integer array1 and array2 according to array1
void AutoGridderFloat::sortIArrays (long left, long right, int *array1,
  int *array2)
{
  long k2, last;

  if (left >= right) // return if arrays are trivial
    return;
  swapI  (left, (left + right) >> 1, array1);
  swapI  (left, (left + right) >> 1, array2);
  last = left;
  for (k2 = left+1; k2 <= right; k2++)
    if (compareI (array1[k2], array1[left]) > 0) {
      swapI  (++last, k2, array1);
      swapI  (  last, k2, array2);
    }
  swapI  (left, last, array1);
  swapI  (left, last, array2);
  sortIArrays (left,   last-1, array1, array2);
  sortIArrays (last+1, right,  array1, array2);
}

// swap integer array elements
void AutoGridderFloat::swapI (long k2, long k3, int *array)
{
  int temp   = array[k2];
  array[k2]  = array[k3];
  array[k3]  = temp;
}

// compare integer values
int AutoGridderFloat::compareI (int value1, int value2)
{
  if (value1 < value2)
    return -1;
  else if (value1 > value2)
    return 1;
  else
    return 0;
}

// descendingly sort integer array
void AutoGridderFloat::sortIArray (long left, long right, int *array)
{
  long k2, last;

  if (left >= right) // return if array is trivial
    return;
  swapI  (left, (left + right) >> 1, array);
  last = left;
  for (k2 = left+1; k2 <= right; k2++)
    if (compareI (array[k2], array[left]) > 0) {
      swapI  (++last, k2, array);
    }
  swapI  (left, last, array);
  sortIArray (left,   last-1, array);
  sortIArray (last+1, right,  array);
}

float AutoGridderFloat::findSingleAveDiff (long count, int *array)
{
// sort array in DESCENDING order
  sortIArray (0, count-1, array);

  if (count < 2) return (float)1;

  double diff = 0;
  long k2;
  for (k2 = 1; k2 < count; k2++)
    diff += (double)(array[k2-1] - array[k2]);

  float ave_diff = (float)(diff / (double)(count - 1));
  if (ave_diff < (float)1) return (float)1; 
  else                     return ave_diff;
}

int AutoGridderFloat::findSingleAveBinSize (ControlPoints *cps)
{
  int x=0, y=1, dimension;
  float bin_range;
  if (_which_dimension == Y_ONLY) {
    dimension = y;
    bin_range =   (cps->getMaximum(y) - cps->getMinimum(y))
                / (_y_max_grid_center - _y_min_grid_center)
                * (float)_num_y_bins;
  }

  else /* if (_which_dimension == X_ONLY) */ {
    dimension = x;
    bin_range =   (cps->getMaximum(x) - cps->getMinimum(x))
                / (_x_max_grid_center - _x_min_grid_center)
                * (float)_num_x_bins;
  }

  if (!(cps && cps->getCount() > 0)) {
    _error_status = AGF_BAD_INPUTS;
    return 0;
  }
  _array_x = new int[cps->getCount()];
  if (!_array_x) {
    _error_status = AGF_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

  float gain = bin_range / (cps->getMaximum(dimension) - cps->getMinimum(dimension));
  float bias = -gain * cps->getMinimum(dimension) + 0.5;

// store the first control points in the _arrays
  _array_x[0] = (int) (gain * cps->get (dimension, 0) + bias);
  for (long k2 = 1; k2 < cps->getCount(); k2++){// loop through rest cntl pnts
    _array_x[k2] = (int) (gain * cps->getNext(dimension) + bias);
  }

  float ave_diff = findSingleAveDiff (cps->getCount(), _array_x);
  if (failed()) return 0;

  if (_which_dimension == Y_ONLY) {
    if (ave_diff == 1.0)
      _anal_y_bin_size = _y_bin_size;
    else {
      if (fabs ((double)gain) < EPSILON) {
        _error_status = AGF_ZERO_DENOMINATOR;
        return 0;
      }
      _anal_y_bin_size = ave_diff / gain;
    }
    _anal_x_bin_size = _x_bin_size;
  }
  else /* if (_which_dimension = X_ONLY) */ {
    if (ave_diff == 1.0)
      _anal_x_bin_size = _x_bin_size;
    else {
      if (fabs ((double)gain) < EPSILON) {
        _error_status = AGF_ZERO_DENOMINATOR;
        return 0;
      }
      _anal_x_bin_size = ave_diff / gain;
    }
    _anal_y_bin_size = _y_bin_size;
  }

  delete [] _array_x, _array_x = 0;

  return 1;
}
