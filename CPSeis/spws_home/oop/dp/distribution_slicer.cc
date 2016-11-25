// distribution_slicer.cc:  Implementation file for DistributionSlicer class
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
#include "dp/distribution_slicer.hh"
#include "dp/uchar_grid.hh"
#include "sp/do_abort.hh"
#include <math.h>

DistributionSlicer::DistributionSlicer (int coding_levels, float min_bin,
  float max_bin, int min_bin_sum, int max_bin_sum, float *upper_bin_limit,
  float *lower_bin_limit):
  DisplayProcessorBase (coding_levels),
  _min_bin              (min_bin),      // value associated with first bin
  _max_bin              (max_bin),      // value associated with last bin
  _min_bin_sum          (min_bin_sum),  // minimum cumulative bin counts
  _max_bin_sum          (max_bin_sum),  // maximum cumulative bin counts
  _value_disposition    (0),            // how to find decoder values (Median)
  _allow_insert_vector  (0)             // default not to allow max vec insert
{
// set pointers to NULL
  _grid = 0;
  _distr_slicer_grid = 0;

// after pointers are NULLified, check for failure in ancestors instantiation
  if (DisplayProcessorBase::failed()) return;

// initialize variables

  if (upper_bin_limit != 0)
    _upper_bin_limit = *upper_bin_limit;
  else
    _upper_bin_limit = _max_bin;
  if (lower_bin_limit != 0)
    _lower_bin_limit = *lower_bin_limit;
  else
    _lower_bin_limit = _min_bin;

  _epsilon = fabs ((double)(_max_bin - _min_bin) / (double)10000);
}

DistributionSlicer::~DistributionSlicer ()
{
  if (_distr_slicer_grid)        delete _distr_slicer_grid;
}

void DistributionSlicer::setUpperBinLimit (float upper_bin_limit)
{
  _upper_bin_limit = snapUpperBinLimit (upper_bin_limit);
  _lower_bin_limit = validateLowerBinLimit ();
}

void DistributionSlicer::setLowerBinLimit (float lower_bin_limit)
{
  _lower_bin_limit = snapLowerBinLimit (lower_bin_limit);
  _upper_bin_limit = validateUpperBinLimit ();
}

int DistributionSlicer::getMinimumUpperBinLimit ()
{
  return (int)(_min_bin + _bin_width + 0.5);
}

int DistributionSlicer::getMaximumUpperBinLimit ()
{
  return (int)(_max_bin + 0.5);
}

int DistributionSlicer::getMinimumLowerBinLimit ()
{
  return (int)(_min_bin + 0.5);
}

int DistributionSlicer::getMaximumLowerBinLimit ()
{
  return (int)(_max_bin - _bin_width + 0.5);
}

int DistributionSlicer::quantizeBinCounts ()
{
  if (!_grid) {
    _error_status = DPB_INITIALIZATION_ERROR;
    return 0;
  }
  if (_min_bin_count != 0) *_grid -= (float)_min_bin_count;
  if (_bin_count_incr == 0) {
    _error_status = DPB_UNDEFINED_ARITHMETIC;
    return 0;
  }
  *_grid /= (float)_bin_count_incr;
  *_grid *= (float)_bin_count_incr;
  if (_min_bin_count != 0) *_grid += (float)_min_bin_count;
  return 1;
}

int DistributionSlicer::initialize (UCharGrid *grid)
{
  if (!grid || grid->getSubYBins() < 2) {
    _error_status = DPB_BAD_INPUTS;
    return 0;
  }
  _grid = grid;

// initialize the bin width based on the given grid size in the "bin"-dimension
  _num_bins = _grid->getSubYBins ();
  _bin_width = (_max_bin - _min_bin) / (float)_num_bins;

  setUpperBinLimit (_upper_bin_limit);  // validate limit
  setLowerBinLimit (_lower_bin_limit);  // validate limit

// analyze the distribution of the grid initially
  if (!findBinCountInfo()) return 0;
// quantize the bin counts
   quantizeBinCounts ();
// compress the grid initially
  if (!execute (0, 0, _grid->getNumZBins(), _grid->getNumXBins())) return 0;
// initialize the Z-LUT
  if (!ZLUTAvailable(ZLUTSize())) {
    _error_status = DPB_INITIALIZATION_ERROR;
    return 0;
  }

  _needs_initializing = 0; // turn initialization switch off
  return 1;
}

unsigned char *DistributionSlicer::getArray ()
{
  if (_needs_initializing) {
    _error_status = DPB_INITIALIZATION_ERROR;
    return (unsigned char *)0;
  }

// return the pointer to the distribution slicer array
  return _distr_slicer_grid->getArray ();
}

int DistributionSlicer::prepareUse ()
{
// If Z's have changed.  Search out the mapping between the Z's in the given
//   RGBZ LUT and the Z's in the decoder.
  if (_z_changed && newZVaries()) {

    int bin_sum = _min_bin_sum;
    int found, k2, k3;
    for (k2 = 0; k2 < _z_lut_size; k2++) {
      found = 0;
      for (k3 = 1; (k3 < _rgbz_count) && !found; k3++)
        found = (bin_sum > _newz[k3-1] && bin_sum <= _newz[k3]);
      if (found)
        _z_lut[k2] = (k3 - 2) * 4;  // store index of red value in RGBZ LUT
      else /* if (!found) */ {
        if (bin_sum <= _newz[0])
          _z_lut[k2] = 0; // use index of first red in RGBZ LUT
        else /* if (bin_sum > _newz[_rgbz_count-1]) */
          _z_lut[k2] = (_rgbz_count - 1) * 4; // use index of last red in RGBZ
      }
      bin_sum++;
    }
    _z_changed = 0;

// store the non-constant new Z's
    k3 = 3;
    for (k2 = 0; k2 < _rgbz_count; k2++) {
      _rgbz[k3] = _newz[k2];
        k3 += 4;
    }
  }

  else if (_z_changed && !newZVaries()) {

    if (!ZVaries()) {
// the Z's in _rgbz do not vary, so interpolate them from _min_bin_sum to
//   _max_bin_sum.
      float bin_sum_incr = (float)(_max_bin_sum - _min_bin_sum)
        / (float)(_rgbz_count - 1);
      float bin_sum = (float)_min_bin_sum; 
      for (int k2 = 3; k2 < _rgbz_count*4; k2+=4) {
        _rgbz[k2] = bin_sum;
        bin_sum += bin_sum_incr;
      }
    }

    int bin_sum = _min_bin_sum;
    int found, k3;
    for (int k2 = 0; k2 < _z_lut_size; k2++) {
      found = 0;
      for (k3 = 7; (k3 < _rgbz_count*4) && !found; k3+=4)
        found = (bin_sum > _rgbz[k3-4] && bin_sum <= _rgbz[k3]);
      if (found)
        _z_lut[k2] = k3 - 7 - 4;  // store index of red value in RGBZ LUT
      else /* if (!found) */ {
        if (bin_sum <= _rgbz[3])
          _z_lut[k2] = 0; // use index of first red in RGBZ LUT
        else /* if (bin_sum > _rgbz[_rgbz_count*4-1]) */
          _z_lut[k2] = (_rgbz_count - 1) * 4; // use index of last red in RGBZ
      }
      bin_sum++;
    }
    _z_changed = 0;
  }
  return 1;
}

void DistributionSlicer::getOneCode (int index, float *red, float *green,
  float *blue, float *attribute)
{
// Provide a starting computed RGB element from the decoder
  _decoder_offset = index * _num_bins;
  computeElement (red, green, blue, attribute); // call to virtual function
}

void DistributionSlicer::getNextCode (float *red, float *green, float *blue,
  float *attribute)
{
// Provide the next computed RGB element from the decoder
  _decoder_offset += _num_bins;
  computeElement (red, green, blue, attribute); // call to virtual function
}

int DistributionSlicer::execute (int min_z_bin, int min_x_bin, int num_z_bins,
  int num_x_bins)
{
  if (!_grid) {
    _error_status = DPB_INITIALIZATION_ERROR;
    return 0;
  }

  if (min_z_bin < 0) min_z_bin = 0;
  if (min_z_bin + num_z_bins > _grid->getNumZBins())
    num_z_bins = _grid->getNumZBins() - min_z_bin;

  if (min_x_bin < 0) min_x_bin = 0;
  if (min_x_bin + num_x_bins > _grid->getNumXBins())
    num_x_bins = _grid->getNumXBins() - min_x_bin;

  if (!(preModify (min_z_bin, min_x_bin, num_z_bins, num_x_bins)))
    return 0;
  if (!(modify ())) return 0;

  return 1;
}

int DistributionSlicer::findBinCountInfo ()
{
  if (!_grid) {
    _error_status = DPB_INITIALIZATION_ERROR;
    return 0;
  }

// find the bin count increment value based on the extrema of the grid
//   and the intent to keep the total possibilites down to exp (36*log(2))

  _min_bin_count = (int)_grid->findMinimum ();
  _max_bin_count = (int)_grid->findMaximum ();
  if (_grid->failed()) {
    _error_status = _grid->errorStatus ();
    return 0;
  }

  int num_bin_counts =
    (int)(exp ((double)36/(double)_num_bins*log((double)2)) + 0.5);
  _bin_count_incr =
    (int)(((float)_max_bin_count - (float)_min_bin_count) /
          (float)num_bin_counts + 0.5);

  if (_bin_count_incr < 1) _bin_count_incr = 1;

// make a couple of checks on the computed values
  if (num_bin_counts < 2) {
    _error_status = DPB_BAD_INPUTS;
    return 0;
  }
  else
    return 1;
}

float DistributionSlicer::snapUpperBinLimit (float upper_bin_limit)
{
  upper_bin_limit = (upper_bin_limit > _max_bin)
    ? _max_bin : upper_bin_limit;
  upper_bin_limit = (upper_bin_limit < _min_bin + _bin_width)
    ? _min_bin + _bin_width : upper_bin_limit;
  int bin = (int)((_max_bin - upper_bin_limit) / _bin_width + 0.5);
  upper_bin_limit = _max_bin - (float)bin * _bin_width;
  return upper_bin_limit;
}

// check to see if _upper_bin_limit is still OK, if not make it OK
float DistributionSlicer::validateUpperBinLimit ()
{
  if (_upper_bin_limit <= _lower_bin_limit + _epsilon) {
    return snapUpperBinLimit (_lower_bin_limit+_bin_width);
  }
  else
    return _upper_bin_limit;
}

float DistributionSlicer::snapLowerBinLimit (float lower_bin_limit)
{
  lower_bin_limit = (lower_bin_limit < _min_bin)
    ? _min_bin : lower_bin_limit;
  lower_bin_limit = (lower_bin_limit > _max_bin - _bin_width)
    ? _max_bin - _bin_width : lower_bin_limit;
  int bin = (int)((lower_bin_limit - _min_bin) / _bin_width + 0.5);
  lower_bin_limit = _min_bin + (float)bin * _bin_width;
  return lower_bin_limit;
}

// check to see if _lower_bin_limit is still OK, if not make it OK
float DistributionSlicer::validateLowerBinLimit ()
{
  if (_lower_bin_limit + _epsilon >= _upper_bin_limit) {
    return snapLowerBinLimit (_upper_bin_limit-_bin_width);
  }
  else
    return _lower_bin_limit;
}

int DistributionSlicer::ZLUTSize ()
{
  if (!_decoder) {
    _error_status = DPB_INITIALIZATION_ERROR;
    return 0;
  }

// find the maximum bin count sum possible given the decoder (may not be equal
//   to the given _max_bin_sum)
  int max_bin_sum = 0, bin_sum, index, k2, k3, index_offset = 0;
  for (k2 = 0; k2 < _coding_levels; k2++) {
    bin_sum = 0;
    index = index_offset;
    for (k3 = 0; k3 < _num_bins; k3++) {
      bin_sum += (int)_decoder[index];
      index++;
    }
    if (max_bin_sum < bin_sum) max_bin_sum = bin_sum;
    index_offset += _num_bins;
  }

// find the minimum bin count sum assuming that only one bin is selected given
//   the decoder (may not be equal to the given _min_bin_sum)
  int min_bin_sum = max_bin_sum, decoder_size = _coding_levels * _num_bins;
  for (k2 = 0; k2 < decoder_size; k2++)
    if (_decoder[k2] < min_bin_sum) min_bin_sum = _decoder[k2];

  if (max_bin_sum == min_bin_sum) {
    _error_status = DPB_BAD_INPUTS;
    return 0;
  }

  return max_bin_sum - min_bin_sum + 1;
}

// sort the maximum histogram sums in the decoder from smallest to largest
int DistributionSlicer::sortDecoder ()
{
  if (!_decoder) {
    _error_status = DPB_INITIALIZATION_ERROR;
    return 0;
  }

// first create an array for the maximum histogram sums
  int *max_sums = 0;
  max_sums = new int[_coding_levels];
  if (!max_sums) {
    _error_status = DPB_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

// fill the array
  int max_sum, k2, k3;
  unsigned char *decoder = _decoder;
  for (k2 = 0; k2 < _coding_levels; k2++) {
    max_sum = 0;
    for (k3 = 0; k3 < _num_bins; k3++)
      max_sum += (int)decoder[k3];
    max_sums[k2] = max_sum;
    decoder += _num_bins;
  }

// sort the array from smallest to largest with decoder array
  int temp, count_unique_entries = 0;
  for (k2 = 0; k2 < _coding_levels; k2++) {
    for (k3 = k2+1; k3 < _coding_levels; k3++) {
      if (max_sums[k3] < max_sums[k2]) {
        temp = max_sums[k2];
        max_sums[k2] = max_sums[k3];
        max_sums[k3] = temp;
        if (!_n_tree->swapLUTEntries(_decoder,(unsigned char)k2,
          (unsigned char)k3)) {
          delete [] max_sums;
          _error_status = DPB_COMPRESSION_ERROR;
          return 0;
	}
        count_unique_entries = 1;
      }
    }
  }

// remove the temporary array
  delete [] max_sums;

// if code entries were swapped then count the unique entries
  if (count_unique_entries)
    if (!_n_tree->computeUniqueLUTCount(_decoder)) {
      _error_status = DPB_COMPRESSION_ERROR;
      return 0;
    }
  return 1;
}

void DistributionSlicer::computeElement (float *red, float *green, float *blue,
  float *attribute)
{

// find the total bin sum that is within the lower and upper bin limits
  int bin_sum = 0;
  float mid_level = _min_bin + _bin_width / 2.0;
  for (int k2 = 0; k2 < _num_bins; k2++) {
    if (_lower_bin_limit <= mid_level && _upper_bin_limit >= mid_level)
      bin_sum += (int)_decoder[_decoder_offset+k2];
    mid_level += _bin_width;
  }
 
  *attribute = (float)bin_sum;

// determine what color is associated with the bin sum found.
  int bin_sum_index = bin_sum - _min_bin_sum;
  int z_index = _z_lut[bin_sum_index];
  if (_rgbz_remapper) {
    *red   = _rgbz_remapper->getValue (z_index);
    *green = _rgbz_remapper->getValue (z_index+1);
    *blue  = _rgbz_remapper->getValue (z_index+2);
  }
  else {
    *red   = _rgbz[z_index];
    *green = _rgbz[z_index+1];
    *blue  = _rgbz[z_index+2];
  }
}

// set the grid sub region for the distribution slicer
int DistributionSlicer::preModify (int min_z_bin, int min_x_bin,
  int num_z_bins, int num_x_bins)
{
  if (!(_grid->setSubSize (num_x_bins, _grid->getNumYBins(), num_z_bins))) {
    _error_status = _grid->errorStatus ();
    return 0;
  }
  if (!(_grid->setSubBinStart (min_x_bin, 0, min_z_bin))) {
    _error_status = _grid->errorStatus ();
    return 0;
  }
  return 1;
}

// execute the distribution slicer on the grid's subregion
int DistributionSlicer::modify ()
{
  if (!_grid) {
    _error_status = DPB_INITIALIZATION_ERROR;
    return 0;
  }

// define some local variables to avoid unnecessary function calls
  int num_x_bins = _grid->getNumXBins ();
  int num_y_bins = _grid->getNumYBins ();
  int num_z_bins = _grid->getNumZBins ();
  long zx_plane_count = num_z_bins * num_x_bins;
  DoAbort *do_abort = _grid->getDoAbort ();
  if (_needs_initializing) {

// initially encode the entire incoming grid to obtain the encoded array
//   along with the accompanying decoder array
    long max_nodes = MAX_NODES;
    long max_classified = zx_plane_count;
    _n_tree = new NTree (_grid->getArray(), zx_plane_count, num_y_bins,
      max_nodes, max_classified, _value_disposition, _allow_insert_vector,
      do_abort);
    if (!_n_tree) {
      _error_status = DPB_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_n_tree->failed()) {
      _error_status = DPB_COMPRESSION_ERROR;  // loses connection with NTree err
      return 0;
    }
    if (!_n_tree->classify()) {
      _error_status = DPB_COMPRESSION_ERROR;  // loses connection with NTree err
      return 0;
    }

    if (!arrayInitializer (_num_bins)) {
      _error_status = errorStatus();
      return 0;
    }
    int levels = _n_tree->reduce (_coding_levels, _decoder);
    _coding_levels = levels;

// the following is a feeble attempt to get around a X-color assignment
//   problem that Mike Sherrill is looking into 12/20/95.  should the
//   problem go away, the sort would be unneccessary.  the problem appears
//   to be that private color map indices 0 & 1 are both being colored as
//   index 0 when there are only 206 colors used in the private color map.
//
// sort decoder results
    int do_sort = 1; // sort the decoder
    if (do_sort)
      if (!sortDecoder()) return 0;

// create the distribution slicer grid
    _distr_slicer_grid = new UCharGrid (num_z_bins, num_x_bins, 1, do_abort);
    if (!_distr_slicer_grid) {
      _error_status = DPB_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_distr_slicer_grid->failed()) {
      _error_status = _distr_slicer_grid->errorStatus ();
      return 0;
    }
  }
  unsigned char *distr_slicer_array = _distr_slicer_grid->getArray ();

// create a temporary array as long as a column
  int sub_x_bins = _grid->getSubXBins ();
  _temp_array = new unsigned char[sub_x_bins];
  if (!_temp_array) {
    _error_status = DPB_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

// encode unsigned char grid by columns
  long xy_plane_count = num_x_bins * num_y_bins;
  long grid_index;
  long grid_index_offset = (long)_grid->getSubZBinStart() * xy_plane_count
    + (long)_grid->getSubXBinStart() * (long)num_y_bins
    + (long)_grid->getSubYBinStart();
  long array_index;
  long array_index_offset = (long)_grid->getSubZBinStart() * (long)num_x_bins
    + (long)_grid->getSubXBinStart();
  int k2;
  unsigned char *grid = _grid->getArray ();
  int sub_z_bins = _grid->getSubZBins ();
  for (k2 = 0; k2 < sub_z_bins; k2++) {
    grid_index = grid_index_offset;
    _n_tree->assign (&(grid[grid_index]), (long)sub_x_bins, _decoder,
      _temp_array);
    array_index = array_index_offset;
    for (int k3 = 0; k3 < sub_x_bins; k3++) {
      distr_slicer_array[array_index] = _temp_array[k3];
      array_index++;
      if (do_abort) {
	if (!(array_index % (long)5000)) {
	  if (do_abort->userAbort()) {
            _error_status = DPB_USER_ABORTED;
            return 0;
          }
        }
      }
    }
    grid_index_offset += xy_plane_count;
    array_index_offset += (long)num_x_bins;
  }

  _distr_slicer_grid->resetExtremaReadyFlag ();
  delete [] _temp_array, _temp_array = 0;
  return 1;
}
