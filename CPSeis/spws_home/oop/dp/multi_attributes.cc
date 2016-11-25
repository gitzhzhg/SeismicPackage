// multiple attributes class implementation
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

#include "dp/multi_attributes.hh"
#include "dp/uchar_grid.hh"
#include "dp/float_grid.hh"
#include "dp/n_tree.hh"
#include "sp/do_abort.hh"

MultiAttributes::MultiAttributes (UCharGrid *inputs, int coding_levels,
  FloatGrid *result):
  Decoder (coding_levels),               // decoder constructor
  _inputs               (inputs),        // grid containing inputs
  _result               (result),        // grid to hold result
  _n_tree               (0),             // N-tree of inputs
  _needs_initializing   (1),             // initialization flag (on initially)
  _allow_insert_vector  (0),             // default not to allow max vec insrt
  _value_disposition    (0),             // how to find decoder value (Median)
  _error_status         (MA_SUCCESSFUL)  // error status flag
{
  if (Decoder::failed()) {
    _error_status = Decoder::errorStatus ();
    return;
  }

// check to make sure that the inputs and result match properly
  if (_inputs->getNumZBins()*_inputs->getNumXBins() !=
      _result->getNumXBins()*_result->getNumYBins()   ) {
    _error_status = MA_BAD_INPUTS;
    return;
  }
  if (!arrayInitializer(_inputs->getNumYBins())) return;
}

MultiAttributes::~MultiAttributes ()
{
  if (_n_tree) delete _n_tree;
}

int MultiAttributes::initialize ()
{
  if (!update(0,0,_inputs->getNumZBins(),_inputs->getNumXBins())) return 0;
  _needs_initializing = 0;  // turn initialization switch off
  return 1;
}

int MultiAttributes::update (int min_z_bin, int min_x_bin, int num_z_bins,
  int num_x_bins)
{
  if (min_z_bin < 0) min_z_bin = 0;
  if (min_z_bin + num_z_bins > _inputs->getNumZBins())
    num_z_bins = _inputs->getNumZBins() - min_z_bin;

  if (min_x_bin < 0) min_x_bin = 0;
  if (min_x_bin + num_x_bins > _inputs->getNumXBins())
    num_x_bins = _inputs->getNumXBins() - min_x_bin;

  if (!setUpdateRegion(min_z_bin,min_x_bin,num_z_bins,num_x_bins)) return 0;
  if (!doUpdate()) return 0;

  return 1;
}

int MultiAttributes::failed ()
{
  return (int)(_error_status != MA_SUCCESSFUL || Decoder::failed());
}

GridErrorCodes MultiAttributes::errorStatus ()
{
  if (Decoder::failed())
    return Decoder::errorStatus ();  // in case of parent failure give parent's
  else
    return _error_status;
}

int MultiAttributes::setUpdateRegion (int min_z_bin, int min_x_bin,
  int num_z_bins, int num_x_bins)
{
  if (!_inputs->setSubSize(num_x_bins,_inputs->getNumYBins(),num_z_bins)) {
    _error_status = _inputs->errorStatus ();
    return 0;
  }
  if (!_inputs->setSubBinStart(min_x_bin,0,min_z_bin)) {
    _error_status = _inputs->errorStatus ();
    return 0;
  }
  return 1;
}

int MultiAttributes::doUpdate ()
{
// define some local variables to avoid unnecessary function calls
  int num_x_bins = _inputs->getNumXBins ();
  int num_y_bins = _inputs->getNumYBins ();
  int num_z_bins = _inputs->getNumZBins ();
  long zx_plane_count = num_z_bins * num_x_bins;
  float *result = _result->getArray ();
  DoAbort *do_abort = _result->getDoAbort ();
  if (_needs_initializing) {

// initially encode the inputs to obtain the encoded result and the
//   accompanying decoder array
    long max_nodes = MAX_NODES;
    long max_classified = zx_plane_count;
    _n_tree = new NTree (_inputs->getArray(), zx_plane_count, num_y_bins,
      max_nodes, max_classified, _value_disposition, _allow_insert_vector,
      do_abort);
    if (!_n_tree) {
      _error_status = MA_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_n_tree->failed()) {
      _error_status = MA_COMPRESSION_ERROR;  // loses connection with NTree err
      return 0;
    }

// do the tree classification
    if (!_n_tree->classify()) {
      _error_status = MA_COMPRESSION_ERROR;  // loses connection with NTree err
      return 0;
    }

// reduce the number of levels to comply with desired number of codes
    int levels = _n_tree->reduce (_coding_levels, _decoder);
    _coding_levels = levels;

// clear the result array
    int zero = 0;
    if (!memset ((void *)result, (unsigned char)zero,
      (size_t)zx_plane_count*sizeof(float))) {
      _error_status = MA_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
  }

// create a temporary array as long as a column
  int sub_x_bins = _inputs->getSubXBins ();
  unsigned char *column_result = new unsigned char[sub_x_bins];
  if (!column_result) {
    _error_status = MA_MEMORY_ALLOCATION_ERROR;
    return 0;
  }

// encode result by columns
  long xy_plane_count = num_x_bins * num_y_bins;
  long input_index;
  long input_index_offset = (long)_inputs->getSubZBinStart() * xy_plane_count
    + (long)_inputs->getSubXBinStart() * (long)num_y_bins
    + (long)_inputs->getSubYBinStart();
  long result_index;
  long result_index_offset = (long)_inputs->getSubZBinStart()
    * (long)num_x_bins
    + (long)_inputs->getSubXBinStart();

  int k2, k3;
  unsigned char *input = _inputs->getArray ();
  int sub_z_bins = _inputs->getSubZBins ();

  long count = 1;
  for (k2 = 0; k2 < sub_z_bins; k2++) {
    input_index = input_index_offset;
    _n_tree->assign (&(input[input_index]), (long)sub_x_bins, _decoder,
      column_result);
    result_index = result_index_offset;
    for (k3 = 0; k3 < sub_x_bins; k3++) {
      result[result_index] = (float)column_result[k3];
      result_index++;
      if (do_abort) {
        count++;
	if (!(count % (long)1000)) {
	  if (do_abort->userAbort()) {
            _error_status = MA_USER_ABORTED;
            return 0;
          }
        }
      }
    }
    input_index_offset  += xy_plane_count;
    result_index_offset += (long)num_x_bins;
  }
  _result->resetExtremaReadyFlag ();

  delete [] column_result;
  return 1;
}
