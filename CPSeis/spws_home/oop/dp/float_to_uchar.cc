// implementation for the FloatToUChar class
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
// class moves a region of a FloatGrid to a region of a plane in a UCharGrid.
// if the subregion of the object receiving the data is smaller than the
//   subregion of the object sending the data, less data is sent.
// if the subregion of the object sending the data is smaller than the
//   subregion of the object receiving the data, less data is received.
// a failure occurs if a single planar (Y) region has not been set in the
//   UCharGrid subregion.

#include "dp/float_to_uchar.hh"
#include "dp/float_grid.hh"
#include "dp/uchar_grid.hh"

FloatToUChar::FloatToUChar () :
  _error_status  (FU_SUCCESSFUL)
{
}

FloatToUChar::~FloatToUChar ()
{
}

int FloatToUChar::move (FloatGrid *from_fg, UCharGrid *to_ucg, float fg_gain,
  float fg_bias)
{
// determine the subregion size to move
// assume that the starting locations are as they should be for the from_fg.
// assume that the starting locations are as they should be for the to_ucg.
// also note that the fg (x,y) -> ucg (z,x).  ucg (y) is a plane.

// note: the fg YBins are analogous to the ucg XBins
  int from_sub_y_bins = from_fg->_sub_y_bins;
  if (from_sub_y_bins > to_ucg->_sub_x_bins)
    from_sub_y_bins = to_ucg->_sub_x_bins;

// note: the fg XBins are analogous to the ucg ZBins
  int from_sub_x_bins = from_fg->_sub_x_bins;
  if (from_sub_x_bins > to_ucg->_sub_z_bins)
    from_sub_x_bins = to_ucg->_sub_z_bins;

// transfer from the ucg to the fg modifying the data as required
  long from_sub_start_y_bin = (long) from_fg->getSubYBinStart ();
  long from_sub_start_x_bin = (long) from_fg->_sub_start_x;
  long from_index_offset = from_sub_start_x_bin * (long)from_fg->_num_y_bins +
    from_sub_start_y_bin;
  if (from_index_offset < 0) { // must start on the from grid
    _error_status = FU_INVALID_SUB_REGION;
    return 0;
  }

// note: the ucg YBins are analogous to plane indices
  if (to_ucg->_sub_y_bins > 1) {
    _error_status = FU_INVALID_SUB_REGION;
    return 0;
  }

  long to_num_xy_bins = (long)to_ucg->_num_x_bins * (long)to_ucg->_num_y_bins;
  long to_sub_start_y_bin = (long)to_ucg->getSubYBinStart ();
  long to_sub_start_x_bin = (long)to_ucg->_sub_start_x;
  long to_sub_start_z_bin = (long)to_ucg->getSubZBinStart ();
  long to_xyz_index_offset
    = to_sub_start_z_bin * to_num_xy_bins
    + to_sub_start_x_bin * (long)to_ucg->_num_y_bins
    + to_sub_start_y_bin;
  if (to_xyz_index_offset < 0) { // must start on the to grid
    _error_status = FU_INVALID_SUB_REGION;
    return 0;
  }

// must stop on the from grid
  long from_max_index = (long)(from_fg->_num_x_bins-1)
    * (long)from_fg->_num_y_bins
    + (long)(from_fg->_num_y_bins - 1);
  long from_sub_max_index = (long)(from_sub_start_x_bin + from_sub_x_bins - 1)
    * (long)from_fg->_num_y_bins + (long)(from_sub_start_y_bin
    + from_sub_y_bins - 1);
  if (from_sub_max_index > from_max_index) {
    _error_status = FU_INVALID_SUB_REGION;
    return 0;
  }

// must stop on the to grid
  long to_max_index = (long)(to_ucg->_num_z_bins - 1) * to_num_xy_bins
    + (long)(to_ucg->_num_x_bins - 1) * (long)to_ucg->_num_y_bins
    + (long)(to_ucg->_num_y_bins - 1);

  long to_sub_max_index = (long)(to_sub_start_z_bin + from_sub_x_bins - 1)
    * to_num_xy_bins
    + (long)(to_sub_start_x_bin + from_sub_y_bins - 1)
    * (long)to_ucg->_num_y_bins
    + (long)(to_sub_start_y_bin + to_ucg->_sub_y_bins - 1);
  if (to_sub_max_index > to_max_index) {
    _error_status = FU_INVALID_SUB_REGION;
    return 0;
  }

  long from_index, to_index;
  int k2, k3, ivalue, add_on, valid_min, valid_max;
  float fg_bias_plus = fg_bias + 0.5;  // institute rounding
  float value;

  if (to_ucg->_not_defined > 0) {
    add_on = -1;
    valid_min = 0;
  }
  else {
    add_on = +1;
    valid_min = 1;
  }

  if (to_ucg->_not_defined > 254)
    valid_max = 254;
  else
    valid_max = 255;

  for (k2 = 0; k2 < from_sub_x_bins; k2++) {
    from_index = from_index_offset;
    to_index = to_xyz_index_offset;
    for (k3 = 0; k3 < from_sub_y_bins; k3++) {
      value = from_fg->_grid[from_index];
      if (value != from_fg->_not_defined) {
// apply a gain and a bias to the float data effecting rounding
        value *= fg_gain;
        value += fg_bias_plus;
// clip or "bump" the result to be a valid ucg value
        ivalue = (int)value;
        if (ivalue == (int)to_ucg->_not_defined)
          ivalue += add_on;
        else if (ivalue < 0)
          ivalue = valid_min;
        else if (ivalue > 255)
          ivalue = valid_max;
// store the result in the unsigned char grid
        to_ucg->_grid[to_index] = (unsigned char)ivalue;
      }
      else // designate the ucg value as not defined
        to_ucg->_grid[to_index] = to_ucg->_not_defined;
      from_index++;
      to_index += to_ucg->_num_y_bins;
    }
    from_index_offset   += (long)from_fg->_num_y_bins;
    to_xyz_index_offset += to_num_xy_bins;
  }
  return 1;
}

// make all elements undefined in any vector that has even one undefined
//   element.
void FloatToUChar::simplifyUndefinedVectors (UCharGrid *ucg)
{
  if (!ucg) return;

  int num_y_bins = ucg->getNumYBins (); // like the spectral-coordinate
  long num_zx_bins = (long)ucg->getNumZBins() * (long)ucg->getNumXBins();
  unsigned char not_defined = ucg->getUndefined ();
  unsigned char *grid = ucg->getArray ();

  int k3, undefined_element = 0;
  long k2, offset = 0;
  for (k2 = 0; k2 < num_zx_bins; k2++) {
    for (k3 = 0; k3 < num_y_bins && !undefined_element; k3++)
      undefined_element = (int)(grid[offset+k3] == not_defined);
    if (undefined_element) {
      for (k3 = 0; k3 < num_y_bins; k3++)
        if (grid[offset+k3] != not_defined) grid[offset+k3] = not_defined;
      undefined_element = 0;
    }
    offset += num_y_bins;
  }
}

int FloatToUChar::failed ()
{
  return (int)(_error_status != FU_SUCCESSFUL);
}
