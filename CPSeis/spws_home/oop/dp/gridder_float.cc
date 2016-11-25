// gridder_float.cc:  Implementation file for GridderFloat class
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
#include "dp/gridder_float.hh"
#include "dp/control_points.hh"
#include "dp/float_grid.hh"

// constructor
GridderFloat::GridderFloat ()
{
// set array pointers to NULL
  _fgrid = 0;
  _counts = 0;
  _warr = 0;
  _yarr = 0;
  _xarr = 0;
  _error_status = GF_SUCCESSFUL;
  _x_bin_reach = 0;
  _y_bin_reach = 0;
}

// Destructor
GridderFloat::~GridderFloat ()
{
  if (_fgrid) delete _fgrid, _fgrid = 0;
  if (_counts) delete _counts, _counts = 0;
  if (_warr) delete [] _warr, _warr = 0;
  if (_yarr) delete [] _yarr, _yarr = 0;
  if (_xarr) delete [] _xarr, _xarr = 0;
}

int GridderFloat::setReach (int x_bin_reach, int y_bin_reach)
{
  if (!(2*x_bin_reach+1 < 256  && 2*y_bin_reach+1 < 256)) {
    _error_status = GF_BAD_REACH_VALUES;
    return 0;
  }
  _x_bin_reach = x_bin_reach;
  _y_bin_reach = y_bin_reach;
  _error_status = GF_SUCCESSFUL;
  return 1;
}

void GridderFloat::setOrigin (float x_bin_min, float y_bin_min)
{
  _x_bin_min = x_bin_min;
  _y_bin_min = y_bin_min;
}

void GridderFloat::setBinSize (float x_bin_size, float y_bin_size)
{
  _x_bin_size = x_bin_size;
  _y_bin_size = y_bin_size;
}

int GridderFloat::setMaxHits (int max_hits)
{
  if (!(max_hits > 0)) {
    _error_status = GF_BAD_MAX_HITS_VALUE;
    return 0;
  }
  _max_hits = max_hits;
  _error_status = GF_SUCCESSFUL;
  return 1;
}

int GridderFloat::doGridding (ControlPoints *cps, FloatGrid *grid)
{
// do some simple data verifications
  if (!(cps && grid)) {
    _error_status = GF_BAD_INPUTS;
    return 0;
  }

// initialize some pointers
  if (_warr) delete [] _warr, _warr = 0;
  if (_yarr) delete [] _yarr, _yarr = 0;
  if (_xarr) delete [] _xarr, _xarr = 0;

// clear the output float grid array
  if (!(grid->fill (0))) {
    _error_status = grid->errorStatus ();
    return 0;
  }

// create the grid counts array & clear
  _counts = new FloatGrid (grid->getNumXBins(), grid->getNumYBins());
  if (!_counts) {
    _error_status = GF_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  if (_counts->failed()) {
    _error_status = _counts->errorStatus ();
    return 0;
  }
  float min_count = grid->getUndefined() < 0 ? 0 : grid->getUndefined()+1;
  float max_count = (float) grid->getArraySize ();
  if (!(_counts->setRange (grid->getUndefined(), min_count, max_count))) {
    _error_status = _counts->errorStatus ();
    return 0;
  }
  if (!(_counts->fill (0))) {
    _error_status = _counts->errorStatus ();
    return 0;
  }

// accumulate control points into grid array
  int x=0, y=1, a=2;
  int ix, iy;
  float value;
  long index;
  value = (cps->get(x,0) - _x_bin_min) / _x_bin_size;
  ix = (int) value;
  value = (cps->get(y,0) - _y_bin_min) / _y_bin_size;
  iy = (int) value;
  value = cps->get (a, 0);
  index = grid->getIndex (ix, iy);
  grid->accumulateIndex (index, value);
  _counts->accumulateIndex (index, 1);
  long k2;
  int itest = 0;
  for (k2 = 1; k2 < cps->getCount(); k2++) {
    value = (cps->getNext(x) - _x_bin_min) / _x_bin_size;
    ix = (int) value;
    value = (cps->getNext(y) - _y_bin_min) / _y_bin_size;
    iy = (int) value;
    value = cps->getNext (a);
    index = grid->getIndex (ix, iy);
    if (itest && value > 4000) {
      printf ("value(%d,%d)[%d]  = %f in cntrl pnts\n",
        ix, iy, index, value);
    }
    grid->accumulateIndex (index, value);
    _counts->accumulateIndex (index, 1);
  }
  grid->resetExtremaReadyFlag ();
  _counts->resetExtremaReadyFlag ();

// check for need to do searching
  if (_x_bin_reach > 0 || _y_bin_reach > 0) {

// determine bin array sizes with pads
    int num_x_bins_w_pads = grid->getNumXBins() + 2 * _x_bin_reach;
    int num_y_bins_w_pads = grid->getNumYBins() + 2 * _y_bin_reach;

// create another float grid instance with a pad as wide as bin reach around
//   the grid
    _fgrid = new FloatGrid (num_x_bins_w_pads, num_y_bins_w_pads);
    if (!_fgrid) {
      _error_status = GF_MEMORY_ALLOCATION_ERROR;
      return 0;
    }
    if (_fgrid->failed()) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }
    if (!(_fgrid->setRange (grid->getUndefined(), grid->getMinimum(),
      grid->getMaximum()))) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }

// pad out the first _y_bin_reach rows
    if (!(_fgrid->setSubSize  (num_x_bins_w_pads, _y_bin_reach))) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }
    if (!(_fgrid->setSubBinStart (0, 0))) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }
    if (!(_fgrid->fill (grid->getUndefined()))) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }

// pad out the last _y_bin_reach rows
    if (!(_fgrid->setSubBinStart (0, num_y_bins_w_pads-_y_bin_reach))) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }
    if (!(_fgrid->fill (grid->getUndefined()))) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }

// pad out the first _x_bin_reach columns
    if (!(_fgrid->setSubSize  (_x_bin_reach,num_y_bins_w_pads-2*_y_bin_reach)))
    {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }
    if (!(_fgrid->setSubBinStart (0, _y_bin_reach))) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }
    if (!(_fgrid->fill (grid->getUndefined()))) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }

// pad out the last _x_bin_reach columns
    if (!(_fgrid->setSubBinStart(num_x_bins_w_pads-_x_bin_reach,_y_bin_reach)))
    {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }
    if (!(_fgrid->fill (grid->getUndefined()))) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }

// determine the average grid values and store in the center unpadded bins
    if (!(grid->setSubSize (grid->getNumXBins(), grid->getNumYBins()))) {
      _error_status = grid->errorStatus ();
      return 0;
    }
    if (!(grid->setSubBinStart (0, 0))) {
      _error_status = grid->errorStatus ();
      return 0;
    }
    if (!(_counts->setSubBinStart (0, 0))) {
      _error_status = _counts->errorStatus ();
      return 0;
    }
    if (!(_fgrid->setSubStart (_x_bin_reach, _y_bin_reach))) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }
    if (!(grid->divide (_counts, _fgrid))) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }

// create processing arrays
    int nx = 2 * _x_bin_reach + 1;
    int ny = 2 * _y_bin_reach + 1;
    int nxys = (int)nx * (int)ny;
    _xarr = new int[nxys];
    _yarr = new int[nxys];
    _warr = new int[nxys];
    if (!(_xarr && _yarr && _warr)) {
      _error_status = GF_MEMORY_ALLOCATION_ERROR;
      return 0;
    }

// generate the x,y,weight arrays
    int k3, k4, k5;
    int cx = _x_bin_reach;
    int cy = _y_bin_reach;
    long mxwtp1 = ((long)_x_bin_reach * (long)_x_bin_reach
       + (long)_y_bin_reach * (long)_y_bin_reach) + 1;
    index = 0;
    for (k2 = 0; k2 < ny; k2++) {
      k4 = (int)k2 - cy;
      for (k3 = 0; k3 < nx; k3++) {
        k5 = (int)k3 - cx;
        _yarr[index] = k4;
        _xarr[index] = k5;
        _warr[index] = (int) (mxwtp1 - ((long)k4
           * (long)k4 + (long)k5 * (long)k5));
        index++;
      }
    }

// sort arrays by descending weights order
    sortWeightsWithXsAndYs (0, (long)nxys-1);

// sort all weight ties by descending abs(x) order
    int top;
    int still_equal;
    k2 = 0;
    while (k2 < nxys) {
      still_equal = 1;
      for (k3 = (int)k2+1; (k3 < nxys) && still_equal; k3++)
        still_equal = _warr[k2] == _warr[k3];
      if (still_equal)
        top = k3 - 1;
      else
        top = k3 - 2;
      if (top > k2)
        sortAbsXsWithWeightsAndYs (k2, (long)top);
      k2 = (long)top + 1;
    }
     
// sort all x ties by descending abs(y) order
    k2 = 0;
    while (k2 < nxys) {
      still_equal = 1;
      for (k3 = (int)k2+1; (k3 < nxys) && still_equal; k3++)
        still_equal = _xarr[k2] == _xarr[k3];
      if (still_equal)
        top = k3 - 1;
      else
        top = k3 - 2;
      if (top > k2)
        sortAbsYsWithWeightsAndXs (k2, (long)top);
      k2 = (long)top + 1;
    }

// pass the search pattern and corresponding weight array to the float grid
    if (!(_fgrid->initializeSearchGridPattern (_max_hits, nxys))) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }
    for (k3 = 0; k3 < nxys; k3++) {
      if (!(_fgrid->setSearchGridPatternOffset (k3, _xarr[k3], _yarr[k3]))) {
        _error_status = _fgrid->errorStatus ();
        return 0;
      }
      if (!(_fgrid->setSearchGridPatternWeight (k3, (float)_warr[k3]))) {
        _error_status = _fgrid->errorStatus ();
        return 0;
      }
    }

// grid the float grid array using search gridding
    if (!_fgrid->setSubSize (grid->getNumXBins(), grid->getNumYBins())) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }
    if (!(_fgrid->setSubBinStart (_x_bin_reach, _y_bin_reach))) {
      _error_status = _fgrid->errorStatus ();
      return 0;
    }
    if (!(grid->setSubBinStart (0, 0))) {
      _error_status = grid->errorStatus ();
      return 0;
    }
    if (!(_fgrid->searchGrid (grid))) {
      _error_status = grid->errorStatus ();
      return 0;
    }
    delete _fgrid, _fgrid = 0;
    
  }
  else {

// simply average the floating point array
    if (!(grid->setSubSize (grid->getNumXBins(), grid->getNumYBins()))) {
      _error_status = grid->errorStatus ();
      return 0;
    }
    if (!(grid->setSubBinStart (0, 0))) {
      _error_status = grid->errorStatus ();
      return 0;
    }
    if (!(_counts->setSubBinStart (0, 0))) {
      _error_status = _counts->errorStatus ();
      return 0;
    }
    if (!(grid->divide (_counts))) {
      _error_status = grid->errorStatus ();
      return 0;
    }
  }

  delete _counts, _counts = 0;

  _error_status = GF_SUCCESSFUL;
  return 1;
}

// insert the control points into the grid; do not clear grid
int GridderFloat::doInserting (ControlPoints *cps, FloatGrid *grid)
{
// do some simple data verifications
  if (!(cps && grid)) {
    _error_status = GF_BAD_INPUTS;
    return 0;
  }

// insert control points into grid array
  int x=0, y=1, a=2;
  int ix, iy;
  float value;
  value = (cps->get(x,0) - _x_bin_min) / _x_bin_size;
  ix = (int) value;
  value = (cps->get(y,0) - _y_bin_min) / _y_bin_size;
  iy = (int) value;
  value = cps->get (a, 0);
  grid->insertXY (ix, iy, value);
  for (long k2 = 1; k2 < cps->getCount(); k2++) {
    value = (cps->getNext(x) - _x_bin_min) / _x_bin_size;
    ix = (int) value;
    value = (cps->getNext(y) - _y_bin_min) / _y_bin_size;
    iy = (int) value;
    value = cps->getNext (a);
    grid->insertXY (ix, iy, value);
  }
  grid->resetExtremaReadyFlag ();

  _error_status = GF_SUCCESSFUL;
  return 1;
}

// average the control points into the grid; first clear the grid
int GridderFloat::doAveraging (ControlPoints *cps, FloatGrid *grid)
{
// do some simple data verifications
  if (!(cps && grid)) {
    _error_status = GF_BAD_INPUTS;
    return 0;
  }

// create the grid counts array & clear
  _counts = new FloatGrid (grid->getNumXBins(), grid->getNumYBins());
  if (!_counts) {
    _error_status = GF_MEMORY_ALLOCATION_ERROR;
    return 0;
  }
  if (_counts->failed()) {
    _error_status = _counts->errorStatus ();
    return 0;
  }
  float min_count = grid->getUndefined() < 0 ? 0 : grid->getUndefined()+1;
  float max_count = (float) grid->getArraySize ();
  if (!(_counts->setRange (grid->getUndefined(), min_count, max_count))) {
    _error_status = _counts->errorStatus ();
    return 0;
  }
  if (!(_counts->fill (0))) {
    _error_status = _counts->errorStatus ();
    return 0;
  }

// accumulate control points into grid array
  int x=0, y=1, a=2;
  int ix, iy;
  float value;
  long index;
  value = (cps->get(x,0) - _x_bin_min) / _x_bin_size;
  ix = (int) value;
  value = (cps->get(y,0) - _y_bin_min) / _y_bin_size;
  iy = (int) value;
  value = cps->get (a, 0);
  index = grid->getIndex (ix, iy);
  grid->accumulateIndex (index, value);
  _counts->accumulateIndex (index, 1);
  for (long k2 = 1; k2 < cps->getCount(); k2++) {
    value = (cps->getNext(x) - _x_bin_min) / _x_bin_size;
    ix = (int) value;
    value = (cps->getNext(y) - _y_bin_min) / _y_bin_size;
    iy = (int) value;
    value = cps->getNext (a);
    index = grid->getIndex (ix, iy);
    grid->accumulateIndex (index, value);
    _counts->accumulateIndex (index, 1);
  }
  grid->resetExtremaReadyFlag ();
  _counts->resetExtremaReadyFlag ();

  if (!(grid->setSubSize (grid->getNumXBins(), grid->getNumYBins()))) {
    _error_status = grid->errorStatus ();
    return 0;
  }
  if (!(grid->setSubBinStart (0, 0))) {
    _error_status = grid->errorStatus ();
    return 0;
  }
  if (!(_counts->setSubBinStart (0, 0))) {
    _error_status = _counts->errorStatus ();
    return 0;
  }
  if (!(grid->divide (_counts))) {
    _error_status = grid->errorStatus ();
    return 0;
  }

  delete _counts, _counts = 0;

  _error_status = GF_SUCCESSFUL;
  return 1;
}

// sum the control points into the grid; first clear the grid
int GridderFloat::doSumming (ControlPoints *cps, FloatGrid *grid)
{
// do some simple data verifications
  if (!(cps && grid)) {
    _error_status = GF_BAD_INPUTS;
    return 0;
  }

// clear the output float grid array
  if (!(grid->fill (0))) {
    _error_status = grid->errorStatus ();
    return 0;
  }

// sum control points into grid array
  int x=0, y=1, a=2;
  int ix, iy;
  float value;
  value = (cps->get(x,0) - _x_bin_min) / _x_bin_size;
  ix = (int) value;
  value = (cps->get(y,0) - _y_bin_min) / _y_bin_size;
  iy = (int) value;
  value = cps->get (a, 0);
  grid->accumulateXY (ix, iy, value);
  for (long k2 = 1; k2 < cps->getCount(); k2++) {
    value = (cps->getNext(x) - _x_bin_min) / _x_bin_size;
    ix = (int) value;
    value = (cps->getNext(y) - _y_bin_min) / _y_bin_size;
    iy = (int) value;
    value = cps->getNext (a);
    grid->accumulateXY (ix, iy, value);
  }
  grid->resetExtremaReadyFlag ();

  _error_status = GF_SUCCESSFUL;
  return 1;
}

void GridderFloat::sortWeightsWithXsAndYs (long left, long right)
{
  long k2, last;

  if (left >= right) // return if array is trivial
    return;
  swapInt (left, (left + right) >> 1, _warr);
  swapInt (left, (left + right) >> 1, _xarr);
  swapInt (left, (left + right) >> 1, _yarr);
  last = left;
  for (k2 = left+1; k2 <= right; k2++)
    if (compareInt (_warr[k2], _warr[left]) > 0) {
      swapInt (++last, k2, _warr);
      swapInt (  last, k2, _xarr);
      swapInt (  last, k2, _yarr);
    }
  swapInt (left, last, _warr);
  swapInt (left, last, _xarr);
  swapInt (left, last, _yarr);
  sortWeightsWithXsAndYs (left,   last-1);
  sortWeightsWithXsAndYs (last+1, right);
}

void GridderFloat::sortAbsXsWithWeightsAndYs (long left, long right)
{
  long k2, last;

  if (left >= right) // return if array is trivial
    return;
  swapInt (left, (left + right) >> 1, _xarr);
  swapInt (left, (left + right) >> 1, _warr);
  swapInt (left, (left + right) >> 1, _yarr);
  last = left;
  for (k2 = left+1; k2 <= right; k2++)
    if (compareAbsInt (_xarr[k2], _xarr[left]) > 0) {
      swapInt (++last, k2, _xarr);
      swapInt (  last, k2, _warr);
      swapInt (  last, k2, _yarr);
    }
  swapInt (left, last, _xarr);
  swapInt (left, last, _warr);
  swapInt (left, last, _yarr);
  sortAbsXsWithWeightsAndYs (left,   last-1);
  sortAbsXsWithWeightsAndYs (last+1, right);
}

void GridderFloat::sortAbsYsWithWeightsAndXs (long left, long right)
{
  long k2, last;

  if (left >= right) // return if array is trivial
    return;
  swapInt (left, (left + right) >> 1, _yarr);
  swapInt (left, (left + right) >> 1, _warr);
  swapInt (left, (left + right) >> 1, _xarr);
  last = left;
  for (k2 = left+1; k2 <= right; k2++)
    if (compareAbsInt (_yarr[k2], _yarr[left]) > 0) {
      swapInt (++last, k2, _yarr);
      swapInt (  last, k2, _warr);
      swapInt (  last, k2, _xarr);
    }
  swapInt (left, last, _yarr);
  swapInt (left, last, _warr);
  swapInt (left, last, _xarr);
  sortAbsYsWithWeightsAndXs (left,   last-1);
  sortAbsYsWithWeightsAndXs (last+1, right);
}

int GridderFloat::compareInt (int value1, int value2)
{
  if (value1 < value2)
    return -1;
  else if (value1 > value2)
    return 1;
  else
    return 0;
}

int GridderFloat::compareAbsInt (int value1, int value2)
{
  int abs_v1 = abs(value1);
  int abs_v2 = abs(value2);
  if (abs_v1 < abs_v2)
    return -1;
  else if (abs_v1 > abs_v2)
    return 1;
  else
    return 0;
}

void GridderFloat::swapInt (long k2, long k3, int *array)
{
  int temp = array[k2];
  array[k2]  = array[k3];
  array[k3]  = temp;
}
