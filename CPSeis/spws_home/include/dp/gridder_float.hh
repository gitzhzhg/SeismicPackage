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
// gridder_float.hh:  User Interface file for GridderFloat class
#ifndef GRIDDER_FLOAT_HH
#define GRIDDER_FLOAT_HH

#include "dp/grid_error_codes.hh"

class ControlPoints;
class FloatGrid;

class GridderFloat {

public:
  GridderFloat ();				// constructor

  ~GridderFloat ();				// destructor

  int setReach					// set the reach distances
    (int x_bin_reach,				//   X-reach for gridding
     int y_bin_reach);				//   Y-reach for gridding

  void setOrigin				// set the grid origin
    (float x_bin_min,				//   X-minimum for the grid
     float y_bin_min);				//   Y-minimum for the grid

  void setBinSize				// set the grid origin
    (float x_bin_size,				//   size of bin in X-dir
     float y_bin_size);				//   size of bin in Y-dir

  int setMaxHits				// set maximum hits per grid
    (int max_hits);				//   maximum # of hits per grid

  int doGridding				// do the gridding
    (ControlPoints *cps,			//   control points for grid
     FloatGrid *grid);				//   resulting grid

  int doInserting				// do the inserting
    (ControlPoints *cps,			//   control points for grid
     FloatGrid *grid);				//   resulting grid

  int doAveraging				// do the averaging
    (ControlPoints *cps,			//   control points for grid
     FloatGrid *grid);				//   resulting grid

  int doSumming					// do the summing
    (ControlPoints *cps,			//   control points for grid
     FloatGrid *grid);				//   resulting grid

  GridErrorCodes errorStatus ()			// return error status flag
    { return _error_status; }

private:
  void sortWeightsWithXsAndYs			// sorts as indicated
    (long left,					//   left index of sort rng
     long right);				//   right index of sort rng
  void sortAbsXsWithWeightsAndYs		// sorts as indicated
    (long left,					//   left index of sort rng
     long right);				//   right index of sort rng
  void sortAbsYsWithWeightsAndXs		// sorts as indicated
    (long left,					//   left index of sort rng
     long right);				//   right index of sort rng
  int compareInt				// compare two ints
    (int value1,				//   first value
     int value2);				//   second value
  int compareAbsInt				// compare two ints
    (int value1,				//   first value
     int value2);				//   second value
  void swapInt					// swap ints
    (long k2,					//   1st index to swap
     long k3,					//   2nd index to swap
     int *array);				//   int array ptr

  int
    _x_bin_reach,				// X-reach for gridding
    _y_bin_reach,				// Y-reach for gridding
    _max_hits,					// max # of hits per grid
    *_xarr,					// pntr to Xs array
    *_yarr,					// pntr to Ys array
    *_warr;					// weight array

  float
    _x_bin_size,				// size of a bin in X-dir
    _y_bin_size,				// size of a bin in Y-dir
    _x_bin_min,					// X-minimum for the grid
    _y_bin_min;					// Y-minimum for the grid

  FloatGrid
    *_fgrid,					// temp grid to hold results
    *_counts;					// temp grid to hold counts

  GridErrorCodes
    _error_status;				// error status flag

};

#endif
