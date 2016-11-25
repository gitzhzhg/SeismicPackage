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
// auto_gridder_float.hh:  User interface file for AutoGridderFloat class
#ifndef AUTO_GRIDDER_FLOAT_HH
#define AUTO_GRIDDER_FLOAT_HH

#include "dp/grid_error_codes.hh"

class ControlPoints;				// control points data type
class FloatGrid;				// float grid data type
class GridderFloat;				// gridder data type
class FloatGridAccessor;			// float grid accessor type

class AutoGridderFloat {

public:
  enum {BOTH_X_AND_Y, X_ONLY, Y_ONLY};

  AutoGridderFloat				// constructor
    (int min_reach=3,				//   minimum reach from a bin
     int dont_resample=0,			//   flag not to resample (F=0)
     int dont_scale_a=1,			//   flag not to scale attrib
     int max_hits=3,				//   max # vlus to use for grid
     int dont_smooth=1,				//   flag not to smooth attrib
     int which_dimension=BOTH_X_AND_Y);		//   which dimension(s) to use

  ~AutoGridderFloat ();				// destructor

  int initializeCalculations			// initialize grid calculations
    (ControlPoints *cps,			//   cntrl pnts obj of whole gd
     FloatGridAccessor *gridA,			//   entire grid object
     float *x_distance,				//   X-reach (rtn) reqd by grd
     float *y_distance);			//   Y-reach (rtn) reqd by grd

  int setPointSize				// used after initializeCalcula
    (FloatGrid *grid,				//   entire grid object
     float num_x_bins,				//   # X-bins of one cntrl pnt
     float num_y_bins);				//   # Y-bins of one cntrl pnt

  int initializeInsert				// initialize insert operations
    (FloatGrid *grid,				//   entire grid object
     float x_min_grid_center,			//   minimum X-bin center
     float x_max_grid_center,			//   maximum X-bin center
     float y_min_grid_center,			//   minimum Y-bin center
     float y_max_grid_center,			//   maximum Y-bin center
     float x_bin_size,				//   size of bin to insert into
     float y_bin_size);				//   size of bin to insert into

  int analyze					// grid over given cntl pnts
    (ControlPoints *cps,			//   cntrl pnts obj all or part
     FloatGrid *grid);				//   entire grid object

  int insert					// insert given cntl pnts
    (ControlPoints *cps,			//   cntrl pnts obj all or part
     FloatGrid *grid);				//   entire grid object

  int insertControlPoints			// insert gvn cntl pnts int grd
    (ControlPoints *cps,			//   cntrl pnts obj all or part
     FloatGrid *grid);				//   entire grid object

  int firstColumn ();				// starting-X where grid chngd
  int firstRow ();				// starting-Y where grid chngd
  int columnCount ();				// # of X's changed in grid
  int rowCount ();				// # of Y's changed in grid

  int failed ();				// return 0 if successful call

  GridErrorCodes errorStatus ()			// return error status flag
    { return _error_status; }

private:
  int analyzeBinReach				// analyze bin reach & size
    (ControlPoints *cps);			//   cntrl pnts obj all or part

  int findAverageBinSizes			// find ave bin size
    (ControlPoints *cps);			//   control points object

  float findAverageDifference			// find average diff in array1
    (long count,				//   size of each array
     int *array1,				//   array to average diffs
     int *array2,				//   array to mark stops in ar1
     double threshold2);			//   thd diff btwn array2 vlus

  void sortIArrays				// descending sort of int array
    (long left,					//   left index to begin sort
     long right,				//   right index to end sort
     int *array1,				//   complete array to sort
     int *array2);				//   complete array to carry

  void sortJArrays				// descend sort of 2nd int arry
    (long count,				//   left index to begin sort
     int *array1,				//   complt array w/ equal runs
     int *array2);				//   complet array to sort runs

  void swapI					// swap two int values
    (long k2,					//   index of first value
     long k3,					//   index of second value
     int *array);				//   complete array

  int compareI					// compare two ints (-,0,+)
    (int value1,				//   first value to compare
     int value2);				//   second value to compare

  void sortIArray				// descending sort of int array
    (long left,					//   left index to begin sort
     long right,				//   right index to end sort
     int *array);				//   complete array to sort

  float findSingleAveDiff			// find average diff in array
    (long count,				//   size of each array
     int *array);				//   array to average diffs

  int findSingleAveBinSize			// find ave bin size
    (ControlPoints *cps);			//   control points object

  float
    *_array,					// temporary array pointer
    _x_min_grid_center,				// X-minimum in 1st grid center
    _x_max_grid_center,				// X-maximum in 1st grid center
    _y_min_grid_center,				// Y-minimum in Lst grid center
    _y_max_grid_center,				// Y-maximum in Lst grid center
    _x_bin_size,				// X-bin size for output grid
    _y_bin_size,				// Y-bin size for output grid
    _anal_x_bin_size,				// X-bin size used for gridding
    _anal_y_bin_size,				// Y-bin size used for gridding
    _anal_x_distance,				// X-reach needed for grid area
    _anal_y_distance,				// Y-reach needed for grid area
    _x_bin_min,					// X-minimum of 1st grid edge
    _y_bin_min,					// Y-minimum of 1st grid edge
    _x_to_y_ratio;				// aspect ratio of bin sizes

  int
    *_array_x,					// temporary array pointer
    *_array_y,					// temporary array pointer
    _num_x_bins,				// number of X-bins in grid
    _num_y_bins,				// number of Y-bins in grid
    _x_resampling_required,			// grid with less X's than outp
    _y_resampling_required,			// grid with less Y's than outp
    _min_reach,					// minimum reach used for grid
    _max_hits,					// max # of vlus usee per grid
    _anal_x_bin_reach,				// X-bin reach used for griddin
    _anal_y_bin_reach,				// Y-bin reach used for griddin
    _out_x_bin_reach,				// X-bin reach for grid as out
    _out_y_bin_reach,				// Y-bin reach for grid as out
    _first_column,				// starting-X where grid chng
    _first_row,					// starting-Y where grid chng
    _column_count,				// # of X's changed in grid
    _row_count,					// # of Y's changed in grid
    _dont_resample,				// flag not to resample (F=0)
    _dont_scale_a,				// flag not to scale (F=0)
    _dont_smooth,				// flag not to smooth (F=0)
    _which_dimension,				// which dimension(s) to use
    _small_not_unique,				// flag for temp float grid
    _large_not_unique;				// flag for temp float grid

  FloatGrid
    *_small_grid,				// temporary float grid
    *_large_grid;				// temporary float grid

  GridderFloat
    *_gdr;					// temporary gridder

  GridErrorCodes
    _error_status;				// error status flag

};

#endif
