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
// float_grid_accessor.hh:  User interface file for FloatGridAccessor class
#ifndef FLOAT_GRID_ACCESSOR_HH
#define FLOAT_GRID_ACCESSOR_HH

#include "dp/grid_error_codes.hh"

class FloatGrid;

class FloatGridAccessor {

public:
  FloatGridAccessor ();				// constructor

  ~FloatGridAccessor ();			// destructor

  int specifyData				// specify which grid to access
    (FloatGrid *grid);				//   float grid pointer selectd

  FloatGrid *getData ()				// return grid to access
    { return _grid; }
        
  int setXBinData				// sets X-coordinate data
    (float x_min_grid_edge,			//   minimum X-bin edge
     float x_max_grid_edge);			//   maximum X-bin edge

  int setYBinData				// sets Y-coordinate data
    (float y_min_grid_edge,			//   minimum Y-bin edge
     float y_max_grid_edge);			//   maximum Y-bin edge

  float getXBinSize ()				// returns size of X-Bin
    { return _x_bin_size; }

  float getYBinSize ()				// returns size of Y-Bin
    { return _y_bin_size; }
  
  int getGridX					// return a grid X-coordinate
    (float x);					//   X-coordinate to access grd

  int getGridY					// return a grid Y-coordinate
    (float y);					//   Y-coordinate to access grd

  float getGridXLength				// return X-grid length given
    (float x1,					//   one X-coordinate and
     float x0);					//   another X-coordinate

  float getGridYLength				// return Y-grid length given
    (float y1,					//   one Y-coordinate and
     float y0);					//   another Y-coordinate

  float getX					// return a bin center X-coord
    (int x);					//   grid X-coordinate

  float getY					// return a bin center Y-coord
    (int y);					//   grid Y-coordinate

  int failed ();				// returns 1 if unsuccessful

  GridErrorCodes errorStatus ()			// returns error status flag
    { return _error_status; }

private:
  FloatGrid *_grid;				// selected FloatGrid to access

  float
    _x_bin_min,					// minimum X-bound
    _x_bin_max,					// maximum X-bound
    _y_bin_min,					// minimum Y-bound
    _y_bin_max,					// maximum Y-bound
    _x_bin_size,				// size of X-bin
    _y_bin_size;				// size of Y-bin

  GridErrorCodes
    _error_status;				// error status flag

};

#endif
