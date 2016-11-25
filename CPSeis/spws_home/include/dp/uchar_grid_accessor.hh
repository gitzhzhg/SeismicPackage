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
// uchar_grid_accessor.hh:  User interface file for UCharGridAccessor class
#ifndef UCHAR_GRID_ACCESSOR_HH
#define UCHAR_GRID_ACCESSOR_HH

#include "dp/grid_error_codes.hh"

class UCharGrid;

class UCharGridAccessor {

public:
  UCharGridAccessor ();				// constructor

  ~UCharGridAccessor () {}			// destructor

  int specifyData				// specify which grid to access
    (UCharGrid *grid);				//   byte grid pointer selected
        
  int setXBinData				// sets X-coordinate data
    (float x_min_grid_edge,			//   minimum X-bin edge
     float x_max_grid_edge);			//   maximum X-bin edge

  int setYBinData				// sets Y-coordinate data
    (float y_min_grid_edge,			//   minimum Y-bin edge
     float y_max_grid_edge);			//   maximum Y-bin edge

  int setZBinData				// sets Z-coordinate data
    (float z_min_grid_edge=0.0,			//   minimum Z-bin edge
     float z_max_grid_edge=0.0);		//   maximum Z-bin edge

  float getXBinSize ()				// returns size of X-Bin
    { return _x_bin_size; }

  float getYBinSize ()				// returns size of Y-Bin
    { return _y_bin_size; }

  float getZBinSize ()				// returns size of Z-Bin
    { return _z_bin_size; }

  int getGridX					// return a grid X-coordinate
    (float x);					//   X-coordinate to access grd

  int getGridY					// return a grid Y-coordinate
    (float y);					//   Y-coordinate to access grd

  int getGridZ					// return a grid Z-coordinate
    (float z);					//   Z-coordinate to access grd

  float getX					// return a bin center X-coord
    (int x);					//   grid X-coordinate

  float getY					// return a bin center Y-coord
    (int y);					//   grid Y-coordinate

  float getZ					// return a bin center Z-coord
    (int z);					//   grid Z-coordinate

  void startROIWatch ()				// begin looking for ROI extrem
    {_first_xyz_point = 1;}

  float ROIMinimumX ()				// returns minimum X accessed
    {return _x_min;}				//   since startROIWatch() call

  float ROIMaximumX ()				// returns maximum X accessed
    {return _x_max;}				//   since startROIWatch() call

  float ROIMinimumY ()				// returns minimum Y accessed
    {return _y_min;}				//   since startROIWatch() call

  float ROIMaximumY ()				// returns maximum Y accessed
    {return _y_max;}				//   since startROIWatch() call

  float ROIMinimumZ ()				// returns minimum Z accessed
    {return _z_min;}				//   since startROIWatch() call

  float ROIMaximumZ ()				// returns maximum Z accessed
    {return _z_max;}				//   since startROIWatch() call

  int accumulateXYZ				// accumulate at a grid by xyz
    (float value,				//   value to add at given xyz
     float x,					//   X-coordinate
     float y,					//   Y-coordinate
     float z=0);				//   Z-coordinate

  int insertXYZ					// insert at a grid by xyz
    (float value,				//   value to add at given xyz
     float x,					//   X-coordinate
     float y,					//   Y-coordinate
     float z=0);				//   Z-coordinate

  int columnIndexAtX				// rtrn grid column index at X
    (float x);					//   X-coordinate

  int rowIndexAtY				// rtrn grid row index at Y
    (float Y);					//   X-coordinate

  int planeIndexAtZ				// rtrn grid plane index at Z
    (float Z);					//   X-coordinate

  int failed ();				// returns 1 if unsuccessful

  GridErrorCodes errorStatus ()			// returns error status flag
    { return _error_status; }

private:
  void watchXYZ					// looks for extrema in x,y,z
    (float x,					//   X-coordinate to check
     float y,					//   Y-coordinate to check
     float z);					//   Z-coordinate to check

  UCharGrid *_grid;				// selected UCharGrid to access

  int
    _first_xyz_point;				// flag=1 when ROI watch reset

  float
    _x_bin_min,					// minimum X-bound
    _x_bin_max,					// maximum X-bound
    _y_bin_min,					// minimum Y-bound
    _y_bin_max,					// maximum Y-bound
    _z_bin_min,					// minimum Z-bound
    _z_bin_max,					// maximum Z-bound
    _x_bin_size,				// size of X-bin
    _y_bin_size,				// size of X-bin
    _z_bin_size,				// size of X-bin
    _x_min,					// current ROI minimum x
    _x_max,					// current ROI maximum x
    _y_min,					// current ROI minimum y
    _y_max,					// current ROI maximum y
    _z_min,					// current ROI minimum z
    _z_max;					// current ROI maximum z

  GridErrorCodes
    _error_status;				// error status flag

};

#endif
