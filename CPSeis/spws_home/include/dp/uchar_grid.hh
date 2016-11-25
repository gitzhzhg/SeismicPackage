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
// uchar_grid.hh:  User interface file for UCharGrid class

// The X-bins, Y-bins, and Z-bins of the grid are organized as follows:
//   The (x_bin,y_bin,z_bin) = (0,0,0) of the grid is in the top left corner
//   of the front plane.
//   Progression through the grid goes first from top to bottom (y) and
//   then from left to right (x) and then from front to back (z).
// The Xs, Ys, and Zs of the grid are as follows:
//   The (x,y,z) = (0,0,0) of the grid is in the lower left corner of the
//   back plane
// Note the XYZ-bin coordinate system reflects the way that the data is stored.
//   The XYZ coordinate system reflects the normal RH cartesian coordinate
//   system.
// Y-varying most rapidly pointing down was selected for historical reasons.
//   X-varying second most rapidly pointing to the right was also inherited.
//   Z-varying third most rapidly pointing from front to back was chosen so
//   that in the case that this class is used to store multi-attribute data
//   for analysis, the Y-variable could be used for the attribute dimension,
//   (varying most rapidly) and then the Z-X plane could be treated analogously
//   to the previous X-Y plane.  (i.e. with Y being used for the attribute
//   dimenision, now Z is like the previous X in direction and order and
//   similarly, X is like the previous Y in direction and order.) 

#ifndef UCHAR_GRID_HH
#define UCHAR_GRID_HH

#include "dp/grid_error_codes.hh"
#include "dp/grid_constants.hh"

class FloatToUChar;
class DoAbort;

class UCharGrid {

friend class FloatToUChar;

public:
  UCharGrid					// constructor
    (int num_x_bins,				//   number of X-bins in grid
     int num_y_bins,				//   number of Y-bins in grid
     int num_z_bins=1,				//   number of Z-bins in grid
     DoAbort *do_abort=0);			//   object for user abort

  UCharGrid					// copy constructor
    (const UCharGrid &from);			//   grid to copy from

  ~UCharGrid ();				// destructor

  void setInsertSize				// set the insert block size
    (int x_insert_size = 1,			//   X-dimensional size of blk
     int y_insert_size = 1,			//   Y-dimensional size of blk
     int z_insert_size = 1);			//   Z-dimensional size of blk

  int setRange					// set the bin value range
    (unsigned char not_defined=255,		//   undefined grid value
     unsigned char set_minimum=0,		//   set minimum grid value
     unsigned char set_maximum=254);		//   set maximum grid value

  unsigned char getUndefined ()			// return undefinded grid vlu
    { return _not_defined; }

  unsigned char getMinimum ()			// return set minimum grid vlu
    { return _set_minimum; }

  unsigned char getMaximum ()			// return set maximum grid vlu
    { return _set_maximum; }

  unsigned char *getArray ()			// return grid array pointer
    { return _grid; }

  DoAbort *getDoAbort ()			// return object for user abort
    { return _do_abort; }

  long getArraySize () const			// return size of grid array
    { return (long)_num_x_bins*_num_y_bins
        *_num_z_bins; }

  int getNumXBins () const			// return number of X-bins
    { return _num_x_bins; }

  int getNumYBins () const			// return number of Y-bins
    { return _num_y_bins; }

  int getNumZBins () const			// return number of Z-bins
    { return _num_z_bins; }

  int setSubSize				// set the size of a sub grid
    (int sub_x_bins,				//   # of bins in X-dir of sub
     int sub_y_bins,				//   # of bins in Y-dir of sub
     int sub_z_bins=1);				//   # of bins in Z-dir of sub

  int getSubXBins () const			// return number of sub X-bins
    { return _sub_x_bins; }

  int getSubYBins () const			// return number of sub Y-bins
    { return _sub_y_bins; }

  int getSubZBins () const			// return number of sub Z-bins
    { return _sub_z_bins; }

  int setSubStart				// set strtn indxs for sub grid
    (int sub_start_x,			        //   X-coord where to strt sub
     int sub_start_y,				//   Y-coord where to strt sub
     int sub_start_z=0);			//   Z-coord where to strt sub

  int setSubEnd					// set endng indxs for sub grid
    (int sub_end_x,			        //   X-coord where to end sub
     int sub_end_y,				//   Y-coord where to end sub
     int sub_end_z=0);				//   Z-coord where to end sub

  int setSubBinStart				// set strtn indxs for sub grid
    (int sub_start_x_bin,			//   col-indx where to strt sub
     int sub_start_y_bin,			//   row-indx where to strt sub
     int sub_start_z_bin=0);			//   pln-indx where to strt sub

  int getSubXBinStart () const			// return starting column
    { return _sub_start_x; }

  int getSubYBinStart () const;			// return starting row

  int getSubZBinStart () const;			// return starting plane

  int getSubXStart () const			// return starting X-coordinate
    { return _sub_start_x; }

  int getSubYStart () const			// return starting Y-coordinate
    { return _sub_start_y; }

  int getSubZStart () const			// return starting Z-coordinate
    { return _sub_start_z; }

  int move					// do the move from sub *this
    (UCharGrid *to) const;			//   what to move *this to

  int fill					// fill the sub *this
    (float value);				//   value to fill with

  long countUndefined ();			// count up undefined grids

  int resetUndefined				// reset the value for undefine
    (unsigned char new_not_defined=255);	//   new value to be tried

  unsigned char findMinimum ();			// return actual grid minimum

  unsigned char findMaximum ();			// return actual grid maximum

  float findStatisticalMinimum			// return actual grid value
    (float std_dev=1.5);			//   this many std dev's < ave

  float findStatisticalMaximum			// return actual grid value
    (float std_dev=1.5);			//   this many std dev's > ave

  int getX					// return X-coordinate
    (long index) const;				//   given array index

  int getXOffset				// return X-offset
    (long offset) const;			//   given array offset

  int getY					// return Y-coordinate
    (long index) const;				//   given array index

  int getYOffset				// return Y-offset
    (long offset) const;			//   given array offset

  int getZ					// return Z-coordinate
    (long index) const;				//   given array index

  int getZOffset				// return Z-offset
    (long offset) const;			//   given array offset

  long getIndex					// return index in array given
    (int x,					//   X-coordinate
     int y,					//   Y-coordinate
     int z=0) const;				//   Z-coordinate

  long getOffset				// return offset in array given
    (int x_offset,				//   X-coordinate offset
     int y_offset,				//   Y-coordinate offset
     int z_offset=0) const;			//   Z-coordinate offset

  int columnIndexAtX				// return column index given X
    (int x) const;				//   X-coordinate

  int rowIndexAtY				// return row index given y
    (int y) const;				//   Y-coordinate

  int planeIndexAtZ				// return plane index given z
    (int z) const;				//   Z-coordinate

  int valueIsInRange				// checks on value range
    (float value) const;			//   value to check out

  int valueIsValid				// checks on value validity
    (float value) const;			//   value to check out

  void resetExtremaReadyFlag ();		// reset the _extrema_ready flg

  int accumulateXYZ				// accumulate at a grid by xyz
    (float value,				//   value to add at given xyz
     int x,					//   X-coordinate
     int y,					//   Y-coordinate
     int z=0);					//   Z-coordinate

  int accumulateIndex				// accumulate at a grid by indx
    (long index,				//   array index
     float value);				//   value to add at given indx

  int insertXYZ					// insert at a grid by x,y,z
    (float value,				//   value to add at given indx
     int x,					//   X-coordinate
     int y,					//   Y-coordinate
     int z=0);					//   Z-coordinate

  int insertIndex				// insert at a grid by index
    (long index,				//   array index
     float value);				//   valu to insert at gvn indx

  UCharGrid &operator=				// assignment operator
    (float value);				//   right hand value

  UCharGrid &operator+=				// self sum operator
    (float value);				//   right hand value

  UCharGrid &operator*=				// self product operator
    (float value);				//   right hand value

  UCharGrid &operator/=				// self quotient operator
    (float value);				//   right hand value

  UCharGrid &operator-=				// self subtraction operator
    (float value);				//   right hand value

  int failed ();				// returns 1 if unsuccessful

  GridErrorCodes errorStatus ()			// returns error status flag
    { return _error_status; }

private:
  int findExtrema ();				// determine actual grid extrma

  int findStatistics ();			// determine actual grid stats

  int process					// generic process on sub *to
    (Function3 function,			//   generic function to do
     UCharGrid *to) const;			//   UCharGrid result

  int process					// generic process on sub *this
    (Function4 function,			//   generic function to do
     float value);				//   constant to do processing

  unsigned char valueAtIndex			// returns the grid vlu @index
    (long index) const;				//   index of bin to do

  unsigned char constantValue			// returns value that is given
    (long /* index */,				//   index of bin to do (unusd)
     float value) const;			//   value given and returned

  unsigned char divideValue			// returns grid quotient @index
    (long index,				//   index of bin to do
     float value) const;			//   right hand value

  unsigned char multiplyValue			// returns grid product @index
    (long index,				//   index of bin to do
     float value) const;			//   right hand value

  unsigned char addValue			// returns the grid sum @index
    (long index,				//   index of bin to do
     float value) const;			//   right hand value

  unsigned char subtractValue			// returns grid differenc @indx
    (long index,				//   index of bin to do
     float value) const;			//   right hand value

  int completelyUsed () const;			// 0 if Sub not equal to all

  int completelyCoveredBy			// 0 if Sub does not cover *by
    (const UCharGrid *by) const;		//   grid to compare to

  int copyHelper				// copy constructor helper
    (const UCharGrid &from);			//   grid to copy from

  DoAbort
    *_do_abort;					// object for user abort

  int
    _extrema_ready,				// extrema status flag (F=0)
    _statistics_ready,				// statistics status flag (F=0)
    _grid_not_defined,				// grid not defined flag (F=0)
    _num_x_bins,				// # of X-bins in grid
    _num_y_bins,				// # of Y-bins in grid
    _num_z_bins,				// # of Z-bins in grid
    _sub_start_x,				// X-coord wher to strt sub grd
    _sub_start_y,				// Y-Coord wher to strt sub grd
    _sub_start_z,				// Z-Coord wher to strt sub grd
    _sub_x_bins,				// # of bins in X-dir of sub gd
    _sub_y_bins,				// # of bins in Y-dir of sub gd
    _sub_z_bins,				// # of bins in Z-dir of sub gd
    _multi_insert,				// flag=0 if insert block = 1X1
    _x_first_insert,				// X-siz of 1st half of ins blk
    _x_last_insert,				// X-siz of Lst half of ins blk
    _y_first_insert,				// Y-siz of 1st half of ins blk
    _y_last_insert,				// Y-siz of Lst half of ins blk
    _z_first_insert,				// Z-siz of 1st half of ins blk
    _z_last_insert;				// Z-siz of Lst half of ins blk

  unsigned char
    _actual_minimum,				// actual minimum grid value
    _actual_maximum,				// actual maximum grid value
    _not_defined,				// undefined grid value
    _set_minimum,				// set minimum grid value
    _set_maximum,				// set maximum grid value
    *_grid;					// pointer to grid array

  float
    _actual_average,				// actual average grid value
    _actual_std_dev;				// actual std dev grid value

  GridErrorCodes
    _error_status;				// error status flag

};

#endif
