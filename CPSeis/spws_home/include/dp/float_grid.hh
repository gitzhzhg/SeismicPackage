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
// The X-bins and Y-bins of the grid are organized as follows:
//   The (X-bin,Y-bin) = (0,0) of the grid is in the top left corner.
//   Progression through the grid goes first from top to bottom (y) and
//   then from left to right (x).
// The Xs and Ys of the grid are as follows:
//   The (x,y) = (0,0) of the grid is in the lower left corner

// float_grid.hh:  User interface file for FloatGrid class
#ifndef FLOAT_GRID_HH
#define FLOAT_GRID_HH

#include "dp/grid_error_codes.hh"
#include "dp/grid_constants.hh"

class FloatToUChar;
class DoAbort;

class FloatGrid {

friend class FloatToUChar;

public:
  FloatGrid					// constructor
    (int num_x_bins,				//   number of X-bins in grid
     int num_y_bins,				//   number of Y-bins in grid
     DoAbort *do_abort = 0);			//   object to allow user abort

  FloatGrid					// copy constructor
    (const FloatGrid &from);			//   grid to copy from

  ~FloatGrid ();				// destructor

  void setInsertSize				// set the insert block size
    (int x_insert_size = 1,			//   X-dimensional size of blk
     int y_insert_size = 1);			//   Y-dimensional size of blk

  int getXInsertSize ()				// get the insert block X-dimen
    { return _x_insert_size; }

  int getYInsertSize ()				// get the insert block Y-dimen
    { return _y_insert_size; }

  int setRange					// set the bin value range
    (float not_defined,				//   undefined grid value
     float set_minimum,				//   set minimum grid value
     float set_maximum);			//   set maximum grid value

  float getUndefined ()				// return undefinded grid vlu
    { return _not_defined; }

  int gridIsDefined ()				// return 1 if grid is defined
    { return !_grid_not_defined; }

  float getMinimum ()				// return set minimum grid vlu
    { return _set_minimum; }

  float getMaximum ()				// return set maximum grid vlu
    { return _set_maximum; }

  float *getArray ()				// return grid array pointer
    { return _grid; }

  DoAbort *getDoAbort ()			// return DoAbort obj pntr
    { return _do_abort; }

  long getArraySize () const			// return size of grid array
    { return (long)_num_x_bins*_num_y_bins; }

  int getNumXBins () const			// return number of X-bins
    { return _num_x_bins; }

  int getNumYBins () const			// return number of Y-bins
    { return _num_y_bins; }

  int setSubSize				// set the size of a sub grid
    (int sub_x_bins,				//   # of bins in X-dir of sub
     int sub_y_bins);				//   # of bins in Y-dir of sub

  int getSubXBins () const			// return number of sub X-bins
    { return _sub_x_bins; }

  int getSubYBins () const			// return number of sub Y-bins
    { return _sub_y_bins; }

  int setSubStart				// set strtn indxs for sub grid
    (int sub_start_x,			        //   X-coord where to strt sub
     int sub_start_y);				//   Y-coord where to strt sub

  int setSubEnd					// set endng indxs for sub grid
    (int sub_end_x,			        //   X-coord where to end sub
     int sub_end_y);				//   Y-coord where to end sub

  int setSubBinStart				// set strtn indxs for sub grid
    (int sub_start_x_bin,			//   col-indx where to strt sub
     int sub_start_y_bin);			//   row-indx where to strt sub

  int getSubXBinStart () const			// return starting column
    { return _sub_start_x; }

  int getSubYBinStart () const;			// return starting row

  int getSubXBinInsertStart () const;		// return starting col with insert

  int getSubYBinInsertStart () const;		// return starting row with insert

  int getSubXBinsInsert () const;		// rtrn # of sub X-bins with insrt

  int getSubYBinsInsert () const;		// rtrn # of sub Y-bins with insrt

  int getSubXBinInsertEnd () const;		// return ending col with insert

  int getSubYBinInsertEnd () const;		// return ending row with insert

  int getSubXStart () const			// return starting X-coordinate
    { return _sub_start_x; }

  int getSubYStart () const			// return starting Y-coordinate
    { return _sub_start_y; }

  int move					// do the move from sub *this
    (FloatGrid *to) const;			//   what to move *this to

  int moveWithInsert				// insert sub *this into to
    (FloatGrid *to) const;			//   what to move *this to

  int fill					// fill the sub *this
    (float value);				//   value to fill with

  int fillOutside				// fill outside the sub
    (float value);				//   value to outside fill with

  int divide					// divide this
    (FloatGrid *by);				//   by by and store in this

  int divide					// divide this
    (FloatGrid *by,				//   by by and
     FloatGrid *to);				//   store in to

  int resample					// resample *this
    (FloatGrid *to,				//   resulting resampled grid
     int NN_only=0);				//   flag = 0 do BL interpolatn

  long countUndefined ();			// count up undefined grids

  int convert ();				// convert *this from actual
						//   extrema to set extrema

  int resetUndefined				// reset the value for undefine
    (float new_not_defined);			//   new value to be tried

  int setRescaleParameters			// set rescale gain and bias
    (float gain,				//   gain to apply in rescale
     float bias);				//   bias to apply in rescale

  int rescale ();				// apply given gain and bias

  int rescaleWithClip ();			// apply given gain and bias

  float findMinimum ();				// return actual grid minimum

  float findMaximum ();				// return actual grid maximum

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

  long getIndex					// return index in array given
    (int x,					//   X-coordinate
     int y) const;				//   Y-coordinate

  long getOffset				// return offset in array given
    (int x_offset,				//   X-coordinate offset
     int y_offset) const;			//   Y-coordinate offset

  int valueIsValid				// checks on value validity
    (float value) const;			//   value to check out

  void resetExtremaReadyFlag ();		// reset the _extrema_ready flg

  void accumulateXY				// accumulate at a grid by x,y
    (int x,					//   X-coordinate
     int y,					//   Y-coordinate
     float value);				//   value to add at given x,y

  void accumulateIndex				// accumulate at a grid by indx
    (long index,				//   array index
     float value);				//   value to add at given indx

  void insertXY					// insert at a grid by x,y
    (int x,					//   X-coordinate
     int y,					//   Y-coordinate
     float value);				//   value to add at given indx

  void insertIndex				// insert at a grid by index
    (long index,				//   array index
     float value);				//   valu to insert at gvn indx

  int initializeSearchGridPattern		// initialize a search grid ptt
    (int max_hits,				//   maximum number of pnts usd
     int length);				//   length of the pattern

  int setSearchGridPatternOffset		// set an indx in search pattrn
    (int index,					//   which index in pattern
     int x_offset,				//   X-coordinate offset
     int y_offset);				//   Y-coordinate offset

  int setSearchGridPatternWeight		// set a weight in search pattn
    (int index,					//   which index in pattern
     float weight);				//   weight to be set in pattrn

  int searchGrid				// search grid function
    (FloatGrid *to) const;			//   resulting grid

  friend FloatGrid searchGrid			// search grid function
    (const FloatGrid &operand);			//   grid operand

  int convolve					// convolve function
    (FloatGrid *to) const;			//   resulting grid

  friend FloatGrid conv 			// convolve function
    (const FloatGrid &operand);			//   grid operand

  int correlate					// correlate function
    (FloatGrid *to) const;			//   resulting grid

  friend FloatGrid correl 			// correlate function
    (const FloatGrid &operand);			//   grid operand

  FloatGrid &operator=				// assignment operator
    (const FloatGrid &right);			//   right hand grid

  FloatGrid &operator=				// assignment operator
    (float value);				//   constant rhs value

  FloatGrid operator+				// sum operator
    (const FloatGrid &right) const;		//   right hand grid

  FloatGrid operator+				// sum operator w/ rhs constant
    (float value) const;			//   right hand constant value

  friend FloatGrid operator+			// sum operator w/ lhs constant
    (float value,				//   left hand constant value
     const FloatGrid &right);			//   right hand grid

  FloatGrid operator*				// mulitiplication operator
    (const FloatGrid &right) const;		//   right hand grid

  FloatGrid operator*				// mulitiplication operator
    (float value) const;			//   right hand constant value

  friend FloatGrid operator*			// mulitiplication operator
    (float value,				//   left hand constant value
     const FloatGrid &right);			//   right hand grid

  FloatGrid operator/				// division operator
    (const FloatGrid &right) const;		//   right hand grid

  FloatGrid operator-				// subtraction operator
    (const FloatGrid &right) const;		//   right hand grid

  FloatGrid &operator+=				// self sum operator
    (const FloatGrid &right);			//   right hand grid

  FloatGrid &operator+=				// self sum operator
    (float value);				//   right hand constant

  FloatGrid &operator*=				// self product operator
    (const FloatGrid &right);			//   right hand grid

  FloatGrid &operator*=				// self product operator
    (float value);				//   right hand constant

  FloatGrid &operator/=				// self quotient operator
    (const FloatGrid &right);			//   right hand grid

  FloatGrid &operator/=				// self quotient operator
    (float value);				//   right hand constant

  FloatGrid &operator-=				// self subtraction operator
    (const FloatGrid &right);			//   right hand grid

  FloatGrid &operator-=				// self subtraction operator
    (float value);				//   right hand constant

  friend FloatGrid sqrt				// sqrt function
    (const FloatGrid &operand);			//   grid operand

  int failed ();				// returns 1 if unsuccessful

  GridErrorCodes errorStatus ()			// returns error status flag
    { return _error_status; }

  void resetErrorStatus ()			// reset error status flag
    { _error_status = FG_SUCCESSFUL; }

private:
  FloatGrid					// processing constructor
    (Function1 function,			//   generic processin function
     const FloatGrid &left,			//   left hand side grid
     const FloatGrid &right);			//   right hand side grid

  FloatGrid					// processing constructor
    (Function1 function,			//   generic processin function
     const FloatGrid &operand);			//   operand grid

  FloatGrid					// processing constructor
    (Function2 function,			//   generic processin function
     const FloatGrid &left,			//   left hand side grid
     const FloatGrid &right);			//   right hand side grid

  FloatGrid					// processing constructor
    (Function2 function,			//   generic processin function
     const FloatGrid &left,			//   left hand side grid
     float value);				//   right hand side constant

  int findExtrema ();				// determine actual grid extrma

  int findStatistics ();			// determine actual grid stats

  int process					// generic process on sub *this
    (Function1 function,			//   generic function to do
     FloatGrid *to) const;			//   resulting grid

  int process					// generic process on sub *this
    (Function2 function,			//   generic function to do
     FloatGrid *to) const;			//   resulting grid

//  int inserter                                  // generic inserter of sub *this
//    (Function5 function,                        //   generic insertion function
//     FloatGrid *to) const;                      //   resulting grid
  int inserter                                  // inserter of sub *this
    (FloatGrid *to) const;                      //   resulting grid

  int process					// generic process on sub *this
    (Function2 function,			//   generic function to do
     float value);				//   constant to do processing

  float searchGridIndex				// search grid at index
    (long index) const;				//   index of bin to do

  float convolveIndex				// convolve grid at index
    (long index) const;				//   index of bin to do

  float correlateIndex				// correlate grid at index
    (long index) const;				//   index of bin to do

  int initializeCorrelatorGradient ();		// init correl gradient operatr

  float valueAtIndex				// returns the grid vlu @index
    (long index) const;				//   index of bin to do

  float constantValue				// returns value that is given
    (long /* index */,				//   index of bin to do (unusd)
     float value) const;			//   value given and returned

  float unity					// returns 1 for every index
    (long /* index */) const;			//   index of bin to do (unusd)

  float zero					// returns 0 for every index
    (long /* index */) const;			//   index of bin to do (unusd)

  float sqrtIndex				// returns grid sqrt vlu @index
    (long index) const;				//   index of bin to do

  float divideIndex				// returns grid quotient @index
    (long index,				//   index of bin to do
     float value) const;			//   left hand value

  float divideValue				// returns grid quotient @index
    (long index,				//   index of bin to do
     float value) const;			//   right hand value

  float multiplyIndex				// returns grid product @index
    (long index,				//   index of bin to do
     float value) const;			//   left hand value

  float addIndex				// returns the grid sum @index
    (long index,				//   index of bin to do
     float value) const;			//   left hand value

  float subtractIndex				// returns grid differenc @indx
    (long index,				//   index of bin to do
     float value) const;			//   left hand value

  float subtractValue				// returns grid differenc @indx
    (long index,				//   index of bin to do
     float value) const;			//   right hand value

  float rescaleIndex				// returns grid rescaled @indx
    (long index) const;				//   index of bin to do

  float rescaleIndexWithClip			// returns grid rescaled @indx
    (long index) const;				//   index of bin to do

  int completelyUsed () const;			// 0 if Sub not equal to all

  int completelyCoveredBy			// 0 if Sub does not cover *by
    (const FloatGrid *by) const;		//   grid to compare to

  int copyHelper				// copy constructor helper
    (const FloatGrid &from);			//   grid to copy from

  DoAbort
    *_do_abort;					// object to allow user abort

  int
    *_across1,					// temp array for resampling
    *_across2,					// temp array for resampling
    *_down1,					// temp array for resampling
    *_down2,					// temp array for resampling
    _extrema_ready,				// extrema status flag (F=0)
    _statistics_ready,				// statistics status flag (F=0)
    _grid_not_defined,				// grid not defined flag (F=0)
    _max_hits,					// maximum search pnts used
    _pattern_length,				// length of search pattern
    _correl_x_reach,				// X-dist from indx to correl
    _correl_y_reach,				// Y-dist from indx to correl
    _num_x_bins,				// # of X-bins in grid
    _num_y_bins,				// # of Y-bins in grid
    _sub_start_x,				// X-coord wher to strt sub grd
    _sub_start_y,				// Y-Coord wher to strt sub grd
    _sub_x_bins,				// # of bins in X-dir of sub gd
    _sub_y_bins,				// # of bins in Y-dir of sub gd
    _multi_insert,				// flag=0 if insert block = 1X1
    _x_insert_size,				// X-siz of insert block
    _y_insert_size,				// Y-siz of insert block
    _x_first_insert,				// X-siz of 1st half of ins blk
    _x_last_insert,				// X-siz of Lst half of ins blk
    _y_first_insert,				// Y-siz of 1st half of ins blk
    _y_last_insert;				// Y-siz of Lst half of ins blk

  long
    *_pattern_offsets;				// search pattern offset array

  float
    *_col1,					// temp array used in resample
    *_col2,					// temp array used in resample
    *_awts1,					// temp array used in resample
    *_awts2,					// temp array used in resample
    *_dwts1,					// temp array used in resample
    *_dwts2,					// temp array used in resample
    _rescale_gain,				// gain to apply in rescale
    _rescale_bias,				// bias to apply in rescale
    _actual_minimum,				// actual minimum grid value
    _actual_maximum,				// actual maximum grid value
    _actual_average,				// actual average grid value
    _actual_std_dev,				// actual std dev grid value
    _not_defined,				// undefined grid value
    _set_minimum,				// set minimum grid value
    _set_maximum,				// set maximum grid value
    *_pattern_weights,				// search pattern weigth array
    *_grid;					// pointer to grid array

  GridErrorCodes
    _error_status;				// error status flag

};

#endif
