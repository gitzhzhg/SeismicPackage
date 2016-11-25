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
// Base class for manipulating grids

#ifndef COMPUTE_GRID_HH
#define COMPUTE_GRID_HH

#include "dp/grid_error_codes.hh"

class FloatGrid;
class FloatGridAccessor;
class UCharGrid;
class MultiAttributes;
class UCharGridAccessor;
class FloatToUChar;
class MultiAttributes;
class InputLUT;
class OutputLUT;

class ComputeGrid {

public:
  ComputeGrid					// constructor
    (int number_of_inputs);			//   number of input grids

  virtual ~ComputeGrid ();			// destructor

  void deleteLUTObjects ();			// delete existing LUT objects

  int numberOfInputs ()				// rtns # of input grids
    { return _number_of_inputs; }

  int inputsOutOfDate ()			// rtn 1 if inputs out of date
    { return _inputs_out_of_date; }

  int resultOutOfDate ()			// rtn 1 if result out of date
    { return _result_out_of_date; }

  OutputLUT *resultLUT ()			// rtn result Look-Up-Table
    { return _result_LUT; }

  int compressionStateIsSet ();			// rtn 1 if inputs compressed

  int enoughGridsAreValid ();			// rtn 1 if min # inputs exist

  int resultIsDisplayed ();			// rtn 1 if result is displayed

  int rescaleResultStateIsSet ();		// rtn 1 if result rescaled

  int setCompressionState			// set compression state
    (long compressed_inputs_flag);		//   whether or not to compress

  int setResultState				// set result state
    (long which_result);			//   which result to do

  int changeProportion ();			// change proportion of reslt

  int byLUT					// generate the result by LUT
    (long which_result);			//   which result to do

  int byGrid					// generate the result by Grid
    (long which_result);			//   which result to do

  int setRescaleResultState			// rescale or unrescale result
    (long rescaled_result_flag);		//   whether or not to scale

  int updateResult ();				// causes a redisplay of result

  virtual int gridValid				// rtn 1 if grid is valid at
    (long which_input) = 0;			//   given index

  void setPercentOfB				// set the percent of B
    (float percent=50);				//   given percent of B

  float percentOfB ();				// return percent of B

  float *percentOfBLoc ();			// return location of % of B

  int failed ();				// returns 1 if unsuccessful

  GridErrorCodes errorStatus ()			// returns error status flag
    { return _error_status; }

protected:
  void inputsChanged ();			// set a result out of date flg

  int validateIndex				// ensure given index is valid
    (int index);				//   at given index

  virtual void setInputGrid			// set an input float grid (pur
    (void *data,				//   object where grid is found
     long which_input) = 0;			//   index for input grid

  virtual void *getInputGrid			// get obj where inpt grid is
    (long which_input) = 0;			//   index for input grid (pure

  virtual int displayLUT () = 0;		// display the LUT (pure)

  virtual int displayGrid () = 0;		// display the result (pure)
  
  virtual FloatGrid *getInputFloatGrid		// get input float grid object
    (int index) = 0;				//   at given index

  virtual FloatGrid *getResultFloatGrid () = 0;	// get result float grid object

  virtual int getNumColors () = 0;		// get number of colors allowed

  virtual float resultMinAmp () = 0;		// get minimum amplitude of res

  virtual float resultMaxAmp () = 0;            // get maximum amplitude of res

  virtual float inputX0				// get first input X-coordinate
    (int index) = 0;				//   at given index

  virtual float inputX1				// get second input X-coordinat
    (int index) = 0;				//   at given index

  virtual float inputY0				// get first input Y-coordinate
    (int index) = 0;				//   at given index

  virtual float inputY1				// get second input Y-coordinat
    (int index) = 0;				//   at given index

  virtual float resultX0 () = 0;		// get first result X-coordinat

  virtual float resultX1 () = 0;		// get second result X-coordina

  virtual float resultY0 () = 0;		// get first result Y-coordinat

  virtual float resultY1 () = 0;		// get second result Y-coordina

  FloatGrid
    *_input;					// resampled temporary grid

  FloatGridAccessor
    *_inp,					// accessor of one input
    *_res;					// accessor of the result

  UCharGrid
    *_ucg;					// multiplexed array of inputs

  UCharGridAccessor
    *_inps;					// accessor to array of inputs

  FloatToUChar
    *_ftuc;					// mover of a input to an array

  MultiAttributes
    *_ma;					// compressed inputs

  InputLUT
    **_LUTs;					// LUTs for the compressd inpts

  OutputLUT
    *_result_LUT;				// LUT for the result

  long
    _which_result,				// which result to compute
    _compressed_inputs_flag,			// whether or not compressed
    _rescaled_result_flag;			// whether or not rescaled

  float
    _percent_of_B;				// percent of B to apply

private:
  int moveInputsToCompress ();			// move input grids to compress

  FloatGrid *prepareInput			// gets input grid ready for
    (int index);				//   given index

  int prepareAllOfInput				// gets all of inpt grd rdy for
    (int index);				//   given index

  void resetInput				// resets input grid as needed
    (int index);				//   at given index

  void deleteInput ();				// _input deleted if its temp

  FloatGrid *prepareResult			// gets result grid ready for
    (int index);				//   given input index

  int prepareAllOfResult ();			// gets all of rsult grid ready

  int rescaleLUT ();				// rescale LUT using results

  int rescaleGrid ();				// rescale result using results

  int initializeResultantGrid ();		// initialize the result Grid

  int
    _inputs_out_of_date,                        // inputs out of date flag (T=1
    _result_out_of_date,			// result out of date flag (T=1
    _compression_out_of_date,                   // compression out of date flag
    _number_of_inputs;				// number of input grids

  GridErrorCodes
    _error_status;				// error status flag

};

#endif
