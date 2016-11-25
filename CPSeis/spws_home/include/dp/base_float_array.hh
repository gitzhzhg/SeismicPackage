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
// base_float_array.hh:  user interface file for the BaseFloatArray class

#ifndef BASE_FLOAT_ARRAY_HH
#define BASE_FLOAT_ARRAY_HH

#include <float.h>
#include "dp/grid_error_codes.hh"

class BaseFloatArray {

public:
  BaseFloatArray				// constructor
    (long size);				//   size of the float array

  virtual ~BaseFloatArray ();			// destructor

  long getSize ()				// return the size of the array
    { return _size; }

  int setRange					// set the array range
    (float not_defined=-FLT_MAX,		//   undefined array value
     float set_minimum=0,			//   set minimum array value
     float set_maximum=FLT_MAX);		//   set maximum array value

  float getUndefined ()				// return undefined array value
    { return _not_defined; }

  float getMinimum ()				// return set minimum array vlu
    { return _set_minimum; }

  float getMaximum ()				// return set maximum array vlu
    { return _set_maximum; }

  float getElement				// return array element at
    (long index)				//   given index
    { _index = index; return _array[_index]; }  //   warning no error checks

  float nextElement ()				// return next array element
    { return _array[++_index]; }		//   warning no error checks

  int fill					// fill the array with
    (float value);				//   given value
  
  int resetUndefined				// reset undefined value with
    (float new_not_defined);			//   given value

  int setRescaleParameters			// set rescale gain and bias
    (float gain,				//   gain to apply in rescale
     float bias);				//   bias to apply in rescale

  int computeRescaleParameters			// cmpute rescale gain and bias
    (float std_dev);				//   using std dev (0->extrema)

  int rescale ();				// apply given gain and bias

  int rescaleWithClip ();			// apply given gain and bias

  float findMinimum ();				// return actual array minimum

  float findMaximum ();				// return actual array maximum

  float findStatisticalMinimum			// return computed array vlu at
    (float std_dev=1.5);			//   this many std dev's < ave

  float findStatisticalMaximum			// return computed array vlu at
    (float std_dev=1.5);			//   this many std dev's > ave

  int valueIsValid				// rtrn 1 if validity found for
    (float value) const;			//   given value

  void resetExtremaReadyFlag ();		// reset the _extrema_ready flg

  int failed ();				// returns 1 if unsuccessful

  GridErrorCodes errorStatus ()			// returns error status flag
    { return _error_status; }

protected:
  virtual int doRescale () = 0;			// sccssr rescales array (pure)

  virtual int doRescaleWithClip () = 0;		// sccssr rescales array (pure)

  int
    _extrema_ready,				// extrema status flag (F=0)
    _statistics_ready,				// statistics status flag (F=0)
    _array_not_defined;				// array not defined flag (F=0)

  float
    _actual_minimum,				// actual minimum array value
    _actual_maximum,				// actual maximum array value
    _actual_average,				// actual average array value
    _actual_std_dev,				// actual std dev array value
    _not_defined,				// undefined array value
    _set_minimum,				// set minimum array value
    _set_maximum,				// set maximum array value
    _rescale_gain,				// gain to apply in rescale
    _rescale_bias,				// bias to apply in rescale
    *_array;					// array of float data

  long
    _index,					// current index into array
    _size;					// size of array

private:
  int findExtrema ();				// determine actual array xtrma

  void findStatistics ();			// determine actual array stats

  int fillAll					// fill the entire array with
    (float value);				//   given value

  GridErrorCodes
    _error_status;				// error status flag

};

#endif
