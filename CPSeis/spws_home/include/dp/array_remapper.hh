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
// array_remapper.hh: user interface file for the ArrayRemapper object

#ifndef ARRAY_REMAPPER_HH
#define ARRAY_REMAPPER_HH

#include "dp/decoder.hh"
#include "dp/grid_error_codes.hh"

class ArrayRemapper : public Decoder {

public:
  ArrayRemapper					// constructor
    (const float *array,			//   given float array
     int coding_levels=256,			//   # of coding levels
     int num_bins=1);				//   # of bins per coding level

  ~ArrayRemapper ();				// destructor

  int resetArray				// reset the given float array
    (const float *array,			//   given float array
     int coding_levels=256,			//   # of coding levels
     int num_bins=1);				//   # of bins per coding level

  int verifyOffset				// return valid offset
    (int offset);				//   given input offset

  int verifyLevel				// return valid level
    (int level);				//   given input level

  int verifyBin					// return valid bin
    (int bin);					//   given input bin

  float getValue				// return value at
    (int offset);				//   given input offset

  void getLevel					// return level set at
    (int offset,				//   given input offset
     float *elem);				//   returned level values

  void setGain					// set the intensity gain
    (float gain)				//   given gain value
    { _gain = gain; }

  float getGain	()				// return the intensity gain
    { return _gain; }

  void setCompression				// set the compression value
    (int compression);				//   given compression value

  int getCompression ()				// return the compression value
    { return _compression; }

  int failed ();				// return 1 if unsuccessful

  GridErrorCodes errorStatus ()			// return error status flag
    { return _error_status; }

private:
  void compress ();			 	// do compression

  int
    _compression,
    _ar_num_bins;

  float
    _gain;

  const float
    *_array;

  GridErrorCodes
    _error_status;				// error status code

};

#endif
