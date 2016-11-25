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
// input_lut.hh: user interface file for the InputLUT object

#ifndef INPUT_LUT_HH
#define INPUT_LUT_HH

#include "dp/grid_error_codes.hh"
#include <float.h>

class Decoder;

class InputLUT {

enum {MAXIMUM_CODE_VALUE_UNDEFINED, MINIMUM_CODE_VALUE_UNDEFINED};

public:
  InputLUT					// constructor
    (Decoder *decoder, 				//   decoder object
     int column);				//   which column in decoder

  ~InputLUT ();					// destructor

  int getColumn ()				// return selected column
    { return _column; }

  int update (int column);			// update decoder objct data

  int establishUndefinedCodes			// establish undefind codes
    (long which_method				//   whether to use max or min
     = MAXIMUM_CODE_VALUE_UNDEFINED,		//   default is maximum
     float not_defined_float=-FLT_MAX);		//   undefined float code

  int establishTwoCodeValues			// establish two byte code vlus
    (unsigned char code0=0,			//   first byte code with
     float value0=0,				//   corresponding value
     unsigned char code1=254,			//   second byte code with
     float value1=254);				//   corresponding value

  unsigned char getCode				// return a code byte for
    (int element) const;			//   given LUT element

  unsigned char undefinedByte () const		// return undefined byte code
    { return _not_defined_byte; }

  float getValue				// returns a LUT float for
    (unsigned char code) const			//   given code byte
    { return _gain * (float)code + _bias; }

  float getValue				// returns a LUT float for
    (int code) const					//   given code int
    { return _gain * (float)code + _bias; }

  unsigned char getCodeFromValue		// rtrn closest code for
    (float value) const				//   given value
    { return (unsigned char)
      ((value - _bias) / _gain + 0.5); }

  int getElementFromCode			// return LUT element from
    (unsigned char code);			//   given code

  int getInsertedLUTCount ()			// rtrn the # of inserted LUTs
    { return _inserted_lut_count; }

  int insertLUT					// insert another LUT using
    (InputLUT *lut,				//   given LUT, at
     int index);				//   given index (<=insert cnt)

  int removeInsertedLUT				// remove an inserted LUT at
    (int index);				//   given index (< insert cnt)

  int removeAllInsertedLUTs ();			// remove all inserted LUTs now

  int failed ();				// return 1 if unsuccessful

  GridErrorCodes errorStatus ()			// returns error status flag
    { return _error_status; }

private:
  void establishExtremaBytes ();		// find min & max byte values

  void establishExtremaFloats ();		// find min & max float values

  int initializeExtrema ();			// find code & value extrema

  int findCodeExtrema ();			// find code extrema for LUT

  int initializeInverseLUT ();			// create inverse LUT array

  Decoder
    *_decoder;					// decoder object

  InputLUT
    **_tempLUTs,				// temporary LUT array pointer
    **_LUTs;					// inserted LUT array pointer

  int
    *_indices,					// indices used in inversLUT
    _bin_count,					// number of bins in decoder
    _column,					// which column in decoder
    _column_size,				// size of column
    _inserted_lut_count;			// number of inserted LUTs

  long
    _undefined_method;				// how to handle undefined vlus

  unsigned char
    *_inverseLUT,				// inverse LUT array
    _min_lut_code,				// current minimum LUT code
    _max_lut_code,				// current maximum LUT code
    _min_byte_value,				// current minimum decoder byte
    _max_byte_value,				// current maximum decoder byte
    _not_defined_byte;				// undefined byte value

  float
    _min_lut_value,				// current minimum LUT value
    _max_lut_value,				// current maximum LUT value
    _min_float_value,				// current minimum decodr float
    _max_float_value,				// current maximum decodr float
    _not_defined_float,				// undefined float value
    _gain,					// gain to get float from byte
    _bias;					// bias to get float from byte

  GridErrorCodes
    _error_status;				// error status flag

};

#endif
