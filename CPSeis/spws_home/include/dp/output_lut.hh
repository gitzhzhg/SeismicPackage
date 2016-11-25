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
// output_lut.hh: user interface file for the OutputLUT object

// in the future you may want the ability to take an InputLUT of an InputLUT of
//   an .... and let that be the resultant OutputLUT.  consider a linked list
//   of InputLUT's in that case.  similarly, you may want to do arithmetic with
//   other OutputLUT's or allow a linked list of OutputLUT's as above.

#ifndef OUTPUT_LUT_HH
#define OUTPUT_LUT_HH

#include <float.h>
#include "dp/base_float_array.hh"
#include "dp/grid_error_codes.hh"

class Decoder;
class InputLUT;

class OutputLUT : public BaseFloatArray {

public:
  OutputLUT					// constructor
    (int num_bins);				//   # of bins in LUT

  ~OutputLUT ();				// destructor

  OutputLUT &operator+=				// self sum operator with
    (const InputLUT *LUT);			//   given input LUT

  OutputLUT &operator+=				// self sum operator with
    (float value);				//   given value

  OutputLUT &operator-=				// self subtractn operator with
    (const InputLUT *LUT);			//   given input LUT

  OutputLUT &operator-=				// self subtractn operator with
    (float value);				//   given value

  OutputLUT &operator*=				// self product operator with
    (const InputLUT *LUT);			//   given input LUT

  OutputLUT &operator*=				// self product operator with
    (float value);				//   given value

  OutputLUT &operator/=				// self quotient operator with
    (const InputLUT *LUT);			//   given input LUT

  OutputLUT &operator/=				// self quotient operator with
    (float value);				//   given value

  int failed ();				// returns 1 if unsuccessful

  GridErrorCodes errorStatus ();		// returns error status flag

protected:
  virtual int doRescale ();			// rescales LUT

  virtual int doRescaleWithClip ();		// rescales LUT with clipping

private:
  GridErrorCodes
    _error_status;				// error status flag

};

#endif
