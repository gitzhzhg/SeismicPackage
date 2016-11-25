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
// decoder.hh:  Decoder class

#ifndef DECODER_HH
#define DECODER_HH

#include "dp/grid_error_codes.hh"

class Decoder {

public:
  Decoder					// constructor
    (int coding_levels=256);			//   number of levels for codin

  virtual ~Decoder ();				// destructor

  int arrayInitializer				// allocate decoder array
    (int num_bins=1,				//   number of bins per element
     int coding_levels=0);			//   number of levels for codin

  int getDecoderSize ()				// returns levels of coding
    { return _coding_levels; }

  int getBinCount ()				// return number of bins
    { return _num_bins; }

  int getMinimumCode ()				// return the minimum code indx
    { return 0; }

  int getMaximumCode ()				// return the maximum code indx
    { return _coding_levels - 1; }

  unsigned char element				// return the decoder element
    (int index) const				//   at the given index
    { return _decoder[index]; }

  unsigned char findMinimum ();			// return the minimum code valu

  unsigned char findMaximum ();			// return the maximum code valu

  int failed ();				// return 1 if unsuccessful

  GridErrorCodes errorStatus ()			// return error status flag
    { return _error_status; }

protected:
  virtual void reset ();			// initialize object

  int
    _decoder_offset,				// current offset for decoder
    _coding_levels,				// number of levels for coding
    _num_bins;					// # of elements for ea decoder

  unsigned char
    *_decoder;					// LUT for decoding array

private:
  GridErrorCodes
    _error_status;				// current error status flag

  void reclaimMemory ();			// delete arrays and objects
};

#endif
