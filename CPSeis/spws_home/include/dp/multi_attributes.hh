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
// multiple attributes class

#ifndef MULTI_ATTRIBUTES_HH
#define MULTI_ATTRIBUTES_HH

#include "dp/decoder.hh"
#include "dp/grid_error_codes.hh"

class UCharGrid;
class FloatGrid;
class NTree;

class MultiAttributes : public Decoder {

public:
  MultiAttributes				// constructor
    (UCharGrid *inputs,				//   grid containing inputs
     int coding_levels,				//   number of coding levels
     FloatGrid *result);			//   grid to hold result

  ~MultiAttributes ();				// destructor

  int initialize ();				// initially compress inputs

  void setValueDisposition			// set how to select decoder
    (long value_disposition)			//   0-Medn, 1-Averg, 2-Selct
    { _value_disposition =
      (unsigned char)value_disposition; }

  void setInsertVectorFlag			// set whether to allow max vec
    (int allow_insert_vector)			//   1->attemp to use max vects
    { _allow_insert_vector =
      allow_insert_vector; }

  int update					// update result in givn region
    (int min_z_bin,				//   starting X-bin in result
     int min_x_bin,				//   starting Y-bin in result
     int num_z_bins,				//   number of X-bins in result
     int num_x_bins);				//   number of Y-bins in result

  int failed ();				// return 1 if failure occured

  GridErrorCodes errorStatus ();			// return error status flag

private:
  int setUpdateRegion				// sets update region
    (int min_z_bin,				//   starting X-bin in result
     int min_x_bin,				//   starting Y-bin in result
     int num_z_bins,				//   number of X-bins in result
     int num_x_bins);				//   number of Y-bins in result

  int doUpdate ();				// do update on result

  UCharGrid
    *_inputs;					// unsigned char grid of inputs

  FloatGrid
    *_result;					// float grid to contain result

  NTree
    *_n_tree;					// N-Tree to compress inputs

  int
    _needs_initializing,			// initializing switch (T=1)
    _allow_insert_vector;			// 1->attempt to use max vects

  unsigned char
    _value_disposition;				// how to select decoder vlus

  GridErrorCodes
    _error_status;				// current error status flag

};

#endif
