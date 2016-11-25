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
// user interface for the FloatToUChar class
// class moves a region of a FloatGrid to a region of a plane in a UCharGrid
// if the subregion of the object receiving the data is smaller than the
//   subregion of the object sending the data, less data is sent.
// if the subregion of the object sending the data is smaller than the
//   subregion of the object receiving the data, less data is received.
// a failure occurs if a single planar (Y) region has not been set in the
//   UCharGrid subregion.

#ifndef FLOAT_TO_UCHAR_HH
#define FLOAT_TO_UCHAR_HH

#include "dp/grid_error_codes.hh"

class FloatGrid;
class UCharGrid;

class FloatToUChar {

public:
  FloatToUChar (); 				// constructor

  ~FloatToUChar ();				// destructor

  int move					// move data from fg to ucg
    (FloatGrid *from_fg,			//   FloatGrid object to get
     UCharGrid *to_ucg,				//   UCharGrid object to put to
     float gain,				//   gain to multiply times fg
     float bias);                               //   bias to add to fg moved

  void simplifyUndefinedVectors			// simplify undefined vectors
    (UCharGrid *ucg);				//   UCharGrid obj to simplify

  int failed ();				// returns 1 if unsuccessful

  GridErrorCodes errorStatus ()			// returns error status flag
    { return _error_status; }

private:
  GridErrorCodes
    _error_status;				// error status flag  

};

#endif
